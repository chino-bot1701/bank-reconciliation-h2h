# ============================================================================
# Headless demo. No credentials, no bank, no ERP, no Snowflake.
#
#   Rscript scripts/run_demo.R
#
# It runs the reconciliation the way production does, on synthetic data:
#
#   1. Resolve which company a movement belongs to, from the account number
#   2. Keep only collections — a withdrawal is not a payment
#   3. Extract the payer's tax ID from the bank's free-text description
#   4. Match payments against invoices, including partials and grouped payments
#   5. Merge into the ledger twice, to show the load is idempotent
#
# Steps 1-3 call the production functions from R/clasificacion_pagos.R,
# unchanged. Only the data source is replaced.
# ============================================================================

suppressMessages({
  library(dplyr)
  library(stringr)
  library(digest)
})

# Works whether you run it from the repo root or from scripts/.
RAIZ <- if (dir.exists("R") && dir.exists("demo")) "." else ".."
if (!dir.exists(file.path(RAIZ, "R"))) {
  stop("Run this from the repository root: Rscript scripts/run_demo.R")
}

source(file.path(RAIZ, "demo", "fixtures.R"))

# ---------------------------------------------------------------------------
# The production functions. `clasificacion_pagos.R` also defines the Shiny UI,
# which needs a running app, so only the pure helpers are pulled in here.
# ---------------------------------------------------------------------------
`%cp||%` <- function(a, b) if (!is.null(a)) a else b
CP_ACCOUNT_MAP <- DEMO_EMPRESAS

prod <- readLines(file.path(RAIZ, "R", "clasificacion_pagos.R"), warn = FALSE)
tomar <- function(nombre) {
  ini <- grep(paste0("^", nombre, " <- function"), prod)[1]
  if (is.na(ini)) stop("No se encontro la funcion: ", nombre)
  prof <- 0
  for (j in seq(ini, length(prod))) {
    prof <- prof + lengths(regmatches(prod[j], gregexpr("\\{", prod[j]))) -
                   lengths(regmatches(prod[j], gregexpr("\\}", prod[j])))
    if (prof <= 0 && j > ini) return(paste(prod[ini:j], collapse = "\n"))
  }
  stop("Funcion sin cerrar: ", nombre)
}
eval(parse(text = tomar("cp_extraer_rfc")))
eval(parse(text = tomar("cp_resolver_empresa")))

linea <- function(c = "-") cat(strrep(c, 74), "\n")

cat("BANK RECONCILIATION - demo\n"); linea("=")

# ---------------------------------------------------------------------------
# Data
# ---------------------------------------------------------------------------
cfdi <- demo_generar_cfdi(60)
mov  <- demo_generar_movimientos(cfdi)
mov  <- demo_calcular_saldo(mov)

cat(sprintf("  invoices from the ERP      : %d\n", nrow(cfdi)))
cat(sprintf("  movements from the bank    : %d across %d accounts\n",
            nrow(mov), length(unique(mov$CUENTA))))

# ---------------------------------------------------------------------------
# 1. Company from the account number  (production function)
# ---------------------------------------------------------------------------
linea(); cat("1. Resolve company from account number\n")
invisible(capture.output(
  mov$EMPRESA <- vapply(mov$CUENTA, cp_resolver_empresa, character(1))))
por_empresa <- sort(table(mov$EMPRESA), decreasing = TRUE)
for (e in names(por_empresa)) cat(sprintf("   %-46s %4d\n", e, por_empresa[[e]]))

# ---------------------------------------------------------------------------
# 2. Collections only
# ---------------------------------------------------------------------------
linea(); cat("2. Keep collections, drop outgoing movements\n")
cobros <- mov[!is.na(mov$DEPOSITOS) & mov$DEPOSITOS > 0, ]
cat(sprintf("   %d movements -> %d collections (%d outgoing dropped)\n",
            nrow(mov), nrow(cobros), nrow(mov) - nrow(cobros)))

# ---------------------------------------------------------------------------
# 3. Tax ID out of the bank's free text  (production function)
# ---------------------------------------------------------------------------
linea(); cat("3. Extract payer tax ID from the description\n")
cobros$RFC <- vapply(cobros$DESCRIPCION_DETALLADA, cp_extraer_rfc, character(1))
con_rfc <- sum(!is.na(cobros$RFC))
cat(sprintf("   tax ID found in %d of %d (%.0f%%)\n",
            con_rfc, nrow(cobros), 100 * con_rfc / nrow(cobros)))
cat(sprintf("   the other %d carry no tax ID and cannot be matched this way\n",
            nrow(cobros) - con_rfc))

# ---------------------------------------------------------------------------
# 4. Match against invoices
# ---------------------------------------------------------------------------
linea(); cat("4. Match payments to invoices\n")
cobros$ESTADO <- "unmatched"
cobros$FOLIOS <- ""
pendiente <- setNames(cfdi$TOTAL, cfdi$UUID)

for (i in order(-cobros$DEPOSITOS)) {
  rfc <- cobros$RFC[i]
  if (is.na(rfc)) next
  candidatas <- cfdi$UUID[cfdi$RFC == rfc & pendiente[cfdi$UUID] > 0.01]
  if (!length(candidatas)) next

  restante <- cobros$DEPOSITOS[i]
  liquidadas <- character(0)
  for (u in candidatas) {
    if (restante <= 0.01) break
    aplica <- min(restante, pendiente[[u]])
    pendiente[[u]] <- pendiente[[u]] - aplica
    restante <- restante - aplica
    liquidadas <- c(liquidadas, cfdi$FOLIO[cfdi$UUID == u])
  }
  if (length(liquidadas)) {
    cobros$FOLIOS[i] <- paste(liquidadas, collapse = "+")
    cobros$ESTADO[i] <- if (length(liquidadas) > 1) "grouped" else "matched"
  }
}

resumen <- table(cobros$ESTADO)
for (k in names(resumen)) cat(sprintf("   %-12s %4d\n", k, resumen[[k]]))
saldadas <- sum(pendiente <= 0.01)
cat(sprintf("   invoices fully settled: %d of %d\n", saldadas, nrow(cfdi)))
cat(sprintf("   outstanding: $%s\n",
            format(round(sum(pendiente[pendiente > 0])), big.mark = ",")))

# ---------------------------------------------------------------------------
# 5. Idempotent merge — the property the hourly job depends on
# ---------------------------------------------------------------------------
linea(); cat("5. Merge into the ledger, twice\n")
cat("   Primary key is (account, movement number). The job runs every hour\n")
cat("   over a 6-hour window, so every row is offered about six times.\n\n")

libro <- new.env(parent = emptyenv())
fusionar <- function(filas) {
  ins <- upd <- igual <- 0L
  for (i in seq_len(nrow(filas))) {
    clave <- paste(filas$CUENTA[i], filas$NUM_MOVIMIENTO[i], sep = "|")
    huella <- digest(filas[i, c("FECHA_OPERACION", "DESCRIPCION",
                                "DESCRIPCION_DETALLADA", "DEPOSITOS",
                                "RETIROS", "SALDO")], algo = "md5")
    if (!exists(clave, envir = libro, inherits = FALSE)) {
      assign(clave, huella, envir = libro); ins <- ins + 1L
    } else if (get(clave, envir = libro) != huella) {
      assign(clave, huella, envir = libro); upd <- upd + 1L
    } else igual <- igual + 1L
  }
  c(insert = ins, update = upd, unchanged = igual)
}

r1 <- fusionar(mov)
cat(sprintf("   first run   %5d insert %5d update %5d unchanged\n",
            r1[["insert"]], r1[["update"]], r1[["unchanged"]]))
r2 <- fusionar(mov)
cat(sprintf("   second run  %5d insert %5d update %5d unchanged\n",
            r2[["insert"]], r2[["update"]], r2[["unchanged"]]))

mov2 <- mov
cambiados <- sample(seq_len(nrow(mov2)), 5)
mov2$DESCRIPCION_DETALLADA[cambiados] <-
  paste(mov2$DESCRIPCION_DETALLADA[cambiados], "[CORREGIDO POR EL BANCO]")
r3 <- fusionar(mov2)
cat(sprintf("   bank amends 5 rows  %5d insert %5d update %5d unchanged\n",
            r3[["insert"]], r3[["update"]], r3[["unchanged"]]))

# ---------------------------------------------------------------------------
linea("="); cat("RESULT\n"); linea()
ok <- TRUE
check <- function(cond, txt) {
  ok <<- ok && cond
  cat(sprintf("  [%s]  %s\n", if (cond) "PASS" else "FAIL", txt))
}
check(nrow(cobros) < nrow(mov), "outgoing movements were excluded")
check(con_rfc > 0 && con_rfc < nrow(cobros),
      "some payments carry a tax ID and some do not, as in production")
check(sum(cobros$ESTADO == "grouped") > 0,
      "at least one payment settled more than one invoice")
check(r2[["insert"]] == 0 && r2[["update"]] == 0,
      sprintf("re-running the same window wrote %d rows (expected 0)",
              r2[["insert"]] + r2[["update"]]))
check(r3[["update"]] == 5 && r3[["insert"]] == 0,
      sprintf("after 5 amendments: %d updated, %d inserted (expected 5, 0)",
              r3[["update"]], r3[["insert"]]))
linea("=")
quit(status = if (ok) 0 else 1)
