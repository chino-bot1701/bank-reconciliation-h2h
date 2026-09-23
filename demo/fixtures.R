# ============================================================================
# Synthetic bank feed and invoice ledger for the demo.
#
# The movements are shaped like what the host-to-host API returns, and the
# invoices like what the ERP returns. Everything is invented: companies,
# accounts, tax IDs, amounts, references.
#
# The point of the generator is the messy cases, not the clean ones:
#   - deposits whose description carries a tax ID, and deposits that do not
#   - one payment settling several invoices
#   - one invoice settled by several payments
#   - deposits from a payer with no invoice at all
#   - withdrawals mixed in, which are not collections and must be filtered out
# ============================================================================

set.seed(1701)

DEMO_EMPRESAS <- list(
  "1101" = "ALMENA ADMINISTRACION INMOBILIARIA SA DE CV",
  "1102" = "ALMENA DESARROLLOS Y PROYECTOS SA DE CV",
  "1103" = "INMOBILIARIA LOMA PRIETA SA DE CV",
  "1104" = "EDIFICIOS DE LA PRADERA SA DE CV",
  "1105" = "BIENES SAN MARCOS SA DE CV",
  "1106" = "OPERADORA REGIONAL DE INMUEBLES SA DE CV"
)

DEMO_CLIENTES <- data.frame(
  RFC = c("CBO180312K72", "FLU150822J41", "BIS090714R58", "GVE120405M90",
          "TSI170228T13", "BNO200119B65", "OCL110930X21", "TAL060517D44",
          "SKA190211Q08", "PNO140723L37"),
  NOMBRE = c("Cafe Bonanza", "Farmacia Lucero", "Banco del Istmo",
             "Gimnasio Vertice", "Telecom Sierra", "Burger Nogal",
             "Optica Clara", "Tiendas Almendro", "Sushi Kaiso",
             "Papeleria Nova"),
  stringsAsFactors = FALSE
)

# ----------------------------------------------------------------------------
# Invoices the ERP would return
# ----------------------------------------------------------------------------
demo_generar_cfdi <- function(n = 60) {
  idx <- sample(seq_len(nrow(DEMO_CLIENTES)), n, replace = TRUE)
  cuentas <- names(DEMO_EMPRESAS)
  data.frame(
    UUID     = sprintf("CFDI-%05d", seq_len(n)),
    FOLIO    = as.character(40000 + seq_len(n)),
    CUENTA   = sample(cuentas, n, replace = TRUE),
    RFC      = DEMO_CLIENTES$RFC[idx],
    CLIENTE  = DEMO_CLIENTES$NOMBRE[idx],
    FECHA    = as.character(as.Date("2026-03-01") + sample(0:27, n, TRUE)),
    TOTAL    = round(runif(n, 8000, 320000), 2),
    stringsAsFactors = FALSE
  )
}

# ----------------------------------------------------------------------------
# Movements the host-to-host API would return
# ----------------------------------------------------------------------------
demo_generar_movimientos <- function(cfdi) {
  filas <- list()
  n_mov <- 0

  agregar <- function(cuenta, fecha, desc, desc_det, deposito, retiro) {
    n_mov <<- n_mov + 1
    filas[[length(filas) + 1]] <<- data.frame(
      CUENTA               = cuenta,
      NUM_MOVIMIENTO       = n_mov,
      FECHA_OPERACION      = fecha,
      REFERENCIA           = sample(100000:999999, 1),
      DESCRIPCION          = desc,
      DESCRIPCION_DETALLADA = desc_det,
      DEPOSITOS            = deposito,
      RETIROS              = retiro,
      stringsAsFactors     = FALSE
    )
  }

  # --- collections that match an invoice, in four shapes ---
  for (i in seq_len(nrow(cfdi))) {
    f <- cfdi[i, ]
    patron <- sample(c("exacto", "parcial", "sin_rfc", "agrupado", "ninguno"),
                     1, prob = c(0.45, 0.20, 0.15, 0.12, 0.08))
    if (patron == "ninguno") next

    fecha <- as.character(as.Date(f$FECHA) + sample(1:12, 1))
    con_rfc <- sprintf("SPEI RECIBIDO RFC %s %s REF %s",
                       f$RFC, toupper(f$CLIENTE), f$FOLIO)
    sin_rfc <- sprintf("SPEI RECIBIDO DE %s", toupper(f$CLIENTE))

    if (patron == "exacto") {
      agregar(f$CUENTA, fecha, "DEPOSITO DE CUENTA DE TERCEROS", con_rfc,
              f$TOTAL, NA)
    } else if (patron == "parcial") {
      mitad <- round(f$TOTAL / 2, 2)
      agregar(f$CUENTA, fecha, "DEPOSITO DE CUENTA DE TERCEROS", con_rfc,
              mitad, NA)
      agregar(f$CUENTA, as.character(as.Date(fecha) + 9),
              "DEPOSITO DE CUENTA DE TERCEROS", con_rfc,
              round(f$TOTAL - mitad, 2), NA)
    } else if (patron == "sin_rfc") {
      # The payer did not put a tax ID in the reference. This is the case
      # that cannot be auto-matched and has to fall back to amount + name.
      agregar(f$CUENTA, fecha, "DEPOSITO DE CUENTA DE TERCEROS", sin_rfc,
              f$TOTAL, NA)
    } else if (patron == "agrupado" && i < nrow(cfdi)) {
      # One payment covering this invoice and the next one from the same payer.
      otra <- cfdi[i + 1, ]
      if (otra$RFC == f$RFC) {
        agregar(f$CUENTA, fecha, "DEPOSITO DE CUENTA DE TERCEROS", con_rfc,
                round(f$TOTAL + otra$TOTAL, 2), NA)
      } else {
        agregar(f$CUENTA, fecha, "DEPOSITO DE CUENTA DE TERCEROS", con_rfc,
                f$TOTAL, NA)
      }
    }
  }

  # --- deposits from a payer with no invoice ---
  for (k in 1:6) {
    agregar(sample(names(DEMO_EMPRESAS), 1),
            as.character(as.Date("2026-03-01") + sample(0:27, 1)),
            "DEPOSITO DE CUENTA DE TERCEROS",
            sprintf("SPEI RECIBIDO RFC XAXX010101000 PAGO NO IDENTIFICADO %d", k),
            round(runif(1, 5000, 90000), 2), NA)
  }

  # --- outgoing movements: not collections, must be filtered out ---
  for (k in 1:25) {
    agregar(sample(names(DEMO_EMPRESAS), 1),
            as.character(as.Date("2026-03-01") + sample(0:27, 1)),
            sample(c("TRASPASO A CUENTA DE TERCEROS", "COMISION MANEJO DE CUENTA",
                     "PAGO DE SUA-IMSS", "COMPRA ORDEN DE PAGO SPEI"), 1),
            "", NA, round(runif(1, 1000, 250000), 2))
  }

  do.call(rbind, filas)
}

# ----------------------------------------------------------------------------
# Running balance per account — same rule the hourly job applies
# ----------------------------------------------------------------------------
demo_calcular_saldo <- function(mov, saldo_inicial = 1e6) {
  mov <- mov[order(mov$CUENTA, mov$NUM_MOVIMIENTO), ]
  mov$SALDO <- NA_real_
  for (cta in unique(mov$CUENTA)) {
    i <- mov$CUENTA == cta
    dep <- ifelse(is.na(mov$DEPOSITOS[i]), 0, mov$DEPOSITOS[i])
    ret <- ifelse(is.na(mov$RETIROS[i]), 0, mov$RETIROS[i])
    mov$SALDO[i] <- saldo_inicial + cumsum(dep - ret)
  }
  mov
}
