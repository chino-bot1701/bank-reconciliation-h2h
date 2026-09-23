# ============================================================================
# H2H_API.R — Integración host-to-host Banco del Istmo
# Se carga via source() en APP1.R
# ============================================================================

# ============================================================================
# CREDENCIALES DE LA API (hardcodeadas)
# ============================================================================
H2H_API_KEY  <- Sys.getenv("BANK_API_KEY")
H2H_SECRET   <- Sys.getenv("BANK_API_SECRET")
H2H_BASE_URL <- Sys.getenv("BANK_API_BASE")

# ============================================================================
# MAPA DE EMPRESAS — últimos 4 dígitos de cuenta -> nombre de empresa
# ============================================================================
BANK_ACCOUNT_MAP <- list(
  "1112" = "ALMENA ADMINISTRACION INMOBILIARIA SA DE CV",
  "1143" = "ALMENA DESARROLLOS Y PROYECTOS SA DE CV",
  "1121" = "ALMENA PROYECTOS Y EDIFICIOS SA DE CV",
  "1113" = "ALMENA Y PASEOS SA DE CV",
  "1128" = "ALMENA Y PASEOS SA DE CV",
  "1107" = "ALMENA Y PASEOS SA DE CV",
  "1117" = "INMOBILIARIA BOSQUES DEL SUR SA DE CV",
  "1118" = "INMOBILIARIA LOMA PRIETA SA DE CV",
  "1116" = "EDIFICIOS DEL CENTRO SA DE CV",
  "1119" = "EDIFICIOS DE LA PRADERA SA DE CV",
  "1110" = "BIENES SAN MARCOS SA DE CV",
  "1120" = "DESARROLLO INMOBILIARIO ALAMEDA SA DE CV",
  "1123" = "OPERADORA REGIONAL DE INMUEBLES SA DE CV",
  "1114" = "PROMOTORA DE PROYECTOS DEL PONIENTE SA DE CV",
  "1101" = "ALMENA ADMINISTRACION INMOBILIARIA SA DE CV",
  "1137" = "ALMENA PROYECTOS Y EDIFICIOS SA DE CV",
  "1130" = "ALMENA Y PASEOS SA DE CV",
  "1129" = "ALMENA Y PASEOS SA DE CV",
  "1105" = "INMOBILIARIA BOSQUES DEL SUR SA DE CV",
  "1124" = "INMOBILIARIA LOMA PRIETA SA DE CV",
  "1127" = "INMOBILIARIA LOMA PRIETA SA DE CV",
  "1122" = "EDIFICIOS DEL CENTRO SA DE CV",
  "1136" = "EDIFICIOS DE LA PRADERA SA DE CV",
  "1134" = "BIENES SAN MARCOS SA DE CV",
  "1103" = "DESARROLLO INMOBILIARIO ALAMEDA SA DE CV",
  "1115" = "OPERADORA REGIONAL DE INMUEBLES SA DE CV",
  "1140" = "PROMOTORA DE PROYECTOS DEL PONIENTE SA DE CV",
  "1139" = "ALMENA ADMINISTRACION INMOBILIARIA SA DE CV",
  "1144" = "ALMENA DESARROLLOS Y PROYECTOS SA DE CV",
  "1133" = "ALMENA PROYECTOS Y EDIFICIOS SA DE CV",
  "1125" = "ALMENA Y PASEOS SA DE CV",
  "1108" = "INMOBILIARIA BOSQUES DEL SUR SA DE CV",
  "1142" = "INMOBILIARIA LOMA PRIETA SA DE CV",
  "1141" = "EDIFICIOS DEL CENTRO SA DE CV",
  "1126" = "EDIFICIOS DE LA PRADERA SA DE CV",
  "1106" = "BIENES SAN MARCOS SA DE CV",
  "1138" = "DESARROLLO INMOBILIARIO ALAMEDA SA DE CV",
  "1131" = "DESARROLLO INMOBILIARIO ALAMEDA SA DE CV",
  "1111" = "OPERADORA REGIONAL DE INMUEBLES SA DE CV",
  "1104" = "PROMOTORA DE PROYECTOS DEL PONIENTE SA DE CV",
  "1132" = "ALMENA ADMINISTRACION INMOBILIARIA SA DE CV",
  "1135" = "ALMENA DESARROLLOS Y PROYECTOS SA DE CV",
  "1109" = "ALMENA PROYECTOS Y EDIFICIOS SA DE CV"
)

# ============================================================================
# FUNCIÓN: Firma HMAC-SHA256
# ============================================================================
h2h_firmar <- function(secret, payload) {
  hex_sig   <- digest::hmac(key = secret, object = payload, algo = "sha256")
  raw_bytes <- as.raw(strtoi(regmatches(hex_sig, gregexpr("..", hex_sig))[[1]], 16L))
  base64enc::base64encode(raw_bytes)
}

# ============================================================================
# FUNCIÓN: Request autenticado
# ============================================================================
h2h_call_api <- function(method = "GET", path, body = "") {
  timestamp <- as.character(as.integer(Sys.time()))
  payload   <- paste0(timestamp, method, path, body)
  signature <- h2h_firmar(H2H_SECRET, payload)
  url       <- paste0(H2H_BASE_URL, path)
  
  h <- curl::new_handle()
  curl::handle_setopt(h,
                      ssl_verifypeer = FALSE,
                      ssl_verifyhost = FALSE,
                      customrequest  = method,
                      httpheader     = c(
                        paste0("X-Api-Key: ",   H2H_API_KEY),
                        paste0("X-Timestamp: ", timestamp),
                        paste0("X-Signature: ", signature)
                      )
  )
  
  tryCatch(
    curl::curl_fetch_memory(url, handle = h),
    error = function(e) { cat("H2H error de red:", e$message, "\n"); NULL }
  )
}

# ============================================================================
# FUNCIÓN: Descargar movimientos del día indicado (default = hoy)
# Devuelve dataframe crudo o NULL si falla
# ============================================================================
h2h_get_movimientos_dia <- function(fecha = Sys.Date()) {
  fecha_str <- format(fecha, "%Y%m%d")
  path      <- sprintf(
    "/api/v1/transactions/collections?fromDate=%s&toDate=%s",
    fecha_str, fecha_str
  )
  
  cat("H2H: consultando", as.character(fecha), "\n")
  
  resp <- h2h_call_api("GET", path)
  if (is.null(resp)) return(NULL)
  
  if (resp$status_code != 200) {
    cat("H2H HTTP", resp$status_code, ":", rawToChar(resp$content), "\n")
    return(NULL)
  }
  
  datos <- tryCatch(
    jsonlite::fromJSON(rawToChar(resp$content), flatten = TRUE),
    error = function(e) { cat("H2H: JSON invalido\n"); NULL }
  )
  
  if (is.null(datos) || !isTRUE(datos$success)) {
    cat("H2H: respuesta sin exito\n")
    return(NULL)
  }
  
  df <- as.data.frame(datos$data)
  cat("H2H: movimientos recibidos:", nrow(df), "\n")
  df
}

# ============================================================================
# FUNCIÓN: Asignar empresa por últimos 4 dígitos de cuenta
# ============================================================================
h2h_asignar_empresa <- function(cuenta) {
  limpia <- gsub("[^0-9]", "", trimws(as.character(cuenta)))
  sufijo <- substr(limpia, nchar(limpia) - 3, nchar(limpia))
  empresa <- BANK_ACCOUNT_MAP[[sufijo]]
  if (is.null(empresa)) "No registrada" else empresa
}

# ============================================================================
# FUNCIÓN: Extraer CLABE / identificador de DESCRIPCION DETALLADA
# Prioriza CLABE 18 dígitos para SPEI/TEF, 10 dígitos para DE LA CUENTA
# ============================================================================
h2h_extraer_identificador <- function(texto) {
  if (is.null(texto) || is.na(texto) || trimws(texto) == "") return("")
  
  texto_upper <- toupper(trimws(texto))
  numeros     <- regmatches(texto, gregexpr("[0-9]+", texto))[[1]]
  if (length(numeros) == 0) return("")
  
  numeros <- numeros[nchar(numeros) <= 18]
  if (length(numeros) == 0) return("")
  
  # Caso DE LA CUENTA (sin TSP) — siempre 10 dígitos inmediatamente después
  if (grepl("DE LA CUENTA", texto_upper) && !grepl("TSP", texto_upper)) {
    n10 <- numeros[nchar(numeros) == 10]
    if (length(n10) > 0) return(n10[1])
    return("")
  }
  
  # Si tiene 18 es CLABE, si tiene 10 es número de cuenta
  # Caso SPEI o TEF — puede ser CLABE (18) o cuenta (10)
  if (grepl("SPEI|TEF", texto_upper)) {
    n18 <- numeros[nchar(numeros) == 18]
    if (length(n18) > 0) return(n18[1])
    n10 <- numeros[nchar(numeros) == 10]
    if (length(n10) > 0) return(n10[1])
    return("")
  }
  
  return("")
}

# ============================================================================
# FUNCIÓN: Transformar el dataframe crudo de la API al formato MOVIMIENTOS_EC2
# conn        = conexión Snowflake ya abierta y configurada
# progress_cb = function(pct, texto) para actualizar barra en la UI
#
# ELIMINADO en esta versión:
#   - Bloque 8: columnas auxiliares de clasificación (PLAZA, EFECTIVIDAD, etc.)
#   - Bloque 9: MERGE a REGISTRAR_CLIENTES con no registrados
#   - df$IDENTIFICADOR <- NULL al final (se conserva para el script de la VM)
# ============================================================================
h2h_transformar_y_enriquecer <- function(df_api, conn, usuario = "app_user",
                                         progress_cb = NULL) {
  
  if (is.null(df_api) || nrow(df_api) == 0) stop("Sin movimientos del dia")
  
  buscar <- function(df, ...) {
    for (op in c(...)) if (op %in% names(df)) return(df[[op]])
    rep(NA_character_, nrow(df))
  }
  
  # ── 1. Mapear columnas de la API ─────────────────────────────────────────
  df <- data.frame(
    CUENTA                = as.character(buscar(df_api, "accountNumber", "CUENTA")),
    FECHA_OPERACION       = as.character(buscar(df_api, "operationDate",  "FECHA DE OPERACION")),
    FECHA                 = as.character(buscar(df_api, "valueDate",       "FECHA")),
    REFERENCIA            = as.character(buscar(df_api, "reference",       "REFERENCIA")),
    DESCRIPCION           = as.character(buscar(df_api, "description",     "DESCRIPCION")),
    COD_TRANSAC           = as.character(buscar(df_api, "conceptCode",     "ALMENA DESARROLLOS Y PROYECTOS SA DE CV")),
    SUCURSAL              = as.character(buscar(df_api, "branchCode",      "SUCURSAL")),
    NUM_MOVIMIENTO        = suppressWarnings(as.integer(
      buscar(df_api, "movementNumber", "MOVIMIENTO"))),
    DESCRIPCION_DETALLADA = as.character(buscar(df_api, "conceptComplements",
                                                "DESCRIPCION DETALLADA")),
    chargeType            = suppressWarnings(as.integer(buscar(df_api, "chargeType"))),
    amount_raw            = suppressWarnings(as.numeric(buscar(df_api, "amount"))),
    stringsAsFactors      = FALSE
  )
  
  # conceptComplements puede venir como lista anidada
  if ("conceptComplements" %in% names(df_api) && is.list(df_api$conceptComplements)) {
    df$DESCRIPCION_DETALLADA <- sapply(df_api$conceptComplements, function(v) {
      paste(unlist(v), collapse = " | ")
    })
  }
  
  # ── 2. DEPOSITOS y RETIROS ───────────────────────────────────────────────
  df$DEPOSITOS <- ifelse(!is.na(df$chargeType) & df$chargeType == 2,
                         df$amount_raw, NA_real_)
  df$RETIROS   <- ifelse(!is.na(df$chargeType) & df$chargeType != 2,
                         df$amount_raw, NA_real_)
  df$chargeType <- NULL
  df$amount_raw <- NULL
  
  # ── 3. Ordenar por NUM_MOVIMIENTO (crítico para el saldo correcto) ───────
  df <- df[order(df$NUM_MOVIMIENTO), ]
  
  # ── 4. EMPRESA y HOJA_EXCEL ──────────────────────────────────────────────
  df$EMPRESA    <- sapply(df$CUENTA, h2h_asignar_empresa)
  df$HOJA_EXCEL <- sapply(df$CUENTA, function(c) {
    limpia <- gsub("[^0-9]", "", trimws(as.character(c)))
    substr(limpia, nchar(limpia) - 3, nchar(limpia))
  })
  
  # ── 5. Extraer IDENTIFICADOR de DESCRIPCION_DETALLADA ───────────────────
  df$IDENTIFICADOR <- sapply(df$DESCRIPCION_DETALLADA, h2h_extraer_identificador)
  
  if (!is.null(progress_cb)) progress_cb(35, "Consultando agenda...")
  
  # ── 6. JOIN con tabla AGENDA de Snowflake ────────────────────────────────
  agenda <- tryCatch(DBI::dbReadTable(conn, "AGENDA"), error = function(e) NULL)
  
  df$CLIENTE      <- ""
  df$NO_CLIENTE   <- ""
  df$RAZON_SOCIAL <- ""
  
  if (!is.null(agenda) && nrow(agenda) > 0) {
    names(agenda) <- toupper(trimws(names(agenda)))
    
    id_col <- intersect(c("IDENTIFICADOR","CLABE","CUENTA","NUMERO_CUENTA","ID"), names(agenda))[1]
    cl_col <- intersect(c("CLIENTE","NOMBRE_CLIENTE","NOMBRE"),                   names(agenda))[1]
    no_col <- intersect(c("NO_CLIENTE","NUM_CLIENTE","NUMERO_CLIENTE"),            names(agenda))[1]
    rs_col <- intersect(c("RAZON_SOCIAL","RAZON SOCIAL","EMPRESA"),               names(agenda))[1]
    
    if (!is.na(id_col)) {
      ag <- data.frame(
        ID = trimws(as.character(agenda[[id_col]])),
        CL = if (!is.na(cl_col)) as.character(agenda[[cl_col]]) else "",
        NO = if (!is.na(no_col)) as.character(agenda[[no_col]]) else "",
        RS = if (!is.na(rs_col)) as.character(agenda[[rs_col]]) else "",
        stringsAsFactors = FALSE
      )
      
      conteos    <- table(ag$ID)
      ids_unicos <- names(conteos[conteos == 1])
      ids_dup    <- names(conteos[conteos > 1])
      ag_unicos  <- ag[ag$ID %in% ids_unicos, ]
      
      for (i in seq_len(nrow(df))) {
        id_mov <- trimws(df$IDENTIFICADOR[i])
        if (id_mov == "" || is.na(id_mov)) next
        
        if (id_mov %in% ids_dup) {
          df$CLIENTE[i]      <- "Identificador duplicado en la agenda"
          df$RAZON_SOCIAL[i] <- "Identificador duplicado en la agenda"
        } else if (id_mov %in% ag_unicos$ID) {
          fila               <- ag_unicos[ag_unicos$ID == id_mov, ]
          df$CLIENTE[i]      <- fila$CL[1]
          df$NO_CLIENTE[i]   <- fila$NO[1]
          df$RAZON_SOCIAL[i] <- fila$RS[1]
        }
      }
    }
  }
  
  if (!is.null(progress_cb)) progress_cb(55, "Calculando saldos por cuenta...")
  
  # ── 7. Calcular SALDO acumulado por cuenta ────────────────────────────────
  # Saldo inicial = último SALDO registrado en SF para esa cuenta (por FECHA_CARGA)
  cuentas_unicas <- unique(df$CUENTA)
  saldo_inicial  <- setNames(rep(0, length(cuentas_unicas)), cuentas_unicas)
  
  for (cta in cuentas_unicas) {
    sql <- paste0(
      "SELECT SALDO FROM DB_ANALYTICS.SCH_CORE.MOVIMIENTOS_EC2 ",
      "WHERE CUENTA = '", gsub("'", "''", cta), "' ",
      "ORDER BY FECHA_CARGA DESC LIMIT 1"
    )
    res <- tryCatch(DBI::dbGetQuery(conn, sql), error = function(e) NULL)
    if (!is.null(res) && nrow(res) > 0 && !is.na(res$SALDO[1])) {
      saldo_inicial[cta] <- as.numeric(res$SALDO[1])
    }
  }
  
  df$SALDO <- NA_real_
  for (cta in cuentas_unicas) {
    idx  <- which(df$CUENTA == cta)
    acum <- saldo_inicial[cta]
    for (i in idx) {
      dep  <- if (!is.na(df$DEPOSITOS[i])) df$DEPOSITOS[i] else 0
      ret  <- if (!is.na(df$RETIROS[i]))   df$RETIROS[i]   else 0
      acum <- acum + dep - ret
      df$SALDO[i] <- round(acum, 2)
    }
  }
  
  if (!is.null(progress_cb)) progress_cb(70, "Preparando para Snowflake...")
  
  df
}

cat("H2H_API.R cargado\n")