# ============================================================================
# vm_sync_movimientos.R
# Script autónomo — se ejecuta cada hora en la virtual machine
#
# Flujo:
#   1. Consulta la API H2H con ventana de las últimas 6 horas
#   2. Transforma y enriquece el dataframe crudo
#   3. JOIN con AGENDA para asignar CLIENTE / NO_CLIENTE / RAZON_SOCIAL
#   4. Calcula SALDO acumulado por cuenta
#   5. MERGE a MOVIMIENTOS_EC2 en Snowflake (inserta nuevos, sobreescribe existentes)
#   6. Escribe log de ejecución en vm_sync_log.txt
#
# Deduplicación: primary key (CUENTA, NUM_MOVIMIENTO)
# Ventana de consulta: últimas 6 horas (fromDate = hoy - 1 día para cubrir medianoche)
# ============================================================================

# ── Librerías ─────────────────────────────────────────────────────────────────
suppressPackageStartupMessages({
  library(curl)
  library(jsonlite)
  library(digest)
  library(base64enc)
  library(DBI)
  library(RJDBC)
  library(rJava)
})

# ============================================================================
# CONFIGURACIÓN — ajustar rutas antes de desplegar en la VM
# ============================================================================
VM_LOG_PATH  <- Sys.getenv("SYNC_LOG_PATH", "./logs/sync.log")
VM_PEM_PATH  <- Sys.getenv("SNOWFLAKE_PRIVATE_KEY_PATH")
VM_JDBC_PATH <- Sys.getenv("SNOWFLAKE_JDBC_JAR")
# ============================================================================
# CREDENCIALES API H2H
# ============================================================================
H2H_API_KEY  <- Sys.getenv("BANK_API_KEY")
H2H_SECRET   <- Sys.getenv("BANK_API_SECRET")
H2H_BASE_URL <- Sys.getenv("BANK_API_BASE")

# ============================================================================
# CREDENCIALES SNOWFLAKE
# ============================================================================
SF_ACCOUNT   <- Sys.getenv("SNOWFLAKE_ACCOUNT")
SF_USER      <- Sys.getenv("SNOWFLAKE_USER")
SF_WAREHOUSE <- "WH_ANALYTICS"
SF_DATABASE  <- "DB_ANALYTICS"
SF_SCHEMA    <- "SCH_CORE"
SF_ROLE      <- "ADMIN_ALMENA"

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
# UTILIDADES DE LOG
# ============================================================================
vm_log <- function(...) {
  msg <- paste0("[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "] ", paste(...))
  cat(msg, "\n")
  tryCatch(
    write(msg, file = VM_LOG_PATH, append = TRUE),
    error = function(e) NULL
  )
}

# ============================================================================
# CONEXIÓN SNOWFLAKE — autenticación JWT con llave PEM
# ============================================================================
vm_crear_conexion_snowflake <- function() {
  
  # Convertir PEM a objeto PrivateKey de Java (misma lógica que APP1.R)
  lineas       <- readLines(VM_PEM_PATH, warn = FALSE)
  lineas       <- lineas[!grepl("^-----", lineas)]
  clave_base64 <- paste(lineas, collapse = "")
  
  decoder    <- .jcall("java/util/Base64", "Ljava/util/Base64$Decoder;", "getDecoder")
  bytes_clave <- .jcall(decoder, "[B", "decode", clave_base64)
  key_spec   <- .jnew("java/security/spec/PKCS8EncodedKeySpec", bytes_clave)
  key_factory <- .jcall("java/security/KeyFactory", "Ljava/security/KeyFactory;",
                        "getInstance", "RSA")
  private_key <- .jcall(key_factory, "Ljava/security/PrivateKey;",
                        "generatePrivate", .jcast(key_spec, "java/security/spec/KeySpec"))
  
  # Driver JDBC
  driver <- RJDBC::JDBC("net.snowflake.client.jdbc.SnowflakeDriver",
                        classPath = VM_JDBC_PATH,
                        identifier.quote = "`")
  
  # Propiedades de conexión
  url_conexion <- paste0("jdbc:snowflake://", SF_ACCOUNT, "/")
  propiedades  <- .jnew("java.util.Properties")
  .jcall(propiedades, "Ljava/lang/Object;", "setProperty", "user",          SF_USER)
  .jcall(propiedades, "Ljava/lang/Object;", "setProperty", "warehouse",     SF_WAREHOUSE)
  .jcall(propiedades, "Ljava/lang/Object;", "setProperty", "db",            SF_DATABASE)
  .jcall(propiedades, "Ljava/lang/Object;", "setProperty", "schema",        SF_SCHEMA)
  .jcall(propiedades, "Ljava/lang/Object;", "setProperty", "role",          SF_ROLE)
  .jcall(propiedades, "Ljava/lang/Object;", "setProperty", "authenticator", "SNOWFLAKE_JWT")
  .jcall(propiedades, "Ljava/lang/Object;", "setProperty", "tracing",       "OFF")
  .jcall(propiedades, "Ljava/lang/Object;", "setProperty", "JDBC_QUERY_RESULT_FORMAT", "JSON")
  .jcall(propiedades, "Ljava/lang/Object;", "put",
         .jcast(.jnew("java/lang/String", "privateKey"), "java/lang/Object"),
         .jcast(private_key, "java/lang/Object"))
  
  # Conectar
  jconn <- .jcall(driver@jdrv, "Ljava/sql/Connection;", "connect", url_conexion, propiedades)
  if (is.jnull(jconn)) stop("Conexion retorno null")
  
  conexion <- new("JDBCConnection", jc = jconn, identifier.quote = driver@identifier.quote)
  if (!DBI::dbIsValid(conexion)) stop("Conexion no valida")
  
  # Configurar contexto
  dbSendUpdate(conexion, paste0("USE WAREHOUSE ", SF_WAREHOUSE))
  dbSendUpdate(conexion, paste0("USE DATABASE ",  SF_DATABASE))
  dbSendUpdate(conexion, paste0("USE SCHEMA ",    SF_SCHEMA))
  
  conexion
}

# ============================================================================
# FIRMA HMAC-SHA256 para la API H2H
# ============================================================================
vm_firmar <- function(secret, payload) {
  hex_sig   <- digest::hmac(key = secret, object = payload, algo = "sha256")
  raw_bytes <- as.raw(strtoi(regmatches(hex_sig, gregexpr("..", hex_sig))[[1]], 16L))
  base64enc::base64encode(raw_bytes)
}

# ============================================================================
# REQUEST AUTENTICADO A LA API H2H
# ============================================================================
vm_call_api <- function(method = "GET", path, body = "") {
  timestamp <- as.character(as.integer(Sys.time()))
  payload   <- paste0(timestamp, method, path, body)
  signature <- vm_firmar(H2H_SECRET, payload)
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
    error = function(e) { vm_log("ERROR red API:", e$message); NULL }
  )
}

# ============================================================================
# CONSULTA A LA API — ventana de las últimas 6 horas
# La API solo acepta fromDate/toDate por día, así que consultamos
# hoy y ayer (para cubrir el cruce de medianoche) y filtramos por hora en R
# ============================================================================
vm_get_movimientos_ventana <- function() {
  ahora     <- Sys.time()
  hace_6h   <- ahora - (6 * 3600)
  fecha_hoy  <- format(ahora,   "%Y%m%d")
  fecha_ayer <- format(hace_6h, "%Y%m%d")
  
  vm_log("Ventana de consulta:", format(hace_6h, "%Y-%m-%d %H:%M"), "->",
         format(ahora, "%Y-%m-%d %H:%M"))
  
  fechas <- unique(c(fecha_ayer, fecha_hoy))
  df_total <- NULL
  
  for (fecha_str in fechas) {
    path <- sprintf(
      "/api/v1/transactions/collections?fromDate=%s&toDate=%s",
      fecha_str, fecha_str
    )
    
    resp <- vm_call_api("GET", path)
    
    if (is.null(resp)) {
      vm_log("AVISO: respuesta nula para fecha", fecha_str)
      next
    }
    if (resp$status_code != 200) {
      vm_log("AVISO: HTTP", resp$status_code, "para fecha", fecha_str)
      next
    }
    
    datos <- tryCatch(
      jsonlite::fromJSON(rawToChar(resp$content), flatten = TRUE),
      error = function(e) { vm_log("ERROR JSON fecha", fecha_str, ":", e$message); NULL }
    )
    
    if (is.null(datos) || !isTRUE(datos$success)) {
      vm_log("AVISO: respuesta sin exito para fecha", fecha_str)
      next
    }
    
    df_dia <- tryCatch(as.data.frame(datos$data), error = function(e) NULL)
    if (is.null(df_dia) || nrow(df_dia) == 0) {
      vm_log("Sin movimientos para fecha", fecha_str)
      next
    }
    
    vm_log("Movimientos recibidos fecha", fecha_str, ":", nrow(df_dia))
    df_total <- if (is.null(df_total)) df_dia else rbind(df_total, df_dia)
  }
  
  if (is.null(df_total) || nrow(df_total) == 0) {
    vm_log("Sin movimientos en la ventana de 6 horas")
    return(NULL)
  }
  
  vm_log("Total movimientos en ventana:", nrow(df_total))
  df_total
}

# ============================================================================
# ASIGNAR EMPRESA por últimos 4 dígitos de cuenta
# ============================================================================
vm_asignar_empresa <- function(cuenta) {
  limpia <- gsub("[^0-9]", "", trimws(as.character(cuenta)))
  sufijo <- substr(limpia, nchar(limpia) - 3, nchar(limpia))
  empresa <- BANK_ACCOUNT_MAP[[sufijo]]
  if (is.null(empresa)) "No registrada" else empresa
}

# ============================================================================
# EXTRAER IDENTIFICADOR (CLABE o cuenta) de DESCRIPCION_DETALLADA
# ============================================================================
vm_extraer_identificador <- function(texto) {
  if (is.null(texto) || is.na(texto) || trimws(texto) == "") return("")
  
  texto_upper <- toupper(trimws(texto))
  numeros     <- regmatches(texto, gregexpr("[0-9]+", texto))[[1]]
  if (length(numeros) == 0) return("")
  
  numeros <- numeros[nchar(numeros) <= 18]
  if (length(numeros) == 0) return("")
  
  # Caso DE LA CUENTA (sin TSP) — siempre 10 dígitos
  if (grepl("DE LA CUENTA", texto_upper) && !grepl("TSP", texto_upper)) {
    n10 <- numeros[nchar(numeros) == 10]
    if (length(n10) > 0) return(n10[1])
    return("")
  }
  
  # Caso SPEI o TEF — CLABE 18 dígitos o cuenta 10 dígitos
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
# TRANSFORMAR Y ENRIQUECER
# Recibe dataframe crudo de la API, devuelve dataframe listo para MOVIMIENTOS_EC2
# ============================================================================
vm_transformar_y_enriquecer <- function(df_api, conn) {
  
  buscar <- function(df, ...) {
    for (op in c(...)) if (op %in% names(df)) return(df[[op]])
    rep(NA_character_, nrow(df))
  }
  
  # ── 1. Mapear columnas ───────────────────────────────────────────────────
  df <- data.frame(
    CUENTA                = as.character(buscar(df_api, "accountNumber", "CUENTA")),
    FECHA_OPERACION       = as.character(buscar(df_api, "operationDate",  "FECHA DE OPERACION")),
    FECHA                 = as.character(buscar(df_api, "valueDate",      "FECHA")),
    REFERENCIA            = as.character(buscar(df_api, "reference",      "REFERENCIA")),
    DESCRIPCION           = as.character(buscar(df_api, "description",    "DESCRIPCION")),
    COD_TRANSAC           = as.character(buscar(df_api, "conceptCode",    "ALMENA DESARROLLOS Y PROYECTOS SA DE CV")),
    SUCURSAL              = as.character(buscar(df_api, "branchCode",     "SUCURSAL")),
    NUM_MOVIMIENTO        = suppressWarnings(as.integer(
      buscar(df_api, "movementNumber", "MOVIMIENTO"))),
    DESCRIPCION_DETALLADA = as.character(buscar(df_api, "conceptComplements",
                                                "DESCRIPCION DETALLADA")),
    chargeType            = suppressWarnings(as.integer(buscar(df_api, "chargeType"))),
    amount_raw            = suppressWarnings(as.numeric(buscar(df_api,  "amount"))),
    stringsAsFactors      = FALSE
  )
  
  # conceptComplements puede venir como lista anidada
  if ("conceptComplements" %in% names(df_api) && is.list(df_api$conceptComplements)) {
    df$DESCRIPCION_DETALLADA <- sapply(df_api$conceptComplements, function(v) {
      paste(unlist(v), collapse = " | ")
    })
  }
  
  # ── 2. DEPOSITOS / RETIROS ───────────────────────────────────────────────
  df$DEPOSITOS <- ifelse(!is.na(df$chargeType) & df$chargeType == 2,
                         df$amount_raw, NA_real_)
  df$RETIROS   <- ifelse(!is.na(df$chargeType) & df$chargeType != 2,
                         df$amount_raw, NA_real_)
  df$chargeType <- NULL
  df$amount_raw <- NULL
  
  # ── 3. Ordenar por NUM_MOVIMIENTO ────────────────────────────────────────
  df <- df[order(df$NUM_MOVIMIENTO), ]
  
  # ── 4. EMPRESA y HOJA_EXCEL ──────────────────────────────────────────────
  df$EMPRESA    <- sapply(df$CUENTA, vm_asignar_empresa)
  df$HOJA_EXCEL <- sapply(df$CUENTA, function(c) {
    limpia <- gsub("[^0-9]", "", trimws(as.character(c)))
    substr(limpia, nchar(limpia) - 3, nchar(limpia))
  })
  
  # ── 5. IDENTIFICADOR ─────────────────────────────────────────────────────
  df$IDENTIFICADOR <- sapply(df$DESCRIPCION_DETALLADA, vm_extraer_identificador)
  
  # ── 6. JOIN con AGENDA ───────────────────────────────────────────────────
  vm_log("Consultando AGENDA en Snowflake...")
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
      
      encontrados <- 0
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
          encontrados        <- encontrados + 1
        }
      }
      
      no_registrados <- sum(df$CLIENTE == "")
      vm_log("JOIN AGENDA — encontrados:", encontrados,
             "| sin match:", no_registrados,
             "| duplicados en agenda:", sum(df$CLIENTE == "Identificador duplicado en la agenda"))
    }
  } else {
    vm_log("AVISO: AGENDA vacía o no disponible, CLIENTE quedará vacío")
  }
  
  # ── 7. Calcular SALDO acumulado por cuenta ───────────────────────────────
  # Busca el último SALDO en MOVIMIENTOS_EC2 como punto de partida
  cuentas_unicas <- unique(df$CUENTA)
  saldo_inicial  <- setNames(rep(0, length(cuentas_unicas)), cuentas_unicas)
  
  for (cta in cuentas_unicas) {
    sql_saldo <- paste0(
      "SELECT SALDO FROM DB_ANALYTICS.SCH_CORE.MOVIMIENTOS_EC2 ",
      "WHERE CUENTA = '", gsub("'", "''", cta), "' ",
      "ORDER BY FECHA_CARGA DESC LIMIT 1"
    )
    res <- tryCatch(DBI::dbGetQuery(conn, sql_saldo), error = function(e) NULL)
    if (!is.null(res) && nrow(res) > 0 && !is.na(res$SALDO[1])) {
      saldo_inicial[cta] <- as.numeric(res$SALDO[1])
      vm_log("Saldo inicial cuenta", cta, ":", saldo_inicial[cta])
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
  
  vm_log("Dataframe enriquecido listo:", nrow(df), "filas,",
         length(cuentas_unicas), "cuenta(s)")
  df
}

# ============================================================================
# MERGE A MOVIMIENTOS_EC2
# WHEN MATCHED     -> UPDATE (sobreescribe con datos frescos + JOIN de agenda)
# WHEN NOT MATCHED -> INSERT (movimiento nuevo)
# Lotes de 200 filas para no exceder el límite de payload de Snowflake
# ============================================================================
vm_merge_a_snowflake <- function(df, conn) {
  
  TAMANO_LOTE <- 200
  
  esc <- function(x) {
    if (is.null(x)) return("NULL")
    v <- as.character(x)
    if (is.na(v) || trimws(v) == "" || v == "NA") return("NULL")
    paste0("'", gsub("'", "''", v), "'")
  }
  esc_num <- function(x) {
    v <- suppressWarnings(as.numeric(x))
    if (is.na(v)) return("NULL")
    as.character(v)
  }
  esc_date <- function(x) {
    v <- as.character(x)
    if (is.na(v) || trimws(v) == "" || v == "NA") return("NULL")
    f <- tryCatch(as.Date(v), error = function(e) NULL)
    if (!is.null(f) && !is.na(f)) return(paste0("'", format(f, "%Y-%m-%d"), "'"))
    return("NULL")
  }
  
  valores_validos <- c()
  
  for (i in seq_len(nrow(df))) {
    cuenta_val <- as.character(df$CUENTA[i])
    mov_val    <- suppressWarnings(as.integer(df$NUM_MOVIMIENTO[i]))
    if (is.na(cuenta_val) || cuenta_val == "" || is.na(mov_val)) next
    
    v <- paste0("(",
                esc(df$EMPRESA[i]),              ",",
                "'", gsub("'","''", cuenta_val), "',",
                esc_date(df$FECHA_OPERACION[i]), ",",
                esc_date(df$FECHA[i]),           ",",
                esc(df$REFERENCIA[i]),           ",",
                esc(df$DESCRIPCION[i]),          ",",
                esc(df$COD_TRANSAC[i]),          ",",
                esc(df$SUCURSAL[i]),             ",",
                esc_num(df$DEPOSITOS[i]),        ",",
                esc_num(df$RETIROS[i]),          ",",
                esc_num(df$SALDO[i]),            ",",
                mov_val,                          ",",
                esc(df$DESCRIPCION_DETALLADA[i]),",",
                esc(df$IDENTIFICADOR[i]),        ",",
                esc(df$CLIENTE[i]),              ",",
                esc(df$RAZON_SOCIAL[i]),         ",",
                esc(df$NO_CLIENTE[i]),           ",",
                esc(df$HOJA_EXCEL[i]),           ",",
                "CURRENT_TIMESTAMP()",
                ")"
    )
    valores_validos <- c(valores_validos, v)
  }
  
  if (length(valores_validos) == 0) {
    vm_log("Sin filas válidas para el MERGE")
    return(list(exitoso = TRUE, procesadas = 0))
  }
  
  lotes            <- split(valores_validos,
                            ceiling(seq_along(valores_validos) / TAMANO_LOTE))
  total_procesadas <- 0
  
  for (idx_lote in seq_along(lotes)) {
    lote <- lotes[[idx_lote]]
    
    sql <- paste0(
      "MERGE INTO DB_ANALYTICS.SCH_CORE.MOVIMIENTOS_EC2 AS t ",
      "USING (SELECT * FROM VALUES ",
      paste(lote, collapse = ","),
      " AS s(",
      "  EMPRESA, CUENTA, FECHA_OPERACION, FECHA, REFERENCIA, DESCRIPCION,",
      "  COD_TRANSAC, SUCURSAL, DEPOSITOS, RETIROS, SALDO, NUM_MOVIMIENTO,",
      "  DESCRIPCION_DETALLADA, IDENTIFICADOR, CLIENTE, RAZON_SOCIAL,",
      "  NO_CLIENTE, HOJA_EXCEL, FECHA_CARGA",
      ")) AS s ",
      "ON (t.CUENTA = s.CUENTA AND t.NUM_MOVIMIENTO = s.NUM_MOVIMIENTO) ",
      # WHEN MATCHED: sobreescribe todo incluyendo CLIENTE/RAZON_SOCIAL del JOIN con AGENDA
      "WHEN MATCHED THEN UPDATE SET ",
      "  t.EMPRESA                = s.EMPRESA, ",
      "  t.FECHA_OPERACION        = s.FECHA_OPERACION, ",
      "  t.FECHA                  = s.FECHA, ",
      "  t.REFERENCIA             = s.REFERENCIA, ",
      "  t.DESCRIPCION            = s.DESCRIPCION, ",
      "  t.COD_TRANSAC            = s.COD_TRANSAC, ",
      "  t.SUCURSAL               = s.SUCURSAL, ",
      "  t.DEPOSITOS              = s.DEPOSITOS, ",
      "  t.RETIROS                = s.RETIROS, ",
      "  t.SALDO                  = s.SALDO, ",
      "  t.DESCRIPCION_DETALLADA  = s.DESCRIPCION_DETALLADA, ",
      "  t.IDENTIFICADOR          = s.IDENTIFICADOR, ",
      "  t.CLIENTE                = s.CLIENTE, ",
      "  t.RAZON_SOCIAL           = s.RAZON_SOCIAL, ",
      "  t.NO_CLIENTE             = s.NO_CLIENTE, ",
      "  t.HOJA_EXCEL             = s.HOJA_EXCEL, ",
      "  t.FECHA_CARGA            = s.FECHA_CARGA ",
      # WHEN NOT MATCHED: inserta el movimiento nuevo
      "WHEN NOT MATCHED THEN INSERT (",
      "  EMPRESA, CUENTA, FECHA_OPERACION, FECHA, REFERENCIA, DESCRIPCION,",
      "  COD_TRANSAC, SUCURSAL, DEPOSITOS, RETIROS, SALDO, NUM_MOVIMIENTO,",
      "  DESCRIPCION_DETALLADA, IDENTIFICADOR, CLIENTE, RAZON_SOCIAL,",
      "  NO_CLIENTE, HOJA_EXCEL, FECHA_CARGA",
      ") VALUES (",
      "  s.EMPRESA, s.CUENTA, s.FECHA_OPERACION, s.FECHA, s.REFERENCIA, s.DESCRIPCION,",
      "  s.COD_TRANSAC, s.SUCURSAL, s.DEPOSITOS, s.RETIROS, s.SALDO, s.NUM_MOVIMIENTO,",
      "  s.DESCRIPCION_DETALLADA, s.IDENTIFICADOR, s.CLIENTE, s.RAZON_SOCIAL,",
      "  s.NO_CLIENTE, s.HOJA_EXCEL, s.FECHA_CARGA",
      ");"
    )
    
    tryCatch({
      dbSendUpdate(conn, sql)
      total_procesadas <- total_procesadas + length(lote)
      vm_log("Lote", idx_lote, "de", length(lotes), "procesado —",
             length(lote), "filas")
    }, error = function(e) {
      vm_log("ERROR en lote", idx_lote, ":", e$message)
    })
  }
  
  list(exitoso = TRUE, procesadas = total_procesadas)
}

# ============================================================================
# FUNCIÓN PRINCIPAL — se llama al final del script
# ============================================================================
vm_ejecutar <- function() {
  inicio <- Sys.time()
  vm_log("==================================================")
  vm_log("INICIO ejecucion vm_sync_movimientos")
  vm_log("==================================================")
  
  # Paso 1: Llamar a la API
  df_api <- tryCatch(
    vm_get_movimientos_ventana(),
    error = function(e) { vm_log("ERROR consultando API:", e$message); NULL }
  )
  
  if (is.null(df_api) || nrow(df_api) == 0) {
    vm_log("Sin datos de la API. Ejecución terminada sin cambios.")
    vm_log("Duración:", round(difftime(Sys.time(), inicio, units = "secs"), 1), "seg")
    return(invisible(NULL))
  }
  
  # Paso 2: Abrir conexión Snowflake
  vm_log("Abriendo conexión Snowflake...")
  conn <- tryCatch(
    vm_crear_conexion_snowflake(),
    error = function(e) { vm_log("ERROR conexión Snowflake:", e$message); NULL }
  )
  
  if (is.null(conn)) {
    vm_log("No se pudo conectar a Snowflake. Ejecución abortada.")
    return(invisible(NULL))
  }
  
  on.exit(tryCatch(dbDisconnect(conn), error = function(e) NULL))
  vm_log("Conexión Snowflake establecida")
  
  # Paso 3: Transformar y enriquecer
  df_enriquecido <- tryCatch(
    vm_transformar_y_enriquecer(df_api, conn),
    error = function(e) { vm_log("ERROR transformando datos:", e$message); NULL }
  )
  
  if (is.null(df_enriquecido)) {
    vm_log("Error en transformación. Ejecución abortada.")
    return(invisible(NULL))
  }
  
  # Paso 4: MERGE a MOVIMIENTOS_EC2
  vm_log("Iniciando MERGE a MOVIMIENTOS_EC2...")
  resultado <- tryCatch(
    vm_merge_a_snowflake(df_enriquecido, conn),
    error = function(e) { vm_log("ERROR en MERGE:", e$message); NULL }
  )
  
  duracion <- round(difftime(Sys.time(), inicio, units = "secs"), 1)
  
  if (!is.null(resultado) && resultado$exitoso) {
    vm_log("MERGE completado —", resultado$procesadas, "filas procesadas")
  } else {
    vm_log("MERGE finalizado con errores — revisar log")
  }
  
  vm_log("==================================================")
  vm_log("FIN ejecucion — Duracion:", duracion, "seg")
  vm_log("==================================================")
}

# ============================================================================
# PUNTO DE ENTRADA
# ============================================================================
vm_ejecutar()

