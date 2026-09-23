# ============================================================================
# MovimientosEC.R — VERSIÓN NUEVA ARQUITECTURA
#
# ELIMINADO en esta versión:
#   - mec_ingestar_excel()          -> ingestión legacy desde archivo Excel
#   - mec_ingestar_desde_api()      -> ingestión desde app, ahora la hace la VM
#   - mec_cargar_registrar_clientes() -> tabla REGISTRAR_CLIENTES ya no existe
#   - DELETE FROM REGISTRAR_CLIENTES en mec_registrar_cliente()
# ============================================================================

`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0 && !is.na(a[1])) a else b

# ============================================================================
# FUNCIÓN: Cargar hoja desde Snowflake
# ============================================================================
mec_cargar_hoja_sf <- function(nombre_hoja) {
  tryCatch({
    cuenta_limpia <- trimws(gsub("[^0-9]", "", nombre_hoja))
    sufijo4 <- substr(cuenta_limpia, nchar(cuenta_limpia) - 3, nchar(cuenta_limpia))
    
    conn <- crear_conexion_snowflake()
    on.exit(tryCatch(dbDisconnect(conn), error = function(e) NULL))
    dbSendUpdate(conn, "USE WAREHOUSE WH_ANALYTICS")
    dbSendUpdate(conn, "USE DATABASE DB_ANALYTICS")
    dbSendUpdate(conn, "USE SCHEMA SCH_CORE")
    
    sql <- paste0(
      "SELECT ",
      "  m.EMPRESA, m.CUENTA, m.FECHA_OPERACION AS \"FECHA DE OPERACIÓN\", ",
      "  m.FECHA, m.REFERENCIA, m.DESCRIPCION AS \"DESCRIPCIÓN\", ",
      "  m.COD_TRANSAC AS \"COD. TRANSAC\", m.SUCURSAL, ",
      "  m.DEPOSITOS AS \"DEPÓSITOS\", m.RETIROS, m.SALDO, ",
      "  m.NUM_MOVIMIENTO AS MOVIMIENTO, ",
      "  m.DESCRIPCION_DETALLADA AS \"DESCRIPCIÓN DETALLADA\", ",
      "  m.CLIENTE, m.RAZON_SOCIAL AS \"RAZON SOCIAL\", ",
      "  m.NO_CLIENTE AS \"No DE CLIENTE\", ",
      "  m.HOJA_EXCEL, ",
      "  c.PLAZA, c.EFECTIVIDAD, c.RECUPERACION, ",
      "  c.ESTACIONAMIENTO, c.CINES, c.FILIALES, ",
      "  c.CUENTA_CHEQUE AS \"CUENTA CHEQUE\", ",
      "  c.DEPARTAMENTO, c.FACTURA, c.CATEGORIA, c.TIPO, c.CLASIFICACION ",
      "FROM DB_ANALYTICS.SCH_CORE.MOVIMIENTOS_EC2 m ",
      "LEFT JOIN DB_ANALYTICS.SCH_CORE.CLASIFICACION_EC c ",
      "  ON m.CUENTA = c.CUENTA AND m.NUM_MOVIMIENTO = c.NUM_MOVIMIENTO ",
      "WHERE RIGHT(m.CUENTA, 4) = '", sufijo4, "' ",
      "ORDER BY m.NUM_MOVIMIENTO ASC"
    )
    
    df <- DBI::dbGetQuery(conn, sql)
    if (nrow(df) > 0) df$`.fila_id` <- seq_len(nrow(df))
    return(df)
    
  }, error = function(e) {
    cat("Error mec_cargar_hoja_sf:", e$message, "\n")
    return(NULL)
  })
}

# ============================================================================
# FUNCIÓN: Guardar clasificación en Snowflake
# Ahora hace UPSERT en CLASIFICACION_EC en lugar de UPDATE en MOVIMIENTOS_EC
# ============================================================================
mec_guardar_clasificacion <- function(cuenta, num_movimiento, clasificacion,
                                      monto_efec = NULL, monto_rec = NULL,
                                      estacionamiento = NULL, cines = NULL,
                                      texto_filiales = NULL,
                                      usuario = "app_user") {
  tryCatch({
    conn <- crear_conexion_snowflake()
    on.exit(tryCatch(dbDisconnect(conn), error = function(e) NULL))
    dbSendUpdate(conn, "USE WAREHOUSE WH_ANALYTICS")
    dbSendUpdate(conn, "USE DATABASE DB_ANALYTICS")
    dbSendUpdate(conn, "USE SCHEMA SCH_CORE")
    
    efec_sql     <- if (!is.null(monto_efec)     && !is.na(monto_efec))     as.character(monto_efec)  else "NULL"
    rec_sql      <- if (!is.null(monto_rec)      && !is.na(monto_rec))      as.character(monto_rec)   else "NULL"
    estac_sql    <- if (!is.null(estacionamiento) && nchar(trimws(estacionamiento)) > 0)
      paste0("'", gsub("'", "''", estacionamiento), "'") else "NULL"
    cines_sql    <- if (!is.null(cines)          && nchar(trimws(cines)) > 0)
      paste0("'", gsub("'", "''", cines), "'")          else "NULL"
    filiales_sql <- if (!is.null(texto_filiales) && nchar(trimws(texto_filiales)) > 0)
      paste0("'", gsub("'", "''", texto_filiales), "'") else "NULL"
    
    cuenta_esc  <- gsub("'", "''", as.character(cuenta))
    usuario_esc <- gsub("'", "''", as.character(usuario))
    clasif_esc  <- gsub("'", "''", as.character(clasificacion))
    
    sql <- paste0(
      "MERGE INTO DB_ANALYTICS.SCH_CORE.CLASIFICACION_EC AS t ",
      "USING (SELECT ",
      "  '", cuenta_esc, "'       AS CUENTA, ",
      "  ", as.integer(num_movimiento), " AS NUM_MOVIMIENTO ",
      ") AS s ",
      "ON (t.CUENTA = s.CUENTA AND t.NUM_MOVIMIENTO = s.NUM_MOVIMIENTO) ",
      "WHEN MATCHED THEN UPDATE SET ",
      "  CLASIFICACION  = '", clasif_esc, "', ",
      "  EFECTIVIDAD     = ", efec_sql, ", ",
      "  RECUPERACION    = ", rec_sql, ", ",
      "  ESTACIONAMIENTO = ", estac_sql, ", ",
      "  CINES           = ", cines_sql, ", ",
      "  FILIALES        = ", filiales_sql, ", ",
      "  USUARIO_CLASIF = '", usuario_esc, "', ",
      "  FECHA_CLASIF   = CURRENT_TIMESTAMP() ",
      "WHEN NOT MATCHED THEN INSERT (",
      "  CUENTA, NUM_MOVIMIENTO, CLASIFICACION, ",
      "  EFECTIVIDAD, RECUPERACION, ESTACIONAMIENTO, CINES, FILIALES, ",
      "  USUARIO_CLASIF, FECHA_CLASIF ",
      ") VALUES (",
      "  '", cuenta_esc, "', ", as.integer(num_movimiento), ", '", clasif_esc, "', ",
      "  ", efec_sql, ", ", rec_sql, ", ", estac_sql, ", ", cines_sql, ", ", filiales_sql, ", ",
      "  '", usuario_esc, "', CURRENT_TIMESTAMP() ",
      ");"
    )
    
    dbSendUpdate(conn, sql)
    cat("Clasificacion SF:", cuenta, "mov:", num_movimiento, "->", clasificacion, "\n")
    return(TRUE)
    
  }, error = function(e) {
    cat("Error guardando clasificacion SF:", e$message, "\n")
    return(FALSE)
  })
}

# ============================================================================
# FUNCIÓN: Generar Excel de descarga desde Snowflake
# Lee MOVIMIENTOS_EC2 con LEFT JOIN a CLASIFICACION_EC
# ============================================================================
mec_generar_excel_descarga <- function(hojas, archivo_destino) {
  tryCatch({
    conn <- crear_conexion_snowflake()
    on.exit(tryCatch(dbDisconnect(conn), error = function(e) NULL))
    dbSendUpdate(conn, "USE WAREHOUSE WH_ANALYTICS")
    dbSendUpdate(conn, "USE DATABASE DB_ANALYTICS")
    dbSendUpdate(conn, "USE SCHEMA SCH_CORE")
    
    sufijos <- unique(sapply(hojas, function(h) {
      cuenta_limpia <- trimws(gsub("[^0-9]", "", h))
      substr(cuenta_limpia, nchar(cuenta_limpia) - 3, nchar(cuenta_limpia))
    }))
    
    sufijos_sql <- paste0("('", paste(sufijos, collapse = "','"), "')")
    
    sql_total <- paste0(
      "SELECT m.EMPRESA, m.CUENTA, m.FECHA_OPERACION, m.FECHA, m.REFERENCIA, m.DESCRIPCION, ",
      "  m.COD_TRANSAC, m.SUCURSAL, m.DEPOSITOS, m.RETIROS, m.SALDO, m.NUM_MOVIMIENTO, ",
      "  m.DESCRIPCION_DETALLADA, m.CLIENTE, m.RAZON_SOCIAL, m.NO_CLIENTE, ",
      "  c.PLAZA, c.EFECTIVIDAD, c.RECUPERACION, c.ESTACIONAMIENTO, c.CINES, c.FILIALES, ",
      "  c.CUENTA_CHEQUE, c.DEPARTAMENTO, c.FACTURA, c.CATEGORIA, c.TIPO, ",
      "  m.HOJA_EXCEL ",
      "FROM DB_ANALYTICS.SCH_CORE.MOVIMIENTOS_EC2 m ",
      "LEFT JOIN DB_ANALYTICS.SCH_CORE.CLASIFICACION_EC c ",
      "  ON m.CUENTA = c.CUENTA AND m.NUM_MOVIMIENTO = c.NUM_MOVIMIENTO ",
      "WHERE RIGHT(m.CUENTA, 4) IN ", sufijos_sql, " ",
      "ORDER BY m.HOJA_EXCEL, m.NUM_MOVIMIENTO ASC"
    )
    
    df_total <- tryCatch(DBI::dbGetQuery(conn, sql_total), error = function(e) NULL)
    if (is.null(df_total) || nrow(df_total) == 0) stop("Sin datos en Snowflake")
    
    wb          <- openxlsx::createWorkbook()
    fmt_moneda  <- openxlsx::createStyle(numFmt = "$#,##0.00")
    headerStyle <- openxlsx::createStyle(
      fontSize       = 11,
      fontColour     = "white",
      halign         = "center",
      fgFill         = "#FF0000",
      border         = "TopBottomLeftRight",
      borderColour   = "black",
      textDecoration = "bold"
    )
    
    anchos_fijos <- list(
      EMPRESA               = 14,
      CUENTA                = 20,
      FECHA_OPERACION       = 14,
      FECHA                 = 12,
      REFERENCIA            = 14,
      DESCRIPCION           = 22,
      COD_TRANSAC           = 12,
      SUCURSAL              = 10,
      DEPOSITOS             = 14,
      RETIROS               = 12,
      SALDO                 = 14,
      NUM_MOVIMIENTO        = 10,
      DESCRIPCION_DETALLADA = 40,
      CLIENTE               = 20,
      RAZON_SOCIAL          = 24,
      NO_CLIENTE            = 14,
      PLAZA                 = 10,
      EFECTIVIDAD           = 14,
      RECUPERACION          = 14,
      ESTACIONAMIENTO       = 16,
      CINES                 = 12,
      FILIALES              = 14,
      CUENTA_CHEQUE         = 16,
      DEPARTAMENTO          = 16,
      FACTURA               = 14,
      CATEGORIA             = 14,
      TIPO                  = 12
    )
    
    for (hoja in hojas) {
      cuenta_limpia <- trimws(gsub("[^0-9]", "", hoja))
      sufijo4       <- substr(cuenta_limpia, nchar(cuenta_limpia) - 3, nchar(cuenta_limpia))
      
      df_hoja <- df_total[!is.na(df_total$HOJA_EXCEL) & df_total$HOJA_EXCEL == hoja, ]
      if (nrow(df_hoja) == 0) {
        df_hoja <- df_total[substr(trimws(gsub("[^0-9]", "", df_total$CUENTA)),
                                   nchar(trimws(gsub("[^0-9]", "", df_total$CUENTA))) - 3,
                                   nchar(trimws(gsub("[^0-9]", "", df_total$CUENTA)))) == sufijo4, ]
      }
      if (nrow(df_hoja) == 0) next
      
      df_hoja$HOJA_EXCEL <- NULL
      
      openxlsx::addWorksheet(wb, hoja)
      openxlsx::writeData(wb, sheet = hoja, x = df_hoja)
      openxlsx::addStyle(wb, sheet = hoja, headerStyle,
                         rows = 1, cols = 1:ncol(df_hoja), gridExpand = TRUE)
      
      cols_moneda <- c("DEPOSITOS", "RETIROS", "SALDO",
                       "EFECTIVIDAD", "RECUPERACION")
      if (length(cols_moneda) > 0) {
        openxlsx::addStyle(wb, sheet = hoja, style = fmt_moneda,
                           rows = 2:(nrow(df_hoja) + 1), cols = cols_moneda,
                           gridExpand = TRUE, stack = TRUE)
      }
      
      anchos <- sapply(names(df_hoja), function(col) {
        ancho <- anchos_fijos[[col]]
        if (is.null(ancho)) 14 else ancho
      })
      openxlsx::setColWidths(wb, sheet = hoja, cols = 1:ncol(df_hoja), widths = anchos)
      openxlsx::addFilter(wb, sheet = hoja, rows = 1, cols = 1:ncol(df_hoja))
    }
    
    openxlsx::saveWorkbook(wb, archivo_destino, overwrite = TRUE)
    cat("Excel SF generado:", archivo_destino, "\n")
    return(TRUE)
    
  }, error = function(e) {
    cat("Error generando Excel SF:", e$message, "\n")
    stop(e$message)
  })
}

# ============================================================================
# FUNCIÓN: Hojas disponibles con conteo
# Lee desde MOVIMIENTOS_EC2 con LEFT JOIN a CLASIFICACION_EC
# ============================================================================
mec_hojas_disponibles_sf <- function() {
  tryCatch({
    conn <- crear_conexion_snowflake()
    on.exit(tryCatch(dbDisconnect(conn), error = function(e) NULL))
    dbSendUpdate(conn, "USE WAREHOUSE WH_ANALYTICS")
    dbSendUpdate(conn, "USE DATABASE DB_ANALYTICS")
    dbSendUpdate(conn, "USE SCHEMA SCH_CORE")
    
    sql <- paste0(
      "SELECT m.HOJA_EXCEL, COUNT(*) AS TOTAL, ",
      "  SUM(CASE WHEN c.CLASIFICACION IS NOT NULL AND c.CLASIFICACION != '' ",
      "      THEN 1 ELSE 0 END) AS CLASIFICADOS ",
      "FROM DB_ANALYTICS.SCH_CORE.MOVIMIENTOS_EC2 m ",
      "LEFT JOIN DB_ANALYTICS.SCH_CORE.CLASIFICACION_EC c ",
      "  ON m.CUENTA = c.CUENTA AND m.NUM_MOVIMIENTO = c.NUM_MOVIMIENTO ",
      "GROUP BY m.HOJA_EXCEL ORDER BY m.HOJA_EXCEL"
    )
    
    df <- DBI::dbGetQuery(conn, sql)
    return(df)
    
  }, error = function(e) {
    cat("Error mec_hojas_disponibles_sf:", e$message, "\n")
    return(NULL)
  })
}

# ============================================================================
# FUNCIÓN: Registrar cliente — inserta o actualiza en AGENDA
# Ya no elimina de REGISTRAR_CLIENTES (tabla eliminada en nueva arquitectura)
# ============================================================================
mec_registrar_cliente <- function(identificador, num_contrato = "",
                                  cliente = "", razon_social = "") {
  tryCatch({
    conn <- crear_conexion_snowflake()
    on.exit(tryCatch(dbDisconnect(conn), error = function(e) NULL))
    dbSendUpdate(conn, "USE WAREHOUSE WH_ANALYTICS")
    dbSendUpdate(conn, "USE DATABASE DB_ANALYTICS")
    dbSendUpdate(conn, "USE SCHEMA SCH_CORE")
    
    id_esc <- gsub("'", "''", trimws(identificador))
    no_esc <- gsub("'", "''", trimws(num_contrato))
    cl_esc <- gsub("'", "''", trimws(cliente))
    rs_esc <- gsub("'", "''", trimws(razon_social))
    
    sql_agenda <- paste0(
      "MERGE INTO DB_ANALYTICS.SCH_CORE.AGENDA AS t ",
      "USING (SELECT '", id_esc, "' AS IDENTIFICADOR) AS s ",
      "ON t.IDENTIFICADOR = s.IDENTIFICADOR ",
      "WHEN MATCHED THEN UPDATE SET ",
      "  NO_CLIENTE  = '", no_esc, "', ",
      "  CLIENTE     = '", cl_esc, "', ",
      "  RAZON_SOCIAL = '", rs_esc, "' ",
      "WHEN NOT MATCHED THEN INSERT (IDENTIFICADOR, NO_CLIENTE, CLIENTE, RAZON_SOCIAL) ",
      "VALUES ('", id_esc, "','", no_esc, "','", cl_esc, "','", rs_esc, "');"
    )
    DBI::dbSendUpdate(conn, sql_agenda)
    
    cat("Cliente registrado en AGENDA:", identificador, "\n")
    return(TRUE)
    
  }, error = function(e) {
    cat("Error mec_registrar_cliente:", e$message, "\n")
    return(FALSE)
  })
}

# ============================================================================
# FUNCIÓN: Cargar no registrados desde MOVIMIENTOS_EC2
# Reemplaza mec_cargar_registrar_clientes() — filtra donde CLIENTE está vacío
# ============================================================================
mec_cargar_no_registrados <- function() {
  tryCatch({
    conn <- crear_conexion_snowflake()
    on.exit(tryCatch(dbDisconnect(conn), error = function(e) NULL))
    dbSendUpdate(conn, "USE WAREHOUSE WH_ANALYTICS")
    dbSendUpdate(conn, "USE DATABASE DB_ANALYTICS")
    dbSendUpdate(conn, "USE SCHEMA SCH_CORE")
    
    df <- DBI::dbGetQuery(conn,
      "SELECT DISTINCT
       EMPRESA,
       CUENTA,
       IDENTIFICADOR,
       CLIENTE,
       RAZON_SOCIAL
       FROM DB_ANALYTICS.SCH_CORE.MOVIMIENTOS_EC2
       WHERE (CLIENTE IS NULL OR TRIM(CLIENTE) = '')
         AND IDENTIFICADOR IS NOT NULL
         AND TRIM(IDENTIFICADOR) != ''
       ORDER BY EMPRESA, CUENTA"
    )
    return(df)
    
  }, error = function(e) {
    cat("Error mec_cargar_no_registrados:", e$message, "\n")
    return(NULL)
  })
}

cat("MovimientosEC.R cargado\n")