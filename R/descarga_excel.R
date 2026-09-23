# ============================================================================
# DescargaEC.R — Generación de Excel de EC desde Snowflake
# Respeta el filtro de fechas activo en Clasificación de Pagos
# Se carga via source() en APP1.R
#
# CAMBIOS respecto a versión anterior:
#   - FROM ahora apunta a V_MOVIMIENTOS_CLASIFICADOS_AUTO en lugar de MOVIMIENTOS_EC2
#   - CATEGORIA y TIPO vienen de m. (vista con reglas automáticas) en lugar de c. (CLASIFICACION_EC)
# ============================================================================

dec_generar_excel_ec <- function(fecha_inicio = NULL, fecha_fin = NULL,
                                 archivo_destino) {
  tryCatch({
    conn <- crear_conexion_snowflake()
    on.exit(tryCatch(dbDisconnect(conn), error = function(e) NULL))
    dbSendUpdate(conn, "USE WAREHOUSE WH_ANALYTICS")
    dbSendUpdate(conn, "USE DATABASE DB_ANALYTICS")
    dbSendUpdate(conn, "USE SCHEMA SCH_CORE")
    
    # ── Construir cláusula WHERE de fechas (usa FECHA_OPERACION) ─────────────
    where_fecha <- ""
    if (!is.null(fecha_inicio) && !is.na(fecha_inicio)) {
      where_fecha <- paste0(where_fecha,
                            "AND m.FECHA_OPERACION >= '", format(as.Date(fecha_inicio), "%Y-%m-%d"), "' ")
    }
    if (!is.null(fecha_fin) && !is.na(fecha_fin)) {
      where_fecha <- paste0(where_fecha,
                            "AND m.FECHA_OPERACION <= '", format(as.Date(fecha_fin), "%Y-%m-%d"), "' ")
    }
    
    cat("DEBUG fecha_inicio:", class(fecha_inicio), "-", as.character(fecha_inicio), "\n")
    cat("DEBUG fecha_fin:", class(fecha_fin), "-", as.character(fecha_fin), "\n")
    
    sql <- paste0(
      "SELECT ",
      "  m.EMPRESA, ",
      "  m.CUENTA, ",
      "  m.FECHA_OPERACION AS \"FECHA DE OPERACION\", ",
      "  m.FECHA, ",
      "  m.REFERENCIA, ",
      "  m.DESCRIPCION, ",
      "  m.COD_TRANSAC AS \"COD TRANSAC\", ",
      "  m.SUCURSAL, ",
      "  m.DEPOSITOS, ",
      "  m.RETIROS, ",
      "  m.SALDO, ",
      "  m.NUM_MOVIMIENTO AS MOVIMIENTO, ",
      "  m.DESCRIPCION_DETALLADA AS \"DESCRIPCION DETALLADA\", ",
      "  c.PLAZA, ",
      "  m.CLIENTE, ",
      "  c.EFECTIVIDAD, ",
      "  c.RECUPERACION, ",
      "  c.ESTACIONAMIENTO, ",
      "  c.CINES, ",
      "  c.FILIALES, ",
      "  m.RAZON_SOCIAL AS \"RAZON SOCIAL\", ",
      "  m.NO_CLIENTE AS \"NO CLIENTE\", ",
      "  c.CUENTA_CHEQUE AS \"CUENTA CHEQUE\", ",
      "  c.DEPARTAMENTO, ",
      "  c.FACTURA, ",
      "  m.CATEGORIA, ",
      "  m.TIPO, ",
      "  m.HOJA_EXCEL ",
      "FROM DB_ANALYTICS.SCH_CORE.V_MOVIMIENTOS_CLASIFICADOS_AUTO m ",
      "LEFT JOIN DB_ANALYTICS.SCH_CORE.CLASIFICACION_EC c ",
      "  ON m.CUENTA = c.CUENTA AND m.NUM_MOVIMIENTO = c.NUM_MOVIMIENTO ",
      "WHERE 1=1 ", where_fecha,
      "ORDER BY m.HOJA_EXCEL, m.NUM_MOVIMIENTO ASC"
    )
    
    df_total <- tryCatch(
      DBI::dbGetQuery(conn, sql),
      error = function(e) {
        cat("ERROR en query DescargaEC:", e$message, "\n")
        NULL
      }
    )
    if (is.null(df_total) || nrow(df_total) == 0) stop("Sin datos para el rango indicado")
    cat("DescargaEC: filas obtenidas:", nrow(df_total), "\n")
    
    # ── Hojas únicas (por HOJA_EXCEL = últimos 4 dígitos de cuenta) ──────────
    hojas_unicas <- unique(df_total$HOJA_EXCEL)
    hojas_unicas <- hojas_unicas[!is.na(hojas_unicas) & hojas_unicas != ""]
    
    # ── Estilos ──────────────────────────────────────────────────────────────
    wb <- openxlsx::createWorkbook()
    
    header_style <- openxlsx::createStyle(
      fontSize       = 11,
      fontColour     = "white",
      halign         = "center",
      fgFill         = "#CC0000",
      border         = "TopBottomLeftRight",
      borderColour   = "black",
      textDecoration = "bold"
    )
    
    fmt_moneda <- openxlsx::createStyle(numFmt = "$#,##0.00")
    
    anchos_fijos <- list(
      EMPRESA                  = 18,
      CUENTA                   = 20,
      `FECHA DE OPERACION`     = 14,
      FECHA                    = 12,
      REFERENCIA               = 16,
      DESCRIPCION              = 22,
      `COD TRANSAC`            = 12,
      SUCURSAL                 = 10,
      DEPOSITOS                = 14,
      RETIROS                  = 12,
      SALDO                    = 14,
      MOVIMIENTO               = 12,
      `DESCRIPCION DETALLADA`  = 40,
      PLAZA                    = 10,
      CLIENTE                  = 22,
      EFECTIVIDAD              = 14,
      RECUPERACION             = 14,
      ESTACIONAMIENTO          = 16,
      CINES                    = 12,
      FILIALES                 = 14,
      `RAZON SOCIAL`           = 24,
      `NO CLIENTE`             = 14,
      `CUENTA CHEQUE`          = 16,
      DEPARTAMENTO             = 16,
      FACTURA                  = 14,
      CATEGORIA                = 14,
      TIPO                     = 12
    )
    
    cols_moneda <- c("DEPOSITOS", "RETIROS", "SALDO",
                     "EFECTIVIDAD", "RECUPERACION")
    
    # ── Una pestaña por cuenta ────────────────────────────────────────────────
    for (hoja in hojas_unicas) {
      
      df_hoja <- df_total[!is.na(df_total$HOJA_EXCEL) & df_total$HOJA_EXCEL == hoja, ]
      if (nrow(df_hoja) == 0) next
      
      df_hoja$HOJA_EXCEL <- NULL
      
      nombre_pestana <- substr(gsub("[^0-9A-Za-z]", "", as.character(hoja)), 1, 31)
      if (nombre_pestana == "" || nombre_pestana %in% openxlsx::sheets(wb)) {
        nombre_pestana <- paste0("CTA_", hoja)
      }
      
      openxlsx::addWorksheet(wb, nombre_pestana)
      openxlsx::writeData(wb, sheet = nombre_pestana, x = df_hoja)
      
      # Encabezados rojos
      openxlsx::addStyle(wb, sheet = nombre_pestana, style = header_style,
                         rows = 1, cols = 1:ncol(df_hoja), gridExpand = TRUE)
      
      # Formato moneda
      idx_moneda <- which(names(df_hoja) %in% cols_moneda)
      if (length(idx_moneda) > 0 && nrow(df_hoja) > 0) {
        openxlsx::addStyle(wb, sheet = nombre_pestana, style = fmt_moneda,
                           rows = 2:(nrow(df_hoja) + 1),
                           cols = idx_moneda,
                           gridExpand = TRUE, stack = TRUE)
      }
      
      # Anchos de columna
      anchos <- sapply(names(df_hoja), function(col) {
        a <- anchos_fijos[[col]]
        if (is.null(a)) 14 else a
      })
      openxlsx::setColWidths(wb, sheet = nombre_pestana,
                             cols = 1:ncol(df_hoja), widths = anchos)
      
      # Filtros automáticos
      openxlsx::addFilter(wb, sheet = nombre_pestana,
                          rows = 1, cols = 1:ncol(df_hoja))
    }
    
    openxlsx::saveWorkbook(wb, archivo_destino, overwrite = TRUE)
    cat("DescargaEC: archivo generado en", archivo_destino, "\n")
    return(TRUE)
    
  }, error = function(e) {
    cat("Error dec_generar_excel_ec:", e$message, "\n")
    stop(e$message)
  })
}

cat("DescargaEC.R cargado\n")