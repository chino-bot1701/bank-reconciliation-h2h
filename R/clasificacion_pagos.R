# ============================================================================
# CONFIGURACIÓN GLOBAL
# ============================================================================

CP_TOKEN_ERP <- Sys.getenv("ERP_API_TOKEN")
CP_BASE_URL  <- Sys.getenv("ERP_API_BASE")

tryCatch({
  httr::set_config(httr::config(http_version = 1))
}, error = function(e) NULL)

CP_COMPANY_LIST <- c(
  "EDP", "EDP", "IBS", "ILP", "ILP", "AAI", "EDC",
  "OCI", "BSM", "DIA", "PPO", "APE", "AAI", "BSM", "IBS", "AYP", "AYP",
  "DIA", "H33", "OCI", "ADP", "AYP", "PPO", "AAI", "APE", "ILP", "IBS",
  "DIA", "BSM", "ILP", "EDC", "EDC", "EDP", "PPO", "AAI", "ADP", "ILP",
  "IBS", "DIA", "EDC", "OCI", "APE", "EDP", "OCI", "ADP", "ILP", "BSM", "EDP"
)

# ============================================================================
# FUNCIONES AUXILIARES
# ============================================================================

CP_ACCOUNT_MAP <- list(
  "1134" = "DIA",  "1121" = "IBS",  "1116" = "AAI", "1119" = "EDC",
  "1120" = "BSM",  "1123" = "DIA",  "1101" = "PPO",  "1137" = "APE",
  "1130" = "AAI",  "1129" = "AAI",  "1105" = "BSM",  "1127" = "IBS",
  "1124" = "IBS",  "1136" = "ILP", "1122" = "H33",  "1115" = "OCI",
  "1140" = "PPO",  "1139" = "AAI",  "1125" = "APE",  "1108" = "AYP",
  "1142" = "ILP",  "1141" = "IBS",  "1126" = "BSM",  "1106" = "EDP",
  "1138" = "DIA",  "1131" = "DIA",  "1111" = "AAI",  "1104" = "ADP",
  "1132" = "ILP",  "1135" = "IBS",  "1109" = "DIA",  "1102" = "IBS"
)

cp_resolver_empresa <- function(nombre_hoja) {
  cuenta_limpia <- stringr::str_trim(gsub("[^0-9]", "", nombre_hoja))
  sufijo4 <- substr(cuenta_limpia, nchar(cuenta_limpia) - 3, nchar(cuenta_limpia))
  empresa <- CP_ACCOUNT_MAP[[sufijo4]]
  cat("[DEBUG resolver_empresa] hoja:", nombre_hoja, "| sufijo:", sufijo4, "| empresa:", empresa %cp||% "NULL", "\n")
  if (is.null(empresa)) return("COMPANY_LIST")
  return(empresa)
}

# ── NUEVO: extrae descripciones de la list-column "partida" ──
extraer_partidas_json <- function(cfdis_df) {
  resultado <- character(nrow(cfdis_df))
  for (i in seq_len(nrow(cfdis_df))) {
    partida <- tryCatch(cfdis_df[["partida"]][[i]], error = function(e) NULL)
    if (!is.null(partida) && is.data.frame(partida) &&
        nrow(partida) > 0 && "descripcion" %in% names(partida)) {
      descs <- partida[["descripcion"]]
      descs <- descs[!is.na(descs) & nchar(trimws(descs)) > 0]
      resultado[i] <- if (length(descs) > 0) paste(descs, collapse = " | ") else NA_character_
    } else {
      resultado[i] <- NA_character_
    }
  }
  resultado
}

cp_extraer_rfc <- function(desc_detallada) {
  if (is.na(desc_detallada) || !grepl("RFC", desc_detallada, ignore.case = TRUE)) return(NA_character_)
  m <- regmatches(desc_detallada,
                  regexpr("RFC\\s{0,5}([A-Z&]{3,4}[0-9]{6}[A-Z0-9]{3})",
                          desc_detallada, perl = TRUE))
  if (length(m) == 0) return(NA_character_)
  rfc <- stringr::str_trim(gsub("^RFC\\s*", "", m[1]))
  if (nchar(rfc) < 12 || nchar(rfc) > 13) return(NA_character_)
  return(rfc)
}

cp_leer_hoja <- function(ruta_archivo, nombre_hoja) {
  tryCatch({
    df <- readxl::read_excel(ruta_archivo, sheet = nombre_hoja, col_names = TRUE)
    for (col in c("EFECTIVIDAD", "RECUPERACION", "DEPOSITOS", "DESCRIPCIÓN DETALLADA")) {
      if (!col %in% names(df)) df[[col]] <- NA
    }
    df$`.fila_id` <- seq_len(nrow(df))
    df
  }, error = function(e) NULL)
}

cp_filtrar_spei <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(df)
  col_dep <- if ("DEPÓSITOS" %in% names(df)) "DEPÓSITOS" else "DEPOSITOS"
  df %>%
    dplyr::filter(
      !is.na(.data[[col_dep]]) &
        .data[[col_dep]] != "" &
        suppressWarnings(as.numeric(.data[[col_dep]])) > 0
    )
}

`%cp||%` <- function(a, b) if (!is.null(a)) a else b

# ============================================================================
# FUNCIÓN CORE: consulta paginada a API Inmoges
# ============================================================================
cp_query_cliente_paginado <- function(company, rfc_o_alias,
                                      fecha_inicio    = NULL,
                                      fecha_fin       = NULL,
                                      token           = CP_TOKEN_ERP,
                                      base_url        = CP_BASE_URL,
                                      registros_x_pag = 1000) {
  pagina <- 1
  todos  <- list()
  total_encontrados <- NA_integer_
  
  cat("[DEBUG query_paginado] INICIO — company:", company,
      "| rfc:", rfc_o_alias,
      "| fechas:", fecha_inicio %cp||% "SIN FECHA", "->", fecha_fin %cp||% "SIN FECHA", "\n")
  
  repeat {
    params <- list(
      empresa               = company,
      arrendatario          = rfc_o_alias,
      tipo_documento        = "Factura",
      no_registros_x_pagina = registros_x_pag,
      no_pagina             = pagina
    )
    if (!is.null(fecha_inicio)) params$fecha_inicio <- fecha_inicio
    if (!is.null(fecha_fin))    params$fecha_fin    <- fecha_fin
    # estatus ya NO se manda a la API — no lo soporta
    
    resp <- tryCatch({
      httr::GET(
        url   = paste0(base_url, "/cfdi"),
        httr::add_headers(.headers = c(
          "Accept"              = "application/json",
          "Authorization-token" = token
        )),
        query = params,
        httr::timeout(90)
      )
    }, error = function(e) {
      cat("[DEBUG query_paginado] ERROR en httr::GET:", conditionMessage(e), "\n")
      NULL
    })
    
    if (is.null(resp)) {
      cat("[DEBUG query_paginado] resp es NULL — fallo de red o timeout\n")
      break
    }
    
    cat("[DEBUG query_paginado] status HTTP:", httr::status_code(resp), "\n")
    
    if (httr::status_code(resp) != 200) {
      cat("[DEBUG query_paginado] body del error:", httr::content(resp, as = "text", encoding = "UTF-8"), "\n")
      break
    }
    
    txt  <- httr::content(resp, as = "text", encoding = "UTF-8")
    
    # Parsear sin aplanar para preservar la list-column "partida"
    data <- tryCatch(jsonlite::fromJSON(txt, flatten = FALSE), error = function(e) {
      cat("[DEBUG query_paginado] ERROR al parsear JSON:", conditionMessage(e), "\n")
      NULL
    })
    
    if (is.null(data)) break
    
    cat("[DEBUG query_paginado] result$process:", isTRUE(data$result$process),
        "| registros_encontrados:", data$result$data$registros_encontrados %cp||% "N/A", "\n")
    
    if (!isTRUE(data$result$process)) {
      cat("[DEBUG query_paginado] process=FALSE — body completo:\n", txt, "\n")
      break
    }
    
    cfdis <- data$result$data$cfdis
    if (pagina == 1) {
      total_encontrados <- as.integer(data$result$data$registros_encontrados %cp||% 0)
    }
    if (is.null(cfdis) || length(cfdis) == 0) {
      cat("[DEBUG query_paginado] cfdis vacio en pagina", pagina, "\n")
      break
    }
    
    df <- tryCatch(as.data.frame(cfdis), error = function(e) NULL)
    if (is.null(df) || nrow(df) == 0) break
    
    # Extraer descripción de partidas ANTES de eliminar la list-column
    df$descripcion_partida <- extraer_partidas_json(df)
    
    # Eliminar todas las list-columns para que rbindlist no falle
    cols_lista <- sapply(df, is.list)
    df         <- df[, !cols_lista, drop = FALSE]
    
    cat("[DEBUG query_paginado] pagina", pagina, "— filas obtenidas:", nrow(df), "\n")
    
    todos[[pagina]] <- df
    acum <- sum(sapply(todos, nrow))
    
    if (!is.na(total_encontrados) && acum >= total_encontrados) break
    if (nrow(df) < registros_x_pag) break
    pagina <- pagina + 1
    Sys.sleep(0.05)
  }
  
  cat("[DEBUG query_paginado] FIN — total filas acumuladas:", sum(sapply(todos, nrow)), "\n")
  
  if (length(todos) == 0) return(data.frame())
  data.table::rbindlist(todos, fill = TRUE)
}

cp_buscar_facturas_rfc <- function(rfc, empresa_asignada = "COMPANY_LIST",
                                   nombre_cliente = NULL,
                                   plazas_override = NULL) {
  
  # Flujo con plazas_override (búsqueda manual)
  if (!is.null(plazas_override) && length(plazas_override) > 0) {
    token_local    <- CP_TOKEN_ERP
    base_url_local <- CP_BASE_URL
    fn_query       <- cp_query_cliente_paginado
    resultados <- future.apply::future_lapply(plazas_override, function(plaza) {
      httr::set_config(httr::config(http_version = 1))
      df <- fn_query(
        company      = plaza,
        rfc_o_alias  = rfc,
        fecha_inicio = NULL,
        fecha_fin    = NULL,
        token        = token_local,
        base_url     = base_url_local
      )
      if (!is.null(df) && nrow(df) > 0) df else NULL
    }, future.seed = TRUE)
    validos <- Filter(function(df) !is.null(df) && nrow(df) > 0, resultados)
    if (length(validos) == 0) return(data.frame())
    resultado <- data.table::rbindlist(validos, fill = TRUE)
    
    # Filtrar CxC en R
    if ("estatus_documento" %in% names(resultado)) {
      resultado <- resultado[grepl("cuenta por cobrar",
                                   tolower(as.character(resultado$estatus_documento)),
                                   fixed = FALSE), ]
    }
    return(resultado)
  }
  
  # Flujo normal: empresa asignada
  empresa_consulta <- if (empresa_asignada != "COMPANY_LIST") empresa_asignada else NULL
  cat("[DEBUG buscar_facturas] empresa_asignada:", empresa_asignada,
      "| empresa_consulta:", empresa_consulta %cp||% "NULL", "\n")
  if (is.null(empresa_consulta)) {
    cat("[DEBUG buscar_facturas] SALIENDO VACIO - empresa no mapeada\n")
    return(data.frame())
  }
  
  # PASO 1: buscar por RFC
  busqueda_rfc <- trimws(rfc)
  if (nchar(busqueda_rfc) > 0) {
    df_rfc <- cp_query_cliente_paginado(
      company      = empresa_consulta,
      rfc_o_alias  = busqueda_rfc,
      fecha_inicio = NULL,
      fecha_fin    = NULL
    )
    if (!is.null(df_rfc) && nrow(df_rfc) > 0) {
      # Filtrar CxC en R
      if ("estatus_documento" %in% names(df_rfc)) {
        df_rfc <- df_rfc[grepl("cuenta por cobrar",
                               tolower(as.character(df_rfc$estatus_documento)),
                               fixed = FALSE), ]
      }
      if (nrow(df_rfc) > 0) return(df_rfc)
    }
  }
  
  # PASO 2: buscar por nombre del cliente
  busqueda_nombre <- trimws(nombre_cliente %cp||% "")
  if (nchar(busqueda_nombre) > 0) {
    df_nombre <- cp_query_cliente_paginado(
      company      = empresa_consulta,
      rfc_o_alias  = busqueda_nombre,
      fecha_inicio = NULL,
      fecha_fin    = NULL
    )
    if (!is.null(df_nombre) && nrow(df_nombre) > 0) {
      # Filtrar CxC en R
      if ("estatus_documento" %in% names(df_nombre)) {
        df_nombre <- df_nombre[grepl("cuenta por cobrar",
                                     tolower(as.character(df_nombre$estatus_documento)),
                                     fixed = FALSE), ]
      }
      if (nrow(df_nombre) > 0) return(df_nombre)
    }
  }
  
  return(data.frame())
}

# ============================================================================
# UI — función que devuelve el tabItem para insertar en APP1.R
# ============================================================================
cp_ui_tab <- function() {
  tabItem(tabName = "clasificacion_pagos",
          
          # CSS específico del módulo
          tags$head(tags$style(HTML("
      /* ============================================================
         CLASIFICACIÓN DE PAGOS — estilos con scope cp_
         ============================================================ */
      .cp-header {
        background: linear-gradient(135deg, #8B0000 0%, #B22222 60%, #6B1C1C 100%);
        padding: 20px 32px 16px;
        border-bottom: 3px solid #6B1C1C;
        box-shadow: 0 4px 16px rgba(139,0,0,0.25);
        margin: -15px -15px 0 -15px;
      }
      .cp-header h4 {
        color: #FFFFFF;
        font-size: 1.4rem;
        font-weight: 700;
        margin: 0 0 2px 0;
        letter-spacing: 0.03em;
      }
      .cp-header p {
        color: rgba(255,255,255,0.78);
        margin: 0;
        font-size: 0.82rem;
        font-weight: 300;
        letter-spacing: 0.06em;
        text-transform: uppercase;
      }
      .cp-carga-panel {
        background: #FFFFFF;
        border: 2px dashed #D4C4B8;
        border-radius: 10px;
        padding: 28px 24px;
        margin: 20px 0 0 0;
        text-align: center;
        transition: border-color 0.25s;
      }
      .cp-carga-panel:hover { border-color: #B22222; }
      .cp-carga-titulo {
        font-size: 1.1rem;
        color: #3D2B1F;
        font-weight: 600;
        margin-bottom: 6px;
      }
      .cp-carga-sub { color: #7A6057; font-size: 0.8rem; }

      .cp-filtros-bar {
        background: #FFFFFF;
        border-bottom: 1px solid #D4C4B8;
        padding: 12px 24px;
        display: flex;
        align-items: center;
        gap: 16px;
        flex-wrap: wrap;
        box-shadow: 0 2px 6px rgba(61,43,31,0.12);
        margin: 12px 0 0 0;
        border-radius: 8px 8px 0 0;
      }
      .cp-filtros-bar label {
        font-weight: 600;
        color: #3D2B1F;
        font-size: 0.82rem;
        text-transform: uppercase;
        letter-spacing: 0.05em;
        white-space: nowrap;
      }

      .cp-tabla-contenedor {
        background: #FFFFFF;
        border-radius: 0 0 10px 10px;
        border: 1px solid #D4C4B8;
        box-shadow: 0 2px 12px rgba(61,43,31,0.12);
        overflow: hidden;
      }
      .cp-tabla-header {
        background: linear-gradient(90deg, #8B0000, #B22222);
        padding: 12px 20px;
        display: flex;
        justify-content: space-between;
        align-items: center;
      }
      .cp-tabla-header-titulo {
        color: #FFFFFF;
        font-size: 0.95rem;
        font-weight: 600;
      }
      .cp-tabla-header-sub {
        color: rgba(255,255,255,0.75);
        font-size: 0.78rem;
      }

      /* Selectores de clasificación */
      .cp-sel-clasif {
        border: 1.5px solid #D4C4B8;
        border-radius: 6px;
        padding: 5px 10px;
        font-size: 0.95rem;
        background: #FFFFFF;
        color: #3D2B1F;
        cursor: pointer;
        outline: none;
        transition: border-color 0.2s;
        min-width: 160px;
      }
      .cp-sel-clasif:focus           { border-color: #ff5b49; }
      .cp-sel-clasif.cp-asig-efec    { border-color: #1A6B3C; color: #1A6B3C; background: #F0FAF5; font-weight: 600; }
      .cp-sel-clasif.cp-asig-rec     { border-color: #ff9d92; color: #802e25; background: #fff0ee; font-weight: 600; }
      .cp-sel-clasif.cp-asig-manual  { border-color: #726f6b; color: #393835; background: #f4f3f2; font-weight: 600; }
      .cp-sel-clasif.cp-asig-estac   { border-color: #1A7A5E; color: #1A7A5E; background: #F0FFF8; font-weight: 600; }
      .cp-sel-clasif.cp-asig-cines   { border-color: #ff5b49; color: #401712; background: #fff3f2; font-weight: 600; }
      .cp-sel-clasif.cp-asig-filiales{ border-color: #802e25; color: #401712; background: #fce8e6; font-weight: 600; }

      /* Botones */
      .cp-btn-ver-api {
        background: none;
        border: 1.5px solid #B22222;
        color: #B22222;
        border-radius: 6px;
        padding: 6px 14px;
        font-size: 0.95rem;
        cursor: pointer;
        transition: all 0.2s;
        font-weight: 600;
        white-space: nowrap;
      }
      .cp-btn-ver-api:hover { background: #B22222; color: #FFFFFF; }

      .cp-btn-descarga {
        background: linear-gradient(135deg, #8B0000, #B22222);
        color: #FFFFFF !important;
        border: none;
        border-radius: 8px;
        padding: 10px 22px;
        font-size: 0.85rem;
        font-weight: 600;
        cursor: pointer;
        letter-spacing: 0.04em;
        text-transform: uppercase;
        transition: all 0.2s;
        box-shadow: 0 3px 10px rgba(139,0,0,0.3);
      }
      .cp-btn-descarga:hover {
        transform: translateY(-1px);
        box-shadow: 0 5px 16px rgba(139,0,0,0.4);
        color: #FFFFFF !important;
      }

      /* Tarjetas de facturas */
      .cp-factura-card {
        background: #FFFFFF;
        border: 1px solid #D4C4B8;
        border-radius: 8px;
        padding: 12px 16px;
        margin-bottom: 8px;
        display: grid;
        grid-template-columns: 1fr 1fr auto;
        gap: 8px;
        align-items: center;
        transition: box-shadow 0.15s;
      }
      .cp-factura-card:hover { box-shadow: 0 2px 10px rgba(61,43,31,0.12); }
      .cp-factura-fecha  { font-size: 1.05rem; color: #7A6057; }
      .cp-factura-folio  { font-size: 1.2rem; color: #3D2B1F; font-weight: 600; }
      .cp-factura-total  { font-weight: 600; color: #B22222; font-size: 1.3rem; }
      .cp-factura-estatus{ font-size: 1rem; padding: 3px 10px; border-radius: 10px; }
      .cp-estatus-pagado { background: #E8F5EC; color: #1A6B3C; }
      .cp-estatus-otro   { background: #F0E8DC; color: #7A6057; }
      .cp-factura-pdf-link {
        font-size: 0.75rem; color: #B22222; font-weight: 600;
        text-decoration: none; border: 1px solid #B22222;
        padding: 2px 8px; border-radius: 4px; white-space: nowrap;
        transition: all 0.15s;
      }
      .cp-factura-pdf-link:hover { background: #B22222; color: #FFFFFF; text-decoration: none; }

      /* Spinner */
      .cp-spinner-api { text-align: center; padding: 40px; color: #7A6057; font-size: 0.9rem; }
      .cp-spinner-dot {
        display: inline-block; width: 10px; height: 10px;
        border-radius: 50%; background: #B22222; margin: 0 3px;
        animation: cp-bounce 1.2s infinite;
      }
      .cp-spinner-dot:nth-child(2) { animation-delay: 0.2s; }
      .cp-spinner-dot:nth-child(3) { animation-delay: 0.4s; }
      @keyframes cp-bounce {
        0%,80%,100% { transform: scale(0); }
        40%          { transform: scale(1); }
      }

      /* RFC chip */
      .cp-rfc-chip {
        display: inline-block; font-family: monospace;
        background: #F0E8DC; border: 1px solid #D4C4B8;
        border-radius: 4px; padding: 2px 8px;
        font-size: 1rem; color: #3D2B1F;
      }

      /* Panel de acciones */
      .cp-acciones-panel {
        background: #FFFFFF;
        border-top: 2px solid #D4C4B8;
        padding: 16px 24px;
        display: flex;
        align-items: center;
        justify-content: space-between;
        gap: 12px;
        margin-top: 0;
        box-shadow: 0 -2px 8px rgba(61,43,31,0.12);
        border-radius: 0 0 10px 10px;
      }
      .cp-acciones-info { font-size: 0.82rem; color: #7A6057; }
      .cp-acciones-info strong { color: #3D2B1F; }

      /* Meta box del modal */
      .cp-api-meta-box {
        background: #FFFFFF;
        border-left: 4px solid #B22222;
        border-radius: 0 8px 8px 0;
        padding: 14px 18px;
        margin: 16px;
        font-size: 0.82rem;
      }
      .cp-api-meta-box .cp-meta-label {
        font-weight: 600; color: #7A6057; font-size: 0.85rem;
        text-transform: uppercase; letter-spacing: 0.06em;
      }
      .cp-api-meta-box .cp-meta-val {
        color: #3D2B1F; font-size: 0.9rem; font-weight: 600;
      }

      /* Modal clasificación */
      .cp-clasif-modal-opt { display: flex; gap: 12px; margin: 18px 16px; }
      .cp-clasif-btn {
        flex: 1; padding: 14px; border-radius: 8px;
        border: 2px solid transparent; cursor: pointer;
        font-size: 0.9rem; font-weight: 600; transition: all 0.18s; text-align: center;
      }
      .cp-clasif-btn-efec { background: #F0FAF5; border-color: #1A6B3C; color: #1A6B3C; }
      .cp-clasif-btn-efec:hover { background: #1A6B3C; color: #FFFFFF; }
      .cp-clasif-btn-rec  { background: #FFF8EC; border-color: #B27700; color: #7A4F00; }
      .cp-clasif-btn-rec:hover  { background: #B27700; color: #FFFFFF; }

      /* Manual inputs */
      .cp-manual-suma-bar {
        margin: 12px 4px 0; font-size: 0.85rem; padding: 10px 14px;
        border-radius: 6px; background: #FFFFFF;
        border: 1px solid #D4C4B8;
        display: flex; justify-content: space-between; align-items: center;
      }
      .cp-suma-ok  { color: #1A6B3C; font-weight: 700; }
      .cp-suma-err { color: #B22222; font-weight: 700; }

      /* Barra de progreso automático */
      .cp-barra-progreso-wrap {
        background: #FFFFFF;
        border-bottom: 1px solid #D4C4B8;
        padding: 10px 24px;
        display: flex;
        align-items: center;
        gap: 16px;
      }

      /* No hay movimientos */
      .cp-no-facturas-msg {
        text-align: center; padding: 32px;
        color: #7A6057; font-size: 0.88rem;
      }
      
      .cp-estado-vacio {
        text-align: center; padding: 60px 24px; color: #7A6057;
      }

      /* ---- Barra de filtros reestructurada ---- */
      .cp-filtros-fila1 {
        border-radius: 8px 8px 0 0;
        border-bottom: none !important;
        padding-bottom: 10px;
      }
      .cp-filtros-fila2 {
        border-top: 1px dashed #e0d4c3 !important;
        border-radius: 0 0 8px 8px;
        padding-top: 10px;
        background: #faf8f5;
        margin-top: 0 !important;
        box-shadow: none !important;
      }
      .cp-control-grupo {
        display: flex;
        flex-direction: column;
        gap: 4px;
      }
      .cp-ctrl-label {
        font-size: 0.82rem !important;
        font-weight: 700 !important;
        text-transform: uppercase !important;
        letter-spacing: 0.05em !important;
        color: #393835 !important;
        margin: 0 !important;
        white-space: nowrap;
      }
      .cp-separador {
        width: 1px;
        height: 36px;
        background: #e0d4c3;
        align-self: flex-end;
        margin-bottom: 2px;
        flex-shrink: 0;
      }
      .cp-badge-spei {
        background: linear-gradient(135deg, #802e25, #ff5b49);
        border-radius: 20px;
        padding: 8px 18px;
        display: flex;
        align-items: center;
        white-space: nowrap;
      }
      .cp-btn-primario {
        background: linear-gradient(135deg, #802e25, #ff5b49) !important;
        color: #ffffff !important;
        border: none !important;
        border-radius: 8px !important;
        padding: 11px 26px !important;
        font-size: 1rem !important;
        font-weight: 700 !important;
        letter-spacing: 0.04em !important;
        cursor: pointer !important;
        transition: all 0.2s !important;
        box-shadow: 0 3px 10px rgba(128,46,37,0.3) !important;
        white-space: nowrap;
        align-self: flex-end;
      }
      .cp-btn-primario:hover {
        transform: translateY(-1px) !important;
        box-shadow: 0 5px 16px rgba(255,91,73,0.4) !important;
        color: #ffffff !important;
      }
      .cp-btn-oscuro {
        background: linear-gradient(135deg, #393835, #726f6b) !important;
        box-shadow: 0 3px 10px rgba(57,56,53,0.3) !important;
      }
      .cp-btn-oscuro:hover {
        box-shadow: 0 5px 16px rgba(57,56,53,0.45) !important;
      }
      .cp-btn-secundario {
        background: #e0d4c3 !important;
        border: 1.5px solid #c4b5a5 !important;
        color: #393835 !important;
        border-radius: 8px !important;
        padding: 10px 18px !important;
        font-size: 0.95rem !important;
        font-weight: 700 !important;
        cursor: pointer !important;
        align-self: flex-end;
        white-space: nowrap;
        transition: background 0.2s !important;
      }
      /* Filtro de hora */
.cp-hora-group {
  display: flex;
  align-items: center;
  gap: 6px;
}
.cp-hora-input {
  border: 1.5px solid #D4C4B8;
  border-radius: 6px;
  padding: 6px 10px;
  font-size: 0.88rem;
  color: #3D2B1F;
  background: #FFFFFF;
  width: 90px;
  outline: none;
}
.cp-hora-input:focus { border-color: #B22222; }
.cp-hora-sep {
  color: #7A6057;
  font-weight: 700;
  font-size: 1rem;
}
    "))),
          
          # ---- Header ----
          div(class = "cp-header",
              tags$h4("Clasificador de Movimientos"),
              tags$p("Módulo de efectividad y recuperación — Almena Intelligence")
          ),
          
          # ---- Panel principal (oculto hasta que cargue archivo) ----
          shinyjs::hidden(
            div(id = "cp_panel_principal",
                
                # Barra de controles — Fila 1: navegacion y fechas
                div(class = "cp-filtros-bar cp-filtros-fila1",
                    
                    div(class = "cp-control-grupo",
                        tags$label(class = "cp-ctrl-label", "Hoja / Cuenta"),
                        selectInput("cp_sel_hoja", NULL, choices = NULL, width = "210px")
                    ),
                    
                    div(class = "cp-separador"),
                    
                    div(class = "cp-control-grupo",
                        tags$label(class = "cp-ctrl-label", "Rango de fechas"),
                        uiOutput("cp_rango_fechas_ui")
                    ),
                    
                    div(class = "cp-separador"),
                    
                    div(class = "cp-control-grupo",
                        tags$label(class = "cp-ctrl-label", "Hora (opcional)"),
                        div(class = "cp-hora-group",
                            tags$input(id = "cp_hora_ini", type = "time", class = "cp-hora-input",
                                       value = "00:00"),
                            tags$span(class = "cp-hora-sep", "—"),
                            tags$input(id = "cp_hora_fin", type = "time", class = "cp-hora-input",
                                       value = "23:59")
                        )
                    ),
                    
                    div(style = "margin-left:auto; display:flex; align-items:flex-end; gap:12px;",
                        div(class = "cp-badge-spei",
                            uiOutput("cp_contador_spei_ui")
                        ),
                        actionButton("cp_btn_clasificar_todo", "Clasificacion automatica",
                                     class = "cp-btn-primario cp-btn-oscuro")
                    )
                ),
                
                # Barra de controles — Fila 2: busqueda manual
                div(class = "cp-filtros-bar cp-filtros-fila2",
                    div(class = "cp-control-grupo",
                        tags$label(class = "cp-ctrl-label", "Busqueda manual — RFC o razon social"),
                        textInput("cp_busqueda_manual", NULL,
                                  placeholder = "Ej: FEMSA840211AG2 o Grupo FEMSA",
                                  width = "300px")
                    ),
                    
                    div(class = "cp-control-grupo",
                        tags$label(class = "cp-ctrl-label", "Empresa(s)"),
                        selectizeInput("cp_plazas_manual", NULL,
                                       choices  = c("Todas las empresas" = "TODAS",
                                                    CP_COMPANY_LIST),
                                       selected = "TODAS",
                                       multiple = TRUE,
                                       width    = "280px",
                                       options  = list(
                                         placeholder      = "Seleccionar empresa(s)...",
                                         maxItems         = NULL,
                                         closeAfterSelect = FALSE
                                       ))
                    ),
                    
                    div(class = "cp-control-grupo",
                        tags$label(class = "cp-ctrl-label", "\u00a0"),
                        actionButton("cp_btn_busqueda_manual", "Buscar facturas",
                                     class = "cp-btn-primario")
                    ),
                    
                    div(class = "cp-control-grupo",
                        tags$label(class = "cp-ctrl-label", "\u00a0"),
                        actionButton("cp_btn_iniciar_descarga_ec", "Descargar EC",
                                     class = "cp-btn-primario cp-btn-oscuro")
                    ),
                    
                    # Modal de descarga EC (oculto inicialmente)
                    div(id = "cp_modal_descarga_ec",
                        style = "display:none; position:fixed; top:0; left:0; width:100%; height:100%;
             background:rgba(57,56,53,0.7); z-index:9999;
             justify-content:center; align-items:center;",
                        div(style = "background:white; border-radius:14px; padding:36px 40px;
                 min-width:420px; max-width:520px; text-align:center;
                 box-shadow:0 15px 40px rgba(64,23,18,0.35);",
                            tags$h4(style = "color:#802e25; font-weight:700; margin-bottom:20px;",
                                    "Generando archivo Excel"),
                            div(style = "background:#F0E8DC; border-radius:20px;
                     height:10px; overflow:hidden; margin-bottom:14px;",
                                div(id = "cp_barra_descarga_ec_fill",
                                    style = "height:10px; border-radius:20px; width:0%;
                         background:linear-gradient(90deg,#8B0000,#B22222);
                         transition:width 0.4s ease;")
                            ),
                            tags$p(id = "cp_descarga_ec_texto",
                                   style = "color:#7A6057; font-size:0.88rem; margin-bottom:20px;",
                                   "Conectando a Snowflake..."),
                            # Botón de descarga — oculto hasta que el archivo esté listo
                            shinyjs::hidden(
                              div(id = "cp_descarga_ec_btn_wrap",
                                  downloadButton("cp_btn_descargar_ec_final",
                                                 "Descargar archivo",
                                                 style = "background:linear-gradient(135deg,#802e25,#401712);
                                      color:white; font-weight:700; border:none;
                                      padding:10px 28px; border-radius:8px;
                                      font-size:0.95rem; margin-bottom:10px;")
                              )
                            ),
                            # Botón cerrar
                            shinyjs::hidden(
                              div(id = "cp_descarga_ec_cerrar_wrap",
                                  actionButton("cp_btn_cerrar_modal_ec", "Cerrar",
                                               style = "background:#e0d4c3; border:none; color:#393835;
                                    padding:8px 22px; border-radius:8px;
                                    font-weight:600; font-size:0.85rem; margin-top:6px;")
                              )
                            )
                        )
                    )
                ),
                
                # Barra de progreso (oculta inicialmente)
                shinyjs::hidden(
                  div(id = "cp_barra_progreso_panel",
                      class = "cp-barra-progreso-wrap",
                      div(style = "flex:1;",
                          div(style = "background:#F0E8DC;border-radius:20px;height:8px;overflow:hidden;",
                              div(id = "cp_barra_progreso_fill",
                                  style = "height:8px;border-radius:20px;width:0%;
                                     background:linear-gradient(90deg,#8B0000,#B22222);
                                     transition:width 0.3s ease;")
                          )
                      ),
                      textOutput("cp_progreso_texto", inline = TRUE) |>
                        tagAppendAttributes(
                          style = "font-size:0.82rem;color:#7A6057;white-space:nowrap;"
                        )
                  )
                ),
                
                # Tabla principal
                div(style = "padding: 16px 0 0 0;",
                    div(class = "cp-tabla-contenedor",
                        div(class = "cp-tabla-header",
                            div(class = "cp-tabla-header-titulo",
                                "Todos los movimientos con depósito"),
                            div(class = "cp-tabla-header-sub",
                                "Movimientos SPEI incluyen botón de consulta API")
                        ),
                        DT::dataTableOutput("cp_tabla_spei")
                    )
                ),
                
                # Panel de acciones
                div(class = "cp-acciones-panel",
                    div(class = "cp-acciones-info",
                        textOutput("cp_contador_cuenta", inline = TRUE)
                    )
                ),
                
            )
          ),
          
          # Contenedor para modal API
          div(id = "cp_modal_api_container")
          
  ) # fin tabItem
}

# ============================================================================
# SERVER — función que registra toda la lógica reactiva
# Llamar dentro de server(): cp_server_logic(input, output, session, cp_rv)
# ============================================================================
cp_server_logic <- function(input, output, session, cp_rv) {
  
  # --------------------------------------------------------------------------
  # Empresa activa según hoja seleccionada
  # --------------------------------------------------------------------------
  cp_empresa_activa <- reactive({
    req(input$cp_sel_hoja)
    cp_resolver_empresa(input$cp_sel_hoja)
  })
  
  # --------------------------------------------------------------------------
  # Datos de la hoja activa
  # --------------------------------------------------------------------------
  cp_datos_hoja <- reactive({
    req(input$cp_sel_hoja)
    hoja <- input$cp_sel_hoja
    
    # Si ya esta en cache, devolver sin ir a Snowflake
    cache_actual <- isolate(cp_rv$cache[[hoja]])
    if (!is.null(cache_actual)) {
      return(cache_actual)
    }
    
    # Primera vez: traer de Snowflake y guardar en cache
    df_sf <- tryCatch(
      mec_cargar_hoja_sf(hoja),
      error = function(e) NULL
    )
    if (!is.null(df_sf) && nrow(df_sf) > 0) {
      cp_rv$cache[[hoja]] <- df_sf
      return(df_sf)
    }
    return(NULL)
  })
  
  # --------------------------------------------------------------------------
  # Fechas disponibles para esta hoja — alimenta el dateRangeInput
  # --------------------------------------------------------------------------
  cp_fechas_disponibles <- reactive({
    req(input$cp_sel_hoja)
    hoja <- input$cp_sel_hoja
    
    tryCatch({
      conn <- crear_conexion_snowflake()
      on.exit(tryCatch(dbDisconnect(conn), error = function(e) NULL))
      dbSendUpdate(conn, "USE WAREHOUSE WH_ANALYTICS")
      dbSendUpdate(conn, "USE DATABASE DB_ANALYTICS")
      dbSendUpdate(conn, "USE SCHEMA SCH_CORE")
      
      cuenta_limpia <- trimws(gsub("[^0-9]", "", hoja))
      sufijo4 <- substr(cuenta_limpia, nchar(cuenta_limpia) - 3, nchar(cuenta_limpia))
      
      res <- DBI::dbGetQuery(conn, paste0(
        "SELECT MIN(FECHA_OPERACION) AS FECHA_MIN, MAX(FECHA_OPERACION) AS FECHA_MAX ",
        "FROM DB_ANALYTICS.SCH_CORE.MOVIMIENTOS_EC2 ",
        "WHERE RIGHT(CUENTA, 4) = '", sufijo4, "' ",
        "AND FECHA_OPERACION IS NOT NULL"
      ))
      
      list(
        min = if (!is.null(res$FECHA_MIN) && !is.na(res$FECHA_MIN)) as.Date(res$FECHA_MIN) else Sys.Date() - 30,
        max = if (!is.null(res$FECHA_MAX) && !is.na(res$FECHA_MAX)) as.Date(res$FECHA_MAX) else Sys.Date()
      )
    }, error = function(e) {
      list(min = Sys.Date() - 30, max = Sys.Date())
    })
  })
  
  # --------------------------------------------------------------------------
  # Renderizar dateRangeInput con fechas habilitadas según la hoja
  # --------------------------------------------------------------------------
  output$cp_rango_fechas_ui <- renderUI({
    fechas <- cp_fechas_disponibles()
    dateRangeInput("cp_rango_fechas",
                   label    = NULL,
                   start    = NA,
                   end      = NA,
                   min      = fechas$min,
                   max      = fechas$max,
                   format   = "dd/mm/yyyy",
                   language = "es",
                   separator = " al ",
                   width    = "280px")
  })
  
  # --------------------------------------------------------------------------
  # Pre-cargar clasificaciones existentes en el Excel al cambiar de hoja
  # --------------------------------------------------------------------------
  observeEvent(input$cp_sel_hoja, {
    req(input$cp_sel_hoja)
    hoja <- input$cp_sel_hoja
    
    # Obtener datos del cache (cp_datos_hoja lo llena si no existe)
    df <- cp_datos_hoja()
    if (is.null(df)) return()
    
    if (is.null(cp_rv$cambios[[hoja]])) cp_rv$cambios[[hoja]] <- list()
    
    col_clasif   <- if ("CLASIFICACION"   %in% names(df)) "CLASIFICACION"   else NULL
    col_efec     <- if ("EFECTIVIDAD"     %in% names(df)) "EFECTIVIDAD"     else NULL
    col_rec      <- if ("RECUPERACION"    %in% names(df)) "RECUPERACION"    else NULL
    col_estac    <- if ("ESTACIONAMIENTO" %in% names(df)) "ESTACIONAMIENTO" else NULL
    col_cines    <- if ("CINES"           %in% names(df)) "CINES"           else NULL
    col_filiales <- if ("FILIALES"        %in% names(df)) "FILIALES"        else NULL
    
    for (i in seq_len(nrow(df))) {
      fila_id <- as.character(df$.fila_id[i])
      if (!is.null(cp_rv$cambios[[hoja]][[fila_id]])) next
      
      if (!is.null(col_clasif)) {
        val_clasif <- as.character(df[[col_clasif]][i])
        if (!is.na(val_clasif) && val_clasif != "" && val_clasif != "NA") {
          cp_rv$cambios[[hoja]][[fila_id]] <- val_clasif
          next
        }
      }
      
      val_estac    <- suppressWarnings(as.numeric(if (!is.null(col_estac))    df[[col_estac]][i]    else NA))
      val_cines    <- suppressWarnings(as.numeric(if (!is.null(col_cines))    df[[col_cines]][i]    else NA))
      val_filiales <- if (!is.null(col_filiales)) as.character(df[[col_filiales]][i]) else NA_character_
      val_efec     <- suppressWarnings(as.numeric(if (!is.null(col_efec))     df[[col_efec]][i]     else NA))
      val_rec      <- suppressWarnings(as.numeric(if (!is.null(col_rec))      df[[col_rec]][i]      else NA))
      
      if (!is.na(val_estac) && val_estac > 0) {
        cp_rv$cambios[[hoja]][[fila_id]] <- "ESTACIONAMIENTO"
      } else if (!is.na(val_cines) && val_cines > 0) {
        cp_rv$cambios[[hoja]][[fila_id]] <- "CINES"
      } else if (!is.na(val_filiales) && val_filiales != "" && val_filiales != "NA") {
        cp_rv$cambios[[hoja]][[fila_id]] <- "FILIALES"
        if (is.null(cp_rv$cambios_filiales[[hoja]])) cp_rv$cambios_filiales[[hoja]] <- list()
        cp_rv$cambios_filiales[[hoja]][[fila_id]] <- val_filiales
      } else if (!is.na(val_efec) && val_efec > 0 && !is.na(val_rec) && val_rec > 0) {
        cp_rv$cambios[[hoja]][[fila_id]] <- "MANUAL"
        if (is.null(cp_rv$cambios_manual[[hoja]])) cp_rv$cambios_manual[[hoja]] <- list()
        cp_rv$cambios_manual[[hoja]][[fila_id]] <- list(
          efectividad  = val_efec,
          recuperacion = val_rec
        )
      } else if (!is.na(val_efec) && val_efec > 0) {
        cp_rv$cambios[[hoja]][[fila_id]] <- "EFECTIVIDAD"
      } else if (!is.na(val_rec) && val_rec > 0) {
        cp_rv$cambios[[hoja]][[fila_id]] <- "RECUPERACION"
      }
    }

    # Recalcular limites de fecha para la hoja activa
    if ("FECHA DE OPERACIÓN" %in% names(df)) {
      fechas_hoja <- suppressWarnings(
        as.Date(as.character(df[["FECHA DE OPERACIÓN"]]), format = "%Y-%m-%d")
      )
      if (all(is.na(fechas_hoja))) {
        fechas_hoja <- suppressWarnings(
          as.Date(as.character(df[["FECHA DE OPERACIÓN"]]), format = "%d/%m/%Y")
        )
      }
      fechas_hoja <- fechas_hoja[!is.na(fechas_hoja)]
      if (length(fechas_hoja) > 0) {
        updateDateRangeInput(session, "cp_rango_fechas",
                             min = min(fechas_hoja),
                             max = max(fechas_hoja))
      }
    }
  }, ignoreInit = FALSE)
  
  # --------------------------------------------------------------------------
  # Filtros de fecha
  # --------------------------------------------------------------------------
  observeEvent(input$cp_rango_fechas, {
    rango <- input$cp_rango_fechas
    if (!is.null(rango) && length(rango) == 2 && !is.na(rango[1]) && !is.na(rango[2])) {
      cp_rv$filtro_fecha_inicio <- rango[1]
      cp_rv$filtro_fecha_fin    <- rango[2]
    } else {
      cp_rv$filtro_fecha_inicio <- NULL
      cp_rv$filtro_fecha_fin    <- NULL
    }
  }, ignoreNULL = FALSE)
  
  # Capturar hora inicio y hora fin desde inputs HTML nativos
  observe({
    shinyjs::runjs("
    document.addEventListener('change', function(e) {
      if (e.target.id === 'cp_hora_ini' || e.target.id === 'cp_hora_fin') {
        Shiny.setInputValue('cp_horas_cambio', {
          ini: document.getElementById('cp_hora_ini').value,
          fin: document.getElementById('cp_hora_fin').value
        }, {priority: 'event'});
      }
    });
  ")
  })
  
  observeEvent(input$cp_horas_cambio, {
    h <- input$cp_horas_cambio
    cp_rv$filtro_hora_ini <- if (!is.null(h$ini) && h$ini != "") h$ini else "00:00"
    cp_rv$filtro_hora_fin <- if (!is.null(h$fin) && h$fin != "") h$fin else "23:59"
  })
  
  # --------------------------------------------------------------------------
  # Reactive: filas SPEI filtradas
  # --------------------------------------------------------------------------
  cp_spei_df <- reactive({
    df        <- cp_datos_hoja()
    df        <- cp_filtrar_spei(df)
    fecha_ini <- cp_rv$filtro_fecha_inicio
    fecha_fin <- cp_rv$filtro_fecha_fin
    hora_ini  <- cp_rv$filtro_hora_ini %cp||% "00:00"
    hora_fin  <- cp_rv$filtro_hora_fin %cp||% "23:59"
    
    if (!is.null(df) && nrow(df) > 0 && (!is.null(fecha_ini) || !is.null(fecha_fin))) {
      
      # Filtro por FECHA_OPERACION
      col_fecha_filtro <- if ("FECHA DE OPERACIÓN" %in% names(df)) "FECHA DE OPERACIÓN" else "FECHA"
      fechas_parsed <- suppressWarnings(
        as.Date(as.character(df[[col_fecha_filtro]]), format = "%Y-%m-%d")
      )
      if (all(is.na(fechas_parsed))) {
        fechas_parsed <- suppressWarnings(
          as.Date(as.character(df[[col_fecha_filtro]]), format = "%d/%m/%Y")
        )
      }
      mask <- rep(TRUE, nrow(df))
      if (!is.null(fecha_ini) && !is.na(fecha_ini)) mask <- mask & (fechas_parsed >= fecha_ini)
      if (!is.null(fecha_fin) && !is.na(fecha_fin)) mask <- mask & (fechas_parsed <= fecha_fin)
      df <- df[mask, ]
      
      # Filtro por hora de FECHA_CARGA (solo si el usuario cambió de 00:00-23:59)
      if (!is.null(df) && nrow(df) > 0 &&
          "FECHA_CARGA" %in% names(df) &&
          !(hora_ini == "00:00" && hora_fin == "23:59")) {
        
        horas_carga <- suppressWarnings(
          format(as.POSIXct(as.character(df$FECHA_CARGA), tz = "America/Mexico_City"), "%H:%M")
        )
        mask_hora <- !is.na(horas_carga) &
          horas_carga >= hora_ini &
          horas_carga <= hora_fin
        df <- df[mask_hora, ]
      }
    }
    df
  })
  
  output$cp_contador_spei_ui <- renderUI({
    n <- nrow(cp_spei_df())
    if (is.null(n)) n <- 0
    tags$span(
      style = "font-size:0.9rem;font-weight:700;color:white;letter-spacing:0.04em;",
      paste0(n, " movimientos SPEI")
    )
  })
  
  # --------------------------------------------------------------------------
  # Helper: clasificación actual de una fila
  # --------------------------------------------------------------------------
  cp_clasif_actual <- function(hoja, fila_id) {
    cambios_hoja <- isolate(cp_rv$cambios[[hoja]])
    if (!is.null(cambios_hoja) && !is.null(cambios_hoja[[as.character(fila_id)]])) {
      return(cambios_hoja[[as.character(fila_id)]])
    }
    return("")
  }
  
  # --------------------------------------------------------------------------
  # Contador de cambios
  # --------------------------------------------------------------------------
  output$cp_contador_cuenta <- renderText({
    hoja <- input$cp_sel_hoja
    req(hoja)
    df <- isolate(cp_rv$cache[[hoja]])
    if (is.null(df) || nrow(df) == 0) return("Sin datos en esta cuenta")
    
    total        <- nrow(df)
    clasificados <- sum(
      !is.na(df$CLASIFICACION) &
        df$CLASIFICACION != "" &
        df$CLASIFICACION != "NA",
      na.rm = TRUE
    )
    paste0(hoja, "  —  ", clasificados, " de ", total, " movimientos clasificados")
  })
  
  # --------------------------------------------------------------------------
  # Renderizado de tabla SPEI
  # --------------------------------------------------------------------------
  output$cp_tabla_spei <- DT::renderDataTable({
    df          <- cp_spei_df()
    hoja_actual <- input$cp_sel_hoja
    
    if (is.null(df) || nrow(df) == 0) {
      return(DT::datatable(
        data.frame(Mensaje = "No hay movimientos bancarios en esta hoja"),
        options = list(dom = "t", paging = FALSE), rownames = FALSE
      ))
    }
    
    col_desc <- if ("DESCRIPCIÓN DETALLADA" %in% names(df)) "DESCRIPCIÓN DETALLADA" else "DESCRIPCION DETALLADA"
    col_dep  <- if ("DEPÓSITOS" %in% names(df)) "DEPÓSITOS" else "DEPOSITOS"
    
    filas <- lapply(seq_len(nrow(df)), function(i) {
      fila_id      <- df$.fila_id[i]
      desc         <- as.character(df[[col_desc]][i])
      deposito_num <- suppressWarnings(as.numeric(df[[col_dep]][i]))
      deposito_fmt <- if (!is.na(deposito_num)) {
        paste0("$", format(round(deposito_num, 2), big.mark = ",", nsmall = 2))
      } else as.character(df[[col_dep]][i])
      deposito_raw <- as.character(df[[col_dep]][i])
      rfc          <- cp_extraer_rfc(desc)
      rfc_str      <- if (!is.na(rfc)) rfc else "—"
      fecha        <- if ("FECHA DE OPERACIÓN" %in% names(df)) as.character(df[["FECHA DE OPERACIÓN"]][i]) else "—"
      
      # Leer clasificación desde el df original
      # Leer clasificación: primero la memoria de sesión (cp_rv$cambios),
      # luego el Excel en disco como respaldo para el estado inicial.
      cambios_hoja_render <- isolate(cp_rv$cambios[[hoja_actual]])
      clasif_memoria       <- cambios_hoja_render[[as.character(fila_id)]]
      
      clasif <- if (!is.null(clasif_memoria) && clasif_memoria != "") {
        clasif_memoria
      } else {
        col_efec_df     <- if ("EFECTIVIDAD"     %in% names(df)) "EFECTIVIDAD"     else NULL
        col_rec_df      <- if ("RECUPERACION"    %in% names(df)) "RECUPERACION"    else NULL
        col_estac_df    <- if ("ESTACIONAMIENTO" %in% names(df)) "ESTACIONAMIENTO" else NULL
        col_cines_df    <- if ("CINES"           %in% names(df)) "CINES"           else NULL
        col_filiales_df <- if ("FILIALES"        %in% names(df)) "FILIALES"        else NULL
        
        val_efec_i     <- suppressWarnings(as.numeric(if (!is.null(col_efec_df))     df[[col_efec_df]][i]     else NA))
        val_rec_i      <- suppressWarnings(as.numeric(if (!is.null(col_rec_df))      df[[col_rec_df]][i]      else NA))
        val_estac_i    <- suppressWarnings(as.numeric(if (!is.null(col_estac_df))    df[[col_estac_df]][i]    else NA))
        val_cines_i    <- suppressWarnings(as.numeric(if (!is.null(col_cines_df))    df[[col_cines_df]][i]    else NA))
        val_filiales_i <- if (!is.null(col_filiales_df)) as.character(df[[col_filiales_df]][i]) else NA_character_
        
        if (!is.na(val_estac_i) && val_estac_i > 0) {
          "ESTACIONAMIENTO"
        } else if (!is.na(val_cines_i) && val_cines_i > 0) {
          "CINES"
        } else if (!is.na(val_filiales_i) && val_filiales_i != "" && val_filiales_i != "NA") {
          "FILIALES"
        } else if (!is.na(val_efec_i) && val_efec_i > 0 && !is.na(val_rec_i) && val_rec_i > 0) {
          "MANUAL"
        } else if (!is.na(val_efec_i) && val_efec_i > 0) {
          "EFECTIVIDAD"
        } else if (!is.na(val_rec_i) && val_rec_i > 0) {
          "RECUPERACION"
        } else {
          ""
        }
      }
      
      tiene_manual <- !is.null(isolate(cp_rv$cambios_manual[[hoja_actual]][[as.character(fila_id)]]))
      
      selected_vac      <- if (clasif == "")                "selected" else ""
      selected_efec     <- if (clasif == "EFECTIVIDAD")     "selected" else ""
      selected_rec      <- if (clasif == "RECUPERACION")    "selected" else ""
      selected_manual   <- if (clasif == "MANUAL")          "selected" else ""
      selected_estac    <- if (clasif == "ESTACIONAMIENTO") "selected" else ""
      selected_cines    <- if (clasif == "CINES")           "selected" else ""
      selected_filiales <- if (clasif == "FILIALES")        "selected" else ""
      
      cls_sel <- switch(clasif,
                        "EFECTIVIDAD"     = "cp-asig-efec",
                        "RECUPERACION"    = "cp-asig-rec",
                        "MANUAL"          = "cp-asig-manual",
                        "ESTACIONAMIENTO" = "cp-asig-estac",
                        "CINES"           = "cp-asig-cines",
                        "FILIALES"        = "cp-asig-filiales",
                        ""
      )
      
      indicador_manual <- if (clasif == "MANUAL" && tiene_manual) {
        paste0(
          '<span style="display:inline-block;width:8px;height:8px;',
          'background:#6B3FA0;border-radius:50%;margin-left:4px;vertical-align:middle;" ',
          'title="Montos configurados"></span>'
        )
      } else ""
      
      # Selector HTML — usa clase cp-sel-clasif y evento cp_clasif_change
      sel_html <- paste0(
        '<div style="display:flex;align-items:center;gap:6px;">',
        '<select class="cp-sel-clasif ', cls_sel, '" ',
        'onchange="Shiny.setInputValue(\'cp_clasif_change\',',
        '{hoja:\'', hoja_actual, '\',fila:', fila_id, ',valor:this.value},{priority:\'event\'});',
        'var m={\'EFECTIVIDAD\':\'cp-asig-efec\',\'RECUPERACION\':\'cp-asig-rec\',',
        '\'MANUAL\':\'cp-asig-manual\',\'ESTACIONAMIENTO\':\'cp-asig-estac\',',
        '\'CINES\':\'cp-asig-cines\',\'FILIALES\':\'cp-asig-filiales\'};',
        'this.className=\'cp-sel-clasif \'+(m[this.value]||\'\')+\'\';">',
        '<option value="" ',             selected_vac,      '>Sin clasificar</option>',
        '<option value="EFECTIVIDAD" ',  selected_efec,     '>Efectividad</option>',
        '<option value="RECUPERACION" ', selected_rec,      '>Recuperación</option>',
        '<option value="MANUAL" ',       selected_manual,   '>Manual</option>',
        '<option value="ESTACIONAMIENTO" ', selected_estac,    '>Estacionamiento</option>',
        '<option value="CINES" ',           selected_cines,    '>Cines</option>',
        '<option value="FILIALES" ',        selected_filiales, '>Filiales</option>',
        '</select>',
        indicador_manual,
        '</div>'
      )
      
      empresa_v  <- if ("EMPRESA"      %in% names(df)) as.character(df[["EMPRESA"]][i])      else "—"
      cuenta_v   <- if ("CUENTA"       %in% names(df)) as.character(df[["CUENTA"]][i])       else "—"
      saldo_num  <- suppressWarnings(as.numeric(if ("SALDO" %in% names(df)) df[["SALDO"]][i] else NA))
      saldo_v    <- if (!is.na(saldo_num)) paste0("$", format(round(saldo_num,2), big.mark=",", nsmall=2)) else "—"
      cliente_v  <- if ("CLIENTE"      %in% names(df)) as.character(df[["CLIENTE"]][i])      else "—"
      razon_v    <- if ("RAZON SOCIAL" %in% names(df)) as.character(df[["RAZON SOCIAL"]][i]) else "—"
      
      # Botón API — usa evento cp_abrir_modal_api
      btn_api <- if (!is.na(rfc)) {
        dep_esc     <- gsub("'", "\\'", deposito_raw)
        cliente_esc <- gsub("'", "\\'", cliente_v)
        paste0(
          '<button class="cp-btn-ver-api" onclick="Shiny.setInputValue(\'cp_abrir_modal_api\',',
          '{rfc:\'', rfc, '\',fila:', fila_id,
          ',hoja:\'', hoja_actual,
          '\',deposito:\'', dep_esc,
          '\',cliente:\'', cliente_esc, '\'},{priority:\'event\'});">',
          'Ver facturas</button>'
        )
      } else {
        '<span style="font-size:0.72rem;color:#7A6057;">Sin RFC</span>'
      }
      
      data.frame(
        Empresa        = empresa_v,
        Cuenta         = cuenta_v,
        Fecha          = fecha,
        Deposito       = deposito_fmt,
        Saldo          = saldo_v,
        Cliente        = cliente_v,
        `Razon Social` = razon_v,
        RFC            = paste0('<span class="cp-rfc-chip">', rfc_str, '</span>'),
        Movimientos    = desc,
        Clasificacion  = sel_html,
        Historial      = btn_api,
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
    })
    
    df_tabla <- dplyr::bind_rows(filas)
    
    DT::datatable(
      df_tabla,
      escape    = FALSE,
      rownames  = FALSE,
      selection = "none",
      options   = list(
        dom        = "frtip",
        pageLength = 20,
        scrollX    = TRUE,
        ordering   = TRUE,
        language   = list(
          search     = "Buscar:",
          lengthMenu = "Mostrar _MENU_ registros",
          info       = "Mostrando _START_ a _END_ de _TOTAL_ movimientos",
          paginate   = list(previous = "Anterior", `next` = "Siguiente")
        ),
        columnDefs = list(
          list(width = "140px", targets = 0),
          list(width = "90px",  targets = 1),
          list(width = "90px",  targets = 2),
          list(width = "110px", targets = 3),
          list(width = "110px", targets = 4),
          list(width = "140px", targets = 5),
          list(width = "160px", targets = 6),
          list(width = "110px", targets = 7),
          list(width = "380px", targets = 8),
          list(width = "150px", targets = 9,  orderable = FALSE),
          list(width = "110px", targets = 10, orderable = FALSE)
        )
      )
    )
  })
  
  # --------------------------------------------------------------------------
  # Recibir cambio de clasificación desde el selector
  # --------------------------------------------------------------------------
  observeEvent(input$cp_clasif_change, {
    req(input$cp_clasif_change)
    info    <- input$cp_clasif_change
    hoja    <- info$hoja
    fila_id <- as.character(info$fila)
    valor   <- info$valor
    
    if (is.null(cp_rv$cambios[[hoja]])) cp_rv$cambios[[hoja]] <- list()
    cp_rv$cambios[[hoja]][[fila_id]] <- valor
    
    # Actualizar cache de datos sin re-render
    if (!is.null(cp_rv$cache[[hoja]])) {
      idx <- which(cp_rv$cache[[hoja]]$.fila_id == as.integer(fila_id))
      if (length(idx) > 0) {
        cp_rv$cache[[hoja]]$CLASIFICACION[idx] <- valor
      }
    }
    
    # Actualizar clase CSS del selector via JS sin re-render de tabla
    nueva_clase <- switch(valor,
                          "EFECTIVIDAD"     = "cp-sel-clasif cp-asig-efec",
                          "RECUPERACION"    = "cp-sel-clasif cp-asig-rec",
                          "MANUAL"          = "cp-sel-clasif cp-asig-manual",
                          "ESTACIONAMIENTO" = "cp-sel-clasif cp-asig-estac",
                          "CINES"           = "cp-sel-clasif cp-asig-cines",
                          "FILIALES"        = "cp-sel-clasif cp-asig-filiales",
                          "cp-sel-clasif"
    )
    
    shinyjs::runjs(sprintf(
      "var sels = document.querySelectorAll('.cp-sel-clasif');
       sels.forEach(function(s) {
         if (s.getAttribute('onchange') && s.getAttribute('onchange').indexOf('fila:%s,') !== -1) {
           s.className = '%s';
         }
       });",
      fila_id, nueva_clase
    ))
    
    # ---- Modal MANUAL ----
    if (valor == "MANUAL") {
      cp_rv$manual_hoja    <- hoja
      cp_rv$manual_fila_id <- fila_id
      
      df_hoja <- cp_rv$cache[[hoja]]
      if (is.null(df_hoja)) {
        df_hoja <- tryCatch(mec_cargar_hoja_sf(hoja), error = function(e) NULL)
        if (!is.null(df_hoja)) cp_rv$cache[[hoja]] <- df_hoja
      }
      
      deposito_val <- 0
      if (!is.null(df_hoja) && nrow(df_hoja) > 0) {
        col_dep    <- if ("DEPÓSITOS" %in% names(df_hoja)) "DEPÓSITOS" else "DEPOSITOS"
        fila_datos <- df_hoja[df_hoja$.fila_id == as.integer(fila_id), ]
        if (nrow(fila_datos) > 0) {
          deposito_val <- suppressWarnings(as.numeric(fila_datos[[col_dep]][1]))
          if (is.na(deposito_val)) deposito_val <- 0
        }
      }
      cp_rv$manual_deposito_max <- deposito_val
      
      prev      <- cp_rv$cambios_manual[[hoja]][[fila_id]]
      prev_efec <- if (!is.null(prev)) prev$efectividad  else 0
      prev_rec  <- if (!is.null(prev)) prev$recuperacion else 0
      
      showModal(modalDialog(
        title = span(style = "color:white;font-weight:700;font-size:1.2rem;",
                     "Asignación manual de montos"),
        size  = "s",
        easyClose = FALSE,
        footer = tagList(
          actionButton("cp_manual_btn_guardar", "Guardar",
                       style = "background:#6B3FA0;color:white;border:none;
                                border-radius:6px;padding:8px 20px;font-weight:600;"),
          actionButton("cp_manual_btn_cancelar", "Cancelar",
                       style = "background:#F0E8DC;border:1px solid #D4C4B8;
                                border-radius:6px;padding:8px 20px;font-weight:600;")
        ),
        div(style = "padding:16px;",
            div(style = "background:#F0E8DC;border-left:4px solid #6B3FA0;
                          border-radius:0 8px 8px 0;padding:12px 16px;margin-bottom:16px;",
                div(style = "font-size:0.78rem;font-weight:600;color:#7A6057;
                              text-transform:uppercase;letter-spacing:0.05em;",
                    "Monto depositado (máximo a distribuir)"),
                div(style = "font-size:1.3rem;font-weight:700;color:#3D2B1F;",
                    paste0("$", format(cp_rv$manual_deposito_max, big.mark=",", nsmall=2)))
            ),
            div(style = "display:grid;grid-template-columns:1fr 1fr;gap:16px;padding:0 4px 8px;",
                div(
                  tags$label(style = "font-size:0.8rem;font-weight:600;text-transform:uppercase;
                                       letter-spacing:0.05em;color:#1A6B3C;margin-bottom:4px;display:block;",
                             "Efectividad ($)"),
                  numericInput("cp_manual_efec", NULL,
                               value = prev_efec, min = 0, step = 0.01, width = "100%")
                ),
                div(
                  tags$label(style = "font-size:0.8rem;font-weight:600;text-transform:uppercase;
                                       letter-spacing:0.05em;color:#B27700;margin-bottom:4px;display:block;",
                             "Recuperación ($)"),
                  numericInput("cp_manual_rec", NULL,
                               value = prev_rec, min = 0, step = 0.01, width = "100%")
                )
            ),
            uiOutput("cp_manual_suma_ui")
        )
      ))
    }
    
    # ---- Modal FILIALES ----
    if (valor == "FILIALES") {
      cp_rv$manual_filiales_hoja    <- hoja
      cp_rv$manual_filiales_fila_id <- fila_id
      prev_texto <- cp_rv$cambios_filiales[[hoja]][[fila_id]] %cp||% ""
      
      showModal(modalDialog(
        title     = span(style = "color:white;font-weight:700;font-size:1.2rem;",
                         "Asignación — Filiales"),
        size      = "s",
        easyClose = FALSE,
        footer    = tagList(
          actionButton("cp_filiales_btn_guardar", "Guardar",
                       style = "background:#1A5FA0;color:white;border:none;
                                border-radius:6px;padding:8px 20px;font-weight:600;"),
          actionButton("cp_filiales_btn_cancelar", "Cancelar",
                       style = "background:#F0E8DC;border:1px solid #D4C4B8;
                                border-radius:6px;padding:8px 20px;font-weight:600;")
        ),
        div(style = "padding:16px;",
            tags$label(style = "font-size:0.8rem;font-weight:600;text-transform:uppercase;
                                 letter-spacing:0.05em;color:#1A5FA0;margin-bottom:6px;display:block;",
                       "Texto para columna Filiales"),
            textInput("cp_filiales_texto", NULL,
                      value = prev_texto,
                      placeholder = "Ej: FEMSA Norte, Grupo 4, etc.",
                      width = "100%"),
            uiOutput("cp_filiales_contador_ui")
        )
      ))
    }
    
    # Obtener cuenta y número de movimiento de la fila
    df_hoja_actual <- cp_rv$cache[[hoja]]
    if (is.null(df_hoja_actual)) {
      df_hoja_actual <- tryCatch(mec_cargar_hoja_sf(hoja), error = function(e) NULL)
      if (!is.null(df_hoja_actual)) cp_rv$cache[[hoja]] <- df_hoja_actual
    }
    
    if (!is.null(df_hoja_actual) && nrow(df_hoja_actual) > 0) {
      fila_datos <- df_hoja_actual[df_hoja_actual$.fila_id == as.integer(fila_id), ]
      if (!is.null(fila_datos) && nrow(fila_datos) > 0) {
        cuenta_mov <- as.character(fila_datos$CUENTA[1])
        num_mov    <- suppressWarnings(as.integer(
          if ("MOVIMIENTO" %in% names(fila_datos)) fila_datos$MOVIMIENTO[1]
          else if ("NUM_MOVIMIENTO" %in% names(fila_datos)) fila_datos$NUM_MOVIMIENTO[1]
          else NA
        ))
        if (!is.null(num_mov) && length(num_mov) > 0 && !is.na(num_mov)) {
          cuenta_guardar <- cuenta_mov
          num_guardar    <- num_mov
          
          # Extraer el depósito exacto de la fila para EFECTIVIDAD/RECUPERACION simples
          col_dep      <- if ("DEPÓSITOS" %in% names(fila_datos)) "DEPÓSITOS" else
            if ("DEPOSITOS" %in% names(fila_datos)) "DEPOSITOS" else NULL
          deposito_val <- if (!is.null(col_dep))
            suppressWarnings(as.numeric(fila_datos[[col_dep]][1]))
          else NA_real_
          if (is.na(deposito_val)) deposito_val <- 0
          
          # Determinar qué montos/textos pasar según la clasificación
          monto_efec_guardar  <- NULL
          monto_rec_guardar   <- NULL
          texto_estac_guardar <- NULL
          texto_cines_guardar <- NULL
          
          if (valor == "EFECTIVIDAD") {
            monto_efec_guardar <- deposito_val
          } else if (valor == "RECUPERACION") {
            monto_rec_guardar <- deposito_val
          } else if (valor == "ESTACIONAMIENTO") {
            texto_estac_guardar <- "ESTACIONAMIENTO"
          } else if (valor == "CINES") {
            texto_cines_guardar <- "CINES"
          }
          # MANUAL y FILIALES tienen sus propios observers que ya guardan correctamente
          
          if (!(valor %in% c("MANUAL", "FILIALES"))) {
            valor_guardar       <- valor
            future_promise({
              mec_guardar_clasificacion(
                cuenta         = cuenta_guardar,
                num_movimiento = num_guardar,
                clasificacion  = valor_guardar,
                monto_efec     = monto_efec_guardar,
                monto_rec      = monto_rec_guardar,
                estacionamiento = texto_estac_guardar,
                cines           = texto_cines_guardar
              )
            }) %...!% (function(e) cat("Aviso SF clasif:", e$message, "\n"))
          }
        }
      }
    }
    
    invisible(NULL)
  })
  
  # --------------------------------------------------------------------------
  # UI reactiva: suma en tiempo real del modal manual
  # --------------------------------------------------------------------------
  output$cp_manual_suma_ui <- renderUI({
    efec <- suppressWarnings(as.numeric(input$cp_manual_efec %cp||% 0))
    rec  <- suppressWarnings(as.numeric(input$cp_manual_rec  %cp||% 0))
    efec <- if (is.na(efec) || length(efec) == 0) 0 else efec
    rec  <- if (is.na(rec)  || length(rec)  == 0) 0 else rec
    suma <- efec + rec
    max  <- if (is.null(cp_rv$manual_deposito_max) || is.na(cp_rv$manual_deposito_max)) 0 else cp_rv$manual_deposito_max
    cls  <- if (suma <= max) "cp-suma-ok" else "cp-suma-err"
    msg  <- if (suma <= max) "✓ Dentro del límite" else "✗ Excede el depósito"
    div(class = "cp-manual-suma-bar",
        span(paste0("Suma asignada: $", format(round(suma, 2), big.mark=",", nsmall=2))),
        span(class = cls, paste0(msg, " ($", format(round(max, 2), big.mark=",", nsmall=2), ")"))
    )
  })
  
  # --------------------------------------------------------------------------
  # Guardar montos manuales
  # --------------------------------------------------------------------------
  observeEvent(input$cp_manual_btn_guardar, {
    efec <- input$cp_manual_efec %cp||% 0
    rec  <- input$cp_manual_rec  %cp||% 0
    suma <- efec + rec
    max  <- cp_rv$manual_deposito_max
    
    if (suma > max) {
      showNotification(
        paste0("La suma ($", format(round(suma,2), big.mark=",", nsmall=2),
               ") excede el depósito ($",
               format(round(max,2), big.mark=",", nsmall=2), ")"),
        type = "error", duration = 4
      )
      return()
    }
    
    hoja    <- cp_rv$manual_hoja
    fila_id <- cp_rv$manual_fila_id
    if (is.null(cp_rv$cambios_manual[[hoja]])) cp_rv$cambios_manual[[hoja]] <- list()
    cp_rv$cambios_manual[[hoja]][[fila_id]] <- list(efectividad = efec, recuperacion = rec)
    cp_rv$cambios[[hoja]][[fila_id]] <- "MANUAL"
    
    # Guardar montos directamente en SF
    df_hoja_manual <- cp_rv$cache[[hoja]]
    if (is.null(df_hoja_manual)) {
      df_hoja_manual <- tryCatch(mec_cargar_hoja_sf(hoja), error = function(e) NULL)
      if (!is.null(df_hoja_manual)) cp_rv$cache[[hoja]] <- df_hoja_manual
    }
    if (!is.null(df_hoja_manual) && nrow(df_hoja_manual) > 0) {
      fila_datos_manual <- df_hoja_manual[df_hoja_manual$.fila_id == as.integer(fila_id), ]
      if (nrow(fila_datos_manual) > 0) {
        cuenta_manual <- as.character(fila_datos_manual$CUENTA[1])
        num_manual    <- suppressWarnings(as.integer(fila_datos_manual$MOVIMIENTO[1]))
        if (!is.na(num_manual)) {
          efec_guardar <- efec
          rec_guardar  <- rec
          future_promise({
            mec_guardar_clasificacion(cuenta_manual, num_manual, "MANUAL",
                                      monto_efec = efec_guardar,
                                      monto_rec  = rec_guardar)
          }) %...!% (function(e) cat("Aviso SF manual:", e$message, "\n"))
        }
      }
    }
    
    # Punto morado indicador via JS
    shinyjs::runjs(sprintf(
      "var sels = document.querySelectorAll('.cp-sel-clasif');
       sels.forEach(function(s) {
         if (s.getAttribute('onchange') && s.getAttribute('onchange').indexOf('fila:%s,') !== -1) {
           var container = s.parentNode;
           if (!container.querySelector('.cp-manual-punto')) {
             var punto = document.createElement('span');
             punto.className = 'cp-manual-punto';
             punto.title = 'Montos configurados';
             punto.style.cssText = 'display:inline-block;width:8px;height:8px;background:#6B3FA0;border-radius:50%%;margin-left:4px;vertical-align:middle;';
             container.appendChild(punto);
           }
         }
       });",
      fila_id
    ))
    
    removeModal()
    showNotification(
      paste0("Manual guardado — Efec: $", format(round(efec,2), big.mark=",", nsmall=2),
             "  /  Rec: $", format(round(rec,2), big.mark=",", nsmall=2)),
      type = "message", duration = 3
    )
  })
  
  observeEvent(input$cp_manual_btn_cancelar, {
    hoja    <- cp_rv$manual_hoja
    fila_id <- cp_rv$manual_fila_id
    if (is.null(cp_rv$cambios_manual[[hoja]][[fila_id]])) {
      cp_rv$cambios[[hoja]][[fila_id]] <- ""
    }
    removeModal()
  })
  
  # --------------------------------------------------------------------------
  # Guardar filiales
  # --------------------------------------------------------------------------
  observeEvent(input$cp_filiales_btn_guardar, {
    txt  <- trimws(input$cp_filiales_texto %cp||% "")
    if (nchar(txt) > 100) {
      showNotification("El texto excede los 100 caracteres permitidos.", type = "error", duration = 3)
      return()
    }
    hoja    <- cp_rv$manual_filiales_hoja
    fila_id <- cp_rv$manual_filiales_fila_id
    if (is.null(cp_rv$cambios_filiales[[hoja]])) cp_rv$cambios_filiales[[hoja]] <- list()
    cp_rv$cambios_filiales[[hoja]][[fila_id]] <- txt
    cp_rv$cambios[[hoja]][[fila_id]]          <- "FILIALES"
    
    # Guardar texto directamente en columna FILIALES de SF
    df_hoja_fil <- if (!is.null(cp_rv$cache[[hoja]])) {
      cp_rv$cache[[hoja]]
    } else {
      tryCatch(mec_cargar_hoja_sf(hoja), error = function(e) NULL)
    }
    if (!is.null(df_hoja_fil) && nrow(df_hoja_fil) > 0) {
      fila_datos_fil <- df_hoja_fil[df_hoja_fil$.fila_id == as.integer(fila_id), ]
      if (nrow(fila_datos_fil) > 0) {
        cuenta_fil <- as.character(fila_datos_fil$CUENTA[1])
        num_fil    <- suppressWarnings(as.integer(fila_datos_fil$MOVIMIENTO[1]))
        if (!is.na(num_fil)) {
          txt_guardar <- txt
          future_promise({
            mec_guardar_clasificacion(cuenta_fil, num_fil, "FILIALES",
                                      texto_filiales = txt_guardar)
          }) %...!% (function(e) cat("Aviso SF filiales:", e$message, "\n"))
        }
      }
    }
    
    removeModal()
    showNotification(paste0("Filiales guardado: \"", txt, "\""), type = "message", duration = 3)
  })
  
  observeEvent(input$cp_filiales_btn_cancelar, {
    hoja    <- cp_rv$manual_filiales_hoja
    fila_id <- cp_rv$manual_filiales_fila_id
    if (is.null(cp_rv$cambios_filiales[[hoja]][[fila_id]])) {
      cp_rv$cambios[[hoja]][[fila_id]] <- ""
    }
    removeModal()
  })
  
  output$cp_filiales_contador_ui <- renderUI({
    txt   <- input$cp_filiales_texto %cp||% ""
    n     <- nchar(txt)
    color <- if (n > 100) "#B22222" else "#7A6057"
    div(style = paste0("font-size:0.78rem;color:", color, ";text-align:right;margin-top:4px;"),
        paste0(n, " / 100 caracteres"))
  })
  
  # --------------------------------------------------------------------------
  # Clasificación automática — botón
  # --------------------------------------------------------------------------
  observeEvent(input$cp_btn_clasificar_todo, {
    req(cp_rv$archivo_ruta, cp_rv$hojas)
    showModal(modalDialog(
      title     = "¿Confirmar clasificación automática?",
      size      = "s",
      easyClose = FALSE,
      p("Este proceso consultará la API para todas las hojas del archivo."),
      p(strong("Podría tardar unos minutos"), " dependiendo del número de movimientos SPEI."),
      p("Durante el proceso no podrás interactuar con la aplicación."),
      footer = tagList(
        actionButton("cp_confirmar_clasificar_todo", "Sí, continuar",
                     style = "background:#B22222;color:white;border:none;
                              border-radius:6px;padding:8px 20px;font-weight:600;"),
        modalButton("Cancelar")
      )
    ))
  })
  
  observeEvent(input$cp_confirmar_clasificar_todo, {
    removeModal()
    req(cp_rv$hojas)
    
    shinyjs::disable("cp_btn_clasificar_todo")
    shinyjs::runjs("document.body.style.pointerEvents='none';document.body.style.opacity='0.7';")
    
    # Recopilar filas pendientes de todas las hojas
    filas_pendientes <- list()
    
    for (hoja_iter in cp_rv$hojas) {
      df_iter <- if (!is.null(cp_rv$cache[[hoja_iter]])) {
        cp_rv$cache[[hoja_iter]]
      } else {
        tryCatch(mec_cargar_hoja_sf(hoja_iter), error = function(e) NULL)
      }
      if (!is.null(df_iter) && is.null(cp_rv$cache[[hoja_iter]])) {
        cp_rv$cache[[hoja_iter]] <- df_iter
      }
      df_spei_iter <- cp_filtrar_spei(df_iter)
      if (is.null(df_spei_iter) || nrow(df_spei_iter) == 0) next
      
      fecha_ini_cl <- cp_rv$filtro_fecha_inicio
      fecha_fin_cl <- cp_rv$filtro_fecha_fin
      if (!is.null(fecha_ini_cl) || !is.null(fecha_fin_cl)) {
        col_fecha_cl <- if ("FECHA DE OPERACIÓN" %in% names(df_spei_iter)) "FECHA DE OPERACIÓN" else "FECHA"
        fechas_cl <- suppressWarnings(
          as.Date(as.character(df_spei_iter[[col_fecha_cl]]), format = "%Y-%m-%d")
        )
        if (all(is.na(fechas_cl))) {
          fechas_cl <- suppressWarnings(
            as.Date(as.character(df_spei_iter[[col_fecha_cl]]), format = "%d/%m/%Y")
          )
        }
        mask_cl <- rep(TRUE, nrow(df_spei_iter))
        if (!is.null(fecha_ini_cl) && !is.na(fecha_ini_cl)) mask_cl <- mask_cl & (fechas_cl >= fecha_ini_cl)
        if (!is.null(fecha_fin_cl) && !is.na(fecha_fin_cl)) mask_cl <- mask_cl & (fechas_cl <= fecha_fin_cl)
        df_spei_iter <- df_spei_iter[mask_cl, ]
      }
      if (nrow(df_spei_iter) == 0) next  
      empresa_iter <- cp_resolver_empresa(hoja_iter)
      col_desc     <- if ("DESCRIPCIÓN DETALLADA" %in% names(df_spei_iter)) "DESCRIPCIÓN DETALLADA" else "DESCRIPCION DETALLADA"
      col_cliente  <- if ("CLIENTE" %in% names(df_spei_iter)) "CLIENTE" else NULL
      
      for (i in seq_len(nrow(df_spei_iter))) {
        fila_id          <- as.character(df_spei_iter$.fila_id[i])
        clasif_existente <- cp_rv$cambios[[hoja_iter]][[fila_id]]
        if (!is.null(clasif_existente) && clasif_existente != "") next
        
        rfc <- cp_extraer_rfc(as.character(df_spei_iter[[col_desc]][i]))
        if (is.na(rfc)) next
        
        cliente <- if (!is.null(col_cliente)) as.character(df_spei_iter[[col_cliente]][i]) else NULL
        
        filas_pendientes[[length(filas_pendientes) + 1]] <- list(
          fila_id = fila_id,
          hoja    = hoja_iter,
          empresa = empresa_iter,
          rfc     = rfc,
          cliente = cliente
        )
      }
    }
    
    total <- length(filas_pendientes)
    if (total == 0) {
      shinyjs::enable("cp_btn_clasificar_todo")
      shinyjs::runjs("document.body.style.pointerEvents='';document.body.style.opacity='1';")
      showNotification("Todas las filas con RFC ya están clasificadas.", type = "message", duration = 3)
      return()
    }
    
    cp_rv$progreso_actual   <- 0
    cp_rv$progreso_total    <- total
    cp_rv$clasificando_todo <- TRUE
    shinyjs::show("cp_barra_progreso_panel")
    
    tamano_lote <- 5
    lotes <- split(filas_pendientes, ceiling(seq_along(filas_pendientes) / tamano_lote))
    
    cp_procesar_lote <- function(idx_lote) {
      if (idx_lote > length(lotes)) {
        cp_rv$clasificando_todo <- FALSE
        shinyjs::hide("cp_barra_progreso_panel")
        shinyjs::enable("cp_btn_clasificar_todo")
        shinyjs::runjs("document.body.style.pointerEvents='';document.body.style.opacity='1';")
        showNotification(
          paste0("Clasificación completada — ", cp_rv$progreso_actual, " filas procesadas."),
          type = "message", duration = 4
        )
        return()
      }
      
      lote <- lotes[[idx_lote]]
      
      future_promise({
        httr::set_config(httr::config(http_version = 1))
        lapply(lote, function(item) {
          resultado <- cp_buscar_facturas_rfc(
            rfc              = item$rfc,
            empresa_asignada = item$empresa,
            nombre_cliente   = item$cliente
          )
          list(fila_id = item$fila_id, hoja = item$hoja, resultado = resultado)
        })
      }) %...>% (function(resultados_lote) {
        for (item_res in resultados_lote) {
          fila_id   <- item_res$fila_id
          hoja_res  <- item_res$hoja
          resultado <- item_res$resultado
          
          if (!is.null(resultado)) {
            if (nrow(resultado) == 0) {
              if (is.null(cp_rv$cambios[[hoja_res]])) cp_rv$cambios[[hoja_res]] <- list()
              if (is.null(cp_rv$cambios[[hoja_res]][[fila_id]]) || cp_rv$cambios[[hoja_res]][[fila_id]] == "") {
                cp_rv$cambios[[hoja_res]][[fila_id]] <- "EFECTIVIDAD"
              }
            } else {
              col_estatus  <- if ("estatus_documento" %in% names(resultado)) "estatus_documento" else NULL
              n_por_cobrar <- if (!is.null(col_estatus)) {
                sum(grepl("cuenta por cobrar", tolower(as.character(resultado[[col_estatus]])),
                          fixed = FALSE), na.rm = TRUE)
              } else 0
              
              if (n_por_cobrar == 0) {
                if (is.null(cp_rv$cambios[[hoja_res]])) cp_rv$cambios[[hoja_res]] <- list()
                if (is.null(cp_rv$cambios[[hoja_res]][[fila_id]]) || cp_rv$cambios[[hoja_res]][[fila_id]] == "") {
                  cp_rv$cambios[[hoja_res]][[fila_id]] <- "EFECTIVIDAD"
                }
              }
            }
          }
          cp_rv$progreso_actual <- cp_rv$progreso_actual + 1
        }
        
        pct <- round((cp_rv$progreso_actual / cp_rv$progreso_total) * 100)
        shinyjs::runjs(paste0(
          "document.getElementById('cp_barra_progreso_fill').style.width='", pct, "%';"
        ))
        cp_procesar_lote(idx_lote + 1)
        
      }) %...!% (function(err) {
        cp_rv$progreso_actual <- cp_rv$progreso_actual + length(lote)
        cp_procesar_lote(idx_lote + 1)
      })
    }
    
    cp_procesar_lote(1)
  })
  
  # --------------------------------------------------------------------------
  # Abrir modal historial API (desde botón "Ver facturas")
  # --------------------------------------------------------------------------
  observeEvent(input$cp_abrir_modal_api, {
    req(input$cp_abrir_modal_api)
    info <- input$cp_abrir_modal_api
    
    cp_rv$modal_rfc      <- info$rfc
    cp_rv$modal_empresa  <- cp_empresa_activa()
    cp_rv$modal_deposito <- info$deposito
    cp_rv$modal_fila_id  <- info$fila
    cp_rv$modal_hoja     <- info$hoja
    cp_rv$modal_cliente  <- info$cliente %cp||% NULL
    cp_rv$api_resultado  <- NULL
    cp_rv$api_cargando   <- TRUE
    
    showModal(modalDialog(
      title     = uiOutput("cp_modal_titulo"),
      size      = "l",
      easyClose = TRUE,
      footer    = tagList(
        div(class = "cp-clasif-modal-opt",
            actionButton("cp_modal_btn_efec", "Marcar como Efectividad",
                         class = "cp-clasif-btn cp-clasif-btn-efec"),
            actionButton("cp_modal_btn_rec",  "Marcar como Recuperación",
                         class = "cp-clasif-btn cp-clasif-btn-rec")
        ),
        modalButton("Cerrar")
      ),
      div(class = "cp-api-meta-box",
          fluidRow(
            column(4,
                   div(class = "cp-meta-label", "RFC del cliente"),
                   div(class = "cp-meta-val",   uiOutput("cp_modal_rfc_ui"))
            ),
            column(4,
                   div(class = "cp-meta-label", "Plaza consultada"),
                   div(class = "cp-meta-val",   uiOutput("cp_modal_empresa_ui"))
            ),
            column(4,
                   div(class = "cp-meta-label", "Monto depósito"),
                   div(class = "cp-meta-val",   uiOutput("cp_modal_deposito_ui"))
            )
          )
      ),
      div(style = "padding: 0 16px 16px;",
          uiOutput("cp_modal_contenido_api")
      )
    ))
    
    # Consulta asíncrona
    rfc_c     <- cp_rv$modal_rfc
    empresa_c <- cp_resolver_empresa(cp_rv$modal_hoja)
    cliente_c <- cp_rv$modal_cliente
    
    future_promise({
      httr::set_config(httr::config(http_version = 1))
      cp_buscar_facturas_rfc(
        rfc              = rfc_c,
        empresa_asignada = empresa_c,
        nombre_cliente   = cliente_c,
        plazas_override  = NULL
      ) 
    }) %...>% (function(resultado) {
      cp_rv$api_resultado <- resultado
      cp_rv$api_cargando  <- FALSE
      
      # Clasificación automática si no tiene facturas por cobrar
      if (!is.null(resultado) && nrow(resultado) > 0) {
        col_estatus  <- if ("estatus_documento" %in% names(resultado)) "estatus_documento" else NULL
        n_por_cobrar <- if (!is.null(col_estatus)) {
          sum(grepl("cuenta por cobrar", tolower(as.character(resultado[[col_estatus]])),
                    fixed = FALSE), na.rm = TRUE)
        } else 0
        
        if (n_por_cobrar == 0) {
          hoja    <- cp_rv$modal_hoja
          fila_id <- as.character(cp_rv$modal_fila_id)
          if (!is.null(hoja) && !is.null(fila_id)) {
            if (is.null(cp_rv$cambios[[hoja]])) cp_rv$cambios[[hoja]] <- list()
            if (is.null(cp_rv$cambios[[hoja]][[fila_id]]) || cp_rv$cambios[[hoja]][[fila_id]] == "") {
              cp_rv$cambios[[hoja]][[fila_id]] <- "EFECTIVIDAD"
            }
          }
        }
      }
    }) %...!% (function(err) {
      cp_rv$api_resultado <- data.frame()
      cp_rv$api_cargando  <- FALSE
    })
  })
  
  # --------------------------------------------------------------------------
  # Búsqueda manual desde barra de filtros
  # --------------------------------------------------------------------------
  observeEvent(input$cp_btn_busqueda_manual, {
    req(input$cp_busqueda_manual)
    busqueda <- trimws(input$cp_busqueda_manual)
    if (nchar(busqueda) == 0) return()
    
    cp_rv$modal_rfc      <- busqueda
    cp_rv$modal_empresa  <- "Búsqueda manual"
    cp_rv$modal_deposito <- NULL
    cp_rv$modal_cliente  <- NULL
    cp_rv$api_resultado  <- NULL
    cp_rv$api_cargando   <- TRUE
    
    # meses_consulta eliminado — ya no se usa
    
    showModal(modalDialog(
      title     = uiOutput("cp_modal_titulo"),
      size      = "l",
      easyClose = TRUE,
      footer    = modalButton("Cerrar"),
      div(class = "cp-api-meta-box",
          fluidRow(
            column(6,
                   div(class = "cp-meta-label", "Búsqueda"),
                   div(class = "cp-meta-val",   busqueda)
            ),
            column(6,
                   div(class = "cp-meta-label", "Historial consultado"),
                   div(class = "cp-meta-val",   "Histórico completo — Cuenta por cobrar")
            )
          )
      ),
      div(style = "padding: 0 16px 16px;",
          uiOutput("cp_modal_contenido_api")
      )
    ))
    
    plazas_sel      <- input$cp_plazas_manual
    plazas_consulta <- if (is.null(plazas_sel) || "TODAS" %in% plazas_sel || length(plazas_sel) == 0) {
      CP_COMPANY_LIST
    } else {
      plazas_sel
    }
    
    future_promise({
      httr::set_config(httr::config(http_version = 1))
      cp_buscar_facturas_rfc(
        rfc              = busqueda,
        empresa_asignada = "COMPANY_LIST",
        nombre_cliente   = NULL,
        plazas_override  = plazas_consulta
      )
    }) %...>% (function(resultado) {
      cp_rv$api_resultado <- resultado
      cp_rv$api_cargando  <- FALSE
    }) %...!% (function(err) {
      cp_rv$api_resultado <- data.frame()
      cp_rv$api_cargando  <- FALSE
    })
  })
  
  # --------------------------------------------------------------------------
  # Outputs del modal
  # --------------------------------------------------------------------------
  output$cp_modal_titulo <- renderUI({
    span("Historial de facturas CFDI — ",
         tags$span(cp_rv$modal_rfc %cp||% "", class = "cp-rfc-chip"))
  })
  
  output$cp_modal_rfc_ui <- renderUI({
    tags$span(cp_rv$modal_rfc %cp||% "—", class = "cp-rfc-chip")
  })
  
  output$cp_modal_empresa_ui <- renderUI({
    cp_rv$modal_empresa %cp||% "—"
  })
  
  output$cp_modal_deposito_ui <- renderUI({
    dep <- cp_rv$modal_deposito
    if (!is.null(dep) && dep != "") {
      paste0("$", format(as.numeric(dep), big.mark = ",", nsmall = 2))
    } else "—"
  })
  
  output$cp_progreso_texto <- renderText({
    if (cp_rv$progreso_total == 0) return("")
    paste0("Procesando ", cp_rv$progreso_actual, " de ", cp_rv$progreso_total, " clientes...")
  })
  
  output$cp_modal_contenido_api <- renderUI({
    if (cp_rv$api_cargando) {
      div(class = "cp-spinner-api",
          div(class = "cp-spinner-dot"),
          div(class = "cp-spinner-dot"),
          div(class = "cp-spinner-dot"),
          br(), br(),
          "Consultando plazas. Esto puede tomar unos segundos..."
      )
    } else {
      df <- cp_rv$api_resultado
      if (is.null(df) || nrow(df) == 0) {
        div(class = "cp-no-facturas-msg",
            tags$i(class = "fa fa-search"), br(), br(),
            "No se encontraron facturas para este cliente.",
            br(),
            tags$span(style = "font-size:0.82rem;color:#7A6057;",
                      "Pruebe con más meses o use el buscador manual.")
        )
      } else {
        cols_mostrar <- c("empresa","fecha","arrendatario","sucursal",
                          "rfc_emisor","rfc_receptor","total_moneda_base",
                          "estatus_documento","folio_fiscal","folio","pdf",
                          "descripcion_partida")
        cols_ok  <- intersect(cols_mostrar, names(df))
        df_show  <- as.data.frame(df)[, cols_ok, drop = FALSE]
        
        get_val <- function(fila, col) {
          v <- if (col %in% names(fila)) as.character(fila[[col]]) else ""
          if (is.na(v) || v == "NA" || v == "NULL") "" else v
        }
        
        tarjetas <- lapply(seq_len(nrow(df_show)), function(i) {
          fila       <- df_show[i, ]
          empresa_v  <- get_val(fila, "empresa")
          fecha_v    <- get_val(fila, "fecha")
          arrend_v   <- get_val(fila, "arrendatario")
          sucursal_v <- get_val(fila, "sucursal")
          rfc_em_v   <- get_val(fila, "rfc_emisor")
          rfc_rec_v  <- get_val(fila, "rfc_receptor")
          total_v    <- get_val(fila, "total_moneda_base")
          estatus_v  <- get_val(fila, "estatus_documento")
          folio_v    <- get_val(fila, "folio_fiscal")
          pdf_v      <- get_val(fila, "pdf")
          partida_v    <- get_val(fila, "descripcion_partida") 
          
          if (grepl("cancelad|pagado", tolower(estatus_v))) return(NULL)
          
          total_fmt   <- tryCatch(
            paste0("$", format(round(as.numeric(total_v), 2), big.mark=",", nsmall=2)),
            error = function(e) total_v
          )
          folio_corto <- folio_v  # mostrar UUID completo, sin truncar
          cls_est     <- if (grepl("pagado|vigente", tolower(estatus_v))) "cp-estatus-pagado" else "cp-estatus-otro"
          pdf_btn     <- if (nchar(pdf_v) > 5 && pdf_v != "None") {
            tags$a(href = pdf_v, target = "_blank", class = "cp-factura-pdf-link", "Ver PDF")
          } else {
            tags$span(style = "font-size:0.75rem;color:#7A6057;font-style:italic;", "PDF no disponible")
          }
          
          div(class = "cp-factura-card",
              style = "grid-template-columns: 1fr 1fr auto; grid-template-rows: auto auto;",
              div(
                div(class = "cp-factura-fecha",  paste0(empresa_v, " — ", fecha_v)),
                div(class = "cp-factura-folio",
                    if ("folio" %in% names(fila) && !is.na(fila[["folio"]]) && fila[["folio"]] != "")
                      div(style = "font-size:0.85rem; color:#7A6057; font-weight:600;",
                          paste0("Folio: ", fila[["folio"]]))
                    else NULL,
                    div(style = "font-size:0.78rem; color:#B0A090; font-family:monospace; word-break:break-all;",
                        folio_corto)
                ),
                div(style = "font-size:1.05rem;color:#7A6057;margin-top:2px;",
                    paste0(arrend_v, if (nchar(sucursal_v) > 0) paste0(" / ", sucursal_v) else ""))
              ),
              div(
                div(style = "font-size:1rem;color:#7A6057;",         paste0("Emisor: ",   rfc_em_v)),
                div(style = "font-size:1rem;color:#7A6057;margin-bottom:4px;", paste0("Receptor: ", rfc_rec_v)),
                div(class = "cp-factura-total", total_fmt),
                tags$span(class = paste("cp-factura-estatus", cls_est), estatus_v)
              ),
              div(pdf_btn),
              # ── NUEVO: descripción de partidas — fila completa debajo ──
              if (nchar(partida_v) > 0) {
                div(
                  style = "grid-column: 1 / -1; margin-top: 6px; padding: 8px 12px;
                 background: #F8F5F2; border-left: 3px solid #B22222;
                 border-radius: 0 6px 6px 0; font-size: 0.82rem; color: #3D2B1F;
                 line-height: 1.5;",
                  tags$span(style = "font-weight:700; color:#B22222; margin-right:6px;",
                            "Concepto:"),
                  partida_v
                )
              } else NULL
          )
        })
        
        tarjetas_validas <- Filter(Negate(is.null), tarjetas)
        if (length(tarjetas_validas) == 0) {
          div(class = "cp-no-facturas-msg",
              "No se encontraron facturas pendientes de pago para este RFC en el periodo consultado.")
        } else {
          do.call(tagList, tarjetas_validas)
        }
      }
    }
  })
  
  # --------------------------------------------------------------------------
  # Clasificar desde modal
  # --------------------------------------------------------------------------
  cp_clasificar_desde_modal <- function(valor) {
    hoja    <- cp_rv$modal_hoja
    fila_id <- as.character(cp_rv$modal_fila_id)
    if (is.null(hoja) || is.null(fila_id)) return()
    if (is.null(cp_rv$cambios[[hoja]])) cp_rv$cambios[[hoja]] <- list()
    cp_rv$cambios[[hoja]][[fila_id]] <- valor
    removeModal()
    showNotification(paste0("Marcado como ", valor), type = "message", duration = 2)
  }
  
  observeEvent(input$cp_modal_btn_efec, { cp_clasificar_desde_modal("EFECTIVIDAD") })
  observeEvent(input$cp_modal_btn_rec,  { cp_clasificar_desde_modal("RECUPERACION") })
  
  # ----------------------------------------------------------------
  # DESCARGA EC — generar Excel desde Snowflake con filtro de fechas
  # ----------------------------------------------------------------
  cp_ec_listo <- reactiveVal(NULL)
  
  output$cp_btn_descargar_ec_final <- downloadHandler(
    filename = function() {
      paste0("EC_", format(Sys.Date(), "%Y%m%d"), ".xlsx")
    },
    content = function(file) {
      ruta <- isolate(cp_ec_listo())
      if (!is.null(ruta) && file.exists(ruta)) {
        file.copy(ruta, file)
      }
    },
    contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
  )
  
  observeEvent(input$cp_btn_iniciar_descarga_ec, {
    shinyjs::disable("cp_btn_iniciar_descarga_ec")
    cp_ec_listo(NULL)
    
    # Mostrar modal y resetear barra
    shinyjs::runjs("
    document.getElementById('cp_modal_descarga_ec').style.display = 'flex';
    document.getElementById('cp_barra_descarga_ec_fill').style.width = '10%';
    document.getElementById('cp_descarga_ec_texto').textContent = 'Conectando a Snowflake...';
  ")
    shinyjs::hide("cp_descarga_ec_btn_wrap")
    shinyjs::hide("cp_descarga_ec_cerrar_wrap")
    
    # Leer filtro de fechas activo
    fecha_ini <- tryCatch(isolate(input$cp_rango_fechas[1]), error = function(e) NA)
    fecha_fin <- tryCatch(isolate(input$cp_rango_fechas[2]), error = function(e) NA)
    archivo_tmp <- tempfile(fileext = ".xlsx")
    
    shinyjs::runjs("
    document.getElementById('cp_barra_descarga_ec_fill').style.width = '35%';
    document.getElementById('cp_descarga_ec_texto').textContent = 'Consultando movimientos...';
  ")
    
    ok <- tryCatch({
      dec_generar_excel_ec(
        fecha_inicio    = if (!is.null(fecha_ini) && !is.na(fecha_ini)) as.Date(fecha_ini) else NULL,
        fecha_fin       = if (!is.null(fecha_fin) && !is.na(fecha_fin)) as.Date(fecha_fin) else NULL,
        archivo_destino = archivo_tmp
      )
      TRUE
    }, error = function(e) {
      cat("Error descarga EC:", e$message, "\n")
      FALSE
    })
    
    if (ok && file.exists(archivo_tmp)) {
      cp_ec_listo(archivo_tmp)
      shinyjs::runjs("
      document.getElementById('cp_barra_descarga_ec_fill').style.width = '100%';
      document.getElementById('cp_descarga_ec_texto').textContent = 'Archivo listo.';
    ")
      shinyjs::show("cp_descarga_ec_btn_wrap")
      shinyjs::show("cp_descarga_ec_cerrar_wrap")
    } else {
      shinyjs::runjs("
      document.getElementById('cp_barra_descarga_ec_fill').style.width = '100%';
      document.getElementById('cp_descarga_ec_texto').textContent = 'Error al generar el archivo.';
    ")
      shinyjs::show("cp_descarga_ec_cerrar_wrap")
    }
    
    shinyjs::enable("cp_btn_iniciar_descarga_ec")
  })
  
  observeEvent(input$cp_btn_cerrar_modal_ec, {
    shinyjs::runjs("
    document.getElementById('cp_modal_descarga_ec').style.display = 'none';
    document.getElementById('cp_barra_descarga_ec_fill').style.width = '0%';
  ")
    cp_ec_listo(NULL)
  })
  
}

# ============================================================================
# FIN DE ClasificacionPagos.R
# ============================================================================
cat("✅ ClasificacionPagos.R cargado correctamente\n")