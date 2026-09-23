library(openxlsx)
library(RJDBC)
library(DBI)
library(dplyr)
library(stringr)

# Convertir PEM a PrivateKey para JWT
convertir_pem_a_privatekey <- function(ruta_pem) {
  tryCatch({
    lineas <- readLines(ruta_pem, warn = FALSE)
    lineas <- lineas[!grepl("^-----", lineas)]
    clave_base64 <- paste(lineas, collapse = "")
    
    decoder <- .jcall("java/util/Base64", "Ljava/util/Base64$Decoder;", "getDecoder")
    bytes_clave <- .jcall(decoder, "[B", "decode", clave_base64)
    
    key_spec <- .jnew("java/security/spec/PKCS8EncodedKeySpec", bytes_clave)
    key_factory <- .jcall("java/security/KeyFactory", "Ljava/security/KeyFactory;", "getInstance", "RSA")
    
    private_key <- .jcall(key_factory, "Ljava/security/PrivateKey;", "generatePrivate", .jcast(key_spec, "java/security/spec/KeySpec"))
    
    return(private_key)
  }, error = function(e) {
    stop(paste("Error al convertir la clave privada:", e$message))
  })
}

# Conectar a Snowflake con JWT
crear_conexion_snowflake_almena <- function(usuario = Sys.getenv("SNOWFLAKE_USER"), ruta_pem = Sys.getenv("SNOWFLAKE_PRIVATE_KEY_PATH")) {
  tryCatch({
    ruta_archivo_jar <- file.path(getwd(), "snowflake-jdbc-3.25.1.jar")
    ruta_pem_absoluta <- file.path(getwd(), ruta_pem)
    
    driver <- RJDBC::JDBC("net.snowflake.client.jdbc.SnowflakeDriver", 
                          classPath = ruta_archivo_jar, 
                          identifier.quote = "`")
    
    private_key <- convertir_pem_a_privatekey(ruta_pem_absoluta)
    
    url_conexion <- paste0("jdbc:snowflake://", Sys.getenv("SNOWFLAKE_ACCOUNT"), ".snowflakecomputing.com/")
    
    propiedades <- .jnew("java.util.Properties")
    .jcall(propiedades, "Ljava/lang/Object;", "setProperty", "user", usuario)
    .jcall(propiedades, "Ljava/lang/Object;", "setProperty", "warehouse", "WH_ANALYTICS")
    .jcall(propiedades, "Ljava/lang/Object;", "setProperty", "db", "DB_ANALYTICS")
    .jcall(propiedades, "Ljava/lang/Object;", "setProperty", "schema", "SCH_CORE")
    .jcall(propiedades, "Ljava/lang/Object;", "setProperty", "role", "ADMIN_ALMENA")
    .jcall(propiedades, "Ljava/lang/Object;", "setProperty", "authenticator", "SNOWFLAKE_JWT")
    .jcall(propiedades, "Ljava/lang/Object;", "setProperty", "tracing", "OFF")
    .jcall(propiedades, "Ljava/lang/Object;", "setProperty", "JDBC_QUERY_RESULT_FORMAT", "JSON")
    
    .jcall(propiedades, "Ljava/lang/Object;", "put", 
           .jcast(.jnew("java/lang/String", "privateKey"), "java/lang/Object"),
           .jcast(private_key, "java/lang/Object"))
    
    jconn <- .jcall(driver@jdrv, "Ljava/sql/Connection;", "connect", url_conexion, propiedades)
    
    if (is.jnull(jconn)) {
      stop("La conexion retorno NULL. Verifica la clave o el usuario.")
    }
    
    conexion <- new("JDBCConnection", jc = jconn, identifier.quote = driver@identifier.quote)
    
    if (!DBI::dbIsValid(conexion)) {
      stop("La conexion no es valida despues de establecerla.")
    }
    
    return(conexion)
  }, error = function(e) {
    stop(paste("Error conectando a Snowflake (JWT):", e$message))
  })
}

# Cargar agenda desde Snowflake
cargar_agenda_snowflake_bbva <- function() {
  tryCatch({
    conn <- crear_conexion_snowflake_almena()
    on.exit(DBI::dbDisconnect(conn))
    
    agenda_df <- DBI::dbReadTable(conn, "AGENDA")
    
    columnas_existentes <- names(agenda_df)
    posibles_identificador <- c("IDENTIFICADOR", "CLABE", "CUENTA", "NUMERO_CUENTA", "ID")
    posibles_cliente_num <- c("Nª DE CLIENTE", "No DE CLIENTE", "NUM_CLIENTE", "NUMERO_CLIENTE", "ID_CLIENTE", "NO_CLIENTE")
    posibles_cliente <- c("CLIENTE", "NOMBRE_CLIENTE", "NOMBRE")
    posibles_razon <- c("RAZON SOCIAL", "RAZON_SOCIAL", "EMPRESA", "NOMBRE_EMPRESA")
    
    identificador_col <- intersect(posibles_identificador, columnas_existentes)[1]
    cliente_num_col <- intersect(posibles_cliente_num, columnas_existentes)[1]
    cliente_col <- intersect(posibles_cliente, columnas_existentes)[1]
    razon_social_col <- intersect(posibles_razon, columnas_existentes)[1]
    
    if (is.na(identificador_col)) {
      stop("No se encontro columna IDENTIFICADOR en la tabla AGENDA.")
    }
    
    agenda_df <- agenda_df %>%
      dplyr::select(
        IDENTIFICADOR = all_of(identificador_col),
        `No DE CLIENTE` = if(!is.na(cliente_num_col)) all_of(cliente_num_col) else NA,
        CLIENTE = if(!is.na(cliente_col)) all_of(cliente_col) else NA,
        `RAZON SOCIAL` = if(!is.na(razon_social_col)) all_of(razon_social_col) else NA
      ) %>%
      dplyr::mutate(
        IDENTIFICADOR = stringr::str_trim(as.character(IDENTIFICADOR))
      ) %>%
      dplyr::filter(!is.na(IDENTIFICADOR) & IDENTIFICADOR != "")
    
    return(agenda_df)
    
  }, error = function(e) {
    stop(paste("Error al cargar la agenda desde Snowflake:", e$message))
  })
}

# Preparar agenda para el join
preparar_agenda_bbva <- function(agenda_df) {
  columnas_existentes <- names(agenda_df)
  
  posibles_identificador <- c("IDENTIFICADOR", "CLABE", "CUENTA", "NUMERO_CUENTA", "ID")
  identificador_col <- NULL
  for (col in posibles_identificador) {
    if (col %in% columnas_existentes) {
      identificador_col <- col
      break
    }
  }
  
  posibles_cliente_num <- c("Nº DE CLIENTE", "No DE CLIENTE", "NUM_CLIENTE", "NUMERO_CLIENTE", "ID_CLIENTE", "NO_CLIENTE")
  cliente_num_col <- NULL
  for (col in posibles_cliente_num) {
    if (col %in% columnas_existentes) {
      cliente_num_col <- col
      break
    }
  }
  
  posibles_cliente <- c("CLIENTE", "NOMBRE_CLIENTE", "NOMBRE")
  cliente_col <- NULL
  for (col in posibles_cliente) {
    if (col %in% columnas_existentes) {
      cliente_col <- col
      break
    }
  }
  
  posibles_razon <- c("RAZON SOCIAL", "RAZON_SOCIAL", "EMPRESA", "NOMBRE_EMPRESA")
  razon_social_col <- NULL
  for (col in posibles_razon) {
    if (col %in% columnas_existentes) {
      razon_social_col <- col
      break
    }
  }
  
  if (is.null(identificador_col)) {
    stop("No se encontro la columna IDENTIFICADOR en la agenda")
  }
  
  agenda_completa <- agenda_df %>%
    select(IDENTIFICADOR = all_of(identificador_col)) %>%
    mutate(
      IDENTIFICADOR = as.character(IDENTIFICADOR),
      IDENTIFICADOR = str_trim(IDENTIFICADOR),
      IDENTIFICADOR = ifelse(is.na(IDENTIFICADOR), "", IDENTIFICADOR)
    )
  
  if (!is.null(cliente_num_col)) {
    agenda_completa <- agenda_completa %>%
      mutate(`No DE CLIENTE` = as.character(agenda_df[[cliente_num_col]]))
  } else {
    agenda_completa <- agenda_completa %>%
      mutate(`No DE CLIENTE` = "-")
  }
  
  if (!is.null(cliente_col)) {
    agenda_completa <- agenda_completa %>%
      mutate(CLIENTE = as.character(agenda_df[[cliente_col]]))
  } else {
    agenda_completa <- agenda_completa %>%
      mutate(CLIENTE = "-")
  }
  
  if (!is.null(razon_social_col)) {
    agenda_completa <- agenda_completa %>%
      mutate(`RAZON SOCIAL` = as.character(agenda_df[[razon_social_col]]))
  } else {
    agenda_completa <- agenda_completa %>%
      mutate(`RAZON SOCIAL` = "-")
  }
  
  agenda_completa <- agenda_completa %>%
    mutate(
      `No DE CLIENTE` = ifelse(is.na(`No DE CLIENTE`) | `No DE CLIENTE` == "", "-", `No DE CLIENTE`),
      CLIENTE = ifelse(is.na(CLIENTE) | CLIENTE == "", "-", CLIENTE),
      `RAZON SOCIAL` = ifelse(is.na(`RAZON SOCIAL`) | `RAZON SOCIAL` == "", "-", `RAZON SOCIAL`)
    ) %>%
    filter(!is.na(IDENTIFICADOR) & IDENTIFICADOR != "")
  
  duplicados_info <- agenda_completa %>%
    group_by(IDENTIFICADOR) %>%
    summarise(count = n(), .groups = "drop") %>%
    filter(count > 1)
  
  identificadores_duplicados <- duplicados_info$IDENTIFICADOR
  
  agenda_join <- agenda_completa %>%
    filter(!IDENTIFICADOR %in% identificadores_duplicados) %>%
    distinct(IDENTIFICADOR, .keep_all = TRUE)
  
  return(list(
    agenda = agenda_join,
    duplicados = identificadores_duplicados
  ))
}

# FUNCION PRINCIPAL
procesar_archivo_bbva <- function(archivo_entrada, agenda_df = NULL) {
  tryCatch({
    
    # PASO 1: CARGAR Y PROCESAR ARCHIVO
    wb <- loadWorkbook(archivo_entrada)
    datos <- read.xlsx(wb, sheet = 1, detectDates = TRUE, colNames = FALSE)
    
    datos_nuevos <- matrix(NA, nrow = nrow(datos), ncol = ncol(datos) + 1)
    datos_nuevos <- as.data.frame(datos_nuevos)
    datos_nuevos[, 1] <- datos[, 1]
    datos_nuevos[, 2] <- NA
    
    for (col in 2:ncol(datos)) {
      datos_nuevos[, col + 1] <- datos[, col]
    }
    
    datos <- datos_nuevos
    
    datos[2, 2] <- "CONCEPTO"
    
    if (!is.na(datos[1, 3])) {
      datos[1, 2] <- datos[1, 3]
    }
    
    datos[2, 9] <- "IDENTIFICADOR"
    datos[2, 10] <- "No DE CLIENTE"
    datos[2, 11] <- "CLIENTE"
    datos[2, 12] <- "RAZON SOCIAL"
    
    for (i in 3:nrow(datos)) {
      valores <- c()
      
      for (col_idx in 3:5) {
        if (!is.na(datos[i, col_idx])) {
          if (inherits(datos[i, col_idx], c("Date", "POSIXct", "POSIXt"))) {
            valores <- c(valores, format(datos[i, col_idx], "%d/%m/%Y"))
          } else if (is.numeric(datos[i, col_idx])) {
            valores <- c(valores, sprintf("%.2f", floor(datos[i, col_idx] * 100) / 100))
          } else if (as.character(datos[i, col_idx]) != "") {
            valores <- c(valores, as.character(datos[i, col_idx]))
          }
        }
      }
      
      if (length(valores) > 0) {
        datos[i, 2] <- paste(valores, collapse = " ")
      }
    }
    
    for (col in 1:ncol(datos)) {
      for (row in 1:nrow(datos)) {
        if (!is.na(datos[row, col]) && is.numeric(datos[row, col]) && 
            !inherits(datos[row, col], c("Date", "POSIXct", "POSIXt"))) {
          datos[row, col] <- as.numeric(floor(datos[row, col] * 100) / 100)
        }
      }
    }
    
    datos <- datos[, -c(3, 4, 5)]
    
    for (col in 3:ncol(datos)) {
      for (row in 1:nrow(datos)) {
        if (!is.na(datos[row, col])) {
          valor <- suppressWarnings(as.numeric(datos[row, col]))
          if (!is.na(valor) && is.numeric(valor)) {
            datos[row, col] <- trunc(valor * 100) / 100
          }
        }
      }
    }
    
    if (nrow(datos) > 2) {
      encabezados <- datos[1:2, ]
      datos_ordenar <- datos[3:nrow(datos), ]
      datos_ordenar <- datos_ordenar[nrow(datos_ordenar):1, ]
      datos <- rbind(encabezados, datos_ordenar)
    }
    
    # PASO 2: EXTRAER IDENTIFICADORES CON LOGICA ESPECIAL PARA IMT, HL, MDI
    
    # Detectar numero de cuenta del archivo
    numero_cuenta_archivo <- NA
    if (!is.na(datos[1, 2])) {
      numero_cuenta_archivo <- as.character(datos[1, 2])
      numero_cuenta_archivo <- gsub("[^0-9]", "", numero_cuenta_archivo)
    }
    
    # Definir cuentas especiales
    cuentas_especiales <- c("0280001101", "0280001102", "0280001103")
    
    # Verificar si es cuenta especial
    es_cuenta_especial <- !is.na(numero_cuenta_archivo) && 
      numero_cuenta_archivo %in% cuentas_especiales
    
    cat("Numero de cuenta detectado:", numero_cuenta_archivo, "\n")
    cat("Es cuenta especial:", es_cuenta_especial, "\n")
    
    identificadores_extraidos <- 0
    
    for (i in 3:nrow(datos)) {
      if (!is.na(datos[i, 2])) {
        concepto <- as.character(datos[i, 2])
        
        if (grepl("^SPEI", concepto, ignore.case = TRUE) ||
            grepl("^PAGO CUENTA DE TERCERO", concepto, ignore.case = TRUE)) {
          
          concepto_limpio <- gsub("'([0-9])", "\\1", concepto)
          
          if (es_cuenta_especial) {
            # Para cuentas especiales: extraer segundos 10 digitos
            todos_digitos <- gregexpr("[0-9]+", concepto_limpio)
            matches <- regmatches(concepto_limpio, todos_digitos)[[1]]
            
            # Concatenar todos los digitos encontrados
            digitos_completos <- paste(matches, collapse = "")
            
            # Saltar primeros 10, tomar siguientes 10
            if (nchar(digitos_completos) >= 20) {
              identificador <- substr(digitos_completos, 11, 20)
              identificador <- as.character(trimws(identificador))
              datos[i, 6] <- identificador
              identificadores_extraidos <- identificadores_extraidos + 1
              cat("  Especial extraido:", identificador, "de fila", i, "\n")
            }
            
          } else {
            # Para cuentas normales: primeros 10 digitos
            patron <- regexpr("[0-9]{10}", concepto_limpio)
            
            if (patron[1] != -1) {
              identificador <- substr(concepto_limpio, patron[1], patron[1] + 9)
              identificador <- as.character(trimws(identificador))
              datos[i, 6] <- identificador
              identificadores_extraidos <- identificadores_extraidos + 1
            }
          }
        }
      }
    }
    
    cat("Total identificadores extraidos:", identificadores_extraidos, "\n")
    
    # PASO 3: LEFT JOIN CON AGENDA
    
    if(is.null(agenda_df)) {
      agenda_df <- cargar_agenda_snowflake_bbva()
    }
    
    agenda_preparada <- preparar_agenda_bbva(agenda_df)
    agenda_join <- agenda_preparada$agenda
    identificadores_duplicados <- agenda_preparada$duplicados
    
    for (i in 3:nrow(datos)) {
      datos[i, 7] <- NA
      datos[i, 8] <- NA
      datos[i, 9] <- NA
    }
    
    registros_encontrados <- 0
    registros_no_encontrados <- 0
    registros_duplicados <- 0
    
    for (i in 3:nrow(datos)) {
      identificador_actual <- datos[i, 6]
      
      if (!is.na(identificador_actual) && identificador_actual != "") {
        
        identificador_limpio <- as.character(trimws(identificador_actual))
        
        if (identificador_limpio %in% identificadores_duplicados) {
          datos[i, 7] <- ""
          datos[i, 8] <- "Identificador duplicado en la agenda"
          datos[i, 9] <- "Identificador duplicado en la agenda"
          registros_duplicados <- registros_duplicados + 1
        } else {
          match <- agenda_join %>%
            filter(IDENTIFICADOR == identificador_limpio)
          
          if (nrow(match) > 0) {
            datos[i, 7] <- as.character(match$`No DE CLIENTE`[1])
            datos[i, 8] <- as.character(match$CLIENTE[1])
            datos[i, 9] <- as.character(match$`RAZON SOCIAL`[1])
            registros_encontrados <- registros_encontrados + 1
          } else {
            datos[i, 7] <- "No registrado"
            datos[i, 8] <- "No registrado"
            datos[i, 9] <- "No registrado"
            registros_no_encontrados <- registros_no_encontrados + 1
          }
        }
      }
    }
    
    # Limpiar formato de No DE CLIENTE
    for (i in 3:nrow(datos)) {
      if (!is.na(datos[i, 7]) && datos[i, 7] != "" && 
          datos[i, 7] != "No registrado" && 
          datos[i, 7] != "Identificador duplicado en la agenda") {
        
        valor_numerico <- suppressWarnings(as.numeric(datos[i, 7]))
        
        if (!is.na(valor_numerico)) {
          datos[i, 7] <- as.character(as.integer(valor_numerico))
        }
      }
    }
    
    cat("Formato de No DE CLIENTE corregido\n")
    
    # PASO 3.5: REORDENAR COLUMNAS
    
    numero_cuenta <- NA
    if (!is.na(datos[1, 2])) {
      numero_cuenta <- as.character(datos[1, 2])
      numero_cuenta <- gsub("[^0-9]", "", numero_cuenta)
    }
    
    cuentas_reorden <- c("0280001101", "0280001103")
    
    if (!is.na(numero_cuenta) && numero_cuenta %in% cuentas_reorden) {
      datos_reordenados <- datos[, c(1, 4, 3, 5, 2, 6:ncol(datos))]
      datos <- datos_reordenados
    } else {
      datos_reordenados <- datos[, c(1, 2, 4, 3, 5:ncol(datos))]
      datos <- datos_reordenados
    }
    
    # PASO 4: ELIMINAR COLUMNA IDENTIFICADOR Y GUARDAR
    
    datos_para_guardar <- datos[, -6]
    
    carpeta_temporal <- tempdir()
    nombre_base <- tools::file_path_sans_ext(basename(archivo_entrada))
    nombre_salida <- paste0(nombre_base, "_modificado.xlsx")
    ruta_salida <- file.path(carpeta_temporal, nombre_salida)
    
    wb_nuevo <- createWorkbook()
    addWorksheet(wb_nuevo, "Sheet1")
    
    writeData(wb_nuevo, sheet = 1, x = datos_para_guardar, 
              startRow = 1, startCol = 1, 
              colNames = FALSE)
    
    for (col in 3:ncol(datos_para_guardar)) {
      for (row in 1:nrow(datos_para_guardar)) {
        if (!is.na(datos_para_guardar[row, col])) {
          valor <- suppressWarnings(as.numeric(datos_para_guardar[row, col]))
          if (!is.na(valor) && is.numeric(valor)) {
            valor_truncado <- trunc(valor * 100) / 100
            writeData(wb_nuevo, sheet = 1, x = valor_truncado, 
                      startRow = row, startCol = col)
          }
        }
      }
    }
    
    style_texto <- createStyle(numFmt = "@", halign = "left")
    addStyle(wb_nuevo, sheet = 1, style = style_texto, 
             rows = 1:nrow(datos_para_guardar), cols = 2,
             gridExpand = TRUE, stack = TRUE)
    
    style_num <- createStyle(numFmt = "0.00", halign = "right")
    
    col_no_cliente <- 6
    
    for (col in 3:ncol(datos_para_guardar)) {
      if (col != col_no_cliente) {
        addStyle(wb_nuevo, sheet = 1, style = style_num, 
                 rows = 3:nrow(datos_para_guardar), cols = col,
                 gridExpand = TRUE, stack = FALSE)
      }
    }
    
    style_texto_cliente <- createStyle(numFmt = "@", halign = "left")
    addStyle(wb_nuevo, sheet = 1, style = style_texto_cliente,
             rows = 1:nrow(datos_para_guardar), cols = col_no_cliente,
             gridExpand = TRUE, stack = TRUE)
    
    style_fecha <- createStyle(numFmt = "DD/MM/YYYY")
    addStyle(wb_nuevo, sheet = 1, style = style_fecha, 
             rows = 1:nrow(datos_para_guardar), cols = 1,
             gridExpand = TRUE, stack = TRUE)
    
    setColWidths(wb_nuevo, sheet = 1, cols = 1:ncol(datos_para_guardar), widths = "auto")
    
    saveWorkbook(wb_nuevo, ruta_salida, overwrite = TRUE)
    
    cat("Columna IDENTIFICADOR eliminada del archivo final\n")
    
    # PASO 5: EXTRAER NO REGISTRADOS
    
    no_registrados_raw <- data.frame(
      IDENTIFICADOR = character(),
      EMPRESA = character(),
      CUENTA = character(),
      stringsAsFactors = FALSE
    )
    
    for (i in 3:nrow(datos)) {
      if (!is.na(datos[i, 8]) && datos[i, 8] == "No registrado") {
        identificador_actual <- as.character(datos[i, 6])
        if (!is.na(identificador_actual) && identificador_actual != "") {
          no_registrados_raw <- rbind(no_registrados_raw, data.frame(
            IDENTIFICADOR = identificador_actual,
            EMPRESA = "",
            CUENTA = "",
            stringsAsFactors = FALSE
          ))
        }
      }
    }
    
    total_antes <- nrow(no_registrados_raw)
    duplicados_detalle <- no_registrados_raw %>%
      group_by(IDENTIFICADOR) %>%
      summarise(count = n(), .groups = "drop") %>%
      filter(count > 1)
    
    num_duplicados <- sum(duplicados_detalle$count - 1)
    
    no_registrados_bbva <- no_registrados_raw %>%
      group_by(IDENTIFICADOR) %>%
      slice(1) %>%
      ungroup()
    
    total_despues <- nrow(no_registrados_bbva)
    
    archivo_no_registrados <- NULL
    
    if (nrow(no_registrados_bbva) > 0) {
      nombre_no_registrados <- paste0("No_registrados_", nombre_base, ".xlsx")
      ruta_no_registrados <- file.path(carpeta_temporal, nombre_no_registrados)
      
      wb_no_registrados <- createWorkbook()
      addWorksheet(wb_no_registrados, "No registrados")
      writeData(wb_no_registrados, "No registrados", no_registrados_bbva)
      
      headerStyle <- createStyle(
        fontSize = 12,
        fontColour = "white",
        halign = "center",
        fgFill = "#FF0000", 
        border = "TopBottomLeftRight",
        borderColour = "black"
      )
      
      addStyle(wb_no_registrados, "No registrados", 
               style = headerStyle, 
               rows = 1, 
               cols = 1:3, 
               gridExpand = TRUE)
      
      setColWidths(wb_no_registrados, "No registrados", cols = 1:3, widths = "auto")
      
      saveWorkbook(wb_no_registrados, ruta_no_registrados, overwrite = TRUE)
      
      archivo_no_registrados <- ruta_no_registrados
    }
    
    # RETORNAR RESULTADO
    
    return(list(
      exito = TRUE,
      archivo_modificado = ruta_salida,
      archivo_no_registrados = archivo_no_registrados,  
      datos = datos,
      registros_procesados = nrow(datos) - 2,
      identificadores_extraidos = identificadores_extraidos,
      registros_encontrados = registros_encontrados,
      registros_no_encontrados = registros_no_encontrados,
      registros_duplicados = registros_duplicados,
      no_registrados_detalle = no_registrados_bbva,
      duplicados_eliminados = num_duplicados,  
      mensaje = "Archivo BANCO_NORTE procesado exitosamente"
    ))
    
  }, error = function(e) {
    return(list(
      exito = FALSE,
      error = e$message,
      mensaje = paste("Error al procesar archivo BANCO_NORTE:", e$message)
    ))
  })
}