library(shiny)
library(DT)
library(openxlsx)
library(shinydashboard)
library(rsconnect)
library(shinyjs)
library(readxl)
library(tools)
library(dbplyr)
library(dplyr)
library(stringr)
library(DBI)
library(RJDBC)
library(rJava)
library(future)
library(promises)
library(httr)
library(jsonlite)
library(data.table)
library(future.apply)

# ============================================================================
# FUNCIONES DE PAZ ROBOT
# ============================================================================

# Función de corrección automática de rutas (de App.R)
normalizar_ruta_automatica <- function(ruta) {
  if(is.null(ruta) || nchar(trimws(ruta)) == 0) return(ruta)
  
  ruta_original <- ruta
  ruta <- gsub("\\\\", "/", ruta)
  ruta <- gsub("/+", "/", ruta)
  ruta <- trimws(ruta)
  ruta <- gsub("/$", "", ruta)
  
  tryCatch({
    if(grepl("^[A-Za-z]:/", ruta)) {
      dir_padre <- dirname(ruta)
      if(dir.exists(dir_padre)) {
        ruta_norm <- normalizePath(ruta, winslash = "/", mustWork = FALSE)
        if(!is.na(ruta_norm) && nchar(ruta_norm) > 0) {
          ruta <- ruta_norm
        }
      }
    }
  }, error = function(e) {})
  
  resultado <- list(
    ruta_original = ruta_original,
    ruta_corregida = ruta,
    fue_modificada = (ruta_original != ruta),
    cambios_realizados = c()
  )
  
  if(grepl("\\\\", ruta_original)) {
    resultado$cambios_realizados <- c(resultado$cambios_realizados, "Barras \\ convertidas a /")
  }
  if(grepl("/+", gsub("\\\\", "/", ruta_original))) {
    resultado$cambios_realizados <- c(resultado$cambios_realizados, "Barras múltiples limpiadas")
  }
  if(ruta_original != trimws(ruta_original)) {
    resultado$cambios_realizados <- c(resultado$cambios_realizados, "Espacios eliminados")
  }
  
  return(resultado)
}

normalizar_ruta <- function(ruta) {
  resultado <- normalizar_ruta_automatica(ruta)
  return(resultado$ruta_corregida)
}

# ============================================================================
# FUNCIÓN: Convertir PEM a PrivateKey para JWT (AUTENTICACIÓN SNOWFLAKE)
# ============================================================================
convertir_pem_a_privatekey <- function(ruta_pem) {
  tryCatch({
    # Leer el contenido del archivo .pem
    lineas <- readLines(ruta_pem, warn = FALSE)
    
    # Remover headers y footers del PEM (BEGIN/END PRIVATE KEY)
    lineas <- lineas[!grepl("^-----", lineas)]
    
    # Unir todas las líneas sin saltos de línea
    clave_base64 <- paste(lineas, collapse = "")
    
    # Decodificar de Base64 a bytes
    decoder <- .jcall("java/util/Base64", "Ljava/util/Base64$Decoder;", "getDecoder")
    bytes_clave <- .jcall(decoder, "[B", "decode", clave_base64)
    
    # Crear PKCS8EncodedKeySpec
    key_spec <- .jnew("java/security/spec/PKCS8EncodedKeySpec", bytes_clave)
    
    # Obtener KeyFactory para RSA
    key_factory <- .jcall("java/security/KeyFactory", "Ljava/security/KeyFactory;", 
                          "getInstance", "RSA")
    
    # Generar la PrivateKey
    private_key <- .jcall(key_factory, "Ljava/security/PrivateKey;", 
                          "generatePrivate", .jcast(key_spec, "java/security/spec/KeySpec"))
    
    return(private_key)
  }, error = function(e) {
    stop(paste("Error al convertir la clave privada:", e$message))
  })
}

# ============================================================================
# FUNCIÓN: Conectar a Snowflake con JWT (SIN USUARIO/PASSWORD)
# ============================================================================
crear_conexion_snowflake <- function(usuario = Sys.getenv("SNOWFLAKE_USER"), ruta_pem = Sys.getenv("SNOWFLAKE_PRIVATE_KEY_PATH")) {
  tryCatch({
    # Ruta del driver JAR
    ruta_archivo_jar <- file.path(getwd(), "snowflake-jdbc-3.25.1.jar")
    ruta_pem_absoluta <- file.path(getwd(), ruta_pem)
    
    # Crear driver JDBC
    driver <- RJDBC::JDBC("net.snowflake.client.jdbc.SnowflakeDriver", 
                          classPath = ruta_archivo_jar, 
                          identifier.quote = "`")
    
    # Convertir la clave privada a objeto PrivateKey de Java
    private_key <- convertir_pem_a_privatekey(ruta_pem_absoluta)
    
    # URL de conexión
    url_conexion <- paste0("jdbc:snowflake://", Sys.getenv("SNOWFLAKE_ACCOUNT"), ".snowflakecomputing.com/")
    
    # Crear Properties usando rJava
    propiedades <- .jnew("java.util.Properties")
    .jcall(propiedades, "Ljava/lang/Object;", "setProperty", "user", usuario)
    .jcall(propiedades, "Ljava/lang/Object;", "setProperty", "warehouse", "WH_ANALYTICS")
    .jcall(propiedades, "Ljava/lang/Object;", "setProperty", "db", "DB_ANALYTICS")
    .jcall(propiedades, "Ljava/lang/Object;", "setProperty", "schema", "SCH_CORE")
    .jcall(propiedades, "Ljava/lang/Object;", "setProperty", "role", "ADMIN_ALMENA")
    .jcall(propiedades, "Ljava/lang/Object;", "setProperty", "authenticator", "SNOWFLAKE_JWT")
    .jcall(propiedades, "Ljava/lang/Object;", "setProperty", "tracing", "OFF")
    .jcall(propiedades, "Ljava/lang/Object;", "setProperty", "JDBC_QUERY_RESULT_FORMAT", "JSON")
    
    # Agregar la clave privada a las propiedades
    .jcall(propiedades, "Ljava/lang/Object;", "put", 
           .jcast(.jnew("java/lang/String", "privateKey"), "java/lang/Object"),
           .jcast(private_key, "java/lang/Object"))
    
    # Obtener el conector usando el driver
    jconn <- .jcall(driver@jdrv, "Ljava/sql/Connection;", "connect", url_conexion, propiedades)
    
    # Verificar que la conexión no sea nula
    if (is.jnull(jconn)) {
      stop("La conexión retornó null. Verifica las credenciales y permisos.")
    }
    
    # Convertir a objeto RJDBC
    conexion <- new("JDBCConnection", jc = jconn, identifier.quote = driver@identifier.quote)
    
    # Verificar que la conexión sea válida
    if (!DBI::dbIsValid(conexion)) {
      stop("La conexión no es válida después de establecerla")
    }
    
    cat("✅ Conexión exitosa a Snowflake (JWT)\n")
    
    # Devolver el objeto de conexión
    return(conexion)
    
  }, error = function(e) {
    stop(paste("❌ Error conectando a Snowflake:", e$message))
  })
}

#source("Consolidado.R")
source("EC_BANCO_NORTE.R")
source("clasificacion_pagos.R")
source("movimientos.R")
source("banco_api.R")
source("descarga_excel.R")

# ============================================================================
#cargar_agenda_snowflake FUNCIONES ORIGINALES DE ACTUALIZACION
# ============================================================================
# Función para cargar datos existentes - VERSIÓN MÁS ROBUSTA
cargar_datos <- function() {
  if (file.exists("clientes_data.rds")) {
    tryCatch({
      datos_existentes <- readRDS("clientes_data.rds")
      
      # Validar que sea un data frame
      if (!is.data.frame(datos_existentes)) {
        warning("El archivo RDS no contiene un data frame válido. Creando estructura nueva.")
        return(crear_estructura_vacia())
      }
      
      # Agregar columna EDITADO si no existe
      if(!"EDITADO" %in% names(datos_existentes)) {
        # Usar rep() para crear un vector del tamaño correcto
        datos_existentes$EDITADO <- rep(FALSE, nrow(datos_existentes))
      }
      
      # Validar que las columnas requeridas existan
      columnas_requeridas <- c("IDENTIFICADOR", "NUM_CLIENTE", "CLIENTE", "RAZON_SOCIAL", "FECHA_REGISTRO", "EDITADO")
      columnas_faltantes <- setdiff(columnas_requeridas, names(datos_existentes))
      
      if(length(columnas_faltantes) > 0) {
        warning(paste("Columnas faltantes detectadas:", paste(columnas_faltantes, collapse = ", ")))
        # Agregar columnas faltantes
        for(col in columnas_faltantes) {
          if(col == "FECHA_REGISTRO") {
            datos_existentes[[col]] <- rep(as.Date(NA), nrow(datos_existentes))
          } else if(col == "EDITADO") {
            datos_existentes[[col]] <- rep(FALSE, nrow(datos_existentes))
          } else {
            datos_existentes[[col]] <- rep("", nrow(datos_existentes))
          }
        }
      }
      
      return(datos_existentes)
      
    }, error = function(e) {
      warning(paste("Error al cargar clientes_data.rds:", e$message, ". Creando estructura nueva."))
      return(crear_estructura_vacia())
    })
  } else {
    return(crear_estructura_vacia())
  }
}

# Función auxiliar para crear estructura vacía
crear_estructura_vacia <- function() {
  data.frame(
    IDENTIFICADOR = character(0),
    NUM_CLIENTE = character(0),
    CLIENTE = character(0),
    RAZON_SOCIAL = character(0),
    FECHA_REGISTRO = as.Date(character(0)),
    EDITADO = logical(0),
    stringsAsFactors = FALSE
  )
}

# Función para validar que un texto sea solo numérico
es_numerico <- function(texto) {
  if (is.null(texto) || texto == "") return(TRUE)
  return(grepl("^[0-9]+$", texto))
}

# Función para validar longitud del identificador
validar_longitud_identificador <- function(identificador) {
  if (is.null(identificador) || identificador == "") return(TRUE)
  return(nchar(identificador) <= 18)
}

# Función para validar la estructura del archivo Excel
validar_estructura_archivo <- function(datos) {
  if (ncol(datos) < 3) {
    return(list(
      valido = FALSE,
      mensaje = "El archivo debe tener al menos 3 columnas (IDENTIFICADOR, EMPRESA, CUENTA)"
    ))
  }
  
  columnas <- names(datos)[1:3]
  
  if (!grepl("IDENTIFICADOR", toupper(columnas[1]), ignore.case = TRUE)) {
    return(list(
      valido = FALSE,
      mensaje = paste0("La columna A debe ser 'IDENTIFICADOR', pero encontré: '", columnas[1], "'")
    ))
  }
  
  if (!grepl("EMPRESA", toupper(columnas[2]), ignore.case = TRUE)) {
    return(list(
      valido = FALSE,
      mensaje = paste0("La columna B debe ser 'EMPRESA', pero encontré: '", columnas[2], "'")
    ))
  }
  
  if (!grepl("CUENTA", toupper(columnas[3]), ignore.case = TRUE)) {
    return(list(
      valido = FALSE,
      mensaje = paste0("La columna C debe ser 'CUENTA', pero encontré: '", columnas[3], "'")
    ))
  }
  
  return(list(
    valido = TRUE,
    mensaje = "✅ Estructura del archivo válida"
  ))
}

# ============================================================================
# UI INTEGRADA CON PAZ ROBOT Y SISTEMA DE LOGIN
# ============================================================================
ui <- fluidPage(
  useShinyjs(),
  
  tags$script(HTML("
  $(document).on('click', '.rc-btn-registrar', function() {
    var id = $(this).data('id');
    Shiny.setInputValue('rc_identificador_seleccionado', id, {priority: 'event'});
  });
")),
  
  tags$head(
    tags$style(HTML("

      /* ==============================================
         ESTILOS PARA EL SISTEMA DE LOGIN
         ============================================== */

      @keyframes slideInUp {
        from { opacity: 0; transform: translateY(50px); }
        to   { opacity: 1; transform: translateY(0); }
      }

      .login-title {
        color: #333;
        font-size: 2.5rem;
        font-weight: bold;
        margin-bottom: 15px;
        background: linear-gradient(135deg, #802e25 0%, #ff5b49 100%);
        -webkit-background-clip: text;
        -webkit-text-fill-color: transparent;
        background-clip: text;
      }

      .login-subtitle {
        color: #726f6b;
        font-size: 1.1rem;
        margin-bottom: 40px;
      }

      .login-form { text-align: left; }

      .login-form .form-group {
        margin-bottom: 25px;
        position: relative;
      }

      .login-form label {
        font-weight: bold;
        color: #393835;
        margin-bottom: 8px;
        display: block;
      }

      .login-form .form-control {
        border: 2px solid #e0d4c3;
        border-radius: 10px;
        padding: 15px 20px;
        font-size: 1rem;
        transition: all 0.3s ease;
        background: #fdf5f4;
      }

      .login-form .form-control:focus {
        border-color: #802e25;
        box-shadow: 0 0 15px rgba(128, 46, 37, 0.25);
        background: white;
        outline: none;
      }

      .btn-login {
        background: linear-gradient(135deg, #401712 0%, #802e25 60%, #ff5b49 100%);
        color: white !important;
        border: none;
        padding: 15px 30px;
        font-size: 1.1rem;
        font-weight: bold;
        border-radius: 10px;
        width: 100%;
        margin-top: 20px;
        transition: all 0.3s ease;
        cursor: pointer;
      }

      .btn-login:hover {
        background: linear-gradient(135deg, #802e25 0%, #401712 100%);
        transform: translateY(-2px);
        box-shadow: 0 8px 25px rgba(128, 46, 37, 0.45);
        color: white !important;
      }

      .btn-logout {
        background: linear-gradient(135deg, #802e25 0%, #401712 100%);
        color: white;
        border: none;
        padding: 8px 20px;
        font-size: 0.9rem;
        font-weight: bold;
        border-radius: 6px;
        transition: all 0.3s ease;
        cursor: pointer;
        margin-left: 10px;
      }

      .btn-logout:hover {
        background: linear-gradient(135deg, #401712 0%, #000000 100%);
        transform: translateY(-1px);
        box-shadow: 0 4px 15px rgba(64, 23, 18, 0.45);
        color: white;
      }

      .alert-login {
        margin-top: 20px;
        padding: 15px;
        border-radius: 8px;
        font-weight: 500;
      }

      .alert-success {
        background: linear-gradient(135deg, #f5ece8 0%, #e0d4c3 100%);
        border: 1px solid #726f6b;
        color: #393835;
      }

      .alert-danger {
        background: linear-gradient(135deg, #fce8e6 0%, #f5d5d2 100%);
        border: 1px solid #802e25;
        color: #401712;
      }

      .alert-warning {
        background: linear-gradient(135deg, #fdf5f4 0%, #ffe8e5 100%);
        border: 1px solid #ff5b49;
        color: #802e25;
      }

      .user-info {
        background: linear-gradient(135deg, #f5ece8 0%, #e0d4c3 100%);
        padding: 8px 15px;
        border-radius: 6px;
        border-left: 4px solid #802e25;
        margin-right: 15px;
        font-weight: bold;
        color: #401712;
        display: inline-block;
      }

      /* ==============================================
         HEADER Y SIDEBAR
         ============================================== */

      .main-header .navbar {
        background: linear-gradient(135deg, #401712 0%, #802e25 60%, #ff5b49 100%) !important;
      }

      .main-header .logo {
        background: #401712 !important;
        border-right: 2px solid #802e25 !important;
        font-weight: bold;
        color: #ffffff !important;
      }
      
      .main-header .logo a {
        color: #ffffff !important;
      }

      .main-sidebar, .left-side {
        background: #393835 !important;
      }

      .sidebar-menu > li > a {
        color: #e0d4c3 !important;
      }

      .sidebar-menu > li.active > a {
        background: #802e25 !important;
        border-left: 3px solid #ff5b49 !important;
        color: #ffffff !important;
      }

      .sidebar-menu > li > a:hover {
        background: #401712 !important;
        color: #ff9d92 !important;
      }

      /* ==============================================
         BOTONES BOOTSTRAP — PALETA CORPORATIVA
         ============================================== */

      .btn-danger {
        color: white !important;
      }
      .btn-danger:hover, .btn-danger:focus, .btn-danger:active {
        color: white !important;
      }

      .btn-success {
        background: linear-gradient(135deg, #393835, #726f6b) !important;
        border-color: #393835 !important;
        color: #e0d4c3 !important;
      }
      .btn-success:hover, .btn-success:focus, .btn-success:active {
        background: linear-gradient(135deg, #000000, #393835) !important;
        border-color: #000000 !important;
        color: #ffffff !important;
      }

      .btn-warning {
        background: linear-gradient(135deg, #726f6b, #393835) !important;
        border-color: #726f6b !important;
        color: #ffffff !important;
      }
      .btn-warning:hover, .btn-warning:focus, .btn-warning:active {
        background: linear-gradient(135deg, #393835, #000000) !important;
        border-color: #393835 !important;
        color: #e0d4c3 !important;
      }

      /* ==============================================
         BOXES SHINYDASHBOARD
         ============================================== */

      .box.box-danger > .box-header {
        background: linear-gradient(135deg, #802e25, #ff5b49) !important;
        color: white !important;
      }
      .box.box-danger {
        border-top-color: #802e25 !important;
      }

      .box.box-success > .box-header {
        background: linear-gradient(135deg, #393835, #726f6b) !important;
        color: #e0d4c3 !important;
      }
      .box.box-success {
        border-top-color: #393835 !important;
      }

      /* ==============================================
         ESTILOS ESPECIFICOS PARA ALMENA INTELLIGENCE
         ============================================== */

      .paz-robot-container {
        background-color: #f5f0eb;
        font-family: 'Arial', sans-serif;
        padding: 20px;
        min-height: 100vh;
      }

      .paz-main-container {
        max-width: 100%;
        margin: 0 auto;
        background-color: white;
        border-radius: 20px;
        box-shadow: 0 10px 30px rgba(57, 56, 53, 0.12);
        padding: 40px;
      }

      .paz-main-title {
        color: #802e25;
        text-align: center;
        font-weight: bold;
        margin-bottom: 40px;
        text-shadow: 1px 1px 3px rgba(128, 46, 37, 0.15);
        font-size: 2.5rem;
      }

      .paz-section-title {
        color: #802e25;
        font-weight: bold;
        border-bottom: 2px solid #ff5b49;
        padding-bottom: 8px;
        margin-bottom: 25px;
        text-align: center;
      }

      .paz-file-input {
        margin-bottom: 25px;
      }

      .paz-file-input .form-control-file {
        border: 2px dashed #ff5b49;
        border-radius: 10px;
        padding: 20px;
        text-align: center;
        background-color: #fdf5f4;
        transition: all 0.3s ease;
      }

      .paz-file-input .form-control-file:hover {
        border-color: #802e25;
        background-color: #fce8e6;
      }

      .btn-paz-execute {
        background: linear-gradient(135deg, #401712 0%, #802e25 60%, #ff5b49 100%);
        color: white !important;
        border: 3px solid #802e25;
        padding: 20px 40px;
        margin: 30px auto;
        border-radius: 15px;
        font-size: 20px;
        font-weight: bold;
        transition: all 0.3s ease;
        box-shadow: 0 6px 20px rgba(128, 46, 37, 0.45);
        text-transform: uppercase;
        letter-spacing: 1px;
        width: 100%;
        display: block;
      }

      .btn-paz-execute:hover {
        background: linear-gradient(135deg, #802e25 0%, #401712 100%) !important;
        transform: translateY(-3px);
        box-shadow: 0 8px 25px rgba(64, 23, 18, 0.55);
        border-color: #401712;
        color: white !important;
      }

      .btn-paz-execute:disabled {
        background: #726f6b !important;
        border-color: #726f6b !important;
        cursor: not-allowed !important;
        transform: none !important;
        box-shadow: none !important;
      }

      .paz-status-box {
        background-color: #fdf5f4;
        border: 2px solid #802e25;
        padding: 20px;
        border-radius: 10px;
        text-align: center;
        font-weight: bold;
        margin: 20px 0;
        color: #802e25;
        font-size: 1.1rem;
      }

      .paz-path-preview {
        background: linear-gradient(135deg, #fdf5f4 0%, #f5ece8 100%);
        border: 2px solid #802e25;
        border-radius: 12px;
        padding: 20px;
        font-family: 'Segoe UI', Arial, sans-serif;
        font-size: 0.9rem;
        color: #393835;
        margin-top: 15px;
        text-align: left;
        box-shadow: 0 4px 12px rgba(128, 46, 37, 0.15);
      }

      .paz-download-section {
        background: linear-gradient(135deg, #fdf5f4 0%, #f5e8e6 100%);
        border: 2px solid #802e25;
        border-radius: 12px;
        padding: 25px;
        margin-top: 20px;
        text-align: center;
      }

      .paz-download-title {
        color: #401712;
        font-size: 1.3rem;
        font-weight: bold;
        margin-bottom: 20px;
      }

      .btn-paz-download {
        background: linear-gradient(135deg, #802e25 0%, #401712 100%);
        color: white !important;
        border: none;
        padding: 12px 25px;
        border-radius: 8px;
        font-size: 0.95rem;
        font-weight: bold;
        cursor: pointer;
        transition: all 0.3s ease;
        text-decoration: none;
        min-width: 180px;
        display: inline-block;
        margin: 5px;
      }

      .btn-paz-download:hover {
        background: linear-gradient(135deg, #401712 0%, #000000 100%) !important;
        transform: translateY(-2px);
        box-shadow: 0 5px 15px rgba(64, 23, 18, 0.45);
        color: white !important;
      }

      .paz-error-section {
        background: linear-gradient(135deg, #fce8e6 0%, #f5d5d2 100%);
        border: 3px solid #802e25;
        border-radius: 15px;
        padding: 30px;
        margin-top: 25px;
        text-align: center;
        animation: errorShake 0.6s ease-in-out;
        box-shadow: 0 8px 25px rgba(128, 46, 37, 0.3);
      }

      @keyframes errorShake {
        0%, 100% { transform: translateX(0); }
        10%, 30%, 50%, 70%, 90% { transform: translateX(-3px); }
        20%, 40%, 60%, 80% { transform: translateX(3px); }
      }

      .paz-error-title {
        color: #401712;
        font-size: 1.5rem;
        font-weight: bold;
        margin-bottom: 20px;
      }

      .paz-error-message {
        color: #401712;
        font-size: 1.1rem;
        margin-bottom: 25px;
        background: white;
        padding: 20px;
        border-radius: 10px;
        border-left: 5px solid #ff5b49;
        text-align: left;
        font-weight: 500;
        line-height: 1.5;
        box-shadow: 0 3px 10px rgba(0,0,0,0.08);
      }

      .paz-processing-overlay {
        position: fixed;
        top: 0; left: 0;
        width: 100%; height: 100%;
        background: rgba(57, 56, 53, 0.85);
        display: flex;
        justify-content: center;
        align-items: center;
        z-index: 9999;
        animation: fadeIn 0.5s ease-out;
      }

      .paz-processing-spinner {
        background: white;
        border-radius: 20px;
        padding: 50px;
        text-align: center;
        box-shadow: 0 15px 40px rgba(64, 23, 18, 0.4);
        animation: slideUp 0.5s ease-out;
        min-width: 400px;
        max-width: 600px;
      }

      .paz-spinner {
        border: 6px solid #e0d4c3;
        border-top: 6px solid #ff5b49;
        border-radius: 50%;
        width: 60px;
        height: 60px;
        animation: spin 1s linear infinite;
        margin: 20px auto;
      }

      @keyframes spin {
        0%   { transform: rotate(0deg); }
        100% { transform: rotate(360deg); }
      }

      .paz-processing-text {
        color: #802e25;
        font-size: 1.5rem;
        font-weight: bold;
        margin-top: 20px;
        animation: pulse 1.5s infinite;
      }

      @keyframes pulse {
        0%, 100% { opacity: 1; }
        50%       { opacity: 0.7; }
      }

      .paz-progress-container {
        width: 100%;
        background-color: #e0d4c3;
        border-radius: 25px;
        padding: 3px;
        margin: 20px 0;
        box-shadow: inset 0 2px 5px rgba(0,0,0,0.08);
      }

      .paz-progress-bar {
        width: 0%;
        height: 20px;
        background: linear-gradient(135deg, #401712 0%, #ff5b49 50%, #ff9d92 100%);
        border-radius: 20px;
        transition: width 0.8s ease-out;
        position: relative;
        overflow: hidden;
        border: 1px solid #802e25;
      }

      .paz-mensaje-temporal {
        position: fixed;
        top: 50%; left: 50%;
        transform: translate(-50%, -50%);
        background: linear-gradient(135deg, #802e25 0%, #ff5b49 100%);
        color: white;
        padding: 35px 55px;
        border-radius: 25px;
        font-size: 1.6rem;
        font-weight: bold;
        text-align: center;
        box-shadow: 0 20px 50px rgba(128, 46, 37, 0.5);
        z-index: 10000;
        min-width: 450px;
        border: 4px solid #401712;
        animation: mensajeAparece 0.6s ease-out;
      }

      .paz-mensaje-temporal.error {
        background: linear-gradient(135deg, #401712 0%, #802e25 100%);
        border-color: #000000;
        box-shadow: 0 20px 50px rgba(64, 23, 18, 0.6);
      }

      @keyframes mensajeAparece {
        0%   { transform: translate(-50%, -50%) scale(0.3); opacity: 0; }
        50%  { transform: translate(-50%, -50%) scale(1.1); opacity: 0.8; }
        100% { transform: translate(-50%, -50%) scale(1);   opacity: 1; }
      }

      /* ==============================================
         ALERTAS DE ESTADO
         ============================================== */

      .status-success {
        background: linear-gradient(135deg, #f5ece8 0%, #e0d4c3 100%);
        border: 2px solid #726f6b;
        color: #393835;
        padding: 15px;
        border-radius: 8px;
        margin: 10px 0;
      }

      .status-error {
        background: linear-gradient(135deg, #fce8e6 0%, #f5d5d2 100%);
        border: 2px solid #802e25;
        color: #401712;
        padding: 15px;
        border-radius: 8px;
        margin: 10px 0;
      }

      .status-warning {
        background: linear-gradient(135deg, #fdf5f4 0%, #ffe8e5 100%);
        border: 2px solid #ff5b49;
        color: #802e25;
        padding: 15px;
        border-radius: 8px;
        margin: 10px 0;
      }

      .fade-in {
        animation: fadeIn 0.5s ease-in;
      }

      @keyframes fadeIn {
        from { opacity: 0; transform: translateY(20px); }
        to   { opacity: 1; transform: translateY(0); }
      }

      .btn-info {
        background: linear-gradient(135deg, #726f6b, #393835) !important;
        border-color: #726f6b !important;
        color: #e0d4c3 !important;
      }
      .btn-info:hover, .btn-info:focus, .btn-info:active {
        background: linear-gradient(135deg, #393835, #000000) !important;
        border-color: #393835 !important;
        color: #ffffff !important;
      }

      /* ==============================================
         ESTILOS PESTANA AGENDA SNOWFLAKE
         ============================================== */

      .agenda-info-box {
        background: linear-gradient(135deg, #fdf5f4 0%, #f5e8e6 100%);
        border: 2px solid #802e25;
        border-radius: 8px;
        padding: 15px;
        margin-bottom: 15px;
        border-left: 4px solid #ff5b49;
      }

      .agenda-info-title {
        color: #401712;
        margin: 0 0 10px 0;
        font-weight: bold;
      }

      .agenda-info-text {
        color: #802e25;
        margin: 5px 0;
      }

      .agenda-status-box {
        background-color: #fdf5f4;
        border: 2px solid #802e25;
        padding: 15px;
        border-radius: 8px;
        border-left: 4px solid #ff5b49;
        color: #401712;
      }

      /* ---- Barra de filtros reestructurada ---- */
      .cp-filtros-fila1 {
        border-radius: 8px 8px 0 0;
        border-bottom: none;
        padding-bottom: 10px;
      }
      .cp-filtros-fila2 {
        border-top: 1px dashed #e0d4c3;
        border-radius: 0 0 8px 8px;
        padding-top: 10px;
        background: #faf8f5;
        margin-top: 0 !important;
      }
      .cp-control-grupo {
        display: flex;
        flex-direction: column;
        gap: 4px;
      }
      .cp-ctrl-label {
        font-size: 0.72rem !important;
        font-weight: 700 !important;
        text-transform: uppercase !important;
        letter-spacing: 0.07em !important;
        color: #726f6b !important;
        margin: 0 !important;
        white-space: nowrap;
      }
      .cp-separador {
        width: 1px;
        height: 36px;
        background: #e0d4c3;
        align-self: flex-end;
        margin-bottom: 2px;
      }
      .cp-badge-spei {
        background: linear-gradient(135deg, #802e25, #ff5b49);
        border-radius: 20px;
        padding: 8px 18px;
        display: flex;
        align-items: center;
      }
      .cp-btn-primario {
        background: linear-gradient(135deg, #802e25, #ff5b49) !important;
        color: #ffffff !important;
        border: none !important;
        border-radius: 8px !important;
        padding: 10px 22px !important;
        font-size: 0.85rem !important;
        font-weight: 700 !important;
        letter-spacing: 0.04em !important;
        cursor: pointer !important;
        transition: all 0.2s !important;
        box-shadow: 0 3px 10px rgba(128,46,37,0.3) !important;
        white-space: nowrap;
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
        padding: 9px 16px !important;
        font-size: 0.82rem !important;
        font-weight: 700 !important;
        cursor: pointer !important;
        align-self: flex-end;
        white-space: nowrap;
        transition: background 0.2s !important;
      }
      .cp-btn-secundario:hover {
        background: #c4b5a5 !important;
      }
      .cp-btn-primario.btn, .cp-btn-secundario.btn {
        padding: 10px 22px !important;
        margin: 0 !important;
      }
      .cp-btn-secundario.btn {
        padding: 9px 16px !important;
      }

    "))
  ),
  
  # CONTENIDO PRINCIPAL
  uiOutput("main_content")
)

# =====================================
# SERVER INTEGRADO CON AUTENTICACIÓN
# =====================================

server <- function(input, output, session) {
  
  # ============================================== 
  # FUNCIÓN PARA ESCAPAR CARACTERES ESPECIALES
  # ==============================================
  escapar_para_js <- function(texto) {
    if(is.null(texto) || is.na(texto)) return("")
    texto <- as.character(texto)
    texto <- gsub("\\\\", "\\\\\\\\", texto)  # Escapar backslashes
    texto <- gsub("'", "\\\\'",
                  texto)         # Escapar comillas simples
    texto <- gsub('"', '\\\\"', texto)         # Escapar comillas dobles
    texto <- gsub("\n", "\\\\n", texto)        # Escapar saltos de línea
    texto <- gsub("\r", "\\\\r", texto)        # Escapar retornos de carro
    return(texto)
  }
  
  # ============================================== 
  # VARIABLES REACTIVAS PARA PAZ ROBOT
  # ==============================================
  
  paz_archivos <- reactiveValues(
    entrada = NULL,
    bbva = NULL,
    tipo_proceso = NULL,
    procesando = FALSE,
    cancelar_proceso = FALSE,
    archivo_principal = NULL,
    archivo_empresas = NULL, 
    archivo_no_registrados = NULL,
    archivo_consolidado = NULL,
    nombre_base = NULL
  )
  
  valores_proceso <- reactiveValues(
    paso_actual = 0,
    datos_temp = NULL, 
    resultado_temp = NULL,
    ejecutando = FALSE,
    error_mensaje = NULL
  )
  
  timer_proceso <- reactiveTimer(200)  
  
  # ============================================== 
  # VARIABLES REACTIVAS ORIGINALES DE ACTUALIZACION
  # ==============================================
  agenda_df <- reactiveVal(NULL)
  agenda_snowflake_df <- reactiveVal(NULL) 
  rc_datos <- reactiveVal(NULL)
  # Reactivo del clasificador
  cp_rv <- reactiveValues(
    archivo_ruta=NULL, hojas=NULL, filtro_fecha_inicio=NULL,
    filtro_fecha_fin=NULL, cambios=list(), cache_api=list(),
    cache=list(),
    cambios_manual=list(), cambios_filiales=list(),
    manual_filiales_hoja=NULL, manual_filiales_fila_id=NULL,
    modal_rfc=NULL, modal_empresa=NULL, modal_deposito=NULL,
    modal_fila_id=NULL, modal_hoja=NULL, modal_cliente=NULL,
    api_cargando=FALSE, api_resultado=NULL,
    manual_hoja=NULL, manual_fila_id=NULL, manual_deposito_max=0,
    progreso_actual=0, progreso_total=0, clasificando_todo=FALSE,
    sf_ingesta_lista=FALSE,
    filtro_hora_ini="00:00", filtro_hora_fin="23:59"
  )
  
  # Registrar lógica del módulo
  cp_server_logic(input, output, session, cp_rv)
  
  # Cargar hojas desde Snowflake después de que la UI esté lista
  session$onFlushed(function() {
    tryCatch({
      hojas_sf <- mec_hojas_disponibles_sf()
      
      if (!is.null(hojas_sf) && nrow(hojas_sf) > 0) {
        hojas_lista <- hojas_sf$HOJA_EXCEL
        hojas_lista <- hojas_lista[!is.na(hojas_lista) & hojas_lista != ""]
        
        if (length(hojas_lista) > 0) {
          cp_rv$hojas     <- hojas_lista
          cp_rv$cambios   <- list()
          cp_rv$cache_api <- list()
          
          updateSelectInput(session, "cp_sel_hoja",
                            choices  = hojas_lista,
                            selected = hojas_lista[1])
          
          shinyjs::show("cp_panel_principal")
          
          cat("Clasificador cargado desde SF:", length(hojas_lista), "hojas disponibles\n")
        }
      }
    }, error = function(e) {
      cat("Aviso carga inicial SF clasificador:", e$message, "\n")
    })
  }, once = TRUE)
  
  # Carga inicial de no registrados
  session$onFlushed(function() {
    tryCatch({
      df <- mec_cargar_no_registrados()
      if (!is.null(df)) rc_datos(df)
    }, error = function(e) cat("Aviso carga no registrados:", e$message, "\n"))
  }, once = TRUE)
  
  observeEvent(input$btn_ir_clasificacion, {
    updateTabItems(session, "sidebar", "clasificacion_pagos")
  })
  
  # AGREGAR ESTE OBSERVE PARA CARGAR AGENDA SNOWFLAKE:
  observe({
    if(is.null(agenda_snowflake_df())) {
      cargar_agenda_snowflake()
    }
  })
  
  # ============================================== 
  # NUEVOS OBSERVEVENT PARA ACTUALIZAR AGENDA EN SNOWFLAKE
  # ==============================================
  # ObserveEvent 1: Mostrar modal de confirmación  
  observeEvent(input$actualizar_agenda, {
    
    
    datos <- datos_clientes()
    
    if (nrow(datos) == 0) {
      showNotification(" No hay datos de clientes para actualizar en AGENDA", type = "warning", duration = 3)
      return()
    }
    
    #FILTRAR solo registros editados 
    datos_editados <- datos[datos$EDITADO == TRUE, ]
    if (nrow(datos_editados) == 0) {
      showNotification("ℹ️ No hay registros editados para actualizar en Snowflake. Debe editar los registros primero.", type = "warning", duration = 5)
      return()
    }
    
    # Mostrar confirmaciÃ³n antes de actualizar
    showModal(modalDialog(
      title = "📄 Agregar Registros EDITADOS a AGENDA",
      div(
        h4("¿Desea sincronizar los registros editados con Snowflake?"),
        br(),
        p(strong(" Registros editados listos: "), nrow(datos_editados)),
        p(strong(" Destino: "), "Tabla AGENDA en Snowflake"),
        p(strong(" Acción: "), "Solo se agregarán registros NUEVOS"),
        br(),
        div(style = "background: #d4edda; padding: 10px; border-radius: 5px; border-left: 4px solid #28a745;",
            p(style = "margin: 0; color: #155724;", 
              "✅ Los datos existentes en AGENDA se conservarán intactos.")
        ),
        div(style = "background: #d1ecf1; padding: 10px; border-radius: 5px; border-left: 4px solid #0c5460; margin-top: 10px;",
            p(style = "margin: 0; color: #0c5460;", 
              "ℹ️ El sistema comparará identificadores y solo insertará los que no existan.")
        )
      ),
      footer = tagList(
        modalButton("❌ Cancelar"),
        actionButton("confirmar_actualizar_agenda", " Agregar Registros", 
                     class = "btn-success", style = "color: white;")
      ),
      easyClose = FALSE
    ))
  })
  
  # ObserveEvent 2: Ejecutar la actualización
  observeEvent(input$confirmar_actualizar_agenda, {
    
    
    removeModal()
    
    # Mostrar mensaje de procesamiento
    showNotification("🔄 Revisando registros y agregando nuevos a Snowflake...", 
                     type = "message", duration = 4)
    
    # Ejecutar actualización (NUEVA función que NO borra datos)
    resultado <- actualizar_agenda_snowflake()
    
    if(resultado) {
      # Actualizar también la agenda local para PAZ Robot
      datos_actuales <- datos_clientes()
      agenda_actualizada <- data.frame(
        IDENTIFICADOR = datos_actuales$IDENTIFICADOR,
        EMPRESA = datos_actuales$CLIENTE,
        CUENTA = datos_actuales$NUM_CLIENTE,
        stringsAsFactors = FALSE
      )
      agenda_df(agenda_actualizada)
      
      cat("✅ AGENDA actualizada - Snowflake sincronizado con datos locales\n")
    } else {
      showNotification("❌ Hubo un problema al actualizar la AGENDA", type = "error", duration = 4)
    }
  })
  
  # ObserveEvent: Mostrar modal de confirmación para actualización individual
  observeEvent(input$actualizar_agenda_individual, {
    
    
    req(input$actualizar_agenda_individual)
    
    identificador_seleccionado <- extraer_identificador_limpio(input$actualizar_agenda_individual)
    
    if (is.null(identificador_seleccionado) || identificador_seleccionado == "") {
      return()
    }
    
    datos_actuales <- datos_clientes()
    cliente_encontrado <- datos_actuales[datos_actuales$IDENTIFICADOR == identificador_seleccionado, ]
    
    if (nrow(cliente_encontrado) > 0) {
      showModal(modalDialog(
        title = "🔄 Actualizar AGENDA Individual",
        div(
          h4("¿Desea enviar este registro específico a Snowflake?"),
          br(),
          div(style = "background: #e8f5e8; padding: 15px; border-radius: 8px;",
              h5(style = "color: #28a745;", "📋 Datos del Cliente:"),
              p(paste("🔢 Identificador:", cliente_encontrado$IDENTIFICADOR[1])),
              p(paste("👤 Cliente:", if(cliente_encontrado$CLIENTE[1] == "") "-" else cliente_encontrado$CLIENTE[1])),
              p(paste("🏢 Razón Social:", if(cliente_encontrado$RAZON_SOCIAL[1] == "") "-" else cliente_encontrado$RAZON_SOCIAL[1]))
          )
        ),
        footer = tagList(
          modalButton("❌ Cancelar"),
          actionButton("confirmar_actualizar_agenda_individual", "✔️ Actualizar", class = "btn-success")
        ),
        easyClose = FALSE
      ))
      
      session$userData$identificador_agenda_individual <- identificador_seleccionado
    }
  }, ignoreInit = TRUE, priority = 100)
  
  # ============================================== 
  # LÓGICA DE PAZ ROBOT - SOLO SI ESTÁ AUTENTICADO
  # RENDERIZAR CONTENIDO PRINCIPAL BASADO EN AUTENTICACIÓN
  # ==============================================
  
  output$main_content <- renderUI({
    # MOSTRAR APLICACIÓN PRINCIPAL
    dashboardPage(
      dashboardHeader(
        title = "ALMENA"
      ),
      
      # Sidebar con menú expandido
      dashboardSidebar(
        sidebarMenu(
          id = "sidebar",
          menuItem("   ALMENA INTELLIGENCE", tabName = "paz_robot", icon = icon("line-chart")),
          menuItem("   Gestión de Clientes", tabName = "gestion",  icon = icon("home")),
          menuItem("   Agenda ", tabName = "agenda_snowflake", icon = icon("database")),
          menuItem("   Clasificación de Pagos", tabName = "clasificacion_pagos", icon = icon("tags"))
        )
      ),
      
      dashboardBody(
        # Contenido por pestañas (igual que antes)
        tabItems(
          # ============================================== 
          # NUEVA PESTAÑA: PAZ ROBOT
          # ==============================================
          tabItem(tabName = "paz_robot",
                  div(class = "paz-robot-container",
                      div(class = "paz-main-container",
                          # Título principal
                          div(class = "paz-main-title",
                              h1("ALMENA INTELLIGENCE")
                          ),
                          
                          # Sección de archivos
                          div(class = "paz-section-title"
                              #,h4("📁 Configuración del Proceso")
                          ),
                          
                          # ============================================== 
                          # SECCIÓN DE CARGA DE ARCHIVOS (DOS OPCIONES)
                          # ==============================================
                          
                          # Carga via API H2H
                          div(style = "text-align: center; margin-bottom: 25px;",
                              h5("Estado de Cuenta Banco del Istmo", 
                                 style = "color: #802e25; font-weight: bold; margin-bottom: 15px;"),
                              actionButton(
                                "paz_btn_cargar_api",
                                "Cargar Movimientos de EC",
                                class = "btn-paz-execute"
                              ),
                              div(id = "paz_barra_api_wrap",
                                  style = "display:none; align-items:center; gap:16px;
                 padding:14px 24px; background:#FFFFFF;
                 border:1px solid #D4C4B8; border-radius:10px;
                 margin-top:16px;",
                                  div(style = "flex:1;",
                                      div(style = "background:#F0E8DC; border-radius:20px;
                         height:10px; overflow:hidden;",
                                          div(id = "paz_barra_api_fill",
                                              style = "height:10px; border-radius:20px; width:0%;
                             background:linear-gradient(90deg,#8B0000,#B22222);
                             transition:width 0.4s ease;")
                                      )
                                  ),
                                  tags$span(id = "paz_barra_api_texto",
                                            style = "font-size:0.85rem; color:#7A6057; white-space:nowrap;",
                                            "Iniciando...")
                              )
                          ),
                          
                          # Separador visual (se mantiene porque sigue habiendo dos opciones)
                          div(style = "text-align: center; margin: 20px 0;",
                              hr(style = "border-top: 2px solid #dc3545;"),
                              span(style = "color: #6c757d; font-weight: bold;", "-   O   -"),
                              hr(style = "border-top: 2px solid #dc3545;")
                          ),
                          
                          # Opción 2: Estado de Cuenta BANCO_NORTE
                          div(class = "paz-file-input",
                              h5(" Estado de Cuenta BANCO_NORTE", style = "color: #0056b3; font-weight: bold; margin-bottom: 10px;"),
                              fileInput("paz_file_bbva", 
                                        "Cargar Documento",
                                        accept = c(".xlsx", ".xls"),
                                        buttonLabel = "Seleccionar...",
                                        placeholder = "Archivo BANCO_NORTE específico")
                          ),
                          
                          div(id = "paz_preview_archivo", class = "paz-path-preview", style = "display: none;"),
                          
                          # Estado de archivos
                          div(class = "paz-status-box",
                              textOutput("paz_archivos_status")
                          ),
                          
                          # Botón ejecutar
                          actionButton("paz_btn_ejecutar", " Ejecutar Proceso", class = "btn-paz-execute"),
                          
                          # Sección de error
                          div(id = "paz_error_section", class = "paz-error-section", style = "display: none;",
                              div(class = "paz-error-title", "❌ Error en el Proceso"),
                              div(id = "paz_error_message", class = "paz-error-message", 
                                  "Se produjo un error durante el procesamiento de los archivos."
                              ),
                              div(class = "error-actions",
                                  actionButton("paz_btn_retry", "🔄 Intentar Nuevamente", class = "btn-retry"),
                                  actionButton("paz_btn_hide_error", "✖️ Cerrar", class = "btn-close-error")
                              )
                          ),
                          
                          # Sección de descargas
                          # Sección de descargas
                          div(id = "paz_download_section", class = "paz-download-section", style = "display: none;",
                              div(class = "paz-download-title", " Archivos Generados - Listos para Descarga"),
                              
                              # Descargas condicionales según tipo de proceso
                              uiOutput("paz_download_buttons")
                          )
                      )
                  ),
                  
                  # Overlay de procesamiento
                  div(id = "paz-processing-overlay", class = "paz-processing-overlay", style = "display: none;",
                      div(id = "paz-processing-content", class = "paz-processing-spinner",
                          div(class = "paz-spinner", id = "paz-spinner"),
                          div(class = "paz-processing-text", id = "paz-processing-text", "🤖 Procesando..."),
                          div(class = "processing-subtitle", id = "paz-processing-subtitle", "Su proceso está ejecutándose, por favor espere..."),
                          div(class = "paz-progress-container", id = "paz-progress-container",
                              div(class = "paz-progress-bar", id = "paz-progress-bar")
                          ),
                          div(class = "progress-text", id = "paz-progress-text", "0% - Iniciando..."),
                          div(class = "progress-step", id = "paz-progress-step", "Preparando el sistema...")
                      )
                  ),
                  
                  # Barra de sincronizacion con Snowflake (oculta inicialmente)
                  div(id = "paz_barra2_wrap",
                      style = "display:none; align-items:center; gap:16px;
             padding:10px 24px; background:#FFFFFF;
             border-top:1px solid #D4C4B8;
             border-radius:0 0 10px 10px;",
                      div(style = "flex:1;",
                          div(style = "background:#F0E8DC;border-radius:20px;
                     height:8px;overflow:hidden;",
                              div(id = "paz_barra2_fill",
                                  style = "height:8px;border-radius:20px;width:0%;
                         background:linear-gradient(90deg,#8B0000,#B22222);
                         transition:width 0.5s ease;")
                          )
                      ),
                      tags$span(id = "paz_barra2_texto",
                                style = "font-size:0.82rem;color:#7A6057;white-space:nowrap;",
                                "Sincronizando con Snowflake...")
                  )
          ),
          
          # ============================================== 
          # PESTAÑAS ORIGINALES DE ACTUALIZACION (mantenidas completas)
          # ==============================================
          
          # PESTAÑA: GESTIÓN PRINCIPAL
          tabItem(tabName = "gestion",
                  fluidRow(
                    box(
                      title = "Clientes No Registrados", status = "danger", 
                      solidHeader = TRUE, width = 12,
                      
                      div(style = "background: linear-gradient(135deg, #fdf5f4 0%, #f5e8e6 100%);
               padding: 12px 16px; border-radius: 8px; 
               border-left: 4px solid #802e25; margin-bottom: 16px;",
                          p(style = "margin: 0; color: #401712; font-size: 0.9rem;",
                            "Clientes detectados en movimientos sin registro en Agenda. 
         Presiona Registrar para darlos de alta.")
                      ),
                      
                      div(style = "margin-bottom: 12px;",
                          actionButton("rc_btn_actualizar", "Actualizar tabla",
                                       class = "btn-danger",
                                       icon = icon("sync"),
                                       style = "font-size: 0.85rem;")
                      ),
                      
                      DT::dataTableOutput("rc_tabla_no_registrados"),
                      
                      # Contador
                      div(style = "background-color: #f5ece8; padding: 12px; border-radius: 8px;
               border-left: 4px solid #802e25; margin-top: 12px;",
                          textOutput("rc_contador"))
                    )
                  )
          ),
          
          # PESTAÑA: AGENDA
          tabItem(tabName = "agenda_snowflake",
                  fluidRow(
                    box(
                      title = " AGENDA desde Snowflake", status = "danger", solidHeader = TRUE,
                      width = 12,
                      
                      div(style = "background: linear-gradient(135deg, #fdf5f4 0%, #f5e8e6 100%);
             padding: 15px; border-radius: 8px; border-left: 4px solid #802e25;",
                          p(style = "margin: 5px 0; color: #401712;",
                            "📡 Datos obtenidos directamente desde Snowflake"),
                          p(style = "margin: 5px 0; color: #401712;", "🔒 Solo visualización - No editable"),
                          p(style = "margin: 5px 0; color: #401712;",
                            "🔄 Se actualiza automáticamente al iniciar sesión")
                      ),
                    ),
                    
                    fluidRow(
                      column(6,
                             actionButton("actualizar_agenda_snowflake", " Actualizar datos", 
                                          class = "btn-danger", icon = icon("sync"))
                      ),
                      column(4,
                             downloadButton("descargar_agenda_snowflake", " Descargar Excel",
                                            style = "background: linear-gradient(135deg, #802e25 0%, #401712 100%);
                        color: white !important; font-weight: bold; border: none;
                        padding: 8px 20px; border-radius: 6px;")
                      ),
                    ),
                    
                    br(),
                    
                    DT::dataTableOutput("tabla_agenda_snowflake"),
                    
                    br(),
                    
                    div(style = "background-color: #f5ece8; padding: 15px; border-radius: 8px;
             border-left: 4px solid #802e25;",
                        textOutput("contador_agenda_snowflake"))
                  )
          ), # ← Cierre del tabItem agenda_snowflake
          
          # PESTAÑA: CLASIFICACIÓN DE PAGOS
          cp_ui_tab()
          
        ) # ← Cierre de tabItems
      )  # ← Cierre de dashboardBody 
    ) # ← Cierre de dashboardPage
  }) # ← Cierre de renderUI
  
  # ============================================== 
  # FUNCIONES AUXILIARES PARA PAZ ROBOT
  # ==============================================
  # Función mejorada para mostrar mensaje temporal de éxito
  mostrar_mensaje_temporal_exito <- function(mensaje, duracion = 3000) {
    cat("🎉 MOSTRANDO MENSAJE DE ÉXITO:", mensaje, "\n")
    
    mensaje_html <- paste0('
          <div class="paz-mensaje-temporal" id="paz-mensaje-temporal">
            <span class="mensaje-icono">🎉</span>
            <div>', mensaje, '</div>
            <div class="mensaje-subtitulo">Los archivos están listos para descarga</div>
          </div>
        ')
    
    ejecutar_js_seguro(paste0("
          var mensaje = document.createElement('div');
          mensaje.innerHTML = '", gsub("'", "\\'", mensaje_html), "';
          document.body.appendChild(mensaje.firstChild);
          
          setTimeout(function() {
            var msgElement = document.getElementById('paz-mensaje-temporal');
            if (msgElement) {
              msgElement.classList.add('desapareciendo');
              setTimeout(function() {
                if (msgElement.parentNode) {
                  msgElement.parentNode.removeChild(msgElement);
                }
              }, 500);
            }
          }, ", duracion, ");
        "))
  }
  
  # Función mejorada para mostrar mensaje temporal de error
  mostrar_mensaje_temporal_error <- function(mensaje, duracion = 4000) {
    cat("💥 MOSTRANDO MENSAJE DE ERROR TEMPORAL:", mensaje, "\n")
    
    mensaje_limpio <- limpiar_texto_para_js(mensaje)
    
    mensaje_html <- paste0('
          <div class="paz-mensaje-temporal error" id="paz-mensaje-temporal">
            <span class="mensaje-icono">💥</span>
            <div>', mensaje_limpio, '</div>
            <div class="mensaje-subtitulo">Revise los archivos y la configuración</div>
          </div>
        ')
    
    ejecutar_js_seguro(paste0("
          var msgAnterior = document.getElementById('paz-mensaje-temporal');
          if (msgAnterior && msgAnterior.parentNode) {
            msgAnterior.parentNode.removeChild(msgAnterior);
          }
          
          var mensaje = document.createElement('div');
          mensaje.innerHTML = '", gsub("'", "\\'", mensaje_html), "';
          document.body.appendChild(mensaje.firstChild);
          
          setTimeout(function() {
            var msgElement = document.getElementById('paz-mensaje-temporal');
            if (msgElement) {
              msgElement.classList.add('desapareciendo');
              setTimeout(function() {
                if (msgElement.parentNode) {
                  msgElement.parentNode.removeChild(msgElement);
                }
              }, 500);
            }
          }, ", duracion, ");
        "))
  }
  
  # Función mejorada para mostrar sección de error en pantalla
  mostrar_error_pantalla <- function(mensaje_error) {
    cat("📺 MOSTRANDO ERROR EN PANTALLA:", mensaje_error, "\n")
    
    mensaje_limpio <- limpiar_texto_para_js(mensaje_error)
    
    tryCatch({
      shinyjs::html("paz_error_message", mensaje_limpio)
      shinyjs::show("paz_error_section")
      
      ejecutar_js_seguro("
            setTimeout(function() {
              var errorSection = document.getElementById('paz_error_section');
              if (errorSection) {
                errorSection.scrollIntoView({ 
                  behavior: 'smooth', 
                  block: 'center' 
                });
              }
            }, 100);
          ")
      
      cat("✅ SECCIÓN DE ERROR MOSTRADA EXITOSAMENTE\n")
    }, error = function(e) {
      cat("❌ ERROR AL MOSTRAR SECCIÓN DE ERROR:", e$message, "\n")
      showNotification(mensaje_limpio, type = "error", duration = 10)
    })
  }
  
  # Función para ocultar sección de error
  ocultar_error_pantalla <- function() {
    cat("👁️ OCULTANDO SECCIÓN DE ERROR\n")
    tryCatch({
      shinyjs::hide("paz_error_section")
    }, error = function(e) {
      cat("❌ ERROR AL OCULTAR SECCIÓN:", e$message, "\n")
    })
  }
  
  # Función auxiliar para limpiar texto para JavaScript
  limpiar_texto_para_js <- function(texto) {
    texto <- gsub("❌ ", "", texto)
    texto <- gsub("'", "&#39;", texto)
    texto <- gsub('"', '&quot;', texto)
    texto <- gsub("\n", " ", texto)
    texto <- gsub("\r", "", texto)
    return(texto)
  }
  
  # Función auxiliar para ejecutar JavaScript de forma segura
  ejecutar_js_seguro <- function(codigo_js) {
    tryCatch({
      runjs(codigo_js)
      return(TRUE)
    }, error = function(e) {
      cat("❌ ERROR AL EJECUTAR JAVASCRIPT:", e$message, "\n")
      return(FALSE)
    })
  }
  
  # Funciones de procesamiento
  mostrar_procesamiento <- function() {
    paz_archivos$procesando <- TRUE
    
    ejecutar_js_seguro("document.getElementById('paz-processing-overlay').style.display = 'flex';")
    ejecutar_js_seguro("document.getElementById('paz_download_section').style.display = 'none';")
    
    actualizar_progreso(0, "Iniciando proceso...", "Por favor espere, el proceso no puede interrumpirse")
  }
  
  ocultar_procesamiento <- function() {
    paz_archivos$procesando <- FALSE
    ejecutar_js_seguro("document.getElementById('paz-processing-overlay').style.display = 'none';")
  }
  
  actualizar_progreso <- function(porcentaje, texto, paso) {
    js_code <- paste0(
      "document.getElementById('paz-progress-bar').style.width = '", porcentaje, "%';",
      "document.getElementById('paz-progress-text').textContent = '", porcentaje, "% - ", texto, "';",
      "document.getElementById('paz-progress-step').textContent = '", paso, "';"
    )
    ejecutar_js_seguro(js_code)
    Sys.sleep(0.1)
  }
  
  # ============================================================================
  # FUNCIÓN: Cargar Agenda desde Snowflake (CON JWT - SIN LOGIN)
  # ============================================================================
  cargar_agenda_snowflake <- function() {
    tryCatch({
      # Conectar a Snowflake usando JWT (SIN usuario/password)
      conn <- crear_conexion_snowflake()
      
      # Leer la tabla de AGENDA
      agenda <- dbReadTable(conn, "AGENDA")
      
      # Cerrar conexión
      dbDisconnect(conn)
      
      # Actualizar variable reactiva
      agenda_snowflake_df(agenda)
      
      showNotification("✅ Agenda cargada desde Snowflake", type = "message", duration = 3)
      cat("✅ Agenda Snowflake cargada:", nrow(agenda), "registros\n")
      
      return(TRUE)
      
    }, error = function(e) {
      cat("❌ Error al cargar agenda desde Snowflake:", e$message, "\n")
      showNotification(paste("Error al cargar agenda:", e$message), type = "error", duration = 5)
      return(FALSE)
    })
  }
  
  # ==============================================================================
  # FUNCIÓN CORREGIDA PARA ACTUALIZAR AGENDA EN SNOWFLAKE (SIN dbGetRowsAffected)
  # ==============================================================================
  # ============================================================================
  # FUNCIÓN: Actualizar Agenda en Snowflake (CON JWT - SIN LOGIN)
  # ============================================================================
  actualizar_agenda_snowflake <- function() {
    tryCatch({
      # Obtener datos actuales de clientes
      datos_clientes_actual <- datos_clientes()
      
      # FILTRAR SOLO REGISTROS EDITADOS
      datos_editados <- datos_clientes_actual[datos_clientes_actual$EDITADO == TRUE, ]
      
      if(nrow(datos_editados) == 0) {
        showNotification("ℹ️ No hay registros editados para actualizar en Snowflake", 
                         type = "warning", duration = 3)
        return(FALSE)
      }
      
      datos_clientes_actual <- datos_editados
      
      # Conectar a Snowflake usando JWT
      conn <- crear_conexion_snowflake()
      
      # FORZAR WAREHOUSE ACTIVO
      dbSendUpdate(conn, "USE WAREHOUSE WH_ANALYTICS")
      
      # Leer agenda existente
      agenda_existente <- tryCatch({
        dbReadTable(conn, "AGENDA")
      }, error = function(e) {
        data.frame(IDENTIFICADOR = character(0), stringsAsFactors = FALSE)
      })
      
      identificadores_existentes <- if(nrow(agenda_existente) > 0 && 
                                       "IDENTIFICADOR" %in% names(agenda_existente)) {
        agenda_existente$IDENTIFICADOR
      } else {
        character(0)
      }
      
      cat("📊 Identificadores existentes en Snowflake:", length(identificadores_existentes), "\n")
      
      # Filtrar solo los registros NUEVOS
      identificadores_locales <- datos_clientes_actual$IDENTIFICADOR
      registros_nuevos <- datos_clientes_actual[!identificadores_locales %in% identificadores_existentes, ]
      
      num_registros_a_insertar <- nrow(registros_nuevos)
      
      cat("📊 Registros locales:", nrow(datos_clientes_actual), "\n")
      cat("🆕 Registros nuevos a insertar:", num_registros_a_insertar, "\n")
      
      if(num_registros_a_insertar == 0) {
        dbDisconnect(conn)
        showNotification("ℹ️ Todos los registros ya existen en Snowflake. No hay nada nuevo que agregar.",
                         type = "message", duration = 4)
        return(TRUE)
      }
      
      # Preparar datos para insertar
      datos_para_insertar <- registros_nuevos %>%
        select(IDENTIFICADOR, NUM_CLIENTE, CLIENTE, RAZON_SOCIAL) %>%
        mutate(
          NUM_CLIENTE = as.character(NUM_CLIENTE),
          CLIENTE = as.character(CLIENTE),
          RAZON_SOCIAL = as.character(RAZON_SOCIAL)
        )
      
      # Construir e insertar filas
      filas_insertadas <- 0
      for(i in 1:nrow(datos_para_insertar)) {
        fila <- datos_para_insertar[i, ]
        
        identificador_valor <- if(is.na(fila$IDENTIFICADOR) || fila$IDENTIFICADOR == "") 
          "NULL" else paste0("'", fila$IDENTIFICADOR, "'")
        num_cliente_valor <- if(is.na(fila$NUM_CLIENTE) || fila$NUM_CLIENTE == "") 
          "NULL" else paste0("'", fila$NUM_CLIENTE, "'")
        cliente_valor <- if(is.na(fila$CLIENTE) || fila$CLIENTE == "") 
          "NULL" else paste0("'", gsub("'", "''", fila$CLIENTE), "'")
        razon_social_valor <- if(is.na(fila$RAZON_SOCIAL) || fila$RAZON_SOCIAL == "") 
          "NULL" else paste0("'", gsub("'", "''", fila$RAZON_SOCIAL), "'")
        
        query_insert <- sprintf(
          "INSERT INTO AGENDA (IDENTIFICADOR, NO_CLIENTE, CLIENTE, RAZON_SOCIAL) VALUES (%s, %s, %s, %s)",
          identificador_valor, num_cliente_valor, cliente_valor, razon_social_valor
        )
        
        tryCatch({
          dbSendUpdate(conn, query_insert)
          filas_insertadas <- filas_insertadas + 1
          cat("✅ Fila", i, "insertada exitosamente\n")
        }, error = function(e) {
          cat("❌ Error insertando fila", i, ":", e$message, "\n")
        })
      }
      
      # Cerrar conexión
      dbDisconnect(conn)
      
      # Mostrar resultado
      cat("✅ Proceso completado:", filas_insertadas, "registros insertados\n")
      
      if(filas_insertadas > 0) {
        showNotification(
          paste("✅", filas_insertadas, "registros nuevos agregados a Snowflake"),
          type = "message", duration = 4
        )
        
        # Recargar agenda actualizada
        cargar_agenda_snowflake()
      } else {
        showNotification(
          "⚠️ No se pudo insertar ningún registro. Revise los logs.",
          type = "warning", duration = 4
        )
      }
      
      return(TRUE)
      
    }, error = function(e) {
      cat("❌ Error al actualizar agenda en Snowflake:", e$message, "\n")
      showNotification(paste("Error al actualizar AGENDA:", e$message), 
                       type = "error", duration = 5)
      return(FALSE)
    })
  }
  
  # ============================================================================
  # FUNCIÓN: Actualizar un Registro Individual en Snowflake (CON JWT)
  # ============================================================================
  actualizar_agenda_individual_snowflake <- function(identificador_objetivo) {
    tryCatch({
      # Obtener datos del cliente específico
      datos_clientes_actual <- datos_clientes()
      cliente_objetivo <- datos_clientes_actual[datos_clientes_actual$IDENTIFICADOR == identificador_objetivo, ]
      
      if(nrow(cliente_objetivo) == 0) {
        cat("❌ No se encontró el cliente con identificador:", identificador_objetivo, "\n")
        return(FALSE)
      }
      
      # Conectar a Snowflake usando JWT (SIN usuario/password)
      conn <- crear_conexion_snowflake()
      
      # Activar warehouse
      dbSendUpdate(conn, "USE WAREHOUSE WH_ANALYTICS")
      
      # Preparar datos del registro individual
      identificador_limpio <- gsub("'", "''", cliente_objetivo$IDENTIFICADOR)
      no_cliente_limpio <- gsub("'", "''", ifelse(cliente_objetivo$NUM_CLIENTE == "" | is.na(cliente_objetivo$NUM_CLIENTE), "-", cliente_objetivo$NUM_CLIENTE))
      cliente_limpio <- gsub("'", "''", ifelse(cliente_objetivo$CLIENTE == "" | is.na(cliente_objetivo$CLIENTE), "-", cliente_objetivo$CLIENTE))
      razon_social_limpia <- gsub("'", "''", ifelse(cliente_objetivo$RAZON_SOCIAL == "" | is.na(cliente_objetivo$RAZON_SOCIAL), "-", cliente_objetivo$RAZON_SOCIAL))
      
      # Verificar si ya existe el registro
      existe_sql <- paste0("SELECT COUNT(*) as count FROM AGENDA WHERE IDENTIFICADOR = '", identificador_limpio, "'")
      resultado_existe <- dbGetQuery(conn, existe_sql)
      ya_existe <- resultado_existe$COUNT > 0
      
      if(ya_existe) {
        # Actualizar registro existente
        update_sql <- paste0(
          "UPDATE AGENDA SET ",
          "NO_CLIENTE = '", no_cliente_limpio, "', ",
          "CLIENTE = '", cliente_limpio, "', ",
          "RAZON_SOCIAL = '", razon_social_limpia, "' ",
          "WHERE IDENTIFICADOR = '", identificador_limpio, "'"
        )
        
        tryCatch({
          dbSendUpdate(conn, update_sql)
          cat("✅ UPDATE ejecutado exitosamente\n")
        }, error = function(e) {
          cat("❌ Error en UPDATE SQL:", e$message, "\n")
          dbDisconnect(conn)
          stop("Error al actualizar registro: ", e$message)
        })
        cat("🔄 Registro actualizado en AGENDA:", identificador_objetivo, "\n")
        
      } else {
        # Insertar nuevo registro
        insert_sql <- paste0(
          "INSERT INTO AGENDA (IDENTIFICADOR, NO_CLIENTE, CLIENTE, RAZON_SOCIAL) VALUES ('",
          identificador_limpio, "', '", no_cliente_limpio, "', '", cliente_limpio, "', '", razon_social_limpia, "')"
        )
        
        tryCatch({
          dbSendUpdate(conn, insert_sql)
          cat("✅ INSERT ejecutado exitosamente\n")
        }, error = function(e) {
          cat("❌ Error en INSERT SQL:", e$message, "\n")
          dbDisconnect(conn)
          stop("Error al insertar registro: ", e$message)
        })
        cat("➕ Nuevo registro insertado en AGENDA:", identificador_objetivo, "\n")
      }
      
      # Cerrar conexión
      dbDisconnect(conn)
      
      # ✅ ELIMINAR EL REGISTRO LOCAL DESPUÉS DE CARGA EXITOSA
      datos_locales_actuales <- datos_clientes()
      datos_locales_filtrados <- datos_locales_actuales[datos_locales_actuales$IDENTIFICADOR != identificador_objetivo, ]
      
      # Actualizar datos locales
      datos_clientes(datos_locales_filtrados)
      
      # Guardar los cambios
      tryCatch({
        saveRDS(datos_locales_filtrados, "clientes_data.rds")
        cat("🗑️ Registro local eliminado después de cargar en Snowflake:", identificador_objetivo, "\n")
      }, error = function(e) {
        cat("⚠️ Error al eliminar registro local:", e$message, "\n")
      })
      
      # Actualizar agenda local también
      if(nrow(datos_locales_filtrados) > 0) {
        agenda_local_actualizada <- data.frame(
          IDENTIFICADOR = datos_locales_filtrados$IDENTIFICADOR,
          EMPRESA = datos_locales_filtrados$CLIENTE,
          CUENTA = datos_locales_filtrados$NUM_CLIENTE,
          stringsAsFactors = FALSE
        )
        agenda_df(agenda_local_actualizada)
      } else {
        agenda_df(data.frame(
          IDENTIFICADOR = character(0),
          EMPRESA = character(0),
          CUENTA = character(0),
          stringsAsFactors = FALSE
        ))
      }
      
      return(TRUE)
      
    }, error = function(e) {
      cat("❌ Error al actualizar registro individual en Snowflake:", e$message, "\n")
      showNotification(paste("Error al enviar registro:", e$message), type = "error", duration = 5)
      return(FALSE)
    })
  }
  
  # ObserveEvent: Ejecutar actualización individual a Snowflake
  observeEvent(input$confirmar_actualizar_agenda_individual, {
    
    
    identificador_objetivo <- session$userData$identificador_agenda_individual
    
    if (!is.null(identificador_objetivo)) {
      removeModal()
      
      showNotification("🔄 Enviando registro a Snowflake...", type = "message", duration = 3)
      
      resultado <- tryCatch({
        actualizar_agenda_individual_snowflake(identificador_objetivo)
      }, error = function(e) {
        cat("❌ ERROR:", e$message, "\n")
        showNotification(paste("Error:", e$message), type = "error", duration = 5)
        return(FALSE)
      })
      
      if(resultado) {
        tryCatch({
          cargar_agenda_snowflake()
        }, error = function(e) {
          cat("⚠️ Error al recargar:", e$message, "\n")
        })
        
        showNotification("✅ ¡Registro enviado exitosamente!", type = "message", duration = 4)
      }
    }
    
    session$userData$identificador_agenda_individual <- NULL
    
  }, ignoreInit = TRUE, priority = 100)
  
  #-------------------------------------------------#
  # LÓGICA DE PAZ ROBOT - SOLO SI ESTÁ AUTENTICADO#
  #-------------------------------------------------#
  
  # Cuando se carga archivo BANCO_NORTE
  observeEvent(input$paz_file_bbva, {
    
    
    if(!is.null(input$paz_file_bbva)) {
      # Limpiar el otro input
      shinyjs::reset("paz_file_entrada")
      paz_archivos$entrada <- NULL
      
      # Configurar archivo actual
      paz_archivos$bbva <- input$paz_file_bbva
      paz_archivos$tipo_proceso <- "BANCO_NORTE"
      paz_archivos$nombre_base <- tools::file_path_sans_ext(input$paz_file_bbva$name)
      
      preview_html <- paste(
        "📁 <strong>Archivo que se procesará (BANCO_NORTE):</strong><br>",
        "&nbsp;&nbsp;• 🏦", paste0(paz_archivos$nombre_base, "_modificado.xlsx")
      )
      
      ejecutar_js_seguro(paste0(
        "document.getElementById('paz_preview_archivo').innerHTML = '", preview_html, "';",
        "document.getElementById('paz_preview_archivo').style.display = 'block';"
      ))
    }
  })
  
  output$paz_download_buttons <- renderUI({
    
    if(paz_archivos$tipo_proceso == "CONSOLIDADO") {
      div(class = "download-buttons",
          #downloadButton("paz_download_principal", " Archivo Principal"),
          downloadButton("paz_download_empresas", " Por Empresas"),
          downloadButton("paz_download_consolidado", " Consolidado"),
          downloadButton("paz_download_no_registrados", "❌ No Registrados")
      )
    } else if(paz_archivos$tipo_proceso == "BANCO_NORTE") {
      # ============================================== 
      # BOTÓN PARA BANCO_NORTE
      # ==============================================
      div(class = "download-buttons",
          downloadButton("paz_download_bbva", " Archivo BANCO_NORTE Procesado", 
                         class = "btn-paz-download",
                         style = "min-width: 250px;"),
          downloadButton("paz_download_bbva_no_registrados", "❌ No Registrados BANCO_NORTE", 
                         class = "btn-paz-download",
                         style = "min-width: 250px;")
      )
      
    } else {
      div()
    }
  })
  
  observeEvent(input$paz_btn_cargar_api, {
    showModal(modalDialog(
      title = "Confirmar carga",
      paste0("Se descargarán todos los movimientos del día de hoy (",
             format(Sys.Date(), "%d/%m/%Y"),
             ") y se sincronizarán con Snowflake. El aplicativo quedará bloqueado durante el proceso."),
      footer = tagList(
        modalButton("Cancelar"),
        actionButton("confirmar_cargar_api", "Sí, cargar", class = "btn-danger")
      )
    ))
  })
  
  observeEvent(input$confirmar_cargar_api, {
    removeModal()
    shinyjs::disable("paz_btn_cargar_api")
    
    # Mostrar overlay bloqueante (el mismo que usa PAZ Robot)
    ejecutar_js_seguro("document.getElementById('paz-processing-overlay').style.display = 'flex';")
    actualizar_progreso(5, "Conectando con la API...", "Descargando movimientos del dia")
    
    # Mostrar barra secundaria
    shinyjs::runjs("
    var w = document.getElementById('paz_barra_api_wrap');
    if(w){ w.style.display = 'flex'; }
    document.getElementById('paz_barra_api_fill').style.width = '10%';
    document.getElementById('paz_barra_api_texto').textContent = 'Descargando movimientos...';
  ")
    
    future_promise({
      df_api <- h2h_get_movimientos_dia(Sys.Date())
      if (is.null(df_api) || nrow(df_api) == 0) stop("La API no devolvio movimientos para hoy")
      
      conn <- crear_conexion_snowflake()
      on.exit(tryCatch(dbDisconnect(conn), error = function(e) NULL))
      dbSendUpdate(conn, "USE WAREHOUSE WH_ANALYTICS")
      dbSendUpdate(conn, "USE DATABASE DB_ANALYTICS")
      dbSendUpdate(conn, "USE SCHEMA SCH_CORE")
      
      df_enriquecido <- h2h_transformar_y_enriquecer(df_api, conn, usuario = "app_user")
      
      TAMANO_LOTE <- 200
      esc <- function(x) {
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
      for (i in seq_len(nrow(df_enriquecido))) {
        cuenta_val <- as.character(df_enriquecido$CUENTA[i])
        mov_val    <- suppressWarnings(as.integer(df_enriquecido$NUM_MOVIMIENTO[i]))
        if (is.na(cuenta_val) || cuenta_val == "" || is.na(mov_val)) next
        v <- paste0("(",
                    esc(df_enriquecido$EMPRESA[i]),              ",",
                    "'", gsub("'","''", cuenta_val),              "',",
                    esc_date(df_enriquecido$FECHA_OPERACION[i]), ",",
                    esc_date(df_enriquecido$FECHA[i]),           ",",
                    esc(df_enriquecido$REFERENCIA[i]),           ",",
                    esc(df_enriquecido$DESCRIPCION[i]),          ",",
                    esc(df_enriquecido$COD_TRANSAC[i]),          ",",
                    esc(df_enriquecido$SUCURSAL[i]),             ",",
                    esc_num(df_enriquecido$DEPOSITOS[i]),        ",",
                    esc_num(df_enriquecido$RETIROS[i]),          ",",
                    esc_num(df_enriquecido$SALDO[i]),            ",",
                    mov_val,                                      ",",
                    esc(df_enriquecido$DESCRIPCION_DETALLADA[i]),",",
                    esc(df_enriquecido$IDENTIFICADOR[i]),        ",",
                    esc(df_enriquecido$CLIENTE[i]),              ",",
                    esc(df_enriquecido$RAZON_SOCIAL[i]),         ",",
                    esc(df_enriquecido$NO_CLIENTE[i]),           ",",
                    esc(df_enriquecido$HOJA_EXCEL[i]),           ",",
                    "CURRENT_TIMESTAMP()",
                    ")"
        )
        valores_validos <- c(valores_validos, v)
      }
      
      total_insertados <- 0
      if (length(valores_validos) > 0) {
        lotes <- split(valores_validos, ceiling(seq_along(valores_validos) / TAMANO_LOTE))
        for (lote in lotes) {
          sql <- paste0(
            "MERGE INTO DB_ANALYTICS.SCH_CORE.MOVIMIENTOS_EC2 AS t ",
            "USING (SELECT * FROM VALUES ",
            paste(lote, collapse = ","),
            " AS s(EMPRESA,CUENTA,FECHA_OPERACION,FECHA,REFERENCIA,DESCRIPCION,",
            "COD_TRANSAC,SUCURSAL,DEPOSITOS,RETIROS,SALDO,NUM_MOVIMIENTO,",
            "DESCRIPCION_DETALLADA,IDENTIFICADOR,CLIENTE,RAZON_SOCIAL,",
            "NO_CLIENTE,HOJA_EXCEL,FECHA_CARGA)) AS s ",
            "ON (t.CUENTA = s.CUENTA AND t.NUM_MOVIMIENTO = s.NUM_MOVIMIENTO) ",
            "WHEN MATCHED THEN UPDATE SET ",
            "  t.EMPRESA=s.EMPRESA,t.FECHA_OPERACION=s.FECHA_OPERACION,",
            "  t.FECHA=s.FECHA,t.REFERENCIA=s.REFERENCIA,",
            "  t.DESCRIPCION=s.DESCRIPCION,t.COD_TRANSAC=s.COD_TRANSAC,",
            "  t.SUCURSAL=s.SUCURSAL,t.DEPOSITOS=s.DEPOSITOS,",
            "  t.RETIROS=s.RETIROS,t.SALDO=s.SALDO,",
            "  t.DESCRIPCION_DETALLADA=s.DESCRIPCION_DETALLADA,",
            "  t.IDENTIFICADOR=s.IDENTIFICADOR,t.CLIENTE=s.CLIENTE,",
            "  t.RAZON_SOCIAL=s.RAZON_SOCIAL,t.NO_CLIENTE=s.NO_CLIENTE,",
            "  t.HOJA_EXCEL=s.HOJA_EXCEL,t.FECHA_CARGA=s.FECHA_CARGA ",
            "WHEN NOT MATCHED THEN INSERT (",
            "EMPRESA,CUENTA,FECHA_OPERACION,FECHA,REFERENCIA,DESCRIPCION,",
            "COD_TRANSAC,SUCURSAL,DEPOSITOS,RETIROS,SALDO,NUM_MOVIMIENTO,",
            "DESCRIPCION_DETALLADA,IDENTIFICADOR,CLIENTE,RAZON_SOCIAL,",
            "NO_CLIENTE,HOJA_EXCEL,FECHA_CARGA",
            ") VALUES (",
            "s.EMPRESA,s.CUENTA,s.FECHA_OPERACION,s.FECHA,s.REFERENCIA,s.DESCRIPCION,",
            "s.COD_TRANSAC,s.SUCURSAL,s.DEPOSITOS,s.RETIROS,s.SALDO,s.NUM_MOVIMIENTO,",
            "s.DESCRIPCION_DETALLADA,s.IDENTIFICADOR,s.CLIENTE,s.RAZON_SOCIAL,",
            "s.NO_CLIENTE,s.HOJA_EXCEL,s.FECHA_CARGA",
            ");"
          )
          tryCatch(dbSendUpdate(conn, sql), error = function(e) cat("Error lote emergencia:", e$message, "\n"))
          total_insertados <- total_insertados + length(lote)
        }
      }
      
      list(
        insertados = total_insertados,
        cuentas    = unique(df_enriquecido$CUENTA)
      )
      
    }) %...>% (function(res) {
      
      ejecutar_js_seguro("document.getElementById('paz-processing-overlay').style.display = 'none';")
      shinyjs::runjs("
      document.getElementById('paz_barra_api_fill').style.width = '100%';
      document.getElementById('paz_barra_api_texto').textContent = 'Movimientos sincronizados con Snowflake';
    ")
      shinyjs::delay(2500, {
        shinyjs::runjs("
        var w = document.getElementById('paz_barra_api_wrap');
        if(w){ w.style.display = 'none'; }
      ")
      })
      
      shinyjs::enable("paz_btn_cargar_api")
      
      # Actualizar el clasificador de pagos con las cuentas del día
      hojas_sf <- tryCatch(mec_hojas_disponibles_sf(), error = function(e) NULL)
      if (!is.null(hojas_sf) && nrow(hojas_sf) > 0) {
        hojas_lista <- hojas_sf$HOJA_EXCEL
        hojas_lista <- hojas_lista[!is.na(hojas_lista) & hojas_lista != ""]
        if (length(hojas_lista) > 0) {
          cp_rv$hojas   <- hojas_lista
          cp_rv$cambios <- list()
          cp_rv$cache   <- list()
          updateSelectInput(session, "cp_sel_hoja",
                            choices  = hojas_lista,
                            selected = hojas_lista[1])
          shinyjs::show("cp_panel_principal")
        }
      }
      
      showNotification(
        paste0(res$insertados, " movimientos cargados. Puedes ir a Clasificacion de Pagos."),
        type = "message", duration = 6
      )
      
    }) %...!% (function(err) {
      ejecutar_js_seguro("document.getElementById('paz-processing-overlay').style.display = 'none';")
      shinyjs::runjs("
      document.getElementById('paz_barra_api_fill').style.width = '100%';
      document.getElementById('paz_barra_api_texto').textContent = 'Error en la carga. Revisar logs.';
    ")
      shinyjs::enable("paz_btn_cargar_api")
      showNotification(paste("Error:", err$message), type = "error", duration = 8)
      cat("Error carga API H2H:", err$message, "\n")
    })
  })
  
  # Configurar procesamiento asíncrono
  plan(multisession)
  
  # ============================================== 
  # PROCESAMIENTO POR PASOS PARA PAZ ROBOT
  # ==============================================
  
  observe({
    timer_proceso()  # Activar el timer
    
    
    if(!valores_proceso$ejecutando) return()
    if(paz_archivos$cancelar_proceso) {
      # Cancelación inmediata
      valores_proceso$ejecutando <- FALSE
      paz_archivos$procesando <- FALSE
      ocultar_procesamiento()
      mostrar_mensaje_temporal_error("Proceso cancelado por el usuario", 3000)
      shinyjs::enable("paz_btn_ejecutar")
      return()
    }
    
    tryCatch({
      if(valores_proceso$paso_actual == 1) {
        # Paso 1: Validaciones iniciales
        actualizar_progreso(5, "Iniciando validaciones...", "Verificando archivos")
        
        # Determinar qué archivo procesar
        if(!is.null(paz_archivos$entrada)) {
          archivo_a_procesar <- paz_archivos$entrada
        } else if(!is.null(paz_archivos$bbva)) {
          archivo_a_procesar <- paz_archivos$bbva
        } else {
          stop("Debe seleccionar un archivo")
        }
        
        if(!file.exists(archivo_a_procesar$datapath)) {
          stop("El archivo no está accesible")
        }
        
        valores_proceso$paso_actual <- 2
      } 
      
      else if(valores_proceso$paso_actual == 2) {
        # Paso 2: Verificar formato Excel
        actualizar_progreso(15, "Verificando formato...", "Validando archivo Excel")
        
        # Determinar qué archivo verificar
        if(!is.null(paz_archivos$entrada)) {
          archivo_a_verificar <- paz_archivos$entrada$datapath
        } else if(!is.null(paz_archivos$bbva)) {
          archivo_a_verificar <- paz_archivos$bbva$datapath
        } else {
          stop("No se encontró archivo para verificar")
        }
        
        hojas_entrada <- excel_sheets(archivo_a_verificar)
        if(length(hojas_entrada) == 0) {
          stop("El archivo no contiene hojas válidas")
        }
        
        valores_proceso$paso_actual <- 3
        
      } else if(valores_proceso$paso_actual == 3) {
        # Paso 3: Leer datos de entrada
        actualizar_progreso(25, "Leyendo datos...", "Cargando archivo de entrada")
        
        # Determinar qué archivo leer
        if(!is.null(paz_archivos$entrada)) {
          archivo_a_leer <- paz_archivos$entrada$datapath
        } else if(!is.null(paz_archivos$bbva)) {
          archivo_a_leer <- paz_archivos$bbva$datapath
        } else {
          stop("No se encontró archivo para leer")
        }
        
        primera_hoja <- read_excel(archivo_a_leer, sheet = 1, n_max = 5)
        if(nrow(primera_hoja) == 0) {
          stop("El archivo parece estar vacío")
        }
        
        # Leer archivo completo ✅ USAR LA VARIABLE
        valores_proceso$datos_temp <- read_excel(archivo_a_leer)  # ← CORRECTO
        valores_proceso$paso_actual <- 4
      }
      
      else if(valores_proceso$paso_actual == 4) {
        # Paso 4: Preparar agenda
        actualizar_progreso(35, "Preparando agenda...", "Configurando datos de referencia")
        
        agenda_datos <- agenda_df()
        if(is.null(agenda_datos) || nrow(agenda_datos) == 0) {
          clientes_data <- datos_clientes()
          if(nrow(clientes_data) > 0) {
            agenda_datos <- data.frame(
              IDENTIFICADOR = clientes_data$IDENTIFICADOR,
              EMPRESA = clientes_data$CLIENTE,
              CUENTA = clientes_data$NUM_CLIENTE,
              stringsAsFactors = FALSE
            )
          } else {
            agenda_datos <- data.frame(
              IDENTIFICADOR = character(0),
              EMPRESA = character(0),
              CUENTA = character(0),
              stringsAsFactors = FALSE
            )
          }
        }
        
        valores_proceso$paso_actual <- 5
        
      }
      else if(valores_proceso$paso_actual == 5) {
        # Paso 5: Procesamiento según tipo
        actualizar_progreso(50, "Procesando datos...", "Ejecutando lógica principal")
        
        if(paz_archivos$tipo_proceso == "CONSOLIDADO") {
          # ============================================== 
          # PROCESO NORMAL - ALMENA INTELLIGENCE
          # ==============================================
          
          if(exists("ejecutar_proceso_completo")) {
            archivo_entrada <- paz_archivos$entrada$datapath
            nombre_original <- paz_archivos$entrada$name
            carpeta_temporal <- tempdir()
            archivo_salida <- file.path(carpeta_temporal, paste0(tools::file_path_sans_ext(nombre_original), ".xlsx"))
            
            valores_proceso$resultado_temp <- ejecutar_proceso_completo(
              archivo_entrada, 
              agenda_df(), 
              archivo_salida,
              nombre_original
            )
          } else {
            stop("Función ejecutar_proceso_completo no encontrada")
          }
          
        } else if(paz_archivos$tipo_proceso == "BANCO_NORTE") {
          # ============================================== 
          # PROCESO BANCO_NORTE
          # ==============================================
          
          if(exists("procesar_archivo_bbva")) {  # ← Nombre de tu función en EC_BANCO_NORTE.R
            archivo_entrada <- paz_archivos$bbva$datapath
            carpeta_temporal <- tempdir()
            
            # Ejecutar tu función de EC_BANCO_NORTE.R
            resultado_bbva <- procesar_archivo_bbva(archivo_entrada, agenda_df = NULL)
            
            # Guardar resultado
            valores_proceso$resultado_temp <- resultado_bbva
            paz_archivos$archivo_principal <- resultado_bbva$archivo_modificado
            
          } else {
            stop("Función procesar_archivo_bbva no encontrada en EC_BANCO_NORTE.R")
          }
          
        } else {
          stop("Tipo de proceso no reconocido")
        }
        
        valores_proceso$paso_actual <- 6
      }
      
      else if(valores_proceso$paso_actual == 6) {
        actualizar_progreso(75, "Generando archivos...", "Creando archivos de descarga")
        
        if(paz_archivos$tipo_proceso == "CONSOLIDADO") {
          # Determinar la ruta correcta del archivo generado
          # Primero intentamos obtenerla del parámetro 'archivo_salida' que definimos en el Paso 5
          ruta_final <- valores_proceso$resultado_temp$archivo_salida
          
          # Si por alguna razón es nula, la reconstruimos manualmente (mismo método que en Paso 5)
          if(is.null(ruta_final)) {
            carpeta_temporal <- tempdir()
            nombre_original <- paz_archivos$entrada$name
            nombre_archivo <- tools::file_path_sans_ext(nombre_original)
            ruta_final <- file.path(carpeta_temporal, paste0(nombre_archivo, ".xlsx"))
          }
          
          # ✅ ASIGNACIÓN CRÍTICA: Mapeamos la ruta a las variables que usan los botones
          if(file.exists(ruta_final)) {
            paz_archivos$archivo_principal <- ruta_final    # Para el botón "Archivo Principal"
            paz_archivos$archivo_consolidado <- ruta_final # Para el botón "Consolidado" (el que te fallaba)
            cat("✅ Ruta de consolidado vinculada con éxito:", ruta_final, "\n")
          } else {
            stop("No se encontró el archivo principal generado en la ruta esperada.")
          }
          
          # Asignación de los otros archivos generados
          paz_archivos$archivo_empresas <- valores_proceso$resultado_temp$archivo_por_empresas$archivo_creado
          paz_archivos$archivo_no_registrados <- valores_proceso$resultado_temp$no_registrados$archivo_creado
          
        } else if(paz_archivos$tipo_proceso == "BANCO_NORTE") {
          # Validaciones para proceso BANCO_NORTE (Se mantiene igual)
          if(is.null(valores_proceso$resultado_temp)) {
            stop("No se encontró resultado del procesamiento BANCO_NORTE")
          }
          
          if(!valores_proceso$resultado_temp$exito) {
            stop(paste("Error en procesamiento BANCO_NORTE:", valores_proceso$resultado_temp$mensaje))
          }
          
          archivo_bbva <- valores_proceso$resultado_temp$archivo_modificado
          
          if(is.null(archivo_bbva) || !file.exists(archivo_bbva)) {
            stop("El archivo BANCO_NORTE procesado no fue encontrado")
          }
          
          paz_archivos$archivo_principal <- archivo_bbva
          paz_archivos$archivo_no_registrados <- valores_proceso$resultado_temp$archivo_no_registrados
        }
        
        valores_proceso$paso_actual <- 7
      }
      
      else if(valores_proceso$paso_actual == 7) {
        # Paso 7: Finalización — procesamiento completado, iniciar ingesta a Snowflake
        actualizar_progreso(100, "¡Completado!", "Proceso finalizado exitosamente")
        
        valores_proceso$ejecutando <- FALSE
        paz_archivos$procesando    <- FALSE
        
        ocultar_procesamiento()
        mostrar_mensaje_temporal_exito("¡Proceso Completado Exitosamente!", 3000)
        ejecutar_js_seguro("document.getElementById('paz_download_section').style.display = 'block';")
        shinyjs::enable("paz_btn_ejecutar")
        
        showNotification("Proceso completado exitosamente", type = "message", duration = 5)
        
        # ── Auto-cargar hojas en Clasificador de Pagos ──
        archivo_para_cp <- if(paz_archivos$tipo_proceso == "CONSOLIDADO") {
          paz_archivos$archivo_empresas
        } else {
          paz_archivos$archivo_principal
        }
        
        if(!is.null(archivo_para_cp) && file.exists(archivo_para_cp)) {
          hojas_cp <- tryCatch(
            readxl::excel_sheets(archivo_para_cp),
            error = function(e) NULL
          )
          if(!is.null(hojas_cp) && length(hojas_cp) > 0) {
            cp_rv$hojas             <- hojas_cp
            cp_rv$cambios           <- list()
            cp_rv$cache             <- list()
            cp_rv$cache_api         <- list()
            cp_rv$sf_ingesta_lista  <- FALSE
            updateSelectInput(session, "cp_sel_hoja",
                              choices  = hojas_cp,
                              selected = hojas_cp[1])
            shinyjs::show("cp_panel_principal")
            
            # ── Barra 2: mostrar ingesta a Snowflake ──
            shinyjs::runjs("
        var b2 = document.getElementById('paz_barra2_wrap');
        if(b2){ b2.style.display = 'flex'; }
        var fill2 = document.getElementById('paz_barra2_fill');
        if(fill2){ fill2.style.width = '0%'; }
        var txt2 = document.getElementById('paz_barra2_texto');
        if(txt2){ txt2.textContent = 'Sincronizando con Snowflake...'; }
      ")
            shinyjs::delay(300, {
              shinyjs::runjs("
          var fill2 = document.getElementById('paz_barra2_fill');
          if(fill2){ fill2.style.width = '40%'; }
        ")
            })
            
            archivo_para_ingestar <- archivo_para_cp
            future_promise({
              mec_ingestar_excel(archivo_para_ingestar, usuario = "app_user")
            }) %...>% (function(res) {
              cat("Ingesta SF desde PAZ Robot:", res$insertados, "nuevos |",
                  res$existentes, "ya existian\n")
              shinyjs::runjs("
          var fill2 = document.getElementById('paz_barra2_fill');
          if(fill2){ fill2.style.width = '100%'; }
          var txt2 = document.getElementById('paz_barra2_texto');
          if(txt2){ txt2.textContent = 'Datos sincronizados con Snowflake'; }
        ")
              shinyjs::delay(2500, {
                shinyjs::runjs("
            var b2 = document.getElementById('paz_barra2_wrap');
            if(b2){ b2.style.display = 'none'; }
          ")
              })
              cp_rv$sf_ingesta_lista <- TRUE
              showNotification(
                "Datos cargados en Snowflake. Puede ir a Clasificacion de Pagos.",
                type = "message", duration = 5
              )
            }) %...!% (function(err) {
              cat("Aviso ingesta SF PAZ Robot:", err$message, "\n")
              shinyjs::runjs("
          var txt2 = document.getElementById('paz_barra2_texto');
          if(txt2){ txt2.textContent = 'Aviso: verificar sincronizacion'; }
          var fill2 = document.getElementById('paz_barra2_fill');
          if(fill2){ fill2.style.width = '100%'; }
        ")
              cp_rv$sf_ingesta_lista <- TRUE
            })
          }
        }
        
        # Resetear paso
        valores_proceso$paso_actual <- 0
      }
      
    }, error = function(e) {
      # Error en cualquier paso
      valores_proceso$ejecutando <- FALSE
      paz_archivos$procesando <- FALSE
      
      ocultar_procesamiento()
      mostrar_mensaje_temporal_error(e$message, 5000)
      mostrar_error_pantalla(e$message)
      shinyjs::enable("paz_btn_ejecutar")
      
      valores_proceso$paso_actual <- 0
    })
  })
  
  observeEvent(input$paz_btn_ejecutar, {
    # Mostrar advertencia antes de procesar
    showModal(modalDialog(
      title = "Confirmar Ejecución",
      "El proceso puede tomar unos segundo y no podrá interrumpirse una vez iniciado. ¿Continuar?",
      footer = tagList(
        modalButton("Cancelar"),
        actionButton("confirmar_ejecutar", "Sí, Ejecutar", class = "btn-danger")
      )
    ))
  })
  # Y crear un nuevo observer para la confirmación
  observeEvent(input$confirmar_ejecutar, {
    removeModal()
    
    if(paz_archivos$procesando) return()
    
    cat("\n INICIANDO PROCESO PAZ ROBOT POR PASOS\n")
    shinyjs::disable("paz_btn_ejecutar")
    
    # Resetear variables
    valores_proceso$paso_actual <- 1
    valores_proceso$datos_temp <- NULL
    valores_proceso$resultado_temp <- NULL
    valores_proceso$ejecutando <- TRUE
    paz_archivos$procesando <- TRUE
    paz_archivos$cancelar_proceso <- FALSE
    
    # Ocultar errores previos
    ocultar_error_pantalla()
    ejecutar_js_seguro("document.getElementById('paz_download_section').style.display = 'none';")
    
    # Mostrar procesamiento
    mostrar_procesamiento()
  }, ignoreInit = TRUE)
  
  
  # ============================================================================
  # FUNCIÓN AUXILIAR PARA EXTRAER IDENTIFICADOR LIMPIO    ← AQUÍ VA
  # ============================================================================
  extraer_identificador_limpio <- function(id_completo) {
    if(is.null(id_completo) || id_completo == "") return(NULL)
    
    partes <- strsplit(as.character(id_completo), "_")[[1]]
    
    if(length(partes) >= 2) {
      identificador <- paste(head(partes, -2), collapse = "_")
      return(identificador)
    }
    
    return(as.character(id_completo))
  }
  
  # ============================================== 
  # LÓGICA COMPLETA DE ACTUALIZACION - SOLO SI ESTÁ AUTENTICADO
  # ==============================================
  
  # Validación en tiempo real del IDENTIFICADOR
  observeEvent(input$identificador, {
    
    
    identificador <- input$identificador
    
    shinyjs::html("identificador_validation", "")
    shinyjs::removeClass("identificador", "input-error")
    
    if (!is.null(identificador) && identificador != "") {
      if (!es_numerico(identificador)) {
        shinyjs::html("identificador_validation", "⚠️ Solo se permiten números")
        shinyjs::addClass("identificador", "input-error")
      }
      else if (!validar_longitud_identificador(identificador)) {
        shinyjs::html("identificador_validation", "⚠️ Máximo 18 dígitos permitidos")
        shinyjs::addClass("identificador", "input-error")
      }
    }
  })
  
  # Validación en tiempo real del NUM_CLIENTE
  observeEvent(input$num_cliente, {
    
    
    num_cliente <- input$num_cliente
    
    shinyjs::html("num_cliente_validation", "")
    shinyjs::removeClass("num_cliente", "input-error")
    
    if (!is.null(num_cliente) && num_cliente != "") {
      if (!es_numerico(num_cliente)) {
        shinyjs::html("num_cliente_validation", "⚠️ Solo se permiten números")
        shinyjs::addClass("num_cliente", "input-error")
      }
    }
  })
  
  # FUNCIONES DE EDICIÓN Y ELIMINACIÓN - VERIFICAR QUE EXISTAN
  # ==============================================
  
  # Validaciones en tiempo real para modal de edición
  observeEvent(input$edit_identificador, {
    
    
    identificador <- input$edit_identificador
    
    shinyjs::html("edit_identificador_validation", "")
    
    if (!is.null(identificador) && identificador != "") {
      if (!es_numerico(identificador)) {
        shinyjs::html("edit_identificador_validation", "⚠️ Solo se permiten números")
      }
      else if (!validar_longitud_identificador(identificador)) {
        shinyjs::html("edit_identificador_validation", "⚠️ Máximo 18 dígitos permitidos")
      }
    }
  })
  
  observeEvent(input$edit_num_cliente, {
    
    
    num_cliente <- input$edit_num_cliente
    
    shinyjs::html("edit_num_cliente_validation", "")
    
    if (!is.null(num_cliente) && num_cliente != "") {
      if (!es_numerico(num_cliente)) {
        shinyjs::html("edit_num_cliente_validation", "⚠️ Solo se permiten números")
      }
    }
  })
  
  # Confirmar eliminación individual
  observeEvent(input$confirmar_eliminar_individual, {
    
    
    identificador_eliminar <- session$userData$identificador_a_eliminar
    
    if (!is.null(identificador_eliminar)) {
      datos_actuales <- datos_clientes()
      datos_actualizados <- datos_actuales[datos_actuales$IDENTIFICADOR != identificador_eliminar, ]
      datos_clientes(datos_actualizados)
      
      # Actualizar agenda
      if(nrow(datos_actualizados) > 0) {
        agenda_actualizada <- data.frame(
          IDENTIFICADOR = datos_actualizados$IDENTIFICADOR,
          EMPRESA = datos_actualizados$CLIENTE,
          CUENTA = datos_actualizados$NUM_CLIENTE,
          stringsAsFactors = FALSE
        )
        agenda_df(agenda_actualizada)
      } else {
        agenda_df(data.frame(
          IDENTIFICADOR = character(0),
          EMPRESA = character(0),
          CUENTA = character(0),
          stringsAsFactors = FALSE
        ))
      }
      
      tryCatch({
        saveRDS(datos_actualizados, "clientes_data.rds")
        showNotification("Cliente eliminado exitosamente", type = "warning", duration = 3)
      }, error = function(e) {
        showNotification(paste("Error al eliminar:", e$message), type = "error", duration = 5)
      })
    }
    
    removeModal()
    session$userData$identificador_a_eliminar <- NULL
    
  }, ignoreInit = TRUE, priority = 100)
  
  # NUEVO: Marcar registro como editado/listo
  observeEvent(input$marcar_editado, {
    
    
    req(input$marcar_editado)
    
    identificador <- extraer_identificador_limpio(input$marcar_editado)
    
    if(is.null(identificador) || identificador == "") {
      return()
    }
    
    datos_actuales <- datos_clientes()
    indice <- which(datos_actuales$IDENTIFICADOR == identificador)
    
    if(length(indice) > 0) {
      datos_actuales$EDITADO[indice[1]] <- TRUE
      datos_clientes(datos_actuales)
      
      agenda_actualizada <- data.frame(
        IDENTIFICADOR = datos_actuales$IDENTIFICADOR,
        EMPRESA = datos_actuales$CLIENTE,
        CUENTA = datos_actuales$NUM_CLIENTE,
        stringsAsFactors = FALSE
      )
      agenda_df(agenda_actualizada)
      
      tryCatch({
        saveRDS(datos_actuales, "clientes_data.rds")
        showNotification("✅ Registro marcado como listo", type = "message", duration = 2)
      }, error = function(e) {
        showNotification(paste("Error:", e$message), type = "error", duration = 5)
      })
    }
  }, ignoreInit = TRUE, priority = 100)
  
  # Confirmar limpieza total
  observeEvent(input$confirmar_limpiar_todos, {
    
    
    datos_vacios <- data.frame(
      IDENTIFICADOR = character(0),
      NUM_CLIENTE = character(0),
      CLIENTE = character(0),
      RAZON_SOCIAL = character(0),
      FECHA_REGISTRO = as.Date(character(0)),
      EDITADO = logical(0), 
      stringsAsFactors = FALSE
    )
    
    datos_clientes(datos_vacios)
    
    # Limpiar agenda también
    agenda_df(data.frame(
      IDENTIFICADOR = character(0),
      EMPRESA = character(0),
      CUENTA = character(0),
      stringsAsFactors = FALSE
    ))
    
    tryCatch({
      saveRDS(datos_vacios, "clientes_data.rds")
      showNotification("Todos los registros han sido eliminados", type = "error", duration = 5)
    }, error = function(e) {
      showNotification(paste("Error al limpiar registros:", e$message), type = "error", duration = 5)
    })
    
    removeModal()
  })
  
  # Observer para limpiar cuando se cierra el modal
  observeEvent(input$shiny_modal_shown, {
    
    session$userData$editando_activo <- TRUE
  })
  
  observeEvent(input$shiny_modal_hidden, {
    
    
    shinyjs::delay(200, {
      session$userData$identificador_original <- NULL
      session$userData$editando_activo <- FALSE
      shinyjs::runjs("
      Shiny.setInputValue('editar_registro', null);
      setTimeout(function() {
        Shiny.setInputValue('editar_registro', undefined);
      }, 100);
    ")
    })
  })
  
  # Confirmar edición - SOLO SI ESTÁ AUTENTICADO  
  observeEvent(input$confirmar_editar, {
    
    
    identificador_original <- session$userData$identificador_original
    
    if(is.null(identificador_original)) {
      showNotification("Error: No se pudo identificar el registro", type = "error")
      return()
    }
    
    # Validaciones
    if (is.null(input$edit_identificador) || trimws(input$edit_identificador) == "") {
      showNotification("El campo IDENTIFICADOR es obligatorio", type = "error", duration = 3)
      return()
    }
    
    if (!es_numerico(input$edit_identificador)) {
      showNotification("El IDENTIFICADOR debe contener solo números", type = "error", duration = 3)
      return()
    }
    
    if (!validar_longitud_identificador(input$edit_identificador)) {
      showNotification("El IDENTIFICADOR no puede exceder los 18 dígitos", type = "error", duration = 3)
      return()
    }
    
    if (!is.null(input$edit_num_cliente) && input$edit_num_cliente != "" && !es_numerico(input$edit_num_cliente)) {
      showNotification("El Nº DE CLIENTE debe contener solo números", type = "error", duration = 3)
      return()
    }
    
    nuevo_identificador <- trimws(input$edit_identificador)
    datos_actuales <- datos_clientes()
    
    # Verificar duplicados (excepto el mismo registro)
    if (nuevo_identificador != identificador_original) {
      if (nrow(datos_actuales) > 0 && nuevo_identificador %in% datos_actuales$IDENTIFICADOR) {
        showNotification("El nuevo identificador ya existe", type = "error", duration = 3)
        return()
      }
    }
    
    indice_cliente <- which(datos_actuales$IDENTIFICADOR == identificador_original)
    
    if (length(indice_cliente) > 0) {
      # Actualizar SOLO el registro encontrado
      datos_actuales[indice_cliente[1], "IDENTIFICADOR"] <- nuevo_identificador
      datos_actuales[indice_cliente[1], "NUM_CLIENTE"] <- if(is.null(input$edit_num_cliente)) "" else trimws(input$edit_num_cliente)
      datos_actuales[indice_cliente[1], "CLIENTE"] <- if(is.null(input$edit_cliente)) "" else trimws(input$edit_cliente)
      datos_actuales[indice_cliente[1], "RAZON_SOCIAL"] <- if(is.null(input$edit_razon_social)) "" else trimws(input$edit_razon_social)
      datos_actuales[indice_cliente[1], "EDITADO"] <- TRUE
      
      datos_clientes(datos_actuales)
      
      # Actualizar agenda
      agenda_actualizada <- data.frame(
        IDENTIFICADOR = datos_actuales$IDENTIFICADOR,
        EMPRESA = datos_actuales$CLIENTE,
        CUENTA = datos_actuales$NUM_CLIENTE,
        stringsAsFactors = FALSE
      )
      agenda_df(agenda_actualizada)
      
      tryCatch({
        saveRDS(datos_actuales, "clientes_data.rds")
        showNotification("¡Cliente editado exitosamente!", type = "message", duration = 3)
      }, error = function(e) {
        showNotification(paste("Error al guardar:", e$message), type = "error", duration = 5)
      })
    }
    
    removeModal()
    session$userData$identificador_original <- NULL
    
  }, ignoreInit = TRUE, priority = 100)
  
  # Manejar carga de archivo - SOLO SI ESTÁ AUTENTICADO
  observeEvent(input$archivo_no_registrados, {
    
    
    archivo_no_registrados(input$archivo_no_registrados)
    
    if (!is.null(input$archivo_no_registrados)) {
      tryCatch({
        # Leer el archivo completo para vista previa
        datos_completos <- read_excel(input$archivo_no_registrados$datapath)
        
        # Validar estructura del archivo
        validacion <- validar_estructura_archivo(datos_completos)
        
        if (!validacion$valido) {
          # Si la estructura no es válida, mostrar error
          shinyjs::html("import_status", 
                        paste0("<div class='status-error'>
                                 ❌ <strong>Archivo no válido:</strong><br>
                                 ", validacion$mensaje, "<br><br>
                                 <strong>Estructura requerida:</strong><br>
                                 • Columna A: IDENTIFICADOR<br>
                                 • Columna B: EMPRESA<br>
                                 • Columna C: CUENTA
                                 </div>"))
          shinyjs::show("import_status")
          shinyjs::hide("preview_container")
          datos_preview(NULL)
          
          showNotification(
            "❌ El archivo no tiene la estructura requerida. Verifique las columnas.", 
            type = "error", 
            duration = 5
          )
          return()
        }
        
        # Si la validación pasa, continuar
        datos_preview(datos_completos)
        
        # Información del archivo
        info_html <- paste0(
          "<div style='background: #e8f5e8; border-radius: 8px; padding: 15px; margin-bottom: 15px; border: 2px solid #28a745;'>",
          "<h5 style='color: #28a745; margin: 0 0 10px 0;'>✅ Archivo Válido</h5>",
          "<p style='margin: 5px 0;'><strong>📁 Nombre:</strong> ", input$archivo_no_registrados$name, "</p>",
          "<p style='margin: 5px 0;'><strong>📊 Total de filas:</strong> ", nrow(datos_completos), "</p>",
          "<p style='margin: 5px 0;'><strong>📋 Columnas detectadas:</strong></p>",
          "<ul style='margin: 5px 0 5px 20px;'>",
          "<li><strong>Columna A:</strong> ", names(datos_completos)[1], " ✅</li>",
          "<li><strong>Columna B:</strong> ", names(datos_completos)[2], " ✅</li>",
          "<li><strong>Columna C:</strong> ", names(datos_completos)[3], " ✅</li>",
          if(ncol(datos_completos) > 3) paste0("<li><strong>Columnas adicionales:</strong> ", ncol(datos_completos) - 3, "</li>") else "",
          "</ul>",
          "</div>"
        )
        
        shinyjs::html("file_info", info_html)
        shinyjs::show("preview_container")
        
        # Status de éxito
        shinyjs::html("import_status", 
                      "<div class='status-success'>
                         ✅ Archivo cargado y validado correctamente. Revise la vista previa y haga clic en 'Importar Datos' para continuar.
                         </div>")
        shinyjs::show("import_status")
        
      }, error = function(e) {
        shinyjs::html("import_status", 
                      paste0("<div class='status-error'>
                               ❌ Error al leer el archivo: ", e$message, "
                               </div>"))
        shinyjs::show("import_status")
        shinyjs::hide("preview_container")
        datos_preview(NULL)
      })
    } else {
      shinyjs::hide("preview_container")
      shinyjs::hide("import_status")
      datos_preview(NULL)
    }
  })
  
  # ✅ AGREGAR ESTE OUTPUT:
  output$tabla_agenda_snowflake <- DT::renderDataTable({
    
    datos <- agenda_snowflake_df()
    
    if (is.null(datos) || nrow(datos) == 0) {
      return(data.frame(Mensaje = "No hay datos de agenda disponibles desde Snowflake"))
    }
    
    # Limpiar datos para mostrar
    datos_mostrar <- datos
    
    # Reemplazar valores vacíos con "-"
    for(col in names(datos_mostrar)) {
      datos_mostrar[[col]][is.na(datos_mostrar[[col]])] <- "-"
      datos_mostrar[[col]][datos_mostrar[[col]] == ""] <- "-"
      datos_mostrar[[col]] <- as.character(datos_mostrar[[col]])
    }
    
    DT::datatable(
      datos_mostrar,
      options = list(
        pageLength = 15,
        scrollX = TRUE,
        ordering = TRUE,
        searching = TRUE,
        dom = 'frtip'
      ),
      rownames = FALSE
    ) %>%
      DT::formatStyle(columns = 1:ncol(datos_mostrar), fontSize = "12px") %>%
      DT::formatStyle(columns = 1:ncol(datos_mostrar), 
                      backgroundColor = "#f8f9fa",
                      border = "1px solid #dee2e6")
  })
  
  # AGREGAR ESTE OUTPUT:
  output$contador_agenda_snowflake <- renderText({
    
    datos <- agenda_snowflake_df()
    
    if (is.null(datos) || nrow(datos) == 0) {
      "📊 No hay registros de agenda disponibles"
    } else {
      total <- nrow(datos)
      paste( total, "registros en la agenda de Snowflake")
    }
  })
  
  # AGREGAR AQUÍ EL OBSERVEEVENT DEL BOTÓN:
  observeEvent(input$actualizar_agenda_snowflake, {
    
    
    cargar_agenda_snowflake()
  })
  
  # ============================================== 
  # DOWNLOAD HANDLERS COMPLETOS PARA PAZ ROBOT
  # ==============================================
  output$paz_download_principal <- downloadHandler(
    filename = function() {
      if(!is.null(paz_archivos$nombre_base)) {
        paste0(paz_archivos$nombre_base, ".xlsx")
      } else {
        paste0("Consolidado_PAZ_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx")
      }
    },
    content = function(file) {
      
      if(!is.null(paz_archivos$archivo_principal) && file.exists(paz_archivos$archivo_principal)) {
        file.copy(paz_archivos$archivo_principal, file)
        cat("✅ Archivo principal descargado:", file, "\n")
      } else {
        stop("Archivo principal no disponible o no existe")
      }
    },
    contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
  )
  
  # Download Handler - Archivo por Empresas PAZ Robot
  output$paz_download_empresas <- downloadHandler(
    filename = function() {
      if(!is.null(paz_archivos$nombre_base)) {
        paste0("EC_", paz_archivos$nombre_base, ".xlsx")
      } else {
        paste0("EC_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx")
      }
    },
    content = function(file) {
      
      if(!is.null(paz_archivos$archivo_empresas) && file.exists(paz_archivos$archivo_empresas)) {
        file.copy(paz_archivos$archivo_empresas, file)
        cat("✅ Archivo por empresas descargado:", file, "\n")
      } else {
        # Crear archivo vacío si no existe
        wb <- createWorkbook()
        addWorksheet(wb, "Sin empresas")
        writeData(wb, "Sin empresas", data.frame(
          Mensaje = "No se generaron datos por empresas en este procesamiento"
        ))
        saveWorkbook(wb, file, overwrite = TRUE)
        cat("⚠️ Archivo por empresas creado vacío\n")
      }
    },
    contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
  )
  
  # Download Handler - Archivo Consolidado (RESUMEN + Fondeo)
  output$paz_download_consolidado <- downloadHandler(
    filename = function() {
      if(!is.null(paz_archivos$nombre_base)) {
        paste0("Consolidado_", paz_archivos$nombre_base, ".xlsx")
      } else {
        paste0("Consolidado_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx")
      }
    },
    content = function(file) {
      
      if(!is.null(paz_archivos$archivo_consolidado) && file.exists(paz_archivos$archivo_consolidado)) {
        file.copy(paz_archivos$archivo_consolidado, file)
      } else {
        # Crear archivo vacío si no existe
        wb <- createWorkbook()
        addWorksheet(wb, "Sin datos")
        writeData(wb, "Sin datos", data.frame(
          Mensaje = "No se generó archivo consolidado en este procesamiento"
        ))
        saveWorkbook(wb, file, overwrite = TRUE)
      }
    },
    contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
  )
  
  # Download Handler - Archivo No Registrados PAZ Robot
  output$paz_download_no_registrados <- downloadHandler(
    filename = function() {
      if(!is.null(paz_archivos$nombre_base)) {
        paste0("No_registrados_", paz_archivos$nombre_base, ".xlsx")
      } else {
        paste0("No_registrados_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx")
      }
    },
    content = function(file) {
      
      if(!is.null(paz_archivos$archivo_no_registrados) && file.exists(paz_archivos$archivo_no_registrados)) {
        file.copy(paz_archivos$archivo_no_registrados, file)
        cat("✅ Archivo no registrados descargado:", file, "\n")
      } else {
        # Crear archivo vacío con mensaje
        wb <- createWorkbook()
        addWorksheet(wb, "Sin registros")
        writeData(wb, "Sin registros", data.frame(
          Mensaje = "No se encontraron registros 'No registrados' en este procesamiento",
          Fecha = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
        ))
        saveWorkbook(wb, file, overwrite = TRUE)
        cat("ℹ️ Archivo no registrados creado vacío\n")
      }
    },
    contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
  )
  
  # Download Handler - Archivo BANCO_NORTE Procesado
  output$paz_download_bbva <- downloadHandler(
    filename = function() {
      if(!is.null(paz_archivos$nombre_base)) {
        paste0(paz_archivos$nombre_base, "_modificado.xlsx")
      } else {
        paste0("BANCO_NORTE_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx")
      }
    },
    content = function(file) {
      
      if(!is.null(paz_archivos$archivo_principal) && file.exists(paz_archivos$archivo_principal)) {
        file.copy(paz_archivos$archivo_principal, file)
        cat("✅ Archivo BANCO_NORTE descargado:", file, "\n")
      } else {
        stop("Archivo BANCO_NORTE no disponible")
      }
    },
    contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
  )
  
  # Download Handler - No Registrados BANCO_NORTE
  output$paz_download_bbva_no_registrados <- downloadHandler(
    filename = function() {
      if(!is.null(paz_archivos$nombre_base)) {
        paste0("No_registrados_", paz_archivos$nombre_base, ".xlsx")
      } else {
        paste0("No_registrados_BANCO_NORTE_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx")
      }
    },
    content = function(file) {
      
      if(!is.null(paz_archivos$archivo_no_registrados) && file.exists(paz_archivos$archivo_no_registrados)) {
        file.copy(paz_archivos$archivo_no_registrados, file)
        cat("✅ Archivo no registrados BANCO_NORTE descargado:", file, "\n")
      } else {
        # Crear archivo vacío si no hay no registrados
        wb <- createWorkbook()
        addWorksheet(wb, "No Registrados")
        writeData(wb, "No Registrados", data.frame(
          IDENTIFICADOR = character(),
          EMPRESA = character(),
          CUENTA = character(),
          stringsAsFactors = FALSE
        ))
        saveWorkbook(wb, file, overwrite = TRUE)
        cat("ℹ️ No hay registros 'No registrado' en este archivo BANCO_NORTE\n")
      }
    },
    contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
  )
  
  # Download Handler - Agenda Snowflake
  output$descargar_agenda_snowflake <- downloadHandler(
    filename = function() {
      paste0("Agenda_Snowflake_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx")
    },
    content = function(file) {
      
      datos <- agenda_snowflake_df()
      
      if (is.null(datos) || nrow(datos) == 0) {
        # Crear archivo vacío con mensaje
        wb <- createWorkbook()
        addWorksheet(wb, "Agenda")
        writeData(wb, "Agenda", data.frame(
          Mensaje = "No hay datos de agenda disponibles"
        ))
        saveWorkbook(wb, file, overwrite = TRUE)
        return()
      }
      
      # Preparar datos para Excel
      datos_excel <- data.frame(
        IDENTIFICADOR = datos$IDENTIFICADOR,
        NO_CLIENTE = datos$NO_CLIENTE,
        CLIENTE = datos$CLIENTE,
        RAZON_SOCIAL = datos$RAZON_SOCIAL,
        stringsAsFactors = FALSE
      )
      
      # Reemplazar valores vacíos
      for(col in names(datos_excel)) {
        datos_excel[[col]][is.na(datos_excel[[col]])] <- "-"
        datos_excel[[col]][datos_excel[[col]] == ""] <- "-"
      }
      
      # Crear workbook con formato
      wb <- createWorkbook()
      addWorksheet(wb, "Agenda")
      
      # Escribir datos
      writeData(wb, "Agenda", datos_excel, startRow = 1)
      
      # Estilo para encabezados (rojo con letra blanca)
      headerStyle <- createStyle(
        fontSize = 12,
        fontColour = "#FFFFFF",
        halign = "center",
        valign = "center",
        textDecoration = "bold",
        fgFill = "#DC3545",
        border = "TopBottomLeftRight",
        borderColour = "#000000"
      )
      
      # Aplicar estilo a encabezados
      addStyle(wb, sheet = "Agenda", headerStyle, rows = 1, cols = 1:4, gridExpand = TRUE)
      
      # Ajustar ancho de columnas
      setColWidths(wb, "Agenda", cols = 1:4, widths = c(18, 15, 30, 35))
      
      # Guardar archivo
      saveWorkbook(wb, file, overwrite = TRUE)
      
      cat("✅ Agenda Snowflake descargada exitosamente\n")
    },
    contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
  )
  
  # ============================================== 
  # MANEJADORES DE BOTONES PAZ ROBOT
  # ==============================================
  
  # Botón Reintentar PAZ Robot
  observeEvent(input$paz_btn_retry, {
    
    
    cat("🔄 USUARIO SOLICITÓ REINTENTAR PAZ ROBOT\n")
    ocultar_error_pantalla()
  })
  
  # Botón Cerrar Error PAZ Robot
  observeEvent(input$paz_btn_hide_error, {
    
    
    cat("✖️ USUARIO CERRÓ SECCIÓN DE ERROR PAZ ROBOT\n")
    ocultar_error_pantalla()
  })
  
  # Auto-ocultar error cuando se inicia nuevo proceso PAZ Robot
  observe({
    if(paz_archivos$procesando) {
      ocultar_error_pantalla()
    }
  })
  
  # Mantener interfaz responsiva PAZ Robot
  observe({
    invalidateLater(1000, session)
    if(!paz_archivos$procesando) {
      shinyjs::enable("paz_btn_ejecutar")
    }
  })
  
  # ============================================================
  # GESTIÓN DE CLIENTES NO REGISTRADOS
  # ============================================================
  
  # Render tabla
  output$rc_tabla_no_registrados <- DT::renderDataTable({
    df <- rc_datos()
    if (is.null(df) || nrow(df) == 0) {
      return(DT::datatable(
        data.frame(Mensaje = "No hay clientes pendientes de registro"),
        options = list(dom = "t"), rownames = FALSE
      ))
    }
    
    df$REGISTRAR <- paste0(
      '<button class="btn btn-danger btn-sm rc-btn-registrar" ',
      'style="font-size:0.8rem; padding:3px 10px;" ',
      'data-id="', htmltools::htmlEscape(df$IDENTIFICADOR), '">',
      'Registrar</button>'
    )
    
    DT::datatable(
      df[, c("EMPRESA", "CUENTA", "IDENTIFICADOR", "CLIENTE", "RAZON_SOCIAL", "REGISTRAR")],
      escape      = FALSE,
      rownames    = FALSE,
      colnames    = c("Empresa", "Cuenta", "Identificador", "Cliente", 
                      "Razón Social", ""),
      selection   = "none",
      options     = list(
        pageLength = 15,
        dom        = "frtip",
        language   = list(search = "Buscar:", paginate = list(
          previous = "Anterior", `next` = "Siguiente")),
        columnDefs = list(list(orderable = FALSE, targets = 5))
      )
    )
  }, server = FALSE)
  
  # Contador
  output$rc_contador <- renderText({
    df <- rc_datos()
    if (is.null(df) || nrow(df) == 0) return("Sin pendientes")
    paste0(nrow(df), " cliente(s) pendientes de registro")
  })
  
  # Botón actualizar tabla
  observeEvent(input$rc_btn_actualizar, {
    df <- tryCatch(mec_cargar_no_registrados(), error = function(e) NULL)
    if (!is.null(df)) {
      rc_datos(df)
      showNotification("Tabla actualizada", type = "message", duration = 2)
    }
  })
  
  # Capturar click en botón Registrar de la tabla
  observeEvent(input$rc_identificador_seleccionado, {
    req(input$rc_identificador_seleccionado)
    
    df    <- rc_datos()
    id    <- input$rc_identificador_seleccionado
    fila  <- df[df$IDENTIFICADOR == id, ]
    
    showModal(modalDialog(
      title = "Registrar Cliente",
      size  = "m",
      
      div(
        style = "background:#fdf5f4; padding:10px 14px; border-radius:8px;
               border-left:4px solid #802e25; margin-bottom:16px;",
        p(style = "margin:0; color:#401712; font-size:0.85rem;",
          paste0("Empresa: ", if(nrow(fila)>0) fila$EMPRESA[1] else ""),
          br(),
          paste0("Cuenta: ",  if(nrow(fila)>0) fila$CUENTA[1]  else ""))
      ),
      
      textInput("rc_input_identificador", 
                HTML("IDENTIFICADOR <span style='color:#802e25;'>*</span>"),
                value = id),
      
      textInput("rc_input_contrato", "NÚMERO DE CONTRATO (solo números)",
                placeholder = "Solo números"),
      
      textInput("rc_input_cliente", "CLIENTE (opcional)",
                placeholder = "Nombre del cliente"),
      
      textInput("rc_input_razon", "RAZÓN SOCIAL (opcional)",
                placeholder = "Razón social"),
      
      footer = tagList(
        modalButton("Cancelar"),
        actionButton("rc_confirmar_registro", "Registrar", class = "btn-danger")
      ),
      easyClose = FALSE
    ))
    
    session$userData$rc_id_actual <- id
  })
  
  # Confirmar registro
  observeEvent(input$rc_confirmar_registro, {
    id          <- session$userData$rc_id_actual
    num_contrato <- trimws(input$rc_input_contrato)
    cliente      <- trimws(input$rc_input_cliente)
    razon_social <- trimws(input$rc_input_razon)
    
    # Validaciones
    if (is.null(id) || id == "") {
      showNotification("El identificador no puede estar vacío", type = "warning")
      return()
    }
    if (num_contrato != "" && !grepl("^[0-9]+$", num_contrato)) {
      showNotification("El número de contrato solo puede contener números", type = "warning")
      return()
    }
    
    removeModal()
    
    ok <- mec_registrar_cliente(
      identificador = id,
      num_contrato  = num_contrato,
      cliente       = cliente,
      razon_social  = razon_social
    )
    
    if (ok) {
      # Quitar la fila de la tabla local sin recargar SF
      df_actual <- rc_datos()
      rc_datos(df_actual[df_actual$IDENTIFICADOR != id, ])
      showNotification("Cliente registrado correctamente en Agenda", 
                       type = "message", duration = 4)
    } else {
      showNotification("Error al registrar. Revisar logs.", 
                       type = "error", duration = 5)
    }
    
    session$userData$rc_id_actual <- NULL
  })
}

# EJECUTAR APP INTEGRADA CON LOGIN
shinyApp(ui = ui, server = server)
