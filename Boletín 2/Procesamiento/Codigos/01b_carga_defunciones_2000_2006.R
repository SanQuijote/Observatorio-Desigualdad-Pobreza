# =============================================================================
# CARGA DE DEFUNCIONES 2000-2006 - VERSIÓN FINAL
# Boletín 2 - Observatorio de Políticas Públicas
# =============================================================================
#
# Este script carga los archivos EDG_2000.sav a EDG_2006.sav,
# los harmoniza y guarda el resultado.
#
# =============================================================================

# Limpiar ambiente
rm(list = ls())

# =============================================================================
# 1. CONFIGURACIÓN INICIAL
# =============================================================================

# Librerías necesarias
library(dplyr)
library(haven)
library(tidyr)

# RUTA BASE (la que ya funciona en tu computadora)
base_dir <- 'C:/Users/Wilson/Documents/GitHub/Observatorio-Desigualdad-Pobreza/Boletín 2'

# Directorio donde están los archivos .sav
defunciones_dir <- file.path(base_dir, "Procesamiento", "Bases", "Defunciones")

# Directorio para guardar resultados
output_dir <- file.path(base_dir, "Procesamiento", "Bases", "Defunciones")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

cat("========================================\n")
cat("CARGA DE DEFUNCIONES 2000-2006\n")
cat("========================================\n")
cat("Base:", base_dir, "\n")
cat("Archivos:", defunciones_dir, "\n")
cat("========================================\n\n")

# =============================================================================
# 2. FUNCIONES DE HARMONIZACIÓN
# =============================================================================

harmonizar_pre2007 <- function(df, anio) {
  
  # PASO 1: Convertir nombres a minúsculas
  names(df) <- tolower(names(df))
  
  # PASO 2: Mapeo de nombres 2000-2006 a estándar
  mapa_nombres <- list(
    # Identificación
    codigoen = "codigo_encuesta",
    
    # Fechas de inscripción
    anoi = "anio_insc",
    mesi = "mes_insc",
    acta = "acta_insc",
    estab = "oficina_insc",
    
    # Fechas de nacimiento
    anon = "anio_nac",
    mesn = "mes_nac",
    
    # Fechas de fallecimiento
    anof = "anio_fall",
    mesf = "mes_fall",
    
    # Edad
    edad = "edad",
    codi = "cod_edad",
    
    # Geográficas de residencia
    provr = "prov_res",
    cantr = "cant_res",
    parrr = "parr_res",
    zonar = "area_res",
    
    # Geográficas de fallecimiento
    provf = "prov_fall",
    cantf = "cant_fall",
    parrf = "parr_fall",
    zona = "area_fall",
    
    # Causas
    causa103 = "causa103",
    causa080 = "causa80",
    causa667 = "causa667",
    causa = "causa",
    causa1 = "causa1",
    codcausa = "codcausa",
    
    # Causas específicas para menores
    caus3671 = "causa3671",
    caus3670 = "causa3670",
    
    # Demográficas
    sexo = "sexo",
    nivel = "niv_inst",
    grado = "grado_inst",
    estado = "est_civil",
    leer = "sabe_leer",
    
    # Ocupacionales
    trabaja = "trabajaba",
    profe = "profesion",
    ocupa = "ocupacion",
    estab1 = "actividad_estab",
    categori = "categoria_ocup",
    notrabaj = "situacion_no_trabaja",
    
    # Mujeres y mortalidad materna
    embara = "embara",
    meses = "meses_emb",
    semana6 = "semana6",
    atencion = "atencion_emb",
    consulta = "consulta_emb",
    
    # Contexto
    ocurrio = "lugar_ocur",
    certi = "cert_por",
    resident = "residente",
    
    # Persona que inscribe
    edadso = "edad_solicitante",
    paren = "parentesco",
    
    # Totales
    t = "total"
  )
  
  # Aplicar el mapeo
  for (nombre_std in names(mapa_nombres)) {
    if (nombre_std %in% names(df)) {
      names(df)[names(df) == nombre_std] <- mapa_nombres[[nombre_std]]
    }
  }
  
  # PASO 3: Manejo de casos especiales por año
  if (anio >= 2003 && anio <= 2006) {
    if ("causa3671" %in% names(df)) {
      df$causa3671 <- as.character(df$causa3671)
    }
    if ("causa3670" %in% names(df)) {
      df$causa3670 <- as.character(df$causa3670)
    }
  }
  
  return(df)
}

verificar_pre2007 <- function(df, anio) {
  
  variables_criticas <- c("anio_fall", "mes_fall", "cod_edad", "edad", 
                          "niv_inst", "causa103", "sexo", "prov_res", 
                          "parr_res", "area_res")
  
  presentes <- variables_criticas[variables_criticas %in% names(df)]
  faltantes <- variables_criticas[!variables_criticas %in% names(df)]
  
  cat("\nAño", anio, ":\n")
  cat("  Variables presentes:", paste(presentes, collapse = ", "), "\n")
  if (length(faltantes) > 0) {
    cat("  Variables faltantes:", paste(faltantes, collapse = ", "), "\n")
  }
  
  invisible(list(presentes = presentes, faltantes = faltantes))
}

# =============================================================================
# 3. VERIFICAR ARCHIVOS DISPONIBLES
# =============================================================================

cat("\nBuscando archivos en:", defunciones_dir, "\n")

archivos_2000_2006 <- list.files(
  defunciones_dir, 
  pattern = "EDG_(200[0-6])\\.sav$", 
  full.names = TRUE
)

cat("\nArchivos encontrados:\n")
if (length(archivos_2000_2006) > 0) {
  print(basename(archivos_2000_2006))
} else {
  cat("  ¡NO SE ENCONTRARON ARCHIVOS!\n")
  cat("  Verifica que los archivos estén en:\n")
  cat("  ", defunciones_dir, "\n")
  stop("No hay archivos para procesar")
}

# Extraer años disponibles
anios_disponibles <- as.numeric(gsub(".*EDG_(\\d{4})\\.sav", "\\1", basename(archivos_2000_2006)))
anios_esperados <- 2000:2006

faltantes <- setdiff(anios_esperados, anios_disponibles)
if (length(faltantes) > 0) {
  cat("\n⚠️  ADVERTENCIA: Faltan años:", paste(faltantes, collapse = ", "), "\n")
} else {
  cat("\n✅ Todos los años 2000-2006 están disponibles.\n")
}

# =============================================================================
# 4. FUNCIÓN PARA CARGAR UN AÑO (VERSIÓN CORREGIDA)
# =============================================================================

cargar_anio_2000_2006 <- function(anio) {
  
  cat("\n📅 Procesando año", anio, "...\n")
  
  archivo <- file.path(defunciones_dir, paste0("EDG_", anio, ".sav"))
  
  if (!file.exists(archivo)) {
    stop("Archivo no encontrado: ", archivo)
  }
  
  cat("  Leyendo archivo...\n")
  df <- read_sav(archivo)
  
  cat("  Armonizando variables...\n")
  df <- harmonizar_pre2007(df, anio)
  
  # 🔴 CORRECCIÓN: Unificar TODAS las variables de causa a character
  vars_causa <- c("causa103", "causa80", "causa667", "causa", "causa1")
  for (var in vars_causa) {
    if (var %in% names(df)) {
      df[[var]] <- as.character(df[[var]])
    }
  }
  
  df$anio <- anio
  
  verificar_pre2007(df, anio)
  
  cat("  Calculando variables derivadas...\n")
  df <- df %>%
    mutate(
      edad_num = case_when(
        !is.na(cod_edad) & !is.na(edad) & as.numeric(cod_edad) == 4 ~ as.numeric(edad),
        !is.na(cod_edad) & !is.na(edad) & as.numeric(cod_edad) == 3 ~ as.numeric(edad) / 12,
        !is.na(cod_edad) & !is.na(edad) & as.numeric(cod_edad) == 2 ~ as.numeric(edad) / 365,
        !is.na(cod_edad) & !is.na(edad) & as.numeric(cod_edad) == 1 ~ as.numeric(edad) / 8760,
        TRUE ~ NA_real_
      ),
      
      grupo_edad = case_when(
        !is.na(edad_num) & edad_num >= 20 & edad_num < 30 ~ "20-29",
        !is.na(edad_num) & edad_num >= 30 & edad_num < 65 ~ "30-64",
        !is.na(edad_num) & edad_num >= 65 & edad_num < 120 ~ "65+",
        TRUE ~ NA_character_
      ),
      
      nivel_educativo_jovenes = case_when(
        !is.na(edad_num) & !is.na(niv_inst) &
          edad_num >= 20 & edad_num < 30 &
          as.numeric(niv_inst) %in% c(4, 5, 6, 7, 8) ~ "Secundaria completa",
        !is.na(edad_num) & !is.na(niv_inst) &
          edad_num >= 20 & edad_num < 30 &
          as.numeric(niv_inst) %in% c(0, 1, 2, 3) ~ "Secundaria incompleta",
        TRUE ~ NA_character_
      ),
      
      nivel_educativo_adultos = case_when(
        !is.na(edad_num) & !is.na(niv_inst) &
          edad_num >= 30 &
          as.numeric(niv_inst) %in% c(6, 7, 8) ~ "Superior",
        !is.na(edad_num) & !is.na(niv_inst) &
          edad_num >= 30 &
          as.numeric(niv_inst) %in% c(0, 1, 2, 3, 4, 5) ~ "No superior",
        TRUE ~ NA_character_
      ),
      
      causa_externa = case_when(
        !is.na(causa103) & grepl("101", causa103) ~ "Suicidio",
        !is.na(causa103) & grepl("102", causa103) ~ "Homicidio",
        !is.na(causa103) & grepl("096", causa103) ~ "Accidente de transporte",
        !is.na(causa103) & grepl("097|098|099|100|103", causa103) ~ "Otras externas",
        TRUE ~ NA_character_
      ),
      
      cod_parroquia = sprintf("%06d", as.numeric(as.character(parr_res)))
    )
  
  cat("  Registros cargados:", format(nrow(df), big.mark = ","), "\n")
  
  return(df)
}


# =============================================================================
# 5. CARGAR TODOS LOS AÑOS
# =============================================================================

cat("\n========== INICIANDO CARGA ==========\n")

defunciones_2000_2006 <- bind_rows(
  lapply(anios_esperados, function(anio) {
    tryCatch({
      cargar_anio_2000_2006(anio)
    }, error = function(e) {
      cat("\n❌ Error en año", anio, ":", e$message, "\n")
      return(NULL)
    })
  })
)

# =============================================================================
# 6. VERIFICAR RESULTADOS
# =============================================================================

cat("\n========== RESUMEN ==========\n")
cat("Total de registros cargados (2000-2006):", 
    format(nrow(defunciones_2000_2006), big.mark = ","), "\n")
cat("Años presentes:", paste(sort(unique(defunciones_2000_2006$anio)), collapse = ", "), "\n")

# =============================================================================
# 7. GUARDAR RESULTADOS
# =============================================================================

output_file <- file.path(defunciones_dir, "defunciones_2000_2006.rds")
cat("\nGuardando en:\n", output_file, "\n")
saveRDS(defunciones_2000_2006, output_file)

cat("\n✅ Archivo guardado exitosamente.\n")
cat("========== PROCESO COMPLETADO ==========\n")