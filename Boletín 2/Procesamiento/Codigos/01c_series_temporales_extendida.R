# =============================================================================
# GRÁFICOS DE SERIES TEMPORALES 2000-2024
# Boletín 2 - Observatorio de Políticas Públicas
# =============================================================================
#
# Este script une las bases 2000-2006 y 2007-2024,
# y genera todos los gráficos con la serie completa.
# Los gráficos se guardan con sufijo "_2000_2024" para no sobrescribir.
#
# =============================================================================

# Limpiar ambiente
rm(list = ls())

# =============================================================================
# 1. CONFIGURACIÓN INICIAL
# =============================================================================

# Librerías
library(dplyr)
library(ggplot2)
library(tidyr)
library(patchwork)
library(haven)

# Ruta base
base_dir <- 'C:/Users/Wilson/Documents/GitHub/Observatorio-Desigualdad-Pobreza/Boletín 2'
setwd(base_dir)

# 🔴 PRIMERO: Cargar configuración (esto define base_dir, output_dir, etc.)
source(file.path(base_dir, "Procesamiento", "Codigos", "00_configuracion.R"))

# 🔴 DESPUÉS: Definir el directorio de defunciones
defunciones_dir <- file.path(base_dir, "Procesamiento", "Bases", "Defunciones")
cat("\n========================================\n")
cat("Directorio de defunciones:", defunciones_dir, "\n")
cat("Archivos encontrados:\n")
print(list.files(defunciones_dir, pattern = "defunciones_.*\\.rds"))
cat("========================================\n\n")

cat("========================================\n")
cat("GRÁFICOS SERIES TEMPORALES 2000-2024\n")
cat("========================================\n")
cat("Gráficos se guardarán en:", output_dir, "\n")
cat("========================================\n\n")

# =============================================================================
# 2. CARGAR Y UNIR BASES
# =============================================================================

cat("Cargando base 2000-2006...\n")
def_2000_2006 <- readRDS(file.path(defunciones_dir, "defunciones_2000_2006.rds"))

cat("Cargando base 2007-2024...\n")
def_2007_2024 <- readRDS(file.path(defunciones_dir, "defunciones_historicas.rds"))

# 🔴 SOLUCIÓN DEFINITIVA: Convertir TODAS las columnas a character
cat("Unificando tipos de variables...\n")

# Obtener todas las columnas que están en ambas bases
columnas_comunes <- intersect(names(def_2000_2006), names(def_2007_2024))

# Convertir TODAS las columnas comunes a character
for (col in columnas_comunes) {
  def_2000_2006[[col]] <- as.character(def_2000_2006[[col]])
  def_2007_2024[[col]] <- as.character(def_2007_2024[[col]])
}

cat("Uniendo bases...\n")
defunciones_completas <- bind_rows(def_2000_2006, def_2007_2024)

# 🔴 CORREGIDO: Convertir variables numéricas de vuelta a número
# (quitamos 'n' que no es una columna, es una función)
defunciones_completas <- defunciones_completas %>%
  mutate(
    anio = as.numeric(anio),
    edad = as.numeric(edad),
    cod_edad = as.numeric(cod_edad),
    edad_num = as.numeric(edad_num)
    # 👆 n no está aquí porque no es una columna
  )

# Verificar que la unión fue exitosa
cat("✅ Unión exitosa\n")
cat("Total de registros 2000-2024:", format(nrow(defunciones_completas), big.mark = ","), "\n")
cat("Años:", paste(range(defunciones_completas$anio, na.rm = TRUE), collapse = " - "), "\n")
cat("Columnas:", paste(names(defunciones_completas)[1:10], collapse = ", "), "...\n\n")



# =============================================================================
# 3. GRÁFICO 1: MORTALIDAD POR EDUCACIÓN (SERIE COMPLETA)
# =============================================================================

cat("Generando gráfico 1: Mortalidad por educación...\n")

# Tasas para jóvenes
tasas_jovenes_tiempo <- defunciones_completas %>%
  filter(!is.na(nivel_educativo_jovenes)) %>%
  count(anio, nivel_educativo_jovenes) %>%
  left_join(
    extrapolacion_poblacion_joven_secundaria %>%
      pivot_longer(cols = c(secundaria_completa, secundaria_incompleta),
                   names_to = "nivel",
                   values_to = "poblacion") %>%
      mutate(nivel_educativo_jovenes = ifelse(nivel == "secundaria_completa",
                                              "Secundaria completa",
                                              "Secundaria incompleta")) %>%
      select(anio, nivel_educativo_jovenes, poblacion),
    by = c("anio", "nivel_educativo_jovenes")
  ) %>%
  mutate(tasa = (n / poblacion) * 1000) %>%
  filter(!is.na(tasa), anio >= 2000)

# Tasas para adultos
tasas_adultos_tiempo <- defunciones_completas %>%
  filter(!is.na(nivel_educativo_adultos)) %>%
  count(anio, nivel_educativo_adultos) %>%
  left_join(
    extrapolacion_poblacion_adulta_superior %>%
      pivot_longer(cols = c(superior, no_superior),
                   names_to = "nivel",
                   values_to = "poblacion") %>%
      mutate(nivel_educativo_adultos = ifelse(nivel == "superior",
                                              "Superior",
                                              "No superior")) %>%
      select(anio, nivel_educativo_adultos, poblacion),
    by = c("anio", "nivel_educativo_adultos")
  ) %>%
  mutate(tasa = (n / poblacion) * 1000) %>%
  filter(!is.na(tasa), anio >= 2000)

# Definir límites comunes
ylim_mortalidad <- range(c(tasas_jovenes_tiempo$tasa, tasas_adultos_tiempo$tasa), na.rm = TRUE)
ylim_mortalidad[1] <- 0
xlim_mortalidad <- c(2000, 2024)

# Gráfico jóvenes
p1 <- ggplot(tasas_jovenes_tiempo, 
             aes(x = anio, y = tasa, color = nivel_educativo_jovenes, group = nivel_educativo_jovenes)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  scale_color_manual(values = c("Secundaria completa" = "#27AE60", "Secundaria incompleta" = "#E74C3C")) +
  scale_x_continuous(breaks = seq(2000, 2024, by = 2)) +
  coord_cartesian(xlim = xlim_mortalidad, ylim = ylim_mortalidad) +
  labs(
    title = "Jóvenes (20-29 años)",
    x = NULL,
    y = "Mortalidad x 1000 hab.",
    color = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Gráfico adultos
p2 <- ggplot(tasas_adultos_tiempo, 
             aes(x = anio, y = tasa, color = nivel_educativo_adultos, group = nivel_educativo_adultos)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  scale_color_manual(values = c("Superior" = "#27AE60", "No superior" = "#E74C3C")) +
  scale_x_continuous(breaks = seq(2000, 2024, by = 2)) +
  coord_cartesian(xlim = xlim_mortalidad, ylim = ylim_mortalidad) +
  labs(
    title = "Adultos (30+ años)",
    x = "Año",
    y = "Mortalidad x 1000 hab.",
    color = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Combinar y guardar
g1 <- p1 + p2 + plot_annotation(
  title = "Mortalidad por nivel educativo",
  subtitle = "Ecuador 2000-2024 - Tasas por 1,000 habitantes"
)

ggsave(file.path(output_dir, "serie_mortalidad_educacion_2000_2024.png"), 
       g1, width = 14, height = 6, dpi = 300, bg = "white")
cat("  Guardado: serie_mortalidad_educacion_2000_2024.png\n")

# =============================================================================
# 4. GRÁFICO 2: HOMICIDIOS POR EDUCACIÓN (SERIE COMPLETA)
# =============================================================================

cat("Generando gráfico 2: Homicidios por educación...\n")

# Tasas homicidios jóvenes
tasas_homicidios_jovenes <- defunciones_completas %>%
  filter(causa_externa == "Homicidio" & !is.na(nivel_educativo_jovenes)) %>%
  count(anio, nivel_educativo_jovenes) %>%
  left_join(
    extrapolacion_poblacion_joven_secundaria %>%
      pivot_longer(cols = c(secundaria_completa, secundaria_incompleta),
                   names_to = "nivel",
                   values_to = "poblacion") %>%
      mutate(nivel_educativo_jovenes = ifelse(nivel == "secundaria_completa",
                                              "Secundaria completa",
                                              "Secundaria incompleta")) %>%
      select(anio, nivel_educativo_jovenes, poblacion),
    by = c("anio", "nivel_educativo_jovenes")
  ) %>%
  mutate(tasa = (n / poblacion) * 100000) %>%
  filter(!is.na(tasa), anio >= 2000)

# Tasas homicidios adultos
tasas_homicidios_adultos <- defunciones_completas %>%
  filter(causa_externa == "Homicidio" & !is.na(nivel_educativo_adultos)) %>%
  count(anio, nivel_educativo_adultos) %>%
  left_join(
    extrapolacion_poblacion_adulta_superior %>%
      pivot_longer(cols = c(superior, no_superior),
                   names_to = "nivel",
                   values_to = "poblacion") %>%
      mutate(nivel_educativo_adultos = ifelse(nivel == "superior",
                                              "Superior",
                                              "No superior")) %>%
      select(anio, nivel_educativo_adultos, poblacion),
    by = c("anio", "nivel_educativo_adultos")
  ) %>%
  mutate(tasa = (n / poblacion) * 100000) %>%
  filter(!is.na(tasa), anio >= 2000)

# Límites comunes
ylim_homicidios <- range(c(tasas_homicidios_jovenes$tasa, tasas_homicidios_adultos$tasa), na.rm = TRUE)
ylim_homicidios[1] <- 0

# Gráfico jóvenes
p3 <- ggplot(tasas_homicidios_jovenes, 
             aes(x = anio, y = tasa, color = nivel_educativo_jovenes, group = nivel_educativo_jovenes)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  scale_color_manual(values = c("Secundaria completa" = "#27AE60", "Secundaria incompleta" = "#E74C3C")) +
  scale_x_continuous(breaks = seq(2000, 2024, by = 2)) +
  coord_cartesian(xlim = xlim_mortalidad, ylim = ylim_homicidios) +
  labs(
    title = "Jóvenes (20-29 años)",
    x = NULL,
    y = "Homicidios x 100,000 hab.",
    color = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Gráfico adultos
p4 <- ggplot(tasas_homicidios_adultos, 
             aes(x = anio, y = tasa, color = nivel_educativo_adultos, group = nivel_educativo_adultos)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  scale_color_manual(values = c("Superior" = "#27AE60", "No superior" = "#E74C3C")) +
  scale_x_continuous(breaks = seq(2000, 2024, by = 2)) +
  coord_cartesian(xlim = xlim_mortalidad, ylim = ylim_homicidios) +
  labs(
    title = "Adultos (30+ años)",
    x = "Año",
    y = "Homicidios x 100,000 hab.",
    color = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Combinar y guardar
g2 <- p3 + p4 + plot_annotation(
  title = "Homicidios por nivel educativo",
  subtitle = "Ecuador 2000-2024 - Tasas por 100,000 habitantes"
)

ggsave(file.path(output_dir, "serie_homicidios_educacion_2000_2024.png"), 
       g2, width = 14, height = 6, dpi = 300, bg = "white")
cat("  Guardado: serie_homicidios_educacion_2000_2024.png\n")

# =============================================================================
# 5. GRÁFICO 3: SUICIDIOS POR EDUCACIÓN (SERIE COMPLETA)
# =============================================================================

cat("Generando gráfico 3: Suicidios por educación...\n")

# Tasas suicidios jóvenes
tasas_suicidios_jovenes <- defunciones_completas %>%
  filter(causa_externa == "Suicidio" & !is.na(nivel_educativo_jovenes)) %>%
  count(anio, nivel_educativo_jovenes) %>%
  left_join(
    extrapolacion_poblacion_joven_secundaria %>%
      pivot_longer(cols = c(secundaria_completa, secundaria_incompleta),
                   names_to = "nivel",
                   values_to = "poblacion") %>%
      mutate(nivel_educativo_jovenes = ifelse(nivel == "secundaria_completa",
                                              "Secundaria completa",
                                              "Secundaria incompleta")) %>%
      select(anio, nivel_educativo_jovenes, poblacion),
    by = c("anio", "nivel_educativo_jovenes")
  ) %>%
  mutate(tasa = (n / poblacion) * 100000) %>%
  filter(!is.na(tasa), anio >= 2000)

# Tasas suicidios adultos
tasas_suicidios_adultos <- defunciones_completas %>%
  filter(causa_externa == "Suicidio" & !is.na(nivel_educativo_adultos)) %>%
  count(anio, nivel_educativo_adultos) %>%
  left_join(
    extrapolacion_poblacion_adulta_superior %>%
      pivot_longer(cols = c(superior, no_superior),
                   names_to = "nivel",
                   values_to = "poblacion") %>%
      mutate(nivel_educativo_adultos = ifelse(nivel == "superior",
                                              "Superior",
                                              "No superior")) %>%
      select(anio, nivel_educativo_adultos, poblacion),
    by = c("anio", "nivel_educativo_adultos")
  ) %>%
  mutate(tasa = (n / poblacion) * 100000) %>%
  filter(!is.na(tasa), anio >= 2000)

# Límites comunes
ylim_suicidios <- range(c(tasas_suicidios_jovenes$tasa, tasas_suicidios_adultos$tasa), na.rm = TRUE)
ylim_suicidios[1] <- 0

# Gráfico jóvenes
p5 <- ggplot(tasas_suicidios_jovenes, 
             aes(x = anio, y = tasa, color = nivel_educativo_jovenes, group = nivel_educativo_jovenes)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  scale_color_manual(values = c("Secundaria completa" = "#27AE60", "Secundaria incompleta" = "#E74C3C")) +
  scale_x_continuous(breaks = seq(2000, 2024, by = 2)) +
  coord_cartesian(xlim = xlim_mortalidad, ylim = ylim_suicidios) +
  labs(
    title = "Jóvenes (20-29 años)",
    x = NULL,
    y = "Suicidios x 100,000 hab.",
    color = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Gráfico adultos
p6 <- ggplot(tasas_suicidios_adultos, 
             aes(x = anio, y = tasa, color = nivel_educativo_adultos, group = nivel_educativo_adultos)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  scale_color_manual(values = c("Superior" = "#27AE60", "No superior" = "#E74C3C")) +
  scale_x_continuous(breaks = seq(2000, 2024, by = 2)) +
  coord_cartesian(xlim = xlim_mortalidad, ylim = ylim_suicidios) +
  labs(
    title = "Adultos (30+ años)",
    x = "Año",
    y = "Suicidios x 100,000 hab.",
    color = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Combinar y guardar
g3 <- p5 + p6 + plot_annotation(
  title = "Suicidios por nivel educativo",
  subtitle = "Ecuador 2000-2024 - Tasas por 100,000 habitantes"
)

ggsave(file.path(output_dir, "serie_suicidios_educacion_2000_2024.png"), 
       g3, width = 14, height = 6, dpi = 300, bg = "white")
cat("  Guardado: serie_suicidios_educacion_2000_2024.png\n")

# =============================================================================
# 6. GRÁFICO 4: MORTALIDAD POR POBREZA (SERIE COMPLETA)
# =============================================================================

cat("Generando gráfico 4: Mortalidad por pobreza...\n")

# Unir con pobreza
def_completa_pobreza <- defunciones_completas %>%
  left_join(df_parroquias_pobreza, by = "cod_parroquia")

# Tasas por pobreza
tasas_pobreza_tiempo <- def_completa_pobreza %>%
  filter(!is.na(nivel_pobreza)) %>%
  count(anio, nivel_pobreza) %>%
  left_join(poblacion_pobreza, by = "nivel_pobreza") %>%
  mutate(tasa = (n / poblacion) * 1000) %>%
  filter(anio >= 2000)

# Gráfico
g4 <- ggplot(tasas_pobreza_tiempo, 
             aes(x = anio, y = tasa, color = nivel_pobreza, group = nivel_pobreza)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  scale_color_manual(values = colores_pobreza) +
  scale_x_continuous(breaks = seq(2000, 2024, by = 2)) +
  labs(
    title = "Mortalidad por nivel de pobreza de la parroquia",
    subtitle = "Ecuador 2000-2024 - Tasa por 1,000 habitantes",
    x = "Año",
    y = "Mortalidad x 1000 hab.",
    color = "Nivel de pobreza"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggsave(file.path(output_dir, "serie_mortalidad_pobreza_2000_2024.png"), 
       g4, width = 12, height = 6, dpi = 300, bg = "white")
cat("  Guardado: serie_mortalidad_pobreza_2000_2024.png\n")

# =============================================================================
# 7. RESUMEN
# =============================================================================

cat("\n========================================\n")
cat("PROCESO COMPLETADO\n")
cat("========================================\n")
cat("Gráficos generados:\n")
cat("  1. serie_mortalidad_educacion_2000_2024.png\n")
cat("  2. serie_homicidios_educacion_2000_2024.png\n")
cat("  3. serie_suicidios_educacion_2000_2024.png\n")
cat("  4. serie_mortalidad_pobreza_2000_2024.png\n")
cat("\nTodos guardados en:\n", output_dir, "\n")
cat("========================================\n")

