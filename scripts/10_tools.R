# Configurar directorio de trabajo automáticamente
if (!require(rstudioapi)) install.packages("rstudioapi")
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# Subir un nivel directorio para acceder a la estructura principal del proyecto
setwd("../")

# Cargar librerías
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,  # Manipulación de datos
  skimr       # Resumen de datos
)

# Fijar semilla
set.seed(123)

###########################################
# 1. CARGAR Y EXPLORAR BASES DE DATOS    #
###########################################

# Cargar datos
train_joined <- read.csv("stores/processed/train_joined.csv")
test_joined  <- read.csv("stores/processed/test_joined.csv")

# Mostrar dimensiones
cat("Dimensiones train:", dim(train_joined), "\n")
cat("Dimensiones test :", dim(test_joined), "\n")

###########################################
# 2. ANALIZAR VALORES FALTANTES          #
###########################################

# Función para calcular resumen de missing
resumen_missing <- function(df, nombre) {
  df %>%
    summarise(across(everything(), ~sum(is.na(.)))) %>%
    pivot_longer(cols = everything(), names_to = "variable", values_to = "n_missing") %>%
    mutate(pct_missing = round(100 * n_missing / nrow(df), 2)) %>%
    arrange(desc(pct_missing)) %>%
    { write.csv(., paste0("views/tables/", nombre, "_missing_summary.csv"), row.names = FALSE); . }
}

# Ejecutar para train y test
cat("\nResumen de valores faltantes en train_joined:\n")
print(resumen_missing(train_joined, "train_joined"))

cat("\nResumen de valores faltantes en test_joined:\n")
print(resumen_missing(test_joined, "test_joined"))

###########################################
# 3. EXPLORACIÓN DE ESTRUCTURA Y COLUMNAS
###########################################

cat("\nEstructura train:\n")
str(train_joined)

cat("\nColumnas train:\n")
print(colnames(train_joined))

cat("\nColumnas test:\n")
print(colnames(test_joined))
