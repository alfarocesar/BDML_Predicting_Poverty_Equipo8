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
train_cleaned <- read.csv("stores/processed/train_cleaned.csv")
test_cleaned  <- read.csv("stores/processed/test_cleaned.csv")

# Mostrar dimensiones
cat("Dimensiones train:", dim(train_cleaned), "\n")
cat("Dimensiones test :", dim(test_cleaned), "\n")

###########################################
# 2. ANALIZAR VALORES FALTANTES          #
###########################################

# Función para calcular resumen de missing
resumen_missing <- function(df, df_name) {
  df %>%
    summarise(across(everything(), ~sum(is.na(.)))) %>%
    pivot_longer(cols = everything(), names_to = "variable", values_to = "n_missing") %>%
    mutate(pct_missing = round(100 * n_missing / nrow(df), 2)) %>%
    arrange(desc(pct_missing)) %>%
    { write.csv(., paste0("views/tables/", df_name, "_missing_summary.csv"), row.names = FALSE); . }
}

# Aplicar a train y test
cat("\nResumen de valores faltantes en train:\n")
print(resumen_missing(train_cleaned, "train_cleaned"))

cat("\nResumen de valores faltantes en test:\n")
print(resumen_missing(test_cleaned, "test_cleaned"))

###########################################
# 3. ESTRUCTURA DE VARIABLES             #
###########################################

cat("\nEstructura train:\n")
str(train_cleaned)

cat("\nColumnas train:\n")
print(colnames(train_cleaned))

cat("\nColumnas test:\n")
print(colnames(test_cleaned))
