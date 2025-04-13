################################################################################
# SCRIPT: Importancia de Variables con Random Forest                          #
# OBJETIVO: Identificar variables más importantes sin balancear clases        #
################################################################################

# Configurar directorio de trabajo automáticamente
if (!require(rstudioapi)) install.packages("rstudioapi")
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# Subir un nivel directorio para acceder a la estructura principal del proyecto
setwd("../")

# Cargar librerías
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,    # Manipulación de datos
  randomForest, # Modelo Random Forest
  skimr         # Resumen de datos
)

# Fijar semilla
set.seed(123)

# ------------------------------------------------------------------------------
# 1. CARGAR BASE
# ------------------------------------------------------------------------------
cat("Cargando base...\n")
train <- read.csv("stores/processed/train_cleaned.csv")

# Asegurar que la variable 'Pobre' esté como factor
train$Pobre <- as.factor(train$Pobre)

# Eliminar columnas no predictoras (id, si existe)
if ("id" %in% names(train)) {
  train <- train %>% select(-id)
}

# ------------------------------------------------------------------------------
# 2. ENTRENAR RANDOM FOREST
# ------------------------------------------------------------------------------
cat("Entrenando modelo Random Forest...\n")

# Eliminar filas con NA
train_rf <- train %>% drop_na()

modelo_rf <- randomForest(Pobre ~ ., data = train_rf, importance = TRUE, ntree = 100)

# ------------------------------------------------------------------------------
# 3. EXTRAER IMPORTANCIA DE VARIABLES
# ------------------------------------------------------------------------------
importancia <- importance(modelo_rf, type = 2)  # MeanDecreaseGini
importancia_df <- data.frame(variable = rownames(importancia), 
                             importancia = importancia[, "MeanDecreaseGini"]) %>%
  arrange(desc(importancia))

# Mostrar top 20 en consola
print(head(importancia_df, 20))

# Crear carpeta si no existe
if (!dir.exists("views/tables")) {
  dir.create("views/tables", recursive = TRUE)
}

# Guardar lista completa
write.csv(importancia_df, "views/tables/rf_variable_importance.csv", row.names = FALSE)

cat("Importancia de variables guardada en views/tables/rf_variable_importance.csv\n")
