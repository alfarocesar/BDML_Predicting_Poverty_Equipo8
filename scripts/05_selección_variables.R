################################################################################
# SCRIPT: Importancia de Variables con Random Forest                          #
# OBJETIVO: Identificar variables más importantes sin balancear clases        #
################################################################################

# Configurar directorio de trabajo automáticamente
if (!require(rstudioapi)) install.packages("rstudioapi")
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../")

# Cargar librerías
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,    # Manipulación de datos
  randomForest, # Modelo Random Forest
  skimr         # Resumen de datos
)

# Fijar semilla unificada
set.seed(1051)

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

print(head(importancia_df, 20))

# Crear carpetas
if (!dir.exists("views/tables")) dir.create("views/tables", recursive = TRUE)
if (!dir.exists("views/figures")) dir.create("views/figures", recursive = TRUE)

# Guardar lista completa
write.csv(importancia_df, "views/tables/rf_variable_importance.csv", row.names = FALSE)

# ------------------------------------------------------------------------------
# 4. VISUALIZACIÓN DE RESULTADOS
# ------------------------------------------------------------------------------
top_vars <- head(importancia_df, 20)
ggplot(top_vars, aes(x = reorder(variable, importancia), y = importancia)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Top 20 Variables por Importancia (Random Forest)",
       x = "Variable", y = "Importancia (MeanDecreaseGini)") +
  theme_minimal() +
  ggsave("views/figures/rf_variable_importance.png", width = 8, height = 6)

cat("Importancia de variables guardada en:\n - CSV: views/tables/rf_variable_importance.csv\n - PNG: views/figures/rf_variable_importance.png\n")
