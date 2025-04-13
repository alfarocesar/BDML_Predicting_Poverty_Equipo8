################################################################################
# SCRIPT: Random Forest con validación cruzada                                #
# OBJETIVO: Entrenar modelo Random Forest y generar predicciones para Kaggle  #
# NOMBRE ARCHIVO: rf_model.csv                                                #
################################################################################

# Configurar directorio de trabajo automáticamente
if (!require(rstudioapi)) install.packages("rstudioapi")
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../")

# Cargar librerías necesarias
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,     # Manipulación de datos
  caret,         # Modelos con validación cruzada
  randomForest,  # Algoritmo Random Forest
  yardstick      # F1-score
)

# Fijar semilla
set.seed(1051)

# ------------------------------------------------------------------------------
# 1. CARGAR DATOS
# ------------------------------------------------------------------------------
train <- read.csv("stores/processed/train_cleaned.csv")
test <- read.csv("stores/processed/test_cleaned.csv")

# Asegurar que la variable objetivo sea factor
train$Pobre <- as.factor(train$Pobre)

# Extraer IDs
id_test <- test$id
if ("id" %in% names(train)) train <- train %>% select(-id)
if ("id" %in% names(test)) test <- test %>% select(-id)

# ------------------------------------------------------------------------------
# 2. CONFIGURAR ENTRENAMIENTO
# ------------------------------------------------------------------------------
ctrl <- trainControl(
  method = "cv",
  number = 5,
  classProbs = TRUE,
  savePredictions = "final",
  summaryFunction = prSummary
)

# Entrenar modelo Random Forest
modelo_rf <- train(
  Pobre ~ .,
  data = train,
  method = "rf",
  metric = "F",
  trControl = ctrl
)

# ------------------------------------------------------------------------------
# 3. EVALUACIÓN SOBRE ENTRENAMIENTO
# ------------------------------------------------------------------------------
pred_train <- modelo_rf$pred %>%
  filter(Resample == "Fold1")  # Una fold como referencia

f1_train <- f_meas_vec(
  truth = pred_train$obs,
  estimate = pred_train$pred,
  estimator = "binary"
)

cat("F1-score sobre entrenamiento (Fold1):", round(f1_train, 4), "\n")

# ------------------------------------------------------------------------------
# 4. PREDICCIÓN SOBRE TEST
# ------------------------------------------------------------------------------
pred_test_class <- predict(modelo_rf, newdata = test)

# ------------------------------------------------------------------------------
# 5. FORMATO KAGGLE
# ------------------------------------------------------------------------------
salida <- data.frame(
  id = id_test,
  pobre = ifelse(pred_test_class == "Yes", 1, 0)
)

if (!dir.exists("stores/processed")) {
  dir.create("stores/processed", recursive = TRUE)
}

write.csv(salida, "stores/processed/rf_model.csv", row.names = FALSE)
cat("Archivo 'rf_model.csv' generado en 'stores/processed/'. Listo para Kaggle.\n")
