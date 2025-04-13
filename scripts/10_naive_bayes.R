################################################################################
# SCRIPT: Naive Bayes con validación cruzada                                  #
# OBJETIVO: Entrenar modelo Naive Bayes y generar predicciones para Kaggle    #
# NOMBRE ARCHIVO: naive_bayes_model.csv                                       #
################################################################################

# Configurar directorio de trabajo automáticamente
if (!require(rstudioapi)) install.packages("rstudioapi")
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../")

# Cargar librerías necesarias
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,     # Manipulación de datos
  caret,         # Entrenamiento con validación cruzada
  naivebayes,    # Modelo Naive Bayes
  yardstick      # Evaluación con F1-score
)

# Fijar semilla para reproducibilidad
set.seed(1051)

# ------------------------------------------------------------------------------
# 1. CARGAR DATOS
# ------------------------------------------------------------------------------
train <- read.csv("stores/processed/train_cleaned.csv")
test <- read.csv("stores/processed/test_cleaned.csv")

# Convertir variable objetivo a factor
train$Pobre <- as.factor(train$Pobre)

# Extraer ID de test
id_test <- test$id
if ("id" %in% names(train)) train <- train %>% select(-id)
if ("id" %in% names(test)) test <- test %>% select(-id)

# ------------------------------------------------------------------------------
# 2. CONFIGURAR CONTROL DE ENTRENAMIENTO
# ------------------------------------------------------------------------------
ctrl <- trainControl(
  method = "cv",
  number = 5,
  classProbs = TRUE,
  savePredictions = "final",
  summaryFunction = prSummary
)

# ------------------------------------------------------------------------------
# 3. ENTRENAMIENTO DEL MODELO NAIVE BAYES
# ------------------------------------------------------------------------------
modelo_nb <- train(
  Pobre ~ .,
  data = train,
  method = "naive_bayes",
  metric = "F",
  trControl = ctrl
)

# ------------------------------------------------------------------------------
# 4. EVALUACIÓN DEL MODELO
# ------------------------------------------------------------------------------
pred_train <- modelo_nb$pred %>%
  filter(Resample == "Fold1")  # Tomamos Fold1 como referencia

f1_train <- f_meas_vec(
  truth = pred_train$obs,
  estimate = pred_train$pred,
  estimator = "binary"
)

cat("F1-score sobre entrenamiento (Fold1):", round(f1_train, 4), "\n")

# ------------------------------------------------------------------------------
# 5. PREDICCIÓN SOBRE TEST
# ------------------------------------------------------------------------------
pred_test_class <- predict(modelo_nb, newdata = test)

# ------------------------------------------------------------------------------
# 6. GENERACIÓN DE ARCHIVO PARA KAGGLE
# ------------------------------------------------------------------------------
salida <- data.frame(
  id = id_test,
  pobre = ifelse(pred_test_class == "Yes", 1, 0)
)

# Crear carpeta de salida si no existe
if (!dir.exists("stores/processed")) {
  dir.create("stores/processed", recursive = TRUE)
}

write.csv(salida, "stores/processed/naive_bayes_model.csv", row.names = FALSE)
cat("Archivo 'naive_bayes_model.csv' generado en 'stores/processed/'. Listo para Kaggle.\n")
