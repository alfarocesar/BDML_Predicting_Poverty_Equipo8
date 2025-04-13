################################################################################
# SCRIPT: Árbol de Clasificación (CART) con validación cruzada                #
# OBJETIVO: Entrenar modelo CART y generar predicciones para Kaggle           #
# NOMBRE ARCHIVO: cart_model.csv                                              #
################################################################################

# Configurar directorio de trabajo automáticamente
if (!require(rstudioapi)) install.packages("rstudioapi")
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../")

# Cargar librerías
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,    # Manipulación de datos
  caret,        # Entrenamiento modelos
  yardstick     # F1-score
)

# Fijar semilla
set.seed(1051)

# ------------------------------------------------------------------------------
# 1. CARGAR DATOS
# ------------------------------------------------------------------------------
train <- read.csv("stores/processed/train_cleaned.csv")
test <- read.csv("stores/processed/test_cleaned.csv")

# Convertir variable objetivo a factor
train$Pobre <- as.factor(train$Pobre)

# Separar ID del test para entrega
id_test <- test$id

# Eliminar ID si está en train o test
if ("id" %in% names(train)) train <- train %>% select(-id)
if ("id" %in% names(test))  test  <- test %>% select(-id)

# ------------------------------------------------------------------------------
# 2. CONFIGURACIÓN DE CONTROL Y ENTRENAMIENTO DEL MODELO
# ------------------------------------------------------------------------------
ctrl <- trainControl(
  method = "cv",
  number = 5,
  classProbs = TRUE,
  savePredictions = "final",
  summaryFunction = prSummary
)

modelo_cart <- train(
  Pobre ~ ., 
  data = train,
  method = "rpart",
  metric = "F",
  trControl = ctrl
)

# ------------------------------------------------------------------------------
# 3. EVALUACIÓN SOBRE ENTRENAMIENTO
# ------------------------------------------------------------------------------
pred_train <- modelo_cart$pred %>%
  filter(Resample == "Fold1")  # Elegimos una fold como referencia

f1_train <- f_meas_vec(
  truth = pred_train$obs,
  estimate = pred_train$pred,
  estimator = "binary"
)

cat("F1-score sobre entrenamiento (Fold1):", round(f1_train, 4), "\n")

# ------------------------------------------------------------------------------
# 4. PREDICCIÓN SOBRE TEST
# ------------------------------------------------------------------------------
pred_test_class <- predict(modelo_cart, newdata = test)

# ------------------------------------------------------------------------------
# 5. EXPORTAR RESULTADOS
# ------------------------------------------------------------------------------
salida <- data.frame(
  id = id_test,
  pobre = ifelse(pred_test_class == "Yes", 1, 0)
)

if (!dir.exists("stores/processed")) {
  dir.create("stores/processed", recursive = TRUE)
}

write.csv(salida, "stores/processed/cart_model.csv", row.names = FALSE)
cat("Archivo 'cart_model.csv' generado en 'stores/processed/'. Listo para Kaggle.\n")
