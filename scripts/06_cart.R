################################################################################
# SCRIPT: 06_model_cart.R                                                     #
# PROYECTO: Predicción de Pobreza en Colombia                                 #
# OBJETIVO: Entrenar modelo CART con validación cruzada y predecir test       #
# FECHA: 13 de abril de 2025                                                  #
# SEMILLA: 1051                                                               #
################################################################################

# ------------------------------------------------------------------------------
# 0. CONFIGURACIÓN INICIAL
# ------------------------------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  rstudioapi, tidyverse, caret, yardstick, rpart.plot
)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../")
set.seed(1051)

# ------------------------------------------------------------------------------
# 1. CARGAR DATOS DE ENTRENAMIENTO Y TEST
# ------------------------------------------------------------------------------
cat("Cargando bases de datos...\n")
train <- read.csv("stores/processed/train_cleaned.csv")
test  <- read.csv("stores/processed/test_cleaned.csv")

# Convertir variable objetivo a factor
train$Pobre <- as.factor(train$Pobre)

# Guardar ID test
id_test <- test$id

# Eliminar columnas no predictivas
train <- train %>% select(-id)
test  <- test %>% select(-id)

# ------------------------------------------------------------------------------
# 2. ENTRENAMIENTO CART CON VALIDACIÓN CRUZADA
# ------------------------------------------------------------------------------
cat("Entrenando modelo CART (rpart)...\n")
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
  trControl = ctrl,
  metric = "F"
)

# ------------------------------------------------------------------------------
# 3. EVALUACIÓN DEL MODELO EN ENTRENAMIENTO
# ------------------------------------------------------------------------------
cat("Evaluando desempeño en validación cruzada...\n")
pred_all <- modelo_cart$pred

# Usar predicciones completas
f1_total <- f_meas_vec(
  truth = pred_all$obs,
  estimate = pred_all$pred,
  estimator = "binary",
  event_level = "second"
)

cat("F1-score promedio validación cruzada (5-fold):", round(f1_total, 4), "\n")

# ------------------------------------------------------------------------------
# 4. VISUALIZAR ÁRBOL DE DECISIÓN
# ------------------------------------------------------------------------------
if (!dir.exists("views/figures")) dir.create("views/figures", recursive = TRUE)

png("views/figures/cart_tree_structure.png", width = 1000, height = 800, res = 120)
rpart.plot(modelo_cart$finalModel, type = 3, extra = 102, fallen.leaves = TRUE)
dev.off()

cat("Visualización del árbol guardada en views/figures/cart_tree_structure.png\n")

# ------------------------------------------------------------------------------
# 5. PREDICCIÓN SOBRE TEST Y EXPORTACIÓN A KAGGLE
# ------------------------------------------------------------------------------
pred_test <- predict(modelo_cart, newdata = test)

salida <- data.frame(
  id = id_test,
  pobre = ifelse(pred_test == "Yes", 1, 0)
)

if (!dir.exists("stores/processed")) {
  dir.create("stores/processed", recursive = TRUE)
}

write.csv(salida, "stores/processed/cart_model.csv", row.names = FALSE)
cat("Archivo 'cart_model.csv' generado en stores/processed/. Listo para Kaggle.\n")
