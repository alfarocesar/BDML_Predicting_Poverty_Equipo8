################################################################################
# SCRIPT: 06_model_logit_cv.R                                                 #
# MODELO: Regresión Logística con Validación Cruzada                          #
# PROYECTO: Predicción de Pobreza en Colombia                                 #
# OBJETIVO: Entrenar un modelo logit con validación cruzada estratificada     #
#           y exportar predicciones en formato Kaggle                         #
# SEMILLA: 1051                                                               #
################################################################################

# Configurar el directorio automáticamente
if (!require("pacman")) install.packages("pacman")
pacman::p_load(rstudioapi, tidyverse, caret, yardstick, glmnet)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../")

# Semilla
set.seed(1051)

# ------------------------------------------------------------------------------
# 1. CARGAR DATOS
# ------------------------------------------------------------------------------
train <- read.csv("stores/processed/train_cleaned.csv")
test <- read.csv("stores/processed/test_cleaned.csv")

# ------------------------------------------------------------------------------
# 2. DEFINIR VARIABLES A UTILIZAR
# ------------------------------------------------------------------------------
vars_modelo <- c(
  "n_menores",
  "edad_promedio",
  "prop_empl_part_empr",
  "Lp",
  "Li",
  "prop_cotiza_pension",
  "promedio_horas_trab",
  "jefe_edad",
  "max_edad",
  "tiene_arriendo"
)

# ------------------------------------------------------------------------------
# 3. PREPARAR DATOS
# ------------------------------------------------------------------------------
train$Pobre <- factor(train$Pobre, levels = c(0, 1), labels = c("No", "Yes"))
X_train <- train %>% select(all_of(vars_modelo))
y_train <- train$Pobre

# ------------------------------------------------------------------------------
# 4. ENTRENAMIENTO CON VALIDACIÓN CRUZADA
# ------------------------------------------------------------------------------
ctrl <- trainControl(
  method = "cv",
  number = 5,
  summaryFunction = prSummary, 
  classProbs = TRUE,
  savePredictions = "final"
)

modelo_logit <- train(
  x = X_train,
  y = y_train,
  method = "glm",
  family = "binomial",
  trControl = ctrl,
  metric = "F1" 
)

# ------------------------------------------------------------------------------
# 5. EVALUACIÓN EN CV
# ------------------------------------------------------------------------------
cat("Resultados promedio en validación cruzada:\n")
print(modelo_logit$results)

# ------------------------------------------------------------------------------
# 6. PREDICCIÓN SOBRE TEST
# ------------------------------------------------------------------------------
id_test <- test$id

# Verificar variables en test
missing_cols <- setdiff(vars_modelo, names(test))
if (length(missing_cols) > 0) {
  stop(paste("Faltan variables en test:", paste(missing_cols, collapse = ", ")))
}
X_test <- test %>% select(all_of(vars_modelo))

# Predicción de probabilidades y clases
pred_test_prob <- predict(modelo_logit, newdata = X_test, type = "prob")[, "Yes"]
pred_test_class <- ifelse(pred_test_prob > 0.5, 1, 0)

# ------------------------------------------------------------------------------
# 7. EXPORTAR RESULTADO PARA KAGGLE
# ------------------------------------------------------------------------------
salida_kaggle <- data.frame(id = id_test, pobre = pred_test_class)

if (!dir.exists("stores/processed")) {
  dir.create("stores/processed", recursive = TRUE)
}
write.csv(salida_kaggle, "stores/processed/logit_cv_top10.csv", row.names = FALSE)
cat("✅ Archivo 'logit_cv_top10.csv' guardado correctamente en 'stores/processed/'.\n")

# ------------------------------------------------------------------------------
# 8. MATRIZ DE CONFUSIÓN EN CV
# ------------------------------------------------------------------------------
conf_matrix <- confusionMatrix(modelo_logit$pred$pred, modelo_logit$pred$obs, positive = "Yes")
cat("\nMatriz de confusión en validación cruzada:\n")
print(conf_matrix)

