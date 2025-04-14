################################################################################
# SCRIPT: 05_logistic_regression.R                                            #
# PROYECTO: Predicción de Pobreza en Colombia - Equipo 8                      #
# DESCRIPCIÓN: Entrenamiento de modelo de Regresión Logística                 #
#              con validación cruzada y métricas de evaluación                #
# FECHA: 13 de abril de 2025                                                  #
################################################################################

# Cargar librerías necesarias
library(caret)
library(Metrics)
library(dplyr)

# Control de entrenamiento con validación cruzada (cv = 5)
set.seed(1051)  # Semilla fija para reproducibilidad
ctrl <- trainControl(
  method = "cv",
  number = 5,
  classProbs = TRUE,
  savePredictions = "final",
  summaryFunction = prSummary  # Precision, Recall, F1
)

# Entrenar modelo de regresión logística
modelo_logit <- train(
  Pobre ~ .,  # Fórmula: variable objetivo vs todas las variables
  data = train,  # Datos de entrenamiento
  method = "glm",  # Generalized Linear Model (logístico)
  family = "binomial",
  metric = "F",  # Métrica objetivo
  trControl = ctrl
)

# Mostrar resultados de entrenamiento
print(modelo_logit)

# Evaluación del modelo con predicciones cruzadas
predicciones <- modelo_logit$pred
f1_logit <- F1_Score(y_true = predicciones$obs,
                     y_pred = predicciones$pred,
                     positive = "Yes")
cat("\n🔹 F1-Score del modelo logístico:", round(f1_logit, 4), "\n")

# Generar predicciones sobre el conjunto de prueba
test_predictions <- test %>% 
  mutate(pobre_lab = predict(modelo_logit, newdata = test, type = "raw")) %>% 
  mutate(pobre = ifelse(pobre_lab == "Yes", 1, 0)) %>%
  select(id, pobre)

# Exportar archivo de predicción con nombre informativo
nombre_archivo <- "05_logit_model_submission.csv"
write.csv(test_predictions, nombre_archivo, row.names = FALSE)

cat("\n✅ Archivo de predicción guardado como:", nombre_archivo, "\n")
