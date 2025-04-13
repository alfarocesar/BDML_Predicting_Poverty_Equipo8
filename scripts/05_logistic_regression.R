################################################################################
# TÍTULO: 05_logistic_regression.R                                            #
# PROYECTO: Predicción de Pobreza en Colombia - Equipo 8                      #
# DESCRIPCIÓN: Entrenamiento del modelo de regresión logística                #
#              para clasificación binaria de pobreza                         #
# FECHA: 10 de abril de 2025                                                  #
################################################################################

# Cargar librerías necesarias
require(pacman)
p_load(tidyverse, caret, Metrics, MLmetrics)

# Fijar semilla para asegurar reproducibilidad
set.seed(1051)

################################################################################
# CONTROL DE ENTRENAMIENTO                                                     #
################################################################################

# Configurar validación cruzada estratificada con 5 folds y evaluación F1
ctrl <- trainControl(
  method = "cv",
  number = 5,
  classProbs = TRUE,
  savePredictions = "final",
  summaryFunction = prSummary
)

################################################################################
# ENTRENAMIENTO DEL MODELO: REGRESIÓN LOGÍSTICA                               #
################################################################################

# Fórmula del modelo
form <- Pobre ~ .

# Entrenar modelo logístico binario
modelo_logit <- train(
  form,
  data = train,
  method = "glm",
  family = "binomial",
  metric = "F",
  trControl = ctrl
)

# Mostrar resumen del modelo
print(modelo_logit)

################################################################################
# EVALUACIÓN DEL MODELO                                                       #
################################################################################

# Obtener predicciones cruzadas
preds_logit <- modelo_logit$pred

# Calcular F1 Score
f1_logit <- F1_Score(
  y_true = preds_logit$obs,
  y_pred = preds_logit$pred,
  positive = "Yes"
)

cat("F1 - Modelo Logístico:", round(f1_logit, 4), "\n")

################################################################################
# GENERACIÓN DE PREDICCIONES SOBRE EL SET DE TEST                             #
################################################################################

# Realizar predicciones en la base de test
predicciones_test <- test %>%
  mutate(
    pobre_lab = predict(modelo_logit, newdata = test, type = "raw"),
    pobre = ifelse(pobre_lab == "Yes", 1, 0)
  ) %>%
  select(id, pobre)

# Exportar resultados
write.csv(predicciones_test, "logit_predictions.csv", row.names = FALSE)

################################################################################
# FIN DEL SCRIPT                                                               #
################################################################################
