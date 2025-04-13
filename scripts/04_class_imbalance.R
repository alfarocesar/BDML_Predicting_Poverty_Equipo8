################################################################################
# TÍTULO: 04_class_imbalance.R                                                #
# PROYECTO: Predicción de Pobreza en Colombia - Equipo 8                      #
# DESCRIPCIÓN: Configuración de control de entrenamiento para modelos         #
#              supervisados. Este script no entrena modelos aún, solo prepara #
#              la estructura para el entrenamiento y evaluación.              #
# FECHA: 08 de abril de 2025                                                  #
################################################################################

# control de entrenamiento que será utilizado por los modelos posteriores.

## Control de validación cruzada para modelos de clasificación
## - Se usa validación cruzada con 5 particiones (folds)
## - Se calculan probabilidades de clase (`classProbs = TRUE`)
## - Se utiliza `prSummary` como función resumen (precision, recall, F1)

ctrl <- trainControl(
  method = "cv",                  # Validación cruzada
  number = 5,                     # Número de folds
  classProbs = TRUE,             # Probabilidades de clase
  savePredictions = "final",     # Guardar predicciones finales
  summaryFunction = prSummary    # Métricas: precisión, recall, F1
)

# Semilla para reproducibilidad
set.seed(1051)

# Entrenamiento de un primer modelo base usando regularización tipo ElasticNet (glmnet)
# Este modelo sirve como punto de comparación inicial.
model1 <- train(
  Pobre ~ .,                      # Fórmula: variable objetivo vs. predictores
  data = train,                   # Conjunto de entrenamiento
  metric = "Accuracy",            # Métrica de optimización: Accuracy (cambiar a F1 si se desea)
  method = "glmnet",              # Algoritmo Elastic Net (regresión regularizada)
  trControl = ctrl,              # Control de entrenamiento
  tuneGrid = expand.grid(        # Grid de hiperparámetros
    alpha = seq(0, 1, by = 0.2),        # Alpha entre Ridge (0) y Lasso (1)
    lambda = 10^seq(10, -2, length = 10) # Valores de penalización lambda
  )
)

# Visualizar resultados del modelo base
model1

################################################################################
# Sección complementaria: redefinición del control con métricas de evaluación #
################################################################################

# Cargar paquete de métricas si no ha sido cargado
p_load(Metrics)

# Definir función resumen personalizada con métricas adicionales si se desea
fiveStats <- function(...) c(prSummary(...))

# Redefinición del objeto `ctrl` si se requiere reutilizar con métricas adicionales
ctrl <- trainControl(
  method = "cv",
  number = 5,
  classProbs = TRUE,
  savePredictions = "final",
  summaryFunction = prSummary
)
