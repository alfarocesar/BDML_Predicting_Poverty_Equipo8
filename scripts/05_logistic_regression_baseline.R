################################################################################
# SCRIPT: Regresión Logística (glm) con validación de variables               #
# OBJETIVO: Entrenar modelo logit y generar predicciones para Kaggle          #
# NOMBRE ARCHIVO: logit_baseline.csv                                          #
################################################################################

# Configurar directorio de trabajo automáticamente
if (!require(rstudioapi)) install.packages("rstudioapi")
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# Subir un nivel directorio para acceder a la estructura principal del proyecto
setwd("../")

# Cargar librerías
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,      # Manipulación de datos
  skimr,          # Resumen
  yardstick       # F1-score en entrenamiento
)

# Fijar semilla
set.seed(123)

# ------------------------------------------------------------------------------
# 1. CARGAR DATOS
# ------------------------------------------------------------------------------
train <- read.csv("stores/processed/train_cleaned.csv")
test <- read.csv("stores/processed/test_cleaned.csv")

# ------------------------------------------------------------------------------
# 2. DEFINIR VARIABLES A USAR (top 10 de importancia por Random Forest)
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
# 3. ENTRENAMIENTO MODELO LOGIT
# ------------------------------------------------------------------------------
train$Pobre <- as.factor(train$Pobre)
X_train <- train %>% select(all_of(vars_modelo))
y_train <- train$Pobre

modelo_logit <- glm(Pobre ~ ., data = cbind(Pobre = y_train, X_train), family = "binomial")

# ------------------------------------------------------------------------------
# 4. EVALUACIÓN (F1-score sobre entrenamiento)
# ------------------------------------------------------------------------------
pred_train <- predict(modelo_logit, type = "response")
pred_train_class <- ifelse(pred_train > 0.5, 1, 0)
f1 <- f_meas_vec(truth = y_train, estimate = as.factor(pred_train_class), estimator = "binary")
cat("F1-score sobre datos de entrenamiento:", round(f1, 4), "\n")

# ------------------------------------------------------------------------------
# 5. PREDICCIÓN SOBRE TEST
# ------------------------------------------------------------------------------
id_test <- test$id

# Validar que todas las variables estén presentes
missing_cols <- setdiff(vars_modelo, names(test))
if (length(missing_cols) > 0) {
  stop(paste("Faltan variables en test:", paste(missing_cols, collapse = ", ")))
}

X_test <- test %>% select(all_of(vars_modelo))

# Predicción
pred_test_prob <- predict(modelo_logit, newdata = X_test, type = "response")
pred_test_class <- ifelse(pred_test_prob > 0.5, 1, 0)

# ------------------------------------------------------------------------------
# 6. FORMATO KAGGLE
# ------------------------------------------------------------------------------
salida_kaggle <- data.frame(
  id = id_test,
  pobre = pred_test_class
)

if (!dir.exists("stores/processed")) {
  dir.create("stores/processed", recursive = TRUE)
}

write.csv(salida_kaggle, "stores/processed/logit_baseline.csv", row.names = FALSE)
cat("✅ Archivo 'logit_baseline.csv' generado en 'stores/processed/'. Listo para Kaggle.\n")
