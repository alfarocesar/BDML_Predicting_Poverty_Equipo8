################################################################################
# SCRIPT: 07_model_rf.R                                                       #
# PROYECTO: Predicción de Pobreza en Colombia                                 #
# OBJETIVO: Entrenar modelo Random Forest con CV y predecir test              #
# FECHA: 13 de abril de 2025                                                  #
# SEMILLA: 1051                                                               #
################################################################################

# ------------------------------------------------------------------------------
# 0. CARGA DE LIBRERÍAS Y CONFIGURACIÓN
# ------------------------------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  rstudioapi, tidyverse, caret, randomForest, yardstick
)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../")
set.seed(1051)

# ------------------------------------------------------------------------------
# 1. CARGAR DATOS DE ENTRENAMIENTO Y TEST
# ------------------------------------------------------------------------------
cat("Cargando bases...\n")
train <- read.csv("stores/processed/train_cleaned.csv")
test <- read.csv("stores/processed/test_cleaned.csv")

train$Pobre <- as.factor(train$Pobre)

id_test <- test$id
train <- train %>% select(-id)
test  <- test %>% select(-id)

# ------------------------------------------------------------------------------
# 2. CONFIGURAR VALIDACIÓN Y ENTRENAMIENTO
# ------------------------------------------------------------------------------
cat("Entrenando modelo Random Forest...\n")
ctrl <- trainControl(
  method = "cv",
  number = 5,
  classProbs = TRUE,
  savePredictions = "final",
  summaryFunction = prSummary
)

modelo_rf <- train(
  Pobre ~ .,
  data = train,
  method = "rf",
  trControl = ctrl,
  metric = "F"
)

# ------------------------------------------------------------------------------
# 3. EVALUACIÓN SOBRE VALIDACIÓN CRUZADA
# ------------------------------------------------------------------------------
cat("Evaluando desempeño promedio en CV...\n")
f1_cv <- f_meas_vec(
  truth = modelo_rf$pred$obs,
  estimate = modelo_rf$pred$pred,
  estimator = "binary",
  event_level = "second"
)

cat("F1-score promedio validación cruzada (5-fold):", round(f1_cv, 4), "\n")

# ------------------------------------------------------------------------------
# 4. IMPORTANCIA DE VARIABLES
# ------------------------------------------------------------------------------
cat("📌 Calculando importancia de variables...\n")
importancia_rf <- varImp(modelo_rf)$importance %>%
  rownames_to_column("variable") %>%
  arrange(desc(Overall))

# Guardar tabla
if (!dir.exists("views/tables")) dir.create("views/tables", recursive = TRUE)
write.csv(importancia_rf, "views/tables/variable_importance_rf.csv", row.names = FALSE)

# Visualizar
if (!dir.exists("views/figures")) dir.create("views/figures", recursive = TRUE)

top_vars <- importancia_rf %>% head(20)
ggplot(top_vars, aes(x = reorder(variable, Overall), y = Overall)) +
  geom_col(fill = "darkgreen") +
  coord_flip() +
  labs(title = "Top 20 Variables - Importancia (Random Forest)",
       x = "Variable", y = "Importancia") +
  theme_minimal()
ggsave("views/figures/importance_rf.png", width = 8, height = 6)

# ------------------------------------------------------------------------------
# 5. PREDICCIÓN SOBRE TEST Y EXPORTACIÓN
# ------------------------------------------------------------------------------
cat("📤 Generando predicciones para Kaggle...\n")
pred_rf <- predict(modelo_rf, newdata = test)

salida <- data.frame(
  id = id_test,
  pobre = ifelse(pred_rf == "Yes", 1, 0)
)

if (!dir.exists("stores/processed")) dir.create("stores/processed", recursive = TRUE)
write.csv(salida, "stores/processed/rf_model.csv", row.names = FALSE)
cat("Archivo 'rf_model.csv' generado en stores/processed/. Listo para Kaggle.\n")
