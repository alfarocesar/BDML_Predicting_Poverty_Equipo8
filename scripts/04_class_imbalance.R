################################################################################
# TÍTULO: 04_class_imbalance.R                                               #
# PROYECTO: Predicción de Pobreza en Colombia - Equipo 8                      #
# DESCRIPCIÓN: Tratamiento del desbalance de clases y comparación de modelos  #
# FECHA: 13 de abril de 2025                                                   #
################################################################################

# Cargar librerías necesarias
if (!require("pacman")) install.packages("pacman")
pacman::p_load(caret, Metrics, MLmetrics, glmnet, rpart, randomForest, xgboost, naivebayes, dplyr)

# 1. CONTROL DE ENTRENAMIENTO --------------------------------------------------
ctrl <- trainControl(
  method = "cv",
  number = 5,
  classProbs = TRUE,
  savePredictions = "final",
  summaryFunction = prSummary
)

# 2. FÓRMULA Y MODELOS ----------------------------------------------------------
form <- Pobre ~ .

modelos <- list(
  logit = train(form, data = train, method = "glm", family = "binomial", trControl = ctrl, metric = "F"),
  elasticnet = train(form, data = train, method = "glmnet", trControl = ctrl, metric = "F",
                     tuneGrid = expand.grid(
                       alpha = seq(0, 1, 0.2),
                       lambda = 10^seq(-1, -3, length = 10)
                     )),
  cart = train(form, data = train, method = "rpart", trControl = ctrl, metric = "F"),
  rf = train(form, data = train, method = "rf", trControl = ctrl, metric = "F"),
  boosting = train(form, data = train, method = "xgbTree", trControl = ctrl, metric = "F"),
  nb = train(form, data = train, method = "naive_bayes", trControl = ctrl, metric = "F")
)

# 3. SELECCIÓN DEL MEJOR MODELO --------------------------------------------------
f1_scores <- sapply(modelos, function(m) max(m$results$F, na.rm = TRUE))
mejor_modelo <- modelos[[which.max(f1_scores)]]

# 4. F1 SCORE EN VALIDACIÓN CRUZADA ---------------------------------------------
preds_bestmodel <- mejor_modelo$pred

if (!is.null(mejor_modelo$bestTune)) {
  best_params <- mejor_modelo$bestTune
  preds_bestmodel <- preds_bestmodel %>%
    dplyr::filter_at(vars(names(best_params)), all_vars(. == best_params[[cur_column()]]))
}

f1_best <- F1_Score(y_true = preds_bestmodel$obs,
                    y_pred = preds_bestmodel$pred,
                    positive = "Yes")
cat("F1-Best Model :", round(f1_best, 4), "\n")

# 5. PREDICCIONES SOBRE TEST ---------------------------------------------------
predictSample <- test %>% 
  mutate(pobre_lab = predict(mejor_modelo, newdata = test, type = "raw")) %>% 
  mutate(pobre = ifelse(pobre_lab == "Yes", 1, 0)) %>% 
  select(id, pobre)

# 6. EXPORTAR RESULTADOS -------------------------------------------------------
template <- read.csv("sample_submission.csv")

if (!is.null(mejor_modelo$bestTune)) {
  lambda_str <- gsub("\\.", "_", as.character(round(mejor_modelo$bestTune$lambda, 4)))
  alpha_str <- gsub("\\.", "_", as.character(mejor_modelo$bestTune$alpha))
  name <- paste0("EN_lambda_", lambda_str, "alpha", alpha_str, ".csv")
} else {
  name <- paste0("mejor_modelo_", names(modelos)[which.max(f1_scores)], ".csv")
}

write.csv(predictSample, name, row.names = FALSE)

################################################################################
#                            FIN DEL SCRIPT                                    #
################################################################################
