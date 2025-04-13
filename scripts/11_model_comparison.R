#install.packages("MLmetrics")
require(MLmetrics)

# Ver resultados
resamples(modelos) %>% summary()


# Elegir el modelo con mejor F1
f1_scores <- sapply(modelos, function(m) max(m$results$F, na.rm = TRUE))
mejor_modelo <- modelos[[which.max(f1_scores)]]

#Obtener las predicciones cruzadas
preds_bestmodel <- mejor_modelo$pred

best_params <- mejor_modelo$bestTune
preds_bestmodel <- preds_bestmodel %>%
  dplyr::filter(alpha == best_params$alpha, lambda == best_params$lambda)

#Calcular f1
f1_best <- F1_Score(y_true = preds_bestmodel$obs,
                    y_pred = preds_bestmodel$pred,
                    positive = "Yes"
                    )

cat("F1-Best Model :", round(f1_best,4), "\n")
