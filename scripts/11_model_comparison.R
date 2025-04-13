# Cargar librería yardstick (mejor integrada con caret/tidyverse)
if (!require("yardstick")) install.packages("yardstick")
library(yardstick)

# Ver resultados de todos los modelos
resamples(modelos) %>% summary()

# Identificar mejor modelo según F1-score
f1_scores <- sapply(modelos, function(m) max(m$results$F, na.rm = TRUE))
mejor_modelo <- modelos[[which.max(f1_scores)]]
cat("Mejor modelo:", names(modelos)[which.max(f1_scores)], "\n")

# Obtener predicciones cruzadas
preds_bestmodel <- mejor_modelo$pred

# Filtrar por los mejores hiperparámetros si existen
if (!is.null(mejor_modelo$bestTune)) {
  cols_best <- names(mejor_modelo$bestTune)
  for (param in cols_best) {
    preds_bestmodel <- preds_bestmodel %>%
      filter(!!sym(param) == mejor_modelo$bestTune[[param]])
  }
}

# Calcular F1 usando yardstick
f1_best <- preds_bestmodel %>%
  mutate(obs = factor(obs, levels = c("No", "Yes")),
         pred = factor(pred, levels = c("No", "Yes"))) %>%
  f_meas(truth = obs, estimate = pred) %>%
  pull(.estimate)

cat("F1-score del mejor modelo:", round(f1_best, 4), "\n")
