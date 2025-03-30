# 📜 Scripts del Proyecto

Este directorio contiene los scripts en R utilizados para la limpieza, transformación y modelado de los datos.

## Contenido
- `01_exploratory_analysis.R`: Análisis exploratorio de los datos.
- `02_data_cleaning.R`: Limpieza y transformación de los datos.
- `03_feature_engineering.R`: Creación de nuevas variables.
- `04_model_training.R`: Entrenamiento de modelos de clasificación.
- `05_model_evaluation.R`: Evaluación de los modelos.
- `06_submission_generation.R`: Generación del archivo de predicciones para Kaggle.

## Requisitos
Para ejecutar estos scripts, instala las dependencias con:

```r
install.packages("pacman")
pacman::p_load(tidyverse, caret, glmnet, Metrics)
