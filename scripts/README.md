# 📜 Scripts del Proyecto

Este directorio contiene los scripts en R utilizados para la limpieza, transformación y modelado de los datos.

## Contenido
- `00_preprocessing.R`: Preprocesamiento inicial de los datos antes de unir las bases de hogares y de personas
- `01_import_and_join.R`: Unir las bases de datos de hogares y de personas
- `02_exploratory_analysis.R`: Análisis exploratorio de los datos.
- `03_data_cleaning.R`: Limpieza y transformación de los datos.
- `04_class_imbalance.R`: 
- `05_logistic_regression.R`: 
- `06_elastic_net.R`: 
- `07_cart.R`: 
- `08_random_forest.R`: 
- `09_boosting.R`: 
- `10_naive_bayes.R`: 
- `11_model_comparison.R`: Evaluación de los modelos.
- `12_generate_submission.R`: Generación del archivo de predicciones para Kaggle.

## Requisitos
Para ejecutar estos scripts, instala las dependencias con:

```r
install.packages("pacman")
pacman::p_load(tidyverse, caret, glmnet, Metrics)

## Script 00_preprocessing.R

Este script realiza un análisis preliminar de las bases de datos de hogares y personas para la predicción de pobreza en Colombia.

### Funcionalidad:

- **Identificación de variables comunes** entre conjuntos de entrenamiento y prueba
- **Verificación de la variable objetivo "Pobre"** y su coincidencia con la definición del DANE
- **Análisis de variables discretas numéricas** con especial atención a los valores NA
- **Análisis comparativo de missing values** entre tres poblaciones:
  - Conjunto completo de personas
  - Personas en edad de trabajar (Pet = 1)
  - Personas mayores de 18 años

### Inputs:
- `train_hogares.csv`, `test_hogares.csv`: Datos de hogares
- `train_personas.csv`, `test_personas.csv`: Datos de personas
- `tipos_datos_personas.csv`: Diccionario con clasificación de variables

### Outputs:
- `vars_comunes_*.csv`: Variables comunes entre train y test
- `distribucion_vars_discretas.csv`: Análisis de variables discretas
- `missing_*.csv`: Análisis de valores faltantes
- `descriptiva_*.csv`: Estadísticas descriptivas por grupos poblacionales

Este script es fundamental para informar la posterior selección de variables y estrategias de feature engineering para el modelo predictivo.