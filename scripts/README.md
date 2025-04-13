# Predicción de Pobreza en Colombia (MESE - DANE)

Este repositorio contiene el desarrollo completo del proyecto de predicción de pobreza a nivel de hogar en Colombia, en el marco del curso de **Big Data y Machine Learning**. Se utilizan datos proporcionados por el **DANE** y la **Misión MESE**, procesados y modelados en R.

---

## 📂 Estructura del Repositorio
├── document/ # Reportes finales, presentaciones o evidencia escrita 
├── scripts/ # Scripts en R con cada etapa del pipeline 
├── stores/ 
├── raw/ # Datos originales (CSV) 
│── processed/ # Datos procesados y predicciones exportadas 
├── views/ │ 


---

## 📑 Scripts del Proyecto

### Preprocesamiento y Análisis Exploratorio
- `00_preprocessing.R`: Análisis inicial de variables, estructuras y valores faltantes.
- `01_import_and_join.R`: Limpieza, eliminación de variables, imputación y unión hogar-persona.
- `02_exploratory_analysis.R`: EDA completo: visualizaciones, correlaciones, outliers, chi-cuadrado.
- `03_data_cleaning.R`: Transformaciones finales y clasificación de variables predictoras.
- `04_class_imbalance.R`: Análisis de desbalance de clases y estrategias de tratamiento.

### Modelado Predictivo
- `05_logistic_regression.R`: Modelo logit base con validación y F1-score.
- `06_elastic_net.R`: Regularización con Elastic Net (`glmnet`) y tuning.
- `07_cart.R`: Árbol de decisión con validación cruzada.
- `08_random_forest.R`: Modelo de Random Forest + extracción de importancia de variables.
- `09_boosting.R`: Modelos Boosting (XGBoost / LightGBM).
- `10_naive_bayes.R`: Modelo de Naive Bayes.
- `11_model_comparison.R`: Comparación de métricas: Accuracy, AUC y F1-score.

### Predicción y Entrega
- `12_generate_submission.R`: Exportación del archivo final para Kaggle (`submission.csv`).

---

## Requisitos

Para ejecutar los scripts, instalar las siguientes dependencias:

install.packages("pacman")
pacman::p_load(
  tidyverse, caret, glmnet, Metrics, yardstick,
  skimr, visdat, xtable, randomForest, rpart,
  GGally, corrplot
)


## Evaluación del Proyecto
Métrica principal: F1-score

Validación: Cross-validation estratificada (5-fold)

F1-score Máximo Alcanzado: 0.5676

Ranking de Variables: Basado en importancia (Random Forest) y correlación con la variable Pobre

Predicción Final: Exportación de archivo .csv con columna binaria pobre (0 = No, 1 = Sí)

## Notas Importantes
La semilla utilizada para asegurar reproducibilidad en todo el proyecto es: 1051

Los datos de entrada están organizados en stores/raw/ y las salidas en stores/processed/

Todos los resultados, tablas y figuras están guardados en la carpeta views/

## Referencias
Departamento Administrativo Nacional de Estadística (DANE), GEIH.
Misión MESE - Colombia: Datos públicos para ciencia de datos social.
Buenas prácticas reproducibles para ciencia de datos en R.

## Créditos
Proyecto realizado por Equipo 8
Curso: Big Data y Machine Learning – 2025

Para sugerencias o comentarios, por favor abrir un issue en este repositorio.

## Instrucciones de Ejecución
Clona el repositorio:
git clone https://github.com/alfarocesar/BDML_Predicting_Poverty_Equipo8.git
