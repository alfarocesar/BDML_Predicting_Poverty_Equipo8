# 🧹 Datos Procesados

Este directorio contiene los datos limpios y transformados para su uso en los modelos.

## Contenido
- `train_clean.csv`: Versión preprocesada de `train_hogares.csv` y `train_personas.csv`.
- `test_clean.csv`: Versión preprocesada de `test_hogares.csv` y `test_personas.csv`.

## Notas
- Estos archivos se generan mediante el script `02_data_cleaning.R`.
- Cualquier cambio en el preprocesamiento debe realizarse en los scripts y no directamente en estos archivos.
- Se recomienda verificar la calidad de los datos antes de su uso en modelos.
