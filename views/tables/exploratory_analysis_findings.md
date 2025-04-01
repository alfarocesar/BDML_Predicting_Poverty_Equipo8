# RESUMEN DE HALLAZGOS DEL ANÁLISIS EXPLORATORIO

## DISTRIBUCIÓN DE LA VARIABLE OBJETIVO:
- El 20% de los hogares en la muestra son pobres.
- Existe un desequilibrio de clases con un ratio de 4 (No Pobres / Pobres).

## VALORES FALTANTES:
- Se identificaron 2 variables con valores faltantes.
- Variables con alta proporción de valores faltantes (>30%): P5140, P5130

## OUTLIERS:
- Se identificaron 6 variables con outliers significativos (>5%).
- Variables con mayor porcentaje de outliers: Lp, P5000, Ingpcug

## VARIABLES REDUNDANTES:
- Se identificaron 11 pares de variables altamente correlacionadas (|r| > 0.7).

## VARIABLES MÁS CORRELACIONADAS CON POBREZA:
- n_menores (0.364)
- Ingtotugarr (-0.303)
- Ingpcug (-0.282)

## RECOMENDACIONES PARA LIMPIEZA DE DATOS:
1. Considerar eliminar variables con más del 30% de valores faltantes.
2. Imputar valores faltantes en variables importantes usando estrategias adecuadas según el tipo de variable.
3. Tratar outliers en variables con alto porcentaje de valores extremos, especialmente en variables de ingreso.
4. Eliminar variables redundantes con alta correlación, manteniendo las más correlacionadas con la variable objetivo.
5. Considerar transformaciones para variables con distribuciones sesgadas.
6. Implementar estrategias para manejar el desbalance de clases en la fase de modelamiento.

## VARIABLES CANDIDATAS PARA ONE-HOT ENCODING:
- Variables categóricas con pocas categorías (<=10): 

Este resumen servirá como guía para el procesamiento y limpieza de datos en el siguiente script.
