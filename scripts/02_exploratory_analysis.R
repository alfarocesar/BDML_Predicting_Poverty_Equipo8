################################################################################
# TÍTULO: 02_exploratory_analysis.R                                           #
# PROYECTO: Predicción de Pobreza en Colombia                                 #
# DESCRIPCIÓN: Análisis exploratorio de datos para la toma de decisiones      #
#              sobre el preprocesamiento y modelamiento                       #
# FECHA: 30 de marzo de 2025                                                  #
################################################################################

# Configurar directorio de trabajo automáticamente
if (!require(rstudioapi)) install.packages("rstudioapi")
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# Subir un nivel directorio para acceder a la estructura principal del proyecto
setwd("../")

# Cargar librerías necesarias
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,   # Manipulación de datos y visualización
  skimr,       # Resumen de datos
  knitr,       # Para generar tablas
  stargazer    # Tablas estadísticas en formato LaTeX
)

# Fijar semilla para reproducibilidad
set.seed(123)

################################################################################
# 1. CARGA DE DATOS UNIDOS                                                     #
################################################################################

cat("Cargando datos unidos...\n")

# Cargar datos de entrenamiento y prueba unidos en el script anterior
train_data <- read.csv("stores/processed/train_joined.csv", stringsAsFactors = FALSE)
test_data <- read.csv("stores/processed/test_joined.csv", stringsAsFactors = FALSE)

# Asegurar que id sea tratado como character (por si acaso)
train_data$id <- as.character(train_data$id)
test_data$id <- as.character(test_data$id)

# Verificar dimensiones
cat("Dimensiones del conjunto de entrenamiento:", dim(train_data), "\n")
cat("Dimensiones del conjunto de prueba:", dim(test_data), "\n")

################################################################################
# 2. IDENTIFICACIÓN Y CLASIFICACIÓN DE VARIABLES                              #
################################################################################

cat("Identificando y clasificando variables...\n")

# Crear dataframe con clasificación de variables
variables_info <- data.frame(
  variable = names(train_data),
  tipo = sapply(train_data, class),
  # Identificar variables de ID (ajustar según el conjunto de datos real)
  es_id = names(train_data) %in% c("id", "directorio", "secuencia_p", "orden"),
  stringsAsFactors = FALSE
)

# Guardar esta información
write.csv(variables_info, "views/tables/variables_clasificacion.csv", row.names = FALSE)

# Mostrar las primeras filas
cat("Clasificación de variables (primeras filas):\n")
print(head(variables_info))

################################################################################
# 3. ANÁLISIS DESCRIPTIVO GENERAL                                              #
################################################################################

cat("Realizando análisis descriptivo general...\n")

# Resumen estadístico completo del conjunto de entrenamiento
train_summary <- skim(train_data)

# Exportar resumen estadístico a CSV
train_summary_export <- as.data.frame(train_summary)
write.csv(train_summary_export, "views/tables/train_summary_statistics.csv", row.names = FALSE)

# Crear tabla con estadísticas descriptivas para variables numéricas
# Excluir variables no numéricas y la id
numeric_vars <- train_data %>% 
  select(-id) %>%  # Excluir id que es identificador alfanumérico
  select_if(is.numeric)  # Seleccionar solo variables numéricas

# Generar estadísticas descriptivas usando stargazer
stat_desc <- stargazer(numeric_vars, 
                       type = "text", 
                       title = "Estadísticas Descriptivas de Variables Numéricas",
                       digits = 2,
                       out = "views/tables/descriptive_statistics.tex")

# Mostrar estadísticas de variables numéricas en consola
print(stat_desc)

################################################################################
# 4. ANÁLISIS DE LA DISTRIBUCIÓN DE POBREZA (VARIABLE OBJETIVO)                #
################################################################################

cat("Analizando distribución de la variable objetivo (pobreza)...\n")

# Calcular proporción de hogares pobres y no pobres
poverty_distribution <- train_data %>%
  count(Pobre) %>%
  mutate(proportion = n / sum(n) * 100)

# Guardar datos de distribución
write.csv(poverty_distribution, "views/tables/poverty_distribution.csv", row.names = FALSE)

# Mostrar desequilibrio de clases en consola
cat("Distribución de la variable objetivo:\n")
print(poverty_distribution)

# Calcular ratio de desequilibrio
if (all(c(0,1) %in% poverty_distribution$Pobre)) {
  imbalance_ratio <- poverty_distribution$n[poverty_distribution$Pobre == 0] / 
    poverty_distribution$n[poverty_distribution$Pobre == 1]
  cat("Ratio de desequilibrio (No Pobres / Pobres):", imbalance_ratio, "\n")
}

################################################################################
# 5. ANÁLISIS DE VALORES FALTANTES                                             #
################################################################################

cat("Analizando valores faltantes...\n")

# Calcular cantidad y porcentaje de valores faltantes por variable
missing_analysis <- data.frame(
  variable = names(train_data),
  n_missing = colSums(is.na(train_data)),
  pct_missing = round(colSums(is.na(train_data)) / nrow(train_data) * 100, 2)
) %>%
  arrange(desc(pct_missing))

# Guardar análisis de valores faltantes
write.csv(missing_analysis, "views/tables/missing_values_analysis.csv", row.names = FALSE)

# Mostrar variables con valores faltantes
cat("Variables con valores faltantes (ordenadas por % de NA):\n")
print(missing_analysis[missing_analysis$pct_missing > 0, ])

################################################################################
# 6. ANÁLISIS DE VARIABLES CATEGÓRICAS                                         #
################################################################################

cat("Analizando variables categóricas...\n")

# Identificar variables categóricas (factor y character)
cat_vars <- names(train_data)[sapply(train_data, function(x) is.factor(x) || is.character(x))]
cat_vars <- cat_vars[cat_vars != "id"]  # Excluir id

# Analizar cada variable categórica
cat_summary <- data.frame()

for (var in cat_vars) {
  # Verificar si la variable existe y no es completamente NA
  if (var %in% names(train_data) && sum(!is.na(train_data[[var]])) > 0) {
    # Crear tabla de frecuencias
    freq_table <- table(train_data[[var]], useNA = "always")
    
    # Obtener categoría más frecuente
    max_cat_index <- which.max(freq_table[-length(freq_table)]) # Excluir NA
    max_cat <- names(freq_table)[max_cat_index]
    max_freq <- freq_table[max_cat_index]
    max_pct <- max_freq / sum(freq_table) * 100
    
    # Contar categorías únicas (excluyendo NA)
    n_cats <- sum(!is.na(freq_table[-length(freq_table)]))
    
    # Añadir al resumen
    cat_summary <- rbind(cat_summary, data.frame(
      variable = var,
      categorias_unicas = n_cats,
      categoria_mas_frecuente = max_cat,
      frecuencia_max_cat = max_freq,
      porcentaje_max_cat = round(max_pct, 2),
      baja_variabilidad = max_pct > 95  # TRUE si una categoría representa >95% de datos
    ))
  }
}

# Guardar resumen de variables categóricas
write.csv(cat_summary, "views/tables/categorical_variables_summary.csv", row.names = FALSE)

# Mostrar resumen
cat("Resumen de variables categóricas:\n")
print(cat_summary)

################################################################################
# 7. ANÁLISIS DE VARIABLES NUMÉRICAS                                          #
################################################################################

cat("Analizando variables numéricas...\n")

# Seleccionar variables numéricas (excluyendo id)
num_vars <- names(train_data)[sapply(train_data, is.numeric)]
num_vars <- num_vars[!num_vars %in% c("id")]

# Analizar cada variable numérica
num_summary <- data.frame()

for (var in num_vars) {
  if (var %in% names(train_data) && sum(!is.na(train_data[[var]])) > 0) {
    # Datos sin NA
    values <- train_data[[var]][!is.na(train_data[[var]])]
    
    # Calcular estadísticas básicas
    mean_val <- mean(values)
    sd_val <- sd(values)
    cv <- (sd_val / mean_val) * 100  # Coeficiente de variación en porcentaje
    
    # Añadir al resumen
    num_summary <- rbind(num_summary, data.frame(
      variable = var,
      media = mean_val,
      sd = sd_val,
      cv_pct = round(cv, 2),              # Coeficiente de variación
      min = min(values),
      q1 = quantile(values, 0.25),
      mediana = median(values),
      q3 = quantile(values, 0.75),
      max = max(values),
      baja_variabilidad = cv < 5          # TRUE si CV < 5%
    ))
  }
}

# Guardar resumen de variables numéricas
write.csv(num_summary, "views/tables/numeric_variables_summary.csv", row.names = FALSE)

# Mostrar variables con baja variabilidad
cat("Variables numéricas con baja variabilidad (CV < 5%):\n")
print(num_summary[num_summary$baja_variabilidad, c("variable", "cv_pct")])

################################################################################
# 8. ANÁLISIS DE OUTLIERS (MÉTODO A: IQR)                                     #
################################################################################

cat("Analizando outliers usando método IQR...\n")

# Función para detectar outliers usando método IQR
analyze_outliers_iqr <- function(data, var_name) {
  # Eliminar NA
  var_data <- data[[var_name]][!is.na(data[[var_name]])]
  
  # Calcular cuartiles y rango intercuartílico
  q1 <- quantile(var_data, 0.25)
  q3 <- quantile(var_data, 0.75)
  iqr <- q3 - q1
  
  # Definir límites para outliers
  lower_bound <- q1 - 1.5 * iqr
  upper_bound <- q3 + 1.5 * iqr
  
  # Identificar outliers
  outliers <- var_data[var_data < lower_bound | var_data > upper_bound]
  
  # Calcular estadísticas
  result <- data.frame(
    variable = var_name,
    n_total = length(var_data),
    n_outliers = length(outliers),
    pct_outliers = round(length(outliers) / length(var_data) * 100, 2),
    lower_bound = lower_bound,
    upper_bound = upper_bound,
    min_value = min(var_data),
    max_value = max(var_data)
  )
  
  return(result)
}

# Aplicar análisis de outliers a variables numéricas importantes
# Excluir variable objetivo y variables binarias/id
outlier_vars <- setdiff(num_vars, c("Pobre", "id"))

# Aplicar función a cada variable
outlier_analysis <- do.call(rbind, lapply(outlier_vars, function(var) {
  analyze_outliers_iqr(train_data, var)
}))

# Guardar análisis de outliers
write.csv(outlier_analysis, "views/tables/outlier_analysis.csv", row.names = FALSE)

# Mostrar variables con más outliers
cat("Variables con mayor porcentaje de outliers:\n")
print(head(outlier_analysis[order(-outlier_analysis$pct_outliers), ]))

################################################################################
# 9. ANÁLISIS DE CORRELACIÓN ENTRE VARIABLES                                   #
################################################################################

cat("Analizando correlaciones entre variables...\n")

# Seleccionar variables numéricas para análisis de correlación
corr_vars <- train_data %>%
  select_if(is.numeric)

# Si existe una columna id y es numérica, la eliminamos
if ("id" %in% names(corr_vars)) {
  corr_vars <- corr_vars %>% select(-id)
}

# Eliminar columnas con desviación estándar cero o muy cercana a cero
# Calculamos las desviaciones estándar de cada columna
std_devs <- sapply(corr_vars, function(x) sd(x, na.rm = TRUE))
zero_sd_cols <- std_devs < 1e-10  # Umbral muy pequeño para evitar problemas numéricos

if (any(zero_sd_cols)) {
  cat("Las siguientes variables numéricas tienen desviación estándar (casi) cero y serán excluidas del análisis de correlación:\n")
  print(names(corr_vars)[zero_sd_cols])
  corr_vars <- corr_vars[, !zero_sd_cols, drop = FALSE]
}

# Verificar que todavía hay al menos dos variables para calcular correlaciones
if (ncol(corr_vars) >= 2) {
  # Calcular matriz de correlación con manejo de errores
  tryCatch({
    correlation_matrix <- cor(corr_vars, use = "pairwise.complete.obs")
    
    # Guardar matriz de correlación
    write.csv(correlation_matrix, "views/tables/correlation_matrix.csv", row.names = TRUE)
    
    # Identificar pares de variables altamente correlacionadas (|r| > 0.7)
    high_corr_pairs <- data.frame(
      variable1 = character(),
      variable2 = character(),
      correlacion = numeric(),
      stringsAsFactors = FALSE
    )
    
    for (i in 1:(ncol(correlation_matrix)-1)) {
      for (j in (i+1):ncol(correlation_matrix)) {
        corr_value <- correlation_matrix[i, j]
        # Verificar que corr_value no es NA antes de comparar
        if (!is.na(corr_value) && abs(corr_value) > 0.7) {
          high_corr_pairs <- rbind(high_corr_pairs, data.frame(
            variable1 = rownames(correlation_matrix)[i],
            variable2 = colnames(correlation_matrix)[j],
            correlacion = corr_value,
            stringsAsFactors = FALSE
          ))
        }
      }
    }
    
    # Ordenar por valor absoluto de correlación
    if (nrow(high_corr_pairs) > 0) {
      high_corr_pairs <- high_corr_pairs[order(-abs(high_corr_pairs$correlacion)), ]
      
      # Guardar pares de alta correlación
      write.csv(high_corr_pairs, "views/tables/variables_altamente_correlacionadas.csv", row.names = FALSE)
      
      # Mostrar pares de variables altamente correlacionadas
      cat("Pares de variables altamente correlacionadas (|r| > 0.7):\n")
      print(high_corr_pairs)
    } else {
      cat("No se encontraron pares de variables con correlación |r| > 0.7\n")
      # Crear un archivo vacío para mantener consistencia
      write.csv(high_corr_pairs, "views/tables/variables_altamente_correlacionadas.csv", row.names = FALSE)
    }
  }, warning = function(w) {
    cat("Advertencia en el cálculo de correlaciones:", conditionMessage(w), "\n")
    # Continuar con el flujo normal después de la advertencia
  }, error = function(e) {
    cat("Error en el cálculo de correlaciones:", conditionMessage(e), "\n")
    cat("Se creará una matriz de correlación vacía para mantener la consistencia del flujo.\n")
    
    # Crear un dataframe vacío para mantener consistencia
    high_corr_pairs <- data.frame(
      variable1 = character(),
      variable2 = character(),
      correlacion = numeric(),
      stringsAsFactors = FALSE
    )
    write.csv(high_corr_pairs, "views/tables/variables_altamente_correlacionadas.csv", row.names = FALSE)
  })
} else {
  cat("No hay suficientes variables numéricas con varianza distinta de cero para calcular correlaciones.\n")
  # Crear un dataframe vacío para mantener consistencia
  high_corr_pairs <- data.frame(
    variable1 = character(),
    variable2 = character(),
    correlacion = numeric(),
    stringsAsFactors = FALSE
  )
  write.csv(high_corr_pairs, "views/tables/variables_altamente_correlacionadas.csv", row.names = FALSE)
}

################################################################################
# 10. ANÁLISIS DE RELACIÓN CON LA VARIABLE OBJETIVO                            #
################################################################################

cat("Analizando relaciones con la variable objetivo (pobreza)...\n")

# 1. Correlación puntual para variables numéricas
numerical_target_corr <- data.frame()

for (var in setdiff(num_vars, "Pobre")) {
  if (sum(!is.na(train_data[[var]])) > 0) {
    corr_value <- cor(train_data[[var]], train_data$Pobre, 
                      use = "pairwise.complete.obs")
    
    numerical_target_corr <- rbind(numerical_target_corr, data.frame(
      variable = var,
      correlacion_con_pobreza = corr_value
    ))
  }
}

# Ordenar por valor absoluto de correlación
numerical_target_corr <- numerical_target_corr[order(-abs(numerical_target_corr$correlacion_con_pobreza)), ]

# Guardar correlaciones con la variable objetivo
write.csv(numerical_target_corr, "views/tables/correlacion_variables_pobreza.csv", row.names = FALSE)

# Mostrar variables numéricas más correlacionadas con pobreza
cat("Variables numéricas más correlacionadas con pobreza:\n")
print(head(numerical_target_corr, 10))

# 2. Análisis para variables categóricas clave
cat_target_analysis <- data.frame()

for (var in cat_vars) {
  # Verificar si la variable existe y tiene suficientes datos
  if (var %in% names(train_data) && sum(!is.na(train_data[[var]])) > nrow(train_data) * 0.5) {
    # Calcular tasa de pobreza por categoría
    poverty_by_cat <- train_data %>%
      group_by_at(var) %>%
      summarise(
        n_total = n(),
        n_pobres = sum(Pobre, na.rm = TRUE),
        tasa_pobreza = round(mean(Pobre, na.rm = TRUE) * 100, 2),
        .groups = "drop"
      ) %>%
      filter(n_total >= 30)  # Solo categorías con suficientes observaciones
    
    # Si hay categorías con suficientes datos, calcular variabilidad de tasas
    if (nrow(poverty_by_cat) > 1) {
      var_rates <- var(poverty_by_cat$tasa_pobreza)
      
      cat_target_analysis <- rbind(cat_target_analysis, data.frame(
        variable = var,
        categorias_analizadas = nrow(poverty_by_cat),
        variabilidad_tasas = round(var_rates, 2),
        min_tasa = min(poverty_by_cat$tasa_pobreza),
        max_tasa = max(poverty_by_cat$tasa_pobreza),
        rango_tasas = max(poverty_by_cat$tasa_pobreza) - min(poverty_by_cat$tasa_pobreza)
      ))
    }
  }
}

# Ordenar por rango de tasas de pobreza
if (nrow(cat_target_analysis) > 0) {
  cat_target_analysis <- cat_target_analysis[order(-cat_target_analysis$rango_tasas), ]
  
  # Guardar análisis
  write.csv(cat_target_analysis, "views/tables/analisis_categoricas_pobreza.csv", row.names = FALSE)
  
  # Mostrar variables categóricas más discriminativas para pobreza
  cat("Variables categóricas con mayor variabilidad en tasas de pobreza:\n")
  print(cat_target_analysis)
}

################################################################################
# 11. RESUMEN DE MÉTRICAS PARA TOMA DE DECISIONES                              #
################################################################################

cat("Generando resumen para toma de decisiones...\n")

# 1. Variables candidatas a eliminar
variables_eliminar <- data.frame(
  variable = character(),
  motivo = character(),
  stringsAsFactors = FALSE
)

# Variables de identificación
id_vars <- variables_info$variable[variables_info$es_id]
if (length(id_vars) > 0) {
  variables_eliminar <- rbind(variables_eliminar, data.frame(
    variable = id_vars,
    motivo = "Variable de identificación"
  ))
}

# Variables con muchos valores faltantes (>30%)
high_missing_vars <- missing_analysis$variable[missing_analysis$pct_missing > 30]
if (length(high_missing_vars) > 0) {
  variables_eliminar <- rbind(variables_eliminar, data.frame(
    variable = high_missing_vars,
    motivo = "Más del 30% de valores faltantes"
  ))
}

# Variables con baja variabilidad
low_var_cat <- cat_summary$variable[cat_summary$baja_variabilidad]
low_var_num <- num_summary$variable[num_summary$baja_variabilidad]
low_var_vars <- c(low_var_cat, low_var_num)
if (length(low_var_vars) > 0) {
  variables_eliminar <- rbind(variables_eliminar, data.frame(
    variable = low_var_vars,
    motivo = "Baja variabilidad"
  ))
}

# 2. Variables potencialmente redundantes
variables_redundantes <- data.frame()
if (nrow(high_corr_pairs) > 0) {
  # Para cada par altamente correlacionado, proponer eliminar la menos correlacionada con el objetivo
  for (i in 1:nrow(high_corr_pairs)) {
    var1 <- high_corr_pairs$variable1[i]
    var2 <- high_corr_pairs$variable2[i]
    
    # Buscar correlación con pobreza para ambas variables
    corr1 <- numerical_target_corr$correlacion_con_pobreza[numerical_target_corr$variable == var1]
    corr2 <- numerical_target_corr$correlacion_con_pobreza[numerical_target_corr$variable == var2]
    
    # Si ambas variables tienen correlación calculada
    if (length(corr1) > 0 && length(corr2) > 0) {
      # Proponer eliminar la de menor correlación absoluta con pobreza
      if (abs(corr1) > abs(corr2)) {
        eliminar <- var2
        mantener <- var1
      } else {
        eliminar <- var1
        mantener <- var2
      }
      
      variables_redundantes <- rbind(variables_redundantes, data.frame(
        par_correlacionado = paste(var1, "y", var2),
        correlacion = high_corr_pairs$correlacion[i],
        propuesta_eliminar = eliminar,
        propuesta_mantener = mantener,
        justificacion = paste("Mayor correlación con objetivo:", abs(max(corr1, corr2)), "vs", abs(min(corr1, corr2)))
      ))
    }
  }
}

# 3. Variables potencialmente importantes (top 10 correlaciones con objetivo)
variables_importantes <- head(numerical_target_corr, 10)

# 4. Variables categóricas candidatas para one-hot encoding (pocas categorías)
variables_one_hot <- cat_summary %>%
  filter(categorias_unicas <= 10) %>%
  select(variable, categorias_unicas)

# 5. Variables con outliers significativos
variables_outliers <- outlier_analysis %>%
  filter(pct_outliers > 5) %>%
  select(variable, pct_outliers, min_value, max_value) %>%
  arrange(desc(pct_outliers))

# Guardar resúmenes para toma de decisiones
write.csv(variables_eliminar, "views/tables/variables_candidatas_eliminar.csv", row.names = FALSE)
write.csv(variables_redundantes, "views/tables/variables_redundantes.csv", row.names = FALSE)
write.csv(variables_importantes, "views/tables/variables_importantes.csv", row.names = FALSE)
write.csv(variables_one_hot, "views/tables/variables_candidatas_one_hot.csv", row.names = FALSE)
write.csv(variables_outliers, "views/tables/variables_con_outliers.csv", row.names = FALSE)

cat("Resúmenes para toma de decisiones guardados en carpeta views/tables/\n")

################################################################################
# 12. RESUMEN DE HALLAZGOS Y RECOMENDACIONES                                   #
################################################################################

cat("Generando resumen de hallazgos y recomendaciones...\n")

# Crear un archivo de texto con los hallazgos principales y recomendaciones
findings <- c(
  "# RESUMEN DE HALLAZGOS DEL ANÁLISIS EXPLORATORIO",
  "",
  "## DISTRIBUCIÓN DE LA VARIABLE OBJETIVO:",
  paste0("- El ", round(poverty_distribution$proportion[poverty_distribution$Pobre == 1], 1), 
         "% de los hogares en la muestra son pobres."),
  paste0("- Existe un desequilibrio de clases con un ratio de ", round(imbalance_ratio, 2), 
         " (No Pobres / Pobres)."),
  "",
  "## VALORES FALTANTES:",
  paste0("- Se identificaron ", sum(missing_analysis$pct_missing > 0), " variables con valores faltantes."),
  paste0("- Variables con alta proporción de valores faltantes (>30%): ", 
         paste(high_missing_vars, collapse=", ")),
  "",
  "## OUTLIERS:",
  paste0("- Se identificaron ", nrow(variables_outliers), " variables con outliers significativos (>5%)."),
  paste0("- Variables con mayor porcentaje de outliers: ", 
         paste(head(variables_outliers$variable, 3), collapse=", ")),
  "",
  "## VARIABLES REDUNDANTES:",
  paste0("- Se identificaron ", nrow(high_corr_pairs), " pares de variables altamente correlacionadas (|r| > 0.7)."),
  "",
  "## VARIABLES MÁS CORRELACIONADAS CON POBREZA:",
  paste0("- ", variables_importantes$variable[1], " (", round(variables_importantes$correlacion_con_pobreza[1], 3), ")"),
  paste0("- ", variables_importantes$variable[2], " (", round(variables_importantes$correlacion_con_pobreza[2], 3), ")"),
  paste0("- ", variables_importantes$variable[3], " (", round(variables_importantes$correlacion_con_pobreza[3], 3), ")"),
  "",
  "## RECOMENDACIONES PARA LIMPIEZA DE DATOS:",
  "1. Considerar eliminar variables con más del 30% de valores faltantes.",
  "2. Imputar valores faltantes en variables importantes usando estrategias adecuadas según el tipo de variable.",
  "3. Tratar outliers en variables con alto porcentaje de valores extremos, especialmente en variables de ingreso.",
  "4. Eliminar variables redundantes con alta correlación, manteniendo las más correlacionadas con la variable objetivo.",
  "5. Considerar transformaciones para variables con distribuciones sesgadas.",
  "6. Implementar estrategias para manejar el desbalance de clases en la fase de modelamiento.",
  "",
  "## VARIABLES CANDIDATAS PARA ONE-HOT ENCODING:",
  paste0("- Variables categóricas con pocas categorías (<=10): ", 
         paste(variables_one_hot$variable, collapse=", ")),
  "",
  "Este resumen servirá como guía para el procesamiento y limpieza de datos en el siguiente script."
)

# Guardar hallazgos en un archivo de texto
writeLines(findings, "views/tables/exploratory_analysis_findings.md")

cat("Resumen de hallazgos guardado en 'views/tables/exploratory_analysis_findings.md'.\n")

################################################################################
#                            FINALIZACIÓN DEL SCRIPT                           #
################################################################################

cat("\n======================================================================\n")
cat("ANÁLISIS EXPLORATORIO COMPLETADO\n")
cat("======================================================================\n")
cat("Resultados guardados en:\n")
cat("  - views/tables/ (tablas y resúmenes estadísticos)\n")
cat("\nHallazgos principales guardados en: views/tables/exploratory_analysis_findings.md\n")
cat("\nUtilizar estos resultados para guiar la limpieza de datos y selección de variables\n")
cat("en el siguiente script (03_data_cleaning.R).\n")
cat("======================================================================\n")