################################################################################
# TÍTULO: 02_exploratory_analysis.R                                           #
# PROYECTO: Predicción de Pobreza en Colombia - Equipo 8                      #
# DESCRIPCIÓN: Análisis exploratorio de datos para la toma de decisiones      #
#              sobre el preprocesamiento y modelamiento                       #
# FECHA: 8 de abril de 2025                                                   #
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
  corrplot,    # Para visualización de correlaciones
  ggplot2,     # Visualizaciones
  knitr,       # Para generar tablas
  xtable,      # Para exportar tablas a LaTeX
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

# Crear directorios para guardar resultados si no existen
if (!dir.exists("views/tables")) {
  dir.create("views/tables", recursive = TRUE)
}
if (!dir.exists("views/figures")) {
  dir.create("views/figures", recursive = TRUE)
}

################################################################################
# 2. ANÁLISIS DE LA DISTRIBUCIÓN DE POBREZA (VARIABLE OBJETIVO)                #
################################################################################

cat("Analizando distribución de la variable objetivo (pobreza)...\n")

# Convertir Pobre a factor para mejor manejo
train_data$Pobre <- factor(train_data$Pobre, levels = c(0, 1), labels = c("No Pobre", "Pobre"))

# Calcular proporción de hogares pobres y no pobres
poverty_distribution <- train_data %>%
  count(Pobre) %>%
  mutate(proportion = n / sum(n) * 100)

# Guardar en formato LaTeX
print(xtable(poverty_distribution, 
             caption = "Distribución de la Variable Objetivo (Pobreza)", 
             label = "tab:poverty_distribution"),
      file = "views/tables/poverty_distribution.tex",
      include.rownames = FALSE,
      floating = TRUE,
      latex.environments = "center",
      booktabs = TRUE)

# Mostrar desequilibrio de clases en consola
cat("Distribución de la variable objetivo:\n")
print(poverty_distribution)

# Crear gráfico de distribución de pobreza
png("views/figures/poverty_distribution.png", width = 800, height = 600)
ggplot(poverty_distribution, aes(x = Pobre, y = n, fill = Pobre)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(proportion, 1), "%")), vjust = -0.5) +
  theme_minimal() +
  labs(title = "Distribución de Pobreza",
       x = "",
       y = "Número de Hogares") +
  scale_fill_manual(values = c("No Pobre" = "steelblue", "Pobre" = "coral"))
dev.off()

# Calcular ratio de desequilibrio para informar estrategias de balanceo
imbalance_ratio <- poverty_distribution$n[poverty_distribution$Pobre == "No Pobre"] / 
  poverty_distribution$n[poverty_distribution$Pobre == "Pobre"]
cat("Ratio de desequilibrio (No Pobres / Pobres):", round(imbalance_ratio, 2), "\n")

################################################################################
# 3. ANÁLISIS DESCRIPTIVO DE VARIABLES CLAVE                                   #
################################################################################

cat("Realizando análisis descriptivo de variables clave...\n")

# Seleccionar variables numéricas (excluyendo id)
numeric_vars <- train_data %>% 
  select(-id) %>%  # Excluir id que es identificador alfanumérico
  select_if(is.numeric)  # Seleccionar solo variables numéricas

# Para análisis descriptivo, volvemos a convertir Pobre a numérico si existe en numeric_vars
if ("Pobre" %in% names(numeric_vars)) {
  numeric_vars$Pobre <- as.numeric(as.character(numeric_vars$Pobre))
}

# Generar estadísticas descriptivas usando stargazer directamente a LaTeX
stargazer(numeric_vars, 
          title = "Estadísticas Descriptivas de Variables Numéricas",
          digits = 2,
          label = "tab:descriptive_stats",
          out = "views/tables/descriptive_statistics.tex")

# Mostrar estadísticas de variables numéricas en consola (versión resumida)
cat("Estadísticas descriptivas (primeras 10 variables numéricas):\n")
print(summary(numeric_vars[, 1:min(10, ncol(numeric_vars))]))

################################################################################
# 4. ANÁLISIS DE VALORES FALTANTES                                             #
################################################################################

cat("Analizando valores faltantes...\n")

# Calcular cantidad y porcentaje de valores faltantes por variable
missing_analysis <- data.frame(
  variable = names(train_data),
  n_missing = colSums(is.na(train_data)),
  pct_missing = round(colSums(is.na(train_data)) / nrow(train_data) * 100, 2)
) %>%
  arrange(desc(pct_missing))

# Guardar análisis de valores faltantes en formato LaTeX
# Solo mostrar variables con valores faltantes
missing_table <- missing_analysis[missing_analysis$pct_missing > 0, ]
if(nrow(missing_table) > 0) {
  print(xtable(missing_table, 
               caption = "Variables con Valores Faltantes", 
               label = "tab:missing_values"),
        file = "views/tables/missing_values_analysis.tex",
        include.rownames = FALSE,
        floating = TRUE,
        latex.environments = "center",
        booktabs = TRUE)
  
  # Crear gráfico de barras con las 15 variables con más valores faltantes
  if(nrow(missing_table) > 0) {
    top_missing <- head(missing_table, 15)
    png("views/figures/missing_values.png", width = 1000, height = 600)
    ggplot(top_missing, aes(x = reorder(variable, pct_missing), y = pct_missing)) +
      geom_bar(stat = "identity", fill = "orangered") +
      coord_flip() +
      labs(title = "Top 15 Variables con Mayor Porcentaje de Valores Faltantes",
           x = "",
           y = "Porcentaje de Valores Faltantes") +
      theme_minimal()
    dev.off()
  }
}

# Mostrar variables con valores faltantes
cat("Variables con valores faltantes (ordenadas por % de NA):\n")
print(missing_analysis[missing_analysis$pct_missing > 0, ])

################################################################################
# 5. ANÁLISIS DE CORRELACIÓN ENTRE VARIABLES                                   #
################################################################################

cat("Analizando correlaciones entre variables...\n")

# Seleccionar variables numéricas para análisis de correlación
# Y convertir Pobre a numérico nuevamente si es necesario
corr_vars <- train_data %>%
  select_if(is.numeric)

# Eliminar columnas con desviación estándar cero o muy cercana a cero
# Y añadir manejo de errores para evitar problemas
std_devs <- sapply(corr_vars, function(x) sd(x, na.rm = TRUE))
zero_sd_cols <- std_devs < 1e-10  # Umbral muy pequeño para evitar problemas numéricos

if (any(zero_sd_cols)) {
  cat("Las siguientes variables numéricas tienen desviación estándar (casi) cero y serán excluidas del análisis de correlación:\n")
  print(names(corr_vars)[zero_sd_cols])
  corr_vars <- corr_vars[, !zero_sd_cols, drop = FALSE]
}

# Verificar que todavía hay al menos dos variables para calcular correlaciones
if (ncol(corr_vars) >= 2) {
  tryCatch({
    # Calcular matriz de correlación
    correlation_matrix <- cor(corr_vars, use = "pairwise.complete.obs")
    
    # Crear visualización de correlaciones
    png("views/figures/correlation_matrix.png", width = 1000, height = 1000)
    corrplot(correlation_matrix, method = "circle", type = "upper", 
             tl.cex = 0.7, tl.col = "black", diag = FALSE)
    dev.off()
    
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
      
      # Guardar pares de alta correlación en formato LaTeX
      print(xtable(high_corr_pairs, 
                   caption = "Variables Altamente Correlacionadas (|r| > 0.7)", 
                   label = "tab:high_correlation"),
            file = "views/tables/variables_altamente_correlacionadas.tex",
            include.rownames = FALSE,
            floating = TRUE,
            latex.environments = "center",
            booktabs = TRUE)
      
      # Mostrar pares de variables altamente correlacionadas
      cat("Pares de variables altamente correlacionadas (|r| > 0.7):\n")
      print(high_corr_pairs)
    } else {
      cat("No se encontraron pares de variables con correlación |r| > 0.7\n")
    }
  }, warning = function(w) {
    cat("Advertencia en el cálculo de correlaciones:", conditionMessage(w), "\n")
  }, error = function(e) {
    cat("Error en el cálculo de correlaciones:", conditionMessage(e), "\n")
    cat("No se pudo generar la matriz de correlación.\n")
  })
} else {
  cat("No hay suficientes variables numéricas con varianza distinta de cero para calcular correlaciones.\n")
}

################################################################################
# 6. ANÁLISIS DE RELACIÓN CON LA VARIABLE OBJETIVO                            #
################################################################################

cat("Analizando relaciones con la variable objetivo (pobreza)...\n")

# Convertir Pobre a numérico para cálculos de correlación
train_data_numeric <- train_data
if ("Pobre" %in% names(train_data_numeric)) {
  train_data_numeric$Pobre <- as.numeric(as.character(train_data_numeric$Pobre))
}

# Calcular correlación con la variable objetivo para variables numéricas
numerical_target_corr <- data.frame(
  variable = character(),
  correlacion_con_pobreza = numeric(),
  stringsAsFactors = FALSE
)

# Solo incluir variables numéricas que no sean la variable objetivo
num_vars <- names(train_data_numeric)[sapply(train_data_numeric, is.numeric)]
num_vars <- setdiff(num_vars, c("id", "Pobre"))

for (var in num_vars) {
  if (sum(!is.na(train_data_numeric[[var]])) > 0) {
    corr_value <- cor(train_data_numeric[[var]], train_data_numeric$Pobre, 
                      use = "pairwise.complete.obs")
    
    if (!is.na(corr_value)) {
      numerical_target_corr <- rbind(numerical_target_corr, data.frame(
        variable = var,
        correlacion_con_pobreza = corr_value
      ))
    }
  }
}

# Ordenar por valor absoluto de correlación
numerical_target_corr <- numerical_target_corr[order(-abs(numerical_target_corr$correlacion_con_pobreza)), ]

# Visualizar top 10 variables correlacionadas con pobreza
if (nrow(numerical_target_corr) > 0) {
  top_corr <- head(numerical_target_corr, 10)
  
  # Crear gráfico
  png("views/figures/top_poverty_correlations.png", width = 800, height = 600)
  ggplot(top_corr, aes(x = reorder(variable, abs(correlacion_con_pobreza)), 
                       y = correlacion_con_pobreza)) +
    geom_bar(stat = "identity", 
             aes(fill = correlacion_con_pobreza > 0)) +
    scale_fill_manual(values = c("TRUE" = "forestgreen", "FALSE" = "coral"),
                      labels = c("TRUE" = "Positiva", "FALSE" = "Negativa"),
                      name = "Correlación") +
    coord_flip() +
    labs(title = "Top 10 Variables Correlacionadas con Pobreza",
         x = "",
         y = "Correlación con Pobreza") +
    theme_minimal()
  dev.off()
  
  # Guardar correlaciones con la variable objetivo en formato LaTeX
  print(xtable(top_corr, 
               caption = "Top 10 Variables Numéricas Correlacionadas con Pobreza", 
               label = "tab:poverty_correlation"),
        file = "views/tables/top_correlacion_variables_pobreza.tex",
        include.rownames = FALSE,
        floating = TRUE,
        latex.environments = "center",
        booktabs = TRUE)
  
  # Mostrar variables numéricas más correlacionadas con pobreza
  cat("Top 10 Variables numéricas más correlacionadas con pobreza:\n")
  print(top_corr)
}

################################################################################
# 7. ANÁLISIS DE VARIABLES CATEGÓRICAS Y TASAS DE POBREZA                     #
################################################################################

cat("Analizando variables categóricas y su relación con pobreza...\n")

# Identificar variables categóricas para análisis
# En este dataset, la mayoría pueden ser variables de texto o factores
cat_vars <- names(train_data)[sapply(train_data, function(x) 
  is.character(x) || is.factor(x))]
cat_vars <- cat_vars[!cat_vars %in% c("id", "Pobre")]

# Si no hay variables categóricas en el formato esperado, considerar variables
# numéricas que podrían ser categóricas (valores enteros con pocos niveles)
if (length(cat_vars) == 0) {
  possible_cat_vars <- names(train_data)[sapply(train_data, function(x) 
    is.numeric(x) && length(unique(x)) <= 10)]
  possible_cat_vars <- possible_cat_vars[!possible_cat_vars %in% c("id", "Pobre")]
  
  if (length(possible_cat_vars) > 0) {
    cat("No se encontraron variables explícitamente categóricas, pero las siguientes variables numéricas podrían ser categóricas:\n")
    print(possible_cat_vars)
    cat_vars <- possible_cat_vars
  }
}

# Si hay variables categóricas para analizar
if (length(cat_vars) > 0) {
  # Analizar relación entre variables categóricas y tasas de pobreza
  cat_poverty_analysis <- data.frame()
  
  for (var in cat_vars) {
    # Solo analizar si la variable tiene variabilidad
    if (length(unique(train_data[[var]])) > 1) {
      # Calcular tasa de pobreza por categoría
      poverty_by_cat <- train_data %>%
        group_by(across(all_of(var))) %>%
        summarise(
          n_total = n(),
          n_pobres = sum(Pobre == "Pobre", na.rm = TRUE),
          tasa_pobreza = round(mean(Pobre == "Pobre", na.rm = TRUE) * 100, 2),
          .groups = "drop"
        ) %>%
        filter(n_total >= 20)  # Solo categorías con suficientes observaciones
      
      # Si hay al menos dos categorías con suficientes datos
      if (nrow(poverty_by_cat) >= 2) {
        var_range <- max(poverty_by_cat$tasa_pobreza) - min(poverty_by_cat$tasa_pobreza)
        
        cat_poverty_analysis <- rbind(cat_poverty_analysis, data.frame(
          variable = var,
          categorias = nrow(poverty_by_cat),
          min_tasa_pobreza = min(poverty_by_cat$tasa_pobreza),
          max_tasa_pobreza = max(poverty_by_cat$tasa_pobreza),
          rango_tasas = var_range
        ))
        
        # Si el rango de tasas es significativo, crear visualización
        if (var_range > 10 && nrow(poverty_by_cat) <= 10) {  # Solo gráficos para variables informativas
          # Crear gráfico para esta variable categórica
          png(paste0("views/figures/poverty_by_", var, ".png"), width = 800, height = 600)
          print(ggplot(poverty_by_cat, 
                       aes(x = reorder(get(var), -tasa_pobreza), y = tasa_pobreza, fill = tasa_pobreza)) +
                  geom_bar(stat = "identity") +
                  geom_text(aes(label = paste0(tasa_pobreza, "%")), vjust = -0.5) +
                  labs(title = paste("Tasa de Pobreza por", var),
                       x = var,
                       y = "Tasa de Pobreza (%)") +
                  theme_minimal() +
                  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
                  scale_fill_gradient(low = "lightblue", high = "darkred"))
          dev.off()
        }
      }
    }
  }
  
  # Ordenar por rango de tasas de pobreza (variables más discriminantes)
  if (nrow(cat_poverty_analysis) > 0) {
    cat_poverty_analysis <- cat_poverty_analysis[order(-cat_poverty_analysis$rango_tasas), ]
    
    # Guardar análisis en formato LaTeX
    print(xtable(cat_poverty_analysis, 
                 caption = "Relación entre Variables Categóricas y Tasa de Pobreza", 
                 label = "tab:cat_poverty"),
          file = "views/tables/analisis_categoricas_pobreza.tex",
          include.rownames = FALSE,
          floating = TRUE,
          latex.environments = "center",
          booktabs = TRUE)
    
    # Mostrar variables categóricas más discriminativas para pobreza
    cat("Variables categóricas con mayor variabilidad en tasas de pobreza:\n")
    print(cat_poverty_analysis)
  }
}

################################################################################
# 8. ANÁLISIS DE OUTLIERS                                                     #
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
    lower_bound = round(lower_bound, 2),
    upper_bound = round(upper_bound, 2),
    min_value = min(var_data),
    max_value = max(var_data)
  )
  
  return(result)
}

# Aplicar análisis de outliers a variables numéricas importantes
# Excluir variable objetivo, variables binarias y el id
outlier_vars <- num_vars[!num_vars %in% c("Pobre", "id")]

# Filtrar solo variables que tienen más de 10 valores únicos (probablemente continuas)
outlier_vars <- outlier_vars[sapply(outlier_vars, function(var) 
  length(unique(train_data[[var]])) > 10)]

# Si hay variables para analizar
if (length(outlier_vars) > 0) {
  # Aplicar función a cada variable
  outlier_analysis <- do.call(rbind, lapply(outlier_vars, function(var) {
    analyze_outliers_iqr(train_data, var)
  }))
  
  # Guardar análisis de outliers en formato LaTeX
  print(xtable(outlier_analysis, 
               caption = "Análisis de Outliers (Método IQR)", 
               label = "tab:outliers"),
        file = "views/tables/outlier_analysis.tex",
        include.rownames = FALSE,
        floating = TRUE,
        latex.environments = "center",
        booktabs = TRUE)
  
  # Mostrar variables con más outliers
  cat("Variables con mayor porcentaje de outliers:\n")
  print(head(outlier_analysis[order(-outlier_analysis$pct_outliers), ]))
  
  # Visualizar outliers para las 5 variables con más outliers
  top_outlier_vars <- head(outlier_analysis[order(-outlier_analysis$pct_outliers), ]$variable, 5)
  
  for (var in top_outlier_vars) {
    # Crear boxplot para visualizar outliers
    png(paste0("views/figures/outliers_", var, ".png"), width = 800, height = 600)
    print(ggplot(train_data, aes(y = get(var))) +
            geom_boxplot(fill = "skyblue") +
            labs(title = paste("Boxplot de", var, "con Outliers"),
                 y = var) +
            theme_minimal())
    dev.off()
  }
}

################################################################################
# 9. RECOMENDACIONES Y CONCLUSIONES                                           #
################################################################################

cat("Generando recomendaciones y conclusiones...\n")

# 1. Crear lista de variables candidatas a eliminar por distintos motivos
variables_eliminar <- data.frame(
  variable = character(),
  motivo = character(),
  stringsAsFactors = FALSE
)

# Variables con muchos valores faltantes (>30%)
high_missing_vars <- missing_analysis$variable[missing_analysis$pct_missing > 30]
if (length(high_missing_vars) > 0) {
  variables_eliminar <- rbind(variables_eliminar, data.frame(
    variable = high_missing_vars,
    motivo = "Más del 30% de valores faltantes"
  ))
}

# Variables con baja variabilidad (desviación estándar cerca de cero)
if (exists("std_devs")) {
  low_var_vars <- names(std_devs)[std_devs < 0.01]
  low_var_vars <- low_var_vars[!low_var_vars %in% c("id", "Pobre")]
  
  if (length(low_var_vars) > 0) {
    variables_eliminar <- rbind(variables_eliminar, data.frame(
      variable = low_var_vars,
      motivo = "Baja variabilidad"
    ))
  }
}

# Variables redundantes (altamente correlacionadas)
if (exists("high_corr_pairs") && nrow(high_corr_pairs) > 0) {
  # Para cada par, proponer eliminar una variable (la de menor correlación con objetivo)
  for (i in 1:nrow(high_corr_pairs)) {
    var1 <- high_corr_pairs$variable1[i]
    var2 <- high_corr_pairs$variable2[i]
    
    # Ver cual tiene mayor correlación con pobreza
    if (var1 %in% numerical_target_corr$variable && var2 %in% numerical_target_corr$variable) {
      corr1 <- numerical_target_corr$correlacion_con_pobreza[numerical_target_corr$variable == var1]
      corr2 <- numerical_target_corr$correlacion_con_pobreza[numerical_target_corr$variable == var2]
      
      # Proponer eliminar la de menor correlación absoluta
      if (abs(corr1) >= abs(corr2)) {
        variables_eliminar <- rbind(variables_eliminar, data.frame(
          variable = var2,
          motivo = paste("Redundante con", var1, "pero menor correlación con objetivo")
        ))
      } else {
        variables_eliminar <- rbind(variables_eliminar, data.frame(
          variable = var1,
          motivo = paste("Redundante con", var2, "pero menor correlación con objetivo")
        ))
      }
    }
  }
}

# 2. Crear lista de variables más predictivas según correlación
if (exists("numerical_target_corr") && nrow(numerical_target_corr) > 0) {
  top_predictors <- head(numerical_target_corr[order(-abs(numerical_target_corr$correlacion_con_pobreza)), ], 10)
  
  # Guardar predictores importantes en formato LaTeX
  print(xtable(top_predictors, 
               caption = "Variables Más Predictivas según Correlación con Pobreza", 
               label = "tab:top_predictors"),
        file = "views/tables/variables_mas_predictivas.tex",
        include.rownames = FALSE,
        floating = TRUE,
        latex.environments = "center",
        booktabs = TRUE)
}

# 3. Guardar variables a eliminar en formato LaTeX
if (nrow(variables_eliminar) > 0) {
  print(xtable(variables_eliminar, 
               caption = "Variables Candidatas a Eliminar", 
               label = "tab:variables_eliminar"),
        file = "views/tables/variables_candidatas_eliminar.tex",
        include.rownames = FALSE,
        floating = TRUE,
        latex.environments = "center",
        booktabs = TRUE)
}

# 4. Crear tabla resumen de recomendaciones
recomendaciones <- data.frame(
  categoria = c("Manejo del desbalance de clases", 
                "Tratamiento de valores faltantes", 
                "Tratamiento de outliers",
                "Transformación de variables",
                "Selección de variables"),
  recomendacion = c(
    paste0("Usar técnicas como SMOTE o ponderación de clases. Ratio de desbalance: ", 
           round(imbalance_ratio, 2), ":1"),
    "Imputar con mediana para variables numéricas y moda para categóricas",
    "Considerar winsorización para las variables con >5% de outliers",
    "Transformación logarítmica para variables con distribución sesgada",
    paste0("Mantener variables con mayor correlación con pobreza y eliminar variables redundantes")
  )
)

# Guardar recomendaciones en formato LaTeX
print(xtable(recomendaciones, 
             caption = "Recomendaciones para Preprocesamiento de Datos", 
             label = "tab:recomendaciones"),
      file = "views/tables/recomendaciones_preprocesamiento.tex",
      include.rownames = FALSE,
      floating = TRUE,
      latex.environments = "center",
      booktabs = TRUE)

cat("Análisis exploratorio completado. Resultados guardados en carpetas 'views/tables' y 'views/figures'.\n")

################################################################################
#                            FINALIZACIÓN DEL SCRIPT                           #
################################################################################