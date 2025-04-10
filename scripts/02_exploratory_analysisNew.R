################################################################################
# TÍTULO: 02_exploratory_analysis.R                                           #
# PROYECTO: Predicción de Pobreza en Colombia - Equipo 8                      #
# DESCRIPCIÓN: Análisis exploratorio de datos para la toma de decisiones      #
#              sobre el preprocesamiento y modelamiento                       #
# FECHA: 9 de abril de 2025                                                   #
################################################################################

# Configurar directorio de trabajo automáticamente
if (!require(rstudioapi)) install.packages("rstudioapi")
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# Subir un nivel directorio para acceder a la estructura principal del proyecto
setwd("../")

# Cargar librerías necesarias usando pacman
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,     # Manipulación de datos y visualización
  skimr,         # Resumen de datos
  visdat,        # Visualización de datos faltantes
  corrplot,      # Para visualización de correlaciones
  ggplot2,       # Visualizaciones
  gridExtra,     # Para organizar múltiples plots
  knitr,         # Para generar tablas
  xtable,        # Para exportar tablas a LaTeX
  stargazer,     # Tablas estadísticas en formato LaTeX
  GGally         # Para crear matrices de scatter plots avanzadas
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

# Convertir Pobre a factor para mejor manejo
train_data$Pobre <- factor(train_data$Pobre, levels = c(0, 1), labels = c("No Pobre", "Pobre"))

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

# Calcular proporción de hogares pobres y no pobres
poverty_distribution <- train_data %>%
  count(Pobre) %>%
  mutate(proportion = n / sum(n) * 100)

# Mostrar desequilibrio de clases en consola
cat("Distribución de la variable objetivo:\n")
print(poverty_distribution)

# Calcular ratio de desequilibrio para informar estrategias de balanceo
imbalance_ratio <- poverty_distribution$n[poverty_distribution$Pobre == "No Pobre"] / 
  poverty_distribution$n[poverty_distribution$Pobre == "Pobre"]
cat("Ratio de desequilibrio (No Pobres / Pobres):", round(imbalance_ratio, 2), "\n")

# Identificar variables categóricas relevantes para gráficos facetados
# Seleccionamos algunas variables que podrían ser relevantes para mostrar distribución de pobreza
# Aquí asumimos algunas variables categóricas, ajustar según la estructura real de datos
potential_cat_vars <- c("P5090", "Estrato1", "Depto") # Ajustar según variables disponibles
cat_vars_for_facet <- c()

# Verificar cuáles de estas variables existen en el dataset
for(var in potential_cat_vars) {
  if(var %in% colnames(train_data)) {
    cat_vars_for_facet <- c(cat_vars_for_facet, var)
  }
}

# Crear gráficos facetados para variables categóricas existentes
if(length(cat_vars_for_facet) > 0) {
  for(var in cat_vars_for_facet) {
    # Verificar que la variable tiene menos de 10 categorías para facilitar visualización
    if(length(unique(train_data[[var]])) < 10) {
      p <- ggplot(train_data, aes(x = Pobre, fill = Pobre)) +
        geom_bar(position = "dodge") +
        facet_wrap(~ get(var), scales = "free_y") +
        theme_minimal() +
        labs(title = paste("Distribución de Pobreza por", var),
             x = "",
             y = "Número de Hogares") +
        scale_fill_manual(values = c("No Pobre" = "steelblue", "Pobre" = "coral"))
      
      # Guardar gráfico
      png(paste0("views/figures/poverty_distribution_by_", var, ".png"), 
          width = 1000, height = 800, res = 100)
      print(p)
      dev.off()
    }
  }
}

################################################################################
# 3. ANÁLISIS DE VALORES FALTANTES                                             #
################################################################################

cat("Analizando valores faltantes...\n")

# 3.1 Visualización general de valores faltantes usando visdat
# Tomar una muestra del dataset para evitar el error de tamaño
sample_size <- min(5000, nrow(train_data))  # Limitar a 5000 filas para visualización
train_data_sample <- train_data %>% 
  sample_n(sample_size)

png("views/figures/missing_values_pattern.png", width = 1200, height = 800, res = 100)
vis_miss(train_data_sample, warn_large_data = FALSE)
dev.off()

# 3.2 Visualización de tipos de datos y valores faltantes
png("views/figures/data_types_missing.png", width = 1200, height = 800, res = 100)
vis_dat(train_data_sample, warn_large_data = FALSE)
dev.off()

# 3.3 Calcular y visualizar cantidad y porcentaje de valores faltantes por variable
missing_analysis <- data.frame(
  variable = names(train_data),
  n_missing = colSums(is.na(train_data)),
  pct_missing = round(colSums(is.na(train_data)) / nrow(train_data) * 100, 2)
) %>%
  arrange(desc(pct_missing))

# Guardar análisis de valores faltantes en formato LaTeX
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
    png("views/figures/missing_values_top15.png", width = 1000, height = 600, res = 100)
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

# 3.4 Análisis de correlación en valores faltantes
# Crear una matriz donde 1=valor faltante, 0=valor presente
missing_matrix <- train_data %>% 
  is.na() %>% 
  as.data.frame() %>%
  select_if(function(x) sum(x) > 0)  # solo variables con al menos un NA

# Si hay suficientes variables con valores faltantes, calcular correlación
if(ncol(missing_matrix) > 1) {
  missing_cors <- cor(missing_matrix)
  
  # Visualizar correlaciones de valores faltantes
  png("views/figures/missing_correlation.png", width = 900, height = 900, res = 100)
  corrplot(missing_cors, method = "circle", type = "upper", 
           tl.cex = 0.7, tl.col = "black", diag = FALSE,
           title = "Correlación entre Valores Faltantes",
           mar = c(0,0,2,0))
  dev.off()
}

# 3.5 Análisis de valores faltantes por estado de pobreza
# Calcular porcentaje de valores faltantes por variable, separado por estado de pobreza
if(ncol(missing_matrix) > 0) {
  missing_by_poverty <- data.frame()
  
  for(var in names(missing_matrix)) {
    # Calcular porcentaje de NA para cada grupo
    no_poor_missing <- mean(missing_matrix[train_data$Pobre == "No Pobre", var]) * 100
    poor_missing <- mean(missing_matrix[train_data$Pobre == "Pobre", var]) * 100
    
    missing_by_poverty <- rbind(missing_by_poverty, data.frame(
      variable = var,
      no_poor_missing = no_poor_missing,
      poor_missing = poor_missing,
      difference = no_poor_missing - poor_missing
    ))
  }
  
  # Ordenar por diferencia absoluta
  missing_by_poverty <- missing_by_poverty %>%
    arrange(desc(abs(difference)))
  
  # Guardar tabla
  print(xtable(missing_by_poverty, 
               caption = "Diferencia en Valores Faltantes por Estado de Pobreza (%)", 
               label = "tab:missing_by_poverty"),
        file = "views/tables/missing_by_poverty.tex",
        include.rownames = FALSE,
        floating = TRUE,
        latex.environments = "center",
        booktabs = TRUE)
  
  # Visualizar diferencias importantes (top 10)
  if(nrow(missing_by_poverty) > 0) {
    # Preparar datos para visualización
    top_vars <- head(missing_by_poverty, 10)$variable
    plot_data <- data.frame()
    
    for(var in top_vars) {
      plot_data <- rbind(plot_data, data.frame(
        variable = var,
        poverty_status = "No Pobre",
        missing_pct = missing_by_poverty$no_poor_missing[missing_by_poverty$variable == var]
      ))
      plot_data <- rbind(plot_data, data.frame(
        variable = var,
        poverty_status = "Pobre",
        missing_pct = missing_by_poverty$poor_missing[missing_by_poverty$variable == var]
      ))
    }
    
    # Crear gráfico
    png("views/figures/missing_by_poverty_status.png", width = 1000, height = 600, res = 100)
    ggplot(plot_data, aes(x = reorder(variable, missing_pct), y = missing_pct, fill = poverty_status)) +
      geom_bar(stat = "identity", position = "dodge") +
      coord_flip() +
      scale_fill_manual(values = c("No Pobre" = "steelblue", "Pobre" = "coral")) +
      labs(title = "Porcentaje de Valores Faltantes por Estado de Pobreza",
           x = "",
           y = "Porcentaje de Valores Faltantes",
           fill = "Estado de Pobreza") +
      theme_minimal()
    dev.off()
  }
}

# Mostrar variables con valores faltantes
cat("Variables con valores faltantes (ordenadas por % de NA):\n")
print(missing_analysis[missing_analysis$pct_missing > 0, ])

################################################################################
# 4. ANÁLISIS DESCRIPTIVO DE VARIABLES NUMÉRICAS Y CATEGÓRICAS                #
################################################################################

cat("Realizando análisis descriptivo de variables...\n")

# 4.1 Identificar tipos de variables
# Seleccionar variables numéricas (excluyendo id)
numeric_vars <- train_data %>% 
  select(-id) %>%  # Excluir id que es identificador alfanumérico
  select_if(is.numeric) %>%  # Seleccionar solo variables numéricas
  names()

# Para variables categóricas, buscar factores, caracteres y numéricas con pocos valores únicos
categorical_vars <- train_data %>%
  select(-id, -Pobre) %>%  # Excluir id y variable objetivo
  select_if(function(x) is.factor(x) || is.character(x) || 
              (is.numeric(x) && length(unique(x)) <= 10)) %>%
  names()

# Eliminar solapamiento entre numéricas y categóricas
numeric_vars <- setdiff(numeric_vars, categorical_vars)

# 4.2 Estadísticas descriptivas de variables numéricas
# Separar variables continuas y discretas para mejor manejo
numeric_vars_continuous <- numeric_vars[sapply(numeric_vars, function(x) 
  length(unique(train_data[[x]])) > 20)]
numeric_vars_discrete <- setdiff(numeric_vars, numeric_vars_continuous)

# Estadísticas descriptivas para variables numéricas continuas
if(length(numeric_vars_continuous) > 0) {
  # Crear data frame solo con variables continuas seleccionadas
  numeric_data_continuous <- train_data %>% select(all_of(numeric_vars_continuous))
  
  # Generar estadísticas descriptivas usando stargazer directamente a LaTeX
  stargazer(numeric_data_continuous, 
            title = "Estadísticas Descriptivas de Variables Numéricas Continuas",
            digits = 2,
            label = "tab:descriptive_stats_continuous",
            out = "views/tables/descriptive_statistics_continuous.tex")
}

# Estadísticas descriptivas para variables numéricas discretas
if(length(numeric_vars_discrete) > 0) {
  numeric_data_discrete <- train_data %>% select(all_of(numeric_vars_discrete))
  
  stargazer(numeric_data_discrete, 
            title = "Estadísticas Descriptivas de Variables Numéricas Discretas",
            digits = 2,
            label = "tab:descriptive_stats_discrete",
            out = "views/tables/descriptive_statistics_discrete.tex")
}

# 4.3 Crear boxplots comparativos para variables numéricas continuas por estado de pobreza
if(length(numeric_vars_continuous) > 0) {
  # Limitar a top 12 variables (basado en correlación con pobreza)
  # Para calcular correlación necesitamos convertir Pobre a numérico
  pobre_numeric <- as.numeric(train_data$Pobre) - 1
  
  correlations <- sapply(numeric_vars_continuous, function(var) {
    tryCatch({
      cor(train_data[[var]], pobre_numeric, use = "pairwise.complete.obs")
    }, error = function(e) NA)
  })
  
  # Eliminar NA y ordenar por correlación absoluta
  correlations <- correlations[!is.na(correlations)]
  top_vars <- names(correlations[order(abs(correlations), decreasing = TRUE)][1:min(12, length(correlations))])
  
  # Crear boxplots para cada variable
  for(var in top_vars) {
    # Evitar variables con demasiados valores NA
    if(sum(is.na(train_data[[var]])) / nrow(train_data) < 0.3) {
      # Crear plot combinando boxplot y puntos
      p <- ggplot(train_data, aes(x = Pobre, y = get(var), fill = Pobre)) +
        geom_boxplot(alpha = 0.7, outlier.shape = NA) +  # Sin outliers para evitar duplicados
        geom_jitter(alpha = 0.1, width = 0.2) +  # Añadir puntos con jitter para mejor visualización
        theme_minimal() +
        scale_fill_manual(values = c("No Pobre" = "steelblue", "Pobre" = "coral")) +
        labs(title = paste("Distribución de", var, "por Estado de Pobreza"),
             y = var,
             x = "")
      
      # Guardar gráfico
      png(paste0("views/figures/boxplot_", var, ".png"), width = 800, height = 600, res = 100)
      print(p)
      dev.off()
    }
  }
  
  # Crear gráficos de densidad para las mismas variables
  for(var in top_vars) {
    if(sum(is.na(train_data[[var]])) / nrow(train_data) < 0.3) {
      p <- ggplot(train_data, aes(x = get(var), fill = Pobre)) +
        geom_density(alpha = 0.5) +
        theme_minimal() +
        scale_fill_manual(values = c("No Pobre" = "steelblue", "Pobre" = "coral")) +
        labs(title = paste("Densidad de", var, "por Estado de Pobreza"),
             x = var,
             y = "Densidad")
      
      # Guardar gráfico
      png(paste0("views/figures/density_", var, ".png"), width = 800, height = 600, res = 100)
      print(p)
      dev.off()
    }
  }
}

# 4.4 Análisis de variables categóricas: tablas de contingencia y pruebas chi-cuadrado
if(length(categorical_vars) > 0) {
  contingency_results <- data.frame(
    variable = character(),
    chi_squared = numeric(),
    p_value = numeric(),
    df = numeric(),
    cramers_v = numeric(),
    stringsAsFactors = FALSE
  )
  
  for(var in categorical_vars) {
    # Evitar errores si hay muchos NA
    if(sum(!is.na(train_data[[var]])) / nrow(train_data) > 0.7) {
      # Crear tabla de contingencia
      cont_table <- table(train_data[[var]], train_data$Pobre)
      
      # Guardar tabla en formato LaTeX si no es demasiado grande
      if(nrow(cont_table) <= 10 && ncol(cont_table) <= 10) {
        # Convertir a data.frame para usar xtable
        cont_df <- as.data.frame.matrix(cont_table)
        
        print(xtable(cont_df, 
                     caption = paste("Tabla de Contingencia para", var, "vs Pobreza"), 
                     label = paste0("tab:contingency_", var)),
              file = paste0("views/tables/contingency_", var, ".tex"),
              floating = TRUE,
              latex.environments = "center",
              booktabs = TRUE)
      }
      
      # Realizar prueba chi-cuadrado
      chi_result <- chisq.test(cont_table, correct = FALSE)
      
      # Calcular V de Cramer (coeficiente de asociación)
      cramers_v <- sqrt(chi_result$statistic / (sum(cont_table) * min(nrow(cont_table)-1, ncol(cont_table)-1)))
      
      # Guardar resultados
      contingency_results <- rbind(contingency_results, data.frame(
        variable = var,
        chi_squared = chi_result$statistic,
        p_value = chi_result$p.value,
        df = chi_result$parameter,
        cramers_v = cramers_v,
        stringsAsFactors = FALSE
      ))
    }
  }
  
  # Ordenar por p-valor (menor a mayor)
  contingency_results <- contingency_results %>% arrange(p_value)
  
  # Guardar resultados en formato LaTeX
  print(xtable(contingency_results, 
               caption = "Pruebas de Independencia Chi-Cuadrado para Variables Categóricas vs Pobreza", 
               label = "tab:chi_squared_tests"),
        file = "views/tables/chi_squared_tests.tex",
        include.rownames = FALSE,
        floating = TRUE,
        latex.environments = "center",
        booktabs = TRUE)
  
  # Mostrar resultados en consola
  cat("Resultados de pruebas chi-cuadrado para variables categóricas vs pobreza:\n")
  print(contingency_results)
}

################################################################################
# 5. ANÁLISIS DE CORRELACIÓN ENTRE VARIABLES                                   #
################################################################################

cat("Analizando correlaciones entre variables...\n")

# 5.1 Análisis de correlación para variables numéricas
# Seleccionar solo variables numéricas continuas
corr_vars <- numeric_vars_continuous

# Verificar que hay suficientes variables para calcular correlaciones
if (length(corr_vars) >= 2) {
  # Crear data frame para correlación
  corr_data <- train_data %>% select(all_of(corr_vars))
  
  # Eliminar filas con NA para cálculo de correlación
  corr_data <- corr_data[complete.cases(corr_data), ]
  
  # Si todavía hay suficientes datos
  if (nrow(corr_data) > 30 && ncol(corr_data) >= 2) {
    # Calcular matriz de correlación
    correlation_matrix <- cor(corr_data)
    
    # Crear visualización de correlaciones usando corrplot con agrupamiento jerárquico
    png("views/figures/correlation_heatmap.png", width = 1200, height = 1200, res = 120)
    corrplot(correlation_matrix, 
             method = "color",     # Usar colores en vez de círculos
             type = "upper",       # Solo mostrar la parte superior de la matriz
             order = "hclust",     # Ordenar por agrupamiento jerárquico
             addrect = 3,          # Añadir rectanglos para mostrar clusters
             tl.cex = 0.7,         # Tamaño del texto
             tl.col = "black",     # Color del texto
             diag = FALSE)         # No mostrar la diagonal
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
    }
    
    # 5.2 Crear matriz de scatter plots para las variables más correlacionadas
    # Limitar a 5-7 variables para mantener la legibilidad
    if (length(corr_vars) > 0) {
      # Añadir la variable objetivo para ver relaciones
      plot_vars <- c(sample(corr_vars, min(6, length(corr_vars))), "Pobre")
      
      # Crear data frame para plotting
      plot_data <- train_data %>% 
        select(all_of(plot_vars)) %>%
        # Tomar una muestra aleatoria si hay demasiadas filas para el scatter plot
        sample_n(min(1000, nrow(.)))
      
      # Crear matriz de scatter plots
      png("views/figures/scatter_matrix.png", width = 1200, height = 1200, res = 120)
      ggpairs(plot_data, 
              aes(color = Pobre, alpha = 0.5),  # Colorear por estado de pobreza
              lower = list(continuous = "points"), # Puntos en panel inferior
              upper = list(continuous = "cor"),   # Correlaciones en panel superior
              diag = list(continuous = "densityDiag"), # Densidades en diagonal
              progress = FALSE) +                 # No mostrar barra de progreso
        scale_color_manual(values = c("No Pobre" = "steelblue", "Pobre" = "coral")) +
        theme_minimal()
      dev.off()
    }
  }
}

################################################################################
# 6. ANÁLISIS DE OUTLIERS                                                      #
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

# Aplicar análisis de outliers a variables numéricas continuas
outlier_vars <- numeric_vars_continuous

# Si hay variables para analizar
if(length(outlier_vars) > 0) {
  # Aplicar función a cada variable
  outlier_analysis <- do.call(rbind, lapply(outlier_vars, function(var) {
    analyze_outliers_iqr(train_data, var)
  }))
  
  # Ordenar por porcentaje de outliers
  outlier_analysis <- outlier_analysis %>% arrange(desc(pct_outliers))
  
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
  print(head(outlier_analysis, 10))
  
  # Visualizar outliers para las 5 variables con más outliers
  top_outlier_vars <- head(outlier_analysis$variable, 5)
  
  for(var in top_outlier_vars) {
    # Crear boxplot con puntos para visualizar outliers
    png(paste0("views/figures/outliers_", var, ".png"), width = 900, height = 600, res = 100)
    p <- ggplot(train_data, aes(x = Pobre, y = get(var), fill = Pobre)) +
      geom_boxplot(alpha = 0.7, outlier.shape = NA) +  # No mostrar outliers en el boxplot
      geom_jitter(aes(color = Pobre), alpha = 0.4, width = 0.25) +  # Añadir puntos con jitter para ver distribución
      scale_fill_manual(values = c("No Pobre" = "steelblue", "Pobre" = "coral")) +
      scale_color_manual(values = c("No Pobre" = "steelblue", "Pobre" = "coral")) +
      labs(title = paste("Distribución y Outliers de", var, "por Estado de Pobreza"),
           y = var,
           x = "") +
      theme_minimal()
    print(p)
    dev.off()
  }
}

################################################################################
# 7. ANÁLISIS DE RELACIÓN CON LA VARIABLE OBJETIVO                             #
################################################################################

cat("Analizando relaciones con la variable objetivo (pobreza)...\n")

# Convertir Pobre a numérico para cálculos de correlación
pobre_numeric <- as.numeric(train_data$Pobre) - 1

# Calcular correlación con la variable objetivo para variables numéricas
numerical_target_corr <- data.frame(
  variable = character(),
  correlacion_con_pobreza = numeric(),
  stringsAsFactors = FALSE
)

# Solo incluir variables numéricas
for (var in numeric_vars) {
  if (sum(!is.na(train_data[[var]])) > 0) {
    corr_value <- tryCatch({
      cor(train_data[[var]], pobre_numeric, use = "pairwise.complete.obs")
    }, error = function(e) NA)
    
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

# Visualizar top 15 variables correlacionadas con pobreza
if (nrow(numerical_target_corr) > 0) {
  top_corr <- head(numerical_target_corr, 15)
  
  # Crear gráfico
  png("views/figures/top_poverty_correlations.png", width = 900, height = 700, res = 100)
  ggplot(top_corr, aes(x = reorder(variable, abs(correlacion_con_pobreza)), 
                       y = correlacion_con_pobreza)) +
    geom_bar(stat = "identity", 
             aes(fill = correlacion_con_pobreza > 0)) +
    scale_fill_manual(values = c("TRUE" = "forestgreen", "FALSE" = "coral"),
                      labels = c("TRUE" = "Positiva", "FALSE" = "Negativa"),
                      name = "Correlación") +
    coord_flip() +
    labs(title = "Top 15 Variables Correlacionadas con Pobreza",
         x = "",
         y = "Correlación con Pobreza") +
    theme_minimal()
  dev.off()
  
  # Guardar correlaciones en formato LaTeX
  print(xtable(top_corr, 
               caption = "Top 15 Variables Numéricas Correlacionadas con Pobreza", 
               label = "tab:poverty_correlation"),
        file = "views/tables/top_correlacion_variables_pobreza.tex",
        include.rownames = FALSE,
        floating = TRUE,
        latex.environments = "center",
        booktabs = TRUE)
}

################################################################################
# 8. RECOMENDACIONES Y CONCLUSIONES                                            #
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

# Variables con alta proporción de outliers (>20%)
if (exists("outlier_analysis")) {
  high_outlier_vars <- outlier_analysis$variable[outlier_analysis$pct_outliers > 20]
  if (length(high_outlier_vars) > 0) {
    variables_eliminar <- rbind(variables_eliminar, data.frame(
      variable = high_outlier_vars,
      motivo = "Más del 20% de outliers"
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

# 2. Crear lista de variables más predictivas
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

# 3. Guardar variables categóricas más relevantes
if (exists("contingency_results") && nrow(contingency_results) > 0) {
  # Filtrar solo aquellas con p-valor significativo
  signif_cat_vars <- contingency_results %>% 
    filter(p_value < 0.05) %>%
    arrange(p_value)
  
  if (nrow(signif_cat_vars) > 0) {
    print(xtable(signif_cat_vars, 
                 caption = "Variables Categóricas Significativamente Asociadas con Pobreza", 
                 label = "tab:signif_cat_vars"),
          file = "views/tables/variables_categoricas_significativas.tex",
          include.rownames = FALSE,
          floating = TRUE,
          latex.environments = "center",
          booktabs = TRUE)
  }
}

# 4. Guardar variables a eliminar en formato LaTeX
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

# 5. Crear tabla resumen de recomendaciones
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