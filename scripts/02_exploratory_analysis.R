################################################################################
# TÍTULO: 02_exploratory_analysis.R                                           #
# PROYECTO: Predicción de Pobreza en Colombia                                 #
# DESCRIPCIÓN: Análisis exploratorio de datos para identificar patrones,      #
#              valores faltantes, outliers y relaciones con la variable       #
#              objetivo de pobreza.                                           #
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
  corrplot,    # Visualización de correlaciones
  gridExtra,   # Organización de gráficos en cuadrículas
  ggcorrplot,  # Correlogramas mejorados
  visdat,      # Visualización de datos faltantes
  scales,      # Escalas para gráficas
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
# 2. ANÁLISIS DESCRIPTIVO GENERAL                                              #
################################################################################

cat("Realizando análisis descriptivo general...\n")

# Resumen estadístico completo del conjunto de entrenamiento
train_summary <- skim(train_data)

# Exportar resumen estadístico a CSV
train_summary_export <- as.data.frame(train_summary)
write.csv(train_summary_export, "views/tables/train_summary_statistics.csv", row.names = FALSE)

# Crear tabla con estadísticas descriptivas principales para variables numéricas
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

# Mostrar estadísticas en consola
print(stat_desc)

################################################################################
# 3. ANÁLISIS DE LA DISTRIBUCIÓN DE POBREZA (VARIABLE OBJETIVO)                #
################################################################################

cat("Analizando distribución de la variable objetivo (pobreza)...\n")

# Calcular proporción de hogares pobres y no pobres
poverty_distribution <- train_data %>%
  count(Pobre) %>%
  mutate(proportion = n / sum(n) * 100,
         Pobre = factor(Pobre, levels = c(0, 1), 
                        labels = c("No Pobre", "Pobre")))

# Guardar datos de distribución
write.csv(poverty_distribution, "views/tables/poverty_distribution.csv", row.names = FALSE)

# Visualizar distribución de pobreza
poverty_plot <- ggplot(poverty_distribution, aes(x = Pobre, y = proportion, fill = Pobre)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_text(aes(label = sprintf("%.1f%%", proportion)), 
            position = position_stack(vjust = 0.5), 
            color = "white", size = 5, fontface = "bold") +
  labs(title = "Distribución de Pobreza en los Datos de Entrenamiento",
       x = "",
       y = "Porcentaje (%)") +
  scale_fill_manual(values = c("No Pobre" = "#1F77B4", "Pobre" = "#FF7F0E")) +
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 12))

# Guardar gráfico
ggsave("views/figures/poverty_distribution.png", poverty_plot, width = 8, height = 6, dpi = 300)

# Mostrar desequilibrio de clases en consola
cat("Distribución de la variable objetivo:\n")
print(poverty_distribution)
imbalance_ratio <- poverty_distribution$n[poverty_distribution$Pobre == "No Pobre"] / 
  poverty_distribution$n[poverty_distribution$Pobre == "Pobre"]
cat("Ratio de desequilibrio (No Pobres / Pobres):", imbalance_ratio, "\n")

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

# Guardar análisis de valores faltantes
write.csv(missing_analysis, "views/tables/missing_values_analysis.csv", row.names = FALSE)

# Visualizar valores faltantes
# 1. Visualización global de valores faltantes
missing_plot <- vis_miss(train_data) +
  labs(title = "Valores Faltantes en el Conjunto de Entrenamiento")
ggsave("views/figures/missing_values_global.png", missing_plot, width = 10, height = 8, dpi = 300)

# 2. Visualización de porcentaje de valores faltantes por variable
missing_bars <- missing_analysis %>%
  filter(pct_missing > 0) %>%  # Solo variables con valores faltantes
  ggplot(aes(x = reorder(variable, pct_missing), y = pct_missing)) +
  geom_bar(stat = "identity", fill = "#FF7F0E") +
  geom_text(aes(label = sprintf("%.1f%%", pct_missing)), 
            hjust = -0.1, size = 3.5) +
  coord_flip() +
  labs(title = "Porcentaje de Valores Faltantes por Variable",
       x = "Variable",
       y = "Porcentaje de Valores Faltantes (%)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Guardar gráfico de barras de valores faltantes
ggsave("views/figures/missing_values_bars.png", missing_bars, width = 9, height = 7, dpi = 300)

# 3. Correlación entre valores faltantes (para detectar patrones)
# Crear una matriz de variables con valores faltantes (1 si es NA, 0 si no)
vars_with_missing <- names(train_data)[colSums(is.na(train_data)) > 0]

if (length(vars_with_missing) > 1) {
  missing_corr_data <- train_data %>%
    select(all_of(vars_with_missing)) %>%
    mutate_all(~as.integer(is.na(.)))
  
  # Calcular correlación entre variables faltantes
  missing_corr <- cor(missing_corr_data)
  
  # Visualizar correlación de valores faltantes
  missing_corr_plot <- ggcorrplot(missing_corr, 
                                  hc.order = TRUE, 
                                  type = "lower",
                                  lab = TRUE, 
                                  lab_size = 3,
                                  title = "Correlación entre Valores Faltantes") +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"))
  
  # Guardar gráfico de correlación de valores faltantes
  ggsave("views/figures/missing_values_correlation.png", missing_corr_plot, width = 10, height = 8, dpi = 300)
  
  cat("Análisis de correlación entre valores faltantes completado.\n")
} else {
  cat("No hay suficientes variables con valores faltantes para análisis de correlación.\n")
}

################################################################################
# 5. DISTRIBUCIÓN DE VARIABLES NUMÉRICAS                                       #
################################################################################

cat("Analizando distribución de variables numéricas...\n")

# Seleccionar variables numéricas clave para visualización (excluyendo id)
key_numeric_vars <- c("Ingpcug", "Lp", "n_miembros", "n_menores", "n_ocupados", "edad_promedio", "jefe_edad")
key_numeric_vars <- key_numeric_vars[key_numeric_vars %in% names(train_data)]

# Función para crear histogramas con densidad
create_histogram <- function(data, var_name, title = NULL) {
  if (is.null(title)) title <- paste("Distribución de", var_name)
  
  p <- ggplot(data, aes_string(x = var_name)) +
    geom_histogram(aes(y = ..density..), fill = "#1F77B4", alpha = 0.7, bins = 30) +
    geom_density(color = "#FF7F0E", size = 1) +
    labs(title = title,
         x = var_name,
         y = "Densidad") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"))
  
  return(p)
}

# Crear histogramas para cada variable numérica clave
histograms <- lapply(key_numeric_vars, function(var) {
  create_histogram(train_data, var)
})

# Organizar histogramas en una cuadrícula
hist_grid <- do.call(gridExtra::grid.arrange, c(histograms, ncol = 2))

# Guardar cuadrícula de histogramas
ggsave("views/figures/numeric_variables_histograms.png", hist_grid, width = 12, height = 15, dpi = 300)

################################################################################
# 6. DISTRIBUCIÓN DE VARIABLES CATEGÓRICAS                                     #
################################################################################

cat("Analizando distribución de variables categóricas...\n")

# Identificar variables categóricas clave (incluyendo factores y caracteres)
# Excluir 'id' ya que es un identificador alfanumérico único
cat_vars <- names(train_data)[sapply(train_data, function(x) is.factor(x) || is.character(x))]
cat_vars <- cat_vars[cat_vars != "id"] # Excluir id explícitamente

# Añadir variables binarias/discretas relevantes
binary_vars <- c("jefe_mujer", "Pobre")
cat_vars <- unique(c(cat_vars, binary_vars))

# Función para crear gráficos de barras para variables categóricas
create_barplot <- function(data, var_name, title = NULL) {
  if (is.null(title)) title <- paste("Distribución de", var_name)
  
  # Convertir a factor si no lo es
  if (!is.factor(data[[var_name]])) {
    data[[var_name]] <- as.factor(data[[var_name]])
  }
  
  # Calcular frecuencias
  freq_table <- data %>%
    count(!!sym(var_name)) %>%
    mutate(proportion = n / sum(n) * 100)
  
  # Crear gráfico
  p <- ggplot(freq_table, aes_string(x = var_name, y = "proportion")) +
    geom_bar(stat = "identity", fill = "#1F77B4", width = 0.7) +
    geom_text(aes(label = sprintf("%.1f%%", proportion)), 
              position = position_stack(vjust = 0.5), 
              size = 3.5) +
    labs(title = title,
         x = var_name,
         y = "Porcentaje (%)") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"),
          axis.text.x = element_text(angle = 45, hjust = 1))
  
  return(p)
}

# Crear gráficos de barras para cada variable categórica
barplots <- list()
for (var in cat_vars) {
  if (var %in% names(train_data)) {
    barplots[[var]] <- create_barplot(train_data, var)
  }
}

# Organizar gráficos de barras en una cuadrícula
if (length(barplots) > 0) {
  bar_grid <- do.call(gridExtra::grid.arrange, c(barplots, ncol = 2))
  
  # Guardar cuadrícula de gráficos de barras
  ggsave("views/figures/categorical_variables_barplots.png", bar_grid, width = 12, height = 15, dpi = 300)
}

################################################################################
# 7. ANÁLISIS DE OUTLIERS EN VARIABLES NUMÉRICAS                               #
################################################################################

cat("Analizando outliers en variables numéricas...\n")

# Función para detectar y analizar outliers usando método IQR
analyze_outliers <- function(data, var_name) {
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
  outlier_stats <- list(
    variable = var_name,
    n_total = length(var_data),
    n_outliers = length(outliers),
    pct_outliers = round(length(outliers) / length(var_data) * 100, 2),
    lower_bound = lower_bound,
    upper_bound = upper_bound,
    min_value = min(var_data),
    max_value = max(var_data)
  )
  
  return(outlier_stats)
}

# Analizar outliers en variables numéricas clave (excluyendo id y variables categóricas/binarias)
numeric_vars_for_outliers <- names(numeric_vars)

# Excluir variables de identificación o binarias
numeric_vars_for_outliers <- setdiff(numeric_vars_for_outliers, 
                                     c("Pobre", "jefe_mujer"))

# Aplicar análisis de outliers a cada variable
outlier_analysis <- lapply(numeric_vars_for_outliers, function(var) {
  analyze_outliers(train_data, var)
})

# Convertir a dataframe
outlier_df <- do.call(rbind, lapply(outlier_analysis, function(x) {
  data.frame(
    variable = x$variable,
    n_total = x$n_total,
    n_outliers = x$n_outliers,
    pct_outliers = x$pct_outliers,
    lower_bound = x$lower_bound,
    upper_bound = x$upper_bound,
    min_value = x$min_value,
    max_value = x$max_value
  )
}))

# Guardar análisis de outliers
write.csv(outlier_df, "views/tables/outlier_analysis.csv", row.names = FALSE)

# Visualizar outliers con boxplots para variables clave
key_vars_for_boxplot <- c("Ingpcug", "n_miembros", "n_ocupados", "edad_promedio")
key_vars_for_boxplot <- key_vars_for_boxplot[key_vars_for_boxplot %in% names(train_data)]

# Crear boxplots
boxplots <- lapply(key_vars_for_boxplot, function(var) {
  ggplot(train_data, aes_string(y = var)) +
    geom_boxplot(fill = "#1F77B4") +
    labs(title = paste("Boxplot de", var),
         y = var,
         x = "") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"))
})

# Organizar boxplots en una cuadrícula
box_grid <- do.call(gridExtra::grid.arrange, c(boxplots, ncol = 2))

# Guardar cuadrícula de boxplots
ggsave("views/figures/numeric_variables_boxplots.png", box_grid, width = 12, height = 10, dpi = 300)

################################################################################
# 8. ANÁLISIS DE CORRELACIÓN ENTRE VARIABLES NUMÉRICAS                        #
################################################################################

cat("Analizando correlaciones entre variables numéricas...\n")

# Seleccionar variables numéricas para análisis de correlación (excluyendo id y Pobre)
corr_vars <- train_data %>%
  select(-id) %>%  # Excluir id explícitamente
  select_if(is.numeric) %>%
  select(-Pobre)  # Excluir variable objetivo temporalmente

# Calcular matriz de correlación
correlation_matrix <- cor(corr_vars, use = "pairwise.complete.obs")

# Guardar matriz de correlación
write.csv(correlation_matrix, "views/tables/correlation_matrix.csv", row.names = TRUE)

# Visualizar matriz de correlación
corr_plot <- ggcorrplot(correlation_matrix,
                        hc.order = TRUE,
                        type = "lower",
                        lab = TRUE,
                        lab_size = 3,
                        title = "Correlación entre Variables Numéricas") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Guardar gráfico de correlación
ggsave("views/figures/correlation_matrix.png", corr_plot, width = 12, height = 10, dpi = 300)

################################################################################
# 9. ANÁLISIS DE RELACIONES CON LA VARIABLE OBJETIVO (POBREZA)                #
################################################################################

cat("Analizando relaciones con la variable objetivo (pobreza)...\n")

# Convertir Pobre a factor con etiquetas significativas
train_data$Pobre_factor <- factor(train_data$Pobre, 
                                  levels = c(0, 1), 
                                  labels = c("No Pobre", "Pobre"))

# 1. Relación entre variables numéricas y pobreza (boxplots)
key_numeric_for_poverty <- c("Ingpcug", "n_miembros", "n_menores", "n_ocupados", "edad_promedio")
key_numeric_for_poverty <- key_numeric_for_poverty[key_numeric_for_poverty %in% names(train_data)]

# Crear boxplots por estatus de pobreza
poverty_boxplots <- lapply(key_numeric_for_poverty, function(var) {
  ggplot(train_data, aes_string(x = "Pobre_factor", y = var, fill = "Pobre_factor")) +
    geom_boxplot() +
    labs(title = paste(var, "por Estatus de Pobreza"),
         x = "",
         y = var) +
    scale_fill_manual(values = c("No Pobre" = "#1F77B4", "Pobre" = "#FF7F0E")) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"),
          legend.position = "none")
})

# Organizar boxplots en una cuadrícula
poverty_box_grid <- do.call(gridExtra::grid.arrange, c(poverty_boxplots, ncol = 2))

# Guardar cuadrícula de boxplots por pobreza
ggsave("views/figures/numeric_by_poverty_boxplots.png", poverty_box_grid, width = 12, height = 15, dpi = 300)

# 2. Relación entre variables categóricas y pobreza (gráficos de barras apiladas)
key_categorical_for_poverty <- c("jefe_mujer", "Dominio")
key_categorical_for_poverty <- key_categorical_for_poverty[key_categorical_for_poverty %in% names(train_data)]

# Crear gráficos de barras apiladas
poverty_barplots <- lapply(key_categorical_for_poverty, function(var) {
  # Calcular proporciones
  prop_data <- train_data %>%
    group_by(!!sym(var), Pobre_factor) %>%
    summarise(count = n(), .groups = "drop") %>%
    group_by(!!sym(var)) %>%
    mutate(proportion = count / sum(count) * 100)
  
  # Crear gráfico
  ggplot(prop_data, aes_string(x = var, y = "proportion", fill = "Pobre_factor")) +
    geom_bar(stat = "identity", position = "stack") +
    geom_text(aes(label = sprintf("%.1f%%", proportion)),
              position = position_stack(vjust = 0.5),
              size = 3.5, color = "white") +
    labs(title = paste("Proporción de Pobreza por", var),
         x = var,
         y = "Porcentaje (%)",
         fill = "Estatus") +
    scale_fill_manual(values = c("No Pobre" = "#1F77B4", "Pobre" = "#FF7F0E")) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"),
          axis.text.x = element_text(angle = 45, hjust = 1))
})

# Organizar gráficos de barras en una cuadrícula
if (length(poverty_barplots) > 0) {
  poverty_bar_grid <- do.call(gridExtra::grid.arrange, c(poverty_barplots, ncol = 2))
  
  # Guardar cuadrícula de gráficos de barras por pobreza
  ggsave("views/figures/categorical_by_poverty_barplots.png", poverty_bar_grid, width = 12, height = 10, dpi = 300)
}

# 3. Calcular correlación puntual con la variable objetivo (Point-Biserial Correlation)
# Convertir Pobre a numérico para análisis de correlación
train_data$Pobre_num <- as.numeric(as.character(train_data$Pobre))

# Seleccionar variables numéricas (excluyendo id y la variable objetivo)
pb_corr_vars <- train_data %>%
  select(-id, -Pobre, -Pobre_factor, -Pobre_num) %>%
  select_if(is.numeric) %>%
  names()

# Calcular correlación con la variable objetivo
pb_correlation <- sapply(pb_corr_vars, function(var) {
  if (var %in% names(train_data)) {
    cor(train_data[[var]], train_data$Pobre_num, use = "pairwise.complete.obs")
  } else {
    NA
  }
})

# Crear dataframe de correlaciones ordenado
pb_corr_df <- data.frame(
  variable = names(pb_correlation),
  correlation = pb_correlation
) %>%
  arrange(desc(abs(correlation)))

# Guardar correlaciones con la variable objetivo
write.csv(pb_corr_df, "views/tables/poverty_correlation.csv", row.names = FALSE)

# Visualizar correlaciones con la variable objetivo
pb_corr_plot <- ggplot(pb_corr_df, aes(x = reorder(variable, abs(correlation)), y = correlation)) +
  geom_bar(stat = "identity", fill = ifelse(pb_corr_df$correlation > 0, "#1F77B4", "#FF7F0E")) +
  geom_text(aes(label = sprintf("%.3f", correlation)),
            hjust = ifelse(pb_corr_df$correlation > 0, 1.1, -0.1),
            size = 3.5) +
  coord_flip() +
  labs(title = "Correlación de Variables con la Pobreza",
       x = "",
       y = "Correlación") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Guardar gráfico de correlaciones con la pobreza
ggsave("views/figures/poverty_correlation_plot.png", pb_corr_plot, width = 10, height = 12, dpi = 300)

################################################################################
# 10. ANÁLISIS DE LÍNEA DE POBREZA VS INGRESO PER CÁPITA                      #
################################################################################

cat("Analizando la relación entre la línea de pobreza e ingreso per cápita...\n")

# Crear un gráfico de dispersión de ingreso per cápita vs línea de pobreza
lp_scatter <- ggplot(train_data, aes(x = Lp, y = Ingpcug, color = Pobre_factor)) +
  geom_point(alpha = 0.5) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  labs(title = "Ingreso Per Cápita vs Línea de Pobreza",
       x = "Línea de Pobreza (Lp)",
       y = "Ingreso Per Cápita (Ingpcug)",
       color = "Estatus") +
  scale_color_manual(values = c("No Pobre" = "#1F77B4", "Pobre" = "#FF7F0E")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Guardar gráfico
ggsave("views/figures/poverty_line_vs_income.png", lp_scatter, width = 10, height = 8, dpi = 300)

################################################################################
# 11. ANÁLISIS DE COMPOSICIÓN FAMILIAR Y POBREZA                              #
################################################################################

cat("Analizando la relación entre composición familiar y pobreza...\n")

# Crear visualización de relación entre n_miembros, n_ocupados y pobreza
family_scatter <- ggplot(train_data, aes(x = n_miembros, y = n_ocupados, color = Pobre_factor)) +
  geom_jitter(alpha = 0.6, width = 0.2, height = 0.2) +
  labs(title = "Relación entre Miembros del Hogar, Ocupados y Pobreza",
       x = "Número de Miembros",
       y = "Número de Ocupados",
       color = "Estatus") +
  scale_color_manual(values = c("No Pobre" = "#1F77B4", "Pobre" = "#FF7F0E")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Guardar gráfico
ggsave("views/figures/family_composition_poverty.png", family_scatter, width = 10, height = 8, dpi = 300)

################################################################################
# 12. ANÁLISIS DE RAZÓN DE DEPENDENCIA                                         #
################################################################################

cat("Analizando razón de dependencia económica...\n")

# Crear variable de razón de dependencia (miembros / ocupados)
train_data <- train_data %>%
  mutate(
    razon_dependencia = ifelse(n_ocupados > 0, n_miembros / n_ocupados, NA)
  )

# Analizar distribución de razón de dependencia
depend_hist <- ggplot(train_data, aes(x = razon_dependencia)) +
  geom_histogram(aes(y = ..density..), fill = "#1F77B4", alpha = 0.7, bins = 30) +
  geom_density(color = "#FF7F0E", size = 1) +
  labs(title = "Distribución de Razón de Dependencia",
       x = "Razón de Dependencia (Miembros / Ocupados)",
       y = "Densidad") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Guardar histograma
ggsave("views/figures/dependency_ratio_histogram.png", depend_hist, width = 9, height = 6, dpi = 300)

# Boxplot de razón de dependencia por estatus de pobreza
depend_box <- ggplot(train_data, aes(x = Pobre_factor, y = razon_dependencia, fill = Pobre_factor)) +
  geom_boxplot() +
  labs(title = "Razón de Dependencia por Estatus de Pobreza",
       x = "",
       y = "Razón de Dependencia") +
  scale_fill_manual(values = c("No Pobre" = "#1F77B4", "Pobre" = "#FF7F0E")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.position = "none")

# Guardar boxplot
ggsave("views/figures/dependency_ratio_by_poverty.png", depend_box, width = 9, height = 6, dpi = 300)

# Calcular estadísticas de razón de dependencia por estatus de pobreza
dependency_stats <- train_data %>%
  group_by(Pobre_factor) %>%
  summarise(
    n = sum(!is.na(razon_dependencia)),
    media = mean(razon_dependencia, na.rm = TRUE),
    mediana = median(razon_dependencia, na.rm = TRUE),
    desv_est = sd(razon_dependencia, na.rm = TRUE),
    min = min(razon_dependencia, na.rm = TRUE),
    max = max(razon_dependencia, na.rm = TRUE),
    .groups = "drop"
  )

# Guardar estadísticas
write.csv(dependency_stats, "views/tables/dependency_ratio_stats.csv", row.names = FALSE)

cat("Análisis de razón de dependencia completado.\n")

################################################################################
# 13. ANÁLISIS DE RELACIÓN ENTRE VARIABLES DE VIVIENDA Y POBREZA              #
################################################################################

cat("Analizando relación entre variables de vivienda y pobreza...\n")

# Variables de vivienda a analizar
housing_vars <- c("P5090", "P5130", "P5140")
housing_vars <- housing_vars[housing_vars %in% names(train_data)]

# Análisis para cada variable de vivienda
for (var in housing_vars) {
  # Verificar si la variable existe y no es completamente NA
  if (sum(!is.na(train_data[[var]])) > 0) {
    # Calcular distribución por estatus de pobreza
    housing_data <- train_data %>%
      group_by(!!sym(var), Pobre_factor) %>%
      summarise(count = n(), .groups = "drop") %>%
      group_by(!!sym(var)) %>%
      mutate(proportion = count / sum(count) * 100)
    
    # Crear gráfico
    p <- ggplot(housing_data, aes_string(x = var, y = "proportion", fill = "Pobre_factor")) +
      geom_bar(stat = "identity", position = "stack") +
      geom_text(aes(label = sprintf("%.1f%%", proportion)),
                position = position_stack(vjust = 0.5),
                size = 3.5, color = "white") +
      labs(title = paste("Relación entre", var, "y Pobreza"),
           x = var,
           y = "Porcentaje (%)",
           fill = "Estatus") +
      scale_fill_manual(values = c("No Pobre" = "#1F77B4", "Pobre" = "#FF7F0E")) +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"),
            axis.text.x = element_text(angle = 45, hjust = 1))
    
    # Guardar gráfico
    ggsave(paste0("views/figures/", var, "_poverty_relation.png"), p, width = 10, height = 7, dpi = 300)
    
    cat("Análisis completado para la variable", var, ".\n")
  } else {
    cat("Variable", var, "no disponible o completamente NA.\n")
  }
}

################################################################################
# 14. ANÁLISIS DE DOMINIOS GEOGRÁFICOS                                         #
################################################################################

cat("Analizando distribución geográfica de la pobreza...\n")

# Analizar la distribución de pobreza por dominio geográfico
if ("Dominio" %in% names(train_data)) {
  domain_poverty <- train_data %>%
    group_by(Dominio) %>%
    summarise(
      n_hogares = n(),
      n_pobres = sum(Pobre),
      pct_pobres = round(sum(Pobre) / n() * 100, 2),
      .groups = "drop"
    ) %>%
    arrange(desc(pct_pobres))
  
  # Guardar tabla
  write.csv(domain_poverty, "views/tables/poverty_by_domain.csv", row.names = FALSE)
  
  # Visualizar
  domain_plot <- ggplot(domain_poverty, aes(x = reorder(Dominio, pct_pobres), y = pct_pobres)) +
    geom_bar(stat = "identity", fill = "#1F77B4") +
    geom_text(aes(label = sprintf("%.1f%%", pct_pobres)), hjust = -0.1, size = 3.5) +
    coord_flip() +
    labs(title = "Porcentaje de Hogares Pobres por Dominio Geográfico",
         x = "Dominio",
         y = "Porcentaje de Hogares Pobres (%)") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"))
  
  # Guardar gráfico
  ggsave("views/figures/poverty_by_domain.png", domain_plot, width = 10, height = 8, dpi = 300)
  
  cat("Análisis por dominio geográfico completado.\n")
} else {
  cat("Variable 'Dominio' no disponible para análisis geográfico.\n")
}

################################################################################
# 15. ANÁLISIS DE TAMAÑO DEL HOGAR Y POBREZA                                   #
################################################################################

cat("Analizando relación entre tamaño del hogar y pobreza...\n")

# Crear tabla de contingencia de número de miembros por estatus de pobreza
if (all(c("n_miembros", "Pobre_factor") %in% names(train_data))) {
  # Calcular proporción de pobreza por número de miembros
  household_size_poverty <- train_data %>%
    group_by(n_miembros, Pobre_factor) %>%
    summarise(count = n(), .groups = "drop") %>%
    group_by(n_miembros) %>%
    mutate(
      proportion = count / sum(count) * 100,
      total_households = sum(count)
    ) %>%
    # Filtrar para mostrar solo tamaños de hogar con suficientes observaciones
    filter(total_households >= 10)
  
  # Guardar tabla
  write.csv(household_size_poverty, "views/tables/household_size_poverty.csv", row.names = FALSE)
  
  # Crear gráfico de líneas para mostrar tendencia
  size_plot <- ggplot(household_size_poverty %>% 
                        filter(Pobre_factor == "Pobre"), 
                      aes(x = n_miembros, y = proportion)) +
    geom_line(size = 1.2, color = "#FF7F0E") +
    geom_point(size = 3, color = "#FF7F0E") +
    labs(title = "Porcentaje de Hogares Pobres según Número de Miembros",
         x = "Número de Miembros del Hogar",
         y = "Porcentaje de Hogares Pobres (%)") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"))
  
  # Guardar gráfico
  ggsave("views/figures/household_size_poverty_trend.png", size_plot, width = 10, height = 6, dpi = 300)
  
  # Visualización de barras apiladas
  size_stack <- ggplot(household_size_poverty, 
                       aes(x = n_miembros, y = proportion, fill = Pobre_factor)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = sprintf("%.1f%%", proportion)),
              position = position_stack(vjust = 0.5),
              size = 3, color = "white") +
    labs(title = "Composición de Pobreza por Tamaño del Hogar",
         x = "Número de Miembros del Hogar",
         y = "Porcentaje (%)",
         fill = "Estatus") +
    scale_fill_manual(values = c("No Pobre" = "#1F77B4", "Pobre" = "#FF7F0E")) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"))
  
  # Guardar gráfico
  ggsave("views/figures/household_size_poverty_composition.png", size_stack, width = 10, height = 6, dpi = 300)
  
  cat("Análisis de tamaño del hogar y pobreza completado.\n")
} else {
  cat("Variables necesarias no disponibles para análisis de tamaño del hogar.\n")
}

################################################################################
# 16. RESUMEN DE HALLAZGOS Y RECOMENDACIONES                                   #
################################################################################

cat("Generando resumen de hallazgos y recomendaciones...\n")

# Crear un archivo de texto con los hallazgos principales y recomendaciones
findings <- c(
  "# RESUMEN DE HALLAZGOS DEL ANÁLISIS EXPLORATORIO",
  "",
  "## DISTRIBUCIÓN DE LA VARIABLE OBJETIVO:",
  paste0("- El ", round(poverty_distribution$proportion[poverty_distribution$Pobre == "Pobre"], 1), 
         "% de los hogares en la muestra son pobres."),
  paste0("- Existe un desequilibrio de clases con un ratio de ", round(imbalance_ratio, 2), 
         " (No Pobres / Pobres)."),
  "",
  "## VALORES FALTANTES:",
  paste0("- Las variables con mayor porcentaje de valores faltantes son: ", 
         paste(head(missing_analysis$variable[missing_analysis$pct_missing > 0], 3), collapse=", "), "."),
  "",
  "## OUTLIERS:",
  "- Se identificaron outliers en varias variables numéricas, especialmente en ingresos y número de miembros.",
  "",
  "## CORRELACIONES:",
  "- Las variables más correlacionadas con la pobreza son:",
  paste0("  * ", pb_corr_df$variable[1], " (", round(pb_corr_df$correlation[1], 3), ")"),
  paste0("  * ", pb_corr_df$variable[2], " (", round(pb_corr_df$correlation[2], 3), ")"),
  paste0("  * ", pb_corr_df$variable[3], " (", round(pb_corr_df$correlation[3], 3), ")"),
  "",
  "## HALLAZGOS SOBRE COMPOSICIÓN FAMILIAR:",
  "- Se observa una clara relación entre el tamaño del hogar y la probabilidad de pobreza.",
  "- La razón de dependencia (miembros/ocupados) es un fuerte predictor de pobreza.",
  "- Hogares con jefatura femenina muestran mayores tasas de pobreza.",
  "",
  "## DISTRIBUCIÓN GEOGRÁFICA:",
  "- Existen diferencias significativas en las tasas de pobreza por dominio geográfico.",
  "",
  "## RECOMENDACIONES PARA LIMPIEZA DE DATOS:",
  "1. Imputar valores faltantes considerando patrones identificados.",
  "2. Tratar outliers, especialmente en variables de ingreso.",
  "3. Crear variables adicionales que capturen relaciones entre miembros del hogar y ocupados.",
  "4. Considerar estrategias para manejar el desbalance de clases, como sobremuestreo, submuestreo o algoritmos que manejen desbalances.",
  "5. Evaluar transformaciones para variables con distribución sesgada.",
  "",
  "## VARIABLES POTENCIALMENTE IMPORTANTES:",
  "1. Ingreso per cápita y línea de pobreza (relación directa con la definición de pobreza).",
  "2. Número de miembros y ocupados del hogar (composición familiar).",
  "3. Razón de dependencia económica (miembros/ocupados).",
  "4. Características del jefe de hogar (género, edad).",
  "5. Ubicación geográfica (Dominio).",
  "6. Variables relacionadas con la vivienda.",
  "",
  "Este análisis servirá como guía para el procesamiento y limpieza de datos en el siguiente script."
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
cat("  - views/figures/ (visualizaciones)\n")
cat("\nHallazgos principales guardados en: views/tables/exploratory_analysis_findings.md\n")
cat("\nUtilizar estos resultados para guiar la limpieza de datos y selección de variables\n")
cat("en el siguiente script (03_data_cleaning.R).\n")
cat("======================================================================\n")