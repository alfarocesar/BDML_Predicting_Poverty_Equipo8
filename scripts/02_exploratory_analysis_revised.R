################################################################################
# TÍTULO: 02_exploratory_analysis.R                                           #
# PROYECTO: Predicción de Pobreza en Colombia - Equipo 8                      #
# DESCRIPCIÓN: Análisis exploratorio de datos para la toma de decisiones      #
#              sobre el preprocesamiento y modelamiento                       #
# FECHA: 10 de abril de 2025                                                  #
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
train_data <- read.csv("stores/processed/train_joined.csv", stringsAsFactors = FALSE)
test_data <- read.csv("stores/processed/test_joined.csv", stringsAsFactors = FALSE)

train_data$id <- as.character(train_data$id)
test_data$id <- as.character(test_data$id)
train_data$Pobre <- factor(train_data$Pobre, levels = c(0, 1), labels = c("No Pobre", "Pobre"))

cat("Dimensiones del conjunto de entrenamiento:", dim(train_data), "\n")
cat("Dimensiones del conjunto de prueba:", dim(test_data), "\n")

if (!dir.exists("views/tables")) dir.create("views/tables", recursive = TRUE)
if (!dir.exists("views/figures")) dir.create("views/figures", recursive = TRUE)

################################################################################
# 2. ANÁLISIS DE VALORES FALTANTES                                             #
################################################################################

cat("Analizando valores faltantes...\n")

missing_summary <- train_data %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "n_missing") %>%
  mutate(
    prop_missing = round(n_missing / nrow(train_data), 3)
  ) %>%
  arrange(desc(prop_missing))

# Exportar tabla para revisión manual
write_csv(missing_summary, "views/tables/valores_faltantes_train.csv")

# Decisión esperada: identificar variables con más del 20% de NA para evaluar si deben eliminarse, imputarse o conservarse con categoría "missing".
# Esta tabla permite tomar esa decisión manualmente según las recomendaciones del material “Visualizing and Handling Missing Values”.

################################################################################
# 3. ANÁLISIS DE VARIABLES CATEGÓRICAS                                         #
################################################################################

cat("Analizando variables categóricas...\n")

categoricas <- train_data %>%
  select(where(is.character)) %>%
  names()

tabla_categoricas <- map_dfr(categoricas, function(var) {
  df <- train_data %>%
    count(!!sym(var)) %>%
    arrange(desc(n)) %>%
    mutate(variable = var,
           prop = round(n / sum(n), 3)) %>%
    slice(1) %>%
    rename(valor_mas_frecuente = !!sym(var), frecuencia = n)
  df
})

write_csv(tabla_categoricas, "views/tables/tabla_categoricas.csv")

# Decisión esperada: si una categoría aparece más del 90% de las veces puede considerarse poco informativa para modelos no lineales.
# Esta tabla permite identificar esas variables para una evaluación manual.

################################################################################
# 4. ANÁLISIS DE VARIABLES NUMÉRICAS                                           #
################################################################################

cat("Analizando variables numéricas...\n")

numericas <- train_data %>%
  select(where(is.numeric)) %>%
  select(-id) %>%  # Excluir ID
  names()

tabla_numericas <- train_data %>%
  select(all_of(numericas)) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "valor") %>%
  group_by(variable) %>%
  summarise(
    min = min(valor, na.rm = TRUE),
    q25 = quantile(valor, 0.25, na.rm = TRUE),
    mediana = median(valor, na.rm = TRUE),
    media = mean(valor, na.rm = TRUE),
    q75 = quantile(valor, 0.75, na.rm = TRUE),
    max = max(valor, na.rm = TRUE),
    desv = sd(valor, na.rm = TRUE),
    n_missing = sum(is.na(valor)),
    prop_missing = round(mean(is.na(valor)), 3)
  ) %>%
  arrange(desc(prop_missing))

write_csv(tabla_numericas, "views/tables/tabla_numericas.csv")

# Decisión esperada: variables con desviación muy baja o sin variación podrían ser descartadas. Las que presentan valores extremos pueden requerir transformación.

################################################################################
# 5. VISUALIZACIONES SENCILLAS                                                 #
################################################################################

cat("Generando visualizaciones básicas...\n")

# Distribución de la variable objetivo
ggplot(train_data, aes(x = Pobre, fill = Pobre)) +
  geom_bar() +
  labs(title = "Distribución de la variable Pobre") +
  theme_minimal() +
  ggsave("views/figures/dist_pobre.png", width = 6, height = 4)

# Histograma de ejemplo para variable numérica
ggplot(train_data, aes(x = Ingtotugarr)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  labs(title = "Histograma de Ingreso total con imputación", x = "Ingreso", y = "Frecuencia") +
  theme_minimal() +
  ggsave("views/figures/hist_ingtotugarr.png", width = 6, height = 4)

# Boxplot para variable de ingreso por pobreza
ggplot(train_data, aes(x = Pobre, y = Ingtotugarr, fill = Pobre)) +
  geom_boxplot() +
  labs(title = "Ingreso total por condición de pobreza") +
  theme_minimal() +
  ggsave("views/figures/box_ingtotugarr_pobre.png", width = 6, height = 4)

# Estas gráficas ayudan a visualizar si la variable tiene poder discriminativo respecto a la pobreza.
# La decisión se basa en si la mediana o distribución es visiblemente diferente entre clases.

################################################################################
# 6. CORRELACIÓN ENTRE VARIABLES NUMÉRICAS                                     #
################################################################################

cat("Calculando correlaciones...\n")

cor_matrix <- train_data %>%
  select(where(is.numeric)) %>%
  cor(use = "pairwise.complete.obs")

png("views/figures/correlacion_numericas.png", width = 800, height = 600)
corrplot(cor_matrix, method = "color", tl.cex = 0.8)
dev.off()

# Decisión esperada: si hay variables altamente correlacionadas, se puede considerar eliminar una de ellas para reducir multicolinealidad.

################################################################################
# FIN DEL SCRIPT                                                               #
################################################################################

cat("Análisis exploratorio finalizado. Resultados guardados en 'views/tables/' y 'views/figures/'\n")
