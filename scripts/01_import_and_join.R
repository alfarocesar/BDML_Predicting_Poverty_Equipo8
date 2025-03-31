################################################################################
# TÍTULO: 01_import_and_join.R                                                #
# PROYECTO: Predicción de Pobreza en Colombia                                 #
# DESCRIPCIÓN: Script para la importación de datos y unión de bases de datos  #
#              de hogares e individuos.                                       #
# FECHA: 30 de marzo de 2025                                                  #
################################################################################

# Configurar directorio de trabajo automáticamente
if (!require(rstudioapi)) install.packages("rstudioapi")
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# Subir un nivel directorio para acceder a la estructura principal del proyecto
setwd("../")

# Cargar librerías necesarias
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse) # Paquete para manipulación de datos

# Fijar semilla para reproducibilidad
set.seed(123)

################################################################################
# 1. IMPORTACIÓN DE DATOS                                                      #
################################################################################

# Importar datos de hogares y personas (tanto de entrenamiento como de prueba)
cat("Importando datos...\n")

train_hogares <- read.csv("stores/raw/train_hogares.csv")
train_personas <- read.csv("stores/raw/train_personas.csv")
test_hogares <- read.csv("stores/raw/test_hogares.csv")
test_personas <- read.csv("stores/raw/test_personas.csv")

# Asegurar que id sea tratado como character en todos los datasets
train_hogares$id <- as.character(train_hogares$id)
train_personas$id <- as.character(train_personas$id)
test_hogares$id <- as.character(test_hogares$id)
test_personas$id <- as.character(test_personas$id)

# Verificar dimensiones de los datos
cat("Dimensiones de train_hogares:", dim(train_hogares), "\n")
cat("Dimensiones de train_personas:", dim(train_personas), "\n")
cat("Dimensiones de test_hogares:", dim(test_hogares), "\n")
cat("Dimensiones de test_personas:", dim(test_personas), "\n")

################################################################################
# 2. VERIFICACIÓN DE LA VARIABLE OBJETIVO (POBREZA)                           #
################################################################################

# Según el DANE, un hogar es pobre si el ingreso per cápita es menor a la línea de pobreza
# Verificamos si la variable 'Pobre' incluida en los datos sigue esta definición
cat("Verificando la definición de pobreza...\n")

train_hogares <- train_hogares %>% 
  mutate(Pobre_calculado = ifelse(Ingpcug < Lp, 1, 0))

# Comparar nuestra variable calculada con la original
cat("Comparación entre variable Pobre original y calculada:\n")
print(table(train_hogares$Pobre, train_hogares$Pobre_calculado))

# Verificamos también con un método alternativo
train_hogares <- train_hogares %>% 
  mutate(Pobre_calculado2 = ifelse(Ingtotugarr < Lp * Npersug, 1, 0))

cat("Comparación con método alternativo (Ingreso total < LP * Num. personas):\n")
print(table(train_hogares$Pobre, train_hogares$Pobre_calculado2))

cat("Confirmado: La variable 'Pobre' está correctamente definida según el criterio del DANE.\n")

################################################################################
# 3. PREPROCESAMIENTO BÁSICO DE LOS DATOS A NIVEL INDIVIDUAL                  #
################################################################################

# Realizar un preprocesamiento mínimo para facilitar la unión de tablas
# No realizamos transformaciones profundas aquí - eso se hará después del EDA
cat("Realizando preprocesamiento básico de datos individuales...\n")

# Función para procesar datos de personas (solo preprocesamiento básico)
preprocess_personas_basico <- function(data) {
  data <- data %>% 
    mutate(
      id = as.character(id),               # Asegurar que id sea character
      mujer = ifelse(P6020 == 2, 1, 0),    # Indicador de mujer (1=mujer, 0=hombre)
      H_Head = ifelse(P6050 == 1, 1, 0),   # Indicador de jefe de hogar
      menor = ifelse(P6040 <= 18, 1, 0),   # Indicador de menor de edad
      edad = P6040,                        # Renombrar edad para claridad
      ocupado = ifelse(is.na(Oc), 0, Oc)   # Indicador de ocupación
    ) %>%
    # Seleccionamos solo las variables básicas necesarias
    select(id, Orden, mujer, H_Head, menor, edad, ocupado)
}

# Aplicar el preprocesamiento básico
train_personas_proc <- preprocess_personas_basico(train_personas)
test_personas_proc <- preprocess_personas_basico(test_personas)

################################################################################
# 4. AGREGACIÓN BÁSICA A NIVEL DE HOGAR                                       #
################################################################################

cat("Generando características básicas a nivel de hogar...\n")

# Agregación simple de variables individuales a nivel hogar
train_hogar_agregados <- train_personas_proc %>%
  group_by(id) %>%
  summarize(
    n_miembros = n(),                          # Número de miembros
    n_mujeres = sum(mujer, na.rm = TRUE),      # Número de mujeres
    n_menores = sum(menor, na.rm = TRUE),      # Número de menores de edad
    n_ocupados = sum(ocupado, na.rm = TRUE),   # Número de ocupados
    edad_promedio = mean(edad, na.rm = TRUE)   # Edad promedio
  )

# Lo mismo para datos de prueba
test_hogar_agregados <- test_personas_proc %>%
  group_by(id) %>%
  summarize(
    n_miembros = n(),
    n_mujeres = sum(mujer, na.rm = TRUE),
    n_menores = sum(menor, na.rm = TRUE),
    n_ocupados = sum(ocupado, na.rm = TRUE),
    edad_promedio = mean(edad, na.rm = TRUE)
  )

# Características básicas del jefe de hogar
train_hogar_jefe <- train_personas_proc %>%
  filter(H_Head == 1) %>%
  select(id, mujer, edad) %>%
  rename(jefe_mujer = mujer,
         jefe_edad = edad)

test_hogar_jefe <- test_personas_proc %>%
  filter(H_Head == 1) %>%
  select(id, mujer, edad) %>%
  rename(jefe_mujer = mujer,
         jefe_edad = edad)

################################################################################
# 5. SELECCIÓN DE VARIABLES BÁSICAS DE HOGARES                                #
################################################################################

cat("Seleccionando variables básicas de hogares...\n")

# Seleccionar variables básicas para el conjunto de entrenamiento
train_hogares_basic <- train_hogares %>%
  # Asegurar que id sea character
  mutate(id = as.character(id)) %>%
  # Seleccionamos un conjunto mínimo de variables de interés
  select(id, Dominio, P5000, P5010, P5090, P5130, P5140, Nper, Npersug, 
         Ingtotugarr, Ingpcug, Lp, Pobre) 

# Seleccionar variables básicas para el conjunto de prueba
test_hogares_basic <- test_hogares %>%
  # Asegurar que id sea character
  mutate(id = as.character(id)) %>%
  # Seleccionamos las mismas variables (excepto Pobre que no está en test)
  select(id, Dominio, P5000, P5010, P5090, P5130, P5140, Nper, Npersug, Lp)

################################################################################
# 6. UNIÓN DE DATOS DE HOGARES CON AGREGADOS DE PERSONAS                      #
################################################################################

cat("Uniendo conjuntos de datos...\n")

# Unir para entrenamiento (left_join para mantener todos los hogares)
train_joined <- train_hogares_basic %>%
  left_join(train_hogar_agregados, by = "id") %>%
  left_join(train_hogar_jefe, by = "id") %>%
  # Asegurar que id esté como primera columna y sea character
  select(id, everything()) %>%
  mutate(id = as.character(id))

# Unir para prueba
test_joined <- test_hogares_basic %>%
  left_join(test_hogar_agregados, by = "id") %>%
  left_join(test_hogar_jefe, by = "id") %>%
  # Asegurar que id esté como primera columna y sea character
  select(id, everything()) %>%
  mutate(id = as.character(id))

# Dimensiones finales
cat("Dimensiones del conjunto de entrenamiento unido:", dim(train_joined), "\n")
cat("Dimensiones del conjunto de prueba unido:", dim(test_joined), "\n")

################################################################################
# 7. VERIFICACIÓN DE VALORES FALTANTES (SOLO REPORTE, NO IMPUTACIÓN)          #
################################################################################

cat("Verificando valores faltantes (no se realiza imputación en esta etapa)...\n")

# Verificar cantidad de valores faltantes en cada variable
train_missing <- colSums(is.na(train_joined))
test_missing <- colSums(is.na(test_joined))

# Calcular porcentaje de valores faltantes
train_missing_pct <- round(train_missing / nrow(train_joined) * 100, 2)
test_missing_pct <- round(test_missing / nrow(test_joined) * 100, 2)

# Mostrar variables con valores faltantes
cat("Variables con valores faltantes en el conjunto de entrenamiento:\n")
print(train_missing_pct[train_missing_pct > 0])

cat("Variables con valores faltantes en el conjunto de prueba:\n")
print(test_missing_pct[test_missing_pct > 0])

cat("Nota: La imputación de valores faltantes se realizará después del análisis exploratorio.\n")

################################################################################
# 8. GUARDAR DATOS UNIDOS                                                     #
################################################################################

cat("Guardando datos unidos para análisis exploratorio...\n")

# Guardar conjuntos de datos unidos (sin imputación)
write.csv(train_joined, "stores/processed/train_joined.csv", row.names = FALSE)
write.csv(test_joined, "stores/processed/test_joined.csv", row.names = FALSE)

cat("Proceso de importación y unión completado con éxito.\n")
cat("Archivos guardados en 'stores/processed/':\n")
cat("  - train_joined.csv\n")
cat("  - test_joined.csv\n")
cat("\nEstos archivos serán utilizados en el análisis exploratorio (02_exploratory_analysis.R).\n")

################################################################################
#                             FIN DEL SCRIPT                                   #
################################################################################