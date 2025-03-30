################################################################################
# TÍTULO: 01_data_preprocessing.R                                             #
# PROYECTO: Predicción de Pobreza en Colombia                                 #
# DESCRIPCIÓN: Script para el preprocesamiento de datos, unión de bases       #
#              y preparación de variables para clasificación                  #
# FECHA: 30 de marzo de 2025                                                  #
################################################################################

# Configurar directorio de trabajo automáticamente
if (!require(rstudioapi)) install.packages("rstudioapi")  
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# Subir un nivel directorio para acceder a la estructura principal del proyecto
setwd("../")

# Cargar librerías necesarias
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse) # Para manipulación de datos

# Fijar semilla para reproducibilidad
set.seed(123)

################################################################################
# 1. IMPORTACIÓN DE DATOS                                                      #
################################################################################

# Importar datos de hogares y personas (tanto de entrenamiento como de prueba)
train_hogares <- read.csv("stores/raw/train_hogares.csv")
train_personas <- read.csv("stores/raw/train_personas.csv")
test_hogares <- read.csv("stores/raw/test_hogares.csv")
test_personas <- read.csv("stores/raw/test_personas.csv")

# Revisar dimensiones de los datos
cat("Dimensiones de train_hogares:", dim(train_hogares), "\n")
cat("Dimensiones de train_personas:", dim(train_personas), "\n")
cat("Dimensiones de test_hogares:", dim(test_hogares), "\n")
cat("Dimensiones de test_personas:", dim(test_personas), "\n")

################################################################################
# 2. VERIFICACIÓN DE LA VARIABLE OBJETIVO (POBREZA)                           #
################################################################################

# Según el DANE, un hogar es pobre si el ingreso per cápita es menor a la línea de pobreza
# Verificamos si la variable 'Pobre' incluida en los datos sigue esta definición
train_hogares <- train_hogares %>% 
  mutate(Pobre_calculado = ifelse(Ingpcug < Lp, 1, 0))

# Comparar nuestra variable calculada con la original
cat("Comparación entre variable Pobre original y calculada:\n")
print(table(train_hogares$Pobre, train_hogares$Pobre_calculado))

# Confirmamos que la variable 'Pobre' está correctamente definida en los datos originales
# Esto es importante para el problema de clasificación

################################################################################
# 3. PREPROCESAMIENTO DE DATOS A NIVEL DE PERSONAS                            #
################################################################################

# Función para realizar el mismo preprocesamiento tanto en train como en test
pre_process_personas <- function(data) {
  data <- data %>% 
    mutate(
      mujer = ifelse(P6020 == 2, 1, 0),                   # Indicador de mujer
      H_Head = ifelse(P6050 == 1, 1, 0),                  # Jefe de hogar
      menor = ifelse(P6040 <= 6, 1, 0),                   # Indicador de menores de 6 años
      edad_grupos = cut(P6040, 
                        breaks = c(0, 5, 12, 18, 30, 45, 60, 100),
                        labels = c("0a5", "6a12", "13a18", "19a30", "31a45", "46a60", "61mas")),
      EducLevel = case_when(
        is.na(P6210) ~ 0,                                # NA -> 0
        P6210 == 9 ~ 0,                                  # No sabe/No responde -> 0
        TRUE ~ P6210                                     # Resto mantiene su valor
      ),
      ocupado = ifelse(is.na(Oc), 0, Oc),                 # Indicador de ocupación
      desocupado = ifelse(is.na(Des), 0, Des),            # Indicador de desocupación
      inactivo = ifelse(is.na(Ina), 0, Ina)               # Indicador de inactividad
    ) %>%
    select(id, Orden, mujer, H_Head, menor, edad_grupos, P6040, EducLevel, 
           ocupado, desocupado, inactivo)
}

# Aplicar preprocesamiento
train_personas_proc <- pre_process_personas(train_personas)
test_personas_proc <- pre_process_personas(test_personas)

################################################################################
# 4. CREAR VARIABLES A NIVEL DE HOGAR A PARTIR DE DATOS DE PERSONAS           #
################################################################################

# Primero para datos de entrenamiento
# Agregación de variables numéricas por hogar
train_hogar_agregados <- train_personas_proc %>%
  group_by(id) %>%
  summarize(
    n_miembros = n(),                           # Número de miembros del hogar
    n_mujeres = sum(mujer, na.rm = TRUE),       # Número de mujeres
    prop_mujeres = n_mujeres / n_miembros,      # Proporción de mujeres
    n_menores = sum(menor, na.rm = TRUE),       # Número de menores de 6 años
    prop_menores = n_menores / n_miembros,      # Proporción de menores
    n_ocupados = sum(ocupado, na.rm = TRUE),    # Número de ocupados
    n_desocupados = sum(desocupado, na.rm = TRUE), # Número de desocupados
    n_inactivos = sum(inactivo, na.rm = TRUE),  # Número de inactivos
    edad_promedio = mean(P6040, na.rm = TRUE),  # Edad promedio
    max_educacion = max(EducLevel, na.rm = TRUE), # Máximo nivel educativo
    # Variables útiles para clasificación 
    tasa_dependencia = (n_miembros - n_ocupados) / 
      (n_ocupados + 0.01),      # Tasa de dependencia
    tasa_ocupacion = n_ocupados / n_miembros,   # Tasa de ocupación en el hogar
    tasa_desempleo = n_desocupados / 
      (n_ocupados + n_desocupados + 0.01) # Tasa de desempleo
  )

# Lo mismo para datos de prueba
test_hogar_agregados <- test_personas_proc %>%
  group_by(id) %>%
  summarize(
    n_miembros = n(),
    n_mujeres = sum(mujer, na.rm = TRUE),
    prop_mujeres = n_mujeres / n_miembros,
    n_menores = sum(menor, na.rm = TRUE),
    prop_menores = n_menores / n_miembros,
    n_ocupados = sum(ocupado, na.rm = TRUE),
    n_desocupados = sum(desocupado, na.rm = TRUE),
    n_inactivos = sum(inactivo, na.rm = TRUE),
    edad_promedio = mean(P6040, na.rm = TRUE),
    max_educacion = max(EducLevel, na.rm = TRUE),
    tasa_dependencia = (n_miembros - n_ocupados) / 
      (n_ocupados + 0.01),
    tasa_ocupacion = n_ocupados / n_miembros,
    tasa_desempleo = n_desocupados / 
      (n_ocupados + n_desocupados + 0.01)
  )

# Crear características específicas del jefe de hogar
train_hogar_jefe <- train_personas_proc %>%
  filter(H_Head == 1) %>%
  select(id, mujer, P6040, EducLevel, ocupado) %>%
  rename(jefe_mujer = mujer,
         jefe_edad = P6040,
         jefe_educacion = EducLevel,
         jefe_ocupado = ocupado)

test_hogar_jefe <- test_personas_proc %>%
  filter(H_Head == 1) %>%
  select(id, mujer, P6040, EducLevel, ocupado) %>%
  rename(jefe_mujer = mujer,
         jefe_edad = P6040,
         jefe_educacion = EducLevel,
         jefe_ocupado = ocupado)

################################################################################
# 5. SELECCIÓN Y PROCESAMIENTO DE VARIABLES DE HOGARES                        #
################################################################################

# Seleccionar y procesar variables relevantes de hogares para entrenamiento
train_hogares_proc <- train_hogares %>%
  # Crear variables adicionales
  mutate(
    tipo_vivienda = case_when(
      P5000 == 1 ~ "Casa",
      P5000 == 2 ~ "Apartamento",
      P5000 == 3 ~ "Cuarto",
      P5000 == 4 ~ "Otro",
      TRUE ~ "Desconocido"
    ),
    regimen_vivienda = case_when(
      P5090 == 1 ~ "Propia_pagada",
      P5090 == 2 ~ "Propia_pagando",
      P5090 == 3 ~ "Arriendo",
      P5090 == 4 ~ "Usufructo",
      P5090 == 5 ~ "Otra",
      TRUE ~ "Desconocido"
    ),
    arrienda = ifelse(P5090 == 3, 1, 0),        # Indicador de arriendo
    densidad = Nper / P5010,                     # Densidad: personas por cuartos
    pobre = Pobre                                # Variable objetivo
  ) %>%
  # Seleccionar variables relevantes
  select(id, Dominio, tipo_vivienda, regimen_vivienda, P5010, arrienda, densidad, pobre)

# Para el conjunto de prueba
test_hogares_proc <- test_hogares %>%
  # Crear variables adicionales (mismo procesamiento que en train)
  mutate(
    tipo_vivienda = case_when(
      P5000 == 1 ~ "Casa",
      P5000 == 2 ~ "Apartamento",
      P5000 == 3 ~ "Cuarto",
      P5000 == 4 ~ "Otro",
      TRUE ~ "Desconocido"
    ),
    regimen_vivienda = case_when(
      P5090 == 1 ~ "Propia_pagada",
      P5090 == 2 ~ "Propia_pagando",
      P5090 == 3 ~ "Arriendo",
      P5090 == 4 ~ "Usufructo",
      P5090 == 5 ~ "Otra",
      TRUE ~ "Desconocido"
    ),
    arrienda = ifelse(P5090 == 3, 1, 0),
    densidad = Nper / P5010
  ) %>%
  # Seleccionar variables relevantes
  select(id, Dominio, tipo_vivienda, regimen_vivienda, P5010, arrienda, densidad)

################################################################################
# 6. UNIR DATOS DE HOGARES CON AGREGADOS DE PERSONAS                          #
################################################################################

# Unir para entrenamiento
train_final <- train_hogares_proc %>%
  left_join(train_hogar_agregados, by = "id") %>%
  left_join(train_hogar_jefe, by = "id")

# Unir para prueba
test_final <- test_hogares_proc %>%
  left_join(test_hogar_agregados, by = "id") %>%
  left_join(test_hogar_jefe, by = "id")

# Resumen de datos procesados
cat("\nDimensiones del conjunto de entrenamiento final:", dim(train_final), "\n")
cat("Dimensiones del conjunto de prueba final:", dim(test_final), "\n")

################################################################################
# 7. MANEJO DE VALORES FALTANTES                                              #
################################################################################

# Verificar valores faltantes en train_final
missing_train <- sapply(train_final, function(x) sum(is.na(x)))
missing_train_pct <- round(missing_train / nrow(train_final) * 100, 2)

# Verificar valores faltantes en test_final
missing_test <- sapply(test_final, function(x) sum(is.na(x)))
missing_test_pct <- round(missing_test / nrow(test_final) * 100, 2)

# Ver variables con valores faltantes
cat("\nPorcentaje de valores faltantes en train:\n")
print(missing_train_pct[missing_train_pct > 0])

cat("\nPorcentaje de valores faltantes en test:\n")
print(missing_test_pct[missing_test_pct > 0])

# Imputar valores faltantes
# 1. Para variables numéricas: usar la mediana
# 2. Para variables categóricas: usar la moda

# Función para imputar valores faltantes
impute_missing <- function(data) {
  # Imputación de variables numéricas
  numeric_vars <- sapply(data, is.numeric)
  for (var in names(data)[numeric_vars]) {
    if (sum(is.na(data[[var]])) > 0) {
      median_value <- median(data[[var]], na.rm = TRUE)
      data[[var]][is.na(data[[var]])] <- median_value
    }
  }
  
  # Imputación de variables categóricas (character o factor)
  character_vars <- sapply(data, is.character)
  factor_vars <- sapply(data, is.factor)
  cat_vars <- character_vars | factor_vars
  
  for (var in names(data)[cat_vars]) {
    if (sum(is.na(data[[var]])) > 0) {
      mode_value <- names(sort(table(data[[var]]), decreasing = TRUE))[1]
      data[[var]][is.na(data[[var]])] <- mode_value
    }
  }
  
  return(data)
}

# Aplicar imputación
train_final_imputed <- impute_missing(train_final)
test_final_imputed <- impute_missing(test_final)

# Verificar que no queden valores faltantes
cat("\nValores faltantes después de imputación en train:", sum(is.na(train_final_imputed)), "\n")
cat("Valores faltantes después de imputación en test:", sum(is.na(test_final_imputed)), "\n")

################################################################################
# 8. CONVERTIR VARIABLES CATEGÓRICAS A FACTORES                              #
################################################################################

# Identificar variables categóricas (character)
char_vars_train <- names(train_final_imputed)[sapply(train_final_imputed, is.character)]
char_vars_test <- names(test_final_imputed)[sapply(test_final_imputed, is.character)]

# Convertir variables character a factores
train_final_imputed <- train_final_imputed %>%
  mutate(across(all_of(char_vars_train), as.factor))

test_final_imputed <- test_final_imputed %>%
  mutate(across(all_of(char_vars_test), as.factor))

# Convertir la variable objetivo a factor con etiquetas adecuadas para clasificación
train_final_imputed <- train_final_imputed %>%
  mutate(pobre = factor(pobre, levels = c(0, 1), labels = c("No", "Yes")))

################################################################################
# 9. GUARDAR DATOS PROCESADOS                                                 #
################################################################################

# Crear directorios si no existen
if (!dir.exists("stores/processed")) {
  dir.create("stores/processed", recursive = TRUE)
}

# Guardar datos procesados
write.csv(train_final_imputed, "stores/processed/train_processed.csv", row.names = FALSE)
write.csv(test_final_imputed, "stores/processed/test_processed.csv", row.names = FALSE)

cat("\nPreprocesamiento completado. Datos guardados en 'stores/processed/'")