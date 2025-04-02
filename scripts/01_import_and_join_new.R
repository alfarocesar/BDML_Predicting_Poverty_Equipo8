################################################################################
# TÍTULO: 01_import_and_join.R                                                #
# PROYECTO: Predicción de Pobreza en Colombia                                 #
# DESCRIPCIÓN: Script para la importación de datos y unión de bases de datos  #
#              de hogares e individuos.                                       #
# FECHA: 2 de abril de 2025                                                   #
################################################################################

# Configurar directorio de trabajo automáticamente
if (!require(rstudioapi)) install.packages("rstudioapi")
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# Subir un nivel directorio para acceder a la estructura principal del proyecto
setwd("../")

# Cargar librerías necesarias
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,  # Manipulación de datos
  skimr       # Resumen de datos
)

# Fijar semilla para reproducibilidad
set.seed(123)

################################################################################
# 1. IMPORTACIÓN DE DATOS                                                      #
################################################################################

cat("Importando datos...\n")

# Importar datos de hogares y personas (tanto de entrenamiento como de prueba)
train_hogares <- read.csv("stores/raw/train_hogares.csv")
train_personas <- read.csv("stores/raw/train_personas.csv")
test_hogares <- read.csv("stores/raw/test_hogares.csv")
test_personas <- read.csv("stores/raw/test_personas.csv")

# Mostrar información básica
cat("Dimensiones de train_hogares:", dim(train_hogares), "\n")
cat("Dimensiones de train_personas:", dim(train_personas), "\n")
cat("Dimensiones de test_hogares:", dim(test_hogares), "\n")
cat("Dimensiones de test_personas:", dim(test_personas), "\n")

################################################################################
# 2. SELECCIÓN DE VARIABLES A NIVEL DE HOGAR                                  #
################################################################################

cat("Seleccionando variables de hogares...\n")

# Variables de la base de hogares a conservar (DECISIÓN MANUAL)
vars_hogares_train <- c(
  "id",          # Identificador del hogar
  "Dominio",     # Área geográfica
  "P5000",       # Tipo de vivienda
  "P5010",       # Material predominante en paredes
  "P5090",       # Tenencia de la vivienda
  "Nper",        # Número de personas en el hogar
  "Npersug",     # Número de personas en la unidad de gasto
  "Lp",          # Línea de pobreza
  "Ingpcug",     # Ingreso per cápita de la unidad de gasto
  "Pobre"        # Indicador de pobreza (objetivo)
)

# Variables de la base de hogares a conservar para test (similar pero sin Pobre e Ingpcug)
vars_hogares_test <- c(
  "id",          # Identificador del hogar
  "Dominio",     # Área geográfica
  "P5000",       # Tipo de vivienda
  "P5010",       # Material predominante en paredes
  "P5090",       # Tenencia de la vivienda
  "Nper",        # Número de personas en el hogar
  "Npersug",     # Número de personas en la unidad de gasto
  "Lp"           # Línea de pobreza
)

# Seleccionar variables de hogares para entrenamiento y prueba
train_hogares_sel <- train_hogares %>% 
  select(all_of(vars_hogares_train))

test_hogares_sel <- test_hogares %>% 
  select(all_of(vars_hogares_test))

################################################################################
# 3. PREPROCESAMIENTO BÁSICO DE LOS DATOS A NIVEL INDIVIDUAL                  #
################################################################################

cat("Realizando preprocesamiento básico de datos individuales...\n")

# Variables de personas a conservar para transformación inicial (DECISIÓN MANUAL)
vars_personas <- c(
  "id",          # Identificador del hogar
  "Orden",       # Identificador de la persona dentro del hogar
  "P6020",       # Sexo (1=hombre, 2=mujer)
  "P6040",       # Edad
  "P6050",       # Relación con el jefe del hogar (1=jefe)
  "P6210",       # Nivel educativo
  "Oc"           # Ocupado (1=ocupado)
)

# Seleccionar variables de personas para entrenamiento y prueba
train_personas_sel <- train_personas %>% 
  select(all_of(vars_personas))

test_personas_sel <- test_personas %>% 
  select(all_of(vars_personas))

# Función para realizar transformaciones básicas a nivel individual
preprocess_personas <- function(data) {
  data %>% 
    mutate(
      # Transformar variables categóricas a binarias para facilitar agregación
      mujer = ifelse(P6020 == 2, 1, 0),                 # 1=mujer, 0=hombre
      H_Head = ifelse(P6050 == 1, 1, 0),                # 1=jefe de hogar, 0=otro
      menor = ifelse(P6040 <= 18, 1, 0),                # 1=menor de edad, 0=adulto
      ocupado = ifelse(is.na(Oc), 0, as.integer(Oc)),   # 1=ocupado, 0=no ocupado
      
      # Transformar nivel educativo a variable numérica
      nivel_educativo = case_when(
        P6210 == 1 ~ 0,  # Ninguno
        P6210 == 2 ~ 1,  # Preescolar
        P6210 == 3 ~ 2,  # Primaria incompleta
        P6210 == 4 ~ 3,  # Primaria completa
        P6210 == 5 ~ 4,  # Secundaria incompleta
        P6210 == 6 ~ 5,  # Secundaria completa
        P6210 == 7 ~ 6,  # Superior incompleta
        P6210 == 8 ~ 7,  # Superior completa
        P6210 == 9 ~ NA_real_,  # No sabe/no informa
        is.na(P6210) ~ NA_real_,
        TRUE ~ NA_real_
      )
    )
}

# Aplicar preprocesamiento
train_personas_proc <- train_personas_sel %>% 
  preprocess_personas()

test_personas_proc <- test_personas_sel %>% 
  preprocess_personas()

################################################################################
# 4. AGREGACIÓN DE VARIABLES A NIVEL DE HOGAR                                 #
################################################################################

cat("Generando variables agregadas a nivel de hogar...\n")

# 4.1 Agregación general por hogar
# ----------------------------------

# Definir agregaciones a nivel de hogar (DECISIÓN MANUAL)
train_hogar_agregado <- train_personas_proc %>%
  group_by(id) %>%
  summarize(
    # Conteos básicos
    n_miembros = n(),                          # Número de miembros
    n_mujeres = sum(mujer, na.rm = TRUE),      # Número de mujeres
    n_menores = sum(menor, na.rm = TRUE),      # Número de menores
    n_ocupados = sum(ocupado, na.rm = TRUE),   # Número de ocupados
    
    # Promedios
    edad_promedio = mean(P6040, na.rm = TRUE),  # Edad promedio
    
    # Niveles máximos
    max_nivel_educativo = max(nivel_educativo, na.rm = TRUE)  # Máximo nivel educativo
  ) %>%
  # Reemplazar Inf por NA en caso de divisiones por cero
  mutate(
    max_nivel_educativo = ifelse(is.infinite(max_nivel_educativo), NA, max_nivel_educativo)
  )

# Lo mismo para test
test_hogar_agregado <- test_personas_proc %>%
  group_by(id) %>%
  summarize(
    # Conteos básicos
    n_miembros = n(),                          # Número de miembros
    n_mujeres = sum(mujer, na.rm = TRUE),      # Número de mujeres
    n_menores = sum(menor, na.rm = TRUE),      # Número de menores
    n_ocupados = sum(ocupado, na.rm = TRUE),   # Número de ocupados
    
    # Promedios
    edad_promedio = mean(P6040, na.rm = TRUE),  # Edad promedio
    
    # Niveles máximos
    max_nivel_educativo = max(nivel_educativo, na.rm = TRUE)  # Máximo nivel educativo
  ) %>%
  # Reemplazar Inf por NA en caso de divisiones por cero
  mutate(
    max_nivel_educativo = ifelse(is.infinite(max_nivel_educativo), NA, max_nivel_educativo)
  )

# 4.2 Características del jefe de hogar
# ---------------------------------------

# Variables específicas del jefe de hogar (DECISIÓN MANUAL)
train_jefe_hogar <- train_personas_proc %>%
  filter(H_Head == 1) %>%
  select(id, mujer, P6040, nivel_educativo, ocupado) %>%
  rename(
    jefe_mujer = mujer,
    jefe_edad = P6040,
    jefe_nivel_educativo = nivel_educativo,
    jefe_ocupado = ocupado
  )

# Lo mismo para test
test_jefe_hogar <- test_personas_proc %>%
  filter(H_Head == 1) %>%
  select(id, mujer, P6040, nivel_educativo, ocupado) %>%
  rename(
    jefe_mujer = mujer,
    jefe_edad = P6040,
    jefe_nivel_educativo = nivel_educativo,
    jefe_ocupado = ocupado
  )

# 4.3 Proporciones adicionales (DECISIÓN MANUAL)
# ----------------------------------------------

train_hogar_proporciones <- train_personas_proc %>%
  group_by(id) %>%
  summarize(
    prop_mujeres = sum(mujer, na.rm = TRUE) / n(),
    prop_menores = sum(menor, na.rm = TRUE) / n(),
    prop_ocupados = sum(ocupado, na.rm = TRUE) / sum(!is.na(Oc))
  ) %>%
  # Reemplazar NaN por NA en caso de divisiones por cero
  mutate(
    across(everything(), ~ifelse(is.nan(.), NA, .))
  )

test_hogar_proporciones <- test_personas_proc %>%
  group_by(id) %>%
  summarize(
    prop_mujeres = sum(mujer, na.rm = TRUE) / n(),
    prop_menores = sum(menor, na.rm = TRUE) / n(),
    prop_ocupados = sum(ocupado, na.rm = TRUE) / sum(!is.na(Oc))
  ) %>%
  # Reemplazar NaN por NA en caso de divisiones por cero
  mutate(
    across(everything(), ~ifelse(is.nan(.), NA, .))
  )

################################################################################
# 5. UNIÓN DE DATOS                                                           #
################################################################################

cat("Uniendo datos...\n")

# 5.1 Unir para entrenamiento (usando left_join para mantener todos los hogares)
train_final <- train_hogares_sel %>%
  left_join(train_hogar_agregado, by = "id") %>%
  left_join(train_jefe_hogar, by = "id") %>%
  left_join(train_hogar_proporciones, by = "id")

# 5.2 Unir para prueba
test_final <- test_hogares_sel %>%
  left_join(test_hogar_agregado, by = "id") %>%
  left_join(test_jefe_hogar, by = "id") %>%
  left_join(test_hogar_proporciones, by = "id")

# 5.3 Verificar dimensiones finales
cat("Dimensiones del conjunto de entrenamiento unido:", dim(train_final), "\n")
cat("Dimensiones del conjunto de prueba unido:", dim(test_final), "\n")

# 5.4 Verificar que todos los hogares tienen datos agregados
cat("Hogares en train sin datos agregados:", 
    sum(is.na(train_final$n_miembros)), "/", nrow(train_final), "\n")
cat("Hogares en test sin datos agregados:",
    sum(is.na(test_final$n_miembros)), "/", nrow(test_final), "\n")

################################################################################
# 6. VERIFICACIÓN DE VALORES FALTANTES                                         #
################################################################################

cat("Verificando valores faltantes...\n")

# Calcular porcentaje de valores faltantes por variable
missing_train <- colSums(is.na(train_final)) / nrow(train_final) * 100
missing_train <- data.frame(
  variable = names(missing_train),
  porcentaje_faltantes = missing_train
) %>%
  arrange(desc(porcentaje_faltantes))

missing_test <- colSums(is.na(test_final)) / nrow(test_final) * 100
missing_test <- data.frame(
  variable = names(missing_test),
  porcentaje_faltantes = missing_test
) %>%
  arrange(desc(porcentaje_faltantes))

# Mostrar variables con valores faltantes
cat("Variables con valores faltantes en train (porcentaje):\n")
print(missing_train[missing_train$porcentaje_faltantes > 0, ])

cat("Variables con valores faltantes en test (porcentaje):\n")
print(missing_test[missing_test$porcentaje_faltantes > 0, ])

################################################################################
# 7. GUARDAR DATOS UNIDOS                                                     #
################################################################################

cat("Guardando datos unidos...\n")

# Crear directorio si no existe
if (!dir.exists("stores/processed")) {
  dir.create("stores/processed", recursive = TRUE)
}

# Guardar conjuntos de datos unidos (sin imputación)
write.csv(train_final, "stores/processed/train_joined.csv", row.names = FALSE)
write.csv(test_final, "stores/processed/test_joined.csv", row.names = FALSE)

# También guardar un archivo RDS que preserva tipos de datos
saveRDS(train_final, "stores/processed/train_joined.rds")
saveRDS(test_final, "stores/processed/test_joined.rds")

cat("Datos unidos guardados en 'stores/processed/'.\n")
cat("La imputación de valores faltantes y transformaciones adicionales\n")
cat("se realizarán después del análisis exploratorio en scripts posteriores.\n")

################################################################################
#                              FIN DEL SCRIPT                                  #
################################################################################