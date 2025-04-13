################################################################################
# SCRIPT RÁPIDO: Limpieza preliminar                                          #
# OBJETIVO: Eliminar variables con >33% NA y imputar medianas a otras         #
################################################################################

# Configurar directorio de trabajo automáticamente
if (!require(rstudioapi)) install.packages("rstudioapi")
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# Subir un nivel directorio para acceder a la estructura principal del proyecto
setwd("../")

# Cargar librerías
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,  # Manipulación de datos
  skimr       # Resumen de datos
)

# Fijar semilla
set.seed(123)

# ------------------------------------------------------------------------------
# 1. IMPORTAR BASES
# ------------------------------------------------------------------------------
cat("Importando bases...\n")
train <- read.csv("stores/processed/train_joined.csv")
test <- read.csv("stores/processed/test_joined.csv")

# ------------------------------------------------------------------------------
# 2. VARIABLES A ELIMINAR POR >33% NA
# ------------------------------------------------------------------------------
vars_na_33 <- c(
  "jefe_tiempo_trabajo", "prop_P6510", "prop_P6545", "prop_P6580", "prop_P6585s1",
  "prop_P6585s2", "prop_P6585s3", "prop_P6585s4", "prop_P6590", "prop_P6600",
  "prop_P6610", "prop_P6620", "prop_P6630s1", "prop_P6630s2", "prop_P6630s3",
  "prop_P6630s4", "prop_P6630s6", "prop_P7472", "prop_P7110", "prop_P7120",
  "prop_P7150", "prop_P7160", "prop_P7310", "prop_P7422", "prop_P7500s2",
  "prop_P7500s3", "prop_P7510s1", "prop_P7510s2", "prop_P7510s3", "prop_P7510s5",
  "prop_P7510s6", "prop_P7510s7"
)

train <- train %>% select(-any_of(vars_na_33))
test  <- test %>% select(-any_of(vars_na_33))

# ------------------------------------------------------------------------------
# 3. IMPUTACIÓN DE MEDIANAS
# ------------------------------------------------------------------------------
vars_imputar <- c(
  "promedio_horas_trab", "prop_cotiza_pension",
  "prop_actividad_adicional", "prop_desea_mas_horas"
)

for (var in vars_imputar) {
  if (var %in% names(train)) {
    mediana_train <- median(train[[var]], na.rm = TRUE)
    train[[var]][is.na(train[[var]])] <- mediana_train
  }
  if (var %in% names(test)) {
    mediana_test <- median(test[[var]], na.rm = TRUE)
    test[[var]][is.na(test[[var]])] <- mediana_test
  }
}

# ------------------------------------------------------------------------------
# 3B. IMPUTACIÓN ADICIONAL DE VARIABLES CON NA DETECTADAS POSTERIORMENTE
# ------------------------------------------------------------------------------
# Moda para max_años_educ
if ("max_años_educ" %in% names(train)) {
  moda_train <- as.numeric(names(sort(table(train$max_años_educ), decreasing = TRUE))[1])
  train$max_años_educ[is.na(train$max_años_educ)] <- moda_train
}
if ("max_años_educ" %in% names(test)) {
  moda_test <- as.numeric(names(sort(table(test$max_años_educ), decreasing = TRUE))[1])
  test$max_años_educ[is.na(test$max_años_educ)] <- moda_test
}

# Mediana para prop_afiliados_ss, prop_busca_trabajo, jefe_años_educ
otras_vars <- c("prop_afiliados_ss", "prop_busca_trabajo", "jefe_años_educ")
for (var in otras_vars) {
  if (var %in% names(train)) {
    mediana_train <- median(train[[var]], na.rm = TRUE)
    train[[var]][is.na(train[[var]])] <- mediana_train
  }
  if (var %in% names(test)) {
    mediana_test <- median(test[[var]], na.rm = TRUE)
    test[[var]][is.na(test[[var]])] <- mediana_test
  }
}

# ------------------------------------------------------------------------------
# 4. GUARDAR NUEVAS BASES
# ------------------------------------------------------------------------------
cat("Guardando nuevas versiones de las bases...\n")
write.csv(train, "stores/processed/train_cleaned.csv", row.names = FALSE)
write.csv(test, "stores/processed/test_cleaned.csv", row.names = FALSE)
cat("Listo.\n")
