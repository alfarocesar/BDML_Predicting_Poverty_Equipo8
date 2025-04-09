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
# 2. ELIMINACIÓN DE VARIABLES SEGÚN INSTRUCCIONES                              #
################################################################################

cat("Eliminando variables especificadas...\n")

# 2.1 Variables a eliminar en train_hogares (no existen en test)
vars_eliminar_train <- c("Ingtotug", "Ingtotugarr", "Ingpcug", "Indigente", "Npobres", "Nindigentes")
vars_eliminar_train <- vars_eliminar_train[vars_eliminar_train %in% names(train_hogares)]
if(length(vars_eliminar_train) > 0) {
  train_hogares <- train_hogares %>%
    select(-all_of(vars_eliminar_train))
}

# 2.2 Variables a eliminar en train_hogares y test_hogares (no agregan valor)
vars_eliminar_ambas <- c("Secuencia_p", "Mes", "P5130", "Fex_c", "Fex_dpto") # "Directorio" no existe

# Para train_hogares
vars_eliminar_train2 <- vars_eliminar_ambas[vars_eliminar_ambas %in% names(train_hogares)]
if(length(vars_eliminar_train2) > 0) {
  train_hogares <- train_hogares %>%
    select(-all_of(vars_eliminar_train2))
}

# Para test_hogares
vars_eliminar_test <- vars_eliminar_ambas[vars_eliminar_ambas %in% names(test_hogares)]
if(length(vars_eliminar_test) > 0) {
  test_hogares <- test_hogares %>%
    select(-all_of(vars_eliminar_test))
}

# 2.3 Eliminar variables en bases de personas que no existen en test
# Lista de variables a eliminar en bases de personas (blacklist)
vars_eliminar_personas <- c(
  "Secuencia_p", "Mes", "Dominio", "P5130", "Fex_c", "Fex_dpto",
  "Estrato1", "P6500", "P6510s1", "P6510s2", "P6545s1", "P6545s2", 
  "P6580s1", "P6580s2", "P6585s1a1", "P6585s1a2", "P6585s2a1", "P6585s2a2", 
  "P6585s3a1", "P6585s3a2", "P6585s4a1", "P6585s4a2", "P6590s1", "P6600s1", 
  "P6610s1", "P6620s1", "P6630s1a1", "P6630s2a1", "P6630s3a1", "P6630s4a1", 
  "P6630s6a1", "P6750", "P6760", "P550", "P7070", "P7140s1", "P7140s2", 
  "P7422s1", "P7472s1", "P7500s1", "P7500s1a1", "P7500s2a1", "P7500s3a1", 
  "P7510s1a1", "P7510s2a1", "P7510s3a1", "P7510s5a1", "P7510s6a1", "P7510s7a1", 
  "Impa", "Isa", "Ie", "Imdi", "Iof1", "Iof2", "Iof3h", "Iof3i", "Iof6", 
  "Cclasnr2", "Cclasnr3", "Cclasnr4", "Cclasnr5", "Cclasnr6", "Cclasnr7", 
  "Cclasnr8", "Cclasnr11", "Impaes", "Isaes", "Iees", "Imdies", "Iof1es", 
  "Iof2es", "Iof3hes", "Iof3ies", "Iof6es", "Ingtotob", "Ingtotes", "Ingtot",
  "P7050" # Variable con >95% valores faltantes
)

# Eliminar variables en train_personas
vars_eliminar_train_personas <- vars_eliminar_personas[vars_eliminar_personas %in% names(train_personas)]
if(length(vars_eliminar_train_personas) > 0) {
  train_personas <- train_personas %>%
    select(-all_of(vars_eliminar_train_personas))
}

# Eliminar variables en test_personas
vars_eliminar_test_personas <- vars_eliminar_personas[vars_eliminar_personas %in% names(test_personas)]
if(length(vars_eliminar_test_personas) > 0) {
  test_personas <- test_personas %>%
    select(-all_of(vars_eliminar_test_personas))
}

# También eliminar la variable Orden según instrucciones
if("Orden" %in% names(train_personas)) {
  train_personas <- train_personas %>% select(-Orden)
}
if("Orden" %in% names(test_personas)) {
  test_personas <- test_personas %>% select(-Orden)
}

# Mostrar número de variables después de eliminar
cat("Variables restantes en train_hogares:", ncol(train_hogares), "\n")
cat("Variables restantes en test_hogares:", ncol(test_hogares), "\n")
cat("Variables restantes en train_personas:", ncol(train_personas), "\n")
cat("Variables restantes en test_personas:", ncol(test_personas), "\n")

################################################################################
# 3. IMPUTACIÓN DE VALORES ESPECÍFICOS                                         #
################################################################################

cat("Imputando valores específicos...\n")

# 3.1 Imputar 0 en lugar de NA para P5100 y P5140
train_hogares <- train_hogares %>%
  mutate(
    P5100 = ifelse(is.na(P5100), 0, P5100),
    P5140 = ifelse(is.na(P5140), 0, P5140)
  )

test_hogares <- test_hogares %>%
  mutate(
    P5100 = ifelse(is.na(P5100), 0, P5100),
    P5140 = ifelse(is.na(P5140), 0, P5140)
  )

################################################################################
# 4. PROCESAMIENTO DE DATOS DE PERSONAS                                       #
################################################################################

cat("Procesando datos de personas...\n")

# 4.1 Función para transformar variables binarias (NA a 0)
preprocess_binarias <- function(data) {
  data %>% 
    mutate(
      # Convertir NA a 0 en variables binarias
      Pet = ifelse(is.na(Pet), 0, Pet),
      Oc = ifelse(is.na(Oc), 0, Oc),
      Des = ifelse(is.na(Des), 0, Des),
      Ina = ifelse(is.na(Ina), 0, Ina)
    )
}

# 4.2 Función para crear las agrupaciones de la variable Oficio
categorizar_oficio <- function(data) {
  # Lista de oficios según la categorización proporcionada
  oficio_cat1 <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 
                   30, 31, 32, 33, 34, 35, 37, 39, 40, 42, 43, 44, 50, 51, 52, 58, 59, 60, 
                   70, 72, 88, 90, 91, 92, 96)
  
  oficio_cat2 <- c(36, 38, 41, 45, 49, 53, 54, 55, 57, 62, 74, 76, 77, 79, 80, 81, 82, 83, 
                   84, 85, 86, 87, 89, 93, 94, 95, 97, 98)
  
  oficio_cat3 <- c(0, 56, 61, 63, 64, 71, 73, 75, 78, 99)
  
  data %>% 
    mutate(
      oficio_grupo1 = ifelse(Oficio %in% oficio_cat1, 1, 0),
      oficio_grupo2 = ifelse(Oficio %in% oficio_cat2, 1, 0),
      oficio_grupo3 = ifelse(Oficio %in% oficio_cat3, 1, 0)
    )
}

# 4.3 Aplicar transformaciones a los datos de personas
train_personas <- train_personas %>% 
  preprocess_binarias() %>%
  categorizar_oficio()

test_personas <- test_personas %>% 
  preprocess_binarias() %>%
  categorizar_oficio()

################################################################################
# 5. CREAR AGREGACIONES A NIVEL DE HOGAR                                      #
################################################################################

cat("Creando agregaciones a nivel de hogar...\n")

# 5.1 Conteos básicos y promedios
train_hogar_agregado <- train_personas %>%
  group_by(id) %>%
  summarize(
    # Conteos de miembros
    n_miembros = n(),
    n_hombres = sum(P6020 == 1, na.rm = TRUE),
    n_mujeres = sum(P6020 == 2, na.rm = TRUE),
    n_menores = sum(P6040 <= 18, na.rm = TRUE),
    n_edad_trabajar = sum(Pet == 1, na.rm = TRUE),
    n_ocupados = sum(Oc == 1, na.rm = TRUE),
    n_desocupados = sum(Des == 1, na.rm = TRUE),
    n_inactivos = sum(Ina == 1, na.rm = TRUE),
    
    # Promedios
    edad_promedio = mean(P6040, na.rm = TRUE),
    promedio_horas_trab = mean(P6800[Pet == 1], na.rm = TRUE),
    
    # Máximos
    max_edad = max(P6040, na.rm = TRUE),
    max_nivel_educativo = max(P6210, na.rm = TRUE),
    max_años_educ = max(P6210s1, na.rm = TRUE),
    
    # Variables de seguridad social (proporción)
    prop_afiliados_ss = mean(P6090 == 1, na.rm = TRUE),
    
    # Variables de trabajo (proporciones)
    prop_busca_trabajo = mean(P6240 == 2, na.rm = TRUE),
    
    # Proporciones de ocupación (grupos 1, 2 y 5)
    prop_empl_part_empr = mean(P6430 %in% c(1, 2, 5) & Pet == 1, na.rm = TRUE),
    
    # Proporción en empresas grandes
    prop_empr_grande = mean(P6870 %in% c(5, 6, 7, 8, 9) & Pet == 1, na.rm = TRUE),
    
    # Proporción que cotiza pensión
    prop_cotiza_pension = mean(P6920 == 1 & Pet == 1, na.rm = TRUE),
    
    # Proporción con actividad adicional
    prop_actividad_adicional = mean(P7040 == 1, na.rm = TRUE),
    
    # Proporción desea trabajar más
    prop_desea_mas_horas = mean(P7090 == 1, na.rm = TRUE),
    
    # Ingresos por arrendamiento u otros
    tiene_arriendo = as.integer(any(P7495 == 1, na.rm = TRUE)),
    tiene_otros_ingresos = as.integer(any(P7505 == 1, na.rm = TRUE)),
    
    # Proporciones de los grupos de oficio (solo entre ocupados)
    prop_oficio_grupo1 = sum(oficio_grupo1 & Oc == 1 & Pet == 1, na.rm = TRUE) / 
      max(1, sum(Oc == 1 & Pet == 1, na.rm = TRUE)),
    prop_oficio_grupo2 = sum(oficio_grupo2 & Oc == 1 & Pet == 1, na.rm = TRUE) / 
      max(1, sum(Oc == 1 & Pet == 1, na.rm = TRUE)),
    prop_oficio_grupo3 = sum(oficio_grupo3 & Oc == 1 & Pet == 1, na.rm = TRUE) / 
      max(1, sum(Oc == 1 & Pet == 1, na.rm = TRUE))
  ) %>%
  # Reemplazar NaN/Inf por NA
  mutate(across(everything(), ~ifelse(is.nan(.) | is.infinite(.), NA, .)))

# 5.2 Lo mismo para test
test_hogar_agregado <- test_personas %>%
  group_by(id) %>%
  summarize(
    # Conteos de miembros
    n_miembros = n(),
    n_hombres = sum(P6020 == 1, na.rm = TRUE),
    n_mujeres = sum(P6020 == 2, na.rm = TRUE),
    n_menores = sum(P6040 <= 18, na.rm = TRUE),
    n_edad_trabajar = sum(Pet == 1, na.rm = TRUE),
    n_ocupados = sum(Oc == 1, na.rm = TRUE),
    n_desocupados = sum(Des == 1, na.rm = TRUE),
    n_inactivos = sum(Ina == 1, na.rm = TRUE),
    
    # Promedios
    edad_promedio = mean(P6040, na.rm = TRUE),
    promedio_horas_trab = mean(P6800[Pet == 1], na.rm = TRUE),
    
    # Máximos
    max_edad = max(P6040, na.rm = TRUE),
    max_nivel_educativo = max(P6210, na.rm = TRUE),
    max_años_educ = max(P6210s1, na.rm = TRUE),
    
    # Variables de seguridad social (proporción)
    prop_afiliados_ss = mean(P6090 == 1, na.rm = TRUE),
    
    # Variables de trabajo (proporciones)
    prop_busca_trabajo = mean(P6240 == 2, na.rm = TRUE),
    
    # Proporciones de ocupación (grupos 1, 2 y 5)
    prop_empl_part_empr = mean(P6430 %in% c(1, 2, 5) & Pet == 1, na.rm = TRUE),
    
    # Proporción en empresas grandes
    prop_empr_grande = mean(P6870 %in% c(5, 6, 7, 8, 9) & Pet == 1, na.rm = TRUE),
    
    # Proporción que cotiza pensión
    prop_cotiza_pension = mean(P6920 == 1 & Pet == 1, na.rm = TRUE),
    
    # Proporción con actividad adicional
    prop_actividad_adicional = mean(P7040 == 1, na.rm = TRUE),
    
    # Proporción desea trabajar más
    prop_desea_mas_horas = mean(P7090 == 1, na.rm = TRUE),
    
    # Ingresos por arrendamiento u otros
    tiene_arriendo = as.integer(any(P7495 == 1, na.rm = TRUE)),
    tiene_otros_ingresos = as.integer(any(P7505 == 1, na.rm = TRUE)),
    
    # Proporciones de los grupos de oficio (solo entre ocupados)
    prop_oficio_grupo1 = sum(oficio_grupo1 & Oc == 1 & Pet == 1, na.rm = TRUE) / 
      max(1, sum(Oc == 1 & Pet == 1, na.rm = TRUE)),
    prop_oficio_grupo2 = sum(oficio_grupo2 & Oc == 1 & Pet == 1, na.rm = TRUE) / 
      max(1, sum(Oc == 1 & Pet == 1, na.rm = TRUE)),
    prop_oficio_grupo3 = sum(oficio_grupo3 & Oc == 1 & Pet == 1, na.rm = TRUE) / 
      max(1, sum(Oc == 1 & Pet == 1, na.rm = TRUE))
  ) %>%
  # Reemplazar NaN/Inf por NA
  mutate(across(everything(), ~ifelse(is.nan(.) | is.infinite(.), NA, .)))

# 5.3 Características del jefe de hogar
train_jefe_hogar <- train_personas %>%
  filter(P6050 == 1) %>%  # Jefe de hogar
  select(id, P6020, P6040, P6210, P6210s1, P6426, Oc) %>%
  rename(
    jefe_sexo = P6020,
    jefe_edad = P6040,
    jefe_nivel_educativo = P6210,
    jefe_años_educ = P6210s1,
    jefe_tiempo_trabajo = P6426,
    jefe_ocupado = Oc
  ) %>%
  # Transformar sexo a variable binaria (1=mujer, 0=hombre)
  mutate(jefe_mujer = ifelse(jefe_sexo == 2, 1, 0)) %>%
  select(-jefe_sexo)  # Eliminar la variable original

test_jefe_hogar <- test_personas %>%
  filter(P6050 == 1) %>%  # Jefe de hogar
  select(id, P6020, P6040, P6210, P6210s1, P6426, Oc) %>%
  rename(
    jefe_sexo = P6020,
    jefe_edad = P6040,
    jefe_nivel_educativo = P6210,
    jefe_años_educ = P6210s1,
    jefe_tiempo_trabajo = P6426,
    jefe_ocupado = Oc
  ) %>%
  # Transformar sexo a variable binaria (1=mujer, 0=hombre)
  mutate(jefe_mujer = ifelse(jefe_sexo == 2, 1, 0)) %>%
  select(-jefe_sexo)  # Eliminar la variable original

# 5.4 Proporciones de variables categóricas
# Lista de variables categóricas con procesamiento específico
vars_categoricas <- c(
  "P6510", "P6545", "P6580", "P6585s1", "P6585s2", "P6585s3", "P6585s4", 
  "P6590", "P6600", "P6610", "P6620", "P6630s1", "P6630s2", "P6630s3", 
  "P6630s4", "P6630s6", "P7110", "P7120", "P7150", "P7160", "P7310", 
  "P7422", "P7472", "P7500s2", "P7500s3", "P7510s1", "P7510s2", "P7510s3", 
  "P7510s5", "P7510s6", "P7510s7"
)

# Función para calcular proporciones
calcular_proporciones <- function(data, variables) {
  # Filtrar solo variables que existen en los datos
  variables_presentes <- variables[variables %in% names(data)]
  
  # Si no hay variables presentes, devolver solo el id
  if(length(variables_presentes) == 0) {
    return(data.frame(id = unique(data$id)))
  }
  
  resultado <- data %>% 
    group_by(id) %>%
    summarize(across(all_of(variables_presentes), 
                     ~mean(. == 1 & Pet == 1, na.rm = TRUE), 
                     .names = "prop_{.col}"))
  return(resultado)
}

# Calcular proporciones para las variables categóricas disponibles
train_proporciones <- calcular_proporciones(train_personas, vars_categoricas)
test_proporciones <- calcular_proporciones(test_personas, vars_categoricas)

# Mostrar cuántas variables se procesaron
cat("Variables categóricas disponibles en train:", 
    sum(vars_categoricas %in% names(train_personas)), "\n")
cat("Variables categóricas disponibles en test:", 
    sum(vars_categoricas %in% names(test_personas)), "\n")

################################################################################
# 6. UNIÓN FINAL DE DATOS                                                      #
################################################################################

cat("Uniendo datos finales...\n")

# 6.1 Unir para entrenamiento
train_final <- train_hogares %>%
  left_join(train_hogar_agregado, by = "id") %>%
  left_join(train_jefe_hogar, by = "id") %>%
  left_join(train_proporciones, by = "id")

# 6.2 Unir para prueba
test_final <- test_hogares %>%
  left_join(test_hogar_agregado, by = "id") %>%
  left_join(test_jefe_hogar, by = "id") %>%
  left_join(test_proporciones, by = "id")

# 6.3 Verificar dimensiones finales
cat("Dimensiones del conjunto de entrenamiento unido:", dim(train_final), "\n")
cat("Dimensiones del conjunto de prueba unido:", dim(test_final), "\n")

# 6.4 Verificación simple de hogares
cat("Hogares en train:", length(unique(train_final$id)), "\n")
cat("Hogares en test:", length(unique(test_final$id)), "\n")

################################################################################
# 7. GUARDAR DATOS UNIDOS                                                     #
################################################################################

cat("Guardando datos unidos...\n")

# Crear directorio si no existe
if (!dir.exists("stores/processed")) {
  dir.create("stores/processed", recursive = TRUE)
}

# Guardar conjuntos de datos unidos
write.csv(train_final, "stores/processed/train_joined.csv", row.names = FALSE)
write.csv(test_final, "stores/processed/test_joined.csv", row.names = FALSE)

# También guardar un archivo RDS que preserva tipos de datos
saveRDS(train_final, "stores/processed/train_joined.rds")
saveRDS(test_final, "stores/processed/test_joined.rds")

cat("Datos unidos guardados en 'stores/processed/'.\n")
cat("La imputación adicional de valores faltantes y transformaciones adicionales\n")
cat("se realizarán en los scripts posteriores.\n")

################################################################################
#                              FIN DEL SCRIPT                                  #
################################################################################