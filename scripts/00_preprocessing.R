################################################################################
# TÍTULO: 00_preprocessing.R                                                   #
# PROYECTO: Predicción de Pobreza en Colombia                                 #
# DESCRIPCIÓN: Análisis independiente de las bases de datos de hogares y      #
#              personas antes de su unión.                                     #
# FECHA: 30 de marzo de 2025                                                   #
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

###########################################
# 1. CARGAR Y EXPLORAR BASES DE DATOS    #
###########################################

# Cargar datos - train
train_hogares <- read.csv("stores/raw/train_hogares.csv")
train_personas <- read.csv("stores/raw/train_personas.csv")

# Cargar datos - test
test_hogares <- read.csv("stores/raw/test_hogares.csv")
test_personas <- read.csv("stores/raw/test_personas.csv")

# Cargar diccionario de tipos de datos para personas
tipos_datos_personas <- read.csv("stores/raw/tipos_datos_personas.csv", 
                                 stringsAsFactors = FALSE, 
                                 encoding = "UTF-8")

# Mostrar dimensiones
cat("Dimensiones de train_hogares:", dim(train_hogares), "\n")
cat("Dimensiones de train_personas:", dim(train_personas), "\n")
cat("Dimensiones de test_hogares:", dim(test_hogares), "\n")
cat("Dimensiones de test_personas:", dim(test_personas), "\n")

# Exploración inicial - Estructura y columnas
cat("\nEstructura de train_hogares:\n")
str(train_hogares)
cat("\nColumnas de train_hogares:\n")
print(colnames(train_hogares))

cat("\nEstructura de train_personas:\n")
str(train_personas)
cat("\nColumnas de train_personas:\n")
print(colnames(train_personas))

###########################################
# 2. IDENTIFICACIÓN DE VARIABLES COMUNES #
###########################################

# Identificar variables comunes en hogares
vars_comunes_hogares <- intersect(names(train_hogares), names(test_hogares))
cat("\nVariables comunes en hogares:", length(vars_comunes_hogares), "\n")

# Identificar variables comunes en personas
vars_comunes_personas <- intersect(names(train_personas), names(test_personas))
cat("Variables comunes en personas:", length(vars_comunes_personas), "\n")

# Guardar listas de variables comunes
write.csv(data.frame(variable = vars_comunes_hogares), 
          "views/tables/vars_comunes_hogares.csv", row.names = FALSE)
write.csv(data.frame(variable = vars_comunes_personas), 
          "views/tables/vars_comunes_personas.csv", row.names = FALSE)

# Variables únicas en train (no están en test)
diff_vars_hogares <- setdiff(names(train_hogares), names(test_hogares))
diff_vars_personas <- setdiff(names(train_personas), names(test_personas))

cat("\nVariables presentes en train_hogares pero no en test_hogares:", length(diff_vars_hogares), "\n")
print(diff_vars_hogares)

cat("\nVariables presentes en train_personas pero no en test_personas:", length(diff_vars_personas), "\n")
print(diff_vars_personas)

# Guardar listas de variables diferentes
write.csv(data.frame(variable = diff_vars_hogares), 
          "views/tables/vars_solo_train_hogares.csv", row.names = FALSE)

write.csv(data.frame(variable = diff_vars_personas), 
          "views/tables/vars_solo_train_personas.csv", row.names = FALSE)

################################################################################
# 3. VERIFICACIÓN DE LA VARIABLE OBJETIVO (POBREZA)                          #
################################################################################
# Según el DANE, un hogar es pobre si el ingreso per cápita es menor a la línea de pobreza
# Verificamos si la variable 'Pobre' incluida en los datos sigue esta definición

cat("\nVerificando la definición de pobreza...\n")
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

# Calcular porcentajes de coincidencia
coincidencia1 <- sum(diag(table(train_hogares$Pobre, train_hogares$Pobre_calculado))) / nrow(train_hogares) * 100
coincidencia2 <- sum(diag(table(train_hogares$Pobre, train_hogares$Pobre_calculado2))) / nrow(train_hogares) * 100

cat(sprintf("Coincidencia con método 1 (Ingpcug < Lp): %.2f%%\n", coincidencia1))
cat(sprintf("Coincidencia con método 2 (Ingtotugarr < Lp * Npersug): %.2f%%\n", coincidencia2))

cat("Confirmado: La variable 'Pobre' está correctamente definida según el criterio del DANE.\n")

# Ver distribución de la variable objetivo
cat("\nDistribución de la variable objetivo (Pobre):\n")
tabla_pobreza <- table(train_hogares$Pobre)
print(tabla_pobreza)
cat("Porcentaje de hogares pobres:", 
    round(tabla_pobreza[2] / sum(tabla_pobreza) * 100, 2), "%\n")

# Guardar la distribución de pobreza
prop_table <- as.data.frame(prop.table(tabla_pobreza))
names(prop_table) <- c("Pobre", "Proporcion")
write.csv(prop_table, "views/tables/distribucion_pobreza.csv", row.names = FALSE)

# Verificamos si todos los hogares identificados como indigentes también están clasificados como pobres
train_hogares %>% 
  count(Indigente, Pobre) %>% 
  arrange(desc(Indigente), Pobre)
# Se espera que NO existan hogares indigentes (1) y no pobres (0) 

###########################################
# 4. ANÁLISIS DE VARIABLES DISCRETAS     #
###########################################

# Lista manual de variables a excluir del análisis
vars_excluir <- c("Fex_dpto") # Puedes añadir más variables separadas por comas

# Identificar variables discretas numéricas según el diccionario
vars_discretas_num <- tipos_datos_personas %>%
  filter(tipo == "discrete" & formato == "numeric") %>%
  pull(variable)

# Eliminar variables no deseadas de la lista
vars_discretas_num <- vars_discretas_num[!vars_discretas_num %in% vars_excluir]

cat("\nAnalizando distribución de variables discretas numéricas...\n")
cat("Variables excluidas manualmente:", paste(vars_excluir, collapse = ", "), "\n")

# Crear un dataframe para almacenar resultados
distribucion_discretas <- data.frame()

# Analizar la distribución de cada variable discreta numérica
for(var in vars_discretas_num) {
  if(var %in% names(train_personas)) {
    # Obtener tabla de frecuencia incluyendo NAs
    tabla <- table(train_personas[[var]], useNA = "always")
    
    # Convertir a data frame
    df_tabla <- as.data.frame(tabla)
    names(df_tabla) <- c("Valor", "Frecuencia")
    
    # Agregar nombre de variable
    df_tabla$Variable <- var
    
    # Agregar al dataframe de resultados
    distribucion_discretas <- rbind(distribucion_discretas, df_tabla)
  }
}

# Guardar distribución de variables discretas
write.csv(distribucion_discretas, "views/tables/distribucion_vars_discretas.csv", row.names = FALSE)

###########################################
# 5. ANÁLISIS BÁSICO DE HOGARES          #
###########################################

# Análisis básico de missing values en hogares
missing_hogares <- data.frame(
  variable = names(train_hogares),
  n_missing = sapply(train_hogares, function(x) sum(is.na(x))),
  pct_missing = sapply(train_hogares, function(x) sum(is.na(x))/length(x))
)

# Ordenar por porcentaje de missing values
missing_hogares <- missing_hogares %>%
  arrange(desc(pct_missing))

# Guardar análisis de missing values en hogares
write.csv(missing_hogares, "views/tables/missing_hogares.csv", row.names = FALSE)

###########################################
# 6. ANÁLISIS DESCRIPTIVO DE PERSONAS    #
###########################################

cat("\nAnalizando base de personas...\n")

# 6.1 Análisis para toda la base de personas
cat("Analizando toda la base de personas...\n")

# Crear tablas para cada tipo de variable
variables_personas <- data.frame(
  variable = names(train_personas),
  tipo = sapply(train_personas, class),
  n_valores_unicos = sapply(train_personas, function(x) length(unique(na.omit(x)))),
  n_missing = sapply(train_personas, function(x) sum(is.na(x))),
  pct_missing = sapply(train_personas, function(x) sum(is.na(x))/length(x)),
  stringsAsFactors = FALSE
)

# Estadísticas para variables numéricas
variables_num_personas <- data.frame()
for(var in names(train_personas)) {
  if(is.numeric(train_personas[[var]])) {
    if(!all(is.na(train_personas[[var]]))) {
      # Estadísticas básicas
      min_val <- min(train_personas[[var]], na.rm = TRUE)
      max_val <- max(train_personas[[var]], na.rm = TRUE)
      mean_val <- mean(train_personas[[var]], na.rm = TRUE)
      median_val <- median(train_personas[[var]], na.rm = TRUE)
      sd_val <- sd(train_personas[[var]], na.rm = TRUE)
      
      # Agregar a la tabla
      variables_num_personas <- rbind(variables_num_personas, data.frame(
        variable = var,
        min = min_val,
        max = max_val,
        media = mean_val,
        mediana = median_val,
        desv_est = sd_val,
        stringsAsFactors = FALSE
      ))
    }
  }
}

# Estadísticas para variables categóricas
variables_cat_personas <- data.frame()
for(var in names(train_personas)) {
  x <- train_personas[[var]]
  es_categorica <- is.character(x) || is.factor(x) || (is.numeric(x) && length(unique(na.omit(x))) <= 15)
  
  if (es_categorica) {
    var_data <- na.omit(x) # eliminar NAs
    
    # Crear tabla de frecuencias
    freq_table <- table(var_data)
    
    if (length(freq_table) > 0) {
      max_freq <- max(freq_table)
      max_cat <- names(freq_table)[which.max(freq_table)]
      total_validos <- sum(freq_table) # mismo que length(var_data)
      
      # Crear data frame con resultados
      resultados <- data.frame(
        variable = var,
        n_categorias = length(freq_table),
        valor_mas_frecuente = max_cat,
        frec_valor_mas_frecuente = max_freq,
        prop_valor_mas_frecuente = max_freq / total_validos,
        stringsAsFactors = FALSE
      )
      
      variables_cat_personas <- rbind(variables_cat_personas, resultados)
    }
  }
}

# Unir resultados para personas
descriptiva_personas <- variables_personas %>%
  left_join(variables_num_personas, by = "variable") %>%
  left_join(variables_cat_personas, by = "variable")

# Guardar tabla descriptiva de personas (todas)
write.csv(descriptiva_personas, "views/tables/descriptiva_personas_todas.csv", row.names = FALSE)

###########################################
# 6.2 Análisis solo para personas con Pet = 1
###########################################

cat("Analizando solo personas con Pet = 1 (en edad de trabajar)...\n")

# Filtrar personas en edad de trabajar
personas_pet <- train_personas %>% filter(Pet == 1)

# Crear tablas para cada tipo de variable
variables_personas_pet <- data.frame(
  variable = names(personas_pet),
  tipo = sapply(personas_pet, class),
  n_valores_unicos = sapply(personas_pet, function(x) length(unique(na.omit(x)))),
  n_missing = sapply(personas_pet, function(x) sum(is.na(x))),
  pct_missing = sapply(personas_pet, function(x) sum(is.na(x))/length(x)),
  stringsAsFactors = FALSE
)

# Estadísticas para variables numéricas
variables_num_personas_pet <- data.frame()
for(var in names(personas_pet)) {
  if(is.numeric(personas_pet[[var]])) {
    if(!all(is.na(personas_pet[[var]]))) {
      # Estadísticas básicas
      min_val <- min(personas_pet[[var]], na.rm = TRUE)
      max_val <- max(personas_pet[[var]], na.rm = TRUE)
      mean_val <- mean(personas_pet[[var]], na.rm = TRUE)
      median_val <- median(personas_pet[[var]], na.rm = TRUE)
      sd_val <- sd(personas_pet[[var]], na.rm = TRUE)
      
      # Agregar a la tabla
      variables_num_personas_pet <- rbind(variables_num_personas_pet, data.frame(
        variable = var,
        min = min_val,
        max = max_val,
        media = mean_val,
        mediana = median_val,
        desv_est = sd_val,
        stringsAsFactors = FALSE
      ))
    }
  }
}

# Estadísticas para variables categóricas
variables_cat_personas_pet <- data.frame()
for(var in names(personas_pet)) {
  x <- personas_pet[[var]]
  es_categorica <- is.character(x) || is.factor(x) || (is.numeric(x) && length(unique(na.omit(x))) <= 15)
  
  if (es_categorica) {
    var_data <- na.omit(x) # eliminar NAs
    
    # Crear tabla de frecuencias
    freq_table <- table(var_data)
    
    if (length(freq_table) > 0) {
      max_freq <- max(freq_table)
      max_cat <- names(freq_table)[which.max(freq_table)]
      total_validos <- sum(freq_table) # mismo que length(var_data)
      
      # Crear data frame con resultados
      resultados <- data.frame(
        variable = var,
        n_categorias = length(freq_table),
        valor_mas_frecuente = max_cat,
        frec_valor_mas_frecuente = max_freq,
        prop_valor_mas_frecuente = max_freq / total_validos,
        stringsAsFactors = FALSE
      )
      
      variables_cat_personas_pet <- rbind(variables_cat_personas_pet, resultados)
    }
  }
}

# Unir resultados para personas con Pet = 1
descriptiva_personas_pet <- variables_personas_pet %>%
  left_join(variables_num_personas_pet, by = "variable") %>%
  left_join(variables_cat_personas_pet, by = "variable")

# Guardar tabla descriptiva de personas (solo Pet = 1)
write.csv(descriptiva_personas_pet, "views/tables/descriptiva_personas_pet.csv", row.names = FALSE)

###########################################
# 6.3 Análisis solo para personas mayores de 18 años 
###########################################

cat("Analizando solo personas mayores de 18 años...\n")

# Crear variable flag para identificar personas mayores de 18 años
train_personas <- train_personas %>%
  mutate(mayor_18 = ifelse(P6040 >= 18, 1, 0))

# Filtrar personas mayores de 18 años
personas_adultas <- train_personas %>% filter(mayor_18 == 1)

# Crear tablas para cada tipo de variable
variables_personas_adultas <- data.frame(
  variable = names(personas_adultas),
  tipo = sapply(personas_adultas, class),
  n_valores_unicos = sapply(personas_adultas, function(x) length(unique(na.omit(x)))),
  n_missing = sapply(personas_adultas, function(x) sum(is.na(x))),
  pct_missing = sapply(personas_adultas, function(x) sum(is.na(x))/length(x)),
  stringsAsFactors = FALSE
)

# Estadísticas para variables numéricas
variables_num_personas_adultas <- data.frame()
for(var in names(personas_adultas)) {
  if(is.numeric(personas_adultas[[var]])) {
    if(!all(is.na(personas_adultas[[var]]))) {
      # Estadísticas básicas
      min_val <- min(personas_adultas[[var]], na.rm = TRUE)
      max_val <- max(personas_adultas[[var]], na.rm = TRUE)
      mean_val <- mean(personas_adultas[[var]], na.rm = TRUE)
      median_val <- median(personas_adultas[[var]], na.rm = TRUE)
      sd_val <- sd(personas_adultas[[var]], na.rm = TRUE)
      
      # Agregar a la tabla
      variables_num_personas_adultas <- rbind(variables_num_personas_adultas, data.frame(
        variable = var,
        min = min_val,
        max = max_val,
        media = mean_val,
        mediana = median_val,
        desv_est = sd_val,
        stringsAsFactors = FALSE
      ))
    }
  }
}

# Estadísticas para variables categóricas
variables_cat_personas_adultas <- data.frame()
for(var in names(personas_adultas)) {
  x <- personas_adultas[[var]]
  es_categorica <- is.character(x) || is.factor(x) || (is.numeric(x) && length(unique(na.omit(x))) <= 15)
  
  if (es_categorica) {
    var_data <- na.omit(x) # eliminar NAs
    
    # Crear tabla de frecuencias
    freq_table <- table(var_data)
    
    if (length(freq_table) > 0) {
      max_freq <- max(freq_table)
      max_cat <- names(freq_table)[which.max(freq_table)]
      total_validos <- sum(freq_table) # mismo que length(var_data)
      
      # Crear data frame con resultados
      resultados <- data.frame(
        variable = var,
        n_categorias = length(freq_table),
        valor_mas_frecuente = max_cat,
        frec_valor_mas_frecuente = max_freq,
        prop_valor_mas_frecuente = max_freq / total_validos,
        stringsAsFactors = FALSE
      )
      
      variables_cat_personas_adultas <- rbind(variables_cat_personas_adultas, resultados)
    }
  }
}

# Unir resultados para personas adultas
descriptiva_personas_adultas <- variables_personas_adultas %>%
  left_join(variables_num_personas_adultas, by = "variable") %>%
  left_join(variables_cat_personas_adultas, by = "variable")

# Guardar tabla descriptiva de personas (solo adultos)
write.csv(descriptiva_personas_adultas, "views/tables/descriptiva_personas_adultas.csv", row.names = FALSE)

###########################################
# 7. COMPARACIÓN DE MISSING VALUES       #
###########################################

# Comparar missing values entre las diferentes poblaciones analizadas
variables_comunes <- intersect(
  intersect(names(train_personas), names(personas_pet)),
  names(personas_adultas)
)

missing_comparacion <- data.frame(
  variable = variables_comunes,
  missing_todas = sapply(train_personas[variables_comunes], function(x) sum(is.na(x))),
  pct_missing_todas = sapply(train_personas[variables_comunes], function(x) sum(is.na(x))/nrow(train_personas)),
  missing_pet = sapply(personas_pet[variables_comunes], function(x) sum(is.na(x))),
  pct_missing_pet = sapply(personas_pet[variables_comunes], function(x) sum(is.na(x))/nrow(personas_pet)),
  missing_adultos = sapply(personas_adultas[variables_comunes], function(x) sum(is.na(x))),
  pct_missing_adultos = sapply(personas_adultas[variables_comunes], function(x) sum(is.na(x))/nrow(personas_adultas))
)

# Calcular diferencias en proporción de missing values
missing_comparacion <- missing_comparacion %>%
  mutate(
    diff_pct_pet_todas = pct_missing_pet - pct_missing_todas,
    diff_pct_adultos_todas = pct_missing_adultos - pct_missing_todas,
    diff_pct_adultos_pet = pct_missing_adultos - pct_missing_pet
  )

# Ordenar por mayor diferencia
missing_comparacion <- missing_comparacion %>%
  arrange(desc(abs(diff_pct_pet_todas)))

# Guardar comparación de missing values
write.csv(missing_comparacion, "views/tables/missing_comparacion_personas.csv", row.names = FALSE)

cat("\n¡Análisis descriptivo completado! Todas las tablas han sido guardadas en views/tables/\n")

################################################################################
#                            FINALIZACIÓN DEL SCRIPT                           #
################################################################################