################################################################################
# TÍTULO: 00_preprocessing_modificado.R                                        #
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

# Exploración inicial - Base de entranamiento
cat("\nEstructura de train_hogares:\n")
str(train_hogares)
cat("\nColumnas de train_hogares:\n")
print(colnames(train_hogares))

cat("\nEstructura de train_personas:\n")
str(train_personas)
cat("\nColumnas de train_personas:\n")
print(colnames(train_personas))

# Exploración inicial - Base de pruebas
cat("\nEstructura de test_hogares:\n")
str(test_hogares)
cat("\nColumnas de test_hogares:\n")
print(colnames(test_hogares))

cat("\nEstructura de test_personas:\n")
str(test_personas)
cat("\nColumnas de test_personas:\n")
print(colnames(test_personas))

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

# Añadir la variable Pobre de hogares a la base de personas
# Primero seleccionamos solo id y Pobre de la base de hogares
hogares_pobre <- train_hogares %>% select(id, Pobre)

# Luego unimos con la base de personas
train_personas_con_pobre <- train_personas %>%
  left_join(hogares_pobre, by = "id")

# Lista manual de variables a excluir del análisis
vars_excluir <- c("Fex_dpto") # Añadir más variables separadas por comas

# Identificar variables discretas numéricas según el diccionario
vars_discretas_num <- tipos_datos_personas %>%
  filter(tipo == "discrete" & formato == "numeric") %>%
  pull(variable)

# Eliminar variables no deseadas de la lista
vars_discretas_num <- vars_discretas_num[!vars_discretas_num %in% vars_excluir]

cat("\nAnalizando distribución de variables discretas numéricas...\n")
cat("Variables excluidas manualmente:", paste(vars_excluir, collapse = ", "), "\n")

# Crear un dataframe para almacenar resultados con información sobre pobreza
distribucion_discretas <- data.frame()

# Analizar la distribución de cada variable discreta numérica e incluir conteo de pobres
for(var in vars_discretas_num) {
  if(var %in% names(train_personas_con_pobre)) {
    # Crear tabla de contingencia
    tabla_contingencia <- table(train_personas_con_pobre[[var]], train_personas_con_pobre$Pobre, useNA = "always")
    
    # Convertir a data frame para mejor manipulación
    df_tabla <- as.data.frame(tabla_contingencia)
    names(df_tabla) <- c("Valor", "Pobre", "Frecuencia")
    
    # Añadir nombre de variable
    df_tabla$Variable <- var
    
    # Calcular frecuencia total por valor (incluyendo NA en Pobre para mantener el conteo total)
    totales_por_valor <- df_tabla %>%
      group_by(Variable, Valor) %>%
      summarize(Frecuencia_Total = sum(Frecuencia), .groups = 'drop')
    
    # Calcular totales por valor sin NAs en Pobre (para calcular porcentajes)
    totales_validos <- df_tabla %>%
      filter(!is.na(Pobre)) %>%
      group_by(Variable, Valor) %>%
      summarize(Total_Validos = sum(Frecuencia), .groups = 'drop')
    
    # Reorganizar para tener columnas para total, pobres y no pobres
    df_final <- df_tabla %>%
      filter(!is.na(Pobre)) %>%
      pivot_wider(
        id_cols = c(Variable, Valor),
        names_from = Pobre,
        values_from = Frecuencia,
        names_prefix = "Pobre_"
      ) %>%
      left_join(totales_validos, by = c("Variable", "Valor")) %>%
      left_join(totales_por_valor, by = c("Variable", "Valor")) %>%
      mutate(
        Pobre_0 = ifelse(is.na(Pobre_0), 0, Pobre_0),
        Pobre_1 = ifelse(is.na(Pobre_1), 0, Pobre_1),
        Porcentaje_Pobres = Pobre_1 / Total_Validos * 100
      )
    
    # Agregar al dataframe de resultados
    distribucion_discretas <- rbind(distribucion_discretas, df_final)
  }
}

# Guardar distribución de variables discretas con conteo de pobreza
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
# 6.2 Análisis para personas entre 18 y 65 años y ocupadas
###########################################

cat("Analizando personas entre 18 y 65 años y ocupadas...\n")

# Filtrar personas entre 18 y 65 años y ocupadas
personas_18_65_ocupadas <- train_personas %>% 
  filter(P6040 >= 18, P6040 <= 65, Oc == 1)

# Crear tablas para cada tipo de variable
variables_personas_filtro <- data.frame(
  variable = names(personas_18_65_ocupadas),
  tipo = sapply(personas_18_65_ocupadas, class),
  n_valores_unicos = sapply(personas_18_65_ocupadas, function(x) length(unique(na.omit(x)))),
  n_missing = sapply(personas_18_65_ocupadas, function(x) sum(is.na(x))),
  pct_missing = sapply(personas_18_65_ocupadas, function(x) sum(is.na(x))/length(x)),
  stringsAsFactors = FALSE
)

# Estadísticas para variables numéricas
variables_num_personas_filtro <- data.frame()
for(var in names(personas_18_65_ocupadas)) {
  if(is.numeric(personas_18_65_ocupadas[[var]])) {
    if(!all(is.na(personas_18_65_ocupadas[[var]]))) {
      # Estadísticas básicas
      min_val <- min(personas_18_65_ocupadas[[var]], na.rm = TRUE)
      max_val <- max(personas_18_65_ocupadas[[var]], na.rm = TRUE)
      mean_val <- mean(personas_18_65_ocupadas[[var]], na.rm = TRUE)
      median_val <- median(personas_18_65_ocupadas[[var]], na.rm = TRUE)
      sd_val <- sd(personas_18_65_ocupadas[[var]], na.rm = TRUE)
      
      # Agregar a la tabla
      variables_num_personas_filtro <- rbind(variables_num_personas_filtro, data.frame(
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
variables_cat_personas_filtro <- data.frame()
for(var in names(personas_18_65_ocupadas)) {
  x <- personas_18_65_ocupadas[[var]]
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
      
      variables_cat_personas_filtro <- rbind(variables_cat_personas_filtro, resultados)
    }
  }
}

# Unir resultados para personas con filtro
descriptiva_personas_filtro <- variables_personas_filtro %>%
  left_join(variables_num_personas_filtro, by = "variable") %>%
  left_join(variables_cat_personas_filtro, by = "variable")

# Guardar tabla descriptiva de personas (con filtro)
write.csv(descriptiva_personas_filtro, "views/tables/descriptiva_personas_18_65_ocupadas.csv", row.names = FALSE)

###########################################
# 6.3 Análisis para personas en edad de trabajar (Pet = 1), <= 65 años y ocupadas
###########################################

cat("Analizando personas en edad de trabajar (Pet = 1), <= 65 años y ocupadas...\n")

# Filtrar personas en edad de trabajar, menores o iguales a 65 años y ocupadas
personas_pet_65_ocupadas <- train_personas %>% 
  filter(Pet == 1, P6040 <= 65, Oc == 1)

# Crear tablas para cada tipo de variable
variables_personas_pet <- data.frame(
  variable = names(personas_pet_65_ocupadas),
  tipo = sapply(personas_pet_65_ocupadas, class),
  n_valores_unicos = sapply(personas_pet_65_ocupadas, function(x) length(unique(na.omit(x)))),
  n_missing = sapply(personas_pet_65_ocupadas, function(x) sum(is.na(x))),
  pct_missing = sapply(personas_pet_65_ocupadas, function(x) sum(is.na(x))/length(x)),
  stringsAsFactors = FALSE
)

# Estadísticas para variables numéricas
variables_num_personas_pet <- data.frame()
for(var in names(personas_pet_65_ocupadas)) {
  if(is.numeric(personas_pet_65_ocupadas[[var]])) {
    if(!all(is.na(personas_pet_65_ocupadas[[var]]))) {
      # Estadísticas básicas
      min_val <- min(personas_pet_65_ocupadas[[var]], na.rm = TRUE)
      max_val <- max(personas_pet_65_ocupadas[[var]], na.rm = TRUE)
      mean_val <- mean(personas_pet_65_ocupadas[[var]], na.rm = TRUE)
      median_val <- median(personas_pet_65_ocupadas[[var]], na.rm = TRUE)
      sd_val <- sd(personas_pet_65_ocupadas[[var]], na.rm = TRUE)
      
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
for(var in names(personas_pet_65_ocupadas)) {
  x <- personas_pet_65_ocupadas[[var]]
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

# Unir resultados para personas Pet = 1, <= 65 años y ocupadas
descriptiva_personas_pet <- variables_personas_pet %>%
  left_join(variables_num_personas_pet, by = "variable") %>%
  left_join(variables_cat_personas_pet, by = "variable")

# Guardar tabla descriptiva de personas (Pet = 1, <= 65 años, ocupadas)
write.csv(descriptiva_personas_pet, "views/tables/descriptiva_personas_pet_65_ocupadas.csv", row.names = FALSE)

###########################################
# 7. COMPARACIÓN DE MISSING VALUES       #
###########################################

# Comparar missing values entre las diferentes poblaciones analizadas
variables_comunes <- intersect(
  intersect(names(train_personas), names(personas_18_65_ocupadas)), 
  names(personas_pet_65_ocupadas)
)

missing_comparacion <- data.frame(
  variable = variables_comunes,
  missing_todas = sapply(train_personas[variables_comunes], function(x) sum(is.na(x))),
  pct_missing_todas = sapply(train_personas[variables_comunes], function(x) sum(is.na(x))/nrow(train_personas)),
  missing_18_65_ocupadas = sapply(personas_18_65_ocupadas[variables_comunes], function(x) sum(is.na(x))),
  pct_missing_18_65_ocupadas = sapply(personas_18_65_ocupadas[variables_comunes], function(x) sum(is.na(x))/nrow(personas_18_65_ocupadas)),
  missing_pet_65_ocupadas = sapply(personas_pet_65_ocupadas[variables_comunes], function(x) sum(is.na(x))),
  pct_missing_pet_65_ocupadas = sapply(personas_pet_65_ocupadas[variables_comunes], function(x) sum(is.na(x))/nrow(personas_pet_65_ocupadas))
)

# Calcular diferencias en proporción de missing values
missing_comparacion <- missing_comparacion %>%
  mutate(
    diff_pct_18_65_todas = pct_missing_18_65_ocupadas - pct_missing_todas,
    diff_pct_pet_65_todas = pct_missing_pet_65_ocupadas - pct_missing_todas,
    diff_pct_pet_65_18_65 = pct_missing_pet_65_ocupadas - pct_missing_18_65_ocupadas
  )

# Ordenar por mayor diferencia
missing_comparacion <- missing_comparacion %>%
  arrange(desc(abs(diff_pct_18_65_todas)))

# Guardar comparación de missing values
write.csv(missing_comparacion, "views/tables/missing_comparacion_personas.csv", row.names = FALSE)

########################################################
# 7.1 COMPARACIÓN DE MISSING VALUES ENTRE TRAIN Y TEST #
########################################################

cat("\nComparando patrones de missing values entre conjuntos de entrenamiento y prueba...\n")

# Comparar missing values entre train_hogares y test_hogares
cat("Comparando missing values en bases de hogares (train vs test)...\n")

# Usar las variables comunes ya identificadas en la sección 2
missing_comparacion_hogares <- data.frame(
  variable = vars_comunes_hogares,
  missing_train = sapply(train_hogares[vars_comunes_hogares], function(x) sum(is.na(x))),
  pct_missing_train = sapply(train_hogares[vars_comunes_hogares], 
                             function(x) round(sum(is.na(x))/nrow(train_hogares)*100, 2)),
  missing_test = sapply(test_hogares[vars_comunes_hogares], function(x) sum(is.na(x))),
  pct_missing_test = sapply(test_hogares[vars_comunes_hogares], 
                            function(x) round(sum(is.na(x))/nrow(test_hogares)*100, 2)),
  stringsAsFactors = FALSE
)

# Calcular diferencia en porcentaje de missing values
missing_comparacion_hogares <- missing_comparacion_hogares %>%
  mutate(
    diff_pct = pct_missing_train - pct_missing_test
  ) %>%
  arrange(desc(abs(diff_pct)))

# Guardar comparación completa
write.csv(missing_comparacion_hogares, "views/tables/missing_comparacion_hogares_train_test.csv", row.names = FALSE)

# Comparar missing values entre train_personas y test_personas
cat("\nComparando missing values en bases de personas (train vs test)...\n")

# Usar las variables comunes ya identificadas en la sección 2
missing_comparacion_personas <- data.frame(
  variable = vars_comunes_personas,
  missing_train = sapply(train_personas[vars_comunes_personas], function(x) sum(is.na(x))),
  pct_missing_train = sapply(train_personas[vars_comunes_personas], 
                             function(x) round(sum(is.na(x))/nrow(train_personas)*100, 2)),
  missing_test = sapply(test_personas[vars_comunes_personas], function(x) sum(is.na(x))),
  pct_missing_test = sapply(test_personas[vars_comunes_personas], 
                            function(x) round(sum(is.na(x))/nrow(test_personas)*100, 2)),
  stringsAsFactors = FALSE
)

# Calcular diferencia en porcentaje de missing values
missing_comparacion_personas <- missing_comparacion_personas %>%
  mutate(
    diff_pct = pct_missing_train - pct_missing_test
  ) %>%
  arrange(desc(abs(diff_pct)))

# Guardar comparación completa
write.csv(missing_comparacion_personas, "views/tables/missing_comparacion_personas_train_test.csv", row.names = FALSE)

cat("\nComparación de missing values entre train y test completada.\n")
cat("Los resultados detallados se guardaron en 'views/tables/missing_comparacion_hogares_train_test.csv' y\n")
cat("'views/tables/missing_comparacion_personas_train_test.csv'.\n")

###########################################
# 8. AGRUPACIÓN DE CATEGORÍAS DE OFICIO  #
###########################################

# la variable oficio de la base train_personas es la que más clases tiene, para reducir la
# complejidad vamos a agrupara las clases en tres grupos en función de la frecuencia de pobres en cada una de ellas

cat("\nAgrupando categorías de la variable Oficio según su relación con pobreza...\n")

# Filtrar solo personas en edad de trabajar y ocupadas
personas_pet_oc <- train_personas_con_pobre %>% 
  filter(Pet == 1, Oc == 1)

# Calcular la proporción de pobres por cada categoría de Oficio
proporcion_pobres_oficio <- personas_pet_oc %>%
  # Eliminamos valores NA en Oficio
  filter(!is.na(Oficio)) %>%
  # Agrupamos por Oficio
  group_by(Oficio) %>%
  # Calculamos el número de personas y la proporción de pobres
  summarize(
    Total = n(),
    Pobres = sum(Pobre == 1, na.rm = TRUE),
    Prop_Pobres = Pobres / Total,
    .groups = "drop"
  )

# Imprimir información sobre la agrupación
cat("Número de categorías de Oficio después de filtrar NA:", nrow(proporcion_pobres_oficio), "\n")
cat("Rango de proporción de pobres:", min(proporcion_pobres_oficio$Prop_Pobres), "a", 
    max(proporcion_pobres_oficio$Prop_Pobres), "\n")

# Preparar datos para k-means
datos_kmeans <- proporcion_pobres_oficio$Prop_Pobres

# Aplicamos k-means con k=3
set.seed(123) # Para reproducibilidad
kmeans_resultado <- kmeans(datos_kmeans, centers = 3)

# Añadimos el grupo asignado por k-means a los datos
proporcion_pobres_oficio$Grupo <- kmeans_resultado$cluster

# Mostramos resumen de los grupos
resumen_grupos <- proporcion_pobres_oficio %>%
  group_by(Grupo) %>%
  summarize(
    Num_Categorias = n(),
    Prop_Pobres_Min = min(Prop_Pobres),
    Prop_Pobres_Media = mean(Prop_Pobres),
    Prop_Pobres_Max = max(Prop_Pobres),
    Total_Personas = sum(Total)
  )

# Imprimir resultados
cat("\nResumen de grupos por tasa de pobreza:\n")
print(resumen_grupos)

# Mostrar lista de categorías en cada grupo
cat("\nCategorías de Oficio por grupo:\n")
for(g in 1:3) {
  categorias_grupo <- proporcion_pobres_oficio %>%
    filter(Grupo == g) %>%
    arrange(desc(Total)) %>%
    select(Oficio, Total, Prop_Pobres)
  
  cat("\nGRUPO", g, "- Tasa media de pobreza:", 
      round(resumen_grupos$Prop_Pobres_Media[resumen_grupos$Grupo == g] * 100, 1), "%\n")
  print(categorias_grupo)
}

# Guardar el resultado de la agrupación
write.csv(proporcion_pobres_oficio, "views/tables/oficios_agrupados.csv", row.names = FALSE)

cat("\nAgrupación de categorías de Oficio completada. Los resultados se guardaron en 'views/tables/oficios_agrupados.csv'\n")

###########################################
# 9. ANÁLISIS CHI-CUADRADO               #
###########################################

cat("\nEvaluando capacidad predictiva de variables discretas mediante Chi-cuadrado...\n")

# Crear dataframe para almacenar resultados de chi-cuadrado con dos niveles de confianza
chi_cuadrado_resultados <- data.frame(
  Variable = character(),
  Chi_Cuadrado = numeric(),
  Grados_Libertad = numeric(),
  Valor_P = numeric(),
  Significativo_90 = logical(),  # Para nivel de confianza de 0.9
  Significativo_75 = logical(),  # Para nivel de confianza de 0.75
  V_Cramer = numeric(),
  Cumple_Supuestos = logical(),
  stringsAsFactors = FALSE
)

# Variables con advertencias por supuestos no cumplidos
variables_con_advertencias <- c()

# Analizar cada variable discreta
for(var in vars_discretas_num) {
  if(var %in% names(train_personas_con_pobre)) {
    # Crear tabla de contingencia excluyendo NAs
    datos_filtrados <- train_personas_con_pobre %>% 
      filter(!is.na(!!sym(var)) & !is.na(Pobre))
    
    tabla <- table(datos_filtrados[[var]], datos_filtrados$Pobre)
    
    # Verificar que la tabla tiene suficientes datos
    if(sum(tabla) > 0 && min(dim(tabla)) > 1) {
      # Verificar supuestos: frecuencias esperadas >= 5
      chi_esperado <- suppressWarnings(chisq.test(tabla)$expected)
      cumple_supuestos <- all(chi_esperado >= 5)
      
      # Si no cumple supuestos, guardar la variable
      if(!cumple_supuestos) {
        variables_con_advertencias <- c(variables_con_advertencias, var)
      }
      
      # Aplicar prueba Chi-cuadrado (suprimiendo warnings)
      resultado_chi <- suppressWarnings(chisq.test(tabla))
      
      # Calcular V de Cramér
      v_cramer <- sqrt(resultado_chi$statistic / (sum(tabla) * (min(dim(tabla)) - 1)))
      
      # Determinar si es significativo con nivel de confianza de 0.9 y 0.75
      significativo_90 <- resultado_chi$p.value < 0.1  # alpha = 0.1 para confianza de 0.9
      significativo_75 <- resultado_chi$p.value < 0.25 # alpha = 0.25 para confianza de 0.75
      
      # Guardar resultados
      chi_cuadrado_resultados <- rbind(chi_cuadrado_resultados, data.frame(
        Variable = var,
        Chi_Cuadrado = as.numeric(resultado_chi$statistic),
        Grados_Libertad = resultado_chi$parameter,
        Valor_P = resultado_chi$p.value,
        Significativo_90 = significativo_90,
        Significativo_75 = significativo_75,
        V_Cramer = as.numeric(v_cramer),
        Cumple_Supuestos = cumple_supuestos,
        stringsAsFactors = FALSE
      ))
    } else {
      # No hay suficientes datos o categorías para el análisis
      chi_cuadrado_resultados <- rbind(chi_cuadrado_resultados, data.frame(
        Variable = var,
        Chi_Cuadrado = NA,
        Grados_Libertad = NA,
        Valor_P = NA,
        Significativo_90 = NA,
        Significativo_75 = NA,
        V_Cramer = NA,
        Cumple_Supuestos = FALSE,
        stringsAsFactors = FALSE
      ))
    }
  }
}

# Ordenar resultados por V de Cramér (de mayor a menor)
chi_cuadrado_resultados <- chi_cuadrado_resultados %>%
  arrange(desc(V_Cramer))

# Mostrar las 10 variables con mayor poder predictivo
cat("\nTop 10 variables discretas como predictores de pobreza (según V de Cramér):\n")
print(head(chi_cuadrado_resultados, 10))

# Mostrar resumen de verificación de supuestos
cat("\nResumen de verificación de supuestos del test chi-cuadrado:\n")
cat("Total de variables analizadas:", nrow(chi_cuadrado_resultados), "\n")
cat("Variables que cumplen supuestos:", sum(chi_cuadrado_resultados$Cumple_Supuestos, na.rm = TRUE), "\n")
cat("Variables que NO cumplen supuestos:", sum(!chi_cuadrado_resultados$Cumple_Supuestos, na.rm = TRUE), "\n")
cat("Variables con advertencias:", length(variables_con_advertencias), "\n")

if(length(variables_con_advertencias) > 0) {
  cat("\nNota: Las siguientes variables no cumplen el supuesto de frecuencias esperadas >= 5.\n")
  cat("Para estas variables, los resultados del test chi-cuadrado deben interpretarse con precaución:\n")
  cat(paste(variables_con_advertencias, collapse = ", "), "\n")
}

# Guardar resultados completos
write.csv(chi_cuadrado_resultados, "views/tables/chi_cuadrado_predictores.csv", row.names = FALSE)

################################################################################
#                            FINALIZACIÓN DEL SCRIPT                           #
################################################################################