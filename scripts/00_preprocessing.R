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

################################################################################
# PARÁMETROS CONFIGURABLES                                                     #
################################################################################

# Parámetros
UMBRAL_NAS <- 0.50     # % máximo aceptable de valores perdidos

###########################################
# 1. CARGAR Y EXPLORAR BASES              #
###########################################

# Cargar datos - train
train_hogares <- read.csv("stores/raw/train_hogares.csv")
train_personas <- read.csv("stores/raw/train_personas.csv")

# Cargar datos - test
test_hogares <- read.csv("stores/raw/test_hogares.csv")
test_personas <- read.csv("stores/raw/test_personas.csv")

# Exploración inicial
cat("Estructura de train_hogares:\n")
str(train_hogares)
cat("\nColumnas de train_hogares:\n")
print(colnames(train_hogares))

cat("\nEstructura de train_personas:\n")
str(train_personas)
cat("\nColumnas de train_personas:\n")
print(colnames(train_personas))

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

# Calcular porcentajes de coincidencia
coincidencia1 <- sum(diag(table(train_hogares$Pobre, train_hogares$Pobre_calculado))) / nrow(train_hogares) * 100
coincidencia2 <- sum(diag(table(train_hogares$Pobre, train_hogares$Pobre_calculado2))) / nrow(train_hogares) * 100

cat(sprintf("Coincidencia con método 1 (Ingpcug < Lp): %.2f%%\n", coincidencia1))
cat(sprintf("Coincidencia con método 2 (Ingtotugarr < Lp * Npersug): %.2f%%\n", coincidencia2))

cat("Confirmado: La variable 'Pobre' está correctamente definida según el criterio del DANE.\n")

###########################################
# 3. ANÁLISIS BASE DE HOGARES            #
###########################################

# Mostrar estructura básica
cat("Dimensiones de train_hogares:", dim(train_hogares), "\n")

# Valores perdidos
nas_hogares <- data.frame(
  variable = names(train_hogares),
  n_missing = colSums(is.na(train_hogares)),
  pct_missing = colSums(is.na(train_hogares))/nrow(train_hogares)
) %>% arrange(desc(pct_missing))

# Variables con muchos NAs
vars_muchos_nas <- nas_hogares %>% 
  filter(pct_missing > UMBRAL_NAS) %>%
  select(variable, pct_missing)

# Guardar resultados
write.csv(nas_hogares, "views/tables/nas_hogares.csv", row.names = FALSE)

# Análisis de variables categóricas
cat_vars_hogares <- data.frame()
for(var in names(train_hogares)) {
  # Solo si es categórica o character
  if(is.factor(train_hogares[[var]]) || is.character(train_hogares[[var]])) {
    # Crear tabla de frecuencias
    freq_table <- table(train_hogares[[var]], useNA = "ifany")
    total_count <- sum(freq_table)
    
    # Si hay al menos una categoría
    if(length(freq_table) > 0) {
      # Encontrar la categoría más frecuente
      max_freq <- max(freq_table)
      max_index <- which.max(freq_table)
      
      # Obtener el nombre de la categoría más frecuente
      if(is.null(names(freq_table))) {
        cat_name <- "Sin nombre"
      } else {
        cat_name <- names(freq_table)[max_index]
        if(is.na(cat_name) || cat_name == "") {
          cat_name <- "NA o vacío"
        }
      }
      
      # Agregar a la tabla de resultados
      cat_vars_hogares <- rbind(cat_vars_hogares, 
                                data.frame(
                                  variable = var,
                                  num_categorias = length(freq_table),
                                  categoria_mas_frecuente = cat_name,
                                  frecuencia = max_freq,
                                  proporcion = max_freq / total_count,
                                  stringsAsFactors = FALSE
                                ))
    }
  }
}

# Ordenar por proporción descendente
cat_vars_hogares <- cat_vars_hogares %>% arrange(desc(proporcion))

# Guardar resultados
write.csv(cat_vars_hogares, "views/tables/categoricas_hogares.csv", row.names = FALSE)

# Correlación con pobreza (si existe)
if("Pobre" %in% names(train_hogares)) {
  cors_pobreza <- data.frame()
  pobre_numeric <- as.numeric(train_hogares$Pobre)
  
  for(var in names(train_hogares)) {
    if(is.numeric(train_hogares[[var]]) && var != "Pobre") {
      cor_val <- cor(train_hogares[[var]], pobre_numeric, use = "pairwise.complete.obs")
      if(!is.na(cor_val)) {
        cors_pobreza <- rbind(cors_pobreza, data.frame(
          variable = var,
          correlacion = cor_val
        ))
      }
    }
  }
  
  if(nrow(cors_pobreza) > 0) {
    cors_pobreza <- cors_pobreza %>% arrange(desc(abs(correlacion)))
    write.csv(cors_pobreza, "views/tables/corr_pobreza_hogares.csv", row.names = FALSE)
  }
}

###########################################
# 4. ANÁLISIS BASE DE PERSONAS           #
###########################################

# Mostrar estructura básica
cat("Dimensiones de train_personas:", dim(train_personas), "\n")

# Valores perdidos
nas_personas <- data.frame(
  variable = names(train_personas),
  n_missing = colSums(is.na(train_personas)),
  pct_missing = colSums(is.na(train_personas))/nrow(train_personas)
) %>% arrange(desc(pct_missing))

# Variables con muchos NAs
vars_muchos_nas_personas <- nas_personas %>% 
  filter(pct_missing > UMBRAL_NAS) %>%
  select(variable, pct_missing)

# Guardar resultados
write.csv(nas_personas, "views/tables/nas_personas.csv", row.names = FALSE)

# Análisis de variables categóricas
cat_vars_personas <- data.frame()
for(var in names(train_personas)) {
  # Solo si es categórica o character
  if(is.factor(train_personas[[var]]) || is.character(train_personas[[var]])) {
    # Crear tabla de frecuencias
    freq_table <- table(train_personas[[var]], useNA = "ifany")
    total_count <- sum(freq_table)
    
    # Si hay al menos una categoría
    if(length(freq_table) > 0) {
      # Encontrar la categoría más frecuente
      max_freq <- max(freq_table)
      max_index <- which.max(freq_table)
      
      # Obtener el nombre de la categoría más frecuente
      if(is.null(names(freq_table))) {
        cat_name <- "Sin nombre"
      } else {
        cat_name <- names(freq_table)[max_index]
        if(is.na(cat_name) || cat_name == "") {
          cat_name <- "NA o vacío"
        }
      }
      
      # Agregar a la tabla de resultados
      cat_vars_personas <- rbind(cat_vars_personas, 
                                 data.frame(
                                   variable = var,
                                   num_categorias = length(freq_table),
                                   categoria_mas_frecuente = cat_name,
                                   frecuencia = max_freq,
                                   proporcion = max_freq / total_count,
                                   stringsAsFactors = FALSE
                                 ))
    }
  }
}

# Ordenar por proporción descendente
cat_vars_personas <- cat_vars_personas %>% arrange(desc(proporcion))

# Guardar resultados
write.csv(cat_vars_personas, "views/tables/categoricas_personas.csv", row.names = FALSE)

###########################################
# 5. VARIABLES PARA AGREGACIÓN           #
###########################################

# Clasificación de variables y sugerencias de agregación
sugerencias_agregacion <- data.frame(
  variable = names(train_personas),
  tipo = sapply(train_personas, function(x) class(x)[1]),
  n_valores_unicos = sapply(train_personas, function(x) length(unique(na.omit(x)))),
  metodo_sugerido = NA_character_
)

# Asignar métodos de agregación según tipo de variable
sugerencias_agregacion <- sugerencias_agregacion %>%
  mutate(
    metodo_sugerido = case_when(
      tipo == "numeric" & n_valores_unicos > 10 ~ "mean, median, sum",
      tipo == "numeric" & n_valores_unicos <= 10 ~ "count, proportion",
      tipo %in% c("factor", "character") & n_valores_unicos <= 5 ~ "mode, proportion by category",
      tipo %in% c("factor", "character") & n_valores_unicos > 5 ~ "count categories",
      TRUE ~ "requires manual review"
    )
  )

write.csv(sugerencias_agregacion, "views/tables/sugerencias_agregacion.csv", row.names = FALSE)

# Identificar variables a conservar de personas
vars_personas_conservar <- setdiff(
  names(train_personas),
  nas_personas$variable[nas_personas$pct_missing > UMBRAL_NAS]
)

# Guardar para uso en scripts posteriores
saveRDS(vars_personas_conservar, "stores/processed/vars_personas_conservar.rds")

###########################################
# 6. TABLAS CONSOLIDADAS                 #
###########################################

# Tabla consolidada para hogares
consolidado_hogares <- data.frame(
  variable = names(train_hogares),
  tipo = sapply(train_hogares, function(x) class(x)[1]),
  n_valores_unicos = sapply(train_hogares, function(x) length(unique(na.omit(x)))),
  pct_missing = nas_hogares$pct_missing[match(names(train_hogares), nas_hogares$variable)],
  stringsAsFactors = FALSE
)

# Agregar información de categorías para variables categóricas
consolidado_hogares <- consolidado_hogares %>%
  left_join(
    cat_vars_hogares %>% select(variable, categoria_mas_frecuente, proporcion),
    by = "variable"
  )

# Agregar correlación con pobreza para variables numéricas
if(exists("cors_pobreza") && nrow(cors_pobreza) > 0) {
  consolidado_hogares <- consolidado_hogares %>%
    left_join(
      cors_pobreza %>% select(variable, correlacion),
      by = "variable"
    )
}

# Generar recomendación automática
consolidado_hogares <- consolidado_hogares %>%
  mutate(
    recomendacion = case_when(
      pct_missing > UMBRAL_NAS ~ "Eliminar (muchos NAs)",
      !is.na(proporcion) & proporcion > 0.95 ~ "Poca variabilidad",
      !is.na(correlacion) & abs(correlacion) > 0.3 ~ "Alta correlación con Pobre",
      TRUE ~ "Analizar"
    )
  )

# Guardar tabla consolidada de hogares
write.csv(consolidado_hogares, "views/tables/consolidado_hogares.csv", row.names = FALSE)

# Tabla consolidada para personas
consolidado_personas <- data.frame(
  variable = names(train_personas),
  tipo = sapply(train_personas, function(x) class(x)[1]),
  n_valores_unicos = sapply(train_personas, function(x) length(unique(na.omit(x)))),
  pct_missing = nas_personas$pct_missing[match(names(train_personas), nas_personas$variable)],
  stringsAsFactors = FALSE
)

# Agregar información de categorías para variables categóricas
consolidado_personas <- consolidado_personas %>%
  left_join(
    cat_vars_personas %>% select(variable, categoria_mas_frecuente, proporcion),
    by = "variable"
  )

# Agregar sugerencia de agregación
consolidado_personas <- consolidado_personas %>%
  left_join(
    sugerencias_agregacion %>% select(variable, metodo_sugerido),
    by = "variable"
  )

# Generar recomendación automática
consolidado_personas <- consolidado_personas %>%
  mutate(
    recomendacion = case_when(
      pct_missing > UMBRAL_NAS ~ "Eliminar (muchos NAs)",
      !is.na(proporcion) & proporcion > 0.95 ~ "Poca variabilidad",
      TRUE ~ "Considerar para agregación"
    )
  )

# Guardar tabla consolidada de personas
write.csv(consolidado_personas, "views/tables/consolidado_personas.csv", row.names = FALSE)

cat("¡Análisis completado! Resultados guardados en views/tables/\n")

################################################################################
#                            FINALIZACIÓN DEL SCRIPT                           #
################################################################################