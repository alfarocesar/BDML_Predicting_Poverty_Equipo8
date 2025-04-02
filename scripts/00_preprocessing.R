################################################################################
# TÍTULO: 00_preprocessing.R                                   #
# PROYECTO: Predicción de Pobreza en Colombia                                 #
# DESCRIPCIÓN: Análisis independiente de las bases de datos de hogares y      #
#              personas antes de su unión.                                     #
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
  tidyverse,   # Manipulación de datos y visualización
  skimr,       # Resumen de datos
  knitr,       # Para generar tablas
  xtable,      # Para exportar tablas a LaTeX
  kableExtra,  # Personalización avanzada de tablas
  stargazer,   # Tablas estadísticas en formato LaTeX
  janitor,     # Limpieza de datos y tablas
  gridExtra,   # Para combinar gráficos
  corrplot     # Visualización de correlaciones
)

# Fijar semilla para reproducibilidad
set.seed(123)

################################################################################
# PARÁMETROS CONFIGURABLES                                                     #
################################################################################

# Umbrales para análisis
UMBRAL_VALORES_PERDIDOS <- 0.50      # % máximo aceptable de valores perdidos
UMBRAL_CATEGORIA_DOMINANTE <- 0.80   # % máximo para categoría dominante
MIN_VARIABILIDAD <- 0.05             # Coeficiente de variación mínimo para variables numéricas
MIN_OBSERVACIONES_CATEGORIA <- 30    # Mínimo de observaciones por categoría

# Crear directorio para tablas si no existe
dir.create("views/tables", recursive = TRUE, showWarnings = FALSE)

################################################################################
# DEFINICIÓN DE FUNCIONES GENÉRICAS PARA ANÁLISIS                              #
################################################################################

#' Analiza los valores perdidos en un dataframe
#' 
#' @param data Dataframe a analizar
#' @param umbral Porcentaje máximo permitido de valores perdidos (0-1)
#' @return Dataframe con variables que tienen valores perdidos y su porcentaje
analizar_valores_perdidos <- function(data, umbral = UMBRAL_VALORES_PERDIDOS) {
  n_obs <- nrow(data)
  
  # Calcular el número y porcentaje de valores perdidos por variable
  nas_por_variable <- data.frame(
    variable = names(data),
    n_missing = sapply(data, function(x) sum(is.na(x))),
    pct_missing = sapply(data, function(x) sum(is.na(x)) / n_obs),
    stringsAsFactors = FALSE
  ) %>%
    arrange(desc(pct_missing))
  
  # Identificar variables que exceden el umbral
  nas_por_variable$excede_umbral <- nas_por_variable$pct_missing > umbral
  
  return(nas_por_variable)
}

#' Analiza las variables categóricas para identificar categorías dominantes
#' 
#' @param data Dataframe a analizar
#' @param umbral Porcentaje máximo permitido para una categoría (0-1)
#' @return Dataframe con variables categóricas que tienen categorías dominantes
analizar_categorias_dominantes <- function(data, umbral = UMBRAL_CATEGORIA_DOMINANTE) {
  # Identificar variables categóricas y de character
  vars_categoricas <- names(data)[sapply(data, function(x) is.factor(x) || is.character(x))]
  resultado <- data.frame()
  
  # Analizar cada variable categórica
  for(var in vars_categoricas) {
    # Crear tabla de frecuencias incluyendo NA
    freq_table <- table(data[[var]], useNA = "always")
    total_values <- sum(freq_table)
    
    # Calcular porcentaje para cada categoría
    freq_percentage <- freq_table / total_values
    
    # Encontrar la categoría más frecuente (excluyendo NA)
    # Si todas son NA, manejarlo apropiadamente
    if(length(freq_table) > 1) {
      # Excluir NA para determinar la categoría más frecuente
      freq_without_na <- freq_table[!is.na(names(freq_table))]
      if(length(freq_without_na) > 0) {
        max_category <- names(freq_without_na)[which.max(freq_without_na)]
        max_freq <- max(freq_without_na)
        max_percentage <- max_freq / total_values
        
        # Añadir al resultado si supera el umbral
        if(max_percentage > umbral) {
          temp_df <- data.frame(
            variable = var,
            categoria_dominante = max_category,
            frecuencia = max_freq,
            porcentaje = max_percentage,
            n_categorias = length(freq_without_na),
            stringsAsFactors = FALSE
          )
          resultado <- rbind(resultado, temp_df)
        }
      }
    }
  }
  
  # Ordenar por porcentaje descendente
  if(nrow(resultado) > 0) {
    resultado <- resultado %>% arrange(desc(porcentaje))
  }
  
  return(resultado)
}

#' Analiza la variabilidad de variables numéricas
#' 
#' @param data Dataframe a analizar
#' @param min_cv Coeficiente de variación mínimo aceptable
#' @return Dataframe con variables numéricas que tienen baja variabilidad
analizar_variabilidad_numericas <- function(data, min_cv = MIN_VARIABILIDAD) {
  # Identificar variables numéricas
  vars_numericas <- names(data)[sapply(data, is.numeric)]
  resultado <- data.frame()
  
  # Analizar cada variable numérica
  for(var in vars_numericas) {
    # Extraer valores no NA
    values <- data[[var]][!is.na(data[[var]])]
    
    # Calcular estadísticas básicas si hay suficientes valores
    if(length(values) > 0) {
      media <- mean(values)
      desv_est <- sd(values)
      
      # Evitar división por cero
      if(media != 0) {
        cv <- desv_est / abs(media)  # Coeficiente de variación
      } else {
        cv <- Inf  # Infinito si la media es cero
      }
      
      # Añadir al resultado si tiene baja variabilidad
      temp_df <- data.frame(
        variable = var,
        media = media,
        desviacion_estandar = desv_est,
        coef_variacion = cv,
        min = min(values),
        max = max(values),
        rango = max(values) - min(values),
        baja_variabilidad = cv < min_cv,
        stringsAsFactors = FALSE
      )
      resultado <- rbind(resultado, temp_df)
    }
  }
  
  # Ordenar por coeficiente de variación
  if(nrow(resultado) > 0) {
    resultado <- resultado %>% arrange(coef_variacion)
  }
  
  return(resultado)
}

#' Analiza la relación entre variables categóricas y la variable objetivo
#' 
#' @param data Dataframe a analizar
#' @param var_categorica Nombre de la variable categórica
#' @param var_objetivo Nombre de la variable objetivo
#' @param min_obs Mínimo de observaciones por categoría
#' @return Dataframe con tasas de pobreza por categoría
analizar_relacion_categorica_objetivo <- function(data, var_categorica, var_objetivo, 
                                                  min_obs = MIN_OBSERVACIONES_CATEGORIA) {
  # Verificar que ambas variables existen
  if(!(var_categorica %in% names(data)) || !(var_objetivo %in% names(data))) {
    return(NULL)
  }
  
  # Calcular tasas por categoría
  resultado <- data %>%
    group_by_at(var_categorica) %>%
    summarise(
      n_total = n(),
      n_positivos = sum(get(var_objetivo) == 1, na.rm = TRUE),
      tasa_positivos = mean(get(var_objetivo) == 1, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    filter(n_total >= min_obs) %>%  # Filtrar categorías con pocas observaciones
    arrange(desc(tasa_positivos))
  
  return(resultado)
}

#' Analiza potenciales variables para agregación desde nivel individuo a hogar
#' 
#' @param data Dataframe de individuos
#' @param id_hogar Nombre de la variable identificadora de hogar
#' @param vars Vector de nombres de variables a analizar
#' @return Lista con recomendaciones de agregación por variable
analizar_agregacion_persona_hogar <- function(data, id_hogar, vars) {
  # Verificar que la variable id existe
  if(!(id_hogar %in% names(data))) {
    stop("La variable ID de hogar no existe en el dataframe")
  }
  
  resultados <- list()
  
  # Para cada variable, determinar posible método de agregación
  for(var in vars) {
    if(var %in% names(data)) {
      # Ignorar variable id
      if(var == id_hogar) next
      
      # Determinar tipo de variable
      if(is.numeric(data[[var]])) {
        # Variables numéricas: promedio, suma, min, max, etc.
        recomendacion <- "Media, suma, mínimo, máximo, o desviación estándar"
      } else if(is.factor(data[[var]]) || is.character(data[[var]])) {
        # Variables categóricas: moda, conteo por categoría, etc.
        recomendacion <- "Conteo, proporción, o moda"
      } else {
        recomendacion <- "Tipo de variable no estándar, revisar manualmente"
      }
      
      resultados[[var]] <- recomendacion
    }
  }
  
  # Convertir a dataframe para mejor visualización
  df_resultados <- data.frame(
    variable = names(resultados),
    recomendacion_agregacion = unlist(resultados),
    stringsAsFactors = FALSE
  )
  
  return(df_resultados)
}

#' Genera una tabla resumen de potencial predictivo para variables
#' 
#' @param data Dataframe a analizar
#' @param var_objetivo Nombre de la variable objetivo
#' @return Dataframe con métricas de potencial predictivo
evaluar_potencial_predictivo <- function(data, var_objetivo) {
  # Verificar que la variable objetivo existe
  if(!(var_objetivo %in% names(data))) {
    stop("La variable objetivo no existe en el dataframe")
  }
  
  variables <- names(data)[names(data) != var_objetivo]
  resultados <- data.frame()
  
  # Para cada variable, evaluar potencial predictivo
  for(var in variables) {
    # Ignorar si hay menos de 30 observaciones no-NA
    if(sum(!is.na(data[[var]])) < 30) next
    
    if(is.numeric(data[[var]])) {
      # Para variables numéricas: correlación
      corr <- cor(data[[var]], data[[var_objetivo]], 
                  use = "pairwise.complete.obs", method = "spearman")
      
      temp_df <- data.frame(
        variable = var,
        tipo = "numérica",
        metrica = "correlación",
        valor = corr,
        abs_valor = abs(corr),
        stringsAsFactors = FALSE
      )
      resultados <- rbind(resultados, temp_df)
      
    } else if(is.factor(data[[var]]) || is.character(data[[var]])) {
      # Para variables categóricas: ratio de información
      # Simplificado como varianza de las proporciones
      proporciones <- tapply(data[[var_objetivo]], data[[var]], mean, na.rm = TRUE)
      varianza_prop <- var(proporciones, na.rm = TRUE)
      
      temp_df <- data.frame(
        variable = var,
        tipo = "categórica",
        metrica = "varianza_proporciones",
        valor = varianza_prop,
        abs_valor = abs(varianza_prop),
        stringsAsFactors = FALSE
      )
      resultados <- rbind(resultados, temp_df)
    }
  }
  
  # Ordenar por valor absoluto descendente
  if(nrow(resultados) > 0) {
    resultados <- resultados %>% arrange(desc(abs_valor))
  }
  
  return(resultados)
}

################################################################################
# SECCIÓN 1: CARGA Y ANÁLISIS INICIAL DE DATOS DE HOGARES                      #
################################################################################

cat("\n\n========== ANÁLISIS DE BASE DE DATOS DE HOGARES ==========\n\n")

# Cargar datos de hogares
cat("Cargando datos de hogares...\n")
train_hogares <- read.csv("stores/raw/train_hogares.csv", stringsAsFactors = FALSE)

# Dimensiones y estructura
cat("\nDimensiones de train_hogares:", dim(train_hogares), "\n")
cat("Estructura de datos:\n")
str(train_hogares, list.len = 10)  # Mostrar solo primeras 10 variables para brevedad

# Resumen de tipos de variables
tipos_variables_hogares <- sapply(train_hogares, class)
resumen_tipos_hogares <- table(tipos_variables_hogares)
cat("\nResumen de tipos de variables en hogares:\n")
print(resumen_tipos_hogares)

# Guardar datos básicos en archivo
info_basica_hogares <- data.frame(
  nombre_base = "train_hogares",
  n_observaciones = nrow(train_hogares),
  n_variables = ncol(train_hogares),
  n_vars_numericas = sum(sapply(train_hogares, is.numeric)),
  n_vars_categoricas = sum(sapply(train_hogares, function(x) is.factor(x) || is.character(x))),
  stringsAsFactors = FALSE
)

print(xtable(info_basica_hogares, 
             caption = "Información Básica de Base de Hogares", 
             label = "tab:info_basica_hogares"),
      file = "views/tables/info_basica_hogares.tex",
      include.rownames = FALSE,
      floating = TRUE,
      latex.environments = "center",
      booktabs = TRUE)

################################################################################
# SECCIÓN 2: ANÁLISIS DE VALORES PERDIDOS EN HOGARES                           #
################################################################################

cat("\n\n========== ANÁLISIS DE VALORES PERDIDOS EN HOGARES ==========\n\n")

# Análisis de valores perdidos
nas_hogares <- analizar_valores_perdidos(train_hogares, UMBRAL_VALORES_PERDIDOS)

# Imprimir resultados de valores perdidos
cat("\nVariables con valores perdidos (ordenadas por porcentaje):\n")
print(head(nas_hogares, 20))

# Identificar variables que exceden el umbral de valores perdidos
vars_exceden_umbral_nas_hogares <- nas_hogares %>%
  filter(excede_umbral == TRUE) %>%
  select(variable, n_missing, pct_missing)

cat("\nVariables que exceden el umbral de", UMBRAL_VALORES_PERDIDOS * 100, 
    "% de valores perdidos:\n")
if(nrow(vars_exceden_umbral_nas_hogares) > 0) {
  print(vars_exceden_umbral_nas_hogares)
} else {
  cat("Ninguna variable excede el umbral.\n")
}

# Guardar resultados completos
write.csv(nas_hogares, "views/tables/valores_perdidos_hogares.csv", row.names = FALSE)

# Crear versión para LaTeX
print(xtable(head(nas_hogares %>% select(-excede_umbral), 20), 
             caption = "Valores Perdidos en Variables de Hogares (Top 20)", 
             label = "tab:nas_hogares"),
      file = "views/tables/valores_perdidos_hogares.tex",
      include.rownames = FALSE,
      floating = TRUE,
      latex.environments = "center",
      booktabs = TRUE)

################################################################################
# SECCIÓN 3: ANÁLISIS DE VARIABLES CATEGÓRICAS EN HOGARES                      #
################################################################################

cat("\n\n========== ANÁLISIS DE VARIABLES CATEGÓRICAS EN HOGARES ==========\n\n")

# Identificar variables categóricas con categorías dominantes
cat_dominantes_hogares <- analizar_categorias_dominantes(train_hogares, UMBRAL_CATEGORIA_DOMINANTE)

# Imprimir resultados
cat("\nVariables categóricas con categorías dominantes (>", 
    UMBRAL_CATEGORIA_DOMINANTE * 100, "%):\n")
if(nrow(cat_dominantes_hogares) > 0) {
  print(cat_dominantes_hogares)
} else {
  cat("Ninguna variable categórica tiene una categoría dominante.\n")
}

# Guardar resultados
write.csv(cat_dominantes_hogares, "views/tables/categorias_dominantes_hogares.csv", row.names = FALSE)

# Crear versión para LaTeX
if(nrow(cat_dominantes_hogares) > 0) {
  print(xtable(cat_dominantes_hogares, 
               caption = "Variables Categóricas con Categorías Dominantes en Hogares", 
               label = "tab:cat_dominantes_hogares"),
        file = "views/tables/categorias_dominantes_hogares.tex",
        include.rownames = FALSE,
        floating = TRUE,
        latex.environments = "center",
        booktabs = TRUE)
}

################################################################################
# SECCIÓN 4: ANÁLISIS DE VARIABLES NUMÉRICAS EN HOGARES                        #
################################################################################

cat("\n\n========== ANÁLISIS DE VARIABLES NUMÉRICAS EN HOGARES ==========\n\n")

# Analizar variabilidad de variables numéricas
var_numericas_hogares <- analizar_variabilidad_numericas(train_hogares, MIN_VARIABILIDAD)

# Identificar variables con baja variabilidad
vars_baja_var_hogares <- var_numericas_hogares %>%
  filter(baja_variabilidad == TRUE) %>%
  select(variable, media, desviacion_estandar, coef_variacion)

# Imprimir resultados
cat("\nVariables numéricas con baja variabilidad (CV <", MIN_VARIABILIDAD, "):\n")
if(nrow(vars_baja_var_hogares) > 0) {
  print(vars_baja_var_hogares)
} else {
  cat("Ninguna variable numérica tiene baja variabilidad.\n")
}

# Guardar resultados
write.csv(var_numericas_hogares, "views/tables/variabilidad_numericas_hogares.csv", row.names = FALSE)

# Crear versión para LaTeX
print(xtable(head(var_numericas_hogares %>% select(-baja_variabilidad), 20), 
             caption = "Variabilidad de Variables Numéricas en Hogares (Top 20)", 
             label = "tab:var_numericas_hogares"),
      file = "views/tables/variabilidad_numericas_hogares.tex",
      include.rownames = FALSE,
      floating = TRUE,
      latex.environments = "center",
      booktabs = TRUE)

################################################################################
# SECCIÓN 5: ANÁLISIS DE RELACIÓN CON VARIABLE OBJETIVO EN HOGARES            #
################################################################################

cat("\n\n========== ANÁLISIS DE RELACIÓN CON POBREZA EN HOGARES ==========\n\n")

# Verificar si existe la variable 'Pobre'
if("Pobre" %in% names(train_hogares)) {
  # Relación de variables numéricas con pobreza
  # Identificar variables numéricas
  vars_numericas <- names(train_hogares)[sapply(train_hogares, is.numeric)]
  vars_numericas <- vars_numericas[vars_numericas != "Pobre"]
  
  # Calcular correlación con pobreza
  cors_pobreza <- sapply(vars_numericas, function(var) {
    cor(train_hogares[[var]], train_hogares$Pobre, use = "pairwise.complete.obs")
  })
  
  # Crear dataframe de correlaciones
  df_cors_pobreza <- data.frame(
    variable = names(cors_pobreza),
    correlacion = unname(cors_pobreza),
    stringsAsFactors = FALSE
  ) %>%
    filter(!is.na(correlacion)) %>%
    arrange(desc(abs(correlacion)))
  
  # Imprimir top 10 correlaciones
  cat("\nTop 10 variables numéricas más correlacionadas con pobreza:\n")
  print(head(df_cors_pobreza, 10))
  
  # Guardar resultados
  write.csv(df_cors_pobreza, "views/tables/correlacion_pobreza_hogares.csv", row.names = FALSE)
  
  # Crear versión para LaTeX
  print(xtable(head(df_cors_pobreza, 15), 
               caption = "Correlación entre Variables Numéricas y Pobreza en Hogares", 
               label = "tab:cors_pobreza_hogares"),
        file = "views/tables/correlacion_pobreza_hogares.tex",
        include.rownames = FALSE,
        floating = TRUE,
        latex.environments = "center",
        booktabs = TRUE)
  
  # Análisis de variables categóricas clave
  # Identificar variables categóricas
  vars_categoricas <- names(train_hogares)[sapply(train_hogares, function(x) is.factor(x) || is.character(x))]
  
  # Para cada variable categórica, analizar relación con pobreza
  cat_relacion_pobreza <- data.frame()
  
  for(var in vars_categoricas) {
    relacion <- analizar_relacion_categorica_objetivo(train_hogares, var, "Pobre")
    
    if(!is.null(relacion) && nrow(relacion) > 1) {
      # Calcular variabilidad de tasas
      var_tasas <- var(relacion$tasa_positivos)
      rango_tasas <- max(relacion$tasa_positivos) - min(relacion$tasa_positivos)
      
      temp_df <- data.frame(
        variable = var,
        n_categorias = nrow(relacion),
        variabilidad_tasas = var_tasas,
        rango_tasas = rango_tasas,
        stringsAsFactors = FALSE
      )
      
      cat_relacion_pobreza <- rbind(cat_relacion_pobreza, temp_df)
    }
  }
  
  # Ordenar por variabilidad de tasas
  if(nrow(cat_relacion_pobreza) > 0) {
    cat_relacion_pobreza <- cat_relacion_pobreza %>% arrange(desc(rango_tasas))
    
    # Imprimir resultados
    cat("\nVariables categóricas con mayor variabilidad en tasas de pobreza:\n")
    print(cat_relacion_pobreza)
    
    # Guardar resultados
    write.csv(cat_relacion_pobreza, "views/tables/categoricas_pobreza_hogares.csv", row.names = FALSE)
    
    # Crear versión para LaTeX
    print(xtable(cat_relacion_pobreza, 
                 caption = "Variabilidad de Tasas de Pobreza por Variable Categórica en Hogares", 
                 label = "tab:cat_pobreza_hogares"),
          file = "views/tables/categoricas_pobreza_hogares.tex",
          include.rownames = FALSE,
          floating = TRUE,
          latex.environments = "center",
          booktabs = TRUE)
  }
  
  # Evaluar potencial predictivo general
  potencial_predictivo <- evaluar_potencial_predictivo(train_hogares, "Pobre")
  
  # Guardar resultados
  write.csv(potencial_predictivo, "views/tables/potencial_predictivo_hogares.csv", row.names = FALSE)
  
  # Crear versión para LaTeX
  print(xtable(head(potencial_predictivo, 20), 
               caption = "Potencial Predictivo de Variables para Pobreza en Hogares", 
               label = "tab:potencial_hogares"),
        file = "views/tables/potencial_predictivo_hogares.tex",
        include.rownames = FALSE,
        floating = TRUE,
        latex.environments = "center",
        booktabs = TRUE)
} else {
  cat("La variable 'Pobre' no existe en el dataframe de hogares.\n")
}

################################################################################
# SECCIÓN 6: RESUMEN HOGARES PARA TOMA DE DECISIONES                           #
################################################################################

cat("\n\n========== RESUMEN DE HALLAZGOS EN HOGARES ==========\n\n")

# Crear lista de variables problemáticas
vars_problematicas_hogares <- data.frame(
  variable = character(),
  criterio = character(),
  stringsAsFactors = FALSE
)

# Variables con muchos valores perdidos
if(nrow(vars_exceden_umbral_nas_hogares) > 0) {
  temp <- data.frame(
    variable = vars_exceden_umbral_nas_hogares$variable,
    criterio = paste("Valores perdidos:", round(vars_exceden_umbral_nas_hogares$pct_missing * 100, 1), "%"),
    stringsAsFactors = FALSE
  )
  vars_problematicas_hogares <- rbind(vars_problematicas_hogares, temp)
}

# Variables categóricas con categorías dominantes
if(nrow(cat_dominantes_hogares) > 0) {
  temp <- data.frame(
    variable = cat_dominantes_hogares$variable,
    criterio = paste("Categoría dominante:", 
                     cat_dominantes_hogares$categoria_dominante, "-", 
                     round(cat_dominantes_hogares$porcentaje * 100, 1), "%"),
    stringsAsFactors = FALSE
  )
  vars_problematicas_hogares <- rbind(vars_problematicas_hogares, temp)
}

# Variables numéricas con baja variabilidad
if(nrow(vars_baja_var_hogares) > 0) {
  temp <- data.frame(
    variable = vars_baja_var_hogares$variable,
    criterio = paste("Baja variabilidad: CV =", round(vars_baja_var_hogares$coef_variacion, 4)),
    stringsAsFactors = FALSE
  )
  vars_problematicas_hogares <- rbind(vars_problematicas_hogares, temp)
}

# Imprimir resumen
cat("\nVariables potencialmente problemáticas en hogares:\n")
if(nrow(vars_problematicas_hogares) > 0) {
  print(vars_problematicas_hogares)
} else {
  cat("No se identificaron variables problemáticas según los criterios establecidos.\n")
}

# Guardar resumen
write.csv(vars_problematicas_hogares, "views/tables/variables_problematicas_hogares.csv", row.names = FALSE)

# Crear versión para LaTeX
if(nrow(vars_problematicas_hogares) > 0) {
  print(xtable(vars_problematicas_hogares, 
               caption = "Variables Potencialmente Problemáticas en Hogares", 
               label = "tab:problematicas_hogares"),
        file = "views/tables/variables_problematicas_hogares.tex",
        include.rownames = FALSE,
        floating = TRUE,
        latex.environments = "center",
        booktabs = TRUE)
}

# Limpiar objetos de hogares para liberar memoria
cat("\nLiberando memoria de objetos de hogares...\n")
rm(list = ls()[grepl("hogares|nas_hogares|cat_dominantes|var_numericas|cors_pobreza|cat_relacion|vars_exceden|vars_baja_var|potencial_predictivo", ls())])
gc()

################################################################################
# SECCIÓN 7: CARGA Y ANÁLISIS INICIAL DE DATOS DE PERSONAS                     #
################################################################################

cat("\n\n========== ANÁLISIS DE BASE DE DATOS DE PERSONAS ==========\n\n")

# Cargar datos de personas
cat("Cargando datos de personas...\n")
train_personas <- read.csv("stores/raw/train_personas.csv", stringsAsFactors = FALSE)

# Dimensiones y estructura
cat("\nDimensiones de train_personas:", dim(train_personas), "\n")
cat("Estructura de datos:\n")
str(train_personas, list.len = 10)  # Mostrar solo primeras 10 variables para brevedad

# Resumen de tipos de variables
tipos_variables_personas <- sapply(train_personas, class)
resumen_tipos_personas <- table(tipos_variables_personas)
cat("\nResumen de tipos de variables en personas:\n")
print(resumen_tipos_personas)

# Guardar datos básicos en archivo
info_basica_personas <- data.frame(
  nombre_base = "train_personas",
  n_observaciones = nrow(train_personas),
  n_variables = ncol(train_personas),
  n_vars_numericas = sum(sapply(train_personas, is.numeric)),
  n_vars_categoricas = sum(sapply(train_personas, function(x) is.factor(x) || is.character(x))),
  stringsAsFactors = FALSE
)

print(xtable(info_basica_personas, 
             caption = "Información Básica de Base de Personas", 
             label = "tab:info_basica_personas"),
      file = "views/tables/info_basica_personas.tex",
      include.rownames = FALSE,
      floating = TRUE,
      latex.environments = "center",
      booktabs = TRUE)

################################################################################
# SECCIÓN 8: ANÁLISIS DE VALORES PERDIDOS EN PERSONAS                          #
################################################################################

cat("\n\n========== ANÁLISIS DE VALORES PERDIDOS EN PERSONAS ==========\n\n")

# Análisis de valores perdidos
nas_personas <- analizar_valores_perdidos(train_personas, UMBRAL_VALORES_PERDIDOS)

# Imprimir resultados de valores perdidos
cat("\nVariables con valores perdidos (ordenadas por porcentaje):\n")
print(head(nas_personas, 20))

# Identificar variables que exceden el umbral de valores perdidos
vars_exceden_umbral_nas_personas <- nas_personas %>%
  filter(excede_umbral == TRUE) %>%
  select(variable, n_missing, pct_missing)

cat("\nVariables que exceden el umbral de", UMBRAL_VALORES_PERDIDOS * 100, 
    "% de valores perdidos:\n")
if(nrow(vars_exceden_umbral_nas_personas) > 0) {
  print(vars_exceden_umbral_nas_personas)
} else {
  cat("Ninguna variable excede el umbral.\n")
}

# Guardar resultados completos
write.csv(nas_personas, "views/tables/valores_perdidos_personas.csv", row.names = FALSE)

# Crear versión para LaTeX
print(xtable(head(nas_personas %>% select(-excede_umbral), 20), 
             caption = "Valores Perdidos en Variables de Personas (Top 20)", 
             label = "tab:nas_personas"),
      file = "views/tables/valores_perdidos_personas.tex",
      include.rownames = FALSE,
      floating = TRUE,
      latex.environments = "center",
      booktabs = TRUE)

################################################################################
# SECCIÓN 9: ANÁLISIS DE VARIABLES CATEGÓRICAS EN PERSONAS                     #
################################################################################

cat("\n\n========== ANÁLISIS DE VARIABLES CATEGÓRICAS EN PERSONAS ==========\n\n")

# Identificar variables categóricas con categorías dominantes
cat_dominantes_personas <- analizar_categorias_dominantes(train_personas, UMBRAL_CATEGORIA_DOMINANTE)

# Imprimir resultados
cat("\nVariables categóricas con categorías dominantes (>", 
    UMBRAL_CATEGORIA_DOMINANTE * 100, "%):\n")
if(nrow(cat_dominantes_personas) > 0) {
  print(cat_dominantes_personas)
} else {
  cat("Ninguna variable categórica tiene una categoría dominante.\n")
}

# Guardar resultados
write.csv(cat_dominantes_personas, "views/tables/categorias_dominantes_personas.csv", row.names = FALSE)

# Crear versión para LaTeX
if(nrow(cat_dominantes_personas) > 0) {
  print(xtable(cat_dominantes_personas, 
               caption = "Variables Categóricas con Categorías Dominantes en Personas", 
               label = "tab:cat_dominantes_personas"),
        file = "views/tables/categorias_dominantes_personas.tex",
        include.rownames = FALSE,
        floating = TRUE,
        latex.environments = "center",
        booktabs = TRUE)
}

################################################################################
# SECCIÓN 10: ANÁLISIS DE VARIABLES NUMÉRICAS EN PERSONAS                      #
################################################################################

cat("\n\n========== ANÁLISIS DE VARIABLES NUMÉRICAS EN PERSONAS ==========\n\n")

# Analizar variabilidad de variables numéricas
var_numericas_personas <- analizar_variabilidad_numericas(train_personas, MIN_VARIABILIDAD)

# Identificar variables con baja variabilidad
vars_baja_var_personas <- var_numericas_personas %>%
  filter(baja_variabilidad == TRUE) %>%
  select(variable, media, desviacion_estandar, coef_variacion)

# Imprimir resultados
cat("\nVariables numéricas con baja variabilidad (CV <", MIN_VARIABILIDAD, "):\n")
if(nrow(vars_baja_var_personas) > 0) {
  print(vars_baja_var_personas)
} else {
  cat("Ninguna variable numérica tiene baja variabilidad.\n")
}

# Guardar resultados
write.csv(var_numericas_personas, "views/tables/variabilidad_numericas_personas.csv", row.names = FALSE)

# Crear versión para LaTeX
print(xtable(head(var_numericas_personas %>% select(-baja_variabilidad), 20), 
             caption = "Variabilidad de Variables Numéricas en Personas (Top 20)", 
             label = "tab:var_numericas_personas"),
      file = "views/tables/variabilidad_numericas_personas.tex",
      include.rownames = FALSE,
      floating = TRUE,
      latex.environments = "center",
      booktabs = TRUE)

################################################################################
# SECCIÓN 11: ANÁLISIS DE DISTRIBUCIÓN POR HOGAR                               #
################################################################################

cat("\n\n========== ANÁLISIS DE DISTRIBUCIÓN DE PERSONAS POR HOGAR ==========\n\n")

# Verificar que exista la variable id para identificar hogares
if("id" %in% names(train_personas)) {
  # Contar número de personas por hogar
  personas_por_hogar <- train_personas %>%
    count(id) %>%
    rename(hogar_id = id, n_personas = n)
  
  # Estadísticas descriptivas
  stats_personas_hogar <- summary(personas_por_hogar$n_personas)
  
  cat("\nEstadísticas del número de personas por hogar:\n")
  print(stats_personas_hogar)
  
  # Distribución de tamaños de hogares
  distribucion_tamanios <- personas_por_hogar %>%
    count(n_personas) %>%
    rename(tamano_hogar = n_personas, frecuencia = n) %>%
    mutate(porcentaje = frecuencia / sum(frecuencia) * 100)
  
  cat("\nDistribución de tamaños de hogares:\n")
  print(distribucion_tamanios)
  
  # Guardar resultados
  write.csv(distribucion_tamanios, "views/tables/distribucion_tamanos_hogares.csv", row.names = FALSE)
  
  # Crear versión para LaTeX
  print(xtable(distribucion_tamanios, 
               caption = "Distribución de Tamaños de Hogares", 
               label = "tab:distribucion_hogares"),
        file = "views/tables/distribucion_tamanos_hogares.tex",
        include.rownames = FALSE,
        floating = TRUE,
        latex.environments = "center",
        booktabs = TRUE)
}

################################################################################
# SECCIÓN 12: ANÁLISIS DE VARIABLES PARA AGREGACIÓN                            #
################################################################################

cat("\n\n========== ANÁLISIS DE VARIABLES PARA AGREGACIÓN A NIVEL HOGAR ==========\n\n")

# Identificar variables potencialmente útiles para agregar a nivel hogar
# Excluir variables de identificación
vars_potenciales <- setdiff(names(train_personas), c("id", "Orden"))

# Analizar métodos de agregación recomendados
agregacion_recomendada <- analizar_agregacion_persona_hogar(train_personas, "id", vars_potenciales)

# Mostrar primeras recomendaciones
cat("\nRecomendaciones de agregación para variables de personas:\n")
print(head(agregacion_recomendada, 20))

# Guardar todas las recomendaciones
write.csv(agregacion_recomendada, "views/tables/recomendaciones_agregacion.csv", row.names = FALSE)

# Crear versión para LaTeX
print(xtable(head(agregacion_recomendada, 20), 
             caption = "Recomendaciones de Agregación de Variables de Personas a Nivel Hogar", 
             label = "tab:recomendaciones_agregacion"),
      file = "views/tables/recomendaciones_agregacion.tex",
      include.rownames = FALSE,
      floating = TRUE,
      latex.environments = "center",
      booktabs = TRUE)

################################################################################
# SECCIÓN 13: ANÁLISIS DE VARIABLES CLAVE PARA PERSONAS                        #
################################################################################

cat("\n\n========== ANÁLISIS DE VARIABLES CLAVE EN PERSONAS ==========\n\n")

# Análisis básico de variables clave comunes como edad, género, etc.
if(all(c("P6020", "P6040") %in% names(train_personas))) {
  # Análisis de género (P6020: 1=Hombre, 2=Mujer)
  if("P6020" %in% names(train_personas)) {
    distribucion_genero <- train_personas %>%
      mutate(genero = case_when(
        P6020 == 1 ~ "Hombre",
        P6020 == 2 ~ "Mujer",
        TRUE ~ "Otro/NA"
      )) %>%
      count(genero) %>%
      mutate(porcentaje = n / sum(n) * 100)
    
    cat("\nDistribución por género:\n")
    print(distribucion_genero)
    
    # Guardar resultados
    write.csv(distribucion_genero, "views/tables/distribucion_genero.csv", row.names = FALSE)
  }
  
  # Análisis de edad (P6040)
  if("P6040" %in% names(train_personas)) {
    # Estadísticas básicas de edad
    stats_edad <- summary(train_personas$P6040)
    
    cat("\nEstadísticas de edad:\n")
    print(stats_edad)
    
    # Distribución por grupos de edad
    distribucion_edad <- train_personas %>%
      mutate(grupo_edad = case_when(
        P6040 < 18 ~ "Menor de 18",
        P6040 >= 18 & P6040 < 30 ~ "18-29",
        P6040 >= 30 & P6040 < 45 ~ "30-44",
        P6040 >= 45 & P6040 < 60 ~ "45-59",
        P6040 >= 60 ~ "60 o más",
        TRUE ~ "NA"
      )) %>%
      count(grupo_edad) %>%
      mutate(porcentaje = n / sum(n) * 100)
    
    cat("\nDistribución por grupos de edad:\n")
    print(distribucion_edad)
    
    # Guardar resultados
    write.csv(distribucion_edad, "views/tables/distribucion_edad.csv", row.names = FALSE)
    
    # Crear versión para LaTeX
    print(xtable(distribucion_edad, 
                 caption = "Distribución por Grupos de Edad", 
                 label = "tab:distribucion_edad"),
          file = "views/tables/distribucion_edad.tex",
          include.rownames = FALSE,
          floating = TRUE,
          latex.environments = "center",
          booktabs = TRUE)
  }
  
  # Análisis de parentesco (P6050)
  if("P6050" %in% names(train_personas)) {
    distribucion_parentesco <- train_personas %>%
      count(P6050) %>%
      mutate(porcentaje = n / sum(n) * 100)
    
    cat("\nDistribución por parentesco (P6050):\n")
    print(distribucion_parentesco)
    
    # Guardar resultados
    write.csv(distribucion_parentesco, "views/tables/distribucion_parentesco.csv", row.names = FALSE)
  }
}

################################################################################
# SECCIÓN 14: VARIABLES ECONÓMICAS EN PERSONAS                                 #
################################################################################

cat("\n\n========== ANÁLISIS DE VARIABLES ECONÓMICAS EN PERSONAS ==========\n\n")

# Identificar variables relacionadas con ingresos
vars_ingresos <- names(train_personas)[grepl("ing|salario|Ing|y_", names(train_personas))]

# Mostrar estadísticas básicas de variables de ingresos
if(length(vars_ingresos) > 0) {
  cat("\nVariables relacionadas con ingresos encontradas:", length(vars_ingresos), "\n")
  
  # Crear un dataframe con estadísticas para cada variable de ingreso
  stats_ingresos <- data.frame()
  
  for(var in vars_ingresos) {
    # Verificar que la variable existe y es numérica
    if(var %in% names(train_personas) && is.numeric(train_personas[[var]])) {
      # Calcular estadísticas
      stats <- summary(train_personas[[var]], na.rm = TRUE)
      
      # Convertir a dataframe
      stats_df <- data.frame(
        variable = var,
        n_no_na = sum(!is.na(train_personas[[var]])),
        pct_no_na = sum(!is.na(train_personas[[var]])) / nrow(train_personas) * 100,
        min = stats[1],
        q1 = stats[2],
        median = stats[3],
        mean = stats[4],
        q3 = stats[5],
        max = stats[6],
        stringsAsFactors = FALSE
      )
      
      stats_ingresos <- rbind(stats_ingresos, stats_df)
    }
  }
  
  # Ordenar por porcentaje de valores no NA
  stats_ingresos <- stats_ingresos %>% arrange(desc(pct_no_na))
  
  # Mostrar estadísticas
  cat("\nEstadísticas de variables de ingresos:\n")
  print(head(stats_ingresos, 10))
  
  # Guardar resultados
  write.csv(stats_ingresos, "views/tables/estadisticas_ingresos_personas.csv", row.names = FALSE)
  
  # Crear versión para LaTeX
  print(xtable(head(stats_ingresos, 10), 
               caption = "Estadísticas de Variables de Ingresos en Personas", 
               label = "tab:ingresos_personas"),
        file = "views/tables/estadisticas_ingresos_personas.tex",
        include.rownames = FALSE,
        floating = TRUE,
        latex.environments = "center",
        booktabs = TRUE)
}

# Análisis de ocupación (variables como Oc, Des, Ina)
vars_ocupacion <- c("Oc", "Des", "Ina")
for(var in vars_ocupacion) {
  if(var %in% names(train_personas) && sum(!is.na(train_personas[[var]])) > 0) {
    # Distribución
    dist_var <- train_personas %>%
      count(!!sym(var)) %>%
      mutate(porcentaje = n / sum(n) * 100)
    
    cat("\nDistribución de", var, ":\n")
    print(dist_var)
  }
}

################################################################################
# SECCIÓN 15: RESUMEN PERSONAS PARA TOMA DE DECISIONES                         #
################################################################################

cat("\n\n========== RESUMEN DE HALLAZGOS EN PERSONAS ==========\n\n")

# Crear lista de variables problemáticas
vars_problematicas_personas <- data.frame(
  variable = character(),
  criterio = character(),
  stringsAsFactors = FALSE
)

# Variables con muchos valores perdidos
if(nrow(vars_exceden_umbral_nas_personas) > 0) {
  temp <- data.frame(
    variable = vars_exceden_umbral_nas_personas$variable,
    criterio = paste("Valores perdidos:", round(vars_exceden_umbral_nas_personas$pct_missing * 100, 1), "%"),
    stringsAsFactors = FALSE
  )
  vars_problematicas_personas <- rbind(vars_problematicas_personas, temp)
}

# Variables categóricas con categorías dominantes
if(nrow(cat_dominantes_personas) > 0) {
  temp <- data.frame(
    variable = cat_dominantes_personas$variable,
    criterio = paste("Categoría dominante:", 
                     cat_dominantes_personas$categoria_dominante, "-", 
                     round(cat_dominantes_personas$porcentaje * 100, 1), "%"),
    stringsAsFactors = FALSE
  )
  vars_problematicas_personas <- rbind(vars_problematicas_personas, temp)
}

# Variables numéricas con baja variabilidad
if(nrow(vars_baja_var_personas) > 0) {
  temp <- data.frame(
    variable = vars_baja_var_personas$variable,
    criterio = paste("Baja variabilidad: CV =", round(vars_baja_var_personas$coef_variacion, 4)),
    stringsAsFactors = FALSE
  )
  vars_problematicas_personas <- rbind(vars_problematicas_personas, temp)
}

# Imprimir resumen
cat("\nVariables potencialmente problemáticas en personas:\n")
if(nrow(vars_problematicas_personas) > 0) {
  print(vars_problematicas_personas)
} else {
  cat("No se identificaron variables problemáticas según los criterios establecidos.\n")
}

# Guardar resumen
write.csv(vars_problematicas_personas, "views/tables/variables_problematicas_personas.csv", row.names = FALSE)

# Crear versión para LaTeX
if(nrow(vars_problematicas_personas) > 0) {
  print(xtable(vars_problematicas_personas, 
               caption = "Variables Potencialmente Problemáticas en Personas", 
               label = "tab:problematicas_personas"),
        file = "views/tables/variables_problematicas_personas.tex",
        include.rownames = FALSE,
        floating = TRUE,
        latex.environments = "center",
        booktabs = TRUE)
}

################################################################################
# SECCIÓN 16: ANÁLISIS DE VARIABLES CLAVE PARA AGREGACIÓN                      #
################################################################################

cat("\n\n========== VARIABLES CLAVE PARA AGREGACIÓN A NIVEL HOGAR ==========\n\n")

# Crear tabla resumen de variables recomendadas para agregar a nivel hogar
vars_recomendadas_agregacion <- data.frame(
  variable = character(),
  descripcion = character(),
  metodo_recomendado = character(),
  justificacion = character(),
  stringsAsFactors = FALSE
)

# Variables demográficas
if("P6040" %in% names(train_personas)) {
  vars_recomendadas_agregacion <- rbind(vars_recomendadas_agregacion, data.frame(
    variable = "P6040",
    descripcion = "Edad",
    metodo_recomendado = "Promedio, rango, desviación estándar",
    justificacion = "Captura la estructura etaria del hogar",
    stringsAsFactors = FALSE
  ))
}

if("P6020" %in% names(train_personas)) {
  vars_recomendadas_agregacion <- rbind(vars_recomendadas_agregacion, data.frame(
    variable = "P6020",
    descripcion = "Sexo (1=Hombre, 2=Mujer)",
    metodo_recomendado = "Contar número de mujeres o proporción",
    justificacion = "Captura la composición de género del hogar",
    stringsAsFactors = FALSE
  ))
}

# Variables educativas
if("P6210" %in% names(train_personas)) {
  vars_recomendadas_agregacion <- rbind(vars_recomendadas_agregacion, data.frame(
    variable = "P6210",
    descripcion = "Nivel educativo",
    metodo_recomendado = "Máximo, moda, promediar categorías",
    justificacion = "Captura el capital humano del hogar",
    stringsAsFactors = FALSE
  ))
}

# Variables laborales
if("Oc" %in% names(train_personas)) {
  vars_recomendadas_agregacion <- rbind(vars_recomendadas_agregacion, data.frame(
    variable = "Oc",
    descripcion = "Ocupado",
    metodo_recomendado = "Contar número de ocupados o proporción",
    justificacion = "Captura la participación laboral del hogar",
    stringsAsFactors = FALSE
  ))
}

if("Des" %in% names(train_personas)) {
  vars_recomendadas_agregacion <- rbind(vars_recomendadas_agregacion, data.frame(
    variable = "Des",
    descripcion = "Desempleado",
    metodo_recomendado = "Contar número de desempleados o proporción",
    justificacion = "Captura el desempleo en el hogar",
    stringsAsFactors = FALSE
  ))
}

if("Ina" %in% names(train_personas)) {
  vars_recomendadas_agregacion <- rbind(vars_recomendadas_agregacion, data.frame(
    variable = "Ina",
    descripcion = "Inactivo",
    metodo_recomendado = "Contar número de inactivos o proporción",
    justificacion = "Captura la inactividad laboral en el hogar",
    stringsAsFactors = FALSE
  ))
}

# Variables de ingresos
if(length(vars_ingresos) > 0) {
  # Identificar la variable de ingreso con menos valores perdidos
  mejor_var_ingreso <- stats_ingresos$variable[1]
  
  vars_recomendadas_agregacion <- rbind(vars_recomendadas_agregacion, data.frame(
    variable = mejor_var_ingreso,
    descripcion = "Ingreso",
    metodo_recomendado = "Suma, promedio, máximo",
    justificacion = "Captura el ingreso total o per cápita del hogar",
    stringsAsFactors = FALSE
  ))
}

# Mostrar variables recomendadas
cat("\nVariables clave recomendadas para agregar a nivel hogar:\n")
print(vars_recomendadas_agregacion)

# Guardar recomendaciones
write.csv(vars_recomendadas_agregacion, "views/tables/variables_recomendadas_agregacion.csv", row.names = FALSE)

# Crear versión para LaTeX
print(xtable(vars_recomendadas_agregacion, 
             caption = "Variables Clave Recomendadas para Agregación a Nivel Hogar", 
             label = "tab:recomendadas_agregacion"),
      file = "views/tables/variables_recomendadas_agregacion.tex",
      include.rownames = FALSE,
      floating = TRUE,
      latex.environments = "center",
      booktabs = TRUE)

################################################################################
# SECCIÓN 17: TABLA COMPARATIVA DE AMBAS BASES                                 #
################################################################################

cat("\n\n========== COMPARACIÓN DE BASES DE DATOS ==========\n\n")

# Crear tabla comparativa para toma de decisiones
comparacion_bases <- data.frame(
  criterio = c(
    "Número de observaciones",
    "Número de variables",
    "Variables numéricas",
    "Variables categóricas",
    "Variables con >50% valores perdidos",
    "Variables con categoría dominante",
    "Variables con baja variabilidad"
  ),
  hogares = c(
    nrow(train_hogares),
    ncol(train_hogares),
    sum(sapply(train_hogares, is.numeric)),
    sum(sapply(train_hogares, function(x) is.factor(x) || is.character(x))),
    sum(nas_hogares$pct_missing > 0.5),
    nrow(cat_dominantes_hogares),
    nrow(vars_baja_var_hogares)
  ),
  personas = c(
    nrow(train_personas),
    ncol(train_personas),
    sum(sapply(train_personas, is.numeric)),
    sum(sapply(train_personas, function(x) is.factor(x) || is.character(x))),
    sum(nas_personas$pct_missing > 0.5),
    nrow(cat_dominantes_personas),
    nrow(vars_baja_var_personas)
  ),
  stringsAsFactors = FALSE
)

# Mostrar tabla comparativa
cat("\nTabla comparativa de bases de datos:\n")
print(comparacion_bases)

# Guardar comparación
write.csv(comparacion_bases, "views/tables/comparacion_bases.csv", row.names = FALSE)

# Crear versión para LaTeX
print(xtable(comparacion_bases, 
             caption = "Comparación entre Bases de Datos de Hogares y Personas", 
             label = "tab:comparacion_bases"),
      file = "views/tables/comparacion_bases.tex",
      include.rownames = FALSE,
      floating = TRUE,
      latex.environments = "center",
      booktabs = TRUE)

################################################################################
# SECCIÓN 18: RESUMEN FINAL Y RECOMENDACIONES                                  #
################################################################################

cat("\n\n========== RESUMEN FINAL Y RECOMENDACIONES ==========\n\n")

cat("\nRecomendaciones para la unión de bases de datos:\n")

cat("\n1. Variables de hogares para mantener:\n")
cat("   - Variable objetivo (Pobre)\n")
cat("   - Variables con alta correlación con pobreza\n")
cat("   - Variables con baja proporción de valores perdidos\n")
cat("   - Variables con suficiente variabilidad\n")

cat("\n2. Variables de personas para agregar a nivel hogar:\n")
cat("   - Variables demográficas: edad, sexo, nivel educativo\n")
cat("   - Variables laborales: ocupación, desempleo, inactividad\n")
cat("   - Variables de ingresos: ingreso laboral, ingreso total\n")

cat("\n3. Métodos de agregación recomendados:\n")
cat("   - Para variables numéricas: promedio, suma, máximo, mínimo, desviación estándar\n")
cat("   - Para variables categóricas: conteo, proporción, moda\n")

cat("\n4. Consideraciones para el preprocesamiento:\n")
cat("   - Tratar valores perdidos después de la agregación\n")
cat("   - Evaluar transformaciones de variables con distribuciones sesgadas\n")
cat("   - Considerar estrategias para manejar el desbalance de clases en la modelación\n")

# Crear archivo de recomendaciones en formato tex
fileConn <- file("views/tables/recomendaciones_finales.tex")
writeLines(c(
  "\\begin{table}[h!]",
  "\\centering",
  "\\caption{Recomendaciones para la Unión de Bases de Datos}",
  "\\label{tab:recomendaciones_finales}",
  "\\begin{tabular}{p{3cm}p{12cm}}",
  "\\toprule",
  "\\textbf{Categoría} & \\textbf{Recomendación} \\\\",
  "\\midrule",
  "Variables de hogares & Mantener variables objetivo (Pobre), variables con alta correlación con pobreza, variables con baja proporción de valores perdidos, y variables con suficiente variabilidad. \\\\",
  "Variables de personas & Agregar a nivel hogar variables demográficas (edad, sexo, nivel educativo), variables laborales (ocupación, desempleo, inactividad), y variables de ingresos (ingreso laboral, ingreso total). \\\\",
  "Métodos de agregación & Para variables numéricas: promedio, suma, máximo, mínimo, desviación estándar. Para variables categóricas: conteo, proporción, moda. \\\\",
  "Preprocesamiento & Tratar valores perdidos después de la agregación, evaluar transformaciones de variables con distribuciones sesgadas, y considerar estrategias para manejar el desbalance de clases en la modelación. \\\\",
  "\\bottomrule",
  "\\end{tabular}",
  "\\end{table}"
), fileConn)
close(fileConn)

# Crear una versión de recomendaciones específicas de variables
recomendaciones_especificas <- data.frame(
  categoria = character(),
  variable = character(),
  accion = character(),
  razon = character(),
  stringsAsFactors = FALSE
)

# Añadir recomendaciones específicas para hogares
vars_hogares_mantener <- c("Pobre", "Lp", "Ingpcug", "Ingtotugarr", "Npersug", "Dominio")
for(var in vars_hogares_mantener) {
  if(var %in% names(train_hogares)) {
    recomendaciones_especificas <- rbind(recomendaciones_especificas, data.frame(
      categoria = "Hogares",
      variable = var,
      accion = "Mantener",
      razon = "Variable clave para análisis de pobreza",
      stringsAsFactors = FALSE
    ))
  }
}

# Añadir recomendaciones específicas para personas
vars_personas_agregar <- c("P6040", "P6020", "P6210", "Oc", "Des", "Ina")
for(var in vars_personas_agregar) {
  if(var %in% names(train_personas)) {
    recomendaciones_especificas <- rbind(recomendaciones_especificas, data.frame(
      categoria = "Personas",
      variable = var,
      accion = "Agregar a nivel hogar",
      razon = "Variable demográfica o laboral clave",
      stringsAsFactors = FALSE
    ))
  }
}

# Guardar recomendaciones específicas
write.csv(recomendaciones_especificas, "views/tables/recomendaciones_especificas.csv", row.names = FALSE)

# Crear versión para LaTeX
print(xtable(recomendaciones_especificas, 
             caption = "Recomendaciones Específicas para Variables", 
             label = "tab:recomendaciones_especificas"),
      file = "views/tables/recomendaciones_especificas.tex",
      include.rownames = FALSE,
      floating = TRUE,
      latex.environments = "center",
      booktabs = TRUE)

cat("\n¡Análisis completado con éxito! Resultados guardados en la carpeta views/tables/\n")

################################################################################
#                            FINALIZACIÓN DEL SCRIPT                           #
################################################################################