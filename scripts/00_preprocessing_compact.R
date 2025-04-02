################################################################################
# TÍTULO: 00_preprocessing.R                                   #
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
UMBRAL_DOM <- 0.80     # % máximo para categoría dominante

###########################################
# 1. ANÁLISIS BASE DE HOGARES            #
###########################################

# Cargar datos
train_hogares <- read.csv("stores/raw/train_hogares.csv")

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

# Análisis de categorías dominantes
cat_dominantes_hogares <- data.frame()
for(var in names(train_hogares)) {
  # Solo si es categórica o character
  if(is.factor(train_hogares[[var]]) || is.character(train_hogares[[var]])) {
    tab <- prop.table(table(train_hogares[[var]], useNA = "always"))
    # Si hay una categoría dominante
    if(max(tab, na.rm = TRUE) > UMBRAL_DOM) {
      cat_dominantes_hogares <- rbind(cat_dominantes_hogares, data.frame(
        variable = var,
        categoria = names(which.max(tab)),
        proporcion = max(tab, na.rm = TRUE)
      ))
    }
  }
}

# Guardar resultados
write.csv(cat_dominantes_hogares, "views/tables/cat_dom_hogares.csv", row.names = FALSE)

# Correlación con pobreza (si existe)
if("Pobre" %in% names(train_hogares)) {
  cors_pobreza <- data.frame()
  for(var in names(train_hogares)) {
    if(is.numeric(train_hogares[[var]]) && var != "Pobre") {
      cor_val <- cor(train_hogares[[var]], train_hogares$Pobre, 
                     use = "pairwise.complete.obs")
      if(!is.na(cor_val)) {
        cors_pobreza <- rbind(cors_pobreza, data.frame(
          variable = var,
          correlacion = cor_val
        ))
      }
    }
  }
  cors_pobreza <- cors_pobreza %>% arrange(desc(abs(correlacion)))
  write.csv(cors_pobreza, "views/tables/corr_pobreza_hogares.csv", row.names = FALSE)
}

###########################################
# 2. ANÁLISIS BASE DE PERSONAS           #
###########################################

# Cargar datos
train_personas <- read.csv("stores/raw/train_personas.csv")

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

# Análisis de categorías dominantes
cat_dominantes_personas <- data.frame()
for(var in names(train_personas)) {
  # Solo si es categórica o character
  if(is.factor(train_personas[[var]]) || is.character(train_personas[[var]])) {
    tab <- prop.table(table(train_personas[[var]], useNA = "always"))
    # Si hay una categoría dominante
    if(max(tab, na.rm = TRUE) > UMBRAL_DOM) {
      cat_dominantes_personas <- rbind(cat_dominantes_personas, data.frame(
        variable = var,
        categoria = names(which.max(tab)),
        proporcion = max(tab, na.rm = TRUE)
      ))
    }
  }
}

# Guardar resultados
write.csv(cat_dominantes_personas, "views/tables/cat_dom_personas.csv", row.names = FALSE)

###########################################
# 3. VARIABLES PARA AGREGACIÓN           #
###########################################

# Identificar variables demográficas clave
vars_demograficas <- c("P6020", "P6040", "P6210")
agregacion_demograficas <- data.frame(
  variable = vars_demograficas,
  descripcion = c("Sexo", "Edad", "Nivel educativo"),
  metodo_agregacion = c("Contar mujeres", "Promedio, máximo", "Máximo nivel")
)

# Identificar variables laborales
vars_laborales <- c("Oc", "Des", "Ina")
agregacion_laborales <- data.frame(
  variable = vars_laborales,
  descripcion = c("Ocupado", "Desempleado", "Inactivo"),
  metodo_agregacion = c("Contar ocupados", "Contar desempleados", "Contar inactivos")
)

# Juntar recomendaciones
recomendaciones_agregacion <- rbind(agregacion_demograficas, agregacion_laborales)
write.csv(recomendaciones_agregacion, "views/tables/recomendaciones_agregacion.csv", 
          row.names = FALSE)

###########################################
# 4. VARIABLES MÁS IMPORTANTES           #
###########################################

# Identificar variables de hogares importantes
vars_importantes_hogares <- c("Pobre", "Lp", "Ingpcug", "Npersug", "Dominio")

# Identificar variables de personas importantes
vars_importantes_personas <- c("P6040", "P6020", "P6210", "Oc", "Des", "Ina")

# Crear tabla de recomendaciones final
recomendaciones_finales <- data.frame(
  base = c(rep("Hogares", length(vars_importantes_hogares)), 
           rep("Personas", length(vars_importantes_personas))),
  variable = c(vars_importantes_hogares, vars_importantes_personas),
  accion = c(rep("Mantener", length(vars_importantes_hogares)),
             rep("Agregar", length(vars_importantes_personas)))
)

write.csv(recomendaciones_finales, "views/tables/recomendaciones_finales.csv",
          row.names = FALSE)

cat("¡Análisis completado! Resultados guardados en views/tables/\n")

################################################################################
#                            FINALIZACIÓN DEL SCRIPT                           #
################################################################################