################################################################################
# SCRIPT: 03_preprocess_personas.R                                            #
# PROYECTO: Predicción de Pobreza en Colombia                                 #
# DESCRIPCIÓN: Limpieza y transformación de la base de personas,              #
#              incluyendo creación de variables binarias y agregadas.         #
# FECHA: 13 de abril de 2025                                                  #
################################################################################

# Configurar directorio de trabajo automáticamente
if (!require(rstudioapi)) install.packages("rstudioapi")
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../")

# Cargar librerías necesarias
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse)

# Semilla
set.seed(1051)

################################################################################
# 1. FUNCIÓN DE PREPROCESAMIENTO DE PERSONAS                                  #
################################################################################

pre_process_personas <- function(data) {
  data <- data %>% 
    mutate(
      # --- Características sociodemográficas ---
      mujer = ifelse(P6020 == 2, 1, 0),
      H_Head = ifelse(P6050 == 1, 1, 0),
      menor = ifelse(P6040 <= 6, 1, 0),
      EducLevel = ifelse(P6210 == 9, 0, P6210),
      edad = P6040,
      experiencia = P6426,

      # --- Condición laboral y ocupacional ---
      ocupado = ifelse(is.na(Oc), 0, 1),
      trabaja = ifelse(P6240 == 1, 1, 0),
      cotiza = ifelse(P6920 == 1, 1, 0),
      otro_trab = ifelse(P7040 == 1, 1, 0),
      trabajar_mas = ifelse(P7090 == 1, 1, 0),
      dil_trab_mas = ifelse(P7110 == 1, 1, 0),
      camb_trabajo = ifelse(P7150 == 1, 1, 0),
      busq_trab = ifelse(P7310 == 1, 1, 0),
      horas_trab = P6800,

      # --- Seguridad social y salud ---
      afliado_salud = ifelse(P6090 == 1, 2, 0),

      # --- Ingresos y beneficios ---
      primas = ifelse(P6545 == 1, 1, 0),
      prima_serv = ifelse(P6630s1 == 1, 1, 0),
      pimra_nav = ifelse(P6630s2 == 1, 1, 0),
      pimra_vac = ifelse(P6630s3 == 1, 1, 0),
      bono_anual = ifelse(P6630s6 == 1, 1, 0),
      bonos = ifelse(P6580 == 1, 1, 0),
      aux_alim = ifelse(P6585s1 == 1, 1, 0),
      aux_trans = ifelse(P6585s2 == 1, 1, 0),
      aux_fam = ifelse(P6585s3 == 1, 1, 0),
      aux_educ = ifelse(P6585s4 == 1, 1, 0),
      pag_alim = ifelse(P6590 == 1, 1, 0),
      pag_vda = ifelse(P6600 == 1, 1, 0),
      pag_esp = ifelse(P6620 == 1, 1, 0),
      viaticos = ifelse(P6630s4 == 1, 1, 0),
      pag_arrdo = ifelse(P7495 == 1, 1, 0),
      pag_pension = ifelse(P7500s2 == 1, 1, 0),
      pag_pen_alim = ifelse(P7500s3 == 1, 1, 0),
      pag_publ = ifelse(P7505 == 1, 1, 0),
      pag_publ_otroP = ifelse(P7505 == 1, 1, 0),
      pag_otraspers = ifelse(P7510s1 == 1, 1, 0),
      pag_otraspers_otroP = ifelse(P7510s3 == 1, 1, 0),
      pag_CDT = ifelse(P7510s5 == 1, 1, 0),
      pag_cesantias = ifelse(P7510s6 == 1, 1, 0),
      pag_otro = ifelse(P7510s7 == 1, 1, 0),

      # --- Indicadores adicionales ---
      per_ingersos = ifelse(P7422 == 1, 1, 0)
    ) %>%
    select(
      id, Orden, mujer, H_Head, menor, EducLevel, edad, experiencia, ocupado, afliado_salud,
      trabaja, primas, prima_serv, pimra_nav, pimra_vac, bono_anual, bonos,
      aux_alim, aux_trans, aux_fam, aux_educ, pag_alim, pag_vda, pag_esp,
      horas_trab, viaticos, cotiza, otro_trab, trabajar_mas, dil_trab_mas,
      camb_trabajo, busq_trab, pag_arrdo, pag_pension, pag_pen_alim, pag_publ,
      pag_otraspers, pag_otraspers_otroP, pag_publ_otroP, pag_CDT, pag_cesantias,
      pag_otro, per_ingersos, Pet
    )
}

################################################################################
# 2. APLICAR PREPROCESAMIENTO                                                  #
################################################################################

cat("Aplicando preprocesamiento a las bases de personas...\n")

train_personas <- pre_process_personas(train_personas)
test_personas  <- pre_process_personas(test_personas)

################################################################################
# 3. IMPUTACIÓN DE VALORES FALTANTES                                          #
################################################################################

cat("Imputando valores faltantes...\n")

imputar_NA <- function(df) {
  for (col in names(df)) {
    if (any(is.na(df[[col]]))) {
      if (is.numeric(df[[col]])) {
        df[[col]][is.na(df[[col]])] <- median(df[[col]], na.rm = TRUE)
      } else if (is.factor(df[[col]]) || is.character(df[[col]])) {
        moda <- names(sort(table(df[[col]]), decreasing = TRUE))[1]
        df[[col]][is.na(df[[col]])] <- moda
      }
    }
  }
  return(df)
}

train_personas <- imputar_NA(train_personas)
test_personas  <- imputar_NA(test_personas)

cat("Preprocesamiento e imputación completados.\n")

################################################################################
#                              FIN DEL SCRIPT                                  #
################################################################################
