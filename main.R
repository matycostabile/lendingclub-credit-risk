# =============================================================================
#  ¿Quién no va a devolver el préstamo?
#  Modelado de riesgo de crédito con datos reales de LendingClub (2012-2014)
# -----------------------------------------------------------------------------
#  Autor   : Matías Costabile — Licenciatura en Ciencia de Datos, UCA
#  Archivo : main.R  (PUNTO DE ENTRADA: correr este archivo)
#
#  Cómo correrlo:
#    1. Abrir LendingClub_RiesgoCredito.Rproj en RStudio (esto fija la carpeta
#       de trabajo en la raíz del proyecto).
#    2. Abrir main.R y presionar "Source" (Ctrl+Shift+S).
#    Tiempo aproximado: 5 a 15 minutos según la PC (Random Forest y Gradient
#    Boosting son los pasos más lentos).
#
#  Qué hace, en orden:
#    R/01_preparacion.R  -> lee el CSV, valida, define variable objetivo y
#                           partición temporal (train 2012-2013 / test 2014)
#    R/02_exploracion.R  -> análisis exploratorio y gráficos
#    R/03_features.R     -> extracción, transformación, imputación,
#                           ingeniería y selección de características
#    R/04_modelos.R      -> entrena 5 modelos + experimento de fuga de datos
#    R/05_evaluacion.R   -> métricas, ROC, matrices de confusión, calibración,
#                           importancia de variables y análisis económico
#
#  Salidas: figures/ (gráficos PDF y PNG), results/ (tablas CSV y LaTeX).
# =============================================================================

# ---- Librerías --------------------------------------------------------------
# Si alguna librería no está instalada, se instala automáticamente.
if (!require("ggplot2")) {      # gráficos
  install.packages("ggplot2")
  library("ggplot2")
}
if (!require("pROC")) {         # curvas ROC, AUC y test de DeLong
  install.packages("pROC")
  library("pROC")
}
if (!require("glmnet")) {       # regresión logística con LASSO
  install.packages("glmnet")
  library("glmnet")
}
if (!require("ranger")) {       # Random Forest rápido (multinúcleo)
  install.packages("ranger")
  library("ranger")
}
if (!require("gbm")) {          # Gradient Boosting Machine
  install.packages("gbm")
  library("gbm")
}

# ---- Configuración general ---------------------------------------------------
# Precondición: la carpeta de trabajo es la raíz del proyecto.
if (!file.exists("data/lending_club_36m_2012_2014.csv")) {
  stop("No se encuentra data/lending_club_36m_2012_2014.csv.\n",
       "Abrí el proyecto con LendingClub_RiesgoCredito.Rproj o usá setwd() ",
       "para posicionarte en la carpeta raíz del proyecto.")
}
SEMILLA <- 2024          # semilla fija => resultados reproducibles
set.seed(SEMILLA)
options(stringsAsFactors = FALSE, scipen = 999)
dir.create("figures", showWarnings = FALSE)
dir.create("results", showWarnings = FALSE)
ARCHIVO_MACROS <- "results/numeros.tex"   # cifras que usa el informe LaTeX
if (file.exists(ARCHIVO_MACROS)) invisible(file.remove(ARCHIVO_MACROS))

t_inicio <- Sys.time()
source("R/utils.R",          encoding = "UTF-8")
source("R/01_preparacion.R", encoding = "UTF-8")
source("R/02_exploracion.R", encoding = "UTF-8")
source("R/03_features.R",    encoding = "UTF-8")
source("R/04_modelos.R",     encoding = "UTF-8")
source("R/05_evaluacion.R",  encoding = "UTF-8")

cat("\n=============================================================\n")
cat(" Listo. Tiempo total:",
    round(as.numeric(difftime(Sys.time(), t_inicio, units = "mins")), 1),
    "minutos\n Gráficos en figures/  |  Tablas en results/\n")
cat("=============================================================\n")
