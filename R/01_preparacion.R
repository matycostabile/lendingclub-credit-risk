# =============================================================================
# 01_preparacion.R — Carga, validación y partición temporal de los datos
# -----------------------------------------------------------------------------
# PRE : main.R ya cargó las librerías y utils.R; la carpeta de trabajo es la
#       raíz del proyecto.
# POST: existen en memoria:
#         datos  -> data.frame completo con variable objetivo `y` (1 = default)
#         train  -> préstamos emitidos 2012-01 a 2013-12 (entrenamiento)
#         test   -> préstamos emitidos 2014-01 a 2014-06 (prueba fuera de tiempo)
# INV : ningún préstamo está a la vez en train y en test; train es
#       cronológicamente anterior a test.
# =============================================================================

cat("\n[1/5] Preparación de datos...\n")

# ---- Lectura -----------------------------------------------------------------
datos <- read.csv("data/lending_club_36m_2012_2014.csv",
                  stringsAsFactors = FALSE, na.strings = c("", "NA"))

# ---- Validación de esquema (precondiciones sobre el archivo) -----------------
columnas_esperadas <- c(
  "issue_d", "loan_amnt", "int_rate", "installment", "grade", "sub_grade",
  "emp_length", "home_ownership", "annual_inc", "verification_status",
  "purpose", "addr_state", "dti", "delinq_2yrs", "earliest_cr_line",
  "inq_last_6mths", "mths_since_last_delinq", "mths_since_last_record",
  "open_acc", "pub_rec", "revol_bal", "revol_util", "total_acc",
  "initial_list_status", "pub_rec_bankruptcies", "mort_acc", "tot_cur_bal",
  "total_rev_hi_lim", "acc_now_delinq", "collections_12_mths_ex_med",
  "loan_status", "funded_amnt", "total_pymnt", "total_rec_prncp",
  "recoveries", "last_pymnt_amnt")
faltan <- setdiff(columnas_esperadas, names(datos))
if (length(faltan) > 0) stop("Faltan columnas en el CSV: ",
                             paste(faltan, collapse = ", "))
stopifnot(nrow(datos) > 100000,
          all(datos$loan_status %in% c("Fully Paid", "Charged Off")),
          all(datos$loan_amnt > 0), all(datos$annual_inc > 0))

# ---- Variable objetivo --------------------------------------------------------
# y = 1 si el préstamo terminó en "Charged Off" (incumplimiento / default)
# y = 0 si fue "Fully Paid" (pagado en su totalidad)
datos$y <- as.integer(datos$loan_status == "Charged Off")

# ---- Clasificación de columnas según CUÁNDO se conocen -----------------------
# Esta separación es la decisión metodológica más importante del trabajo:
# un modelo de otorgamiento sólo puede usar información disponible el día en
# que se pide el préstamo. Las variables "posteriores" describen cómo se pagó
# el préstamo y por lo tanto contienen la respuesta (fuga de información).
VARS_POSTERIORES <- c("total_pymnt", "total_rec_prncp", "recoveries",
                      "last_pymnt_amnt", "funded_amnt", "loan_status")

# ---- Partición temporal (out-of-time) -----------------------------------------
# Se entrena con el pasado y se evalúa con el "futuro", igual que en un banco.
# Una partición aleatoria sería optimista porque mezcla períodos económicos.
corte <- "2014-01"
train <- datos[datos$issue_d <  corte, ]
test  <- datos[datos$issue_d >= corte, ]

# Invariantes de la partición
stopifnot(nrow(train) + nrow(test) == nrow(datos),
          max(train$issue_d) < min(test$issue_d))

cat(sprintf("  Préstamos totales: %d | train: %d (%s a %s) | test: %d (%s a %s)\n",
            nrow(datos), nrow(train), min(train$issue_d), max(train$issue_d),
            nrow(test), min(test$issue_d), max(test$issue_d)))
cat(sprintf("  Tasa de default -> total: %.2f%% | train: %.2f%% | test: %.2f%%\n",
            100 * mean(datos$y), 100 * mean(train$y), 100 * mean(test$y)))

# ---- Cifras para el informe ---------------------------------------------------
escribir_macro(ARCHIVO_MACROS, "nTotal", fmt_miles(nrow(datos)))
escribir_macro(ARCHIVO_MACROS, "nTrain", fmt_miles(nrow(train)))
escribir_macro(ARCHIVO_MACROS, "nTest",  fmt_miles(nrow(test)))
escribir_macro(ARCHIVO_MACROS, "nDefault", fmt_miles(sum(datos$y)))
escribir_macro(ARCHIVO_MACROS, "tasaTotal", fmt_pct(mean(datos$y)))
escribir_macro(ARCHIVO_MACROS, "tasaTrain", fmt_pct(mean(train$y)))
escribir_macro(ARCHIVO_MACROS, "tasaTest",  fmt_pct(mean(test$y)))
escribir_macro(ARCHIVO_MACROS, "montoTotal",
               paste0(fmt(sum(datos$funded_amnt) / 1e9, 2), "~mil millones"))
