# =============================================================================
# 00_construir_dataset.R
# -----------------------------------------------------------------------------
# Script de REFERENCIA (no hace falta correrlo): documenta cómo se generó
# data/lending_club_36m_2012_2014.csv a partir de los datos públicos de
# LendingClub empaquetados en https://github.com/kuhnrl30/LendingClubData
# (archivos .rda por año de emisión, foto de la cartera a fines de 2017).
#
# Precondición : existe la carpeta `ruta_rda` con LendingClubData_2012.rda,
#                LendingClubData_2013.rda y LendingClubData_2014.rda.
# Postcondición: se escribe un CSV con préstamos a 36 meses emitidos entre
#                2012-01 y 2014-06, con estado final "Fully Paid" o
#                "Charged Off" y sólo las columnas listadas abajo.
# =============================================================================

ruta_rda <- "LendingClubData/data"   # carpeta del repo clonado

# Variables conocidas AL MOMENTO DE OTORGAR el préstamo (candidatas a features)
vars_originacion <- c(
  "issue_d", "loan_amnt", "term", "int_rate", "installment", "grade",
  "sub_grade", "emp_length", "home_ownership", "annual_inc",
  "verification_status", "purpose", "addr_state", "dti", "delinq_2yrs",
  "earliest_cr_line", "inq_last_6mths", "mths_since_last_delinq",
  "mths_since_last_record", "open_acc", "pub_rec", "revol_bal", "revol_util",
  "total_acc", "initial_list_status", "pub_rec_bankruptcies", "mort_acc",
  "tot_cur_bal", "total_rev_hi_lim", "acc_now_delinq",
  "collections_12_mths_ex_med"
)
# Variable objetivo
var_objetivo <- "loan_status"
# Variables POSTERIORES al otorgamiento: NUNCA se usan como features.
# Se conservan sólo para (a) medir el resultado económico real de cada
# préstamo y (b) el experimento didáctico de fuga de información (leakage).
vars_posteriores <- c("funded_amnt", "total_pymnt", "total_rec_prncp",
                      "recoveries", "last_pymnt_amnt")

lista <- list()
for (anio in 2012:2014) {
  e <- new.env()
  load(file.path(ruta_rda, sprintf("LendingClubData_%d.rda", anio)), envir = e)
  d <- get(ls(e)[1], envir = e)
  d <- d[d$term == 36, c(vars_originacion, var_objetivo, vars_posteriores)]
  lista[[as.character(anio)]] <- d
}
datos <- do.call(rbind, lista)

# Ventana: emitidos hasta junio 2014 => todos vencidos antes de la foto (2017)
datos <- datos[as.Date(datos$issue_d) <= as.Date("2014-06-30"), ]
cat("Estados antes de filtrar:\n"); print(table(datos$loan_status))
datos <- datos[datos$loan_status %in% c("Fully Paid", "Charged Off"), ]

# Invariante: sólo dos estados finales y todos los préstamos a 36 meses
stopifnot(all(datos$loan_status %in% c("Fully Paid", "Charged Off")),
          all(datos$term == 36))

datos$term <- NULL
num_post <- c("total_pymnt", "total_rec_prncp", "recoveries", "last_pymnt_amnt")
datos[num_post] <- lapply(datos[num_post], round, 2)
datos$issue_d <- format(as.Date(datos$issue_d), "%Y-%m")
datos$earliest_cr_line <- format(as.Date(datos$earliest_cr_line), "%Y-%m")
write.csv(datos, "data/lending_club_36m_2012_2014.csv", row.names = FALSE)
cat("Filas escritas:", nrow(datos), "\n")
