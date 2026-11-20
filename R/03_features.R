# =============================================================================
# 03_features.R — Ingeniería y selección de características
# -----------------------------------------------------------------------------
# Las etapas pedidas por la consigna aparecen en este orden:
#   (A) EXTRACCIÓN     : nuevas variables derivadas de las originales
#   (B) TRANSFORMACIÓN : logaritmos, recorte de extremos, agrupación de
#                        categorías raras, codificación WoE, estandarización
#   (C) IMPUTACIÓN     : estructural (el faltante tiene significado) y
#                        estadística (mediana) + indicadores de faltante
#   (D) SELECCIÓN      : Information Value (IV) + filtro de correlación
#                        (+ LASSO en 04_modelos.R como selección embebida)
#
# REGLA DE ORO: todo parámetro que "aprende" de los datos (percentiles,
# medianas, medias, desvíos, WoE, IV) se calcula SÓLO con entrenamiento y
# después se APLICA, sin recalcular, a validación y prueba.
#
# PRE : existen `train` y `test` (01_preparacion.R).
# POST: existen
#         receta        -> lista con todos los parámetros aprendidos de train
#         tabla_iv      -> IV de cada característica candidata
#         vars_finales  -> características seleccionadas
#         df_ajuste, df_valid, df_train, df_test -> data.frames listos
#         X_*           -> matrices de diseño estandarizadas (para glmnet)
# INV : el número de filas de cada conjunto no cambia en todo el script.
# =============================================================================

cat("\n[3/5] Ingeniería y selección de características...\n")

# -----------------------------------------------------------------------------
# (A) EXTRACCIÓN — extraer_variables(df)
#   Crea variables nuevas fila por fila (no aprende nada de los datos, por eso
#   puede aplicarse igual a cualquier conjunto).
# PRE : df tiene las columnas originales del CSV.
# POST: mismo número de filas; columnas nuevas agregadas.
# -----------------------------------------------------------------------------
extraer_variables <- function(df) {
  n0 <- nrow(df)
  # Antigüedad del historial crediticio en meses al momento de pedir el préstamo
  df$antiguedad_credito <- meses_entre(df$earliest_cr_line, df$issue_d)
  # Antigüedad laboral: texto -> número de años ("n/a" queda como NA)
  emp <- gsub("[^0-9<]", "", df$emp_length)       # "10+ years" -> "10"
  df$emp_anios <- ifelse(emp == "<1", 0, suppressWarnings(as.numeric(emp)))
  # Sub-grado A1..G5 -> 1..35 (orden de riesgo asignado por LendingClub)
  df$subgrado_num <- (match(substr(df$sub_grade, 1, 1), LETTERS[1:7]) - 1) * 5 +
                     as.numeric(substr(df$sub_grade, 2, 2))
  # Ratios de capacidad de pago (la variable más relevante en riesgo de crédito)
  df$prestamo_ingreso <- df$loan_amnt / df$annual_inc
  df$cuota_ingreso    <- 12 * df$installment / df$annual_inc
  # "DTI nuevo" (Namvar et al., 2018): endeudamiento INCLUYENDO la cuota del
  # préstamo pedido. El DTI original de LendingClub la excluye.
  df$dti_con_prestamo <- df$dti + 100 * df$cuota_ingreso
  df$revol_ingreso    <- df$revol_bal / df$annual_inc
  # (total_acc = 0 => sin cuentas => ratio 0; evita dividir por cero)
  df$ratio_cuentas_abiertas <- ifelse(df$total_acc > 0,
                                      df$open_acc / df$total_acc, 0)
  stopifnot(nrow(df) == n0, all(df$subgrado_num %in% 1:35))
  df
}

train <- extraer_variables(train)
test  <- extraer_variables(test)

# -----------------------------------------------------------------------------
# Partición interna de entrenamiento para ajustar hiperparámetros y el umbral:
#   ajuste     : 2012-01 a 2013-06
#   validación : 2013-07 a 2013-12  (también fuera de tiempo)
# -----------------------------------------------------------------------------
es_valid  <- train$issue_d >= "2013-07"
ajuste    <- train[!es_valid, ]
valid     <- train[es_valid, ]
stopifnot(nrow(ajuste) + nrow(valid) == nrow(train))

# Variables candidatas (sólo información disponible al originar el préstamo)
NUMERICAS <- c("loan_amnt", "int_rate", "installment", "subgrado_num",
               "annual_inc", "dti", "dti_con_prestamo", "delinq_2yrs",
               "inq_last_6mths", "mths_since_last_delinq",
               "mths_since_last_record", "open_acc", "pub_rec", "revol_bal",
               "revol_util", "total_acc", "pub_rec_bankruptcies", "mort_acc",
               "tot_cur_bal", "total_rev_hi_lim", "acc_now_delinq",
               "collections_12_mths_ex_med", "antiguedad_credito", "emp_anios",
               "prestamo_ingreso", "cuota_ingreso", "revol_ingreso",
               "ratio_cuentas_abiertas")
CATEGORICAS <- c("home_ownership", "verification_status", "purpose",
                 "addr_state", "initial_list_status")

# Imputación ESTRUCTURAL: el faltante significa "nunca ocurrió".
# Ej.: mths_since_last_delinq = NA => el cliente nunca tuvo un atraso.
# Se reemplaza por un valor "muy lejano" (el máximo observado en train) y se
# agrega un indicador binario.
IMPUT_ESTRUCTURAL <- c("mths_since_last_delinq", "mths_since_last_record")
# Imputación ESTADÍSTICA: el faltante se debe a que el dato no se registró.
# Se reemplaza por la mediana de train y se agrega un indicador binario.
IMPUT_MEDIANA <- c("revol_util", "mort_acc", "tot_cur_bal", "total_rev_hi_lim",
                   "emp_anios")
# Variables muy asimétricas: se les aplica log(1 + x)
LOG1P <- c("annual_inc", "revol_bal", "tot_cur_bal", "total_rev_hi_lim",
           "revol_ingreso")

# -----------------------------------------------------------------------------
# (B)+(C) ajustar_receta(df)
#   Aprende de `df` (= entrenamiento) todos los parámetros de transformación.
# PRE : df tiene NUMERICAS, CATEGORICAS y la variable `y`.
# POST: lista con percentiles, medianas, máximos, niveles de factores,
#       tablas WoE, medias y desvíos. No modifica df.
# -----------------------------------------------------------------------------
ajustar_receta <- function(df) {
  stopifnot(all(c(NUMERICAS, CATEGORICAS, "y") %in% names(df)))
  r <- list()
  # Límites de winsorización (percentiles 1% y 99%)
  r$limites <- lapply(df[NUMERICAS], quantile, probs = c(0.01, 0.99), na.rm = TRUE)
  r$max_estructural <- sapply(df[IMPUT_ESTRUCTURAL], max, na.rm = TRUE)
  r$medianas <- sapply(df[IMPUT_MEDIANA], median, na.rm = TRUE)
  # Categorías con < 1% de frecuencia se agrupan en "OTRA"
  r$niveles <- lapply(df[setdiff(CATEGORICAS, "addr_state")], function(v) {
    frec <- table(v) / length(v)
    c(sort(names(frec)[frec >= 0.01]), "OTRA")
  })
  # Estado (50 categorías): codificación WoE para no crear 50 columnas
  r$woe_estado <- woe_iv(df$addr_state, df$y)$tabla
  r
}

# -----------------------------------------------------------------------------
# aplicar_receta(df, r)
#   Aplica la receta (sin re-aprender nada) a cualquier conjunto.
# PRE : r proviene de ajustar_receta(); df pasó por extraer_variables().
# POST: mismo número de filas; sin NA en las variables del modelo.
# INV : el orden de las filas se mantiene.
# -----------------------------------------------------------------------------
aplicar_receta <- function(df, r) {
  n0 <- nrow(df)
  out <- data.frame(y = df$y)
  # Indicadores de faltante (antes de imputar)
  for (v in c(IMPUT_ESTRUCTURAL, IMPUT_MEDIANA)) {
    out[[paste0("falta_", v)]] <- as.integer(is.na(df[[v]]))
  }
  for (v in NUMERICAS) {
    x <- winsorizar(df[[v]], r$limites[[v]])
    if (v %in% IMPUT_ESTRUCTURAL) x[is.na(x)] <- r$max_estructural[[v]]
    if (v %in% IMPUT_MEDIANA)     x[is.na(x)] <- r$medianas[[v]]
    if (v %in% LOG1P)             x <- log1p(x)
    out[[v]] <- x
  }
  for (v in names(r$niveles)) {
    x <- ifelse(df[[v]] %in% r$niveles[[v]], df[[v]], "OTRA")
    out[[v]] <- factor(x, levels = r$niveles[[v]])
  }
  w <- r$woe_estado
  out$estado_woe <- w$woe[match(df$addr_state, w$grupo)]
  out$estado_woe[is.na(out$estado_woe)] <- 0      # estado no visto => neutro
  stopifnot(nrow(out) == n0, !anyNA(out))
  out
}

receta <- ajustar_receta(ajuste)    # se aprende SÓLO de la porción de ajuste
df_ajuste <- aplicar_receta(ajuste, receta)
df_valid  <- aplicar_receta(valid,  receta)
df_test   <- aplicar_receta(test,   receta)

# -----------------------------------------------------------------------------
# (D) SELECCIÓN — Paso 1: Information Value
#   Escala usual en scoring crediticio (Siddiqi, 2006):
#     IV < 0,02 inútil | 0,02-0,1 débil | 0,1-0,3 media | > 0,3 fuerte
#   Se calcula sobre la variable SIN imputar para que el grupo "faltante"
#   cuente como información.
# -----------------------------------------------------------------------------
candidatas <- c(NUMERICAS, CATEGORICAS)
iv <- sapply(candidatas, function(v) woe_iv(ajuste[[v]], ajuste$y)$iv)
tabla_iv <- data.frame(variable = names(iv), iv = as.numeric(iv))
tabla_iv <- tabla_iv[order(-tabla_iv$iv), ]
tabla_iv$poder <- cut(tabla_iv$iv, c(-Inf, 0.02, 0.1, 0.3, Inf),
                      labels = c("Inútil", "Débil", "Medio", "Fuerte"))
write.csv(tabla_iv, "results/information_value.csv", row.names = FALSE)

g_iv <- ggplot(tabla_iv, aes(x = reorder(variable, iv), y = iv, fill = poder)) +
  geom_col(width = 0.75) +
  geom_hline(yintercept = 0.02, linetype = "dashed") +
  coord_flip() +
  scale_fill_manual(values = c("Inútil" = "#bdbdbd", "Débil" = "#9ecae1",
                               "Medio" = "#2a7ab9", "Fuerte" = "#08306b"),
                    drop = FALSE) +
  labs(title = "Information Value de cada característica candidata",
       subtitle = "Línea punteada: umbral de 0,02 por debajo del cual se descarta",
       x = NULL, y = "Information Value (IV)", fill = "Poder predictivo") +
  tema_tp()
guardar_figura(g_iv, "features_information_value", alto = 6.5)

seleccion_iv <- tabla_iv$variable[tabla_iv$iv >= 0.02]
descartadas_iv <- setdiff(candidatas, seleccion_iv)

# -----------------------------------------------------------------------------
# (D) SELECCIÓN — Paso 2: filtro de correlación
#   Si dos variables numéricas tienen |r| > 0,85 aportan casi la misma
#   información: se conserva la de mayor IV. Esto evita multicolinealidad
#   (coeficientes inestables en la regresión logística).
# -----------------------------------------------------------------------------
num_sel <- intersect(seleccion_iv, NUMERICAS)
cm <- abs(cor(df_ajuste[num_sel]))
descartadas_cor <- character(0)
pares_cor <- character(0)
orden <- num_sel[order(-iv[num_sel])]       # de mayor a menor IV
for (i in seq_along(orden)) {
  a <- orden[i]
  if (a %in% descartadas_cor) next
  for (b in orden[-(1:i)]) {
    if (!(b %in% descartadas_cor) && cm[a, b] > 0.85) {
      descartadas_cor <- c(descartadas_cor, b)
      pares_cor <- c(pares_cor, sprintf("%s (se queda) vs %s (sale): |r| = %.2f",
                                        a, b, cm[a, b]))
    }
  }
}
vars_finales <- setdiff(seleccion_iv, descartadas_cor)
vars_finales[vars_finales == "addr_state"] <- "estado_woe"
# Los indicadores de faltante acompañan a su variable si ésta quedó
indicadores <- paste0("falta_", intersect(c(IMPUT_ESTRUCTURAL, IMPUT_MEDIANA),
                                          vars_finales))
vars_finales <- c(vars_finales, indicadores)
stopifnot(all(vars_finales %in% names(df_ajuste)))

cat("  Candidatas:", length(candidatas),
    "| descartadas por IV:", length(descartadas_iv),
    "| descartadas por correlación:", length(descartadas_cor),
    "| finales (incl. indicadores):", length(vars_finales), "\n")
cat("  Pares correlacionados:\n   ", paste(pares_cor, collapse = "\n    "), "\n")
writeLines(c("Descartadas por IV < 0.02:", descartadas_iv, "",
             "Descartadas por correlación:", pares_cor, "",
             "Variables finales:", vars_finales),
           "results/seleccion_variables.txt")

# -----------------------------------------------------------------------------
# Matrices de diseño estandarizadas (para regresión logística y LASSO)
#   z = (x - media_train) / desvío_train ; categóricas -> variables dummy
# -----------------------------------------------------------------------------
formula_x <- as.formula(paste("~", paste(vars_finales, collapse = " + ")))
matriz_diseno <- function(df) model.matrix(formula_x, data = df)[, -1, drop = FALSE]
X_ajuste <- matriz_diseno(df_ajuste)
medias  <- colMeans(X_ajuste)
desvios <- apply(X_ajuste, 2, sd)
desvios[desvios == 0] <- 1
estandarizar <- function(X) sweep(sweep(X, 2, medias, "-"), 2, desvios, "/")
X_ajuste <- estandarizar(X_ajuste)
X_valid  <- estandarizar(matriz_diseno(df_valid))
X_test   <- estandarizar(matriz_diseno(df_test))
stopifnot(identical(colnames(X_ajuste), colnames(X_test)),
          nrow(X_test) == nrow(test), !anyNA(X_test))

# Tabla LaTeX de IV para el informe
nombres_tex <- function(v) gsub("_", "\\\\_", v)
filas <- sprintf("%s & %s & %s \\\\", nombres_tex(tabla_iv$variable),
                 fmt(tabla_iv$iv, 3), tabla_iv$poder)
writeLines(filas, "results/tabla_iv.tex")

escribir_macro(ARCHIVO_MACROS, "nCandidatas", length(candidatas))
escribir_macro(ARCHIVO_MACROS, "nDescIV", length(descartadas_iv))
escribir_macro(ARCHIVO_MACROS, "nDescCor", length(descartadas_cor))
escribir_macro(ARCHIVO_MACROS, "nFinales", length(vars_finales))
escribir_macro(ARCHIVO_MACROS, "nColumnasX", ncol(X_ajuste))
escribir_macro(ARCHIVO_MACROS, "nAjuste", fmt_miles(nrow(ajuste)))
escribir_macro(ARCHIVO_MACROS, "nValid", fmt_miles(nrow(valid)))
escribir_macro(ARCHIVO_MACROS, "ivSubgrado", fmt(iv[["subgrado_num"]], 3))
escribir_macro(ARCHIVO_MACROS, "ivTasa", fmt(iv[["int_rate"]], 3))
