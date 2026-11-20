# =============================================================================
# 02_exploracion.R — Análisis exploratorio de datos (EDA)
# -----------------------------------------------------------------------------
# PRE : existen `datos`, `train`, `test` (01_preparacion.R).
# POST: se generan en figures/ los gráficos exploratorios.
# INV : no modifica `datos`, `train` ni `test` (sólo los lee).
#       Toda conclusión que guíe el modelado se toma mirando TRAIN; el gráfico
#       temporal muestra test sólo para describir la partición.
# =============================================================================

cat("\n[2/5] Análisis exploratorio...\n")
n_filas_antes <- nrow(datos)   # para verificar el invariante al final

etiqueta_estado <- function(y) factor(ifelse(y == 1, "Default", "Pagó"),
                                      levels = c("Pagó", "Default"))

# ---- 1. Tasa de default por grado de LendingClub ------------------------------
por_grado <- aggregate(y ~ grade, data = train, FUN = mean)
por_grado$n <- as.numeric(table(train$grade)[por_grado$grade])
g1 <- ggplot(por_grado, aes(x = grade, y = y)) +
  geom_col(fill = COLOR_DEFAULT, width = 0.7) +
  geom_text(aes(label = sprintf("%.1f%%\n(n=%s)", 100 * y,
                                formatC(n, format = "d", big.mark = ".", decimal.mark = ","))),
            vjust = -0.2, size = 3) +
  scale_y_continuous(labels = function(v) paste0(100 * v, "%"),
                     expand = expansion(mult = c(0, 0.25))) +
  labs(title = "Tasa de default según el grado asignado por LendingClub",
       subtitle = "Entrenamiento (2012-2013). A = menor riesgo, G = mayor riesgo",
       x = "Grado", y = "Tasa de default") +
  tema_tp()
guardar_figura(g1, "eda_default_por_grado")

# ---- 2. Evolución temporal y partición -----------------------------------------
por_mes <- aggregate(y ~ issue_d, data = datos, FUN = mean)
por_mes$n <- as.numeric(table(datos$issue_d)[por_mes$issue_d])
por_mes$fecha <- as.Date(paste0(por_mes$issue_d, "-01"))
por_mes$conjunto <- ifelse(por_mes$issue_d < "2014-01",
                           "Entrenamiento", "Prueba (fuera de tiempo)")
g2 <- ggplot(por_mes, aes(x = fecha, y = y)) +
  geom_col(aes(fill = conjunto), alpha = 0.85) +
  scale_fill_manual(values = c("Entrenamiento" = COLOR_PAGO,
                               "Prueba (fuera de tiempo)" = "#e7a33e")) +
  scale_y_continuous(labels = function(v) paste0(100 * v, "%")) +
  labs(title = "Tasa de default mensual por fecha de emisión",
       subtitle = "Préstamos a 36 meses. Se entrena con el pasado y se prueba con el futuro",
       x = "Mes de emisión", y = "Tasa de default", fill = NULL) +
  tema_tp()
guardar_figura(g2, "eda_default_temporal")

# ---- 3. Scatter: tasa de interés vs ingreso ------------------------------------
set.seed(SEMILLA)
muestra <- train[!is.na(train$revol_util), ]
muestra <- muestra[sample(nrow(muestra), 6000), ]
muestra$estado <- etiqueta_estado(muestra$y)
g3 <- ggplot(muestra, aes(x = annual_inc, y = int_rate, color = estado)) +
  geom_point(alpha = 0.35, size = 1) +
  scale_x_log10(labels = function(v) paste0("$", v / 1000, "k")) +
  coord_cartesian(xlim = c(10000, 500000)) +
  scale_y_continuous(labels = function(v) paste0(100 * v, "%")) +
  scale_color_manual(values = c("Pagó" = COLOR_PAGO, "Default" = COLOR_DEFAULT)) +
  labs(title = "Tasa de interés vs. ingreso anual",
       subtitle = "Muestra aleatoria de 6.000 préstamos de entrenamiento (eje x logarítmico)",
       x = "Ingreso anual (USD)", y = "Tasa de interés", color = NULL) +
  tema_tp()
guardar_figura(g3, "eda_scatter_tasa_ingreso")

# ---- 4. Scatter: endeudamiento vs uso de tarjetas -------------------------------
g4 <- ggplot(muestra, aes(x = dti, y = revol_util, color = estado)) +
  geom_point(alpha = 0.35, size = 1) +
  geom_smooth(method = "loess", se = FALSE, linewidth = 1, span = 0.9,
              formula = y ~ x) +
  scale_y_continuous(labels = function(v) paste0(100 * v, "%")) +
  coord_cartesian(ylim = c(0, 1.2)) +
  scale_color_manual(values = c("Pagó" = COLOR_PAGO, "Default" = COLOR_DEFAULT)) +
  labs(title = "Relación deuda/ingreso (DTI) vs. uso del crédito rotativo",
       subtitle = "Las curvas son suavizados LOESS por grupo",
       x = "DTI (% del ingreso mensual destinado a deudas)",
       y = "Uso del crédito rotativo", color = NULL) +
  tema_tp()
guardar_figura(g4, "eda_scatter_dti_revol")

# ---- 5. Default por propósito del préstamo --------------------------------------
por_prop <- aggregate(y ~ purpose, data = train, FUN = mean)
por_prop$n <- as.numeric(table(train$purpose)[por_prop$purpose])
por_prop <- por_prop[por_prop$n >= 300, ]
g5 <- ggplot(por_prop, aes(x = reorder(purpose, y), y = y)) +
  geom_col(fill = COLOR_DEFAULT, width = 0.7) +
  geom_hline(yintercept = mean(train$y), linetype = "dashed") +
  coord_flip() +
  scale_y_continuous(labels = function(v) paste0(100 * v, "%")) +
  labs(title = "Tasa de default según el propósito declarado",
       subtitle = "Línea punteada: promedio de entrenamiento (propósitos con n >= 300)",
       x = NULL, y = "Tasa de default") +
  tema_tp()
guardar_figura(g5, "eda_default_proposito", alto = 4.2)

# ---- 6. Datos faltantes ----------------------------------------------------------
vars_orig <- setdiff(names(train), c(VARS_POSTERIORES, "y"))
falt <- data.frame(variable = vars_orig,
                   pct = sapply(train[vars_orig], function(v) mean(is.na(v))))
falt <- falt[falt$pct > 0, ]
g6 <- ggplot(falt, aes(x = reorder(variable, pct), y = pct)) +
  geom_col(fill = "#6c757d", width = 0.7) +
  geom_text(aes(label = sprintf("%.1f%%", 100 * pct)), hjust = -0.1, size = 3) +
  coord_flip() +
  scale_y_continuous(labels = function(v) paste0(100 * v, "%"),
                     expand = expansion(mult = c(0, 0.15))) +
  labs(title = "Porcentaje de datos faltantes por variable (entrenamiento)",
       x = NULL, y = "% faltante") +
  tema_tp()
guardar_figura(g6, "eda_faltantes", alto = 3.2)

# ---- 7. Correlaciones entre variables numéricas -----------------------------------
num_vars <- c("loan_amnt", "int_rate", "installment", "annual_inc", "dti",
              "revol_bal", "revol_util", "open_acc", "total_acc",
              "inq_last_6mths", "delinq_2yrs", "mort_acc", "tot_cur_bal",
              "total_rev_hi_lim")
cm <- cor(train[num_vars], use = "pairwise.complete.obs")
cm_df <- data.frame(v1 = rep(rownames(cm), times = ncol(cm)),
                    v2 = rep(colnames(cm), each = nrow(cm)),
                    r  = as.vector(cm))
g7 <- ggplot(cm_df, aes(x = v1, y = v2, fill = r)) +
  geom_tile(color = "white") +
  geom_text(aes(label = sprintf("%.2f", r)), size = 2.3) +
  scale_fill_gradient2(low = COLOR_PAGO, mid = "white", high = COLOR_DEFAULT,
                       limits = c(-1, 1)) +
  labs(title = "Matriz de correlación de Pearson (entrenamiento)",
       x = NULL, y = NULL, fill = "r") +
  tema_tp() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
guardar_figura(g7, "eda_correlacion", ancho = 6.5, alto = 5.8)

escribir_macro(ARCHIVO_MACROS, "tasaGradoA", fmt_pct(por_grado$y[por_grado$grade == "A"]))
escribir_macro(ARCHIVO_MACROS, "tasaGradoG", fmt_pct(por_grado$y[por_grado$grade == "G"]))
escribir_macro(ARCHIVO_MACROS, "corrCuotaMonto", fmt(cm["loan_amnt", "installment"], 2))

stopifnot(nrow(datos) == n_filas_antes)   # invariante: EDA no modifica datos
cat("  Gráficos exploratorios guardados en figures/\n")
