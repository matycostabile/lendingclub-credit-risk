# =============================================================================
# 05_evaluacion.R — Evaluación final en el conjunto de PRUEBA (2014-S1)
# -----------------------------------------------------------------------------
# PRE : existen prob_test, prob_valid, umbrales, modelos, y_test, test,
#       tabla_coef, auc_fuga (04_modelos.R).
# POST: en results/ quedan las tablas de métricas (CSV y LaTeX) y en figures/
#       las curvas ROC, matrices de confusión, calibración, importancia de
#       variables, scatter de resultados y análisis económico.
# INV : las predicciones no se modifican; el modelo "principal" se elige por
#       su AUC en VALIDACIÓN (no en prueba), para no hacer trampa.
# =============================================================================

cat("\n[5/5] Evaluación en prueba (2014-S1)...\n")

nombres_modelos <- names(prob_test)
auc_valid <- sapply(nombres_modelos, function(n) auc(prob_valid[[n]], y_valid))
MEJOR <- names(which.max(auc_valid[nombres_modelos != "Benchmark (sub-grado LC)"]))
cat("  Modelo principal (mejor AUC en validación):", MEJOR, "\n")

# ---- 1. Tabla de métricas ------------------------------------------------------
# Brier = (1/n) * sum (p_i - y_i)^2   (error cuadrático de la probabilidad)
filas <- list(); rocs <- list(); matrices <- list()
for (n in nombres_modelos) {
  p <- prob_test[[n]]
  rocs[[n]] <- pROC::roc(y_test, p, quiet = TRUE, direction = "<")
  ic <- as.numeric(pROC::ci.auc(rocs[[n]], method = "delong"))
  cls <- metricas_clasificacion(p, y_test, umbrales[[n]])
  matrices[[n]] <- cls$matriz
  filas[[n]] <- data.frame(
    modelo = n, auc_valid = auc_valid[[n]], auc = ic[2],
    auc_ic_inf = ic[1], auc_ic_sup = ic[3], gini = 2 * ic[2] - 1,
    ks = estadistico_ks(p, y_test), brier = mean((p - y_test)^2),
    umbral = umbrales[[n]], t(cls$metricas), row.names = NULL)
}
tabla_metricas <- do.call(rbind, filas)
write.csv(tabla_metricas, "results/metricas_prueba.csv", row.names = FALSE)
print(format(tabla_metricas[, c("modelo", "auc", "gini", "ks", "brier",
                                "sensibilidad", "especificidad", "f1")],
             digits = 3), row.names = FALSE)

# Test de DeLong: ¿la diferencia de AUC es estadísticamente significativa?
dl_bench <- pROC::roc.test(rocs[[MEJOR]], rocs[["Benchmark (sub-grado LC)"]],
                           method = "delong")
dl_logit <- pROC::roc.test(rocs[[MEJOR]], rocs[["Regresión logística"]],
                           method = "delong")
cat(sprintf("  DeLong %s vs benchmark: p = %.2e | vs logística: p = %.3f\n",
            MEJOR, dl_bench$p.value, dl_logit$p.value))

# Tabla LaTeX
filas_tex <- with(tabla_metricas, sprintf(
  "%s & %s & %s {\\scriptsize [%s; %s]} & %s & %s & %s & %s & %s & %s & %s \\\\",
  modelo, fmt(auc_valid), fmt(auc), fmt(auc_ic_inf), fmt(auc_ic_sup),
  fmt(gini), fmt(ks), fmt(brier, 4), fmt(accuracy), fmt(sensibilidad),
  fmt(especificidad), fmt(f1)))
filas_tex[nombres_modelos == MEJOR] <- paste0("\\rowcolor{gray!15} ",
                                              filas_tex[nombres_modelos == MEJOR])
writeLines(filas_tex, "results/tabla_metricas.tex")
filas_tex2 <- with(tabla_metricas, sprintf("%s & %s & %s & %s & %s \\\\",
  modelo, fmt(umbral), fmt(precision), fmt(accuracy_balanceada), fmt(f1)))
writeLines(filas_tex2, "results/tabla_metricas_umbral.tex")

# ---- 2. Curvas ROC ------------------------------------------------------------------
df_roc <- do.call(rbind, lapply(nombres_modelos, function(n) {
  r <- rocs[[n]]
  data.frame(modelo = sprintf("%s (AUC = %.3f)", n, as.numeric(r$auc)),
             base = n, fpr = 1 - r$specificities, tpr = r$sensitivities)
}))
etiquetas <- setNames(unique(df_roc$modelo), unique(df_roc$base))
g_roc <- ggplot(df_roc, aes(x = fpr, y = tpr, color = modelo)) +
  geom_abline(linetype = "dashed", color = "grey60") +
  geom_line(linewidth = 0.8) +
  scale_color_manual(values = setNames(PALETA_MODELOS[names(etiquetas)], etiquetas)) +
  coord_equal() +
  labs(title = "Curvas ROC en el conjunto de prueba (2014-S1)",
       x = "1 - Especificidad (tasa de falsos positivos)",
       y = "Sensibilidad (tasa de verdaderos positivos)", color = NULL) +
  tema_tp() + theme(legend.position = "right") +
  guides(color = guide_legend(ncol = 1))
guardar_figura(g_roc, "resultado_roc", ancho = 7.5, alto = 4.6)

# ---- 3. Matrices de confusión ----------------------------------------------------
df_cm <- do.call(rbind, lapply(c("Benchmark (sub-grado LC)", MEJOR), function(n) {
  m <- matrices[[n]]
  d <- data.frame(real = rep(rownames(m), 2), pred = rep(colnames(m), each = 2),
                  n = as.vector(m), modelo = n)
  d$pct_fila <- d$n / ave(d$n, d$real, FUN = sum)
  d
}))
df_cm$real <- factor(df_cm$real, levels = rev(c("Pagó (0)", "Default (1)")))
df_cm$pred <- factor(df_cm$pred, levels = c("Pagó (0)", "Default (1)"))
df_cm$modelo <- factor(df_cm$modelo, levels = c("Benchmark (sub-grado LC)", MEJOR))
g_cm <- ggplot(df_cm, aes(x = pred, y = real, fill = pct_fila)) +
  geom_tile(color = "white") +
  geom_text(aes(label = sprintf("%s\n(%.1f%% de la fila)",
                                formatC(n, format = "d", big.mark = ".", decimal.mark = ","), 100 * pct_fila)),
            size = 3.2) +
  facet_wrap(~ modelo) +
  scale_fill_gradient(low = "#f7fbff", high = "#6baed6", limits = c(0, 1),
                      guide = "none") +
  labs(title = "Matrices de confusión en prueba",
       subtitle = "Umbral elegido en validación con el índice de Youden",
       x = "Predicción", y = "Valor real") +
  tema_tp()
guardar_figura(g_cm, "resultado_matriz_confusion", ancho = 7, alto = 3.4)

# ---- 4. Calibración ---------------------------------------------------------------
df_cal <- do.call(rbind, lapply(nombres_modelos, function(n) {
  p <- prob_test[[n]]
  dec <- cut(p, unique(quantile(p, seq(0, 1, 0.1))), include.lowest = TRUE)
  data.frame(modelo = n, predicha = tapply(p, dec, mean),
             observada = tapply(y_test, dec, mean))
}))
g_cal <- ggplot(df_cal, aes(x = predicha, y = observada, color = modelo)) +
  geom_abline(linetype = "dashed", color = "grey60") +
  geom_line(linewidth = 0.7) + geom_point(size = 1.6) +
  scale_color_manual(values = PALETA_MODELOS) +
  scale_x_continuous(labels = function(v) paste0(100 * v, "%")) +
  scale_y_continuous(labels = function(v) paste0(100 * v, "%")) +
  labs(title = "Calibración: probabilidad predicha vs. tasa de default observada",
       subtitle = "Cada punto es un decil de probabilidad predicha (prueba)",
       x = "Probabilidad de default predicha (promedio del decil)",
       y = "Tasa de default observada", color = NULL) +
  tema_tp() + guides(color = guide_legend(nrow = 2))
guardar_figura(g_cal, "resultado_calibracion", alto = 4.4)

# ---- 5. Distribución de scores (separación de clases / KS) -------------------------
df_den <- data.frame(p = prob_test[[MEJOR]],
                     estado = factor(ifelse(y_test == 1, "Default", "Pagó"),
                                     levels = c("Pagó", "Default")))
g_den <- ggplot(df_den, aes(x = p, fill = estado)) +
  geom_density(alpha = 0.5, color = NA) +
  geom_vline(xintercept = umbrales[[MEJOR]], linetype = "dashed") +
  scale_fill_manual(values = c("Pagó" = COLOR_PAGO, "Default" = COLOR_DEFAULT)) +
  scale_x_continuous(labels = function(v) paste0(100 * v, "%")) +
  labs(title = paste("Distribución de la probabilidad predicha —", MEJOR),
       subtitle = "Prueba. Línea punteada: umbral de decisión",
       x = "Probabilidad de default predicha", y = "Densidad", fill = NULL) +
  tema_tp()
guardar_figura(g_den, "resultado_densidad_scores", alto = 3.6)

# ---- 6. Scatter de resultados: modelo vs. precio de LendingClub ----------------------
set.seed(SEMILLA)
idx <- sample(length(y_test), 6000)
df_sc <- data.frame(tasa = test$int_rate[idx], p = prob_test[[MEJOR]][idx],
                    estado = factor(ifelse(y_test[idx] == 1, "Default", "Pagó"),
                                    levels = c("Pagó", "Default")))
cor_tasa_pd <- cor(test$int_rate, prob_test[[MEJOR]], method = "spearman")
g_sc <- ggplot(df_sc, aes(x = tasa, y = p, color = estado)) +
  geom_point(alpha = 0.35, size = 1) +
  scale_color_manual(values = c("Pagó" = COLOR_PAGO, "Default" = COLOR_DEFAULT)) +
  scale_x_continuous(labels = function(v) paste0(100 * v, "%")) +
  scale_y_continuous(labels = function(v) paste0(100 * v, "%")) +
  labs(title = "Riesgo estimado por el modelo vs. tasa cobrada por LendingClub",
       subtitle = sprintf("%s, 6.000 préstamos de prueba. Correlación de Spearman = %.2f",
                          MEJOR, cor_tasa_pd),
       x = "Tasa de interés asignada por LendingClub",
       y = "Probabilidad de default (modelo)", color = NULL) +
  tema_tp()
guardar_figura(g_sc, "resultado_scatter_pd_tasa")

# ---- 7. Importancia de variables ------------------------------------------------------
imp_rf <- modelos[["Random Forest"]]$variable.importance
imp_gbm <- summary(modelos[["Gradient Boosting"]], plotit = FALSE)
imp_gbm <- setNames(imp_gbm$rel.inf, as.character(imp_gbm$var))
df_imp <- rbind(
  data.frame(variable = names(imp_rf), imp = 100 * imp_rf / max(imp_rf),
             modelo = "Random Forest (permutación)"),
  data.frame(variable = names(imp_gbm), imp = 100 * imp_gbm / max(imp_gbm),
             modelo = "Gradient Boosting (influencia relativa)"))
orden_imp <- names(sort(tapply(df_imp$imp, df_imp$variable, mean)))
df_imp$variable <- factor(df_imp$variable, levels = orden_imp)
g_imp <- ggplot(df_imp, aes(x = variable, y = imp, fill = modelo)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.75) +
  coord_flip() +
  scale_fill_manual(values = c("Random Forest (permutación)" = "#e7a33e",
                               "Gradient Boosting (influencia relativa)" = COLOR_DEFAULT)) +
  labs(title = "Importancia relativa de las variables",
       subtitle = "Escalada a 100 para la variable más importante de cada modelo",
       x = NULL, y = "Importancia relativa", fill = NULL) +
  tema_tp()
guardar_figura(g_imp, "resultado_importancia", alto = 5.6)

# Coeficientes de la regresión logística (odds ratios con IC 95%)
tc <- head(tabla_coef, 12)
tc$ic_inf <- exp(tc$beta - 1.96 * tc$error_std)
tc$ic_sup <- exp(tc$beta + 1.96 * tc$error_std)
g_or <- ggplot(tc, aes(x = reorder(variable, odds_ratio), y = odds_ratio)) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  geom_pointrange(aes(ymin = ic_inf, ymax = ic_sup),
                  color = ifelse(tc$odds_ratio > 1, COLOR_DEFAULT, COLOR_PAGO)) +
  coord_flip() +
  labs(title = "Regresión logística: odds ratio por 1 desvío estándar",
       subtitle = "12 variables de mayor efecto, con IC 95%. Mayor a 1 = más riesgo",
       x = NULL, y = "Odds ratio") +
  tema_tp()
guardar_figura(g_or, "resultado_odds_ratios", alto = 4.2)

filas_coef <- with(head(tabla_coef, 10), sprintf("%s & %s & %s & %s \\\\",
  gsub("_", "\\\\_", variable), fmt(beta), fmt(odds_ratio),
  ifelse(p_valor < 0.001, "$<0{,}001$", fmt(p_valor))))
writeLines(filas_coef, "results/tabla_coeficientes.tex")

# ---- 8. Análisis económico: ¿cuánta plata se gana o se pierde? -----------------------
# Para cada tasa de aprobación a (10%, 20%, ..., 100%) se aprueban los
# préstamos de MENOR riesgo según cada criterio y se calcula el retorno
# REAL obtenido (usando lo que efectivamente se cobró):
#   retorno = sum(total_pymnt) / sum(funded_amnt) - 1   (en los 3 años)
# Nota: total_pymnt sólo se usa para EVALUAR; nunca entró al modelo.
retorno_por_aprobacion <- function(score, tasas) {
  ord <- order(score)                     # menor riesgo primero
  sapply(tasas, function(a) {
    k <- ceiling(a * length(score)); sel <- ord[seq_len(k)]
    c(retorno = sum(test$total_pymnt[sel]) / sum(test$funded_amnt[sel]) - 1,
      default = mean(y_test[sel]),
      ganancia = sum(test$total_pymnt[sel] - test$funded_amnt[sel]))
  })
}
tasas_aprob <- seq(0.1, 1, by = 0.1)
set.seed(SEMILLA)
criterios <- list("Benchmark (sub-grado LC)" = prob_test[["Benchmark (sub-grado LC)"]],
                  "Regresión logística" = prob_test[["Regresión logística"]],
                  "Aleatorio" = runif(length(y_test)))
criterios[[MEJOR]] <- prob_test[[MEJOR]]
df_eco <- do.call(rbind, lapply(names(criterios), function(n) {
  r <- retorno_por_aprobacion(criterios[[n]], tasas_aprob)
  data.frame(criterio = n, aprobacion = tasas_aprob, retorno = r["retorno", ],
             default = r["default", ], ganancia = r["ganancia", ])
}))
write.csv(df_eco, "results/analisis_economico.csv", row.names = FALSE)
paleta_eco <- c(PALETA_MODELOS, "Aleatorio" = "black")
g_eco <- ggplot(df_eco, aes(x = aprobacion, y = retorno, color = criterio)) +
  geom_line(linewidth = 0.9) + geom_point(size = 1.6) +
  scale_color_manual(values = paleta_eco) +
  scale_x_continuous(labels = function(v) paste0(100 * v, "%"), breaks = tasas_aprob) +
  scale_y_continuous(labels = function(v) paste0(100 * v, "%")) +
  labs(title = "Retorno real de la cartera según el criterio de aprobación",
       subtitle = "Prueba (2014-S1). Se aprueban primero los préstamos de menor riesgo estimado",
       x = "Porcentaje de solicitudes aprobadas",
       y = "Retorno total a 3 años", color = NULL) +
  tema_tp() + guides(color = guide_legend(nrow = 2))
guardar_figura(g_eco, "resultado_economico", alto = 4.2)

# Tabla económica en 3 puntos de operación
filas_eco <- c()
for (a in c(0.5, 0.7, 0.9, 1.0)) {
  d <- df_eco[abs(df_eco$aprobacion - a) < 1e-9, ]
  get_v <- function(crit, col) d[d$criterio == crit, col]
  filas_eco <- c(filas_eco, sprintf("%s & %s & %s & %s & %s & %s \\\\",
    paste0(100 * a, "\\%"),
    fmt_pct(get_v("Aleatorio", "default")),
    fmt_pct(get_v("Benchmark (sub-grado LC)", "default")),
    fmt_pct(get_v(MEJOR, "default")),
    fmt_pct(get_v("Benchmark (sub-grado LC)", "retorno"), 2),
    fmt_pct(get_v(MEJOR, "retorno"), 2)))
}
writeLines(filas_eco, "results/tabla_economica.tex")

# ---- 9. Fuga de información: comparación visual ----------------------------------------
df_fuga <- data.frame(
  modelo = factor(c("Benchmark (sub-grado LC)", MEJOR, "Con variables posteriores\n(fuga de información)"),
                  levels = c("Benchmark (sub-grado LC)", MEJOR,
                             "Con variables posteriores\n(fuga de información)")),
  auc = c(as.numeric(rocs[["Benchmark (sub-grado LC)"]]$auc),
          as.numeric(rocs[[MEJOR]]$auc), auc_fuga))
g_fuga <- ggplot(df_fuga, aes(x = modelo, y = auc, fill = modelo)) +
  geom_col(width = 0.6) +
  # 4 decimales para el modelo con fuga (con 3 se redondearía a 1.000)
  geom_text(aes(label = ifelse(auc > 0.99, sprintf("%.4f", auc),
                               sprintf("%.3f", auc))), vjust = -0.4) +
  scale_fill_manual(values = c("#9e9e9e", COLOR_PAGO, COLOR_DEFAULT), guide = "none") +
  coord_cartesian(ylim = c(0.5, 1.05)) +
  labs(title = "El espejismo de la fuga de información",
       subtitle = "AUC en prueba. El tercer modelo usa datos que no existen al otorgar el préstamo",
       x = NULL, y = "AUC") +
  tema_tp()
guardar_figura(g_fuga, "resultado_fuga_informacion", alto = 3.6)

# ---- 10. Cifras para el informe --------------------------------------------------------
tm <- function(n, col) tabla_metricas[tabla_metricas$modelo == n, col]
escribir_macro(ARCHIVO_MACROS, "mejorModelo", MEJOR)
for (par in list(c("Bench", "Benchmark (sub-grado LC)"), c("Logit", "Regresión logística"),
                 c("Lasso", "LASSO logístico"), c("RF", "Random Forest"),
                 c("GBM", "Gradient Boosting"), c("Mejor", MEJOR))) {
  escribir_macro(ARCHIVO_MACROS, paste0("auc", par[1]), fmt(tm(par[2], "auc")))
  escribir_macro(ARCHIVO_MACROS, paste0("aucValid", par[1]), fmt(tm(par[2], "auc_valid")))
  escribir_macro(ARCHIVO_MACROS, paste0("gini", par[1]), fmt(tm(par[2], "gini")))
  escribir_macro(ARCHIVO_MACROS, paste0("ks", par[1]), fmt(tm(par[2], "ks")))
  escribir_macro(ARCHIVO_MACROS, paste0("sens", par[1]), fmt(tm(par[2], "sensibilidad")))
  escribir_macro(ARCHIVO_MACROS, paste0("espec", par[1]), fmt(tm(par[2], "especificidad")))
  escribir_macro(ARCHIVO_MACROS, paste0("acc", par[1]), fmt(tm(par[2], "accuracy")))
  escribir_macro(ARCHIVO_MACROS, paste0("prec", par[1]), fmt(tm(par[2], "precision")))
  escribir_macro(ARCHIVO_MACROS, paste0("fUno", par[1]), fmt(tm(par[2], "f1")))
  escribir_macro(ARCHIVO_MACROS, paste0("umbral", par[1]), fmt(tm(par[2], "umbral")))
  escribir_macro(ARCHIVO_MACROS, paste0("brier", par[1]), fmt(tm(par[2], "brier"), 4))
}
exp_p <- floor(log10(dl_bench$p.value))
escribir_macro(ARCHIVO_MACROS, "pDelongBench",
               sprintf("$%s \\times 10^{%d}$",
                       gsub(",", "{,}", fmt(dl_bench$p.value / 10^exp_p, 1),
                            fixed = TRUE), exp_p))
escribir_macro(ARCHIVO_MACROS, "pDelongLogit", fmt(dl_logit$p.value, 3))
escribir_macro(ARCHIVO_MACROS, "corrTasaPD", fmt(cor_tasa_pd, 2))
eco <- function(crit, a, col) df_eco[df_eco$criterio == crit & abs(df_eco$aprobacion - a) < 1e-9, col]
escribir_macro(ARCHIVO_MACROS, "retTodos", fmt_pct(eco(MEJOR, 1, "retorno"), 2))
escribir_macro(ARCHIVO_MACROS, "retMejorSetenta", fmt_pct(eco(MEJOR, 0.7, "retorno"), 2))
escribir_macro(ARCHIVO_MACROS, "retBenchSetenta", fmt_pct(eco("Benchmark (sub-grado LC)", 0.7, "retorno"), 2))
escribir_macro(ARCHIVO_MACROS, "defMejorSetenta", fmt_pct(eco(MEJOR, 0.7, "default")))
escribir_macro(ARCHIVO_MACROS, "defBenchSetenta", fmt_pct(eco("Benchmark (sub-grado LC)", 0.7, "default")))
escribir_macro(ARCHIVO_MACROS, "ganMejorSetenta",
               paste0(fmt(eco(MEJOR, 0.7, "ganancia") / 1e6, 1), "~millones"))
escribir_macro(ARCHIVO_MACROS, "ganBenchSetenta",
               paste0(fmt(eco("Benchmark (sub-grado LC)", 0.7, "ganancia") / 1e6, 1), "~millones"))
escribir_macro(ARCHIVO_MACROS, "ganTodos",
               paste0(fmt(eco(MEJOR, 1, "ganancia") / 1e6, 1), "~millones"))
escribir_macro(ARCHIVO_MACROS, "difGanSetenta",
               paste0(fmt((eco(MEJOR, 0.7, "ganancia") -
                           eco("Benchmark (sub-grado LC)", 0.7, "ganancia")) / 1e6, 1),
                      "~millones"))
cat("  Tablas en results/ y gráficos en figures/\n")
