# =============================================================================
# 04_modelos.R — Entrenamiento de modelos de clasificación
# -----------------------------------------------------------------------------
# Modelos (de menor a mayor complejidad):
#   M0  Benchmark      : regresión logística usando SÓLO el sub-grado de
#                        LendingClub. Responde: ¿qué tan bien predice la
#                        calificación que ya calcula la plataforma?
#   M1  Regresión logística con todas las características seleccionadas
#   M2  LASSO logístico (penalización L1: selección embebida de variables)
#   M3  Random Forest
#   M4  Gradient Boosting
# + Experimento de FUGA DE INFORMACIÓN (qué pasa si se usan variables que
#   sólo se conocen después de otorgar el préstamo).
#
# Protocolo:
#   1) Hiperparámetros elegidos maximizando el AUC en VALIDACIÓN (2013-S2).
#   2) Umbral de decisión elegido en VALIDACIÓN (índice de Youden).
#   3) Con los hiperparámetros elegidos se re-entrena con TODO el
#      entrenamiento (2012-2013) y recién ahí se predice PRUEBA (2014-S1).
#   La prueba se usa una única vez, al final.
#
# PRE : existen df_ajuste, df_valid, df_test, X_ajuste, X_valid, X_test,
#       vars_finales (03_features.R).
# POST: existen las listas `modelos`, `prob_valid`, `prob_test` y `umbrales`
#       con una entrada por modelo; todas las probabilidades están en [0,1].
# INV : df_test y X_test no se usan para ninguna decisión de modelado.
# =============================================================================

cat("\n[4/5] Entrenamiento de modelos...\n")

NUCLEOS <- max(1, parallel::detectCores() - 1)
df_train <- rbind(df_ajuste, df_valid)          # entrenamiento completo
X_train  <- rbind(X_ajuste, X_valid)
y_ajuste <- df_ajuste$y; y_valid <- df_valid$y
y_train  <- df_train$y;  y_test  <- df_test$y
stopifnot(nrow(X_train) == length(y_train), nrow(X_test) == length(y_test))

auc <- function(prob, y) as.numeric(pROC::auc(pROC::roc(y, prob, quiet = TRUE,
                                                        direction = "<")))
modelos <- list(); prob_valid <- list(); prob_test <- list()
formula_rf <- as.formula(paste("factor(y) ~", paste(vars_finales, collapse = " + ")))

# -----------------------------------------------------------------------------
# M0 — Benchmark: sólo el sub-grado de LendingClub
# -----------------------------------------------------------------------------
NOMBRE <- "Benchmark (sub-grado LC)"
m0_aj <- glm(y ~ subgrado_num, data = df_ajuste, family = binomial)
prob_valid[[NOMBRE]] <- predict(m0_aj, df_valid, type = "response")
modelos[[NOMBRE]] <- glm(y ~ subgrado_num, data = df_train, family = binomial)
prob_test[[NOMBRE]] <- predict(modelos[[NOMBRE]], df_test, type = "response")
cat(sprintf("  M0 benchmark            AUC valid = %.4f\n",
            auc(prob_valid[[NOMBRE]], y_valid)))

# -----------------------------------------------------------------------------
# M1 — Regresión logística completa
#   P(y=1|x) = 1 / (1 + exp(-(b0 + b'x)))  — estimada por máxima verosimilitud
# -----------------------------------------------------------------------------
NOMBRE <- "Regresión logística"
ajustar_glm <- function(X, y) glm(y ~ ., data = data.frame(y = y, X),
                                  family = binomial)
m1_aj <- ajustar_glm(X_ajuste, y_ajuste)
prob_valid[[NOMBRE]] <- predict(m1_aj, data.frame(X_valid), type = "response")
modelos[[NOMBRE]] <- ajustar_glm(X_train, y_train)
prob_test[[NOMBRE]] <- predict(modelos[[NOMBRE]], data.frame(X_test), type = "response")
cat(sprintf("  M1 regresión logística  AUC valid = %.4f\n",
            auc(prob_valid[[NOMBRE]], y_valid)))

# Tabla de coeficientes (odds ratio por 1 desvío estándar de cada variable)
coefs <- summary(modelos[[NOMBRE]])$coefficients
tabla_coef <- data.frame(variable = rownames(coefs), beta = coefs[, 1],
                         error_std = coefs[, 2], p_valor = coefs[, 4],
                         odds_ratio = exp(coefs[, 1]), row.names = NULL)
tabla_coef <- tabla_coef[tabla_coef$variable != "(Intercept)", ]
tabla_coef <- tabla_coef[order(-abs(tabla_coef$beta)), ]
write.csv(tabla_coef, "results/coeficientes_logistica.csv", row.names = FALSE)

# -----------------------------------------------------------------------------
# M2 — LASSO logístico
#   min_b  -logL(b) + lambda * sum|b_j|   ; lambda por validación cruzada (5)
#   Se usa lambda.1se: el modelo más simple a 1 error estándar del mejor.
# -----------------------------------------------------------------------------
NOMBRE <- "LASSO logístico"
set.seed(SEMILLA)
m2_aj <- cv.glmnet(X_ajuste, y_ajuste, family = "binomial", alpha = 1,
                   nfolds = 5, type.measure = "auc")
prob_valid[[NOMBRE]] <- as.numeric(predict(m2_aj, X_valid, s = "lambda.1se",
                                           type = "response"))
set.seed(SEMILLA)
modelos[[NOMBRE]] <- cv.glmnet(X_train, y_train, family = "binomial", alpha = 1,
                               nfolds = 5, type.measure = "auc")
prob_test[[NOMBRE]] <- as.numeric(predict(modelos[[NOMBRE]], X_test,
                                          s = "lambda.1se", type = "response"))
coef_lasso <- as.matrix(coef(modelos[[NOMBRE]], s = "lambda.1se"))[-1, 1]
vars_lasso_cero <- names(coef_lasso)[coef_lasso == 0]
write(c("", "Coeficientes llevados a cero por LASSO (lambda.1se):", vars_lasso_cero),
      file = "results/seleccion_variables.txt", append = TRUE)
cat(sprintf("  M2 LASSO                AUC valid = %.4f | coeficientes en cero: %d de %d\n",
            auc(prob_valid[[NOMBRE]], y_valid), length(vars_lasso_cero),
            length(coef_lasso)))

# -----------------------------------------------------------------------------
# M3 — Random Forest (Breiman, 2001)
#   Promedio de muchos árboles entrenados con muestras bootstrap y un
#   subconjunto aleatorio de `mtry` variables en cada división.
#   Búsqueda en grilla pequeña de mtry y tamaño mínimo de nodo.
# -----------------------------------------------------------------------------
NOMBRE <- "Random Forest"
grilla_rf <- expand.grid(mtry = c(3, 5), min.node.size = c(50, 200))
grilla_rf$auc_valid <- NA
for (i in seq_len(nrow(grilla_rf))) {
  m <- ranger(formula_rf, data = df_ajuste, num.trees = 300,
              mtry = grilla_rf$mtry[i], min.node.size = grilla_rf$min.node.size[i],
              probability = TRUE, num.threads = NUCLEOS, seed = SEMILLA)
  p <- predict(m, df_valid)$predictions[, "1"]
  grilla_rf$auc_valid[i] <- auc(p, y_valid)
  if (grilla_rf$auc_valid[i] == max(grilla_rf$auc_valid, na.rm = TRUE)) {
    prob_valid[[NOMBRE]] <- p
  }
}
mejor_rf <- grilla_rf[which.max(grilla_rf$auc_valid), ]
modelos[[NOMBRE]] <- ranger(formula_rf, data = df_train, num.trees = 500,
                            mtry = mejor_rf$mtry,
                            min.node.size = mejor_rf$min.node.size,
                            probability = TRUE, importance = "permutation",
                            num.threads = NUCLEOS, seed = SEMILLA)
prob_test[[NOMBRE]] <- predict(modelos[[NOMBRE]], df_test)$predictions[, "1"]
write.csv(grilla_rf, "results/grilla_random_forest.csv", row.names = FALSE)
cat(sprintf("  M3 Random Forest        AUC valid = %.4f (mtry=%d, min.node.size=%d)\n",
            mejor_rf$auc_valid, mejor_rf$mtry, mejor_rf$min.node.size))

# -----------------------------------------------------------------------------
# M4 — Gradient Boosting (Friedman, 2001)
#   Suma secuencial de árboles chicos; cada árbol corrige los errores
#   (gradiente de la log-verosimilitud) de los anteriores.
#   F_m(x) = F_{m-1}(x) + nu * h_m(x)
#   Se elige profundidad y cantidad de árboles por AUC en validación.
# -----------------------------------------------------------------------------
NOMBRE <- "Gradient Boosting"
formula_gbm <- as.formula(paste("y ~", paste(vars_finales, collapse = " + ")))
N_MAX <- 1000
arboles_eval <- seq(100, N_MAX, by = 100)
grilla_gbm <- expand.grid(profundidad = c(3, 5), n_arboles = arboles_eval)
grilla_gbm$auc_valid <- NA
mejor_auc_gbm <- -Inf
for (prof in c(3, 5)) {
  set.seed(SEMILLA)
  m <- gbm(formula_gbm, data = df_ajuste, distribution = "bernoulli",
           n.trees = N_MAX, interaction.depth = prof, shrinkage = 0.03,
           n.minobsinnode = 100, bag.fraction = 0.8, verbose = FALSE)
  P <- predict(m, df_valid, n.trees = arboles_eval, type = "response")
  for (j in seq_along(arboles_eval)) {
    a <- auc(P[, j], y_valid)
    grilla_gbm$auc_valid[grilla_gbm$profundidad == prof &
                         grilla_gbm$n_arboles == arboles_eval[j]] <- a
    if (a > mejor_auc_gbm) { mejor_auc_gbm <- a; prob_valid[[NOMBRE]] <- P[, j] }
  }
}
mejor_gbm <- grilla_gbm[which.max(grilla_gbm$auc_valid), ]
set.seed(SEMILLA)
modelos[[NOMBRE]] <- gbm(formula_gbm, data = df_train, distribution = "bernoulli",
                         n.trees = mejor_gbm$n_arboles,
                         interaction.depth = mejor_gbm$profundidad,
                         shrinkage = 0.03, n.minobsinnode = 100,
                         bag.fraction = 0.8, verbose = FALSE)
prob_test[[NOMBRE]] <- predict(modelos[[NOMBRE]], df_test,
                               n.trees = mejor_gbm$n_arboles, type = "response")
write.csv(grilla_gbm, "results/grilla_gradient_boosting.csv", row.names = FALSE)
cat(sprintf("  M4 Gradient Boosting    AUC valid = %.4f (profundidad=%d, árboles=%d)\n",
            mejor_gbm$auc_valid, mejor_gbm$profundidad, mejor_gbm$n_arboles))

g_gbm <- ggplot(grilla_gbm, aes(x = n_arboles, y = auc_valid,
                                color = factor(profundidad))) +
  geom_line(linewidth = 1) + geom_point(size = 1.5) +
  geom_point(data = mejor_gbm, size = 4, shape = 21, fill = NA, color = "black") +
  scale_color_manual(values = c("3" = COLOR_PAGO, "5" = COLOR_DEFAULT)) +
  labs(title = "Ajuste de hiperparámetros de Gradient Boosting",
       subtitle = "AUC en validación (2013-S2) según cantidad de árboles; círculo = elegido",
       x = "Cantidad de árboles", y = "AUC en validación",
       color = "Profundidad") +
  tema_tp()
guardar_figura(g_gbm, "modelo_tuning_gbm", alto = 3.6)

# -----------------------------------------------------------------------------
# Umbrales de decisión: se eligen en VALIDACIÓN con el índice de Youden
# -----------------------------------------------------------------------------
umbrales <- sapply(names(prob_valid), function(n) umbral_youden(prob_valid[[n]], y_valid))

# Postcondiciones
stopifnot(all(names(prob_test) == names(prob_valid)),
          all(sapply(prob_test, function(p) all(p >= 0 & p <= 1))),
          all(sapply(prob_test, length) == length(y_test)))

# -----------------------------------------------------------------------------
# EXPERIMENTO: fuga de información (data leakage)
#   Se agregan tres variables que sólo existen DESPUÉS de otorgar el préstamo:
#   capital efectivamente devuelto, recuperos y monto del último pago.
#   Un modelo así parece casi perfecto pero es inútil para decidir a quién
#   prestarle, porque el día de la solicitud esas variables no existen.
# -----------------------------------------------------------------------------
posteriores <- function(df_orig) {
  cbind(prop_capital_devuelto = df_orig$total_rec_prncp / df_orig$funded_amnt,
        hubo_recupero = as.numeric(df_orig$recoveries > 0),
        log_ultimo_pago = log1p(df_orig$last_pymnt_amnt))
}
X_fuga_train <- cbind(X_train, posteriores(rbind(ajuste, valid)))
X_fuga_test  <- cbind(X_test,  posteriores(test))
m_fuga <- suppressWarnings(ajustar_glm(X_fuga_train, y_train))
prob_fuga <- suppressWarnings(predict(m_fuga, data.frame(X_fuga_test),
                                      type = "response"))
auc_fuga <- auc(prob_fuga, y_test)
cat(sprintf("  Experimento de fuga: AUC en prueba = %.4f (¡engañosamente alto!)\n",
            auc_fuga))

escribir_macro(ARCHIVO_MACROS, "rfMtry", mejor_rf$mtry)
escribir_macro(ARCHIVO_MACROS, "rfNodo", mejor_rf$min.node.size)
escribir_macro(ARCHIVO_MACROS, "gbmProf", mejor_gbm$profundidad)
escribir_macro(ARCHIVO_MACROS, "gbmArboles", mejor_gbm$n_arboles)
escribir_macro(ARCHIVO_MACROS, "lassoCeros", length(vars_lasso_cero))
escribir_macro(ARCHIVO_MACROS, "lassoTotal", length(coef_lasso))
escribir_macro(ARCHIVO_MACROS, "aucFuga", fmt(auc_fuga, 4))
