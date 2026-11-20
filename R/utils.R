# =============================================================================
# utils.R — Funciones auxiliares del proyecto
# -----------------------------------------------------------------------------
# Cada función documenta:
#   PRE  : precondiciones (lo que debe cumplirse al llamarla)
#   POST : postcondiciones (lo que garantiza al terminar)
#   INV  : invariantes (lo que no cambia durante su ejecución)
# Las precondiciones y postcondiciones se verifican en tiempo de ejecución con
# stopifnot(): si algo no se cumple, el programa se detiene con un error claro
# en lugar de seguir con datos corruptos.
# =============================================================================


# -----------------------------------------------------------------------------
# meses_entre(desde, hasta)
#   Cantidad de meses completos entre dos fechas "AAAA-MM".
# PRE : `desde` y `hasta` son vectores character de igual longitud, formato
#       "AAAA-MM" (se admiten NA).
# POST: devuelve un vector numérico de la misma longitud; NA donde haya NA.
# INV : no modifica los argumentos.
# -----------------------------------------------------------------------------
meses_entre <- function(desde, hasta) {
  stopifnot(is.character(desde), is.character(hasta),
            length(desde) == length(hasta))
  a_num <- function(x) {
    anio <- as.numeric(substr(x, 1, 4))
    mes  <- as.numeric(substr(x, 6, 7))
    anio * 12 + mes
  }
  res <- a_num(hasta) - a_num(desde)
  stopifnot(length(res) == length(desde))
  res
}


# -----------------------------------------------------------------------------
# winsorizar(x, limites)
#   Recorta valores extremos a los límites dados (típicamente percentiles 1 y
#   99 calculados SOLO en entrenamiento).
# PRE : x numérico; limites numérico de largo 2 con limites[1] <= limites[2].
# POST: mismo largo que x; todos los valores no-NA dentro de [lim1, lim2].
# INV : los NA siguen siendo NA (la imputación se hace en otro paso).
# -----------------------------------------------------------------------------
winsorizar <- function(x, limites) {
  stopifnot(is.numeric(x), length(limites) == 2, limites[1] <= limites[2])
  y <- pmin(pmax(x, limites[1]), limites[2])
  stopifnot(length(y) == length(x), identical(is.na(y), is.na(x)))
  y
}


# -----------------------------------------------------------------------------
# woe_iv(x, y, n_bins)
#   Weight of Evidence (WoE) e Information Value (IV) de una variable.
#   Numéricas: se discretizan en `n_bins` cuantiles. Los NA forman su propio
#   grupo (la ausencia de dato puede ser informativa).
#   WoE_i = ln( %buenos_i / %malos_i ) ;  IV = sum( (%buenos_i - %malos_i) * WoE_i )
# PRE : length(x) == length(y); y binaria 0/1 con ambas clases presentes.
# POST: lista con `tabla` (data.frame por grupo) e `iv` (numérico >= 0).
# INV : x e y no se modifican.
# -----------------------------------------------------------------------------
woe_iv <- function(x, y, n_bins = 10) {
  stopifnot(length(x) == length(y), all(y %in% c(0, 1)),
            sum(y == 1) > 0, sum(y == 0) > 0)
  if (is.numeric(x)) {
    cortes <- unique(quantile(x, probs = seq(0, 1, length.out = n_bins + 1),
                              na.rm = TRUE))
    if (length(cortes) < 3) {
      grupo <- as.character(x)
    } else {
      grupo <- as.character(cut(x, breaks = cortes, include.lowest = TRUE))
    }
  } else {
    grupo <- as.character(x)
  }
  grupo[is.na(grupo)] <- "(faltante)"
  malos  <- tapply(y, grupo, sum)
  buenos <- tapply(1 - y, grupo, sum)
  # Corrección de 0.5 para evitar log(0) en grupos sin malos o sin buenos
  p_buenos <- (buenos + 0.5) / (sum(buenos) + 0.5)
  p_malos  <- (malos  + 0.5) / (sum(malos)  + 0.5)
  woe <- log(p_buenos / p_malos)
  iv  <- sum((p_buenos - p_malos) * woe)
  tabla <- data.frame(grupo = names(woe), n = as.numeric(buenos + malos),
                      tasa_default = as.numeric(malos / (buenos + malos)),
                      woe = as.numeric(woe), row.names = NULL)
  stopifnot(is.finite(iv), iv >= 0)
  list(tabla = tabla, iv = iv)
}


# -----------------------------------------------------------------------------
# estadistico_ks(prob, y)
#   Estadístico de Kolmogorov-Smirnov: máxima distancia entre las funciones de
#   distribución acumulada de los scores de buenos y malos. Métrica estándar
#   en la industria bancaria.
# PRE : prob numérico en [0,1]; y binaria 0/1; mismo largo.
# POST: numérico en [0,1].
# -----------------------------------------------------------------------------
estadistico_ks <- function(prob, y) {
  stopifnot(length(prob) == length(y), all(y %in% c(0, 1)),
            all(prob >= 0 & prob <= 1))
  F_malos  <- ecdf(prob[y == 1])
  F_buenos <- ecdf(prob[y == 0])
  grilla <- sort(unique(prob))
  ks <- max(abs(F_buenos(grilla) - F_malos(grilla)))
  stopifnot(ks >= 0, ks <= 1)
  ks
}


# -----------------------------------------------------------------------------
# metricas_clasificacion(prob, y, umbral)
#   Matriz de confusión y métricas derivadas para un umbral dado.
#   Convención: clase positiva = 1 = "default" (Charged Off).
# PRE : prob en [0,1]; y binaria; umbral en (0,1).
# POST: lista con matriz de confusión 2x2 (suma = length(y)) y métricas en [0,1].
# INV : TP + FP + TN + FN == length(y).
# -----------------------------------------------------------------------------
metricas_clasificacion <- function(prob, y, umbral) {
  stopifnot(length(prob) == length(y), all(y %in% c(0, 1)),
            umbral > 0, umbral < 1)
  pred <- as.integer(prob >= umbral)
  TP <- sum(pred == 1 & y == 1); FP <- sum(pred == 1 & y == 0)
  TN <- sum(pred == 0 & y == 0); FN <- sum(pred == 0 & y == 1)
  stopifnot(TP + FP + TN + FN == length(y))          # invariante
  div <- function(a, b) ifelse(b == 0, NA_real_, a / b)
  sens <- div(TP, TP + FN); espec <- div(TN, TN + FP)
  prec <- div(TP, TP + FP)
  f1   <- div(2 * prec * sens, prec + sens)
  matriz <- matrix(c(TN, FN, FP, TP), nrow = 2,
                   dimnames = list(Real = c("Pagó (0)", "Default (1)"),
                                   Predicho = c("Pagó (0)", "Default (1)")))
  list(matriz = matriz,
       metricas = c(accuracy = (TP + TN) / length(y),
                    sensibilidad = sens, especificidad = espec,
                    precision = prec, f1 = f1,
                    accuracy_balanceada = (sens + espec) / 2))
}


# -----------------------------------------------------------------------------
# umbral_youden(prob, y)
#   Umbral que maximiza el índice J de Youden = sensibilidad + especificidad - 1.
#   Se calcula en VALIDACIÓN, nunca en test.
# PRE : prob en [0,1]; y binaria con ambas clases.
# POST: numérico en (0,1).
# -----------------------------------------------------------------------------
umbral_youden <- function(prob, y) {
  stopifnot(length(prob) == length(y), all(y %in% c(0, 1)))
  roc_obj <- pROC::roc(y, prob, quiet = TRUE, direction = "<")
  u <- pROC::coords(roc_obj, "best", best.method = "youden",
                    ret = "threshold", transpose = TRUE)[1]
  u <- as.numeric(u)
  stopifnot(is.finite(u), u > 0, u < 1)
  u
}


# -----------------------------------------------------------------------------
# guardar_figura(grafico, nombre, ancho, alto)
#   Guarda un ggplot en PDF (vectorial, para LaTeX) y PNG (para el README).
# PRE : `grafico` es un objeto ggplot; `nombre` sin extensión.
# POST: existen figures/<nombre>.pdf y figures/<nombre>.png.
# -----------------------------------------------------------------------------
guardar_figura <- function(grafico, nombre, ancho = 6.5, alto = 4) {
  stopifnot(inherits(grafico, "ggplot"), is.character(nombre))
  dir.create("figures", showWarnings = FALSE)
  ggplot2::ggsave(file.path("figures", paste0(nombre, ".pdf")), grafico,
                  width = ancho, height = alto, device = cairo_pdf)
  ggplot2::ggsave(file.path("figures", paste0(nombre, ".png")), grafico,
                  width = ancho, height = alto, dpi = 150)
  stopifnot(file.exists(file.path("figures", paste0(nombre, ".pdf"))))
  invisible(NULL)
}


# -----------------------------------------------------------------------------
# tema_tp()
#   Tema visual común a todos los gráficos.
# -----------------------------------------------------------------------------
tema_tp <- function() {
  ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"),
                   panel.grid.minor = ggplot2::element_blank(),
                   legend.position = "bottom")
}

# Paleta fija: mismo color = mismo significado en todo el trabajo
COLOR_PAGO    <- "#2a7ab9"   # azul  -> pagó
COLOR_DEFAULT <- "#d1495b"   # rojo  -> default
PALETA_MODELOS <- c("Benchmark (sub-grado LC)" = "#9e9e9e",
                    "Regresión logística"      = "#2a7ab9",
                    "LASSO logístico"          = "#66a61e",
                    "Random Forest"            = "#e7a33e",
                    "Gradient Boosting"        = "#d1495b")


# -----------------------------------------------------------------------------
# escribir_macro(archivo, nombre, valor)
#   Agrega una macro LaTeX \newcommand{\nombre}{valor} al archivo de números.
#   Así el informe toma los resultados directamente del código y nunca quedan
#   cifras desactualizadas.
# PRE : nombre contiene solo letras (requisito de LaTeX).
# POST: el archivo tiene una línea más.
# -----------------------------------------------------------------------------
escribir_macro <- function(archivo, nombre, valor) {
  stopifnot(grepl("^[A-Za-z]+$", nombre))
  cat(sprintf("\\newcommand{\\%s}{%s}\n", nombre, valor),
      file = archivo, append = TRUE)
}

# Formatos numéricos para el informe (coma decimal, estilo argentino)
fmt <- function(x, d = 3) formatC(x, format = "f", digits = d, decimal.mark = ",")
fmt_pct <- function(x, d = 1) paste0(formatC(100 * x, format = "f", digits = d,
                                             decimal.mark = ","), "\\%")
fmt_miles <- function(x) formatC(x, format = "d", big.mark = ".", decimal.mark = ",")
