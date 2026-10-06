###############################################################################
# Medidas de desigualdad con la ENIGH 2022 y 2024 (concentrado de hogares)
#
#  1. Deciles de hogares por ingreso corriente total per cápita: participación,
#     medias, totales, CV con survey e indicadores de brecha (X/I, Palma, S80/S20)
#  2. Centiles del decil X
#  3. Gini y Theil con datos agrupados
#  4. Descomposición de la varianza por tamaño de localidad
#  5. Descomposición del Gini por fuentes de ingreso (Lerman y Yitzhaki)
#  6. Descomposición del Theil por tamaño de localidad (réplica de -ineqdeco-)
#  7. Ingreso 2022 a pesos de 2024, curvas de incidencia y cambios 2022-2024
#
###############################################################################

setwd("D:/Desktop/Clase PUED")   # carpeta donde están los concentrados


# ==============================================================================
# paquetes
# ==============================================================================
library(survey)     # estimación con diseño muestral complejo
library(dplyr)      # manipulación de datos
library(tidyr)      # reorganizar tablas para graficar
library(ggplot2)    # gráficas
library(knitr)      # cuadros
library(kableExtra) # formato de cuadros
library(haven)
library(dineq)      #Indice de Theil 

# Fuentes del ingreso corriente (ing_cor) en el concentrado
fuentes <- c("ingtrab", "rentas", "transfer", "estim_alqu", "otros_ing")
etq_fuentes <- c(ingtrab = "Ingreso por trabajo", rentas = "Renta de la propiedad",
                 transfer = "Transferencias", estim_alqu = "Estimación del alquiler",
                 otros_ing = "Otros ingresos")
etq_tam <- c("100 mil y más hab.", "15 mil a 99,999 hab.",
             "2,500 a 14,999 hab.", "Menos de 2,500 hab.")


base22 <- read_dta("concentradohogar22.dta")
base24 <- read_dta("concentradohogar24.dta")

# Ingreso per cápita y una variable para contar hogares
base22 <- mutate(base22, ictpc = ing_cor / tot_integ, uno = 1)
base24 <- mutate(base24, ictpc = ing_cor / tot_integ, uno = 1)

# Deciles de hogares por ingreso per cápita
base22$deciles <- ntiles.wtd(x = base22$ictpc, n = 10, weights = base22$factor)
base24$deciles <- ntiles.wtd(x = base24$ictpc, n = 10, weights = base24$factor)

# Ingreso de 2022 a pesos de 2024
def24 <- 0.9102291693
base22 <- mutate(base22, ing_cor24 = ing_cor / def24, ictpc24 = ictpc / def24)
base24 <- mutate(base24, ing_cor24 = ing_cor / 1,     ictpc24 = ictpc / 1)

# Diseño muestral
diseno22 <- svydesign(ids = ~upm, weights = ~factor, strata = ~est_dis, data = base22)
diseno24 <- svydesign(ids = ~upm, weights = ~factor, strata = ~est_dis, data = base24)



########## Cuadro 1 ##########

# ---- Cuadro 1. Verificación de las bases ------------------------------------
verifica <- function(b) {
  c(nrow(b),                                        # hogares en muestra
    sum(b$factor),                                  # hogares expandidos
    sum(b$factor * b$tot_integ),                    # personas expandidas
    round(max(abs(b$ing_cor - rowSums(b[, fuentes]))), 2),  # diferencia máxima, en pesos
    sum(b$ing_cor == 0))                            # hogares con ingreso cero (en muestra)
}

cuadro1 <- data.frame(
  Concepto = c("Hogares en muestra",
               "Hogares expandidos",
               "Personas expandidas",
               "Diferencia máxima entre ing_cor y la suma de las cinco fuentes (pesos)",
               "Hogares con ingreso corriente igual a cero"),
  `2022` = verifica(base22),
  `2024` = verifica(base24),
  check.names = FALSE)

cuadro1


kable(cuadro1, format.args = list(big.mark = ","), align = "lrr",
      caption = "Verificación de las bases") |>
  kable_styling(latex_options = "hold_position")

############################


# ==============================================================================
# 2022
# ==============================================================================

# ---- Estimaciones por decil --------------------------------------------------
tot22 <- svyby(~ing_cor24, ~deciles, diseno22, svytotal)   # ingreso total del decil
mh22  <- svyby(~ing_cor24, ~deciles, diseno22, svymean)    # ingreso promedio por hogar
mpc22 <- svyby(~ictpc24,   ~deciles, diseno22, svymean)    # media del ingreso per cápita
hog22 <- svyby(~uno,       ~deciles, diseno22, svytotal)   # hogares
per22 <- svyby(~tot_integ, ~deciles, diseno22, svytotal)   # personas

# Participación de cada decil en el ingreso total (razón de dos totales) y su CV
part22 <- lapply(1:10, function(d)
  svyratio(~I(ing_cor24 * (deciles == d)), ~ing_cor24, diseno22))

tabla22 <- data.frame(
  decil      = as.character(1:10),
  hogares    = coef(hog22),
  personas   = coef(per22),
  total      = coef(tot22),         cv_total = 100 * cv(tot22),
  prom_hogar = coef(mh22),          cv_hogar = 100 * cv(mh22),
  prom_pc    = coef(mpc22),         cv_pc    = 100 * cv(mpc22),
  part       = 100 * sapply(part22, coef),
  cv_part    = 100 * sapply(part22, cv))

# ---- Total nacional ----------------------------------------------------------
totn22 <- svytotal(~ing_cor24, diseno22)
mhn22  <- svymean(~ing_cor24, diseno22)
mpcn22 <- svymean(~ictpc24, diseno22)

nacional22 <- data.frame(
  decil      = "Total",
  hogares    = sum(base22$factor),
  personas   = sum(base22$factor * base22$tot_integ),
  total      = coef(totn22),        cv_total = 100 * cv(totn22)[1],
  prom_hogar = coef(mhn22),         cv_hogar = 100 * cv(mhn22)[1],
  prom_pc    = coef(mpcn22),        cv_pc    = 100 * cv(mpcn22)[1],
  part       = 100,                 cv_part  = NA)

cuadro22 <- rbind(tabla22, nacional22)
rownames(cuadro22) <- NULL
cuadro22

# ---- Indicadores de brecha ---------------------------------------------------
s22 <- tabla22$part / 100                                        # participaciones
q22 <- coef(svyquantile(~ictpc24, diseno22, c(.10, .50, .90)))   # P10, P50, P90

brechas22 <- c(
  "Razón X/I (participaciones)"         = s22[10] / s22[1],
  "Razón X/I (medias per cápita)"       = tabla22$prom_pc[10] / tabla22$prom_pc[1],
  "S80/S20"                             = (s22[9] + s22[10]) / (s22[1] + s22[2]),
  "Palma (X / I a IV)"                  = s22[10] / sum(s22[1:4]),
  "P90/P10 (ingreso per cápita)"        = q22[[3]] / q22[[1]],
  "P90/P50"                             = q22[[3]] / q22[[2]],
  "P50/P10"                             = q22[[2]] / q22[[1]],
  "Participación del 50 % inferior (%)" = 100 * sum(s22[1:5]),
  "Participación del decil X (%)"       = 100 * s22[10])

# ---- Gini y Theil con datos agrupados (diez participaciones) -----------------
L22 <- cumsum(s22)                                      # Lorenz acumulada
gini_agr22  <- 1 - sum((L22 + c(0, L22[-10])) / 10)     # 1 - suma (1/10)(L_d + L_d-1)
theil_agr22 <- sum(s22 * log(10 * s22))                 # suma s_d ln(10 s_d)


# ==============================================================================
# 2024
# ==============================================================================

# ---- Estimaciones por decil --------------------------------------------------
tot24 <- svyby(~ing_cor24, ~deciles, diseno24, svytotal)
mh24  <- svyby(~ing_cor24, ~deciles, diseno24, svymean)
mpc24 <- svyby(~ictpc24,   ~deciles, diseno24, svymean)
hog24 <- svyby(~uno,       ~deciles, diseno24, svytotal)
per24 <- svyby(~tot_integ, ~deciles, diseno24, svytotal)

part24 <- lapply(1:10, function(d)
  svyratio(~I(ing_cor24 * (deciles == d)), ~ing_cor24, diseno24))

tabla24 <- data.frame(
  decil      = as.character(1:10),
  hogares    = coef(hog24),
  personas   = coef(per24),
  total      = coef(tot24),         cv_total = 100 * cv(tot24),
  prom_hogar = coef(mh24),          cv_hogar = 100 * cv(mh24),
  prom_pc    = coef(mpc24),         cv_pc    = 100 * cv(mpc24),
  part       = 100 * sapply(part24, coef),
  cv_part    = 100 * sapply(part24, cv))

# ---- Total nacional ----------------------------------------------------------
totn24 <- svytotal(~ing_cor24, diseno24)
mhn24  <- svymean(~ing_cor24, diseno24)
mpcn24 <- svymean(~ictpc24, diseno24)

nacional24 <- data.frame(
  decil      = "Total",
  hogares    = sum(base24$factor),
  personas   = sum(base24$factor * base24$tot_integ),
  total      = coef(totn24),        cv_total = 100 * cv(totn24)[1],
  prom_hogar = coef(mhn24),         cv_hogar = 100 * cv(mhn24)[1],
  prom_pc    = coef(mpcn24),        cv_pc    = 100 * cv(mpcn24)[1],
  part       = 100,                 cv_part  = NA)

cuadro24 <- rbind(tabla24, nacional24)
rownames(cuadro24) <- NULL
cuadro24

##############################################################

# ==============================================================================
# CUADROS 2, 3 y 4  (CV por decil, y deciles 2022 y 2024)  


options(knitr.kable.NA = "")     # los NA (participación del Total) salen en blanco

# ---- Funciones auxiliares ----------------------------------------------------
romano <- c(as.character(as.roman(1:10)), "Total")        # I, II, ..., X, Total

f_num <- function(x, d = 0) {                              # número con comas
  ifelse(is.na(x), "", formatC(x, format = "f", digits = d, big.mark = ","))
}

# Criterio INEGI: CV < 15 % alta; 15 a 30 % moderada; > 30 % baja
precision <- function(cv) ifelse(cv < 15, "Alta", ifelse(cv <= 30, "Moderada", "Baja"))


# ==============================================================================
# CUADRO 2. Coeficientes de variación (%) por decil
# ==============================================================================
cv22 <- cuadro22[, c("cv_total", "cv_hogar", "cv_pc", "cv_part")]
cv24 <- cuadro24[, c("cv_total", "cv_hogar", "cv_pc", "cv_part")]

# La precisión de cada fila se juzga con el CV más grande de esa fila (ambos años)
cv_max <- apply(cbind(cv22, cv24), 1, max, na.rm = TRUE)

cuadro2 <- data.frame(
  Decil = romano,
  f_num(cv22[[1]], 2), f_num(cv22[[2]], 2), f_num(cv22[[3]], 2), f_num(cv22[[4]], 2),
  f_num(cv24[[1]], 2), f_num(cv24[[2]], 2), f_num(cv24[[3]], 2), f_num(cv24[[4]], 2),
  Precision = precision(cv_max),
  stringsAsFactors = FALSE
)

cuadro2        # versión simple en la consola

kable(cuadro2, format = "html", row.names = FALSE,
      align = c("l", rep("r", 8), "l"),
      col.names = c("Decil", "Total", "Prom. hogar", "Media p. c.", "Particip.",
                    "Total", "Prom. hogar", "Media p. c.", "Particip.", "Precisión"),
      caption = "Cuadro 2: Coeficientes de variación (%) de las estimaciones por decil") %>%
  add_header_above(c(" " = 1, "2022" = 4, "2024" = 4, " " = 1)) %>%
  row_spec(10, extra_css = "border-bottom: 1px solid black;") %>%
  kable_styling(full_width = FALSE, font_size = 12)


# ==============================================================================
# CUADROS 3 y 4. Deciles de hogares (una función para los dos años)
# ==============================================================================
cuadro_deciles <- function(cuadro, anio, num) {
  # 'cuadro' ya trae los 10 deciles + la fila Total (cuadro22 o cuadro24)
  lorenz <- c(cumsum(cuadro$part[1:10]), NA)              # Lorenz acumulada; Total en blanco
  
  tabla <- data.frame(
    Decil    = romano,
    Hogares  = f_num(cuadro$hogares),
    Personas = f_num(cuadro$personas),
    PPH      = f_num(cuadro$personas / cuadro$hogares, 2),   # personas por hogar
    Ingreso  = f_num(cuadro$total / 1e6),                    # millones de pesos
    PromHog  = f_num(cuadro$prom_hogar),
    MediaPC  = f_num(cuadro$prom_pc),
    Part     = f_num(cuadro$part, 2),
    Lorenz   = f_num(lorenz, 2),
    stringsAsFactors = FALSE
  )
  
  print(tabla)     # versión simple en la consola
  
  kable(tabla, format = "html", row.names = FALSE, escape = FALSE,
        align = c("l", rep("r", 8)),
        col.names = c("Decil", "Hogares", "Personas",
                      "Personas<br>por hogar", "Ingreso total<br>(millones)",
                      "Promedio<br>por hogar", "Media per<br>cápita",
                      "Particip.<br>(%)", "Lorenz (%)"),
        caption = paste0("Cuadro ", num, ": Deciles de hogares por ingreso corriente ",
                         "total per cápita, ", anio,
                         " (ingreso trimestral, pesos de 2024)")) %>%
    row_spec(10, extra_css = "border-bottom: 1px solid black;") %>%
    kable_styling(full_width = FALSE, font_size = 12)
}

cuadro_deciles(cuadro22, 2022, 3)    # Cuadro 3
cuadro_deciles(cuadro24, 2024, 4)    # Cuadro 4



################################################################3




# ---- Indicadores de brecha ---------------------------------------------------
s24 <- tabla24$part / 100
q24 <- coef(svyquantile(~ictpc24, diseno24, c(.10, .50, .90)))

brechas24 <- c(
  "Razón X/I (participaciones)"         = s24[10] / s24[1],
  "Razón X/I (medias per cápita)"       = tabla24$prom_pc[10] / tabla24$prom_pc[1],
  "S80/S20"                             = (s24[9] + s24[10]) / (s24[1] + s24[2]),
  "Palma (X / I a IV)"                  = s24[10] / sum(s24[1:4]),
  "P90/P10 (ingreso per cápita)"        = q24[[3]] / q24[[1]],
  "P90/P50"                             = q24[[3]] / q24[[2]],
  "P50/P10"                             = q24[[2]] / q24[[1]],
  "Participación del 50 % inferior (%)" = 100 * sum(s24[1:5]),
  "Participación del decil X (%)"       = 100 * s24[10])

# ---- Gini y Theil con datos agrupados ----------------------------------------
L24 <- cumsum(s24)
gini_agr24  <- 1 - sum((L24 + c(0, L24[-10])) / 10)

theil_agr24 <- sum(s24 * log(10 * s24))


# ==============================================================================
# Los dos años juntos
# ==============================================================================
brechas <- data.frame("2022" = brechas22, "2024" = brechas24, check.names = FALSE)
round(brechas, 2)

agrupados <- data.frame("2022" = c(gini_agr22, theil_agr22),
                        "2024" = c(gini_agr24, theil_agr24),
                        row.names = c("Gini agrupado", "Theil agrupado"),
                        check.names = FALSE)
round(agrupados, 4)


# ==============================================================================
# funciones-descomposicion
# ==============================================================================
# ---- Estadísticos ponderados -------------------------------------------------
w_mean <- function(x, w) sum(w * x) / sum(w)
w_var  <- function(x, w) sum(w * (x - w_mean(x, w))^2) / sum(w)
w_cov  <- function(x, y, w) sum(w * (x - w_mean(x, w)) * (y - w_mean(y, w))) / sum(w)
# Rango fraccional F(y) ponderado (punto medio de cada observación)
w_rank <- function(x, w) {
  o <- order(x); Fo <- (cumsum(w[o]) - 0.5 * w[o]) / sum(w)
  r <- numeric(length(x)); r[o] <- Fo; r
}
# Gini por la fórmula de la covarianza: G = 2 cov(y, F(y)) / media
gini_w <- function(x, w) 2 * w_cov(x, w_rank(x, w), w) / w_mean(x, w)

# ---- Varianza: intra + entre -------------------------------------------------
descomp_var <- function(y, w, g) {
  m <- w_mean(y, w)
  t <- bind_rows(lapply(levels(g), function(k) {
    i <- g == k
    data.frame(grupo = k, pi = sum(w[i]) / sum(w), media = w_mean(y[i], w[i]),
               var = w_var(y[i], w[i]))
  }))
  t$intra <- t$pi * t$var               # aporte del grupo a la intravarianza
  t$entre <- t$pi * (t$media - m)^2     # aporte del grupo a la intervarianza
  list(tabla = t, media = m, total = w_var(y, w), intra = sum(t$intra), entre = sum(t$entre))
}

# ---- Gini por fuentes (Lerman y Yitzhaki) ------------------------------------
gini_fuentes <- function(datos, fuentes, w) {
  Y  <- rowSums(datos[, fuentes]); mu <- w_mean(Y, w)
  FY <- w_rank(Y, w); G <- 2 * w_cov(Y, FY, w) / mu
  t <- bind_rows(lapply(fuentes, function(v) {
    yk <- datos[[v]]; Fk <- w_rank(yk, w)
    Sk <- w_mean(yk, w) / mu                        # peso de la fuente en el ingreso
    Gk <- 2 * w_cov(yk, Fk, w) / w_mean(yk, w)      # Gini de la fuente (incluye ceros)
    Rk <- w_cov(yk, FY, w) / w_cov(yk, Fk, w)       # correlación-Gini con el ingreso total
    data.frame(fuente = v, S = Sk, G = Gk, R = Rk, contrib = Sk * Gk * Rk)
  }))
  t$part_G   <- t$contrib / G           # participación de la fuente en el Gini
  t$eta      <- t$G * t$R / G           # elasticidad-Gini (pseudo-Gini relativo)
  t$marginal <- t$S * (t$eta - 1)       # cambio % del Gini si la fuente sube 1 %
  list(gini = G, tabla = t)
}

# ---- Entropía generalizada por subgrupos (réplica de -ineqdeco- de S. P. Jenkins)
ineqdeco <- function(y, w, g) {
  ok <- !is.na(y) & y > 0 & w > 0
  y <- y[ok]; w <- w[ok]; g <- droplevels(g[ok])
  nucleo <- function(y, w) {
    m <- w_mean(y, w); r <- y / m
    c(sumw = sum(w), mean = m, gem1 = (w_mean(1 / r, w) - 1) / 2,
      ge0 = log(m) - w_mean(log(y), w), ge1 = w_mean(r * log(r), w),
      ge2 = (w_mean(r^2, w) - 1) / 2, gini = gini_w(y, w))
  }
  tot <- nucleo(y, w)
  sub <- as.data.frame(t(sapply(levels(g), function(k) nucleo(y[g == k], w[g == k]))))
  sub$grupo     <- levels(g)
  sub$pop_share <- sub$sumw / tot[["sumw"]]       # pi_k
  sub$rel_mean  <- sub$mean / tot[["mean"]]       # lambda_k
  sub$inc_share <- sub$pop_share * sub$rel_mean   # s_k = pi_k * lambda_k
  v <- sub$pop_share; l <- sub$rel_mean
  list(total = tot, sub = sub, excluidas = sum(!ok),
       within  = c(gem1 = sum(v / l * sub$gem1), ge0 = sum(v * sub$ge0),
                   ge1 = sum(v * l * sub$ge1),   ge2 = sum(v * l^2 * sub$ge2)),
       between = c(gem1 = (sum(v / l) - 1) / 2,  ge0 = sum(v * log(1 / l)),
                   ge1 = sum(v * l * log(l)),    ge2 = (sum(v * l^2) - 1) / 2))
}


# Fuentes del ingreso corriente (ing_cor) en el concentrado
fuentes <- c("ingtrab", "rentas", "transfer", "estim_alqu", "otros_ing")
etq_fuentes <- c(ingtrab = "Ingreso por trabajo", rentas = "Renta de la propiedad",
                 transfer = "Transferencias", estim_alqu = "Estimación del alquiler",
                 otros_ing = "Otros ingresos")
etq_tam <- c("100 mil y más hab.", "15 mil a 99,999 hab.",
             "2,500 a 14,999 hab.", "Menos de 2,500 hab.")

# Tamaño de localidad como factor con etiquetas
base22 <- mutate(base22, tam = factor(as.numeric(tam_loc), levels = 1:4, labels = etq_tam))
base24 <- mutate(base24, tam = factor(as.numeric(tam_loc), levels = 1:4, labels = etq_tam))


# ==============================================================================
# 2022
# ==============================================================================

# ---- 1. Varianza del ingreso corriente por tamaño de localidad ---------------
var22 <- descomp_var(y = base22$ing_cor24, w = base22$factor, g = base22$tam)

cuadro_var22 <- var22$tabla
cuadro_var22$pct_intra <- 100 * cuadro_var22$intra / var22$total   # % de la varianza total
cuadro_var22$pct_entre <- 100 * cuadro_var22$entre / var22$total
cuadro_var22

resumen_var22 <- c(total = var22$total, intra = var22$intra, entre = var22$entre,
                   pct_intra = 100 * var22$intra / var22$total,
                   pct_entre = 100 * var22$entre / var22$total)
resumen_var22

# ---- 2. Gini por fuentes de ingreso ------------------------------------------
gini22 <- gini_fuentes(datos = base22, fuentes = fuentes, w = base22$factor)

cuadro_gini22 <- gini22$tabla
cuadro_gini22$fuente <- etq_fuentes[cuadro_gini22$fuente]   # nombres de las fuentes
cuadro_gini22
gini22$gini                    # Gini del ingreso corriente total
sum(cuadro_gini22$contrib)     # comprobación: la suma de S*G*R es igual al Gini

# ---- 3. Theil por tamaño de localidad ----------------------------------------
theil22 <- ineqdeco(y = base22$ing_cor24, w = base22$factor, g = base22$tam)

cuadro_theil22 <- data.frame(
  grupo  = theil22$sub$grupo,
  pi     = theil22$sub$pop_share,     # proporción de hogares
  media  = theil22$sub$mean,          # ingreso medio del grupo
  lambda = theil22$sub$rel_mean,      # media relativa a la nacional
  s      = theil22$sub$inc_share,     # proporción del ingreso
  theil  = theil22$sub$ge1,           # Theil dentro del grupo
  ge0    = theil22$sub$ge0,           # desviación media logarítmica
  gini   = theil22$sub$gini)
cuadro_theil22

resumen_theil22 <- data.frame(
  indice    = c("Theil GE(1)", "GE(0)"),
  total     = c(theil22$total[["ge1"]],   theil22$total[["ge0"]]),
  intra     = c(theil22$within[["ge1"]],  theil22$within[["ge0"]]),
  entre     = c(theil22$between[["ge1"]], theil22$between[["ge0"]]))
resumen_theil22$pct_intra <- 100 * resumen_theil22$intra / resumen_theil22$total
resumen_theil22$pct_entre <- 100 * resumen_theil22$entre / resumen_theil22$total
resumen_theil22


# ==============================================================================
# 2024
# ==============================================================================

# ---- 1. Varianza del ingreso corriente por tamaño de localidad ---------------
var24 <- descomp_var(y = base24$ing_cor24, w = base24$factor, g = base24$tam)

cuadro_var24 <- var24$tabla
cuadro_var24$pct_intra <- 100 * cuadro_var24$intra / var24$total
cuadro_var24$pct_entre <- 100 * cuadro_var24$entre / var24$total
cuadro_var24

resumen_var24 <- c(total = var24$total, intra = var24$intra, entre = var24$entre,
                   pct_intra = 100 * var24$intra / var24$total,
                   pct_entre = 100 * var24$entre / var24$total)
resumen_var24

# ---- 2. Gini por fuentes de ingreso ------------------------------------------
gini24 <- gini_fuentes(datos = base24, fuentes = fuentes, w = base24$factor)

cuadro_gini24 <- gini24$tabla
cuadro_gini24$fuente <- etq_fuentes[cuadro_gini24$fuente]
cuadro_gini24
gini24$gini
sum(cuadro_gini24$contrib)

# ---- 3. Theil por tamaño de localidad ----------------------------------------
theil24 <- ineqdeco(y = base24$ing_cor24, w = base24$factor, g = base24$tam)

cuadro_theil24 <- data.frame(
  grupo  = theil24$sub$grupo,
  pi     = theil24$sub$pop_share,
  media  = theil24$sub$mean,
  lambda = theil24$sub$rel_mean,
  s      = theil24$sub$inc_share,
  theil  = theil24$sub$ge1,
  ge0    = theil24$sub$ge0,
  gini   = theil24$sub$gini)
cuadro_theil24

resumen_theil24 <- data.frame(
  indice    = c("Theil GE(1)", "GE(0)"),
  total     = c(theil24$total[["ge1"]],   theil24$total[["ge0"]]),
  intra     = c(theil24$within[["ge1"]],  theil24$within[["ge0"]]),
  entre     = c(theil24$between[["ge1"]], theil24$between[["ge0"]]))
resumen_theil24$pct_intra <- 100 * resumen_theil24$intra / resumen_theil24$total
resumen_theil24$pct_entre <- 100 * resumen_theil24$entre / resumen_theil24$total
resumen_theil24


anios <- 2    # años entre los dos levantamientos


# ==============================================================================
# A. GRÁFICAS
# ==============================================================================

# ---- A1. Participación de cada decil en el ingreso total ---------------------
g_part <- rbind(data.frame(anio = "2022", decil = 1:10, part = tabla22$part),
                data.frame(anio = "2024", decil = 1:10, part = tabla24$part))

grafica1<-ggplot(g_part, aes(factor(decil), part, fill = anio)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("2022" = "grey60", "2024" = "#9b2247")) +
  labs(x = "Decil de hogares (ingreso per cápita)", y = "% del ingreso total", fill = NULL,
       title = "Participación de cada decil en el ingreso corriente total") +
  theme_minimal() + theme(legend.position = "top")
# Lectura: si las barras de 2024 son más altas en los deciles bajos y más bajas
# en el decil X, el ingreso se repartió de forma menos concentrada.

# ---- A2. Curva de Lorenz con deciles -----------------------------------------
g_lor <- rbind(data.frame(anio = "2022", p = (0:10) / 10, L = c(0, cumsum(s22))),
               data.frame(anio = "2024", p = (0:10) / 10, L = c(0, cumsum(s24))))

grafica1<-ggplot(g_lor, aes(p, L, colour = anio)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey50") +
  geom_line() + geom_point() +
  scale_colour_manual(values = c("2022" = "grey45", "2024" = "#9b2247")) +
  labs(x = "Hogares acumulados (ordenados por ingreso per cápita)",
       y = "Ingreso acumulado", colour = NULL, title = "Curva de Lorenz con deciles") +
  coord_equal() + theme_minimal() + theme(legend.position = "top")
# Lectura: la diagonal es la igualdad perfecta. Si la curva de 2024 queda por
# encima de la de 2022 en todos los puntos, 2024 es menos desigual con cualquier
# índice (dominancia de Lorenz). Si se cruzan, la conclusión depende del índice.

# ---- A3. Peso de cada fuente en el ingreso y en el Gini ----------------------
g_f <- rbind(
  data.frame(anio = "2022", fuente = cuadro_gini22$fuente,
             ingreso = 100 * cuadro_gini22$S, gini = 100 * cuadro_gini22$part_G),
  data.frame(anio = "2024", fuente = cuadro_gini24$fuente,
             ingreso = 100 * cuadro_gini24$S, gini = 100 * cuadro_gini24$part_G))
g_f <- pivot_longer(g_f, c(ingreso, gini), names_to = "concepto", values_to = "pct")
g_f$concepto <- ifelse(g_f$concepto == "ingreso", "En el ingreso", "En el Gini")

ggplot(g_f, aes(pct, fuente, fill = concepto)) +
  geom_col(position = "dodge") + facet_wrap(~anio) +
  scale_fill_manual(values = c("En el ingreso" = "grey60", "En el Gini" = "#9b2247")) +
  labs(x = "%", y = NULL, fill = NULL,
       title = "Participación de cada fuente en el ingreso y en el Gini") +
  theme_minimal() + theme(legend.position = "top")
# Lectura: una fuente que pesa más en el Gini que en el ingreso concentra
# (elasticidad eta > 1); una que pesa menos en el Gini que en el ingreso iguala.


# ==============================================================================
# C. CAMBIOS 2022 - 2024
# ==============================================================================

# ---- C1. Curva de incidencia del crecimiento por deciles ---------------------
# Tasa geométrica anual: g = (y24 / y22)^(1/T) - 1 ;  lineal: (y24 / y22 - 1) / T
# Intervalo de 95 % con el método delta: ee(g) = (1 + g)/T * raiz(CV22^2 + CV24^2)
gic <- data.frame(decil = 1:10, y22 = tabla22$prom_pc, y24 = tabla24$prom_pc)
gic$geo <- (gic$y24 / gic$y22)^(1 / anios) - 1
gic$lin <- (gic$y24 / gic$y22 - 1) / anios
gic$ee  <- (1 + gic$geo) / anios * sqrt((tabla22$cv_pc / 100)^2 + (tabla24$cv_pc / 100)^2)
gic$li  <- gic$geo - 1.96 * gic$ee
gic$ls  <- gic$geo + 1.96 * gic$ee
# Las mismas tasas para el promedio por hogar y para la masa total de ingreso
gic$geo_hogar <- (tabla24$prom_hogar / tabla22$prom_hogar)^(1 / anios) - 1
gic$geo_total <- (tabla24$total / tabla22$total)^(1 / anios) - 1
gic[, c("geo", "lin", "ee", "li", "ls", "geo_hogar", "geo_total")] <-
  100 * gic[, c("geo", "lin", "ee", "li", "ls", "geo_hogar", "geo_total")]
gic

# Referencia: crecimiento de la media nacional del ingreso per cápita
geo_nac <- 100 * ((nacional24$prom_pc / nacional22$prom_pc)^(1 / anios) - 1)
geo_nac

ggplot(gic, aes(decil, geo)) +
  geom_ribbon(aes(ymin = li, ymax = ls), fill = "#9b2247", alpha = .15) +
  geom_line(colour = "#9b2247") + geom_point(colour = "#9b2247") +
  geom_hline(yintercept = geo_nac, linetype = "dashed", colour = "grey30") +
  scale_x_continuous(breaks = 1:10) +
  labs(x = "Decil de hogares (ingreso per cápita)", y = "Crecimiento real anual (%)",
       title = "Curva de incidencia del crecimiento, 2022 a 2024",
       subtitle = "Media del ingreso per cápita; línea punteada = media nacional") +
  theme_minimal()
# Lectura: curva decreciente = los deciles bajos crecieron más (crecimiento
# igualador); plana = la Lorenz no cambió; creciente = crecimiento concentrador.
# Si todos los deciles tienen tasa positiva, la pobreza por ingresos cae con
# cualquier línea. La curva es anónima: el decil I de 2024 no son los mismos
# hogares que el decil I de 2022.


# ---- C3. Cambio en las participaciones por decil -----------------------------
cambio_part <- data.frame(decil = 1:10, part22 = tabla22$part, part24 = tabla24$part)
cambio_part$cambio <- cambio_part$part24 - cambio_part$part22          # puntos porcentuales
cambio_part$ee <- sqrt((tabla22$part * tabla22$cv_part / 100)^2 +
                         (tabla24$part * tabla24$cv_part / 100)^2)        # muestras independientes
cambio_part$z  <- cambio_part$cambio / cambio_part$ee
cambio_part
# Lectura: |z| > 1.96 indica que el cambio en la participación es
# estadísticamente distinto de cero al 95 %.

# ---- C4. Cambio en los indicadores de brecha ---------------------------------
brechas$Cambio <- brechas$`2024` - brechas$`2022`
round(brechas, 2)
# Lectura: X/I, S80/S20 y Palma a la baja = se cerró la distancia entre extremos.
# P90/P50 y P50/P10 dicen si el cambio ocurrió arriba o abajo de la mediana.

# ---- C5. Cambio en los índices de desigualdad --------------------------------
indices <- data.frame(
  indice = c("Gini (ingreso corriente del hogar)", "Theil GE(1)", "GE(0)",
             "Gini agrupado (deciles)", "Theil agrupado (deciles)"),
  a2022  = c(gini22$gini, theil22$total[["ge1"]], theil22$total[["ge0"]],
             gini_agr22, theil_agr22),
  a2024  = c(gini24$gini, theil24$total[["ge1"]], theil24$total[["ge0"]],
             gini_agr24, theil_agr24))
indices$cambio     <- indices$a2024 - indices$a2022
indices$cambio_pct <- 100 * indices$cambio / indices$a2022
indices
# Lectura: el Gini pondera más el centro, el Theil la parte alta y la GE(0) la
# parte baja. Si todos se mueven en la misma dirección, la conclusión es robusta.

# ---- C6. ¿Qué fuentes movieron el Gini? --------------------------------------
# Como G = suma de S*G*R en cada año, el cambio del Gini es la suma de los
# cambios en las contribuciones de las fuentes.
cambio_fuentes <- data.frame(
  fuente    = cuadro_gini22$fuente,
  S22       = cuadro_gini22$S,        S24 = cuadro_gini24$S,
  G22       = cuadro_gini22$G,        G24 = cuadro_gini24$G,
  R22       = cuadro_gini22$R,        R24 = cuadro_gini24$R,
  contrib22 = cuadro_gini22$contrib,  contrib24 = cuadro_gini24$contrib)
cambio_fuentes$cambio <- cambio_fuentes$contrib24 - cambio_fuentes$contrib22
cambio_fuentes
sum(cambio_fuentes$cambio)       # igual al cambio del Gini:
gini24$gini - gini22$gini
# Lectura: la contribución de una fuente cambia porque cambió su peso en el
# ingreso (S), su desigualdad propia (G) o su correlación con el ingreso total (R).

# ---- C7. Descomposición dinámica de la varianza de los logaritmos ------------
# V24 - V22 = efecto entre (cambian las brechas de medias)
#           + efecto intra (cambia la dispersión dentro de los grupos)
#           + efecto composición (cambia el peso poblacional de los grupos)
# Se usa el logaritmo porque su varianza no depende de la escala (ni del deflactor).
k22 <- base22$ing_cor24 > 0
k24 <- base24$ing_cor24 > 0
vlog22 <- descomp_var(log(base22$ing_cor24[k22]), base22$factor[k22], base22$tam[k22])
vlog24 <- descomp_var(log(base24$ing_cor24[k24]), base24$factor[k24], base24$tam[k24])

r2_22 <- (vlog22$tabla$media - vlog22$media)^2    # distancia de cada grupo a la media
r2_24 <- (vlog24$tabla$media - vlog24$media)^2

var_dinamica <- data.frame(
  grupo       = vlog22$tabla$grupo,
  composicion = (vlog24$tabla$pi - vlog22$tabla$pi) * (r2_24 + vlog24$tabla$var),
  entre       = (r2_24 - r2_22) * vlog22$tabla$pi,
  intra       = (vlog24$tabla$var - vlog22$tabla$var) * vlog22$tabla$pi)
var_dinamica$total <- var_dinamica$composicion + var_dinamica$entre + var_dinamica$intra
var_dinamica
colSums(var_dinamica[, -1])        # la columna total es igual a:
vlog24$total - vlog22$total
# Lectura: el efecto más grande en valor absoluto dice qué movió la desigualdad:
# las brechas entre tamaños de localidad, la dispersión interna o la recomposición.

# ---- C8. Descomposición dinámica de la GE(0) (Mookherjee y Shorrocks) --------
# A: cambia la desigualdad dentro de los grupos
# B y C: cambia la composición de la población
# D: cambian los ingresos medios relativos de los grupos
t22 <- theil22$sub
t24 <- theil24$sub
prom <- function(a, b) (a + b) / 2     # promedio de los dos años

A <- prom(t22$pop_share, t24$pop_share) * (t24$ge0 - t22$ge0)
B <- prom(t22$ge0, t24$ge0) * (t24$pop_share - t22$pop_share)
C <- (prom(t22$rel_mean, t24$rel_mean) - prom(log(t22$rel_mean), log(t24$rel_mean))) *
  (t24$pop_share - t22$pop_share)
D <- (prom(t22$inc_share, t24$inc_share) - prom(t22$pop_share, t24$pop_share)) *
  (log(t24$mean) - log(t22$mean))

ge0_dinamica <- data.frame(
  grupo = t22$grupo, A = A, B = B, C = C, D = D,
  crec_media = 100 * ((t24$mean / t22$mean)^(1 / anios) - 1))   # crecimiento real anual
ge0_dinamica
colSums(ge0_dinamica[, c("A", "B", "C", "D")])
sum(A + B + C + D)                                     # aproximación del cambio:
theil24$total[["ge0"]] - theil22$total[["ge0"]]        # cambio observado de la GE(0)
# Lectura: D negativo significa que los grupos con media inferior a la nacional
# crecieron más rápido que los de media superior (se cierra la brecha entre
# grupos). La descomposición es una aproximación y deja un residuo pequeño.


# ==============================================================================
# SÍNTESIS DE LOS CAMBIOS
# ==============================================================================
cat("\nSÍNTESIS 2022 - 2024 (pesos de 2024)\n",
    "Crecimiento real anual de la media per cápita: nacional ", round(geo_nac, 2),
    " %, decil I ", round(gic$geo[1], 2), " %, decil X ", round(gic$geo[10], 2), " %\n",
    "Participación del decil X: ", round(tabla22$part[10], 2), " % a ",
    round(tabla24$part[10], 2), " %\n",
    "Participación del 50 % inferior: ", round(sum(tabla22$part[1:5]), 2), " % a ",
    round(sum(tabla24$part[1:5]), 2), " %\n",
    "Razón X/I: ", round(brechas$`2022`[1], 1), " a ", round(brechas$`2024`[1], 1),
    " | Palma: ", round(brechas$`2022`[4], 2), " a ", round(brechas$`2024`[4], 2), "\n",
    "Gini: ", round(gini22$gini, 4), " a ", round(gini24$gini, 4),
    " | Theil: ", round(theil22$total[["ge1"]], 4), " a ", round(theil24$total[["ge1"]], 4), "\n",
    "Fuente que más movió el Gini: ",
    cambio_fuentes$fuente[which.max(abs(cambio_fuentes$cambio))], " (",
    round(cambio_fuentes$cambio[which.max(abs(cambio_fuentes$cambio))], 4), ")\n",
    "Parte del Theil entre tamaños de localidad: ",
    round(resumen_theil22$pct_entre[1], 1), " % a ", round(resumen_theil24$pct_entre[1], 1), " %\n",
    sep = "")
