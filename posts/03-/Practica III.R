# Librerías ----
install.packages("survey")
#remotes::install_github("djalmapessoa/convey")

library(convey) #Paqueteria para hacer análisis de concentración del ingreso con "Complex Survey Samples"
library(survey) #Análisis de encuestas de muestras complejas
library(flextable) 
library(dplyr) 
library(vardpoor) #Para el cálculo del coeficiente de Gini con muestras complejas
library(reldist) #Incluye la función para el cálculo del Gini
library(dineq) #Indice de Theil 
library(tidyverse)
library(psych)
library(haven)
library(data.table)

source("../../visuales.R")

# Datos ----
base<-read_csv("concentradohogar.csv") %>% 
  mutate(ent=substr(ubica_geo,1,2),
         entidad=case_when(ent=="01"  ~ "Aguascalientes",
                           ent=="02"  ~ "Baja California",
                           ent=="03"  ~ "Baja California Sur",
                           ent=="04"  ~ "Campeche",
                           ent=="05"  ~ "Coahuila",
                           ent=="06"  ~ "Colima",
                           ent=="07"  ~ "Chiapas",
                           ent=="08"  ~ "Chihuahua",
                           ent=="09"  ~ "Ciudad de Mexico",
                           ent=="10" ~	"Durango",
                           ent=="11" ~	"Guanajuato",
                           ent=="12" ~	"Guerrero",
                           ent=="13" ~	"Hidalgo",
                           ent=="14" ~	"Jalisco",
                           ent=="15" ~	"Mexico",
                           ent=="16" ~	"Michoacan",
                           ent=="17" ~	"Morelos",
                           ent=="18" ~	"Nayarit",
                           ent=="19" ~	"Nuevo Leon",
                           ent=="20" ~	"Oaxaca",
                           ent=="21" ~	"Puebla",
                           ent=="22" ~	"Queretaro",
                           ent=="23" ~	"Quintana Roo",
                           ent=="24" ~	"San Luis Potosi",
                           ent=="25" ~	"Sinaloa",
                           ent=="26" ~	"Sonora",
                           ent=="27" ~	"Tabasco",
                           ent=="28" ~	"Tamaulipas",
                           ent=="29" ~	"Tlaxcala",
                           ent=="30" ~	"Veracruz",
                           ent=="31" ~	"Yucatan",
                           ent=="32" ~	"Zacatecas"),
         rururb=case_when(tam_loc=="1"  ~ "Urbano",
                          tam_loc=="2"  ~ "Urbano",
                          tam_loc=="3"  ~ "Urbano",
                          tam_loc=="4"  ~ "Rural"),
         ictpc=ing_cor/tot_integ
         ) %>% 
  select(folioviv, 
         foliohog, 
         ubica_geo, 
         tam_loc, 
         est_socio,
         est_dis, 
         upm, 
         factor, 
         clase_hog, 
         sexo_jefe, 
         edad_jefe, 
         educa_jefe,
         tot_integ, 
         ing_cor, 
         ingtrab, 
         negocio, 
         rentas, 
         transfer, 
         otros_ing,
         ent, 
         entidad, 
         rururb)

describe(base)

des_base <- svydesign(ids = ~upm , 
                      weights = ~factor, 
                      strata=~est_dis, 
                      data = base) 

des_base <- convey_prep(des_base)

# Curva de Lorenz ----

curva_lorenz <- svylorenz(~ing_cor, 
                          des_base, 
                          quantiles = seq(0,1,.10), 
                          na.rm = TRUE)

curva_lorenz <- as.data.frame(curva_lorenz)

# Vector para el eje X (deciles)
X <- seq(from = 0, 
         to = 1, 
         by = 0.1)

# Creamos el gráfico a partir de la paqueteria ggplot2
grafica_lorenz <- ggplot(curva_lorenz, 
                         aes(x=X, 
                             y=lorenz)) +
  geom_line(color = "red") + 
  geom_point() +
  geom_abline(intercept=0, 
              slope=1) +
  labs(x = "Participación acumulada de la población", y = "Participación acumulada del ingreso total",
       title = "Curva de Lorenz para el ingreso de los hogares en México 2024", 
       caption = "Fuente: Elaboración propia con datos de la ENIGH (2024)") +
  scale_x_continuous(n.breaks = 10)


grafica_lorenz

# Ahora generamos los intervalos de confianza
curva_lorenz_ic <- data.frame(estimate = coef(curva_lorenz), 
                              standard_error = SE(curva_lorenz), 
                              ci_lower_bound = 
                                coef(curva_lorenz)+
                                SE(curva_lorenz)*qt(0.025,
                                                    degf(subset(des_base,
                                                                !is.na(ing_cor)))),
                              ci_upper_bound = 
                                coef(curva_lorenz) + SE(curva_lorenz)*qt(0.975,degf(subset(des_base,!is.na(ing_cor)))))

# Vaciamos los resultados en una tabla con la paqueteria flextable
tabla_lorenz <- flextable(lorenz_curve_ci) %>% 
  theme_vanilla() %>% 
  set_header_labels(estimate = "Estimación", standard_error = "Error estándar",
                    ci_lower_bound = "IC inferior", ci_upper_bound = "IC superior") %>% 
  add_header_row(values = c("Tabla 1. Curva de Lorenz para el ingreso de los hogares en México 2024"), colwidths = c(4)) %>%
  add_footer_lines("Fuente: Elaboración propia con datos de la ENIGH (2024)") %>% 
  align_nottext_col(align = "center", header = T, footer = F) %>% 
  fontsize(size = 9, part = "footer")

tabla_lorenz

# Gráfica de la Curva de Lorenz con intervalos de confianza
lorenz_ci <- ggplot(lorenz_curve_ci, aes(x=X, y=estimate)) +
  geom_line(color = "red") + 
  geom_line(aes(y = ci_lower_bound), color = 1, linetype = "dashed", lwd = 0.6) +
  geom_line(aes(y = ci_upper_bound), color = 1, linetype = "dashed", lwd = 0.6) +
  geom_abline(intercept=0, slope=1) +
  labs(x = "Participación acumulada de la población", y = "Participación en el ingreso total",
       title = "Curva de Lorenz del ingreso corriente total con intervalos de confianza. \nMéxico 2024", 
       caption = "Fuente: Elaboración propia con datos de la ENIGH (2024)") +
  theme_classic() + 
  scale_x_continuous(n.breaks = 10) + 
  theme(panel.grid.major.y = element_line(colour = "grey80"),
        panel.grid.major.x = element_line(colour = "grey80")) +
  theme(plot.caption.position = "plot", plot.caption = element_text(hjust = 0)) 

lorenz_ci

############################
###MEDIDAS DE DESIGUALDAD###
############################

#### GINI ####

# Dos formas distintas para calcular el gini
# 1. Utilizando directamente el factor de expansión
gini(base$ing_cor, weights=base$factor)
# 2. Utilizando todo el diseño de la muestra compleja
svygini(~ing_cor, design = des_base)

# Podemos obtener el gini de acuerdo a distintas clasificaciones, ej. por estados:
gini_ent <- svyby(~ing_cor, ~entidad, design=des_base, svygini)
gini_ent <- as.data.frame(gini_ent)

# Coeficientes de variación
CV_ent <- cv(svyby(~ing_cor, ~entidad, design=des_base, svygini))
CV_ent <- as.data.frame(CV_ent)

#Generamos una tabla con ambos data frame para ver resultados juntos
Gini_ent_cv <- cbind(gini_ent, CV_ent)

# Para redondear los datos a dos dígitos
Gini_ent_cv$ing_cor <- round(Gini_ent_cv$ing_cor,2)
Gini_ent_cv$se <- round(Gini_ent_cv$se,2)
Gini_ent_cv$CV_ent <- round(Gini_ent_cv$CV_ent,2)

tabla_Gini_ent_cv <- flextable(Gini_ent_cv) %>% 
  theme_vanilla() %>% 
  set_header_labels(entidad = "Entidad federativa", ing_cor = "Gini del ingreso corriente",
                    se = "Error estándar", CV_ent = "Coeficiente de variación") %>% 
  set_caption(caption = "Tabla 2. Coeficiente de Gini por entidad federativa, México 2024.") %>% 
  add_footer_lines("Fuente: Elaboración propia con datos de la ENIGH (2024)") %>% 
  align_nottext_col(align = "center", header = T, footer = T) %>% 
  fontsize(size = 9, part = "footer")

tabla_Gini_ent_cv

####THEIL####
# Nos quedamos con los ingresos corrientes mayores estricto a cero
base1<-subset(base, ing_cor>0)
theil.wtd(base1$ing_cor, weights = base1$factor)

# Con svy
svygei(~ing_cor,subset(des_base, ing_cor>0), epsilon = 1)

### Resumen de varias medidas de desigualdad ###
#(Sin errores estandar)
inequality_decomp <- dineq_rb(ing_cor~1, weights="factor", data=base)
medidas_de_desigualdad <- as.data.frame(inequality_decomp$inequality_measures)
medidas_de_desigualdad





############################## MEDIDAS DE DESIGUALDAD ########################
################ Clase Estadistica e Indicadores Sociales 2027-1 #############
######################### Practica III - version Rural/Urbano #################
#
# Comparacion del ingreso corriente de los hogares entre DOS entidades
# federativas contrastantes (una de ingreso alto, una de ingreso bajo),
# segmentado por ambito Rural/Urbano.
#
# Flujo:
#   1) Preparar base (igual que el script original)
#   2) Definir las dos entidades a comparar (parametrizable)
#   3) Armar los 4 grupos (entidad x rururb) con el diseño muestral
#   4) Coeficiente de variacion del ingreso -> valida si conviene el analisis
#   5) Curvas de Lorenz (separadas y superpuestas)
#   6) Tabla flextable con Gini, Theil y varianza de logaritmos por grupo
#   7) Prueba de hipotesis (Wald) comparando Gini entre grupos
################################################################################

setwd("C:/Users/cesar/Desktop/Clase PUED/enigh2024")

# ---- 0. Paqueteria ----
# install.packages("survey")
# remotes::install_github("djalmapessoa/convey")

library(convey)     # Analisis de concentracion del ingreso con "Complex Survey Samples"
library(survey)     # Analisis de encuestas de muestras complejas
library(flextable)  # Crear tablas
library(dplyr)      # Manipulacion de datos
library(vardpoor)   # Calculo del Gini con muestras complejas
library(reldist)    # Funcion alterna para el calculo del GINI
library(dineq)      # Indice de Theil
library(tidyverse)
library(psych)
library(haven)
library(data.table)

################################################################################
# 1) PREPARACION DE LA BASE (igual estructura que el script original)
################################################################################

concentrado <- read_dta("concentradohogar24.dta")

concen <- mutate(concentrado, ent = substr(ubica_geo, 1, 2),
                 entidad = case_when(
                   ent == "01" ~ "Aguascalientes",
                   ent == "02" ~ "Baja California",
                   ent == "03" ~ "Baja California Sur",
                   ent == "04" ~ "Campeche",
                   ent == "05" ~ "Coahuila",
                   ent == "06" ~ "Colima",
                   ent == "07" ~ "Chiapas",
                   ent == "08" ~ "Chihuahua",
                   ent == "09" ~ "Ciudad de Mexico",
                   ent == "10" ~ "Durango",
                   ent == "11" ~ "Guanajuato",
                   ent == "12" ~ "Guerrero",
                   ent == "13" ~ "Hidalgo",
                   ent == "14" ~ "Jalisco",
                   ent == "15" ~ "Mexico",
                   ent == "16" ~ "Michoacan",
                   ent == "17" ~ "Morelos",
                   ent == "18" ~ "Nayarit",
                   ent == "19" ~ "Nuevo Leon",
                   ent == "20" ~ "Oaxaca",
                   ent == "21" ~ "Puebla",
                   ent == "22" ~ "Queretaro",
                   ent == "23" ~ "Quintana Roo",
                   ent == "24" ~ "San Luis Potosi",
                   ent == "25" ~ "Sinaloa",
                   ent == "26" ~ "Sonora",
                   ent == "27" ~ "Tabasco",
                   ent == "28" ~ "Tamaulipas",
                   ent == "29" ~ "Tlaxcala",
                   ent == "30" ~ "Veracruz",
                   ent == "31" ~ "Yucatan",
                   ent == "32" ~ "Zacatecas"))

concen <- mutate(concen, rururb = case_when(
  tam_loc == "1" ~ "Urbano",
  tam_loc == "2" ~ "Urbano",
  tam_loc == "3" ~ "Urbano",
  tam_loc == "4" ~ "Rural"))

base <- select(concen, folioviv, foliohog, ubica_geo, tam_loc, est_socio,
               est_dis, upm, factor, clase_hog, sexo_jefe, edad_jefe, educa_jefe,
               tot_integ, ing_cor, ingtrab, negocio, rentas, transfer, otros_ing,
               ent, entidad, rururb)

# Ingreso corriente total per capita (util como referencia adicional)
base <- base %>%
  mutate(ictpc = ing_cor / tot_integ)

describe(base)

################################################################################
# 2) ENTIDADES A COMPARAR (aqui se elige el contraste alto vs bajo ingreso)
################################################################################


estado1_alto <- "Ciudad de Mexico"   # entidad de ingreso alto
estado2_alto <- "Nuevo Leon"         # entidad de ingreso alto 
estado1_bajo <- "Oaxaca"             # entidad de ingreso bajo 
estado2_bajo <- "Chiapas"            # entidad de ingreso bajo


################################################################################
# 3) DISENO MUESTRAL Y LOS 4 GRUPOS (entidad x rururb)
################################################################################

# Diseno de encuesta compleja a nivel nacional
des_nacional <- svydesign(ids = ~upm, weights = ~factor, strata = ~est_dis, data = base)
des_nacional <- convey_prep(des_nacional)

# Se subsetea el DISENO (no la base de datos) para conservar correctamente
# estratos y UPMs al momento de estimar varianzas
des_2estados <- subset(des_nacional, entidad %in% c(estado1_alto, estado2_alto, estado1_bajo, estado2_bajo))

# Tabla de combinaciones -> los 4 grupos del ejercicio
grupos <- expand.grid(entidad = c(estado1_alto, estado2_alto, estado1_bajo, estado2_bajo),
                      rururb = c("Urbano", "Rural"),
                      stringsAsFactors = FALSE)
grupos$grupo <- paste(grupos$entidad, grupos$rururb, sep = " - ")
grupos

################################################################################
# 4) COEFICIENTE DE VARIACION DEL INGRESO (viabilidad del analisis)
################################################################################
# CV de la media del ingreso corriente por cada uno de los 4 grupos.
# Regla practica: CV < 15% confiable, 15-30% usar con cautela, > 30% poco fiable.

tab_cv <- svyby(~ing_cor, ~entidad + rururb, des_2estados, svymean,
                vartype = c("se", "cv"), na.rm = TRUE)
tab_cv <- as.data.frame(tab_cv)
tab_cv$ing_cor <- round(tab_cv$ing_cor, 1)
tab_cv$se <- round(tab_cv$se, 1)
tab_cv$cv <- round(tab_cv$cv * 100, 2)  # en porcentaje

tabla_cv <- flextable(tab_cv) %>%
  theme_vanilla() %>%
  set_header_labels(entidad = "Entidad", rururb = "Ambito",
                    ing_cor = "Ingreso corriente medio", se = "Error estandar",
                    cv = "CV (%)") %>%
  set_caption(caption = "Tabla 1. Coeficiente de variacion del ingreso corriente por entidad y ambito, ENIGH 2024.") %>%
  add_footer_lines("Fuente: Elaboracion propia con datos de la ENIGH (2024).") %>%
  align_nottext_col(align = "center", header = TRUE, footer = TRUE) %>%
  fontsize(size = 9, part = "footer")

tabla_cv

# Revisen aqui si algun grupo tiene CV alto (o muestra muy chica) antes de
# interpretar sus curvas de Lorenz / indices de desigualdad mas adelante.
svyby(~ing_cor, ~entidad + rururb, des_2estados, unwtd.count)


################################################################################
# 5) CURVAS DE LORENZ POR GRUPO, CON INTERVALO DE CONFIANZA (separadas y superpuestas)
################################################################################

lorenz_list <- list()

for (i in 1:nrow(grupos)) {
  sub_des <- subset(des_2estados,
                    entidad == grupos$entidad[i] & rururb == grupos$rururb[i])
  
  lc <- svylorenz(~ing_cor, sub_des, quantiles = seq(0, 1, .10), na.rm = TRUE)
  df <- as.data.frame(lc)
  df$decil <- seq(0, 1, .10)
  df$grupo <- grupos$grupo[i]
  df$entidad <- grupos$entidad[i]
  df$rururb <- grupos$rururb[i]
  
  # --- Intervalo de confianza (misma logica que el script original) ---
  gl <- degf(subset(sub_des, !is.na(ing_cor)))
  df$ci_lower <- coef(lc) + SE(lc) * qt(0.025, gl)
  df$ci_upper <- coef(lc) + SE(lc) * qt(0.975, gl)
  
  lorenz_list[[i]] <- df
}

lorenz_all <- bind_rows(lorenz_list)

# --- Funcion para graficar dos entidades a la vez, dentro de un mismo ambito ---
graficar_lorenz <- function(datos, entidades, ambito, titulo) {
  df <- datos %>% filter(entidad %in% entidades, rururb == ambito)
  
  ggplot(df, aes(x = decil, y = lorenz, color = entidad, fill = entidad)) +
    geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), alpha = 0.15, color = NA) +
    geom_line() +
    geom_point(size = 1) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "grey40") +
    labs(x = "Participacion acumulada de la poblacion",
         y = "Participacion acumulada del ingreso total",
         color = "Entidad", fill = "Entidad",
         title = titulo,
         caption = "Fuente: Elaboracion propia con datos de la ENIGH (2024).") +
    theme_classic() +
    scale_x_continuous(n.breaks = 10) +
    theme(panel.grid.major.y = element_line(colour = "grey85"),
          panel.grid.major.x = element_line(colour = "grey85"),
          plot.caption.position = "plot", plot.caption = element_text(hjust = 0))
}

# Rural: Nuevo Leon vs. Oaxaca
fig_rural_nl_oax <- graficar_lorenz(lorenz_all, c(estado2_alto, estado1_bajo), "Rural",
                                    "Curvas de Lorenz - Rural: Nuevo Leon vs. Oaxaca")
fig_rural_nl_oax

# Rural: Ciudad de Mexico vs. Chiapas
fig_rural_cdmx_chis <- graficar_lorenz(lorenz_all, c(estado1_alto, estado2_bajo), "Rural",
                                       "Curvas de Lorenz - Rural: CDMX vs. Chiapas")
fig_rural_cdmx_chis

# Urbano: Nuevo Leon vs. Oaxaca
fig_urbano_nl_oax <- graficar_lorenz(lorenz_all, c(estado2_alto, estado1_bajo), "Urbano",
                                     "Curvas de Lorenz - Urbano: Nuevo Leon vs. Oaxaca")
fig_urbano_nl_oax

# Urbano: Ciudad de Mexico vs. Chiapas
fig_urbano_cdmx_chis <- graficar_lorenz(lorenz_all, c(estado1_alto, estado2_bajo), "Urbano",
                                        "Curvas de Lorenz - Urbano: CDMX vs. Chiapas")
fig_urbano_cdmx_chis

# --- Curvas separadas, una por panel (las 8 juntas, con IC 95%) ---
fig_lorenz_separadas <- ggplot(lorenz_all, aes(x = decil, y = lorenz)) +
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), fill = "red", alpha = 0.15) +
  geom_line(color = "red") +
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  facet_wrap(~grupo) +
  labs(x = "Participacion acumulada de la poblacion",
       y = "Participacion acumulada del ingreso total",
       title = "Curvas de Lorenz por entidad y ambito (con IC 95%)",
       caption = "Fuente: Elaboracion propia con datos de la ENIGH (2024).") +
  theme_classic() +
  theme(plot.caption.position = "plot", plot.caption = element_text(hjust = 0))

fig_lorenz_separadas



################################################################################
# 6) TABLA CON GINI, THEIL Y VARIANZA DE LOGARITMOS POR GRUPO
################################################################################

calcular_medidas <- function(sub_des, entidad_i, rururb_i) {
  
  # --- Gini con error estandar (diseno complejo) ---
  g <- svygini(~ing_cor, design = sub_des)
  gini_est <- coef(g)[1]
  gini_se  <- SE(g)[1]
  
  # --- Theil (sin error estandar, con svygei epsilon = 1) ---
  sub_des_pos <- subset(sub_des, ing_cor > 0)
  theil_est <- coef(svygei(~ing_cor, sub_des_pos, epsilon = 1))[1]
  
  # --- Varianza de logaritmos del ingreso (ponderada) ---
  datos <- model.frame(sub_des_pos)
  log_ing <- log(datos$ing_cor)
  pesos <- weights(sub_des_pos)
  media_pond <- weighted.mean(log_ing, pesos)
  var_log <- sum(pesos * (log_ing - media_pond)^2) / sum(pesos)
  
  data.frame(entidad = entidad_i, rururb = rururb_i,
             gini = gini_est, gini_se = gini_se,
             theil = theil_est, var_log = var_log)
}

medidas_list <- list()
for (i in 1:nrow(grupos)) {
  sub_des <- subset(des_2estados,
                    entidad == grupos$entidad[i] & rururb == grupos$rururb[i])
  medidas_list[[i]] <- calcular_medidas(sub_des, grupos$entidad[i], grupos$rururb[i])
}

medidas_desigualdad <- bind_rows(medidas_list)
medidas_desigualdad[ , c("gini", "gini_se", "theil", "var_log")] <-
  round(medidas_desigualdad[ , c("gini", "gini_se", "theil", "var_log")], 3)

medidas_desigualdad

tabla_medidas <- flextable(medidas_desigualdad) %>%
  theme_vanilla() %>%
  set_header_labels(entidad = "Entidad", rururb = "Ambito",
                    gini = "Gini", gini_se = "Error estandar (Gini)",
                    theil = "Theil (GE1)", var_log = "Varianza de logaritmos") %>%
  set_caption(caption = "Tabla 2. Medidas de desigualdad del ingreso corriente por entidad y ambito, ENIGH 2024.") %>%
  add_footer_lines("Fuente: Elaboracion propia con datos de la ENIGH (2024).") %>%
  align_nottext_col(align = "center", header = TRUE, footer = TRUE) %>%
  fontsize(size = 9, part = "footer")

tabla_medidas


################################################################################
# 7) PRUEBA DE HIPOTESIS: ¿DIFIEREN LOS GINI ENTRE GRUPOS?
################################################################################
# Prueba de Wald para la diferencia de dos medias (aqui, dos estimaciones de
# Gini), usando la estimacion y el error estandar que ya tenemos:
#     z = (Gini_1 - Gini_2) / sqrt(SE_1^2 + SE_2^2)
# Valido cuando los dos grupos provienen de muestras independientes, lo cual
# se cumple aqui porque las entidades no comparten UPMs/estratos.

prueba_gini <- function(tabla, fila1, fila2) {
  g1 <- tabla$gini[fila1]; se1 <- tabla$gini_se[fila1]
  g2 <- tabla$gini[fila2]; se2 <- tabla$gini_se[fila2]
  
  z <- (g1 - g2) / sqrt(se1^2 + se2^2)
  p <- 2 * (1 - pnorm(abs(z)))
  
  data.frame(grupo_1 = tabla$grupo[fila1], grupo_2 = tabla$grupo[fila2],
             diferencia = round(g1 - g2, 3), z = round(z, 3), valor_p = round(p, 4))
}

medidas_desigualdad$grupo <- paste(medidas_desigualdad$entidad, medidas_desigualdad$rururb, sep = " - ")

# Ejemplo: comparar el ambito Urbano entre las dos entidades
fila_urbano_alto <- which(medidas_desigualdad$entidad == estado1_alto & medidas_desigualdad$rururb == "Urbano")
fila_urbano_bajo  <- which(medidas_desigualdad$entidad == estado1_bajo  & medidas_desigualdad$rururb == "Urbano")
prueba_gini(medidas_desigualdad, fila_urbano_alto, fila_urbano_bajo)

# Ejemplo: comparar Rural vs Urbano dentro de la misma entidad de ingreso bajo
fila_rural_bajo <- which(medidas_desigualdad$entidad == estado1_bajo & medidas_desigualdad$rururb == "Rural")
prueba_gini(medidas_desigualdad, fila_urbano_bajo, fila_rural_bajo)

fila_urbano_nl <- which(medidas_desigualdad$entidad == estado2_alto & medidas_desigualdad$rururb == "Urbano")
prueba_gini(medidas_desigualdad, fila_urbano_nl, fila_urbano_bajo)



