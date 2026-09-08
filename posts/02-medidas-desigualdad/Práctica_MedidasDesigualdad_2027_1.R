############################## MEDIDAS DE DESIGUALDAD   ################################
################ Clase Estadistica e indicadores sociales 2027-1 #######################

################################ Practica II ###############################

setwd("G:/Mi unidad/Cursos2027_1/EstadisticaIndicadoresSociales/CodigosR")

# Instalar paqueteria 
#install.packages("survey")
#remotes::install_github("djalmapessoa/convey")

library(convey) #Paqueteria para hacer análisis de concentración del ingreso con "Complex Survey Samples"
library(survey) #Análisis de encuestas de muestras complejas
library(flextable) #Crear tablas
library(dplyr) #Manipulación de datos
library(vardpoor) #Para el cálculo del coeficiente de gini con muestras complejas
library(reldist)# libreria incluye la funci?n para el c?lculo del GINI
library(dineq) #Indice de Theil 
library(tidyverse)
library(psych)
library(haven)
library(data.table)

### Con la ENIGH 2024
#Se carga la base de datos de concentrado de hogar. 

concentrado<-read_dta("concentradohogar.dta")

#Generar la variable de entidad federativa 

concen <-mutate(concentrado, ent=substr(ubica_geo,1,2),
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
                                          ent=="32" ~	"Zacatecas"))

concen <-mutate(concen, rururb=case_when(tam_loc=="1"  ~ "Urbano",
                                              tam_loc=="2"  ~ "Urbano",
                                              tam_loc=="3"  ~ "Urbano",
                                              tam_loc=="4"  ~ "Rural"))

base<-select(concen, folioviv, foliohog, ubica_geo, tam_loc, est_socio,
             est_dis, upm, factor, clase_hog, sexo_jefe, edad_jefe, educa_jefe,
             tot_integ, ing_cor, ingtrab, negocio, rentas, transfer, otros_ing, 
             ent, entidad, rururb)

# Creamos una nueva variable 'ictpc' (ingreso corriente total per cápita)
base <- base %>% 
  mutate(ictpc=ing_cor/tot_integ)

describe(base)

#### CURVA DE LORENZ ####

# Objeto de diseño de levantamiento linealizado
#se define el diseño de muestra con la unidad  primaria de muestreo, el factor de 
#expansion, el estrato de diseño muestral. 

des_base <- svydesign(ids = ~upm , weights = ~factor, strata=~est_dis, data = base) 
des_base <- convey_prep(des_base)

# Estimacion de la Curva de Lorenz
lorenz_curve <- svylorenz(~ing_cor, des_base, quantiles = seq(0,1,.10), na.rm = TRUE)
# Datos
lorenz_curve

# Gráfica personalizable en ggplot 2
# Transformamos los datos a data frame
curva_lorenz <- as.data.frame(lorenz_curve)

# Creamos un vector para el eje X (deciles)
X <- seq(from = 0, to = 1, by = 0.1)

# Creamos el gráfico a partir de la paqueteria ggplot2
 fig_lorenz <- ggplot(curva_lorenz, aes(x=X, y=lorenz)) +
  geom_line(color = "red") + 
  geom_point() +
  geom_abline(intercept=0, slope=1) +
  labs(x = "Participación acumulada de la población", y = "Participación acumulada del ingreso total",
       title = "Curva de Lorenz para el ingreso de los hogares en México 2024", 
       caption = "Fuente: Elaboración propia con datos de la ENIGH (2024)") +
  theme_classic() + 
  scale_x_continuous(n.breaks = 10) + 
  theme(panel.grid.major.y = element_line(colour = "grey80"),
        panel.grid.major.x = element_line(colour = "grey80")) +
  theme(plot.caption.position = "plot", plot.caption = element_text(hjust = 0))

fig_lorenz

# Ahora generamos los intervalos de confianza
lorenz_curve_ci <- data.frame(estimate = coef(lorenz_curve), 
                              standard_error = SE(lorenz_curve), 
                              ci_lower_bound = 
                                coef(lorenz_curve)+SE(lorenz_curve)*qt(0.025,degf(subset(des_base,!is.na(ing_cor)))) ,
                              ci_upper_bound = 
                                coef(lorenz_curve) + SE(lorenz_curve)*qt(0.975,degf(subset(des_base,!is.na(ing_cor)))))

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
