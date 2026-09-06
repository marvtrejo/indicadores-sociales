# ==============================================================================
# Repaso de R - ENIGH 2024
# Script de trabajo y práctica
# ==============================================================================
# Breve historia del software
#==============================================================================

#R es un lenguaje y entorno de programación **libre y de código abierto**,
#desarrollado en 1993 por Robert Gentleman y Ross Ihaka, del Departamento de
#Estadística de la Universidad de Auckland (Nueva Zelanda). Nació como una
#reinterpretación del lenguaje S (creado en los Laboratorios Bell), pensada
#para hacer más accesible el análisis estadístico.

# Hoy en día R es ampliamente usado en economía, ciencias sociales,
# biología, medicina, ingeniería y ciencia de datos en general, tanto en el
# sector académico como en instituciones públicas (el propio INEGI usa R en
# varios de sus procesos) y privadas.

#==============================================================================
# Ventajas de su utilización
#==============================================================================

# Es **software libre**: se puede usar, modificar y distribuir sin costo.
# Cuenta con una **comunidad enorme** y en constante crecimiento, lo que se
# traduce en mucha documentación, foros, cursos y paquetes especializados.
# Tiene paquetes diseñados específicamente para **encuestas complejas**
# (como la ENIGH), lo que lo hace ideal para trabajar con factores de
# expansión, estratos y conglomerados.
# Excelente para **visualización de datos** (ggplot2) y para la
# automatización de reportes (R Markdown, como este mismo documento).
# Es un estándar de facto en la docencia de estadística y econometría.

#==============================================================================
# Detalles y reglas importantes
#==============================================================================

# R es **"case sensitive"**: distingue entre mayúsculas y minúsculas
# (`Ingreso` y `ingreso` son objetos distintos).
# Para ejecutar una línea de código se usa `Ctrl + Enter`, o el botón
# "Run" de la esquina superior derecha del script.
# El símbolo `#` convierte el texto que le sigue en un comentario
# (atajo: `Ctrl + Shift + C` para comentar/descomentar varias líneas).
# Cada instrucción normalmente ocupa una línea; no es obligatorio usar
# `;` al final (a diferencia de otros lenguajes).
# Los nombres de objetos no pueden empezar con un número ni contener
# espacios; se recomienda usar `_` o `.` como separador (ej. `ingreso_hogar`).

#==============================================================================
# Introducción: bases del lenguaje
#==============================================================================

#==============================================================================
# Operadores básicos
#==============================================================================

# **Aritméticos**: `+`, `-`, `*`, `/`, `^` (potencia)
# **De asignación**: `<-`, `=` (equivalentes casi siempre; se recomienda `<-`)
# **De comparación**: `<`, `<=`, `>`, `>=`, `==` (igual a), `!=` (diferente de)
# **Lógicos**: `&` (y), `|` (o), `!` (negación), `TRUE`/`FALSE`

# Operadores aritméticos
2 + 2      # Suma
5 - 2      # Resta
2 * 5      # Multiplicación
10 / 5     # División
2 ^ 3      # Potencia

# Operadores de comparación (regresan TRUE o FALSE)
1 < 5
1 > 5
1 == 1     # ¿Es 1 igual a 1?
1 != 5     # ¿Es 1 diferente de 5?

# Operadores lógicos
(1 < 5) & (2 < 5)   # TRUE y TRUE -> TRUE
(1 < 5) | (2 > 5)   # TRUE u FALSE -> TRUE
!(1 == 1)           # Negación de TRUE -> FALSE

#==============================================================================
# Operaciones con objetos guardados
#==============================================================================

# R es un lenguaje **orientado a objetos**: todo lo que creamos (números,
# texto, bases de datos, resultados de modelos) se guarda en un objeto con
# un nombre, y podemos operar sobre esos objetos.

# Asignamos valores a objetos con "<-" (atajo: Alt + "-")
a <- 3.1416     # Numérico (constante pi aproximada)
e <- 1.6180     # Numérico (número áureo aproximado)

a   # Al escribir el nombre del objeto, R muestra su contenido
e

# Podemos operar directamente con los objetos ya creados
b <- a + e
b

# Un vector es la estructura más sencilla para concatenar varios datos
# del mismo tipo. Se construye con la función c() ("combine")
edades <- c(25, 34, 41, 19, 60)
edades

# Los "strings" o "character" son cadenas de texto
carrera <- "Economía"
carrera

# Vector de texto (varios strings concatenados)
nombres <- c("Ana", "Luis", "Marta", "Carlos")
nombres

# class() nos indica de qué tipo es un objeto
class(a)
class(carrera)
class(nombres)
class(TRUE)

#==============================================================================
# Tipos de objetos en R
#==============================================================================

# Es importante distinguir entre **tipos de dato** (lo que contiene el
# objeto) y **estructuras** (cómo se organiza esa información):

# | Tipo / estructura | Descripción                                            | Ejemplo                         |
# |--------------------|--------------------------------------------------------|----------------------------------|
# | `numeric`          | Números (enteros o decimales)                          | `3.1416`                         |
# | `character`        | Texto ("strings")                                      | `"Hidalgo"`                      |
# | `logical`          | Valores lógicos                                        | `TRUE`, `FALSE`                  |
# | `factor`           | Variable categórica con niveles (útil para var. cualitativas) | `factor(c("Bajo","Alto"))`|
# | `vector`           | Colección de datos del **mismo** tipo                  | `c(1,2,3)`                       |
# | `list`             | Colección de objetos de **distinto** tipo o tamaño      | `list(a = 1, b = "texto")`       |
# | `data.frame`/`tibble` | Tabla rectangular: columnas = variables, filas = observaciones | Una base de datos como la ENIGH |

# Ejemplos rápidos de cada estructura
vec_num   <- c(1, 2, 3, 4, 5)              # vector numérico
vec_chr   <- c("Hidalgo", "Nuevo Leon")     # vector de texto
vec_log   <- c(TRUE, FALSE, TRUE)           # vector lógico
mi_factor <- factor(c("Bajo", "Alto", "Bajo"), levels = c("Bajo","Alto"))
mi_lista  <- list(nombre = "ENIGH", anio = 2024, activo = TRUE)

class(vec_num)
class(mi_factor)
class(mi_lista)
str(mi_lista)   # str() describe la estructura interna de un objeto

#==============================================================================
# Paqueterías de R
#==============================================================================

#==============================================================================
# ¿Qué son las paqueterías?
#==============================================================================

# Las **paqueterías** (o "packages") son conjuntos de funciones, datos y
# documentación creados por la comunidad para ampliar las capacidades base
# de R. En lugar de programar todo desde cero, podemos instalar y cargar
# paquetes ya construidos para tareas específicas (manipular datos, hacer
# gráficos, trabajar con encuestas complejas, etc.).

#==============================================================================
# Instalación y carga de paqueterías
#==============================================================================

# La instalación se hace **una sola vez** por computadora con
# `install.packages()`. La carga (activación) se debe hacer **cada vez**
# que abrimos una nueva sesión de R con `library()`.

# NOTA: quitar el "#" de las siguientes líneas la primera vez que se use
# este script, para instalar los paquetes necesarios.

# install.packages("tidyverse")   # incluye dplyr, ggplot2, stringr, etc.
# install.packages("dplyr")
# install.packages("ggplot2")
# install.packages("foreign")     # importar .dbf, .csv de otros softwares
# install.packages("readxl")      # importar archivos de Excel
# install.packages("haven")       # importar archivos de Stata (.dta), SPSS
# install.packages("psych")       # estadística descriptiva (describe())
# install.packages("survey")      # diseño de muestras complejas
# install.packages("srvyr")       # sintaxis "tidy" para el paquete survey
# install.packages("flextable")   # tablas con formato para reportes
# install.packages("janitor")     # limpieza de nombres de variables

# CUANDO INICIEN UNA NUEVA SESIÓN, SIEMPRE CARGAR LOS PAQUETES QUE VAYAN A USAR
library(tidyverse)  # incluye dplyr (manipulación) y ggplot2 (gráficos)
library(dplyr)
library(ggplot2)
library(foreign)    # leer .dbf
library(readxl)     # leer .xlsx
library(haven)      # leer .dta (Stata)
library(psych)      # función describe()
library(survey)     # diseño muestral complejo, svydesign(), svymean(), cv()
library(srvyr)      # sintaxis tidy para "survey"
library(flextable)  # tablas para reportes
library(janitor)    # limpieza de nombres de columnas
library(stringr)    # manipulación de texto (str_pad, str_sub, etc.)

#==============================================================================
# Cómo abrir y cargar una base de datos en R
#==============================================================================

# Antes de leer cualquier archivo, es necesario decirle a R en qué carpeta
# de nuestra computadora se encuentran los datos. Esto se hace con
# `setwd()` ("set working directory").

# Definimos nuestro directorio de trabajo (AJUSTAR A LA RUTA PROPIA)
setwd("C:/Users/usuario/Documents/ENIGH2024")

# Dependiendo del formato en que descarguemos la base, usamos una función
# distinta para leerla:

# CSV (formato de texto separado por comas)
datos_csv  <- read.csv("concentradohogar.csv")

# DBF (formato dBase, usado por algunos programas antiguos)
datos_dbf  <- read.dbf("concentradohogar.dbf")

# DTA (formato nativo de Stata)
datos_dta  <- read_dta("concentradohogar.dta")

# XLSX (Excel)
datos_xlsx <- read_excel("concentradohogar.xlsx")

# **Nota:** las bases de la ENIGH se pueden descargar directamente del
# sitio del INEGI en distintos formatos (csv, dbf, dta). El contenido es el
# mismo; solo cambia el programa que originalmente las generó.

#==============================================================================
# Ejemplo de uso con la ENIGH 2024 (nivel nacional)
#==============================================================================

# Vamos a trabajar con la tabla **`CONCENTRADOHOGAR`** de la Encuesta
# Nacional de Ingresos y Gastos de los Hogares (ENIGH) 2024, Nueva Serie.
# Esta tabla contiene, para cada hogar de la muestra, variables ya
# "concentradas" (sumarizadas) de ingreso, gasto y características
# sociodemográficas del hogar y de su jefe(a).

# Algunas variables clave que usaremos (tomadas del documento
# *"Descripción de la base de datos"* de la ENIGH 2024):

# | Variable      | Descripción                                             |
# |---------------|----------------------------------------------------------|
# | `folioviv`    | Identificador de la vivienda (incluye clave de entidad)  |
# | `foliohog`    | Identificador del hogar                                  |
# | `tam_loc`     | Tamaño de localidad: 1 y 2 = grandes urbes; 3 = urbano medio; 4 = rural (< 2 500 hab.) |
# | `est_socio`   | Estrato socioeconómico: 1 = Bajo, 2 = Medio bajo, 3 = Medio alto, 4 = Alto |
# | `est_dis`     | Estrato de diseño muestral                                |
# | `upm`         | Unidad primaria de muestreo                               |
# | `factor`      | Factor de expansión (número de hogares que representa cada hogar de la muestra) |
# | `sexo_jefe`   | Sexo del jefe(a) del hogar: 1 = Hombre, 2 = Mujer          |
# | `edad_jefe`   | Edad del jefe(a) del hogar                                 |
# | `educa_jefe`  | Escolaridad del jefe(a) del hogar (11 categorías, de "sin instrucción" a "posgrado") |
# | `tot_integ`   | Número de integrantes del hogar                            |
# | `menores`     | Integrantes del hogar de 11 años o menos (**ver nota abajo**) |
# | `p65mas`      | Integrantes del hogar de 65 años o más                      |
# | `ing_cor`     | Ingreso corriente total del hogar (trimestral)              |
# | `ingtrab`     | Ingreso por trabajo                                         |
# | `bene_gob`    | Beneficios gubernamentales recibidos por el hogar            |

# **Nota importante sobre "menores de 18 años":** en la tabla
# `CONCENTRADOHOGAR` la variable `menores` está definida oficialmente
# como integrantes del hogar de **11 años o menos**, no de menos de 18.
# La ENIGH no incluye en esta tabla un conteo directo de personas de
# 12 a 17 años (ese rango queda mezclado dentro de `p12_64`, junto con
# adultos hasta los 64 años). Por lo tanto, para identificar "menores de
# 18 años" de forma exacta sería necesario cruzar esta tabla con la
# tabla `POBLACION` (a nivel de individuo) y contar ahí a las personas
# con edad < 18. En este ejercicio usaremos `menores` (< 12 años) como
# la mejor aproximación disponible dentro de `CONCENTRADOHOGAR`, dejando
# comentado el código para hacerlo de forma exacta si se cuenta con la
# tabla `POBLACION`.

# Cargamos la base de concentrado de hogar de la ENIGH 2024
# (ajustar el nombre/ruta del archivo según el formato descargado)

concentrado <- read_dta("concentradohogar.dta")
# concentrado <- read.csv("concentradohogar.csv")

# Observamos la base cargada en una ventana (similar a "Browse" de Stata)
View(concentrado)

# Exploración inicial de la base
dim(concentrado)        # número de filas (hogares) y columnas (variables)
colnames(concentrado)   # nombres de todas las variables
str(concentrado)        # estructura y clase de cada variable
head(concentrado)       # primeras 6 filas
colSums(is.na(concentrado)) # número de valores faltantes (NA) por variable

# Se puede acceder a una variable específica con el operador "$"
concentrado$ing_cor
concentrado$sexo_jefe

#==============================================================================
# Manipulación de la base de datos con `dplyr`
#==============================================================================

# `dplyr` es la paquetería más usada para manipular data frames en R. Se
# basa principalmente en 5 "verbos":

# `select()`   -> elegir columnas (variables)
# `mutate()`   -> crear nuevas variables a partir de las existentes
# `filter()`   -> elegir filas (observaciones) que cumplan una condición
# `arrange()`  -> ordenar filas
# `summarise()`-> calcular estadísticos resumen

# Con `dplyr` ya no es necesario usar el operador `$`: basta con indicar el
# nombre de la base como primer argumento (o usar el operador "pipe" `%>%`,
# atajo `Ctrl + Shift + M`, que se lee como "y luego...").

#==============================================================================
# Selección de variables
#==============================================================================

colnames(concentrado)

# Seleccionamos únicamente las variables que usaremos en el ejercicio
muestra_concen <- select(concentrado, folioviv, foliohog, ubica_geo, tam_loc,
                         est_socio, est_dis, upm, factor, sexo_jefe, edad_jefe,
                         educa_jefe, tot_integ, menores, p65mas, ing_cor,
                         ingtrab, bene_gob)

#==============================================================================
# Creación de nuevas variables (`mutate`)
#==============================================================================

# Creamos la entidad federativa a partir de los dos primeros dígitos de
# `folioviv`, una etiqueta de ámbito rural/urbano a partir de `tam_loc`, y
# el **ingreso corriente per cápita del hogar** (una de las estadísticas
# solicitadas más adelante).

muestra_concen <- muestra_concen %>%
  mutate(
    # folioviv siempre viene como texto de 10 caracteres; nos aseguramos
    # de que tenga ceros a la izquierda si hiciera falta
    folioviv = str_pad(folioviv, 10, "left", pad = "0"),

    # Los dos primeros dígitos de folioviv son la clave de la entidad
    ent = as.numeric(str_sub(folioviv, 1, 2)),

    entidad = case_when(
      ent == 1  ~ "Aguascalientes",      ent == 2  ~ "Baja California",
      ent == 3  ~ "Baja California Sur", ent == 4  ~ "Campeche",
      ent == 5  ~ "Coahuila",            ent == 6  ~ "Colima",
      ent == 7  ~ "Chiapas",             ent == 8  ~ "Chihuahua",
      ent == 9  ~ "Ciudad de Mexico",    ent == 10 ~ "Durango",
      ent == 11 ~ "Guanajuato",          ent == 12 ~ "Guerrero",
      ent == 13 ~ "Hidalgo",             ent == 14 ~ "Jalisco",
      ent == 15 ~ "Mexico",              ent == 16 ~ "Michoacan",
      ent == 17 ~ "Morelos",             ent == 18 ~ "Nayarit",
      ent == 19 ~ "Nuevo Leon",          ent == 20 ~ "Oaxaca",
      ent == 21 ~ "Puebla",              ent == 22 ~ "Queretaro",
      ent == 23 ~ "Quintana Roo",        ent == 24 ~ "San Luis Potosi",
      ent == 25 ~ "Sinaloa",             ent == 26 ~ "Sonora",
      ent == 27 ~ "Tabasco",             ent == 28 ~ "Tamaulipas",
      ent == 29 ~ "Tlaxcala",            ent == 30 ~ "Veracruz",
      ent == 31 ~ "Yucatan",             ent == 32 ~ "Zacatecas"
    ),

    # tam_loc puede llegar como texto o numérico según el formato leído;
    # as.numeric() lo estandariza. tam_loc == 4 es la categoría oficial
    # de localidades rurales (menos de 2,500 habitantes)
    tam_loc   = as.numeric(tam_loc),
    ambito    = if_else(tam_loc == 4, "Rural", "Urbano"),

    sexo_jefe = as.numeric(sexo_jefe),
    sexo_jefe_lab = if_else(sexo_jefe == 2, "Mujer", "Hombre"),

    est_socio = as.numeric(est_socio),
    estrato_lab = case_when(
      est_socio == 1 ~ "Bajo",
      est_socio == 2 ~ "Medio bajo",
      est_socio == 3 ~ "Medio alto",
      est_socio == 4 ~ "Alto"
    ),

    # Ingreso corriente PER CÁPITA del hogar: ingreso corriente total del
    # hogar entre el número de integrantes. Es una medida más comparable
    # entre hogares de distinto tamaño que el ingreso corriente total.
    ictpc = ing_cor / tot_integ
  )

#==============================================================================
# Creación de subgrupos (`filter`)
#==============================================================================

# Construimos el grupo solicitado: **jefas de familia mujeres, en hogares
# con al menos un adulto mayor (65+) y al menos un menor (según la
# aproximación de `menores` explicada arriba), en el ámbito rural**, para
# Hidalgo y para Nuevo León.

# Grupo: jefas mujeres + hogar con adulto(s) mayor(es) + hogar con
# menor(es) + ámbito rural + Hidalgo
hidalgo_mujeres_am_menores <- muestra_concen %>%
  filter(entidad == "Hidalgo",
         sexo_jefe == 2,      # jefa mujer
         ambito == "Rural",
         p65mas > 0,          # al menos un adulto mayor en el hogar
         menores > 0)         # al menos un menor (< 12 años, ver nota)

# El mismo criterio, pero para Nuevo León
nl_mujeres_am_menores <- muestra_concen %>%
  filter(entidad == "Nuevo Leon",
         sexo_jefe == 2,
         ambito == "Rural",
         p65mas > 0,
         menores > 0)

# Revisamos cuántos hogares de la MUESTRA (no expandidos) cumplen la
# condición en cada estado -- ojo, esto es solo el tamaño muestral, no
# el número de hogares que representan en la población (eso se calcula
# más adelante con el factor de expansión)
nrow(hidalgo_mujeres_am_menores)
nrow(nl_mujeres_am_menores)

# -----------------------------------------------------------------------
# EXTENSIÓN OPCIONAL (requiere la tabla POBLACION):
# Para obtener el conteo EXACTO de menores de 18 años por hogar habría
# que hacer algo como:
#
# menores18 <- poblacion %>%
#   filter(edad < 18) %>%
#   group_by(folioviv, foliohog) %>%
#   summarise(n_menores18 = n(), .groups = "drop")
#
# muestra_concen <- muestra_concen %>%
#   left_join(menores18, by = c("folioviv", "foliohog")) %>%
#   mutate(n_menores18 = replace_na(n_menores18, 0))
#
# Y luego filtrar con n_menores18 > 0 en lugar de menores > 0.
# -----------------------------------------------------------------------

#==============================================================================
# Ordenar (`arrange`) y resumir (`summarise`) — recordatorio rápido
#==============================================================================

# arrange ordena las filas (desc() invierte el orden a "de mayor a menor")
muestra_concen %>% arrange(desc(ing_cor)) %>% select(entidad, ing_cor) %>% head()

# summarise crea un resumen (no modifica la base original)
muestra_concen %>%
  summarise(prom_ing = mean(ing_cor, na.rm = TRUE),
            max_ing  = max(ing_cor, na.rm = TRUE))

#==============================================================================
# Diseño muestral complejo: total de hogares, ingreso promedio, error estándar y coeficiente de variación
#==============================================================================

# La ENIGH **no** es una muestra simple: tiene estratos (`est_dis`),
# conglomerados o unidades primarias de muestreo (`upm`) y un factor de
# expansión (`factor`). Para que las estimaciones (promedios, totales,
# errores estándar) sean correctas, es indispensable declarar el diseño
# muestral con `svydesign()` **antes** de calcular cualquier estadístico.

# **Importante:** para obtener estimaciones de subgrupos (por ejemplo,
# "mujeres jefas de hogar rural en Hidalgo con adultos mayores y
# menores") lo correcto es declarar el diseño muestral con **toda** la
# base y después usar `subset()` sobre el objeto de diseño, en lugar de
# filtrar primero el data frame y crear un `svydesign` nuevo con menos
# datos. Esto es porque `subset()` conserva la información completa de
# estratos y conglomerados necesaria para estimar correctamente la
# varianza, mientras que recalcular el diseño sobre una base ya filtrada
# puede sub/sobre-estimar el error estándar.

# Declaramos el diseño muestral con la base COMPLETA a nivel nacional
concen_design <- svydesign(ids = ~upm, strata = ~est_dis, weights = ~factor,
                           data = muestra_concen, nest = TRUE)

# Subconjuntos del diseño (conservando la estructura de varianza)
design_hidalgo_grupo <- subset(concen_design,
                               entidad == "Hidalgo" & sexo_jefe == 2 &
                                 ambito == "Rural" & p65mas > 0 & menores > 0)

design_nl_grupo <- subset(concen_design,
                          entidad == "Nuevo Leon" & sexo_jefe == 2 &
                            ambito == "Rural" & p65mas > 0 & menores > 0)

#==============================================================================
# Función para construir el resumen (total de hogares, media, error estándar, CV)
#==============================================================================

# Función auxiliar: dado un objeto de diseño de encuesta (survey design)
# y el nombre de un grupo, regresa una fila con:
#   - total de hogares que representa el grupo en la población
#   - ingreso corriente promedio
#   - error estándar del promedio
#   - coeficiente de variación (%)
resumen_ingreso <- function(design, nombre_grupo) {

  total_hogares <- svytotal(~1, design)                 # total de hogares (expandido)
  media_ing     <- svymean(~ing_cor, design)             # promedio e info del EE

  data.frame(
    grupo         = nombre_grupo,
    total_hogares = as.numeric(total_hogares[1]),
    ing_prom      = as.numeric(coef(media_ing)),
    error_est     = as.numeric(SE(media_ing)),
    cv_pct        = as.numeric(cv(media_ing)) * 100   # cv() regresa proporción; *100 = %
  )
}

tabla_resumen <- bind_rows(
  resumen_ingreso(concen_design,        "Nacional"),
  resumen_ingreso(design_hidalgo_grupo, "Hidalgo - jefas, rural, AM y menores"),
  resumen_ingreso(design_nl_grupo,      "Nuevo Leon - jefas, rural, AM y menores")
)

tabla_resumen

#==============================================================================
# Comparación contra el criterio de calidad del INEGI (coeficiente de variación)
#==============================================================================

# De acuerdo con la ficha *"Coeficiente de variación"* del Comité de
# Aseguramiento de la Calidad del INEGI, para **proyectos en viviendas**
# (como la ENIGH) los umbrales de referencia son:

# | Rango del CV      | Calificación   | Interpretación                                                |
# |--------------------|----------------|------------------------------------------------------------------|
# | 0% – 15%           | Buena          | Alto grado de confiabilidad                                       |
# | > 15% – 25%        | Aceptable      | Confiabilidad tolerable                                            |
# | > 25%              | Con reserva    | Baja confiabilidad; usar solo con fines descriptivos o de tendencia |

# Función para clasificar el CV según el criterio INEGI para viviendas/hogares
clasifica_cv <- function(cv_pct) {
  case_when(
    cv_pct <= 15            ~ "Buena",
    cv_pct > 15 & cv_pct <= 25 ~ "Aceptable",
    cv_pct > 25              ~ "Con reserva"
  )
}

tabla_resumen <- tabla_resumen %>%
  mutate(across(c(total_hogares, ing_prom, error_est), ~round(.x, 1)),
         cv_pct = round(cv_pct, 2),
         calidad_INEGI = clasifica_cv(cv_pct))

tabla_resumen

# Nota esperada: al ser subgrupos MUY específicos (cruce de estado, sexo
# del jefe, ámbito rural y presencia simultánea de adultos mayores y
# menores), el tamaño de muestra efectivo es pequeño, por lo que es común
# que el CV de estos subgrupos sea sustancialmente más alto (peor
# calidad) que el CV de la estimación nacional. Esto ilustra de forma
# muy clara para qué sirve el coeficiente de variación: advertirnos
# cuándo una estimación, aunque calculable, debe interpretarse con
# cautela por su bajo tamaño de muestra.

#==============================================================================
# Estadísticas descriptivas y gráficos
#==============================================================================

# Ahora exploramos, a nivel nacional, cinco variables clave: el **ingreso
# corriente del hogar**, el **ingreso corriente per cápita**
# (`ictpc`, creada arriba con `mutate`), el **grado de escolaridad del
# jefe(a) del hogar**, los **beneficios gubernamentales** recibidos y el
# **estrato socioeconómico**.

#==============================================================================
# Estadística descriptiva
#==============================================================================

# Estadísticos clásicos para las variables numéricas
muestra_concen %>%
  summarise(
    media_ing_cor = mean(ing_cor, na.rm = TRUE),
    mediana_ing_cor = median(ing_cor, na.rm = TRUE),
    de_ing_cor = sd(ing_cor, na.rm = TRUE),
    media_ictpc = mean(ictpc, na.rm = TRUE),
    mediana_ictpc = median(ictpc, na.rm = TRUE),
    de_ictpc = sd(ictpc, na.rm = TRUE),
    media_bene_gob = mean(bene_gob, na.rm = TRUE),
    pct_con_bene_gob = mean(bene_gob > 0, na.rm = TRUE) * 100
  )

# describe() de la paquetería psych regresa un resumen más completo
# (n, media, de, mediana, min, max, rango, asimetría, curtosis, error estándar)
describe(muestra_concen$ing_cor)
describe(muestra_concen$ictpc)

# Para variables categóricas usamos tablas de frecuencia
table(muestra_concen$educa_jefe)
table(muestra_concen$estrato_lab)

# Combinando dplyr y janitor podemos obtener frecuencias y porcentajes
muestra_concen %>% tabyl(estrato_lab)
muestra_concen %>% tabyl(educa_jefe)

#==============================================================================
# Gráficos
#==============================================================================

# Histograma del ingreso corriente del hogar
ggplot(muestra_concen, aes(x = ing_cor)) +
  geom_histogram(bins = 40, fill = "steelblue", color = "white") +
  labs(title = "Distribución del ingreso corriente del hogar. ENIGH 2024",
       x = "Ingreso corriente trimestral ($)", y = "Número de hogares")

# Histograma del ingreso per cápita del hogar
ggplot(muestra_concen, aes(x = ictpc)) +
  geom_histogram(bins = 40, fill = "darkorange", color = "white") +
  labs(title = "Distribución del ingreso corriente per cápita del hogar. ENIGH 2024",
       x = "Ingreso corriente per cápita trimestral ($)", y = "Número de hogares")

# Barras: escolaridad del jefe(a) del hogar
ggplot(muestra_concen, aes(x = educa_jefe)) +
  geom_bar(fill = "seagreen") +
  labs(title = "Escolaridad del jefe(a) del hogar. ENIGH 2024",
       x = "Nivel de escolaridad (código educa_jefe)", y = "Número de hogares")

# Barras: estrato socioeconómico
ggplot(muestra_concen, aes(x = estrato_lab)) +
  geom_bar(fill = "purple") +
  labs(title = "Distribución de hogares por estrato socioeconómico. ENIGH 2024",
       x = "Estrato socioeconómico", y = "Número de hogares")

# Boxplot: beneficios gubernamentales por estrato socioeconómico
ggplot(muestra_concen, aes(x = estrato_lab, y = bene_gob)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Beneficios gubernamentales por estrato socioeconómico. ENIGH 2024",
       x = "Estrato socioeconómico", y = "Beneficios gubernamentales ($ trimestrales)")

#==============================================================================
# Ejercicio propuesto y preguntas de investigación
#==============================================================================

# Con estas cinco variables (`ing_cor`, `ictpc`, `educa_jefe`, `bene_gob`,
# `estrato_lab`) se puede construir un ejercicio muy rico de análisis
# socioeconómico. Algunas preguntas de investigación interesantes:

# 1. **¿La escolaridad del jefe(a) del hogar se relaciona con el estrato
# socioeconómico del hogar?** (tabla cruzada `educa_jefe` x
# `estrato_lab`, o un boxplot de `ictpc` según nivel de escolaridad).
# 2. **¿Los beneficios gubernamentales están más concentrados en los
# estratos bajos?** Es decir, ¿`bene_gob` funciona como un mecanismo
# redistributivo? (comparar el promedio de `bene_gob` y qué proporción
# representa del `ing_cor` en cada estrato).
# 3. **¿El ingreso per cápita reduce o amplía las diferencias entre
# hogares en comparación con el ingreso corriente total?** (comparar la
# dispersión relativa —coeficiente de variación muestral, no el de
# diseño— de `ing_cor` frente a `ictpc`).
# 4. **¿Existe una relación entre escolaridad del jefe(a) y la
# probabilidad de recibir beneficios gubernamentales?** (por ejemplo,
# `mean(bene_gob > 0)` por nivel de `educa_jefe`).
# 5. **¿El "premio" de la escolaridad sobre el ingreso per cápita es
# distinto según el estrato socioeconómico?** (interacción entre
# `educa_jefe` y `estrato_lab` sobre `ictpc`).

# Un ejercicio concreto para practicar: construir con `dplyr` una tabla
# que muestre, para cada combinación de `estrato_lab` y nivel de
# escolaridad agrupado (por ejemplo "Sin instrucción/Primaria",
# "Secundaria/Preparatoria", "Profesional/Posgrado"), el ingreso per
# cápita promedio, el porcentaje de hogares que reciben beneficios
# gubernamentales y el beneficio gubernamental promedio; y después
# representar esos resultados con un `geom_boxplot()` o un `geom_col()`
# por grupo.

muestra_concen <- muestra_concen %>%
  mutate(educa_agrupada = case_when(
    educa_jefe %in% c("01", "02", "03", "04") ~ "Sin instr./Primaria",
    educa_jefe %in% c("05", "06", "07", "08") ~ "Secundaria/Preparatoria",
    educa_jefe %in% c("09", "10", "11")       ~ "Profesional/Posgrado"
  ))

tabla_cruce <- muestra_concen %>%
  group_by(estrato_lab, educa_agrupada) %>%
  summarise(
    ictpc_prom      = mean(ictpc, na.rm = TRUE),
    pct_con_bene    = mean(bene_gob > 0, na.rm = TRUE) * 100,
    bene_gob_prom   = mean(bene_gob, na.rm = TRUE),
    .groups = "drop"
  )

tabla_cruce

ggplot(tabla_cruce, aes(x = estrato_lab, y = ictpc_prom, fill = educa_agrupada)) +
  geom_col(position = "dodge") +
  labs(title = "Ingreso per cápita promedio por estrato y escolaridad del jefe(a). ENIGH 2024",
       x = "Estrato socioeconómico", y = "Ingreso per cápita promedio ($)",
       fill = "Escolaridad del jefe(a)")

#==============================================================================
# Estadísticas del ingreso corriente del hogar por entidad federativa, sexo del jefe(a) y estrato socioeconómico
#==============================================================================

# Cerramos el ejercicio replicando, con el diseño muestral (`svydesign`),
# el cálculo de ingreso corriente promedio, error estándar y coeficiente
# de variación **por grupos**, y comparándolos visualmente con
# `geom_boxplot()`.

#==============================================================================
# Tablas resumen con `svyby`
#==============================================================================

# --- Por entidad federativa ---
ing_entidad <- svyby(~ing_cor, ~entidad, design = concen_design, svymean)
cv_entidad  <- cv(svyby(~ing_cor, ~entidad, design = concen_design, svymean))

tabla_entidad <- as.data.frame(ing_entidad) %>%
  mutate(cv_pct = as.numeric(cv_entidad) * 100,
         calidad_INEGI = clasifica_cv(cv_pct)) %>%
  rename(error_est = se) %>%
  arrange(desc(ing_cor))

tabla_entidad

# --- Por sexo del jefe(a) del hogar ---
ing_sexo <- svyby(~ing_cor, ~sexo_jefe_lab, design = concen_design, svymean)
cv_sexo  <- cv(svyby(~ing_cor, ~sexo_jefe_lab, design = concen_design, svymean))

tabla_sexo <- as.data.frame(ing_sexo) %>%
  mutate(cv_pct = as.numeric(cv_sexo) * 100,
         calidad_INEGI = clasifica_cv(cv_pct)) %>%
  rename(error_est = se)

tabla_sexo

# --- Por estrato socioeconómico ---
ing_estrato <- svyby(~ing_cor, ~estrato_lab, design = concen_design, svymean)
cv_estrato  <- cv(svyby(~ing_cor, ~estrato_lab, design = concen_design, svymean))

tabla_estrato <- as.data.frame(ing_estrato) %>%
  mutate(cv_pct = as.numeric(cv_estrato) * 100,
         calidad_INEGI = clasifica_cv(cv_pct)) %>%
  rename(error_est = se)

tabla_estrato

#==============================================================================
# Tabla con formato usando `flextable` (semaforización INEGI)
#==============================================================================

# **Nota sobre PDF:** `flextable` sí puede generar tablas dentro de un
# PDF vía `xelatex`, pero requiere que la paquetería esté actualizada y
# que LaTeX tenga instalados algunos paquetes adicionales (`booktabs`,
# `colortbl`, etc., que `tinytex::install_tinytex()` suele resolver
# automáticamente al primer intento de knitting). Si al compilar el PDF
# `flextable` da error, una alternativa simple y muy compatible con PDF
# es usar `knitr::kable()` + `kableExtra`:
# 
# ```r
# library(kableExtra)
# tabla_entidad %>%
# kbl(booktabs = TRUE, digits = c(NA, 0, 0, 2, NA),
# caption = "Ingreso corriente promedio por entidad federativa. ENIGH 2024") %>%
# kable_styling(latex_options = c("hold_position", "striped")) %>%
# row_spec(which(tabla_entidad$cv_pct > 25), background = "orange") %>%
# row_spec(which(tabla_entidad$cv_pct > 15 & tabla_entidad$cv_pct <= 25), background = "yellow")
# ```

tabla_entidad_ft <- flextable(tabla_entidad) %>%
  theme_vanilla() %>%
  set_header_labels(entidad = "Entidad federativa", ing_cor = "Ingreso corriente promedio",
                    error_est = "Error estándar", cv_pct = "CV (%)",
                    calidad_INEGI = "Calidad (norma INEGI)") %>%
  set_caption(caption = "Ingreso corriente promedio por entidad federativa. ENIGH 2024") %>%
  add_footer_lines("Fuente: elaboración propia con datos de la ENIGH 2024, INEGI.") %>%
  colformat_double(j = c("ing_cor", "error_est"), digits = 0) %>%
  colformat_double(j = "cv_pct", digits = 2) %>%
  bg(j = "cv_pct", i = ~ cv_pct > 15 & cv_pct <= 25, part = "body", bg = "yellow") %>%
  bg(j = "cv_pct", i = ~ cv_pct > 25, part = "body", bg = "orange") %>%
  autofit()

tabla_entidad_ft

#==============================================================================
# Diferencias observadas con `geom_boxplot()`
#==============================================================================

# Ingreso corriente por entidad federativa (ordenado de mayor a menor mediana)
ggplot(muestra_concen,
       aes(x = reorder(entidad, ing_cor, FUN = median), y = ing_cor)) +
  geom_boxplot(outlier.alpha = 0.2, fill = "steelblue") +
  coord_flip() +
  labs(title = "Ingreso corriente del hogar por entidad federativa. ENIGH 2024",
       x = "Entidad federativa", y = "Ingreso corriente trimestral ($)")

# Ingreso corriente por sexo del jefe(a) del hogar
ggplot(muestra_concen, aes(x = sexo_jefe_lab, y = ing_cor, fill = sexo_jefe_lab)) +
  geom_boxplot() +
  labs(title = "Ingreso corriente del hogar según sexo del jefe(a) del hogar. ENIGH 2024",
       x = "Sexo del jefe(a) del hogar", y = "Ingreso corriente trimestral ($)",
       fill = "Sexo del jefe(a)")

# Ingreso corriente por estrato socioeconómico
ggplot(muestra_concen, aes(x = estrato_lab, y = ing_cor, fill = estrato_lab)) +
  geom_boxplot() +
  labs(title = "Ingreso corriente del hogar por estrato socioeconómico. ENIGH 2024",
       x = "Estrato socioeconómico", y = "Ingreso corriente trimestral ($)",
       fill = "Estrato socioeconómico")

#==============================================================================
# Interpretación esperada
#==============================================================================

# Es de esperarse que los **hogares con jefatura de hombres** muestren,
# en promedio, un ingreso corriente mayor que los hogares con jefatura
# de mujeres, aunque la magnitud y significancia de esa diferencia debe
# evaluarse con el error estándar y no solo con el promedio puntual.
# El **estrato socioeconómico** debería mostrar una relación
# prácticamente monótona con el ingreso corriente (a mayor estrato,
# mayor ingreso), ya que el propio estrato se construyó, entre otras
# cosas, a partir de características socioeconómicas del hogar.
# Las diferencias entre **entidades federativas** normalmente reflejan
# patrones conocidos de desigualdad regional en México (por ejemplo,
# entidades del centro-norte y la Ciudad de México con ingresos
# promedio más altos que entidades del sur-sureste), aunque siempre hay
# que revisar el coeficiente de variación antes de sacar conclusiones
# fuertes, sobre todo en entidades con tamaños de muestra más chicos.
