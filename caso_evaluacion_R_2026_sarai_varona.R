# ============================================================
# ESTUDIO DE CASOS Y CONTROLES:
# ACTIVIDAD FISICA BAJA Y CANCER COLORRECTAL
# ============================================================

# Uso desde la terminal
# ---------------------
#   Rscript analisis_caso_control.R --help
#   Rscript analisis_caso_control.R
#   Rscript analisis_caso_control.R --input "datos/mi_archivo.csv"
#   Rscript analisis_caso_control.R -i "datos/mi_archivo.csv" --output-dir resultados
#
# Si no se indica --input, se usa la ruta relativa:
#   ANALISIS-ESTADISTICO-CON-R-DOCTORADO/caso_evaluacion_R_2026.csv
# La ruta relativa se interpreta desde el directorio de trabajo actual (getwd()).
#

# ------------------------------------------------------------
# 0. IMPORTACION Y EXPLORACION INICIAL
# ------------------------------------------------------------

# ------------------------------------------------------------
# 0.1 IMPORTACION
# ------------------------------------------------------------

rm(list = ls())
gc()

archivo <- "caso_evaluacion_R_2026.csv"

if (!file.exists(archivo)) {
  stop(
    "No se encuentra el archivo: ", archivo,
    "\nDirectorio de trabajo actual: ", getwd(),
    "\nColoca el CSV en ese directorio."
  )
}

cat("\nCargando archivo de datos")

datos <- read.csv(
  archivo,
  stringsAsFactors = FALSE,
  na.strings = c("", "NA", "N/A", "NULL"),
  check.names = FALSE
)

cat("\nArchivo de datos cargado correctamente.")

# ------------------------------------------------------------
# 0.2 EXPLORACION INICIAL
# ------------------------------------------------------------

cat("\nComprobación de dimensiones y estructura de datos.")

# Comprobar dimensiones, nombres y estructura.
cat("\nNumero de participantes y variables:\n")
print(dim(datos))

cat("\nNombres de las variables:\n")
print(names(datos))

cat("\nEstructura de los datos:\n")
str(datos)

cat("\nResumen inicial de las variables:\n")
print(summary(datos))

# ------------------------------------------------------------
# 0.3 COMPROBACION DE FRECUENCIAS Y DATOS PERDIDOS
# ------------------------------------------------------------

cat("\nComprobación de frecuencias y datos perdidos.")

# Comprobamos las categorias observadas de las variables
# cualitativas antes de resumirlas.

variables_cualitativas <- names(datos)[sapply(datos, is.character)]
variables_cuantitativas <- setdiff(names(datos)[sapply(datos, is.numeric)], "id") #quitamos id porque no tiene relevancia clinica

for (v in variables_cualitativas) {
  cat("\nFrecuencias de", v, ":")
  print(table(datos[[v]], exclude = NULL))
}

# Numero y porcentaje de datos perdidos por variable.
n_perdidos <- sapply(datos, function(x) sum(is.na(x)))
tabla_perdidos <- data.frame(
  variable = names(datos),
  n_perdidos = as.integer(n_perdidos),
  porcentaje_perdido = round(100 * n_perdidos / nrow(datos), 2),
  row.names = NULL
)

if (all(tabla_perdidos$n_perdidos == 0)) {
  cat("\nNo hay datos perdidos.")
} else {
  cat("\nDatos perdidos por variable:")
  print(tabla_perdidos, row.names = FALSE)
}

# ------------------------------------------------------------
# 0.4. PREPARACION DE LA VARIABLE DE GRUPO
# ------------------------------------------------------------

# Establecemos el orden de los grupos para que las tablas y los
# graficos aparezcan siempre primero para controles y despues
# para casos.
categorias_grupo <- unique(na.omit(datos$cancer_colorrectal))

if (!all(c("Control", "Caso") %in% categorias_grupo)) {
  stop(
    "La variable cancer_colorrectal debe contener las categorias ",
    "'Control' y 'Caso'. Categorias observadas:\n ",
    paste(categorias_grupo, collapse = ", ")
  )
}

datos$cancer_colorrectal <- factor(
  datos$cancer_colorrectal,
  levels = c("Control", "Caso")
)

cat("\nNumero de participantes por grupo:")
print(table(datos$cancer_colorrectal, exclude = NULL))

# ------------------------------------------------------------
# 1. ANALISIS DESCRIPTIVO
# ------------------------------------------------------------
cat("\nComenzamos el análisis descriptivo.")

# ------------------------------------------------------------
# 1.1. Análisis Descriptivo Según Condición caso/control
# ------------------------------------------------------------
cat("\nEjercicio 1: Realice un análisis descriptivo de la muestra según la condición de caso/control.
    Describa adecuadamente las variables cuantitativas y cualitativas e interprete los resultados.")


# ------------------------------------------------------------
# 1.1.2. DESCRIPCION DE VARIABLES CUANTITATIVAS
# ------------------------------------------------------------

cat("\nComenzamos el análisis descriptivo de las variables cuantitativas.")
cat("\nComenzamos con descipciones de las variables.")

for (v in variables_cuantitativas) {
  x <- datos[[v]]
  
  cat("\nVariable:", v, "\n")
  cat("Numero de observaciones:", sum(!is.na(x)), "\n")
  cat("Media:", mean(x, na.rm = TRUE), "\n")
  cat("Desviacion tipica:", sd(x, na.rm = TRUE), "\n")
  cat("Mediana:", median(x, na.rm = TRUE), "\n")
  cat("Cuartiles:\n")
  print(quantile(x, probs = c(0, 0.25, 0.5, 0.75, 1),
                 na.rm = TRUE))
  cat("Rango intercuartilico:", IQR(x, na.rm = TRUE), "\n")
  cat("Rango (minimo y maximo):\n")
  print(range(x, na.rm = TRUE))
}

# Resumen de las variables cuantitativas por caso/control.
# tapply aplica una funcion a cada grupo definido por la variable
# cancer_colorrectal, como en los ejemplos de las practicas.
subset(datos,select=variables_cuantitativas) %>%
  tbl_summary(
    by = cancer_colorrectal,
    type = all_continuous() ~ "continuous2",
    statistic = all_continuous() ~ c(
      "{mean} ({sd})",
      "{median}",
      "{p25}–{p75}"
    ),
    label = list(
      edad ~ "Edad (años)",
      imc ~ "Índice de masa corporal (IMC)"
    ),
    digits = all_continuous() ~ 2,
    missing = "no"
  ) %>%
  add_overall()

# ------------------------------------------------------------
# 1.1.3. GRAFICOS DE VARIABLES CUANTITATIVAS
# ------------------------------------------------------------

cat("\nSeguimos con los graficos descriptivos.")
dir.create("figuras/")
# Histogramas para visualizar forma, dispersion y posibles
# valores extremos. Se muestran por separado para cada grupo.
png(
  filename = "figuras/histograma_cuantitativas.png",
  width = 10, height = 15, units = "in", res = 150
)

# Configurar la cuadrícula DESPUÉS de abrir el dispositivo PNG
op <- par(mfrow = c(length(variables_cuantitativas), 2))

for (v in variables_cuantitativas) {
  
  x_control <- datos[[v]][datos$cancer_colorrectal == "Control"]
  x_caso <- datos[[v]][datos$cancer_colorrectal == "Caso"]
  
  hist(
    x_control,
    main = paste(v, "- Controles"),
    xlab = v,
    ylab = "Frecuencia",
    col = "darkgreen",
    border = "lightgreen"
  )
  
  hist(
    x_caso,
    main = paste(v, "- Casos"),
    xlab = v,
    ylab = "Frecuencia",
    col = "darkred",
    border = "indianred1"
  )
}

dev.off()

cat("\nGráfico guardado: figuras/histograma_cuantitativas.png")
par(op)

# Diagramas de caja: permiten comparar medianas, dispersion
# y posibles observaciones alejadas entre casos y controles.

png(
  filename = paste0("figuras/boxplot_cuantitativas.png"),
  width = 15, height = 15, units = "in", res = 150
)
op <- par(mfrow = c(length(variables_cuantitativas), 1))

for (v in variables_cuantitativas) {
  b <- boxplot(
    datos[[v]] ~ datos$cancer_colorrectal,
    main = paste(v, "segun condicion de caso/control"),
    xlab = "Grupo",
    ylab = v,
    col = c("firebrick3", "chartreuse4"),
    border = c("darkred", "darkgreen")
  )

  # Etiquetar cada outlier con su valor, justo debajo del punto
  if (length(b$out) > 0) {
    text(x = b$group, y = b$out, labels = b$out, pos = 1, cex = 0.8)
  }
}
dev.off()
cat("\nGráfico guardado: figuras/boxplot_cuantitativas.png")
par(op)


# Graficos Q-Q globales para explorar la forma de las
# distribuciones. Son herramientas descriptivas, no una prueba
# concluyente de normalidad.
# Guardar la configuracion grafica actual y poner 2 filas x 1 columna
op <- par(mfrow = c(length(variables_cuantitativas), 1))

for (v in variables_cuantitativas) {
  qqnorm(datos[[v]], main = paste("Grafico Q-Q:", v))
  qqline(datos[[v]])
}

# Restaurar la configuracion original
par(op)

# ------------------------------------------------------------
# 1.1.4. DESCRIPCION DE VARIABLES CUALITATIVAS
# ------------------------------------------------------------
cat("\nComenzamos el análisis descriptivo de las variables cualitativas.")
cat("\nComenzamos con descipciones de las variables.")

# Para cada variable cualitativa mostramos:
#   - tabla de frecuencias absolutas;
#   - porcentajes sobre el total de datos no perdidos;
#   - tabla cruzada con caso/control;
#   - porcentajes dentro de cada grupo (por columna).
#
# Como cancer_colorrectal se encuentra en las columnas, prop.table
# con margin = 2 calcula porcentajes dentro de cada grupo.

for (v in variables_cualitativas) {
  
  cat("\n========================================\n")
  cat("VARIABLE CUALITATIVA:", v, "\n")
  cat("========================================\n")
  
  cat("\nFrecuencias:\n")
  print(table(datos[[v]], exclude = NULL))
  
  cat("\nProporciones entre observaciones:\n")
  print(prop.table(table(datos[[v]])))
  
  cat("\nPorcentajes entre observaciones:\n")
  print(round(prop.table(table(datos[[v]])) * 100, 2))
  
  cat("\nTabla cruzada con caso/control:\n")
  tabla_cruzada <- table(
    datos[[v]],
    datos$cancer_colorrectal,
    exclude = NULL
  )
  print(tabla_cruzada)
  
  cat("\nPorcentajes dentro de cada grupo de caso/control:")
  print(round(prop.table(tabla_cruzada, margin = 2) * 100, 2))
}

# ------------------------------------------------------------
# 1.1.5. GRAFICOS DE VARIABLES CUALITATIVAS
# ------------------------------------------------------------
cat("\nSeguimos con los graficos descriptivos.")

# Barras agrupadas para comparar la distribucion de cada variable
# cualitativa entre controles y casos.
# Variables a comparar (se excluye la propia variable de agrupacion)
vars_comparar <- setdiff(variables_cualitativas, "cancer_colorrectal")

# Panel de 2 columnas y las filas necesarias (2 x 2 con 4 variables)
png(
  filename = paste0("figuras/barplot_cualitativas.png"),
  width = 15, height = 10, units = "in", res = 150
)
op <- par(mfrow = c(ceiling(length(vars_comparar) / 2), 2))

for (v in vars_comparar) {
  
  tabla_grafico <- table(
    datos[[v]],
    datos$cancer_colorrectal
  )
  
  barplot(
    tabla_grafico,
    beside = TRUE,
    main = paste(v),
    xlab = v,
    ylab = "Frecuencia",
    legend.text = rownames(tabla_grafico),
    args.legend = list(title = "Grupo", x = "topright"),
    col = c("grey70", "grey30")
  )
}
dev.off()
cat("\nGráfico guardado: figuras/barplot_cualitativas.png")
# Restaurar la configuracion original
par(op)

# ------------------------------------------------------------
# 1.1.6. Tablas summary
# ------------------------------------------------------------

library(gtsummary)

cat("\nVamos a ver las tablas summary de las distintas categorías.\n")
cat("\nVariables categóricas\n")
tabla_categoricas <- subset(datos,select=variables_cualitativas) %>%
  tbl_summary(by="cancer_colorrectal")%>%
  add_overall()
tabla_categoricas
gt::gtsave(
  gtsummary::as_gt(tabla_categoricas),
  filename = "tabla_categoricas.html"
)

variables_cuantitativas <- c(
  setdiff(names(datos)[sapply(datos, is.numeric)], "id"),
  "cancer_colorrectal"
)
cat("\nVariables numericas\n")
tabla_numericas <- subset(datos,select=variables_cuantitativas) %>%
  tbl_summary(by="cancer_colorrectal",statistic = all_continuous() ~ "{mean}")%>%
  add_overall()%>%
  add_ci(pattern = "{stat} ({ci})")%>%
  add_p(test = everything() ~ "t.test", test.args = all_tests("t.test") ~ list(var.equal = TRUE))

tabla_numericas
gt::gtsave(
  gtsummary::as_gt(tabla_numericas),
  filename = "tabla_numericas.html"
)
