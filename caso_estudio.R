euramic = read.csv("regression/data/euramic.csv")
library(splines)

## Convertimos la columnas numericas en categoricas

euramic$tipfum = factor(euramic$tipfum, levels = 1:3, labels = c("nunca", "ex", "activo"))

euramic$statcc <- factor(euramic$statcc,
                         levels = c(0, 1),
                         labels = c("Control", "Caso"))
euramic$hta <- factor(euramic$hta,
                      levels = c(0, 1),
                      labels = c("Normotenso", "Hipertenso"))
euramic$diabetes <- factor(euramic$diabetes,
                           levels = c(0, 1),
                           labels = c("No diabético", "Diabético"))
str(euramic)

# Correlación y regresión simple
# Utilizando la muestra de los controles (representativos de la población general,
#   1. Representar en un diagrama de dispersión el colesterol HDL y el índice de masa corporal
#   (imc = peso (kg)/altura (m)2 ).

## Crear la columna IMC
euramic$imc <- euramic$peso / (euramic$altura / 100)^2

## Seleccionar únicamente los controles
controles <- subset(euramic, statcc == "Control")

## Diagrama de dispersión
plot(hdlcol ~ imc, data = controles,
     main = "Relación entre IMC y colesterol HDL",
     xlab = "Índice de masa corporal (kg/m²)",
     ylab = "Colesterol HDL (mmol/l)",
     pch = 19, cex =.1,
     col = "pink3")

#   2. Evaluar la asociación lineal entre estas dos variables (interpretar)
cor(controles$hdlcol , controles$imc, use="pairwise.complete.obs")

# [1] -0.2764168
# El coeficiente de correlación lineal de Pearson entre el IMC y el colesterol HDL en los controles es de
# (r=-0,2764). Este resultado indica que existe una asociación lineal negativa débil entre ambas variables:
# los individuos con mayor IMC tienden a presentar niveles más bajos de colesterol HDL.

#   3. Estimar mediante un modelo de regresión lineal el efecto del IMC sobre el nivel de HDL e
simple = lm(hdlcol~imc, data=controles)
summary(simple)

# Call:
#   lm(formula = hdlcol ~ imc, data = controles)

# Residuals:
#   Min      1Q  Median      3Q     Max 
# -0.8407 -0.1844 -0.0463  0.1505  1.2107 

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  1.693956   0.092042  18.404  < 2e-16 ***
#   imc         -0.023238   0.003506  -6.628 8.38e-11 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Residual standard error: 0.2833 on 531 degrees of freedom
# (167 observations deleted due to missingness)
# Multiple R-squared:  0.07641,	Adjusted R-squared:  0.07467 
# F-statistic: 43.93 on 1 and 531 DF,  p-value: 8.385e-11

# por cada aumento de una unidad de IMC (1 kg/m²), el nivel medio estimado de HDL disminuye en 0,0232 mmol/l,
# en los controles.

# La asociación es estadísticamente significativa porque el valor p es inferior a 0,05.
# Por otra parte, el R^2=0.07641 indica que el IMC explica aproximadamente el 7,64 %
# de la variabilidad observada en el HDL mediante este modelo lineal.

#   4. Chequear las asunciones del modelo

par(mfrow=c(2,2))
plot(simple)

# la relación parece aproximadamente lineal, pero hay indicios leves de que la linealidad no es perfecta.

# los residuos no siguen una distribución normal. El gráfico Q-Q también muestra esta desviación, especialmente en las colas.
# Los puntos se alejan claramente de la línea diagonal, especialmente en la cola superior, donde aparecen algunos residuos extremos.

# no se aprecia una heterocedasticidad muy marcada en este gráfico, aunque conviene ser prudentes. 
# El gráfico por sí solo no permite confirmar que la varianza sea constante.

# conviene revisar las observaciones señaladas, especialmente la identificada como 746 
# y las que presentan leverage más elevado. No obstante, el gráfico no muestra por sí solo 
# que exista una observación claramente influyente según la distancia de Cook.

#   5. Calcular el intervalo de confianza de este efecto (interpretar)

confint(simple, "imc", level = 0.95)

#   6. Representar la recta de regresión

plot(hdlcol ~ imc, data = controles,
     main = "Relación entre IMC y colesterol HDL",
     xlab = "Índice de masa corporal (kg/m²)",
     ylab = "Colesterol HDL (mmol/l)",
     pch = 19,
     col = "pink3")
abline(reg=simple,col="blue2")

# Regresión múltiple

# 1. Evaluar la no-linealidad del efecto del IMC sobre el nivel de HDL
fit.spline <- lm(hdlcol ~ ns(imc, 3), data = controles)

rango <- data.frame(
  imc = seq(min(controles$imc, na.rm = TRUE),
            max(controles$imc, na.rm = TRUE),
            length.out = 100)
)

pred.spline <- predict(fit.spline, newdata = rango)

plot(hdlcol ~ imc, data = controles,
     main = "Relación entre IMC y colesterol HDL",
     xlab = "Índice de masa corporal (kg/m²)",
     ylab = "Colesterol HDL (mmol/l)",
     pch = 19, col = "pink3")

grid()

lines(rango$imc, pred.spline,
      col = "blue2", lwd = 2)

m_lineal <- lm(hdlcol ~ imc, data = controles)

anova(m_lineal, fit.spline)

for(k in 1:4) {
  fit=lm(hdlcol~ ns(imc,k), data=controles)
  cat("AIC para spline con",k,"grados de libertad:",AIC(fit),"\n")
}

# La comparación mostró diferencias estadísticamente significativas (\(F=4,024\); \(p=0,018\)),
# por lo que existe evidencia de que el modelo spline proporciona un mejor ajuste que el modelo lineal.
# Esto sugiere que la asociación entre el IMC y el HDL en los controles podría ser no lineal.

# 2. Estimar el efecto del IMC ajustando por el hábito tabáquico (interpretar)

modelo_ajustado <- lm(hdlcol ~ imc + tipfum, data = controles)
summary(modelo_ajustado)

#R^2=0,09198: el modelo explica aproximadamente el 9,2 % de la variabilidad del HDL.
#Prueba global: F=17,83, p=4,9*10^-11. El modelo es estadísticamente significativo en conjunto.
# n comparación con quienes nunca habían fumado, los fumadores activos presentaron niveles de HDL 
# significativamente menores (β = −0,0658; p = 0,034), mientras que no se observaron diferencias 
# estadísticamente significativas entre exfumadores y quienes nunca habían fumado (p = 0,585).

# 3. Contrastar si el efecto del IMC está modificado por el consumo de tabaco.

modelo_interaccion <- lm(
  hdlcol ~ imc * tipfum,
  data = controles
)

summary(modelo_interaccion)

# Ambos valores p son mayores que 0,05. Por tanto, ninguna de las dos interacciones es estadísticamente significativa.

anova(modelo_ajustado, modelo_interaccion)

# Aunque cada interacción por separado no era significativa (p = 0,1252 y p = 0,5199),
# la prueba conjunta indica que, consideradas en conjunto, las interacciones mejoran significativamente el ajuste.
# No hay contradicción: las pruebas individuales y la prueba conjunta evalúan hipótesis distintas.

# 4. Representar en un grafico, la relación entre HDL e IMC según el hábito tabáquico

# Crear un rango de valores de IMC
rango <- data.frame(
  imc = seq(min(controles$imc, na.rm = TRUE),
            max(controles$imc, na.rm = TRUE),
            length.out = 100)
)

# Colores para cada grupo
colores <- c("pink3", "blue3", "darkgreen")

# Diagrama de dispersión
plot(hdlcol ~ imc, data = controles,
     col = tipfum, pch = 19,
     xlab = "IMC (kg/m²)",
     ylab = "Colesterol HDL (mmol/l)",
     main = "IMC y HDL según hábito tabáquico")

grid()

legend("topright",
       legend = levels(controles$tipfum),
       col = 1:3, pch = 19, lty = 1)


ggplot(controles, aes(x=imc, y=hdlcol,color=tipfum))+
  geom_smooth(se=FALSE)

# Regresión logistica
modelo1 <- glm(
  hta ~ (tipfum=="activo"),
  data = controles,
  family = binomial
)

summary(modelo1)

# OR e intervalo de confianza del 95 %
exp(cbind(OR = coef(modelo1), confint(modelo1)))

crudo <- glm(
  hta ~ hdlcol,
  data = controles,
  family = binomial
)
summary(crudo)
exp(coef(crudo))
