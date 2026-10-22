n=1000 #tamaño muestral

# Generar el habito tabaquico de la madre al azar
fuma=sample(c(FALSE,TRUE),n,replace = TRUE)

# Ahora lo mismo para la edad gestacional
gestacion=sample(c(32:42),n,replace=TRUE)

# Queremos que el peso de los niños de 32 semanas pesen 2000gr (2kg). Además, queremos que pesen más los niños cuando la madre no fuma que cuando fuma. Añadimos el error normal al final, con desviación tipica de media cero
sigma=100
peso=2000 + (gestacion-32)*(fuma==TRUE)*100 + (gestacion-32)*(fuma==FALSE)*150 + rnorm(n,0,sigma)

# Creamos el dataframe con los datos que hemos generado
simu=data.frame(peso=peso,fuma=fuma,gestacion=gestacion)

# analizamos el peso respecto de la edad gestacional
simple=lm(peso~gestacion,data=simu)
summary(simple)
plot(peso~gestacion,data=simu,cex=.1)
abline(reg=simple,col="red3")

#para ver la linealidad vemos la gráfica residual vs fitted
# en q-q residuals vemos que en pesos grandes se acaba volviendo bimodal porque tenemos pesos muy grandes y muy pequeños
plot(simple)

## la hipotesis de homocedasticidad no se cumple

# Incluimos el tabaco en el modelo 
ajuste1 = lm(peso~gestacion+fuma,data=simu)
summary(ajuste1)
plot(ajuste1,cex=.1)

# el grupo de referencia es el grupo de NO fumadora
ajuste2 = lm(peso~gestacion+fuma+gestacion:fuma,data=simu)
plot(ajuste2, cex=.1)
# en los niños de referencia el peso crece 150 gramos por cada semana gestacional
# 1500 gr en fumaTRUE significa el valor que habria en la semana gestacional = 0 que no tendría sentido biologico y es una estimación
# en el termino de interacion gestacion:fumaTRUE los niños de madre fumadora crecen 50 gramos menos que los de madre no fumadora
# En la funcion de interaccion de beta1 es el incremento por la semana gestacional en las mujeres no fumadoras que es 150.
# Este modelo se ajusta mejor a los datos. Las hipotesis se cumplen mejor porque corregimos por la interacción entre la gestación y el habito tabaquico

AIC(simple)
AIC(ajuste1)
AIC(ajuste2)
# El AIC baja por lo que tenemos un ajuste mejor.

