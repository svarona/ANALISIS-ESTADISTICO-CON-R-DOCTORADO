rm(list=ls())
gc()

setwd('/Users/pfernandezn/Desktop/ANALISIS-ESTADISTICO-CON-R-DOCTORADO-main 2')

getwd()

curso<-"Curso 2026"
curso<-2026

curso_2026 <- 2026
curso_2026 =2026

x <- 1:15
length(x)
class(x)


y <- matrix(5, nrow = 3, ncol = 4)
dim(y)
class(y)


datos<-data.frame(ID=c("a1", "a2", "a3"), edad=c(15,22,7))
as.factor(x)


demo(graphics)
library(ggplot2)

openxlsx::read.xlsx()
ggplot2::ggplot()

my.data.frame<-data.frame(ID=c("paciente1","paciente2","paciente3"), edad=c(10,25,33),altura=c(NA,150,180), cancer=c(TRUE,TRUE,FALSE),bmi=c(22,25,20))

load("datos/datos.curso1.RData")
#library(data.table)

#datos<-data.table(datos)
class(datos)

datos$"sexo"
length(datos$"sexo")

datos[,3]

datos[1:3,3]
datos[1:10,1:3]
datos[datos$"sexo"=="Mujer",] 
datos[datos$"sexo"=="Mujer"&datos$"estado.civil"=="Casado",1:10]
datos[datos$"sexo"%in%"Mujer"&datos$"estado.civil"%in%"Casado",1:10]


datos.new2<-datos 
datos.new2[datos.new2$”ID”==200,sexo]<-"Mujer"
datos.new2$”sexo”[datos.new2$”ID”==200]<-"Mujer"

datos.nuevos1 <-datos[1 ,1:3]

datos.union1<-cbind(datos[1:20,],datos.nuevos1)


datos.nuevos2 <-datos[1:5, ]
datos.union2 <-rbind(datos,datos.nuevos2)

indice5 <-datos$"sexo"!="Mujer"
datos[indice5,]
datos[ , names(datos)!="peso"]

indice7 <-which(datos$"sexo"=="Mujer")
datos[indice7,]


datos$cancer.mama[is.na(datos$cancer.mama)]<-"No"

prueba<-datos[indice7,]
row.names(prueba)<-NULL


x<-c(1:10); y<-x/2 
x+15; x2<-c(1:5)

datos.con.missing<-datos
datos.con.missing$peso[datos.con.missing$peso<60]<-NA

mean(datos.con	.missing$peso)
sum(is.na(datos.con.missing$"peso"))

summary(datos)

datos$ratio<-datos$altura/datos$edad

?openxlsx::write.xlsx

a1 <-rnorm(10); a2 <-1:10; a3 <-letters[10:20]
save(a1, a2, file="datos/unos.datos.guardados.RData")

?median

install.packages("data.table")
library(data.table)
library(SNPannot)

datos$"ratio"<-datos$"altura"/datos$"edad"

x<-c(2:6)
lista<-list(vector1=x,datos=datos)
lista$vector1 # lista[[1]]
lista$datos

########################################################
########################################################
# Estadistica descriptiva
########################################################
########################################################
rm(list=ls())
gc()

setwd('/Users/pfernandezn/Desktop/ANALISIS-ESTADISTICO-CON-R-DOCTORADO-main 2')

load("datos/datos.curso1.RData")

mean(datos$"peso")
median(datos$"peso")
exp(mean(log(datos$"peso")))
quantile(datos$"peso")

quantile(datos$"peso",prob=seq(0,1,1/4))
quantile(datos$"peso",prob=seq(0,1,1/3))
quantile(datos$"peso",prob=seq(0,1,1/10))

datos$"peso.gr" <- cut(datos$"peso",breaks=quantile(datos$"peso", prob=seq(0,1,1/3)),right=TRUE, include.lowest=TRUE)

sd(datos$"peso")
IQR(datos$"peso")

range(datos$"peso")
hist(datos$"peso",xlab="Peso (kg)",ylab="N",main="Peso",ylim=c(0,60))
qqnorm(datos$"peso")

library(crosstable)
library(knitr)
res<-crosstable(datos, c(peso))
kable(res)


datos.hombre<-datos[datos$"sexo"%in%"Hombre",]
datos.mujer<-datos[datos$"sexo"%in%"Mujer",]
tapply(datos$"peso",datos$"sexo",mean)

tapply(datos$"peso",datos$"sexo",mean)[1]

res<-crosstable(datos, c(peso), by=sexo)
kable(res)

par(mfrow=c(1,2))
hist(datos$"peso"[datos$"sexo"%in%"Hombre"],main="Peso Hombres",xlab="Peso (Kg)")
hist(datos$"peso"[datos$"sexo"%in%"Mujer"],main="Peso Mujeres",xlab="Peso (Kg)")

library(ggplot2)

ggplot(datos, aes(x = peso)) +geom_histogram() +facet_wrap(~sexo, nrow = 1,scales="free_x") +labs(x = "Peso (Kg)",y = "Frecuencia")+theme_minimal()

boxplot(datos$"peso" ~ datos$"sexo", main="Peso",xlab="Sexo",ylab="Peso (Kg)")

table(datos$"estado.civil",exclude=NULL)
as.data.frame(table(datos$"estado.civil",exclude=NULL))

margin.table(table(datos$"estado.civil",exclude=NULL))

prop.table(table(datos$"estado.civil",exclude=NULL))

prop.table(table(datos$"estado.civil",exclude=NULL))*100


barplot(table(datos$"estado.civil"))

barplot(prop.table(table(datos$"estado.civil"))*100,ylim=c(0,50))

table(datos$"estado.civil",datos$"sexo",exclude=NULL)

margin.table(table(datos$"estado.civil",datos$"sexo",exclude=NULL),1)

margin.table(table(datos$"estado.civil",datos$"sexo",exclude=NULL),2)


prop.table(table(datos$"estado.civil",datos$"sexo",exclude=NULL))*100

prop.table(table(datos$"estado.civil",datos$"fumador",exclude=NULL))*100
table(datos$"estado.civil",datos$"fumador",exclude=NULL)

prop.table(table(datos$"estado.civil",datos$"sexo",exclude=NULL),1)*100

prop.table(table(datos$"estado.civil",datos$"sexo",exclude=NULL),2)*100

par(mfrow=c(2,1))
barplot(table(datos$"estado.civil"[datos$"sexo"%in%"Hombre"]),
main="Hombres",ylim=c(0,50))
barplot(table(datos$"estado.civil"[datos$"sexo"%in%"Mujer"]),
main="Mujeres",ylim=c(0,50))


gmodels::CrossTable(datos$"estado.civil",datos$"sexo",prop.r=TRUE, prop.c=TRUE,prop.chisq=FALSE)
library(crosstable)
crosstable(datos,c(estado.civil),by=sexo)%>%
as_flextable(keep_id=TRUE)


crosstable(datos,c(peso,estado.civil),by=sexo)%>%
as_flextable(keep_id=TRUE)


#############################################
# Ejercicios
#############################################
d<-crosstable(
 datos,
	c(edad, peso, altura),
funs = c(
"Media" = mean,
"Mediana" = median,
 "Media geométrica" = function(x) exp(mean(log(x))),
 "Q1" = function(x) quantile(x, 0.25),
"Q2" = function(x) quantile(x, 0.50),
"Q3" = function(x) quantile(x, 0.75),
 "Coef. variación (%)" = function(x) sd(x) / mean(x) * 100,
"Desv. típica" = sd,
"Máximo" = max,
 "Mínimo" = min
  )
 )

kable(d)

d1<-crosstable(
  datos,
  c(edad, peso, altura),
  by = sexo,
  funs = c(
    "Media" = mean,
    "Mediana" = median,
    "Media geométrica" = function(x) exp(mean(log(x))),
    "Q1" = function(x) quantile(x, 0.25),
    "Q2" = function(x) quantile(x, 0.50),
    "Q3" = function(x) quantile(x, 0.75),
    "Coef. variación (%)" = function(x) sd(x) / mean(x) * 100,
    "Desv. típica" = sd,
    "Máximo" = max,
    "Mínimo" = min
  )
)

kable(d1)

stripchart(datos$peso~datos$fumador)
stripchart(datos$peso~datos$fumador,method="jitter")

margin.table(table(datos$nivel.estudios),margin=1)

prop.table(table(datos$nivel.estudios))


########################################################
########################################################


ASOCIADOS EN SEXO (H/M) Y EL PESO (NUMERICA) (REGRESION)

COMPARANDO EL PESO ENTRE HOMBRES Y MUJERES (TECNICAS DE COMPARACION)

ANALISIS DESCRIPTIVOS (TECNICAS DESCRIPTIVAS Y TÉCNICAS INFERENCIALES DE COMPARACION (TESTS...))


########################################################
########################################################


########################################################
########################################################
# Analisis de comparacion
########################################################
########################################################

rm(list=ls())
gc()

setwd("/Users/pfernandezn/Desktop/ANALISIS_ESTADISTICO_CON_R_DOCTORADO_2026/DATOS/")

load("datos.curso1.RData")


teorico_datos <- rnorm(n=10000, mean = mean(datos$"peso"), sd = sd(datos$"peso"))
par(mfrow=c(1,2))
hist(datos$"peso",main="Muestral datos")
hist(teorico_datos,main="Teórico datos")


mujeres<- datos[datos$"sexo"%in%"Mujer",]
mean(mujeres$"peso")
sd(mujeres$"peso")
teorico_mujeres<- rnorm(n=10000, mean = mean(mujeres$"peso"), sd = sd(mujeres$"peso"))
par(mfrow=c(1,2))
hist(mujeres$"peso",main="Muestral")
hist(teorico_mujeres,main="Teórico")


res <- t.test(x=mujeres$"peso",conf.level = 0.95)
class(res)
names(res)

hombres <- datos[datos$sexo=="Hombre",]
mujeres <- datos[datos$sexo=="Mujer",]

mean(hombres$"peso")-mean(mujeres$"peso")

res<-t.test(hombres$"peso",mujeres$"peso", conf.level = 0.95)
res
names(res)
res$"p.value"


res<-t.test(hombres$"peso",mujeres$"peso", conf.level = 0.95,var.equal=TRUE)
res$"p.value"


res<-t.test(hombres$"peso",mu = 90,sd=1, conf.level = 0.95)
res

res_dos_colas<-t.test(hombres$"peso",mu = 79.80,sd=1,
                      conf.level = 0.95,alternative = c("two.sided"))
res_dos_colas$"p.value"


res_less<-t.test(hombres$"peso",mu = 79.80 ,sd=1,
                 conf.level = 0.95,alternative = c("less"))
res_less$"p.value"


res_less<-t.test(hombres$"peso",mu = 79.80 ,sd=1,
                 conf.level = 0.95,alternative = c("greater"))
res_less$"p.value"

# Asunciones

shapiro.test(hombres$"peso")

shapiro.test(mujeres$"peso")

var.test(x=hombres$"peso",y=mujeres$"peso")


# test no paremetricos

res<-wilcox.test(hombres$"peso",mujeres$"peso")
res

res<-wilcox.test(hombres$"peso",mu=79.80)
res

res<-t.test(hombres$"peso",mu=79.80)
res

# Mas de dos  grupos

anova(lm(hombres$"peso"~hombres$"nivel.estudios"))
pairwise.t.test(x=hombres$"peso",g=as.factor(hombres$"nivel.estudios"),p.adjust="BH")


shapiro.test(hombres$"peso"[hombres$"nivel.estudios"%in%"Alto"])

shapiro.test(hombres$"peso"[hombres$"nivel.estudios"%in%"Medio"])

shapiro.test(hombres$"peso"[hombres$"nivel.estudios"%in%"Bajo"])

bartlett.test(hombres$"peso" ~ hombres$"nivel.estudios") # en el limite


kruskal.test(hombres$"peso"~hombres$"nivel.estudios")
pairwise.wilcox.test(x=hombres$"peso",g=as.factor(hombres$"nivel.estudios"),p.adj="BH")

#################################
# COMPARACION PROPORCIONES
#################################

# Inferencia

table(datos$"fumador",exclude=NULL)

prop.table(table(datos$"fumador",exclude=NULL))

prop.test(table(datos$"fumador",exclude=NULL)[2:1])

prop.test(table(datos$"fumador",exclude=NULL)[2:1],correct = FALSE)


# Comparar con un valor concreto

prop.test(as.numeric(table(datos$"fumador"))[2] , 
          dim(datos)[1] ,
          0.40,
          alternative="two.sided")
          
# Comparar la proporcion de fumadores entre hombres y mujeres

table(datos$sexo,datos$fumador)

table(datos$sexo,datos$fumador)[,c(2,1)]

prop.table(table(datos$sexo,datos$fumador),1)

prop.test(table(datos$sexo,datos$fumador)[,c(2,1)])

prop.test(table(datos$sexo,datos$fumador)[,c(2,1)],correct=FALSE)


# Compara la proporcion de fumadores entre los distintos niveles de estudios
table(datos$nivel.estudios,datos$fumador)[c(2,3,1),c(2,1)]
margin.table(table(datos$nivel.estudios,datos$fumador)[c(2,3,1),c(2,1)],1)

# igual que el anova en variables cuantitativas
prop.test(table(datos$nivel.estudios,datos$fumador)[c(2,3,1),c(2,1)])

prop.trend.test(table(datos$nivel.estudios,datos$fumador)[c(2,3,1),c(2)],
margin.table(table(datos$nivel.estudios,datos$fumador)[c(2,3,1),c(2,1)],1))



############
# TABLAS
############

# install.packages("gtsummary")

library("gtsummary")

datos1<-subset(datos,select=c(ID,altura,edad,sexo)) # centrada en variables numericas

subset(datos1,select=-c(ID,sexo)) %>%
tbl_summary()%>%


subset(datos1,select=-c(ID)) %>%
tbl_summary(by="sexo")%>%
add_overall()


subset(datos1,select=-c(ID)) %>%
tbl_summary(by="sexo",statistic = all_continuous() ~ "{mean}")%>%
add_overall()


subset(datos1,select=-c(ID)) %>%
tbl_summary(by="sexo",statistic = all_continuous() ~ "{mean}")%>%
add_overall()%>%
add_ci(pattern = "{stat} ({ci})")%>%
add_p()

subset(datos1,select=-c(ID)) %>%
tbl_summary(by="sexo",statistic = all_continuous() ~ "{mean}")%>%
add_overall()%>%
add_ci(pattern = "{stat} ({ci})")%>%
add_p(test = everything() ~ "t.test")


subset(datos1,select=-c(ID)) %>%
tbl_summary(by="sexo",statistic = all_continuous() ~ "{mean}")%>%
add_overall()%>%
add_ci(pattern = "{stat} ({ci})")%>%
add_p(test = everything() ~ "t.test", test.args = all_tests("t.test") ~ list(var.equal = TRUE))


datos2<-subset(datos,select=c(ID,diabetes,estado.civil,fumador)) # centrada en variables categoricas

subset(datos2,select=-c(ID)) %>%
tbl_summary()

subset(datos2,select=-c(ID)) %>%
tbl_summary(by="fumador")


subset(datos2,select=-c(ID)) %>%
tbl_summary(by="fumador")%>%
add_p()


datos3<-subset(datos,select=c(ID,altura,edad,sexo,diabetes,estado.civil,fumador)) # centrada en variables numericas

subset(datos3,select=-c(ID)) %>%
tbl_summary(by="sexo",statistic = all_continuous() ~ "{mean}")%>%
add_overall()%>%
add_ci(pattern = "{stat} ({ci})")%>%
add_p()

subset(datos3, select = -c(ID)) %>%
  tbl_summary(by = "sexo",statistic = all_continuous() ~ "{mean}") %>%
  add_overall() %>%
  add_ci(pattern = "{stat} ({ci})") %>%
  add_p(
    test = list(
      all_continuous() ~ "t.test"),
    test.args = all_tests("t.test") ~ list(var.equal = TRUE)
  )

subset(datos3, select = -c(ID)) %>%
  tbl_summary(
    by = "sexo",
    statistic = all_continuous() ~ "{mean}",
    label = edad ~ "Age"
  ) %>%
  add_overall() %>%
  add_ci(pattern = "{stat} ({ci})") %>%
  add_p(
    test = list(
      all_continuous() ~ "t.test"
    ),
    test.args = all_tests("t.test") ~ list(var.equal = TRUE)
  )


# Combinar tablas

tbl0<-subset(datos1,select=-c(ID,sexo)) %>%
  tbl_summary(statistic = all_continuous() ~ "{mean}") %>%
  add_ci(pattern = "{stat} ({ci})")

tbl1<-subset(datos1,select=-c(ID)) %>%
  tbl_summary(by="sexo",
              statistic = all_continuous() ~ "{mean}")%>%
  add_ci(pattern = "{stat} ({ci})")

tbl2<-subset(datos1,select=-c(ID)) %>%
  tbl_summary(by="sexo") %>%
  add_p(test = everything() ~ "t.test")%>%
  modify_column_hide(all_stat_cols())

tbl3<-subset(datos1,select=-c(ID)) %>%
  tbl_summary(by="sexo") %>%
  add_p(test = everything() ~ "wilcox.test")%>%
  modify_column_hide(all_stat_cols())

tbl_final <- 
  tbl_merge(list(tbl0,tbl1, tbl2, tbl3)) %>%
  modify_spanning_header(everything() ~ NA)

tbl_final

# Exportacion

setwd("/Users/pfernandezn/Desktop/")

library("flextable")
tf <- tempfile(fileext = ".docx")
tf<-("tabla_exportar.docx")
ft1 <- as_flex_table(tbl_final)
save_as_docx(ft1, path = tf)

library(flextable)
tf <- tempfile(fileext = ".png")
tf<-("tabla_resultados.png")
ft1 <- as_flex_table(tbl_final)
save_as_image(ft1, path = tf)

##### CORRELACIONES ###########
starwars=read.csv("regression/data/starwars.csv")

cor(starwars$height,starwars$mass)
cor(starwars$height,starwars$mass, use = "pairwise.complete.obs")
mean(starwars$height)
mean(starwars$height,na.rm = TRUE)


## El miercoles no tome apuntas
## El jueves empezamos con la simulacion
## Ahora vemos el ODDs ratio
neonatos=read.csv(file = "./regression/data/neonatos.csv")
neonatos40=subset(neonatos,gestacion == 40) # neonatos con una edad de gestación de 40 semanas
neonatos40$peg = (neonatos40$peso <= 2800) # niños pequeños para su edad gestacional (PEG)
table(neonatos40$peg)
xtabs(~ fuma + peg, data=neonatos40)


### ejercicio del titanic
titanic <- read.csv(file = "./regression/data/titanic.csv")
mortalidad=glm(cbind(No,Yes)~Class, data=titanic, family=binomial)
summary(mortalidad)
exp(coef(mortalidad))
# la gente de segunda clase tiene dos veces más probabilidades de morirse que los de primera, y los de tercera casi 5 veces más. Y la tripulación más de 5 veces más.
