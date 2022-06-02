#Tarea 2 Mario Avila
setwd(dir = "~/Econometría/Tarea 2")
#6.1
library(haven)
CEOSAL1 <- read_dta("D:/Stata/Wooldridge Data old/CEOSAL1.dta")
View(CEOSAL1)
Roesq<-(CEOSAL1$roe^2)
Regceosal1<-(lm(CEOSAL1$lsalary~CEOSAL1$lsales+CEOSAL1$roe+Roesq))
stargazer::stargazer(Regceosal1, type="text", style = "all", digits = 5)
#No, su valor t (-0.3) es muy bajo, esto demuestra que el impacto de esta
#Variable, ademas tampoco tiene un gran impacto en la pendiente.

#6.7
#Eligiria la segunda ecuacion, su valor de R^2 y R^2 Ajustado es mayor, ademas
#la mejor manera de incluir la variable de empleados totales parece ser la segunda

#6.8
#i. No, pues para apreciar el efecto total del alcohol en ColGPA attend no debe ser incluida
#Pero, si es incluida, el coeficiente del alcohol se interpreta como el efecto que tiene en COLGPA el alcohol pero 
#Que no son causa de no asistir a clase
#ii. Si, pues estas variables serian buenas como variables control, pues pueden ayudar a medir las abilidades de los estudiantes

#Ejercicios de computadora

#C6.7
ATTEND <- read_dta("D:/Stata/Wooldridge Data old/ATTEND.dta")
View(ATTEND)
#i. 
-1.63+2*((0.296)*(2.59))+0.0056*(82)
#Un aumento de 1 en PriGPA causa un aumento en la calificación final de 0.36248 aproximadamente

#ii
Prmsq<-((ATTEND$priGPA-2.59)^2)
ACTsq<-((ATTEND$ACT)^2)
GpAtnd<-(ATTEND$priGPA*(ATTEND$atndrte-82))
RegStndr<-lm(ATTEND$stndfnl~ATTEND$atndrte+ATTEND$priGPA+ATTEND$ACT+Prmsq+ACTsq+GpAtnd)
stargazer::stargazer(RegStndr, type = "text", style = "all")
#El error estándar es aproximadamente =0.362

#iii. Esto hace que las variables sean ajustadas a su valor promedio.

#C6.12
#i.
X401KSUBS <- read_dta("D:/Stata/Wooldridge Data old/401KSUBS.dta")
View(X401KSUBS)
Size1<-(subset(X401KSUBS, X401KSUBS$fsize==1))
summary(Size1$age)
nrow(subset(Size1, Size1$age==25))
#Luego de crear la base de datos que contenga solo los hogares de una persona, se procede a buscar el valor mínimo.
#Luego, se crea un subconjunto que solo incluya los valores mínimos para encontrar cuantas personas tienen 25 años,
#En este caso, 99.

#ii.La interpretación seria el impacto que tiene el aumento de un año de edad sobre nettfa, que son los activos financieros finales netos
#iii.
regnetffa<-lm(Size1$nettfa~Size1$inc+Size1$age+Size1$agesq, data = Size1)
stargazer::stargazer(regnetffa, type = "text", style = "all", digits = 5)
abs(-1.321815 / (2*0.025562)) 
#No, puesto que si calculamos el punto de inflexión (Como encontrarlo está en la tarea 1) podemos ver que es alrededor de los 25 años tardíos
#Esto explica perfectamente la negatividad del coeficiente de age

#iv. 
Agem25<-((Size1$age-25)^2)
Regnetiv<-lm(Size1$nettfa~Size1$inc+Size1$age+Agem25)
stargazer::stargazer(Regnetiv, type = "text", style = "all" )
#El efecto parcial comenzando en 25 años es -0.044, su valor t es aprox -0.134, su valor p es aprox 0.894. Comprobamos que es estadísticamente insignificante 

#v.
RegnetV<-lm(Size1$nettfa~Size1$inc+Agem25)
stargazer::stargazer(RegnetV, type = "text", style = "all")
#La bondad de ajuste es un casi la misma, aunque un poco mayor, lo importante es que al quitar age por su valor t pequeño el modelo se interpreta mejor

#vi. 

  