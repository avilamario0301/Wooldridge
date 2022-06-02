#Tarea 1 Mario Avila 1100370 C6.2
setwd("~/Econometría/Tarea 1")
library(haven)
WAGE1 <- read_dta("D:/Stata/Wooldridge Data old/WAGE1.dta")
View(WAGE1)
reg1 <- lm(lwage~educ+exper+expersq, data = WAGE1)
stargazer::stargazer(reg1, type="text", style = "all", digits = 4)
#ii. La tabla arroja que el valor de t es de -6.164, esto significa que exper es significativa al 1% de significancia
100*(0.041-2*(0.0007)*4)
#El quinto año de experiencia tiene un rendimiento aproximado de 3.54%
100*(0.041-2*(0.0007)*19)
#El vigésimo año de experiencia tiene un rendimiento aproximado de 1.44%

#iii. Al derivar %Δw con respecto a Δexper e igualar a 0 dicha derivada tenemos que: B2/2B3=Exper
0.041/(2*(0.0007))
#Esto da como resultado que alrededor de los 29 años de experiencia empieza a reducir el log(wage) predecido
nrow(subset(WAGE1, exper>=29))
#Alrededor de 121 personas tienen más de 29 años de experiencia.

