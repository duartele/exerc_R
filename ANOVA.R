library(car)
#library(lawstat)
library(MASS)

#Teste de homocedasticidade
leveneTest(birthwt$bwt, group=birthwt$race, center=mean) #library car
bartlett.test(x=birthwt$bwt, g=birthwt$race)
bartlett.test(birthwt$bwt~birthwt$race) #formula

#Teste de normalidade
shapiro.test(birthwt$bwt[birthwt$race==2])

#ANOVA
fit<- aov(bwt~factor(race), data=birthwt) #race is integer!!!
class(birthwt$race)
str(birthwt)
summary(fit)

#Comparações Múltiplas
TukeyHSD(fit)
