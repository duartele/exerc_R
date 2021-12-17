x1<-c(1.74,1.83,1.68,1.89,1.78,1.83)
y1<-c(68,80,62,89,70,78)

x2<-c(68.5,72,74.4,72,70.1,66.1)
y2<-c(149.9,176.4,196.2,172,154.3,136.7)

cov(x1,y1)
cov(x2,y2)
cor(x1,y1)
cor(x2,y2)

plot(x1,y1)
plot(x2,y2)

data("anscombe")
summary(anscombe)
#mesmos valores e mesmas correlação. No entando, se representarmos graficamente, teremos
windows()
layout(matrix(c(1,2,3,4),ncol=2))
for (i in 1:4){
  cat('A correlação entre', colnames(anscombe[i]),
      'e', colnames(anscombe[i+4]), 'é:',
      cor(anscombe[,i], anscombe[,4+i]),'\n')
  plot(anscombe[,i], anscombe[,i+4])
  }

x<-(-3:3)
y<- 2*x+3
z<- x*x
w<-y+rnorm(7,0,4) #criando um ruído com distr normal
cor(x,y)
cor(x,z)
cor(x,w)


#Pearson vs Spearman
x3<-(1:5)
y3<-c(2,5,8,11,14)
x4<-(1:5)
y4<-c(2,5,9,12,18)
cor(x3,y3)
cor(x4,y4)
cor(x3,y3,method = "spearman")
cor(x4,y4,method ="spearman")
plot(x3,y3)
plot(x4,y4)

perfume_x<- c(10,1,2,5,4,3,6,7,9,8)
perfume_y<- c(95,60,52.5,51.5,49.5,47.5,55,48,56,53)
plot(perfume_x, perfume_y)
cor(perfume_x, perfume_y)
cor(perfume_x, perfume_y, method ="spearman")

#Kendall
x_ken<- c(1,2,3,4)
y_ken<- c(0.9,2.7,1.8,2.9)
cor(x_ken, y_ken, method = "kendall")

#Exercício 1 da folhaexercicios2
x<-c(1,7,13,20,27,34,62) #dias - var explicativa
y<-c(5,10,12,29,36,83,102) #comprimento da raiz
plot(x,y)
modelo<-lm(y~x) 
summary(modelo) #R2 = 0.9001
modelo2<-lm(y~0+x) #put b0 = 0 
summary(modelo2) #R2 = 0.9552 - it's better!

'Significa que 90% da variabilidade de y é explicada pelo modelo de 
regressão linear'
cor(x,y)^2 #R2 = cor(x,y)^2 - funciona só na regressão simples
'SST = var(y)*(7-1)
SSE = sum(y-fitted(modelo)^2)
R2 = 1 - SSE/SST
SSR = sum((fitted(modelo)-mean(y))^2)
R2 = SSR/SST'
?lm
fitted(modelo)
residuals(modelo)
confint(modelo, level=0.95)
anova(modelo)
#SSR = 7864.9
#SSE = 872.8
872.8/5
7864.9+872.8 = SST
ET=7864.9/174.6

p_valor = 1-pf(45.056,1,5) 
qqnorm(rstandard(modelo), las=1, pch=19)
qqline(rstandard(modelo)) # Add reference line
#Percebemos que temos dois pontos fora. O fato de ter poucas observações atrapalha
'testar a normalidade dos erros
testar variância constante e média nula'
plot(rstandard(modelo) ~ fitted(modelo), ylab="Standardized residuals", xlab="Fitted values")

#testar independência dos erros
plot(rstandard(modelo))
'
Intervalo de previsão para uma observação futura X=x:
  predict(modelo,new=data.frame(x=c(10)),interval="prediction")

Intervalo de confiança para a média (valor esperado) de Y quando X=x
predict(modelo,data.frame(x=c(10)),interval="confidence")
'