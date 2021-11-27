x1<-c(1.74,1.83,1.68,1.89,1.78,1.83)
y1<-c(68,80,62,89,70,78)

x2<-c(68.5,72,74.4,72,70.1,66.1)
y2<-c(149.9,176.4,196.2,172,154.3,136.7)

cov(x1,y1)
cov(x2,y2)
cor(x1,y1)
cor(x2,y2)

data("anscombe")
summary(anscombe)
#mesmos valores e mesmas correlação. No entando, se representarmos graficamente, teremos
plot(anscombe$x2, anscombe$y2)

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
summary(modelo)
fitted(modelo)
residuals(modelo)
confint(modelo, level=0.95)
