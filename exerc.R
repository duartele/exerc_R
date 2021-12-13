#Exerc 1
x<-c(66,114,137,124,104,124,111,48)
y<-c(40,155,187,156,118,162,149,15)
#a)
modelo1<-lm(y~x)
summary(modelo1)
'b1 = 2.0035, significa que para cada cm de perímetro de peito temos 
um aumento médio de 2kg no peso
'
#b) temos um valor de R2 elevado (0.9842), indicando um bom ajuste do modelo
#c) A correlação linear de x e y é de 0.9920484
cor(x,y)
#d) IC de 90% para beta0 IC: (-106.269685 , -62.949934)
confint(modelo1, level=0.90)
#e)
'Para sabermos se de fato há uma relação entre o perímetro e o peso, temos
que verificar se beta1 é igual a 0. Se for, significa que não há uma relação
H0: beta1=0 vs H1:beta01 != 0
Para verificarmos isso podemos verificar se 0 está no intervalo de confiança de 
95% ou verificar no se o p-valor é maior que alpha.
Como p-valor é maior que alpha, rejeitamos a hipótese nula, e
então temos que a relaçao entre o perímetro e o peso'
summary(modelo1)
#f) E[Y|x=80] = 75.6683 = Yhat_x=80. Para saber o intervalo de confiança
#para o valor esparado digitamos interval="conf". Para saber de um urso em específico
#digitamos interval="pred"
predict(modelo1, data.frame(x=c(80)),level=0.99, interval = "pred")
#g) Para obtermos as informações digitaremos o comando anova(modelo1)
anova(modelo1)
SSR = 26556.1
MSE = 71.2
#SST = SSR + SSE = 26983.5
26556.1 + 427.4

#h) Precisamos verificar se os resíduos são independentes, possuem variância constante
#e se estão normalmente distribuídos
'Para a normalidade, podemos fazer um qqnorm dos resíduos standardizados'
qqnorm(residuals(modelo1))

#Grupo2
#a) sigma2hat=8.3/83=0.1 Ymean=b0+b1*xmean = 80.75
-2.5+0.5*166.5
#b) Podemos obter pela raiz quadrada de R2 e o sinal será positivo popr causa de b1
R2=1-SSE/SST
R2=1-8.3/100 #0.917
corxy=sqrt(R2) #0.9576012
#c)                 SS         gl        MS      F0         pvalor
'Regressão         91.7         1       91.7     917         0
Resíduos           8.3         83       0.1
Total              100         84'

#d) H0: beta0 = 0 vs H1: bet0 != 0
sigmahatbo = sqrt(0.1*(1/85+166.5/(84*11.15)))
ET = -2.5/sigmahatbo #-18.15912
qt(.975,83)
#|ET|> qt(.975,83) Rejeitamos H0 ao nível de 5%
#e) IC 95% = (76.8534,78.1466)
mu_hat<--2.5+0.5*160 #77.5
qt(0.975,83)
sigma2hat<-0.1*(1+1/85+(160-166.5)^2/(84*11.15))
IC <- mu_hat + c(-1,1)*qt(0.975,83)*sqrt(sigma2hat)

