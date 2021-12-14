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

#Porva 2
y<-c(2.8,2.9,3.0,3.1,3.2,3.2,3.2,3.3,3.4)
x<-c(27,23,30,28,30,32,34,33,30)
#a)
modelo1<-lm(y~x)
summary(modelo1)
'b1hat = 0.03963, o que significa que, em média, a cada aumento de 1 unidade no 
teor de ferro é esperado um aumento na sua densidade de 0.03963'
#b)
confint(modelo1, level=.99)
'Após executarmos o comando acima, obtemos o intervalo para b0 e b1.
Queremos o de b0 (ordenada na origem). 
Então temos o IC de 99% (0.31307060, 3.58001582)'
#c)
'Pede-se para testar se H0: b1 = 0 vs H1: b1 != 0, pois se b1 for zero,
significa que não há relação entre o teor de ferro e sua densidade
Se o p-valor for maior que alpha, não rejeitaremos H0. Ao digitarmos
summary(modelo1), vemos que o p-valor para b1 é de 0.03906 < 5%.
Então, rejetamos H0 ao nível de significância de 5%'
summary(modelo1)
#d)
'Para calcular a correlação linear entre as vairáveis, basta digitar,
cor(x,y). Então o coedficiente de correlação é 0.691547'
#e)
'Para obter uma estimativa para o valor esperado de Y dado x ou de um
indivídio Yi em particular, basta usar o comando predict. Vale ressaltar,
o que muda entre o valor esperado de Y dado x e um particular Yi é que o segundo
possui maior variância. O Valor obtido foi de 3.531728.
Porém, como ele se encontra fora do escopo da variável x, temos que verificar se 
o comportamento de x e y continua  mesmo fora do escopo das variáveis.
Com os dados que temos, não conseguimos garantir isso'
predict(modelo1, data.frame(x=c(40)))
#g
'lny = ln(ax^b) = ln(a) + b*ln(x)
Portanto Y*=lny, b0*=ln(a), b1*=b e x*=ln(x)
Vamos fazer essas transformações e calcular novamento a retad e regressão
'
y_trans <- log(y)
x_trans <- log(x)
modelo2 <- lm(y_trans~x_trans)
summary(modelo2)
'Pelo summary(modelo2) do modelo2 temos que b0*=-0.1022
e b1*=0.3662. Lembando que b0*=ln(a) <=> a = exp(-0.1022) = 0.902849
e que b1*=b, então b=0.3662'

#2 a)
'Para se obter o coeficiente de correlação amostral entre as variáveis,
basta calcular a raiz quadrada de R2, pois o modelo é de regressão simples
Então, |cor(x,y)|=sqrt(0.6080) = 0.7797435. O sinal dessa correlação obtemos pela es
timativa de b1, como ele é negativo, temos que a correlção será de -0.7797435'
#b)
'Vemos que o R2 é 0.6080, valor este que é relativamente baixo, 
ou seja, o modelo não está bem ajustado, pois a variável temperatura
explica pouco mais de 60% da variação total de y.
O valor da Estatística F é obtido por MSR/MSE = SSR/(SSE/(n-2)).
Forma alternativa é: (n-2)*R2/(1-R2^2) = 83.7551'
(54)*0.6080/(1-0.6080)
'Para testar o ajuste do modelo pelo teste F, temos que ver se F pertence a RC
Considerando nível de 5% temos que f_(1-alpha),(1,n-2). Como F>f_(1-alpha),(1,n-2)
rejeitamos h0 ao nível de 5%, ou seja, temos evidência significativa que o modelo não é inutil'
qf(.95,1,54)
#c)
'Uma estimativa para a variância dos erros aleatórios do modelo é 
o quadrado de Residual standard error= (3.369)^2 = 11.35016'

#d) IC 90% para b0: 80.88771 102.16929
b0<-91.5285
qmre<-11.35016
sigmahatbo<-sqrt(qmre*(1/56+(22.96^2)/(55*2.7046)))
91.5285+c(-1,1)*qt(0.95,54)*sigmahatbo

#e)           SS                 gl        MS          F0         pvalor
'Regressão  950.6338             1       950.6338   83.7551       1.430966e-12
Resíduos    612.9086            54       11.35016
Total       1563.542            55'
1-pf(83.7551,1,54)
#f)26.87370 40.38258
'Para calcular o IC para um observação individual, fazemos assim:'
mu<-91.5285-2.5284*22.9
mu+c(-1,1)*qt(.975,54)*sqrt(qmre*(1+1/56*(22.9-22.96)^2/(55*2.7046)))

#Análise dos Resíduos
#Verificar normalidade dos erros
qqnorm(rstandard(modelo1))
abline(0,1)
shapiro.test(rstandard(modelo1))

#Média Nula e variância constante dos erros
plot(fitted(modelo1),rstandard(modelo1))
abline(h=0)

#Independência dos erros
plot(rstandard(modelo1))
