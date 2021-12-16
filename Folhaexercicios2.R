setwd("C:/Users/lndox/OneDrive/Documentos/Mestrado Portugal/Modelos_Lineares/Exercicios")
ex1<-read.table("p027.txt", TRUE, colClasses = c("numeric", "numeric"))

#a)
plot(ex1$Units,ex1$Minutes)
#b)
cor(ex1$Minutes, ex1$Units)
#c)
ex1.lm<-lm(Minutes~Units, data=ex1)
#d)
predict(ex1.lm,newdata=data.frame(Units=c(4)))
y<-4.162+15.509*4 #y=66.198
#e)
confint(ex1.lm)
#declive beta1. IC: (14,4085;16.6090)
#f)confint intercept beta0 IC: (-3.1485;11.4718)
#g)para alpha=5, testar H0:b0=0
summary(ex1.lm)
#A estatística t é 1.24 e o p-valor é 0.239 > alpha.
#Então, não rejeitamos H0.

#ex 1 pag 2
#a)
x<-c(1,7,13,20,27,34,62)
y<-c(5,10,12,29,36,83,102)
#b)
plot(x,y)
#c)
ex2.lm<-lm(y~x)
summary(ex2.lm)
'Por b1, temos que o comprimento da raiz cresce em média 1.77 mm.
Por b0, temos que no dia zero o comprimento da raiz seria de -1.93, sem sentido
biológico. Não rara são as vezes que b0 carece de interpretação.
'
#d) sb0 = 7.9483 e sb1 = 0.2639
s2b0 <- 7.9483^2 #63.17547
s2b1 <- 0.2639^2 #0.06964321
#e)
rse2 <- 13.21^2 #174.5041
#f)
confint(ex2.lm)
'IC de 95% para b0: (-22.367847;18.495555)
IC de 95% para b1: (1.093188;2.450142)
#g)'
summary(ex2.lm)
#vemos que p-valor = 0.81722 > 0.01, então não rejeitamos h0
#h) Se H0: beta1 = 0 não for rejeitada a alpha=5%, podemos afirmar que
#não existe relação linear significativa entre x e y
#Como p-value: 0.001111, rejeitamos H0 com significância de 5%, então podemos
#concluir que há associação entre as variáveis
#i)Pelo summary temos que F-statistic: 45.06 on 1 and 5 DF
#Se F>f(alpha,n-2) rejeitamos h0(unilateral).
1-pf(45.06,1,5) #pvalue
qf(.95,1,5) #Falpha,1,5 Como Fcalc>Falpha, rejeita-se H0 a 5%
#j H0:b1=2 vs H1:b1!=2 Esse precisa fazer na mão
'b1hat = 1.7717
'
ET = (1.7717-2)/0.2639 #-0.8651004 ~ tn-2
RC = qt(.975,5) #2.570582
#Como abs(-0.8651004)<2.570582, não rejeitamos H0 com alpha a 5%, ou seja, temos evidências
#de que b1 não é 2
#k) Observando o summary, vemos que não podemos rejeitar a hipótese de que b0=0
'Então, seria melhor construir uma reta sem b0.
Com exceção deste fato, temos um bom ajuste da reta, pois R^2 é 0.9
e R^2 é igual a correlação de x e y ao quadrado (no caso de ser regressão simples)'
summary(ex2.lm)
#l) SST = (n-1)*S²y
SST = 6*var(y) #8737.714
#m
anova(ex2.lm)
SSE = 872.8
#n
y2<-1/y
modelo2<-lm(y2~x)
summary(modelo2)
1/y = 0.1276574 -0.0025985

#Ex2 pg 3
((51.93-116.826087)/3)*9.5+51.93
12.5-9.5
#a) Yhati = 153.5743 - 21.63203*Xi
SST<-22*6071.882798 #133581.4
SSR<-124742.0703
SSE <- SST - SSR #8839.351
R2<-SSR/SST #0.933828
corxy<-sqrt(R2)
sy=sqrt(6071.882798)
sx=-0.9663478*sy/(-21.63203)
x1<-12.50
y1<-153.5743-21.63203*x1
sx^2*(-21.63203)

#3)
x<-c(5,10,15,20,25)
y<-c(20,17,13,11,9)
plot(x,y)
#a)
modelo3<-lm(y~x)
summary(modelo3)
#yhat<-22.40000-0.56000*x
#b)A cada aumento de 1°C diminui em média 0.56 o encargo com eletricidade
#c)o R2 deu um valor elevado (0.98) e o gráfico de dispersão x,y está coerente
#com os paraâmetros estimados, então o ajuste está bom
#d)
predict(modelo3, data.frame(x=c(17)),interval = "conf")
#IC 95% para E[Y|x=15] é (11.79984,13.96016)
#e)
R2<-0.98
corxy<-sqrt(R2) #-0.9899495, negativo por causa do b1
#Vendo o gráfico e de posse desse valor, temos que há uma forte
#correlação linear entre x e y
#f)
confint(modelo3)
#IC com 95% de confiança para b1 é IC=(-0.7069909,-0.4130091)
#g)IC com 95% de confiança para b0 é IC=(19.9624317,24.8375683)
#4)
x<-c(4.7,5,5.2,5.2,6.1,4.7,5.9,5.2,5.3)
y<-c(3,3,4,5,10,2,9,3,7)
#a
modelo4<-lm(y~x)
summary(modelo4)
yhat<--24.7238+5.6768*x
#b
confint(modelo4)
#IC 95% para beta0 (-34.703116 -14.744445)
#IC 95% para beta1 (3.784934,7.568724)
#c) Como p-valor de b1 é > alpha=0.05, rejeitamos h0
#d)
fitted(modelo4)
#e)
plot(fitted(modelo4),y)
abline(0,1)
#f)
predict(modelo4,data.frame(x=c(6)),interval = "conf")
#E[Y|x=6] = 9.337195 e IC de 95% para E[Y|x=6] é (7.6915,10.98289)
#g)
predict(modelo4,data.frame(x=c(6)),interval = "pred")
# Yhat = 9.337195, mas o IC fica maior: (6.299016,12.37537)
#h)i) R2=0.8779, então 87,79% da variação de Y é explicada pela reta de regressão
#ii)
anova(modelo4)
#De acordo com a tabela ANOVA, a prob do modelo ser inutil é de 
#p-valor=0.0001945, então o modelo é significativamente útil

#5 a)
b1num<-88.618-(37.36*47.42)/20
b1denum<-69.8978-(37.36*37.36)/20
b1=b1num/b1denum
b0=47.42/20-b1*37.36/20
b0

#b)
rnum<-b1num
rdenum<-sqrt(b1denum)*sqrt(112.4638-(47.42*47.42)/20)
R2<-(rnum/rdenum)^2
#c)
SST <- 19*(112.4638-(47.42)^2/20)
SSR <- R2*SST
SSE <- SST-SSR
sigma2hat <- SSE/18
#d) Testar H0
T <- b1/sqrt((sigma2hat/b1denum))
t <- qt(.975,18)
pvalor<-2*(1-pt(3.605,18))
#Como T>t, não rejeitamos h0 ao nível de 5%
#e)
yhat <- b0 + b1*2 #E[Y|x=2] 2.416207
#f)
#IC de 99% para E[Y|x=2] é (2.363706, 2.468709)
IC <- yhat+c(-1,1)*pt(.975,18)*sqrt(sigma2hat*(1/20+(37.36/20-2)^2/(b1denum))) 
#g)
'ANOVA    SS            gl    MS            F0          pvalor
Regressao 0.2436271     1   0.2436271       12.71124    0.002211519
Erros     0.3449929     18  0.01916627
Total     0.58862       19            '
p.valor <- 1-pf(12.71124,1,18)

#5 a) r = -0.78, pois b1<0
corxy <- sqrt(0.6084)
#b)
'F0 = MSR/MSE = SSR/(SSE/n) = (SST-SSE)/(SSE/n) = n*(SST-SSE)/SSE =
= n*(SST/SSE - 1)
Nosso caso é n=48 e SST/SSE = 2.553626 Prova:
R2 = SSR/SST = (SST-SSE)/SST = 1 - SSE/SST <=> SSE/SST = 1 - R2
<=> SSE/SST = 1 - 0.6084 = 0.3916 <=> SST/SSE = 1/0.3916 = 2.553626'
F0 <- 48*(1.553626)
pvalor <- 1-pf(F0,1,48) #p é 0
#Decisão, como alpha=5% > pvalor, rejeitamos H0 ao nível alpha
