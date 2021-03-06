library(MASS)
?birthwt
a<-sum(birthwt$bwt)
b<-sum(birthwt$bwt^2)
m<-a/189
var<-(b-189*(m^2))/188
t1<-qt(0.975,188)
m + c(-1,1)*t1*sqrt((var)/189)      

?t.test
t.test(x=birthwt$bwt, conf.level = 0.95)

#IC para vari�ncia - primeiro verificar se � normal a distr - Shapiro
#peso no processo de empacotamento de 1kg de arroz
peso<-c(1.03,1.1,0.99,0.98,1.05,1.02,0.92,1.08)
shapiro.test(peso) #p-valor alto - ent�o �!
s2_peso<-var(peso)
qchisq(0.025,df=7) #X2_alpha/2 - LS
qchisq(0.975,df=7) #X2_1-alpha/2 - LI

IC <- c(7*s2_peso/qchisq(0.975,df=7), 7*s2_peso/qchisq(0.025,df=7))

#Teste de Normalidade
#Teste de Shapiro-Wilk -> shapiro.test
#biblioteca nortest -> lillie.test

shapiro.test(birthwt$bwt[birthwt$smoke == 0]) #OK!
shapiro.test(birthwt$bwt[birthwt$smoke == 1]) #OK!

library(nortest)
lillie.test(birthwt$bwt[birthwt$smoke == 0])
lillie.test(birthwt$bwt[birthwt$smoke == 1])

#IC a 95% para a M_NF e M_F
t.test(x=birthwt$bwt[birthwt$smoke == 0],
       y=birthwt$bwt[birthwt$smoke == 1],
       paired = FALSE,
       var.equal = TRUE,
       conf.level = 0.95)
# IC(72.75612;494.79735) - Implica que a diferen�a � significativa
#Al�m disso, indica que o peso dos beb�s provenientes de m�es n�o fumadora � maior

#IC para raz�o de vari�ncias
?var.test
var.test(x=birthwt$bwt[birthwt$smoke == 0],
         y=birthwt$bwt[birthwt$smoke == 1],
          conf.level = 0.95)
#IC = (0.8486407 ; 1.9589574) - inclui o 1 significa que as vari�ncias podem ser iguais
#Portanto, o teste t que fizemos est� v�lido!!!!

#Uso de f�rmula nesses testes - fica mais f�cil de escrever
t.test(birthwt$bwt~birthwt$smoke, conf.level=0.95)
var.test(birthwt$bwt~birthwt$smoke, conf.level=0.95)

n<-c(8,11,12,14,20,43,111)
e<-c(35,56,83,92,128,150,176,208)

#Primeiro verificar se s�o normais - amostra pequena, ent�o pode usar shapiro
shapiro.test(n) #teste negativo para normalidade - ent�o vamos continuar as contas s� para efeitos did�ticos
shapiro.test(e)
#Segundo, usar var.test para ver se as vari�ncias sao iguais
var.test(x=n,y=e,paired=FALSE,conf.level = 0.95) #Cont�m o valor 1
t.test(x=n,y=e,paired=FALSE,conf.level = 0.95)

#Ex slide 30
antes<-c(209, 210, 205, 198, 216, 217, 238, 240, 222)
depois<-c(199, 207, 189, 209, 217, 202, 211, 223, 201)

t.test(x=antes, y=depois, paired=TRUE,conf.level=0.95)

#D=antes-depois
D<-antes-depois
#teste de normalidade
shapiro.test(D) #Ok, � normal
#Slide 29 est� com a f�rmula errada: faltou dividir por sqrt(n)
mean(D)+c(-1,1)*qt(0.975,8)*sd(D)/sqrt(9)

'
slide 37
n=100
p^=0.2
alpha=0.05
qual deveria ser o tamanho da amostra de modo que o erro seja
inferior a 0.05?

1.96*sqrt(0.2*0.8/n)<=0.05
<=> n>=0.16/(0.05/1.96)^2
Portanto, n= 246

Existe uma fun��o para propoor��es: binom.test
Este seria o resultado exato, enquanto que o c�lculo que fazemos �
uma aproxima��o para a normal.

?prop.test
'
binom.test(x=59,n=189) #peso das crian�as

#Agora vamos fazer um intervalo de confian�a para a diferen�a na birthwt
table(baixo.peso=birthwt$low,fumador=birthwt$smoke)
pf<-30/74
pnf<-29/115
se <- sqrt(pf*(1-pf)/74 + pnf*(1-pnf)/115) 
pf-pnf+c(-1,1)*qnorm(0.975)*se

#fazendo por prop.test
prop.test(x=c(30,29),n=c(74,115))

#Exemplo Framingham 
pf<-81/(81+663)
pnf<-298/(298+2757)
se<-sqrt(pnf*(1-pnf)/(298+2757) + pf*(1-pf)/(81+663))

pf-pnf + c(-1,1)*qnorm(0.975)*se

#Ou, usando o prop.test
prop.test(x=c(81,298),n=c(81+663,298+2757))

#-----------------------------------
#Teste de Hip�teses
library(MASS)
ET<- (mean(birthwt$bwt) - 3000)/(sd(birthwt$bwt)/sqrt(189))

#using t.test
t.test(x=birthwt$bwt, alternative="g", mu=3000, conf.level = 0.95)
#Conclus�o, n�o rejeitamos H0

#exer 10 ficha 4
alt<-c(41, 50, 52, 49, 54, 50, 49, 47, 52, 49, 50, 52, 50, 47, 49, 51, 46, 50, 49, 50) 
mean(alt)
sd(alt)
#Verificar a normalidade dos dados
shapiro.test(alt) #N�o s�o normais

#exer 3 ficha 4
meninos<-c(2968, 2795, 3163, 2925, 2625, 2847, 3292, 3473, 2628, 3176, 3421, 2975)
meninas<-c(3317, 2729, 2935, 2754, 3210, 2817, 3126, 2539, 2412, 2991, 2875, 3231)
#Verificar a normalidade dos dados
shapiro.test(meninos)
shapiro.test(meninas) #ambos ok
#Verificar se as vari�ncias s�o iguais
'H0: Var_meninos = Var_meninas
H1: S�o diferentes
Raz�o de vari�ncias - distribui��o F11,11
ET (Estat�stica de teste)
RC: 
Decis�o: manter H0 para o n�vel de signific�ncia de 5%'
et<-var(meninos)/var(meninss)
qf(0.025,11,11)
qf(0.975,11,11)
var.test(x=meninos,y=meninas)

#Agora sim, podemos come�ar
num<- mean(meninos) - mean(meninas)
#Dividir em v1 e v2 os denominadores pra diminuir chance de erro
v1<-sqrt((11*var(meninos)+11*var(meninas))/22)
v2<-sqrt(1/12+1/12)
et<-num/(v1*v2)
#H0 = mu_meninos=mu_meninas vs H1 = mu_meninos!=mu_meninas
qt(0.025,22)
qt(0.975,22)
'rc:(-Inf, -2.0739) U (2.0739, Inf)
decis�o: mantemos H0 para o n�vel de 5%
p.valor=2*p(T>|ET|)
'
2*(1-pt(0.97747,22))

t.test(meninos,meninas,alternative = "t",mu=0, paired = FALSE,
       var.equal = TRUE)
