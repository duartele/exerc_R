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

#Teste Qui-quadrado
tab<-table(b.peso=birthwt$low, fumador = birthwt$smoke)
tab
'Observado   Fumador
Baixo peso    0     1
0             86    44
1             29    30

             
Esperado         0               1
0         130*115/189        130*74/189
1         59*115/189          59*74/189

(Obs-E)^2/eij         0               1
0                   0.6018                    
1                   

H0: são independentes = não existe associação entre o 
estatuto fumador e o peso ao nascimento
ET = 49242 ~ X^2,1
p-valor = 1-pchisq(49242,1) = 0.02649
Portanto, rejeita H0)'
chisq.test(tab, correct = FALSE)

tab2<-table(b.peso=birthwt$low, raça = birthwt$race)
chisq.test(tab2, correct = FALSE)

#exer 15 ft4
tab3<-matrix(c(945,2555,1015,2485),2,byrow = TRUE)
ob1<-chisq.test(tab3, correct = FALSE)$observed
ob2<-chisq.test(tab3, correct = FALSE)$expected
residuos<-(ob1-ob2)^2/ob2
sum(residuos)
chisq.test(tab3, correct = FALSE)

#teste exato de Fisher #tabelas 2x2 e com poucas observações
fisher.test()

#teste de mcnemar para grupos emparelhados
tab4<-matrix(c(50,6,8,80),2,byrow = TRUE)
mcnemar.test(tab4)

tab5<-table(raça=birthwt$race, b.peso=birthwt$low)

#H0: p(baixo peso|fumador) vs H1: p(baixo peso|não fumador)
chisq.test(tab, correct = FALSE)
prop.test(x=c(29,30),n=c(115,74))

#H0 x~poi(2)
'k      x p(x=x0) (supondo ho)   esperado (n=100)
0   16            0.1353                13.53
1   12
2   20
3   13
4   8
>5   11 #Temos que agrupar as células

ET = S(nk-ek)^2/ek, k=número de níveis
ET=11.02 ~X^2,6, pvalue = 0.068
'

#Exemplo teste não paramétrico pg 35
freq<-c(16,32,20,13,8,6,5)
prob<-c(depois(0:5,2),1-sum(depois(0:5,2)))
esp<-100*prob

#temos duas categorias com valor esperado <5
#20% e 7 é 1,4
freq<-c(16,32,20,13,8,11)
prob<-c(depois(0:4,2),1-sum(depois(0:4,2)))
esp<-100*prob
res<-(freq-esp)^2/esp
ET<-sum(res)
1-pchisq(ET,6)

#exercicios folha trabalho
'Exercício 1: 

a) Verificar i) independência; ii) normalidade;
iii) inexistencia de outliers; iv) homogeneidade das variancias'
a1<-c(6.2,9.3,6.8,6.1,6.7,7.5)
a2<-c(7.5,8.2,8.5,8.2,7.0,9.3)
a3<-c(5.8,6.4,5.6,7.1,3.0,3.5)

boxplot(a1,a2,a3)
val<-c(a1,a2,a3)
grupo<-c(rep(1,6),rep(2,6),rep(3,6))
bartlett.test(val, grupo)
#h0: var1=var2=var3
#Como p-valor 0.33 não rejeita o pressuposto de homogeneidade das variâncias

#Para normalidade
shapiro.test(a1)
shapiro.test(a2)
shapiro.test(a3)

#Verificamos todos os pressupostos
#b
#ho: mu_1=mu_2=m_3 vs h1: existe ao menos uma diferença
fit<-aov(val~factor(grupo))
summary(fit)
#ET~F_2,15, ET=12.832/1.569 = 8.176
pvalue = 1-pf(8.176,2,15)
#Conclusão, para um nível de significancia de 5%, rejeitamos h0
# e concluímos que existem diferenças entre os grupos

#c) Só faz sentido se rejeitarmos h0
TukeyHSD(fit)
'Observando os intervalos de confiança, vemos que há diferença entre 
3 e 2. Porem, podemos ver que entre 3 e 1 quase deu estat. diferentes!!
 às vezes, com uma amostra um pouco maior seria esperado dar que 3 e 1 são estat. diferentes


Exercício 2
              SS      gl      MS      F     p-valor
Entre grupos  1446    2       723   7.876   0.0046
Nos grupos    1377    15      91.8
Total

h0:
Decisão:'
