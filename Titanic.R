
setwd("C:/Users/lndox/OneDrive/Documentos/Mestrado Portugal/Computacao/Trabalho")
df<-read.csv2("titanic.csv",colClasses = c("factor", "factor", "factor", "numeric",
               "integer", "integer", "numeric", "factor",
               "factor"))


calc_faixa_etaria<-function(idade=NA){
  if(is.na(idade)){
    faixa_etaria = "3.Sem idade informada"
    return(faixa_etaria)
  }else if(idade<=15){
    faixa_etaria = "1.Crianças e jovens"
    return(faixa_etaria)
  } else{
    faixa_etaria = "2.Adultos"
    return(faixa_etaria)
  }
}

df["faixa_etaria"]<-sapply(df$age,FUN=calc_faixa_etaria)

table(df$survived,df$faixa_etaria)

boxplot(df$age~df$survived)

boxplot(df$fare~df$survived, ylab = "Valor da Tarifa", xlab="Sobrevivência",main="")

barplot(table(df$embarked,df$pclass), xlab="Porto de Embarque por Classe", ylab="Frequência",
        legend.text = FALSE, beside=FALSE)

boxplot(df$fare~df$pclass, ylab = "Valor da Tarifa", xlab="Classe",main="")



df$fare<-ifelse(df$fare>50,50,df$fare)


for (val in df$fare){
  if(val>300){
  df$fare <- 300
}
} 
df$fare[df$fare>300]

df<- subset(df, select = -death)
summary(df)
?subset
barplot(table(df$survived,df$sex),beside =TRUE,legend=c("Morreu","Viveu"))
barplot(table(df$survived,df$sex))
legend("topleft",legend=c("Morreu","Viveu"))
?legend
?barplot
library(lattice)
which(is.na(df$survived))
?theme()
pie(x=table(pathscat), labels=c("<2cm","2-5 cm",">5 cm")
    , col = c("lightblue", "lightgreen", "lightgrey"))

cor(df$sibsp,df$parch)
?cor




?hist
min(df$age,na.rm=TRUE)
max(df$age)
hist(df$age, 
     breaks=c(min(df$age,na.rm=TRUE),35,60,69,max(df$age,na.rm=TRUE)),
       main="Idade", xlab="Idade",
     
     freq=TRUE,ylab="frequências",labels = FALSE, col="blue"
     )
windows()

jpeg(filename = "histograma.jpeg")
hist(df$age, 
     main="Idade", xlab="Idade",
     
     freq=TRUE,ylab="frequências",labels = FALSE, col="blue"
     )
curve(dnorm(x, mean = mean(df$age,na.rm = T), sd = sd(df$age, na.rm =T)), add = T)   
dev.off()
?dnorm()

hist(df$age, 
     main="Idade", xlab="Idade",
     
     freq=FALSE,ylab="frequências",labels = FALSE, col="blue"
)
curve(dnorm(x, mean = mean(df$age,na.rm = T), sd = sd(df$age, na.rm =T)), add = T)   
legend("bottomright",legend = c("fcom","norm"),text.col = 1:2)
locator(1)

legend(x=c(60,80),y=c(160,200),legend = c("fcom","norm"),text.col = 1:2)



?read.csv2
is.num
is.numeric(df$fare)
500/(500+809)
vivos<- df$death[df$survived==0]
table(vivos) 
table(df$pclass)/length(df$pclass)
843/(500+809)
?barplot
main="GRÁFICO"
?barplot
a<-table(df$survived,df$sex)
b<-table(df$survived)
b
names(b)<-c("sobreviveu","morreu")
a
table(df$survived)
barplot(a,legend.text = TRUE)
barplot(b)
windows()
barplot(table(df$survived,df$sex),legend.text = TRUE)

legend("bottomright", legend=c("sobreviveu", "morreu"))
legend(x=3,y=1000, legend=c("sobreviveu", "morreu"))
legend("topleft", inset = 0.50, legend=c(1, 0))


barplot(cbind(Employed, Unemployed) ~ Year, data = longley ,legend.text = TRUE)
longley
?legend

barplot(table(mtcars$gear), col = 2:4, density = 30, angle = 90)

legend("topright",
       legend = c(3, 4, 5),
       fill = 2:4,
       density = 30, # Shading lines density
       angle = 90)   # Angle of the shading lines


?plot
?legend.text
head(df$sex)
pie(df$sex)
str(df)
table(df$sex)
barplot(frequency(df$sex))
?mfrow
barplot(table(df$survived,df$pclass),legend.text = TRUE)
table(df$survived,df$pclass)/length(df$survived)
123/(123+200)
158/(158+119)
528/(528+181)
hist(table(df$survived,df$sibsp),legend.text = TRUE)
summary(df$sibsp)
hist(df$sibsp)
table(df$sibsp)/length(df$sibsp)

# Create a sample of 50 numbers which are incremented by 1.
x <- seq(0,50,by = 1)

# Create the binomial distribution.
y <- dbinom(x,50,0.2)

# Give the chart file a name.
png(file = "dbinom.png")

# Plot the graph for this sample.
plot(x,y)

pie(table(df$survived),labels = c("morreu","viveu"))
pie.table<-table(df$survived)
names(pie.table)<-c("morreu", "viveu")
pie.table
pie(pie.table)
pie(c(10,20,30,40))
pie(c(10,20,30,40),labels=c("Grupo I", "Grupo II", "Grupo III","Grupo IV"))


col.pie <- c("red", "yellow", "green", "violet")
label.pie <- c ("Grupo I", "Grupo II", "Grupo III","Grupo IV")
freq <- round(c(10,20,30,40)/ sum(c(10,20,30,40)), 3)*100
label.pie <- paste(label.pie, freq)
label.pie
label.pie <- paste(label.pie,"%",sep="")
pie(freq, col = col.pie, labels = label.pie)
windows()

tab1<-table(pathscat,status)
counts<-tab1
barplot(counts,main="tpitulo")












# Variáveis Qualitativas Ordinais
Pclass (1, 2 ou 3)
Das 1309 observações temos 323 da primeira (24,68%), 277 da segunda (21,16%) e 709 da terceira (54,16%) , sem valores faltantes, portanto. No gráfico a seguir, veremos para cada uma dessas categorias o comportamento da variável resposta.
Aqui podemos ver que as pessoas da primeira classe tiveram mais chances de sobrevivência, seguindo pela segunda classe, tendo portanto a terceira classe com as maiores taxas de morte.

# Variáveis Quantitativas Discreta
Sibsp (Número de irmãos e ou esposos)
Como podemos observar no gráfico, a maioria dos valores assume o valor 0 ou 1. Talvez podemos analisar esta variável agrupando em três categorias (0, 1 e maior que 1) - ou em duas("Não tem" ou "Tem").

#Conclusão


De acordo com o exposto, pessoas ricas e do sexo feminino tem mais chances de sobreviverem ao acidente do Titanic.

#Rascunho

+ **survived**: Sobrevivência (ou não) dos passageiros. Possíveis valores: 0 (morreu) ou 1 (sobreviveu). Classificação: Variável Resposta (de interesse). Quantidade de sobreviventes na amostra: `r table(df$survived)[2]` pessoas(`r round(table(df$survived)[2]/nrow(df)*100,2)`%);

+ **pclass**: Classe do Bilhete. Possíveis valores: 1(primeira), 2(segunda) ou 3 (terceira). Classificação: Variável Qualitativa Ordinal
Número de obs faltantes: `r  which(is.na(df$pclass));
`
+ **age**: Idade em anos. Possíveis valores: Para crianças menores de um ano, a idade pode assumir um valor contínuo (no intervalo  ]0,1[). Para mais velhos que isso, usa-se o valores discretos  {1,2,3...}. Quando tem o valor da forma xx.5 é porque a idade da pessoa foi estimada. Não obstante essas informações, podemos classificar a Variável como Quantitativa Contínua.
Número de obs faltantes: `r  which(is.na(df$age));

+ **sibsp**: Número de irmãos e/ou esposos a bordo. Possíveis valores: {0,1,2,3...}. Classificação: Variável Quantitativa Discreta.
Número de obs faltantes: `r  which(is.na(df$sibsp));

+ **parch**: Número de pais e/ou filhos a bordo. Possíveis valores: {0,1,2,3...}. Classificação: Variável Quantitativa Discreta.
Número de obs faltantes: `r  which(is.na(df$parch));

+ **fare**: Valor da tarifa paga. Possíveis valores: ]0,+???[ Classificação: Variável Quantitativa Contínua.
                                                            Número de obs faltantes: `r  which(is.na(df$fare));
                                                            
                                                            + **embarked**: Porto de Embarque. Possíveis valores: C (Cherbourg), Q (Queenstown) ou S (Southampton). Classificação: Variável Qualitativa Nominal.
                                                            Número de obs faltantes: `r  which(is.na(df$embarked));
  
                                                                                                                      
                                                            + **death**: Morte (ou não) do passageiro. Possíveis valores: 0 (não morreu) ou 1(morreu). Esta variável será descartada porque foi obtida pela variável **survived**.
                                                            
                                                            
                                                            Vale dizer, seria mais interessante se a variável sibsp fosse dividida em duas (uma só para irmãos e outra só para esposas). Nesse sentido, parch também poderia estar separada
                                                            
                                                            hist(df$parch)
                                                            table(df$parch)
df$embarked[df$embarked != "S"  "Q", "S")]                                                            
split(df, df$embarked)
df[169,]
summary(df)
?drop
?remove


cor(df$sibsp,df$parch)

plot(df$sibsp~df$parch)
abline(lm(df$sibsp~df$parch), col="red", lty=1, lwd=2)

plot(df$sibsp~df$parch, xlab="Número de pais (ou filhos)", ylab="Número de irmãos (ou esposas)", main="")
abline(lm(df$sibsp~df$parch), col="red", lty=1, lwd=2)


boxplot(df$parch~df$faixa_etaria, ylab = "Némero de pais (ou filhos)", xlab="Faixa etária",main="")

sozinho<-function(a){
  
  if(a>0){
    sozinho = 0
    return(sozinho)
  }else{
    sozinho = 1
    return(sozinho)
  }
}
sozinho(0,0)
sozinho(1,0)
sozinho(NA,1)

df["sozinho"]<-df$sibsp+df$parch
df["sozinho"]<-sapply(df$sozinho,FUN=sozinho)

lot(df$sibsp~df$parch, col=df$sozinho)
table(df$sozinho)
warnings()
calc_faixa_etaria<-function(idade=NA){
  if(is.na(idade)){
    faixa_etaria = "3.Sem idade informada"
    return(faixa_etaria)
  }else if(idade<=15){
    faixa_etaria = "1.Crianças e jovens"
    return(faixa_etaria)
  } else{
    faixa_etaria = "2.Adultos"
    return(faixa_etaria)
  }
}

df["faixa_etaria"]<-sapply(df$age,FUN=calc_faixa_etaria)
table(df$faixa_etaria)

do_mau<-function(sibsp=0, parch=0){
  if(is.na(sibsp)){sibsp=0}
  if(is.na(parch)){parch=0}
  if(sibsp+parch>0){
    sozinho = 0
    return(sozinho)
  }else{
    sozinho = 1
    return(sozinho)
  }
}

df["sozinho"]<-sapply(df$sibsp+df$parch,FUN=do_mau)

p_est<- table(df$survived)[2]/nrow(df)

sd <- sqrt(p_est*(1-p_est)/nrow(df))

LI <- p_est - qnorm(0.975)*sd
LS <- p_est + qnorm(0.975)*sd

table(df$survived,df$sex)[2,2]/table(df$sex)[2]
table(df$sex)[2]

sd <- sqrt(p_est*(1-p_est)/nrow(df))

LI <- p_est - qnorm(0.975)*sd
LS <- p_est + qnorm(0.975)*sd

p_xx<-table(df$survived,df$sex)[2,1]/table(df$sex)[1]
p_xy<-table(df$survived,df$sex)[2,2]/table(df$sex)[2]  

se <- sqrt(p_xx*(1-p_xx)/table(df$sex)[1] + p_xy*(1-p_xy)/table(df$sex)[1]) 
p_xx-p_xy+c(-1,1)*qnorm(0.975)*se
