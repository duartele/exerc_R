set.seed(17)
#setwd("C:/Users/lndox/OneDrive/Documentos/Repositorios/exerc_R")
df<-read.csv2("titanic.csv",colClasses = c("factor", "factor", "factor", "numeric",
               "integer", "integer", "numeric", "factor",
               "factor"))

df<-subset(df, select=-death)

calc_faixa_etaria<-function(idade=NA){
  if(is.na(idade)){
    faixa_etaria = "3.Sem idade"
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

calc_parch_agg<-function(parch=NA){
  if(parch<1) return("1.Sem filhos")
  else if(parch<4) return("2.Tem até 3 filhos")
  else if(parch>=4) return("3.Tem 4 ou mais filhos")
}

df["parch_agg"]<-sapply(df$parch,FUN=calc_parch_agg)

calc_sibsp_agg<-function(sibsp=NA){
  if(sibsp<1) return("1.Sem irmãos")
  else if(sibsp<3) return("2.Tem 1 ou 2 irmãos")
  else return("3.Tem 3 ou mais irmãos")
}

df["sibsp_agg"]<-sapply(df$sibsp,FUN=calc_sibsp_agg)

df["tamanho_familia"]<-df$sibsp+df$parch+1

sozinho<-function(fam=1) ifelse(fam==1, 1, 0)

df["sozinho"]<-sapply(df$tamanho_familia,FUN=sozinho)

df["age_2"] = df$age
df$age_2[is.na(df$age_2)]<-mean(df$age_2,na.rm = TRUE)

df$faixa_etaria<-factor(df$faixa_etaria)
df$sozinho<-factor(df$sozinho)
df$sibsp_agg<-factor(df$sibsp_agg)
df$parch_agg<-factor(df$parch_agg)
df$fare[is.na(df$fare)]<-mean(df$fare,na.rm = TRUE)
df$embarked[df$embarked==""]<-"S" #classe mais frequente
df$embarked<-factor(df$embarked) #Quando importei, ficou com 
sample<-sort(sample(nrow(df),nrow(df)*0.7))
train<-df[sample,]
val<-df[-sample,]

#Árvore de Decisão Simples
library(rpart)
library(rpart.plot)
tree<-rpart(survived ~., data=train)
prp(tree)
pred<-predict(tree, val, type="class")
acc<-sum(table(pred,val$survived)[1,1],table(pred,val$survived)[2,2])/length(pred)
print(acc)
table(pred,real=val$survived)

fare_agg<-rpart(survived ~fare, data=df)
prp(fare_agg)

calc_fare_agg<-function(fare=NA){
  if(fare<15) return("1.Fare_1")
  else if(fare<35) return("2.Fare_2")
  else if(fare<52) return("3.Fare_3")
  else if(fare<76) return("4.Fare_4")
  else return("5.Fare_5")
}
df["fare_agg"]<-sapply(df$fare,FUN=calc_fare_agg)
df$fare_agg<-factor(df$fare_agg)

max_fare<-function(fare=0) ifelse(fare<76, fare, 76)
df["fare_2"]<-sapply(df$fare,FUN=max_fare)

#val["p_arvore"]<-pred_tree - SERA USADA NO FUTURO!!!!
pred_tree<-predict(tree, val, type="class")
acc<-sum(table(pred_tree,val$survived)[1,1],table(pred_tree,val$survived)[2,2])/length(pred_tree)
print(acc)

#Regressão Logística
df_reg<-subset(df, select=-c(fare_2, age, fare_agg, sozinho, tamanho_familia,
                     sibsp_agg, parch_agg, faixa_etaria))
train<-df_reg[sample,]
val<-df_reg[-sample,]

df["embarked_agg"]<-df$embarked
df$embarked_agg[df$embarked_agg=="Q"]<-"S"
df$embarked_agg<-factor(df$embarked_agg)

df_reg<-subset(df, select=-c(fare_2, age, fare_agg, sozinho, tamanho_familia,
                             sibsp_agg, parch_agg, faixa_etaria))
train<-df_reg[sample,]
val<-df_reg[-sample,]



train<-subset(train, select=-c(fare,parch,embarked))
mf<-glm(formula = survived ~ ., data = train, family="binomial")
summary(mf)

res<-predict(mf,val,type="response")
confmatrix<-table(verd=val$survived,pred=res>0.5)
acc<-(confmatrix[1,1]+confmatrix[2,2])/sum(confmatrix)
print(acc)

#Tentar tunar o modelo!!!
train<-df[sample,]
val<-df[-sample,]
train<-subset(train, select=-c(age,fare,parch,embarked, tamanho_familia,
                               fare_agg,fare_2, faixa_etaria, parch_agg, sibsp_agg))
mf<-glm(formula = survived ~ ., data = train, family="binomial")
summary(mf)
res<-predict(mf,val,type="response")
confmatrix<-table(pred=res>0.5, verd=val$survived)
acc<-(confmatrix[1,1]+confmatrix[2,2])/sum(confmatrix)
print(acc)
confmatrix
#val["p_tree"]<-pred - SERA USADA NO FUTURO!!!!

#KNN
library(caret)
target<-subset(df, select=survived)
df_knn<-subset(df, select=-c(fare_2, age, fare_agg, sozinho, tamanho_familia,
                             sibsp_agg, parch_agg, faixa_etaria,survived,embarked_agg))
#df$pclass<-as.integer(df$pclass)#Não quero criar dummies nesta variável porque a ordem importa: classe1>classe2>classe3
dummy<-dummyVars(" ~. ", data=df_knn)
df_knn<-data.frame(predict(dummy,newdata=df_knn))

normalize<-function(x) return((x-min(x))/(max(x)-min(x)))
#Aplicando a normalização na base:
df_norm<-as.data.frame(lapply(df_knn, normalize))
X_train<-df_norm[sample,] #seed(17) já foi fixado
X_val<-df_norm[-sample,]
y_train<-target[sample,]
y_val<-target[-sample,]

require(class)
m1<-knn(train = X_train, test = X_val, cl=y_train, k=3)
acc<-sum(table(m1,y_val)[1,1],table(m1,y_val)[2,2])/length(y_val)
print(acc)

l_acc<-c()#inicializando o vetor de acc
for( i in 1:41){
  m1<-knn(train = X_train, test = X_val, cl=y_train, k=i)
  acc<-sum(table(m1,y_val)[1,1],table(m1,y_val)[2,2])/length(y_val)
  l_acc<-append(l_acc,acc)
}

cat('Para k = ',which.max(l_acc)," temos acc = ",max(l_acc))






m1<-glm(formula = survived ~ ., data = train, family="binomial")
summary(m1)

res<-predict(m1,val,type="response")
confmatrix<-table(verd=val$survived,pred=res>0.5)
acc<-(confmatrix[1,1]+confmatrix[2,2])/sum(confmatrix)
print(acc)

table(val$faixa_etaria)




























sozinho<-function(a){
  if(a>0){
    sozinho = 0
    return(sozinho)
  }else{
    sozinho = 1
    return(sozinho)
  }
}
df["sozinho"]<-df$sibsp+df$parch
df["sozinho"]<-sapply(df$sozinho,FUN=sozinho)

df$faixa_etaria<-factor(df$faixa_etaria)
df$sozinho<-factor(df$sozinho)

cor(df$fare,df$pclass)

quant<-data.frame(subset(df, select = c(age, parch, sibsp,fare)))
quant<-quant[complete.cases(quant),]
quant.cor<-cor(quant)
quant.cor
library(corrplot)
corrplot(quant.cor)

windows()
layout(matrix(c(1,2,3,4),ncol=2))
plot(quant$age~quant$fare, xlab="Tarifa", ylab="Idade", main="")
abline(lm(quant$age~quant$fare), col="red", lty=1, lwd=2)
plot(quant$sibsp~quant$parch, xlab="Número filhos", ylab="Número de irmãos", main="")
abline(lm(quant$sibsp~quant$parch), col="red", lty=1, lwd=2)
plot(quant$age~quant$parch, xlab="Número filhos", ylab="Idade", main="")
abline(lm(quant$age~quant$parch), col="red", lty=1, lwd=2)
plot(quant$sibsp~quant$fare, xlab="Tarifa", ylab="Número irmãos", main="")
abline(lm(quant$sibsp~quant$fare), col="red", lty=1, lwd=2)


#Tratando valores Faltantes
df["age_2"] = df$age
df$age_2[is.na(df$age_2)]<-mean(df$age_2,na.rm = TRUE)
#df$age[is.na(df$age)]<-mean(df$age,na.rm = TRUE)
df$fare[is.na(df$fare)]<-mean(df$fare,na.rm = TRUE)
df$embarked[df$embarked==""]<-"S" 
df$embarked<-factor(df$embarked)

library(caret)
#subset(df, select=-c(survived, pclass )
df$survived<-as.integer(df$survived)#survived e pclass n?o podem ser categorizadas assim
df$pclass<-as.integer(df$pclass)#elas seriam transformadas em num?rica de qualquer forma pela fun??o dummyVars
dummy<-dummyVars(" ~. ", data=df) 
df<-data.frame(predict(dummy,newdata=df))
str(df)

sample<-sort(sample(nrow(df),nrow(df)*0.7))
train<-df[sample,]
val<-df[-sample,]

#modelo 1
train<-subset(train, select=-age)
train<-subset(train, select=-fare)
#train<-subset(train, select=-parch)
#train<-subset(train, select=-c(age, sibsp, parch))
m1<-glm(formula = survived ~ ., data = train, family="binomial")
summary(m1)

train<-subset(train, select=-c(sozinho))
m2<-glm(formula = survived ~ ., data = train, family="binomial")
summary(m2)

df["embarked_agg"]<-df$embarked
df$embarked_agg[df$embarked_agg=="Q"]<-"S"
df$embarked_agg<-factor(df$embarked_agg)
train<-df[sample,]
val<-df[-sample,]
train<-subset(train, select=-c(age,fare,parch,embarked))
mf<-glm(formula = survived ~ ., data = train, family="binomial")
summary(mf)

train<-subset(train, select=-c(sozinho))
m2<-glm(formula = survived ~ ., data = train, family="binomial")
summary(m2)

res<-predict(mf,val,type="response")
confmatrix<-table(verd=val$survived,pred=res>0.5)
acc<-(confmatrix[1,1]+confmatrix[2,2])/sum(confmatrix)

predicted.data<-data.frame(prob=regl$fitted.values, survived=df$survived)
predicted.data<-predicted.data[order(predicted.data$prob,decreasing = FALSE),]
predicted.data$rank<-1:nrow(predicted.data)

head(predicted.data)

predict.glm(regl, newdata = df, type="response")
?predict.glm
summary(regl)

fitted(regl)
df$survived<-factor(df$survived)


#KNN
target<-subset(df, select=survived)
df<-subset(df, select=-c(survived,embarked_agg))
df$pclass<-as.integer(df$pclass)
library(caret)
dummy<-dummyVars(" ~. ", data=df)
df<-data.frame(predict(dummy,newdata=df))
set.seed(17) #Para manter a mesma sequ?ncia pseudo-aleat?ria
sample<-sort(sample(nrow(df),nrow(df)*0.7))
X_train<-df[sample,]
X_val<-df[-sample,]
y_train<-target[sample,]
y_val<-target[-sample,]
str(df)
table(df$pclass)

summary(df)
df$embarked[df$embarked==""]<-"S"
which(is.na(df$age))
sum(is.na(df$fare))
sample<-sort(sample(nrow(df),nrow(df)*0.8))
train<-df[sample,]
val<-df[-sample,]

df$fare<-ifelse(df$fare>50,50,df$fare)


for (val in df$fare){
  if(val>300){
  df$fare <- 300
}
} 

normalize<-function(x){
  return((x-min(x))/(max(x)-min(x)))
}
df_norm<-as.data.frame(lapply(df, normalize))

require(class)
mi<-knn(train = df, test = df_, cl=target, k=13)
table(target,m1)
summary(df_norm)

normalize<-function(x) return((x-min(x))/(max(x)-min(x)))
#Aplicando a normalização na base:
df_norm<-as.data.frame(lapply(df, normalize))
X_train<-df_norm[sample,] #seed(17) já foi fixado
X_val<-df_norm[-sample,]
y_train<-target[sample,]
y_val<-target[-sample,]
l_acc<-c()#inicializando o vetor de acc
for( i in 1:40){
  m1<-knn(train = X_train, test = X_val, cl=y_train, k=i)
  acc<-sum(table(m1,y_val)[1,1],table(m1,y_val)[2,2])/length(y_val)
  l_acc<-append(l_acc,acc)
} 
library(FSelector)
library(rpart)
library(rpart.plot)
library(dplyr)
library(data.tree)

#Decision Tree
library(rpart)
library(rpart.plot)

train<-subset(train, select=-age)

tree<-rpart(survived ~., data=train)
prp(tree)
pred<-predict(tree, val, type="class")
table(pred,val$survived)
acc<-sum(table(pred,val$survived)[1,1],table(pred,val$survived)[2,2])/length(pred)


#tree<-rpart(survived ~., data=X_train, method="anova") - target is nummeric
pred<-predict(tree, X_val, type="class")
acc<-sum(table(pred,y_val)[1,1],table(pred,y_val)[2,2])/length(y_val)
prp(tree)

#for nummeric response
MAE<-function(actual, pred){
  mean(abs(actual-pred))
}
MAE(y_val, pred)
rpart.plot(m1, type=3, digits=3, fallen.leaves=TRUE)