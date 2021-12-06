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
pred_tree<-predict(tree, val, type="class")
acc<-sum(table(pred_tree,val$survived)[1,1],table(pred_tree,val$survived)[2,2])/length(pred_tree)
print(acc)
#table(pred_tree,real=val$survived)

#fare_agg<-rpart(survived ~fare, data=df)
#prp(fare_agg)

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

#pred_tree<-predict(tree, val, type="class")
#acc<-sum(table(pred_tree,val$survived)[1,1],table(pred_tree,val$survived)[2,2])/length(pred_tree)
#print(acc)

#Regressão Logística
df["embarked_agg"]<-df$embarked
df$embarked_agg[df$embarked_agg=="Q"]<-"S"
df$embarked_agg<-factor(df$embarked_agg)

train<-df[sample,]
val<-df[-sample,]
train<-subset(train, select=-c(age,fare,parch,embarked, tamanho_familia,
                               fare_agg,fare_2, faixa_etaria, parch_agg, sibsp_agg))
mf<-glm(formula = survived ~ ., data = train, family="binomial")
summary(mf)
res<-predict(mf,val,type="response")
#confmatrix<-table(pred_log=res>0.5, verd=val$survived)
#acc<-(confmatrix[1,1]+confmatrix[2,2])/sum(confmatrix)
#print(acc)

#KNN
library(caret)
require(class)

normalize<-function(x) return((x-min(x))/(max(x)-min(x)))

target<-subset(df, select=survived)
df_knn<-subset(df, select=-c( age, sozinho, fare_agg, tamanho_familia,parch_agg, sibsp_agg, embarked_agg,
                             survived, fare,faixa_etaria))
dummy<-dummyVars(" ~. ", data=df_knn)
df_knn<-data.frame(predict(dummy,newdata=df_knn))

df_norm<-as.data.frame(lapply(df_knn, normalize))
X_train<-df_norm[sample,] #seed(17) já foi fixado
X_val<-df_norm[-sample,]
y_train<-target[sample,]
y_val<-target[-sample,]

knn_final<-knn(train = X_train, test = X_val, cl=y_train, k=15)
#acc<-sum(table(knn_final,y_val)[1,1],table(knn_final,y_val)[2,2])/length(y_val)

val<-df[-sample,]
val["p_arvore"]<-as.numeric(pred_tree)-1
val["pred_log"]<-ifelse(res>0.5,1,0)
val["p_knn"]<-as.numeric(knn_final)
val["p_voto"]<-ifelse(val$p_arvore+val$pred_log+val$p_knn>1,1,0)
acc_voto<-(table(val$p_voto, val$survived)[1,1]+table(val$p_voto, val$survived)[2,2])/nrow(val)
