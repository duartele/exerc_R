' Atalhos uteis
Ctrl + Enter Para rodar apenas o que esta selecionado
ou a linha que esta o cursor
Ctrl + Alt + R - roda tudo
Ctrl + L - limpa o console
Ctrl + S - salva o script
q() #fecha o programa - quit
'

#Pedindo ajuda

?read.table #qdo sabe o nome, basta usar ? para conseguir ajuda
help("read.table")#o mesmo que ?
help.start()# inicia a versao html
help.search("read table") #outra forma de procurar
library(help=spatial)#Como acha conteudos de um pacote
find("lowess")#da o nome do pacote
example("read.table")
library(help=stats)
#Instalar pacotes
install.packages("")

#Primeiros comandos
ls() #lista todos os objetos no workspace
rm()#remove objetos do workspace
rm(list=ls())#limpa todo o workspace
dir()#mostra os arquivos no diretorio
list.files()#semelhante ao dir()
getwd()
setwd(dir)#Set or Set Working Directory
q()#fecha o programa
save(a,file="x.RData")#salva objetos do workspace
save.image()#atalho para salvar workspace

#Operadores
a <- -8 #Parecido com igual ("="), mas menos restrito
a = 9 # Só pode ser usado em top level
a <<- 10 # Diferente de <- por só poder ser variaveis globais
15 -> b #Atribui valor ao da direita
b%%2 #resto da divisao inteira
b%/%2 #resultado da divisao inteira
m1%*%m2 #Produto matricial

a!=b #diferente
a > 0 & b > 0 #Operador AND  
a > 0 | b > 0 #Operador OR
a==b #Verifica a igualdade

#Input e Output
getwd()#Obter qual é a diretoria de trabalho
setwd()#Mudar a diretoria de trabalho
read.table(file) 
'cria um data frame -> OBS Use dec="," qdo decimais sao separados por virgula.
Use as.is=TRUE para que não haja conversao nos caracteres
'
read.csv("file")
read.csv2("file") #diferenca é que dec=","
read.spss(file) #precisa instalar o pacote foreign
load("x.RData") #reload datasets written with the function
data(x) #load specified data sets

write.table(x,file="",row.names = TRUE,col.names = FALSE,sep=" ")
write.csv()
write.csv2()

#Criacao de Dados
a=c(1,2,3,4,17) #cria um vetor
10:17 #cria uma sequencia
seq(10,17,2) #aqui da pra especificar o by(=2 no exemplo)
rep(5,3) #faz repeticoes de rep(x,times) 
rep(c(5,3),2) #aqui repete o vetor 2 vezes 5 3 5 3
rep(c(5,3),each=2) #repete cada el. 2 vezes 5 5 3 3
f = data.frame(aluno=1:4, nota=c("a","B","c","d"),idade=10) #cria um data frame
list(a,f) #cria lista de named or unnamed arguments
l<-list(b=c(1,13),c=c("hi","mom"),j=3)

array(1:24,dim = c(3,4,5))
'
seq 1 a 24 com 3 linhas, 4 colunas e 5 de profund.
Era pra ter, entao, 60 elementos ( 3*4*5).
O R fica reciclando dos dados de x até finalizar o array
'
matrix(data = c(1:5),nrow =2 ,ncol = 3)
m1<-matrix(rep(c(3,7),3),2,3)#nao precisa especificar as linhas e col
factor(m1)#categoriza uma base x
expand.grid(c(1,2,3),c("n","m"))#cria um data.frame fazendo as combinacoes dos vetores
m2<-rbind(c(1,2,3),c(4,5,6))#combina elementos das linhas de matrizes, data frames etc
m3<-cbind(c(1,2,3),c(4,5,6))#combina para as colunas
dim(m3)#mostra a dimensao da matriz l e col
length(m3)#quantidade de elementos

#slicing and extracting datas
#1.1 indexing vector
a[5]#quinto elem. Maior que length(a) da NA
a[-3]#todos exceto o terceiro
a[2:4]#mostra do segundo ao quarto
a[-(2:4)]#todos exceto do segundo ao quarto
a[c(2,5,1)]#mostra os que voce quer na ordem que quer
v<-c("V1"=1,"V2"=6)
v["V2"]#mostra elemento que se chama V2
a[a>0.5 & a<16] #mostra os elementos que estao entre (0.5,16)
#1.2 indexing lista
l[2]#Mostra o segundo elemento da lista: "hi" e "mom"
l[[2]]#n-esimo elemento da lista: o que chama c
l[["c"]]#mostra elemento que se chama c
l$c #identico a l[["c"]]
#1.3 indexing matrizes
m2[2,1]#linha 2 e col 1 
m2[2,]#todos da linha 2
m2[,c(1,3)]#todas as linhas das col 1 e 3
#1.4 indexing data frames - mesmo que matrizes mais os seguintes
f[["nota"]]
f$idade

#Informacao das variables
fix(a)#arruma na mao um objeto
is.na(a)#olha todos os elementos de a e ver os NA
is.array(m1)#matriz tambem é um array
is.data.frame(f)
is.numeric(a)
nrow(m3)
ncol(m3)
class(f) #mostra (ou define) qual é a classe de  um objeto
attributes(f) #mostra (ou def) os atributos de um obj

#Data selection and manipulation
j = c(1,0,17,3)
k = c(2,1,0,17)
which.max(j) #mostra o índice do elemento mais alto
which.min(j)
rev(j) #reverte a posicao dos objetos
sort(j) #ordem crescente (rev(sort(j)) na ordem decre.)
cut(j,3) #divide o objeto em intervalos
match(j,k)#Para cada el de j, mostra a primeira pos que deu match em k
which(j==k)#Compara ji com ki e marca as posicoes que da match
which(j>2)#Aqui está mostrando as posicoes dos que da TRUE no teste logico
choose(n,k)#combinacao de k eventos em n repeticoes
choose(5,2)
'Aqui temos, por exemplo, a resposta para a seguinte pergunta:
De quantas formas poderiamos ter aulas no mestrado
ao longo da semana sem ter aula aos sabados ou domingos?
Res = 10 (seg e ter; seg e qua; seg e qui; seg e sex;
ter e qua; ter e qui; ter e sex; qua e qui; qua e sex; qui e sex)
'
na.omit(a) #remove as observacoes com NA
na.fail(a) #dispara um erro se há NA no objeto
unique(f) #remove duplicatas de f
fix(f)
View(f)
table(f$nota) #cria uma tabela mostrando os valores e suas freq. No exemplo, col nota de f
table(f) #Aqui mostrara tablas cruzadas - aqui esta com 3 dim - aluno, nota e idade
subset(f,f$idade==9)#cria um subset com os criterios escolhidos
df<-subset.data.frame(f,f$idade==9,select = c(aluno,nota))
"aqui voce escolhe quais colunas quer manter
drop = FALSE cria um data frame df.
Se drop = TRUE, aí é criada uma lista
"
x<-1:5
sample(x,10, replace = TRUE)#cria uma amostra de x de tamanho 10. replace=FALSE por default

#Funcoes matematicas
sin(pi/2) #Temos cos, tan, asin, acos, atan, atan2,...
log(10, base=exp(1))#Por default a base é e. Deixei assim apenas para lembrar
max(y)
min(y)
range(y) #mostra o a=min(y) e o b=max(y) (a,b) e nao max-min
sum(y,na.rm=TRUE) #Mostra a soma dos el. de y. NA deixara a soma NA se nao usar rm
fix(y)
diff(y)#Faz a subtracao de y(i+1) - y(i). Por isso mostra um vetor com n-1 elementos (nao da pra subtrair o primeiro elemento - ja que nao existe y0)
prod(x)==factorial(5)#faz o produto dos elementos de x. A outra formula calcula o fatorial de um numero
mean(y)
median(y)
quantile(y, 0.25) #Calcula os quantis em qualquer prob
weighted.mean(y,y/sum(y))#Faz a media ponderada pelos pesos apresentados depois da virgula. Bom pra calcular medias de classes, por exemplo
"Por exemplo. Seja A={1,2,2,3,3,3,4} uma amostra
A media de a sera (1+2*2+3*3+4)/7 = 2,57
Aih podemos fazer assim z=c(1,2,3,4) w=c(1,2,3,1)/7
weighted.mean(z,w)"
rank(y)#Mostra a posicao de cada el de y
var(y)#Variancia amostral (usa n-1)
var(m2)#Mostra a matriz de Covariancias
cov(m2)#Outra forma de chamar a matriz de Covariancias
sd(y)
cor(x,wt)#Calcula a correlacao de duas variaveis
cor(m2)#Mostra a matriz de correlacao de uma matriz
round(pi,2)#Arredonda um numero com n casas decimais
scale(m2)#Centraliza e escala uma matriz
pmin(w,wt)#Compara dois vetores e coloca o valor minimo
pmax(a,b)#Compara e coloca o max. NA sempre ganha
cumsum(b)#Para cada elemento vai colocando a soma. (x1,x1+x2,x1+x2+x3,etc...)
cumprod(b)#O mesmo para produto
union(a,b)#Apresenta a uniao dos cjtos (ou vetores)
intersect(a,b)#mostra a interseccao
setdiff(a,b)#Mostra todos os elementos de a que nao estao em b. Logo setdiff(a,b)!=setdiff(b,a)
setequal(a,b)#Verifica se os conjuntos sao o mesmo
Mod(-17)==abs(17)#Mod é o mesmo que absoluto
Mod(c(-17,18,20))#Pode colocar outros objetos
t(m2)#Calcula a transposta da matriz
d<-diag(m2)#Coloca os elementos da diagonal num vetor
m3%*%m2#Multiplicacao de matrizes
m3*m2#multiplica o el m3[i,j] com o m2[i,j]
solve(m2,m3)#obtem a matriz tal que m2%*%x=b
solve(m2)#obtem a matriz inversa de m2
rowsum(m3,group)
'Para cada coluna, soma os valores das linhas de um determinado grupo.
Se colocar tudo de um grupo, aí vai ser igual a colSums
Nao existe colsum() - seria sem sentido'
colSums(m4)
colMeans(m4)#Existe rowMeans() e rowSums(), mas nao fazem sentido analitico

#Plotting
#1-Parametros comuns dos gráficos
add=FALSE #Se esta TRUE, o gráfico será colado interpolando o anterior
axes=TRUE #Se esta FALSE, nao será colocados os desenhos eixos
type="p" # p é pontos, l linhas, b pontos e linhas, 
         # o pontos e linhas, h vertical lines, s steps, S steps
xlim=c(-1,20) #specifica os limites do eixo x
ylim=range(y)
xlab="FREQ" #Coloca nomes no eixo
ylab=
main="GRÁFICO" #Coloca título
sub="Sub-Título" #Fica na parte inferior do grafico
?legend(x,y,legend="Sexo")

#2-Tipos de Gráficos
plot(y) #Plot dos valores de y ordenados
plot(x,y)#Plot do par (x,y)
hist(x)#Histograma de freq de x
barplot(x,horiz=TRUE)
dotchart(x)
pie(x)
boxplot(x)
matplot(m4,m2)
pairs(m2)
qqnorm(x)
qqplot(x,y)

#Distribuicoes
rnorm(n, mean=0, sd=1)              #Normal - Gaussian
rexp(n, rate=1)                     #Exponencial
rgamma(n, shape, scale=1)           #Gamma
rpois(n, lambda = )                 #Poisson
rweibull(n, shape, scale = 1)       #Weibull
rcauchy(n, location = 0, scale = 1) #Cauchy
rbeta(n, shape1 = , shape2 = )      #Beta
rt(n, df, ncp)                      #T-Student
rf(n, df1, df2)                     #Fisher-Snedecor (F)(c2)
rchisq(n, df, ncp=0)                #Chi quadrado
rbinom(n,size,prob = )              #Binomial
rgeom(n,prob = )                    #Geometric
rhyper(nn,m,n,k)                    #Hypergeometric
rlogis(n,location = 0, scale=1)     #Logistic
rlnorm(n,meanlog = 0,sdlog = 1)     #Lognormal
rnbinom(n,size,prob = )             #Negative binomial
runif(n, min = 0, max = 1)          #Uniforme
rwilcox(nn,m,n)                     #Wilcoxon's statistics
'All these functions can be used by replacing
the letter "r" for "d", "p" or "q" to get respectively,
the probability density (dnorm), cumulative (pbinom)
and the value od the quantile (qpois())'

#Summary Statistics
summary(f)
str(f) #The internal structure
table(f)
head(f)
tail(f)
attach(f)
detach(f)
data.frame(v1,v2)#create a data frame from the vectors v1 and v2

#Statistical Tests
cor.test()
shapiro.test()
ks.test()
var.test()
bartlett.test()
t.test()
pairwise.t.test()
binom.test()
chisq.test()
fisher.test()
wilcox.test()
pairwise.wilcox.test()
aov(formula=) #Specialized ANOVA fubction
oneway.test()
anova() #compare two or more linear models (LRT)
kruskal.test()
friedman.test()
mantelhaen.test()
mcnemar.test()
prop.trend.test()

#Linear Models
lm()
glm()
Anova()           #car
anova()
coef()            #stats
coeftest()        #lmtest
confint()
deviance()
effects()
fitted()
formula()
linear.hypothesis() #car
model.matrix()
predict()
residuals()
summary.lm()
vcov()
AIC()
extractAIC()
step()
cooks.distance()
hat()
influence.measures()
lm.influence()
is.diag()           #stats
outlier.test()      #car
rstandard()
rstudent()
ceres.plot()        #car
cr.plots()          #car
influence.plot()    #car
leverage.plots()    #car
panel.car()         #car
plot.lm()           #stats
prplot()            #faraway
qq.plot()           #car
qqplot()
qqline()
qqnorm()
reg.line()          #car
scatterplot.matrix() #car
scatterplot()        #car
spread.level.plot() #car
bartlett.test()
ad.test()          #nortest
cmv.test()         #nortest
durbin.watson()    #car
dwtest()           #lmtest
levene.test()     #car
lilie.test()      #nortest
ncv.test()        #car
box.cox()         #car

lm(y1~x1,data = df1)
y~x1+x2+x3
y~x1+x2+x1:x2
y~x1*x2
y~(x1+x2)^2
y~x1+l(x1^2)
lm1=lm(formula)
update(lm1,formula2)
anova(lm1,lm2)
predict(lm1,newdata=df2)
par(mfrow=c(2,2));plot(lm1) # 4 plots: residuals, normal q-q
#Scale-location and Cooks

#packages
install.packages("pkgs", lib)
update.packages()
library(pkg)
detach("package:pkg")

#Programming
function(arglist)
returnValue()
if{} else{}
for(var in seq)
while
repeat
break
next
ifelse(test, yes, no)
do.call(funname,args)
