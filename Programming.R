#Creating a function
integrand <-function(x){1/((x+1)*sqrt(x))}

integrate(integrand, lower = 0, upper = Inf)

#pow function
pow<-function(x=9,y=2){
  result <- x^y
  print(paste(x,"raised to the power",y,"is",result))
}

#Call the function pow()
pow(2,3)
pow(x=2,y=3)
pow(y=3,x=2)
pow(9)
pow()

#Factorial of x
#Using "WHILE"
x<-5
fac<-1
while(x>0){
  fac<-fac*x
  x <- x - 1
}

#Using "FOR"
x<-4
fac<-1
for(i in seq(1,x)){
  fac <- fac*i
}
fac

#Using FOR in a array - count even numbers in a array
x<-c(2,5,6,3,9,11,8)
c<-0
for(val in x){
  if(val%%2==0)
    c<-c+1
}
print(c)

#Using "IFELSE()" Function
'It is if...else for vectors
ifelse(test_expression, x, y)
test_expression must be a logical vector
if i is true so x[i] else y[i]
'
a<-c(5,7,2,9)
ifelse(a%%2==0,"even","odd")
#Output: "odd"  "odd"  "even" "odd"

#Using If...else Statement
'if(test_expression1){
statement1
} else if(test_expression2) {
statement2
}else{
statement3
}'

#Example
x<- 0
if(x>0){
  print("Non-negative number")
}else if(x<0){
  print("Negative number")
}else{
  print("Zero")
}
#We can simplify this
x<- -5
y<-if(x>0) TRUE else FALSE
#or
y<-if(x>0) 1 else 0

