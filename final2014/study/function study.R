convert=function(x) x*2.54
x10 =function(x) mean(x)*10

#if/else/else if

x <- function(x,y){
 if(condition){
  statement1
} else {
  statement2
}
 }

#If u have used multiple if statements then if the condition is true all will be executed and 
#if u have used if and else if combination only one will be executed where first comes the true value

#if/else statements can be nested.
if(condition1){
  statement1
} else if(condition2){
  statement2
} else if(condition3){
  statement3
} else {
  statement4
}


normt = function(n, dist){
  if (dist =="normal"){
    return( rnorm(n))
  } else if (dist =="t"){
    return( rt(n,df=1,ncp=0))
  } else stop("distribution not")
}



#with logical arguments

corplot = function(x,y,plotit=TRUE){
  if(plotit == TRUE) plot(x,y)
  cor(x,y)
}

#To verify the arguments
if(!is.matrix(m))
  stop("m must be a matrix")

if(any(y==0)) warning("Dividing by zero")




#for Loops

printfor <- function(n) {
  for(i in 1:n) {
    print(i)
  }
}


n <- 100
boot_mean <- c()
for (i in 1:n){
  dat <- sample(iris$Sepal.Length, size=nrow(iris), replace=T)
  boot_mean[i] <- mean(dat)
 #boot_mean <- c(boot_mean,boot_mean[i])
 #same argument with above, so it makes one more elment
}


var(boot_mean)





####Simulation

coin = 0:1
n = 5
tosses = sample(coin, n, replace = TRUE)


x <- sapply(1:10, function(x) sum(sample(coin, n, replace = TRUE)))

            
coinToss <- function(n,x){
  sim <- c()
for (i in 1:x){
  Toss <- sample(1:6,n,replace=T)
  x<- which(Toss == 6)
  sim[i] <- (length(x))/n
 }
print(length(sim))
return(sim)
}


n.toss <- 4
n.iter <- 1000

# initialize samplesA
samplesA = c()
for (i in 1:n.iter){
  samplesA[i] = coinToss(n.toss)
}
length(samplesA)




samplesC = sapply(1:1000, function(x){sum(sample(0:1, 4, replace=TRUE))/4})








tmp <- c(tmp,"2015spring")
  
grep("[A-z][0-9]", tmp)

grep("[[:digit:]]", tmp)



#for we know how many times we want to enter the loop

#while stop when some condition is met
#while(k <100){