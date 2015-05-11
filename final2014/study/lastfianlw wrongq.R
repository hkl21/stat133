#################################################################
# Statistics 133, Lecture 1, Fall 2014
# Final Exam, Friday December 19th, 7 pm - 10 pm

# Please read all instructions carefully.

# When you are done SAVE the file final.R
# plug ethernet back in and please email it to
# ingileif@gmail.com
#################################################################
# IMPORTANT
# Enter your personal information here, between the quotation marks:
name <- ""
github.name <- ""
email.address <- ""

#################################################################
# The exam has a total of 100 points and for each task 
# the point is given in square brackets.
# The exam has 6 parts and the total number of points by part is:
# 15 pts : Part I, general R commands
# 20 pts : Part II, plotting
# 15 pts : Part III, apply and by statements
# 20 pts : Part IV, functions 
# 10 pts : Part V, simulations
# 20 pts : Part VI, string manipulation and regular expressions
#100 pts in Total

# If you spend about 30 min or so reading instructions and the like
# this gives you 1.5 min per point -- the exam is LONG ON PURPOSE!
# Please just work your way through it the best you can, prioritizing
# things you can easily do and remember everyone else is also
# racing the clock and will be graded on a curve.

#################################################################
# DO NOT manually set the path anywhere in this script, instead:

# RIGHT NOW!!   In the RStudio menu select:
# Session > Set Working Directory > To Source File Location

# NOTE!
# All "load" statements are included in the file already,
# DO NOT change them in any way or add a path to them,
# it will prevent us from being able to run your script.

# NOTE!
# There are a few calls to the function set.seed()
# DO NOT change these in any way.

#################################################################

#### PART I : General R commands [15 pts]

# [1 pt]
# Create [x], a numeric vector of length 1000 with 
# entries: 5, 10, 15, etc.
x <- seq(5,5000, by=5)
x <- seq(from=5, by=5, length.out=1000)
  
  # [1 pt]
  # Create [y], a logical vector of length 1000 
  # with y[i]=T if x[i] is divisible by 10, otherwise F
  
  y <- (x %% 10) == 0
  
y <- ifelse(x%%10, F, T)  
  
  # [1 pt]
  # Create [z], a numeric vector of length 111 with entries
  # that are drawn from a standard normal distribution (hint: rnorm)
  # *and* stored in increasing order
  set.seed(42)
z <- sort(rnorm(111),decreasing =F)
  
  
  # [1 pt]
  # Create [v], a numeric vector with :
  # a random permutation of the even numbers from 2 to 222
  set.seed(31415)

v <- sample(seq(2,222,by=2))

  
  
  # [1 pt]
  # Create [w], a random permutation of the numeric values of a deck of cards
  # (i.e. just the numbers 1 through 13 each repeated 4 times)
  set.seed(2718)
w <- sample(rep(1:13,4))
  
  
  # [1 pt]
  # Create [m], a matrix of size 10x10 with entries that are 
  # Exponential random variables (hint: rexp) with rate 3
  # (arrange the values by column, as per default)
  set.seed(344)
m <- matrix(rexp(100,rate=3),nrow=10)
  
  
  # [1 pt]
  # Create [l], a list with 12 elements, each a vector of length 100.
  # Each vector of length 100 of Poisson (hint:rpois) random variables with mean 5
  set.seed(71)

l <- rep(list(rpois(100,5)),12)
  
  
  # For the next few tasks you will use the data frame family (size 14x5)
  # LEAVE AS IS:
  load("family.rda")

# [1 pt]
# Create [f1] a subset of family with only women age 50 or over
f1 <- family[family$gender == "f" & family$age >= "50",]
  
  
  # [1 pt]
  # Create [f2] a subset of family with only men 6 foot tall or more
  f2 <- family[family$gender =="m" & family$height >= 6*12,]
  
  
  # [1 pt]
  # Create [f3] a subset of family of people whose name starts with T
  f3 <- family[substr(family$name,1,1)=="T",]
  
  
  # [1 pt]
  # Create [f4] a subset of family with just the youngest individual (so just one row)
  f4 <- family[which.min(family$age),]
  
  
  # for the next two tasks you will use the data frame infants (size 1236x15)
  # LEAVE AS IS:
  load("KaiserBabies.rda") 

# [2 pt]
# Create a table [t] of the education level ($ed) of all married ($marital) first time ($parity=1) mothers:

first <- infants[infants$parity ==1,]
t <- table(first$ed, first$marital,first$parity)
  
  # [2 pt]
  # Calculate [mw], the average birthweight ($bwt) of all babies whose were full term, i.e. gestation equal or more than 259 days.
b4 <- infants[infants$gestation >= 259,]
b4 <- b4[!is.na(b4$bwt),]
mw <- mean(b4$bwt)
  
  
  #################################################################
#### PART II : Plotting [20 pts]

##### Flowers [8 pts total, 2+3+3]
# We will now use the dataset "iris" which is icluded in the R package.
# To look at the dataframe you can just type "iris" at the prompt
# It is a data frame of size 150x5 with measurements of 4 attributes
# for 150 flowers, 50 each of 3 different species of irises.

# [2 pts]
# Make a box plot of Sepal Length by Species (so 3 boxplots in one plot)
plot(iris$Species,iris$Sepal.Length,type="b")


# [3 pts]
# Make a scatterplot of petal width (y-axis) versus petal length (x-axis)
# The axes labels should be "Petal Length" and "Petal Width",
# Color the plotting symbol by Species (any 3 colors)
#color <- rainbow(3)
plot(iris$Petal.Length,iris$Petal.Width,col=as.numeric(iris$Species))


# [3 pt]
# Make a scatterplot of ( sepal length / petal length) as a function of index (order)
# Color the plotting symbol by Species (any 3 colors)
colorr <- rainbow(length(unique(iris$Species)))
plot((iris$Sepal.Length/iris$Petal.Length),col=as.numeric(iris$Species))

##### We will now use the infant birth data again (data frame infants)

# [6 pts]
# Make a scatterplot of infant birthweight in ounces (bwt) vs. gestation time in days (gestation)
# The plotting symbol should be a red star (*)
# Put on custom made x-axis and y-axis labels that fully describe the variables
# Add a vertical line at gestation=259 (full length pregnancy)
plot(infants$bwt ~ infants$gestation,pch="*",col="red",xlab="gestation time in days",ylab="infant birthweight in ounces"
    ,xlim=c(0,500),ylim=c(0,150))
abline(x,v=259)


# [6 pts]
# Make a histogram of mother's age (age) and superimpose on it a _blue_ density plot (same variable)
# Note that the y-axis of the histogram and the density have to be the same...
# Add x-axis labels
hist(infants$age,xlab="AGE",freq=F)
lines(density(infants$age,na.rm=T),col="blue",type="p")


##Example 1: Write a function that makes a histogram of the inputting data, 
##with a red vertical line showing its mean.

hist <- function(data){
  hist(data)
  abline(v=mean(data),col="red")
}


##Example 2:Write a function that simulate B values following a standard normal distribution,
##Discard the negative values.

sim <- function(B){
  pool=rnorm(B)
  return(pool[pool > 0])
}



# For the next few tasks you will use the list Cache500 
# (list of length 500, each element is a numeric vector of various lengths)
# LEAVE AS IS:
load("Cache500.rda")


# [3 pts]
# Create [first.cache], a vector where each entry is the _first_ element of the
# corresponding vector in the list Cache500

first.cache <- sapply(Cache500, function(x) x[1])




# [3 pts]
# Create [mean.cache], a vector of length 500 where each entry is the mean 
# of the corresponding element of the list Cache500

mean.cache <- sapply(Cache500, mean)

#mean.cache <- sapply(Cache500, function(x) mean(x,na.rm=T))

# [2 pts]
# Create [sd.cache], a vector of length 500 where each entry is the sd
# of the corresponding element of the list Cache500

sd.cache <- sapply(Cache500, sd)


# [4 pts]
# Create [mean.long.cache], a vector where 
# mean.long.cache[i] is:
# the mean of Cache500[[i]] IF it has 50 or more entries.
# NA IF Cache500[[i]] has less than 50 entries.

mean.long.cache <- sapply(1:length(Cache500), function(i){
  if (length(Cache500[[i]]) >= 50) {
    return (mean(Cache500[[i]], na.rm=T))
  } else {
    return(NA)
  }
})

  

#function mean.long.cache



# Consider again the iris dataset
# [3 pts]
# Create a variable [max.petal.width] _a numeric vector of length 3_
# that has the maximum petal length for each iris species.


mpw <-as.numeric(tapply(iris$Petal.Length,iris$Species,max))




# [6 pts]
# Write a function [firstColToNames] that takes a matrix or a data frame
# and converts the first column to row names.
# Input:  a matrix or a data frame
# Output : a matrix of data frame that is like the input except 
# -- the first column has been removed
# -- what was the first column is now row names.

# Example, make a small 2x4 matrix, test:
# test <- matrix(c(1, 5, 3, 8, 2, 5, 7, 9), ncol=4, byrow=T)
# > test
# [,1] [,2] [,3] [,4]
# [1,]    1    5    3    8
# [2,]    2    5    7    9
# The output from firstColToNames(test) should be a 2x3 matrix with row names
# > firstColToNames(test)
#   [,1] [,2] [,3]
# 1    5    3    8
# 2    5    7    9




firstColToNames <- function(x) {
  nx <- x[,-1]
  names <- x[,1]
  rownames(nx) <- names
  return(nx)
}




# [6 pts]
# Write a function [longerRange()] with
# Input [m1 and m2] : two numeric matrices or data frames, don't have to have the same dimension 
# Output : 1 if the range of m1 is larger than or equal to the range of m2, 
#        : 2 otherwise 
# The range is the distance between the maximum and minimum values
# The function should ignore NA values (i.e. if a matrix has an entry that is NA)

longerRange <- function(m1,m2){
  if(NA %in% m1 == TRUE) stop ("no NA")
  if(NA %in% m2 == TRUE) stop ("no NA")
  m1 <- as.numeric(m1)
  m2 <- as.numeric(m2)
  min1<- min(m1)
  min2<- min(m2)
  max1<- max(m1)
  max2<- max(m2)
  if(max1-min1 >= max2-min2){
    return(1)
  } else if (max1-min1 < max2-min2){
    return(2)
  }
}

?range 



# [8 pts]
# Write a function [TempConv()] that takes
# Inputs
# [t] : a temperature (numeric value)
# [scale] : a character "F" or "C" depending on whether the 
#           input temperature is in Fahrenheit or Celcius
# Output 
# [t.new] : the temperature converted to the other scale.

# The conversion formulas are: 
# F = C * 9/5 + 32
# C = (F - 32) * 5/9
# so e.g. 30 F=-1.11 C and 30 C = 86 F

TempConv <- function(t, scale){
  if(scale == "F"){
    t.new <- round((t-32)*5/9,digits = 2)
    return(paste(t.new,"C",sep=" "))
  } 
  if(scale == "C"){
    t.new <- round((t*9/5)+32,digits = 2)
    return(paste(t.new,"F",sep=" "))
  } else if(scale != "F") stop("Don't forget '' !")
    else if(scale != "C") stop("Don't")
    else {
      return(NA)
    }
}






#################################################################
##### PART V : simulations [10 pts]

# leave this here:
set.seed(123456)

# [5 pts]
# Write a function, [dice_sum()], that takes as input:
# [k] : the number of dice rolled
# [B] : the number of rolls
# and returns:
# [dsum] : a vector of length B where each element is the sum of a roll 
#          k dice 

# So if k=1 pick a number between 1 and 6 at random B times and return them,
# if k=2 then in each roll you pick twice a number between 1 and 6 at random,
# calculate their sum, do this B times and return
# and so on.

dice_sum <-function(k,B){
  dice_sum <- sapply(1:B, function(i){
    sum(sample(1:6,k,replace=T))
  })
  return(dice_sum)
}



###2

dice_sum <- function(k,B){
    dice_sumB <- c()
    for (i in 1:B){
      dice <- sample(1:6,k,replace=T)
      dice_sumB[i] <- sum(dice)
      #print(dice_sumB)
    }
    print(length(dice_sumB))
    return(dice_sumB)
  }

  
  
  
  
  
  
  











# [5 pts]
# Lets run four simulations:
# Fix k=2 for all simulations
# Use B=20, 100, 1000, 5000 (in this order)

# For each value of B we will:

# Calculate the mean and sd of the output and store in 
# [ave.diceRoll] : a vector of length 4 where each entry is the mean of the simulation output
# [sd.diceRoll] : a vector of length 4 where each entry is the sd of the simulation output


# Then plot four histograms of the output from dice_sum()
# On each histogram the x-axis label should be "sum of dice roll" and the title should be
# "Histogram for B=[correct number]"

# CAREFUL : just run 4 simulations and save the mean and sd each time and make the histogram
# DO NOT : run 4 simulations to get the mean, then another 4 simulations to make the plots.

# NOTE : if for some reason you can not write the function [dice_sum] create four vectors
# of length 20, 100, 1000 and 5000 and complete the tasks below.

#----------------------
# To get all four histograms on one plot we include a command that splits the plotting
# screen into four panels, 2 in each row.  When a plotting function is called the plot
# goes into the first panel (upper left), when you call a plotting function again it goes
# into the second panel (upper right), etc.

# DO NOT DELETE THIS:
par(mfrow=c(2,2))
#----------------------

ave.diceRoll <- c()
sd.diceRoll <- c()

k<-2

Bvec <- c(20,100,1000,5000)

for (i in 1:4){
  sim <- dice_sum(k,Bvec[i])
  sd.diceRoll[i] <- sd(sim)
  ave.diceRoll[i] <- mean(sim)
  hist(sim, xlab="sum of dice roll",main= paste("Histogram for B=",Bvec[i],sep=" "))
}  
print(sd.diceRoll)
print(ave.diceRoll)








#################################################################
##### PART VI : string manipulation and regular expressions [20 pts]

phrases <- c("coat", "cat", "ct", "mat", "Sat!", "Now?", "match", "How much? $10", "7 cats", "ratatatcat", "atatatatatatatatat")

# [2 pts]
# Create a vector [text1] that lists the elements in phrases that have 
# a match to "at", anywhere 
text1 <- grep("at",phrases)
  
  # [2 pts]
  # Create a vector [text2] that lists the elements in phrases that have 
  # a match to "at", _at the end of the phrase_ 
#text2 <- grep("\\<at",phrases)

text2 <- grep("at\\>",phrases)


  # [4 pts]
  # Create a vector [text3] that lists the elements in phrases that have 
  # a match to any multiple of "at", _two or more times_ (atat" or "atatat" etc.)
  # and anything before or after that match
  text3 <- grep(".+((at){2,}).+",phrases)
  
  te# [3 pts]
  # Create a vector [tests] that is of length 200 and has the entries
  # "test1", "test2", ..., "test200"
  
   tests <- paste("test",seq(1:200),sep="")
  #tests <- paste("test",seq(1:200),sep="",collapse=" fuck ")
  
  # [3 pts]
  # Take the vector [tests] from above and create a character string
  # [tests.all] (so a vector of length 1)
  # that stores the entries of [tests] as one long string
  # i.e. tests.all should be "test1 test2 test3 ... test200"
  tests.all <- paste(tests,collapse=" ")



  # [6 pts]
  # Start with [minchin] which is a character string, create 
  # a _vector_ (not list) [minchin.split] which 
  # stores the words of [minchin] each as a separate element.
  # Also, convert all upper case letters to lower case.
  # You can leave punctuation marks in.
  
  minchin <- "And try as hard as I like, A small crack appears In my diplomacy-dike. By definition, I begin, Alternative Medicine, I continue Has either not been proved to work, Or been proved not to work. You know what they call alternative medicine That's been proved to work? Medicine."

minchin.split <- tolower(unlist(strsplit(minchin," ")))


======================================================================================================

# [1 pt]
# Create [w], a random permutation of the numeric values of a deck of cards
# (i.e. just the numbers 1 through 13 each repeated 4 times)
set.seed(2718)
w <- sample(rep(1:13,4))



# [1 pt]
# Create [f3] a subset of family of people whose name starts with T
f3 <- family[substr(family$name,1,1)=="A",]
  
  
# Make a box plot of Sepal Length by Species (so 3 boxplots in one plot)
  
  => Species should be on x axis then, Sepal Length is on y axis

# Plot life expectancy against fertility rate for each country,

=> same reason. fertiltity rate should be on x axis




$$$ question i got wrong

# Create a table [t] of the education level ($ed) of all married ($marital) first time ($parity=1) mothers

t <- table(infants$ed[infants$marital=="Married" & infants$parity=="1"]) 


# Create [l], a list with 12 elements, each a vector of length 100.
# Each vector of length 100 of Poisson (hint:rpois) random variables with mean 5
set.seed(71)

l <- lapply(1:12,function(x) {x<-rpois(100,lambda=5)})



#############3ex1
# For each individual in the family dataframe, plot their age (x-axis) against
# their bmi (y-axis).  Males should be represented with 'red' circles and
# females with 'blue' circles.  Set pch to 'o'. The x-axis should range from 23
# to 80 and your y-axis from 16 to 31. Finally put a legend in the 'topright'
# corner of the plot with a 'red' circle in front the label 'male' and a 'blue'
# circle in front the label 'female'. 
xlim = c(23, 80)
ylim = c(16, 31)
plot(family[family$gender == 'm' , c('age', 'bmi')], col='red', xlim=xlim, ylim=ylim)
points(family[family$gender == 'f' , c('age', 'bmi')], col='blue')
legend('topright', c('male', 'female'), pch='o', col=c('red', 'blue'))



##################3ex2
# 
# Takes the following arguments:
#   d: a dataframe
#
# Returns the number of factor vectors in the dataframe.

num.factors = function(d) {
  return(sum(sapply(d, is.factor)))
}



# Takes the following arguments:
#   l: a list of dataframes
#
# Returns the total number of factor vectors in all of the dataframes in the
# list.

num.factors2 = function(l) {
  return(sum(sapply(l, num.factors)))
}


# Implement the odd function
# 
# Takes the following arguments:
#   x: a numeric vector
#   flip: TRUE/FALSE 
#
# Returns a logical vector where
#  if flip is FALSE, then each element is
#     TRUE when the number is odd
#     FALSE when the number is even
#  if flip is TRUE, then each element is
#     TRUE when the number is even
#     FALSE when the number is odd

odd = function(x, flip=FALSE) {
  if (flip) x = x+1
  return(as.logical(x %% 2))
}


# Implement the sum.adm function
#
# Takes the following arguments:
#   x: a numeric vector
#   na.rm: TRUE/FALSE 
#
# Returns the sum of the absolute deviations from the median for the input
# vector x.  If na.rm is TRUE, the function removes all the NAs from the
# computation of the return value.

sum.adm = function(x, na.rm=FALSE){
  if (na.rm) x = x[!is.na(x)]
  return(sum(abs(x-median(x))))
}


========================Wrong questions


## In one R command, create a variable [[ m ]] that is a 3-by-4 matrix and has
## the entries 10, 20, through 120, where the numbers are entered row by row
## (so the first row is [ 10 20 30 40 ]).
m <- matrix(seq(10, 120, by=10), nrow=3, byrow=T)


###


## Create a variable [[ n ]] that is a data frame of dimension 10 x 3
## where the first column is the numbers 1-10, the second column the letters a-j and the
## third column has T/F, T if the number in column 1 is even, F if the number in column 1 is odd.
## Then add the column names :  num, lett, even


n <- data.frame(num=c(1:10),lett=letters[1:10],even=as.logical(1:10 %% 2))


####


## In one R command create the variable [[ n.obs ]] , a vector which stores the number
## of observations at EACH of the stations in rain

n.obs <- sapply(rain, length)



####


## Make a scatterplot that shows the rainfall at station 1 in year 2000.
## Precipitation should go on the y-axis and day on the x-axis
## Make new labels for the x- and y-axis.
## Hint: the date information is stored in the object day.
yr.2000 <- floor(day[[1]]) == 2000
rainfall.subset <- rain[[1]][yr.2000]
day.subset <- day[[1]][yr.2000]
plot(day.subset, rainfall.subset, xlab='day', ylab='rainfall')


###



## Let us do a linear regression of birth weight (bwt) as a function of the length
## of the pregnancy
## make sure to keep this line:
fit <- lm(bwt ~ gestation, data=infants)



###


# [3 pts]
# Create [first.cache], a vector where each entry is the _first_ element of the
# corresponding vector in the list Cache500

first.cache <- sapply(Cache500,function(x) x[1])

