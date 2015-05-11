# Simulation of Johnny's Office Hours

# The number of students coming to Johnny's OH can be modeled 
# by a Poisson random variable with mean k.

# Write a function called num_students:
# Input: <k>: mean of a Poisson random variable (a single number)
# Output: <num>: a Poisson(k) random variable (a single number)

num_students <- function(k) {
  rpois(1, k)
}

# Assume all students arrive at different times.
# The first student arrives Y_1 minutes after OH begins. 
# For i = 2, ..., number of students, 
# student i arrives Y_i minutes after student i - 1 arrives.
# These Y_i are independently idenitically distributed
# Exponential random variables with rate 1/8.
# (This corresponds to the fact that on average,
# a new student comes to OH every eight minutes.)

# Write a function called interarrival_times:
# Input: <num>: number of students (a number)
# Output: <inter>: a random sample of num Exponential(1/8) 
# random variables (a vector of length num)
interarrival_times <- function(num) {
  rexp(num, 1/8)
}

# For student i, it takes Z_i minutes for Johnny to answer questions.
# These Z_i are independently idenitically distributed
# Exponential random variables with rate 1/10.
# (This corresponds to the fact that on average,
# it takes Johnny ten minutes to fully answer a student's questions.)

# Write a function called service_times:
# Input: <num>: number of students (a number)
# Output: <serv>: a random sample of num Exponential(1/10) 
# random variables (a vector of length num)
service_times <- function(num) {
  rexp(num, 1/10)
}

# Compute the waiting time for each student.
# The waiting time for the first student is always 0.
# The waiting time for the ith student (i is at least 2) can be computed
# by the following:
#####################################
# waiting time for the ith student
# = service time for the (i-1)th student 
# + waiting time for the (i-1)th student
# - interarrival time for the ith student
#########################################
# If the waiting time computed using the formula above is 
# negative, set the waiting time equal to 0. 

# Write a function called waiting_times:
# Input: 
# <inter>: a vector of interrarrival times
# <serv>: a vector of service times
# Output: 
# <wait>: a vector that contains the waiting time for each student
waiting_times <- function(inter, serv){
  wait <- rep(0, length(inter))
  for(i in 2:length(inter)){
    wait[i] <- max(serv[i-1]  + wait[i-1] - inter[i], 0)
  }
  wait
}

# Simulation
# Write a function called queueing_sim:
# Input: 
# <k>: the mean number of students coming to Johnny's OH
# Output: 
# <sim>: a data frame with four columns:
#   inter: interarrival times
#   serv: service times
#   wait: waiting times
#   total: total times spent in Johnny's OH (serv + wait)

queueing_sim <- function(k) {
  num <- num_students(k)
  inter <- interarrival_times(num)
  serv <- service_times(num)
  wait <- waiting_times(inter, serv)
  total <- serv + wait
  data.frame(inter, serv, wait, total)
}

set.seed(1234)
# Run the simulation 500 times with k = 16. 
# Save the output in a variable called sim500.
# sim500 is a list of 500 data frames.
sim500 <- replicate(500, queueing_sim(16), simplify = F)

# For each simulation, compute the average waiting time and
# the average total time spent in OH.
# Save the result in a matrix called avg_wait_total.
# avg_wait_total is a 2x500 matrix (without any row names or column names).
avg_wait_total <- sapply(sim500, function(df) c(mean(df$wait), mean(df$total)))

#-----------------------------------------------------------------------
# Suppose Johnny is not feeling well and he needs to take a break of 
# around 5 minutes for every two students he serves.
# That is, he takes a break after serving the 2nd student, the 4th student, etc.
# He stops serving students until the break is over.
# Also, he needs to take a 30-minute nap right after the fifth student he serve.
# Again, he is not serving any students during his nap.
# In addition, he talks more slowly after the 30-minute nap: 
# The service times of 6th, 7th, ... students are multiplied by 1.5.

# The break times are independent and each can be modeled
# by a Normal random variable with mean 5 and variance 4,
# with the condition that whenever the random variable generated is 
# negative, set it equal to 0.
# DO NOT INCLUDE THE NAP TIME INTO BREAK TIMES.

# Write a function called break_times that generates
# n break times:
# Input: 
# <n>: the number of breaks Johnny is going to take (a single number)
# Output:
# <br_times>: a numeric vector of break times
break_times <- function(n){
  br_times <- rnorm(n , mean = 5, sd = sqrt(4))
  br_times <- br_times * (br_times > 0)
  br_times
}

# Write a function called serv_wait_sick that computes
# the service time and the waiting time for each student, taking into account 
# that Johnny is taking a break from time to time as 
# described above and a 30-minute nap after serving the fifth student,
# and the service times for 6th, 7th, ... students are multiplied by 1.5:
# Input: 
# <inter>: a vector of interrarrival times
# <serv>: a vector of the usual service times
# <br_times>: a vector of break times
# Output: 
# <wait_serv>: a data frame with two columns,
#  <serv>: a vector of the (updated) service times
#  <wait>: a vector of the waiting times
# Remark: There is no randomness in this function.
serv_wait_sick <- function(inter, serv, br_times){
  wait <- rep(0, length(inter))
  if(length(serv) >= 6){
    serv[6:length(serv)] <- serv[6:length(serv)] * 1.5  
  }
  for(i in 2:length(inter)){
    if(i != 6){
      if(i%%2 == 1) {
        wait[i] <- max(serv[i-1]  + wait[i-1] + br_times[i/2] - inter[i], 0)  
      } else{
        wait[i] <- max(serv[i-1]  + wait[i-1] - inter[i], 0) 
      }  
    } else{
      wait[i] <- max(serv[i-1]  + wait[i-1] + 30 - inter[i], 0)
    }
  }
  data.frame(serv, wait)
}

# End of quiz.

# Just a test case for break_times and a test case for waiting_times_sick. 
# No need to modify the following code.
# If your functions work properly, the following code should return TRUE.
set.seed(12)
all.equal(break_times(3), c(2.038865, 8.154339, 3.086511), tolerance = 0.001)
all.equal(serv_wait_sick(rep(10, 10), rep(10, 10), c(0, 1, 3, 2, 1)), 
          data.frame(serv = c(10,10,10,10,10,15,15,15,15,15), 
                     wait = c(0,0,0,0,1,31,39,44,51,56)))






####333
# Write a function called numAtElements. Your function should take the following
# arguments
#   <chvec>: A character vector containing strings of letters, possibly possibly
#     with the "@" symbol
#
# and return the following
#   <num.at>: an integer indicating how many elements of <chvec> contain the "@"
#     symbol. For example: numAtElements(c('karl', 'k@rl', '@@@')) should return 2
numAtElements <- function(chvec){
  ats <- grep("@", chvec)
  return(length(ats))
}

# Write a function called unexclaim. Your function should take the following
# arguments
#   <chstring>: a character vector of length 1 (contains only one string).
#
# and return the following
#   <newstring>: a character vector of length 1 where all ! symbols have been
#     replaced by . symbols
unexclaim <- function(chstring) {
  return(gsub('!', '.', chstring))
}


# Write a function called updateDate. Your function should take the following
# arguments
#   <dates>: a character vector of dates of the form "month, year" (e.g. "May, 2001")
#   <old.yr>: a string indicating the year for which elements will be updated
#     (e.g. "2002")
#
# and return the following
#   <updated.dates>: a character vector of dates where <old.yr> has been replaced by
#     '2015'. This vector should only contain the elements whose date has been
#     updated. For example updateDate(c('May, 2010', 'June, 2011'), '2010') should
#     return 'May, 2015'.
updateDate <- function(dates, old.yr) {
  
  old.yr.idcs <- grep(old.yr, dates)
  new.dates <- gsub(old.yr, '2015', dates[old.yr.idcs])
  return(new.dates)
}


# Write a function called countcatdog that counts the number of instances of
# "cat" (case-insensitive) and the number of instances of "dog"
# (case-insensitive) in a string. Your function should take the following
# arguments 
#   <chvec>: A character vector of length 1 (chvec contains only 1 string)
#
# and return the following
#   <counts>: An integer vector of length 2 with names "cat" and "dog".
#             For example, countcatdog("doGCATcat abcAt") returns:
#                    cat dog
#                     3   1
countcatdog<- function(chvec){
  
  chvec <- tolower(chvec)
  
  num.cat <- 0
  num.dog <- 0
  ind.cat <- gregexpr("cat", chvec)[[1]]
  ind.dog <- gregexpr("dog", chvec)[[1]]
  
  if(ind.cat[1] != -1) num.cat <- length(ind.cat)
  if(ind.dog[1] != -1) num.dog <- length(ind.dog)
  counts <- c(cat = num.cat, dog = num.dog)
  return(counts)
}


# Write a function called sumDigits that compute the sum of all the digits in
# a string.  The function should return 0 if there is no digit in the
# string. Your function should take the following arguments:
#   <chvec>: A character vector of length 1 (chvec contains only 1 string)
#
# and return the following
#   <total>: A single number (the sum of all the digits in chvec)
sumDigits <- function(chvec){
  
  temp <- unlist(strsplit(chvec, ""))
  sum(as.numeric(grep("[0-9]", temp, value = T)))
}

# Some test cases:
# all.equal(sumDigits("1z3p ! 21"), 7)
# all.equal(sumDigits("abcdefg"), 0)



# DNA.vec is a character vector of strings of DNA. It contains at least two
# strings of DNA. For simplicity, each string has only 10 characters.  Note that
# a DNA is always made up of A, T, C, G's. Write a function called dnaTransform
# that performs the following:

# Step 1: Find the first two DNA strings in DNA.vec that contains the sequence
# "ATTA"; call them DNA1 and DNA2. If there are less than two DNA strings that
# contains the sequence "ATTA", the function ends immediately and returns the
# first two elements in DNA.vec

# Step 2: Split DNA1 into two halves (i.e. strings of length 5 each).
# (For example, if DNA1 is "ATTATAGCCA", then we have "ATTAT" as the first half
# and "AGCCA" as the second half)

# Step 3: Split DNA2 into two halves (as in Step 2).

# Step 4: Return a character vector of two strings:
# --first string: the first half of DNA1 combined with the second half of DNA2
# --second string: the first half of DNA2 combined with the second half of DNA1

# Input:
#   <DNA_vec>: A character vector of DNAs
# Output:
#   <DNA_final>: A character vector of two DNAs

dnaTransform <- function(DNA.vec){
  ind <- grep("ATTA", DNA.vec)
  if(length(ind) < 2) return(DNA.vec[1:2])
  
  DNA1 <- DNA.vec[ind[1]]
  DNA2 <- DNA.vec[ind[2]]
  string1 <- paste0(substring(DNA1, 1, 5), substring(DNA2, 6, 10))
  string2 <- paste0(substring(DNA2, 1, 5), substring(DNA1, 6, 10))
  DNA.final <- c(string1, string2)
  
  return(DNA.final)
}

# Some test cases:
# all.equal(dnaTransform(c("AAAAAAAAAA", "ATTAGATACT", "ATACATTACG")), c("ATTAGTTACG", "ATACAATACT"))
# all.equal(dnaTransform(c("ATCGATCGAT", "TCGATCGATT", "ATTTTTTTTT")), c("ATCGATCGAT", "TCGATCGATT"))

