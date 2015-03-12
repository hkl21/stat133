###############################################################################
#### Functions for BML Simulation Study


#### Initialization function.
## Input : size of grid [r and c] and density [p]
## Output : A matrix [m] with entries 0 (no cars) 1 (red cars) or 2 (blue cars)
## that stores the state of the system (i.e. location of red and blue cars)

bml.init <- function(r, c, p){  
  ncars = p*r*c
  total = r*c
    sample.car= c(rep(0, total - ncars), rep(1, ncars/2), rep(2, ncars/2))
     m = matrix(sample(sample.car,total, replace = T), nrow = r)
      image(t(m)[,nrow(m):1], axes=FALSE, col = c("white", "red", "blue"))
  return(m)
}



  

#### Function to move the system one step (east and north)
## Input : a matrix [m] of the same type as the output from bml.init()
## Output : TWO variables, the updated [m] and a logical variable
## [grid.new] which should be TRUE if the system changed, FALSE otherwise.

## NOTE : the function should move the red cars once and the blue cars once,
## you can write extra functions that do just a step north or just a step east.

red <- function(row){
  ones<-rev(which(row==1))
  zeros<-which(row==0)
  for(i in ones){
    if(length(row) %in% ones & row[1] == 0){
      row[1]<-1
      row[length(row)]<-0
    }
    if(!(i+1) %in% zeros){
      next
    }
    if(i+1 %in% zeros){
      row[i+1]<-row[i]
      row[i]<-0
    }
  }
  return(row)
}


blue <- function(row){
  twos<-which(row==2)
  zeros<-which(row==0)
  for(i in twos){
    if(1 %in% twos & row[length(row)] == 0){
      row[length(row)] <- 2
      row[1] <- 0
    }
    if(!(i-1) %in% zeros){
      next 
    }
    if(i-1 %in% zeros){
      row[i-1] <- row[i]
      row[i] <- 0
    }
  }
  return(row)
}

bml.step <- function(m){
  grid.new<-T
  if(nrow(m) == 1){
    new.m<-red(m)
  }
  else{
    move.reds<-matrix(apply(m, 1, red), ncol=ncol(m), byrow=T)
    new.m<-apply(t(move.reds), 1, blue)
    image(t(apply(new.m,2,rev)), col=c('white','red','blue'), ask =T, axes=F)
  }
  
  for(i in (new.m==m)){
    if(i==F){
      grid.new<-F
    }
  }
  return(list(new.m, grid.new))
}



#### Function to do a simulation for a given set of input parameters
## Input : size of grid [r and c] and density [p]
## Output : *up to you* (e.g. number of steps taken, did you hit gridlock, ...)

bml.sim <- function(r, c, p){
  m = bml.init(r, c, p)
  image(t(apply(m,2,rev)), axes = FALSE, col = c("white", "red", "blue"))
  for (i in 1:2000) {
    n = bml.step(m)
    if (n[[2]] == TRUE) {
      m = n[[1]]
      image(t(apply(m,2,rev)), axes = FALSE, col = c("white", "red", "blue"))
    } else {
      image(t(apply(m,2,rev)), axes = FALSE, col = c("white", "red", "blue"))
      return (list(i, TRUE))
    }
  }
  return (list(i, FALSE))
}








