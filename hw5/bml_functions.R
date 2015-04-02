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

red.gnext <- function(i, j, m) {
  if (j == ncol(m)) {
    return(m[i, 1])
  } else {
    return(m[i, j+1])
  }
}

blue.gnext <- function(i, j, m) {
  if (i == 1) {
    return(m[nrow(m), j])
  } else {
    return(m[i-1, j])
  }
}

red.set <- function(i, j, m) {
  if (j == ncol(m)) {
    m[i, 1] = 1
    m[i, j] = 0
  } else {
    m[i, j+1] = 1
    m[i, j] = 0
  }
  return(m)
}

blue.set <- function(i, j, m) {
  if (i == 1) {
    m[nrow(m), j] = 2
    m[i, j] = 0
  } else {
    m[i-1, j] = 2
    m[i, j] = 0
  }
  return(m)
}


bml.step <- function(m){
  n1 = m
  n2 = m
  for (i in 1:nrow(n1)) {
    for (j in 1:ncol(n1)) {
      if ((n1[i,j] == 1) & (red.gnext(i,j,n1) == 0)) {
        n2 = red.set(i,j,n2)
      }
    }
  }
  n1 = n2
  for (i in 1:nrow(n1)) {
    for (j in 1:ncol(n1)) {
      if ((n1[i,j] == 2) & (blue.gnext(i,j,n1) == 0)) {
        n2 = blue.set(i,j,n2)
      }
    }
  }
  grid.new = any(n2 != m)
  m = n2
  return(list(m, grid.new))
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








