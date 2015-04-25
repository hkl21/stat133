xUnique = 1:5
trueCoeff = c(0, 1, 1)

getData = function(coefs = c(0, 1, 1), xs = 1:5, dupl = 10,
                   sd = 5, seed=2222){
  ### This function creates the artificial data
  set.seed(seed)
  x = rep(xs, each = dupl)
  y = coefs[1] + coefs[2]*x + coefs[3] * x^2 + 
      rnorm(length(x), 0, sd)
  return(data.frame(x, y))
}

### 
genBootY = function(x, y, rep = TRUE){
  ### For each unique x value, take a sample of the
  ### corresponding y values, with replacement.
  ### Return a vector of random y values the same length as y
  ### You can assume that the xs are sorted
  ### Hint use tapply here!
 Y <- unlist(tapply(y,x,sample,replace = TRUE))
  return(Y)
}

genBootR = function(fit, err, rep = TRUE){
  ### Sample the errors 
  ### Add the errors to the fit to create a y vector
  ### Return a vector of y values the same length as fit
  ### HINT: It can be easier to sample the indices than the values
 R <- fit + sample(err, length(fit), replace = FALSE)
  return(R)
}

fitModel = function(x, y, degree = 1){
  ### use the lm function to fit a line of a quadratic 
  ### e.g. y ~ x or y ~ x + I(x^2)
  ### y and x are numeric vectors of the same length
  ### Return the coefficients as a vector 
  ### HINT: Take a look at the repBoot function to see how to use lm()
  if (degree ==1){
    coeff = lm(y~x)[1][[1]]
  }
  else if (degree ==2){
    coeff = lm(y~x + I(x^2))[1][[1]]
  }
  return(coeff)
}

oneBoot = function(data, fit = NULL, degree = 1){
  ###  data are either your data (from call to getData)
  ###  OR fit and errors from fit of line to data
  ###  OR fit and errors from fit of quadratic to data  
  ### Use fitModel to fit a model to this bootstrap Y 
  if (is.null(fit)){
    oneY = genBootY(data$x, data$y, rep=T)
  }
  else{
    oneY = genBootR(fit=fit[,1], error=fit[,2], rep=T)
  }
  return(fitModel(data$x, oneY, degree))
}

repBoot = function(data, B = 1000){
  
  ### Set up the inputs you need for oneBoot, i.e.,
  ### create errors and fits for line and quadratic
  line = lm(data$y~data$x)
  quad = lm(data$y~data$x + I(data$x^2))
  
  lerror = line$residuals 
  lfits = line$fitted.values
  qerror = quad$residuals
  qfits = quad$fitted.values
  ### replicate a call to oneBoot B times
  ### format the return value so that you have a list of
  ### length 4, one for each set of coefficients
  ### each element will contain a data frame with B rows
  ### and one or two columns, depending on whether the 
  ### fit is for a line or a quadratic
  ### Return this list
  ldata= data.frame(lfits, lerror)
  qdata= data.frame(qfits, qerror)
  ### Replicate a call to oneBoot B times for 
  ### each of the four conditions
  qq= replicate(n=B,oneBoot(data=data,degree=1))
  ww= replicate(n=B,oneBoot(data=data,degree=2))
  ee= replicate(n=B,oneBoot(data=data,fit=ldata,degree=1))
  rr= replicate(n=B,oneBoot(data=data,fit=qdata,degree=2))
  
  coeff<-list(qq,ww,ee,rr)
  ### Format the return value so that you have a list of
  ### length 4, one for each set of coefficients
  ### each element will contain a matrix with B columns
  ### and two or three rows, depending on whether the 
  ### fit is for a line or a quadratic
  ### Return this list
  
  return(coeff)
} 

bootPlot = function(x, y, coeff, trueCoeff){
  ### x and y are the original data
  ### coeff is a matrix from repBoot
  ### trueCoeff contains the true coefficients 
  ### that generated the data
  
  ### Make a scatter plot of data
  plot(x,y,main="Visualizing")
  ### Add lines or curves for each row in coeff
  ### Use transparency
  ### You should use mapply to construct all 
  ### 1000 of the bootstrapped lines of best fit 
  ### Have a look at ?mapply for details.
  ### This can be done in ggplot2 or base graphics.
  
  ### Use trueCoeff to add true line/curve - 
  ###  Make the true line/curve stand out
  if (nrow(coeff) == 2){
    mapply(abline,coeff[1,],coeff[2,],col=rgb(1,0,0,alpha=0.5))
  } 
  else if(nrow(coeff) == 3){
    mapply(function(a,b,c){curve(a+b*x+c*(x^2), add=TRUE, col=rgb(1,0.3,0.4, alpha=0.5))},coeff[1,],coeff[2,],coeff[3,])   
  }
  curve(trueCoeff[1]+trueCoeff[2]*x+trueCoeff[3]*(x^2),add=TRUE,col="blue")
}


### Run your simulation by calling this function
### This function doesn't need any changing
runSim = function() {
  xUnique = 1:5
  trueCoeff = c(0, 1, 1)
  myData = getData(coefs = trueCoeff, xs = xUnique)
  expt = repBoot(data = myData)
  par(mfrow = c(2, 2))
  for (i in 1:4){
   bootPlot(myData$x, myData$y, 
            coeff = expt[[i]], trueCoeff) 
  }
  return(expt)
}

