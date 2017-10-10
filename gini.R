# By Mark Waddle
# in the reply of https://www.kaggle.com/c/ClaimPredictionChallenge/discussion/703
normalizedGini <- function(aa, pp) {
  Gini <- function(a, p) {
    if (length(a) !=  length(p)) stop("Actual and Predicted need to be equal lengths!")
    temp.df <- data.frame(actual = a, pred = p, range=c(1:length(a)))
    temp.df <- temp.df[order(-temp.df$pred, temp.df$range),]
    population.delta <- 1 / length(a)
    total.losses <- sum(a)
    null.losses <- rep(population.delta, length(a)) # Hopefully is similar to accumulatedPopulationPercentageSum
    accum.losses <- temp.df$actual / total.losses # Hopefully is similar to accumulatedLossPercentageSum
    gini.sum <- cumsum(accum.losses - null.losses) # Not sure if this is having the same effect or not
    sum(gini.sum) / length(a)
  }
  Gini(aa,pp) / Gini(aa,aa)
}

test.normalizedGini <- function(){
  if(Gini(c(1,2,3), c(10,20,30)) - 0.1111 < 0.01 ){
    cat("passed test1\n")
  } else {
    cat("failed test1\n")
  }
  
  if(Gini(c(1,2,3), c(30,20,10)) + 0.1111 < 0.01 ){
    cat("passed test2\n")
  } else {
    cat("failed test2\n")
  }
  
  if(normalizedGini(c(1,2,3), c(10,20,30)) == 1){
    cat("passed test3\n")
  } else {
    cat("failed test3\n")
  }
  
  if(normalizedGini(c(1,2,3), c(30,20,10)) +1< 0.01){
    cat("passed test4\n")
  } else {
    cat("failed test4\n")
  }
  
  if(Gini(c(1,2,4,3), c(0,0,0,0)) + 0.1 < 0.01 ){
    cat("passed test5\n")
  } else {
    cat("failed test5\n")
  }
  
  if(Gini(c(2,1,4,3), c(0,0,2,1)) - 0.125 < 0.01 ){
    cat("passed test6\n")
  } else {
    cat("failed test6\n")
  }
}


test.normalizedGini()
