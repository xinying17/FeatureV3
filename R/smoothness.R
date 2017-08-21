
smoothness <- function(Lap,f,s){
  # f is the function vector
  # s is parameter on Laplacian function
  f <- as.matrix(f)
  require('expm', quietly = TRUE)
  require('matrixcalc',quietly = TRUE)
  # if(is.positive.definite(Lap,tol=0)){
  #lap_fun <- function(x) {x %*% expm(s*logm(as.matrix(Lap))) %*% x}
  r <- eigen(Lap)
  V <- r$vectors
  lam <- r$values
  lam[lam<0] = 0
  Lmbd = diag(lam ** s)
  newL = V %*% Lmbd %*% solve(V)
  lap_fun <- function(x) {x %*% newL %*% x}

  smooth_value <- apply(f, 1, lap_fun)
  return(smooth_value)
}
