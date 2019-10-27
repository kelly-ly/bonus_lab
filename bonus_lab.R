library("MASS")
data(iris)
mod_objext <- lm.ridge(Petal.Length~Species, data = iris, lambda = 1)
coef(mod_objext)
mod_objext1 <- lm(Petal.Length~Species, data = iris)
print(mod_objext1)

ridgereg <- function(formula, data, lambda){
  X <- model.matrix(formula, data)
  y = as.matrix(data[all.vars(formula)[1]])
  # QR = qr(X)
  # Q = qr.Q(QR)
  # R = qr.R(QR)
  # d = t(Q) %*% y
  # x = solve(R, d)
  # I <- diag(nrow(X),ncol(X),ncol(X))
  # I = solve(A, t(X)%*%y)
  
  ngene = nrow(X)
  nmotif = ncol(X)
  QR = qr(crossprod(X) + diag(nrow(X)*lambda,ncol(X),ncol(X)))
  result = qr.coef(QR,crossprod(X,y))
  # result <- solve(t(X) %*% X + diag(lambda, ncol(X)) , t(X) %*% y)
  # I <- diag(1,nmotif,nmotif)
  # result <- solve(crossprod(X)+lambda*I,crossprod(X,y))
  
  
  
  b_ridge <- solve(t(X) %*% X + (lambda * diag(dim(X)[2])), t(X) %*% y)
  return(t(b_ridge))
}

ridgereg(Petal.Length~Species, data = iris, lambda = 1)

