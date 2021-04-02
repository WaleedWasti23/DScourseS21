# ********************************************************
#     Waleed Wasti - Problem Set 8
# ********************************************************

# Packages
library(nloptr)
library(modelsummary)

# 4. Creating dataset with the given properties

# Setting seed
set.seed(100)

# Defining matrix X
X <- matrix(rnorm(100000*10,mean=0,sd=0.5),nrow=100000,ncol=10)
X[,1] <- 1

# Defining epsilon
eps <- rnorm(n=100000, mean=0, sd=0.5)

# Defining beta
beta <- matrix(c(1.5 ,-1 , -0.25 , 0.75 , 3.5 , -2 , 0.5 , 1 , 1.25 , 2),10,1)
typeof(beta)
length(beta)
class(beta)
str(beta)

# Generating Y
Y <- X%*%beta + eps


#5. Computing the OLS beta
beta_OLS <- solve(t(X)%*%X)%*%(t(X)%*%Y)
beta_OLS

#6. Computing OLS beta using gradient descent
# Our objective function
obj_func <- function(beta_o,y,x) {
  return (sum((y-x%*%beta_o)^2))
}

# Gradient of our objective function
gradient <- function(beta_g,y,x) {
  return (as.vector(-2*t(x)%*%(y-X%*%beta_g)))
}

# initial values
beta0 <- matrix(rnorm(10,mean = 0,sd=0.5),10,1)


gradientDesc <- function(y, x, beta_i,object,grad,learn_rate, tol,  max_iter) {
  
  n=dim(y)[1]
  m <- beta_i
  c <- matrix(rnorm(n,mean = 0,sd=0.5),n,1)
  
  yhat <- x%*%m+c
  MSE <- object(m,y,x)
  
  converged = F
  iterations = 0
  while(converged == F) {
    ## Implement the gradient descent algorithm
    m_new <- m - learn_rate * grad(m,y,x)
    m <- m_new
    c <- y-X%*%m
    yhat <- X%*%m+c
    MSE_new <- object(m,y,x)
    if(MSE - MSE_new <= tol) {
      converged = T
      
      return(m)
    }
    iterations = iterations + 1
    if(iterations > max_iter) { 
      converged = T
      return(m)
    }
  }
}


# Run the function 

beta_grad=gradientDesc(Y, X, beta0,obj_func,gradient,0.0000003, 0.0000000001, 1000)


#7. 

options <- list("algorithm"="NLOPT_LD_LBFGS","xtol_rel"=1.0e-6,"maxeval"=1e3)
beta_BFGS <- nloptr( x0=beta0,eval_f=obj_func,eval_grad_f=gradient,opts=options,y=Y,x=X)
beta_BFGS$solution

options <- list("algorithm"="NLOPT_LN_NELDERMEAD","xtol_rel"=1.0e-8)
beta_NM <- nloptr( x0=beta0,eval_f=obj_func,opts=options,y=Y,x=X)

beta_NM$solution

#8.

gradient_MLE <- function (theta ,y,x) {
  grad <- as.vector( rep (0, length (theta )))
  beta <- theta [1:( length ( theta) -1)]
  sig <- theta [ length (theta )]
  grad [1:( length ( theta) -1)] <- -t(X)%*%(Y - X%*%beta )/(sig ^2)
  grad[ length (theta )] <- dim (X)[1] /sig - crossprod (Y-X%*%beta )/(sig^3)
  return ( grad )
}
objfun_MLE <- function(beta_h,y,x) {
  return (sum((y-x%*%beta_h[1:( length ( beta_h) -1)])^2))
}
#theta0 <- runif(dim(X)[2]+1)
theta0 <- append(as.vector(summary(lm(Y~X-1))$coefficients[,1]),runif(1))
options <- list("algorithm"="NLOPT_LD_LBFGS","xtol_rel"=1.0e-6,"maxeval"=1e3)
beta_MLE <- nloptr( x0=theta0,eval_f=objfun_MLE,eval_grad_f=gradient_MLE,opts=options,y=Y,x=X)

round(beta_MLE$solution,3)



#9. 

estimate <-lm(Y~X-1)
modelsummary(estimate, output = 'latex')





