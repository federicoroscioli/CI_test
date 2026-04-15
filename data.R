# This code generates a synthetic dataset with three variables (x1, x2, x3) 
# that have a specified correlation structure.
# x1 and x2 are skewed with outliers, while x3 is normally distributed. 
# The correlation matrix is designed to have strong positive correlation 
# between x1 and x2, and weak
# install.packages("MASS")
library(MASS)

set.seed(123)

n <- 2000   # larger n = more stable sample correlations

# Function to generate data from latent correlations
make_data <- function(r12, r13, r23, n = 2000, seed = 123) {
  set.seed(seed)
  
  Sigma <- matrix(c(
    1,   r12, r13,
    r12, 1,   r23,
    r13, r23, 1
  ), 3, 3, byrow = TRUE)
  
  # Ensure positive definite
  if (any(eigen(Sigma)$values <= 0)) return(NULL)
  
  Z <- mvrnorm(n, mu = c(0, 0, 0), Sigma = Sigma)
  
  # Two skewed variables
  x1 <- exp(Z[,1])
  x2 <- exp(Z[,2])
  
  # One normal variable
  x3 <- Z[,3]
  
  # Add a few outliers to skewed vars
  n_out <- round(0.01 * n)   # 1%
  id1 <- sample(seq_len(n), n_out)
  id2 <- sample(seq_len(n), n_out)
  x1[id1] <- x1[id1] * 6
  x2[id2] <- x2[id2] * 6
  
  data.frame(x1 = x1, x2 = x2, x3 = x3)
}

# Objective: get final correlations close to target
target <- c(0.80, -0.10, -0.10)  # cor(x1,x2), cor(x1,x3), cor(x2,x3)

obj_fun <- function(par) {
  dat <- make_data(r12 = par[1], r13 = par[2], r23 = par[3], n = n, seed = 123)
  if (is.null(dat)) return(1e6)
  
  cc <- cor(dat)
  obs <- c(cc[1,2], cc[1,3], cc[2,3])
  
  sum((obs - target)^2)
}

# Starting values
start <- c(0.90, -0.05, -0.05)

fit <- optim(
  par = start,
  fn = obj_fun,
  method = "L-BFGS-B",
  lower = c(-0.99, -0.99, -0.99),
  upper = c( 0.99,  0.99,  0.99)
)

fit$par
fit$value

# Generate final dataset using optimized latent correlations
dat <- make_data(
  r12 = fit$par[1],
  r13 = fit$par[2],
  r23 = fit$par[3],
  n = n,
  seed = 999
)

# Check final correlations
round(cor(dat), 3)

# Look at distributions
par(mfrow = c(1,3))
hist(dat$x1, breaks = 40, main = "x1: skewed + outliers")
hist(dat$x2, breaks = 40, main = "x2: skewed + outliers")
hist(dat$x3, breaks = 40, main = "x3: normal")

# Optional: skewness-like quick check
summary(dat)

write.csv(dat, "synthetic_data.csv", row.names = FALSE)
