# Load required library
library(parallel)

start <- Sys.time()
# Number of simulations and trading days
time_steps <- 41 * 390
simulations <- 10000
dt <- 1 / (252 * 390)

# Initial values and parameters
initial_s <- 183.17
initial_v <- 2.8873
theta <- 13.907
kappa <- 1.0
xi <- 24.3481
nu <- .0705
p <- 0

# Cholesky decomposition for correlated noise
cov_matrix <- matrix(c(1, p, p, 1), nrow = 2)
chol_decomp <- chol(cov_matrix)

# Function to simulate one path
simulate_path <- function(initial_s, initial_v, time_steps, dt, chol_decomp, theta, kappa, xi, nu) {
  s <- initial_s
  v <- initial_v
  daily_prices <- numeric(time_steps / 390 + 1)
  daily_prices[1] <- s
  
  for (j in 1:time_steps) {
    wt <- chol_decomp %*% c(rnorm(1), rnorm(1))
    wt1 <- wt[1]
    wt2 <- wt[2]
    
    # Euler-Maruyama Approximations
    s <- s + nu * s * dt + sqrt(v * dt) * s * wt1
    v <- max(0, v + kappa * (theta - v) * dt + xi * sqrt(v * dt) * wt2)
    
    if (j %% 390 == 0) {
      daily_prices[j / 390 + 1] <- s
    }
  }
  
  return(daily_prices)
}

# Set up parallel backend
num_cores <- detectCores() - 1  # Use all available cores except one
cat("Using", num_cores, "cores for parallel processing\n")

# Run simulations in parallel

results <- mclapply(1:simulations, function(i) {
  simulate_path(initial_s, initial_v, time_steps, dt, chol_decomp, theta, kappa, xi, nu)
}, mc.cores = num_cores)

# Combine results into a matrix
matrix3 <- do.call(rbind, results)

# Verify results
cat("Simulation complete. Matrix dimensions:", dim(matrix2), "\n")

# Save results to disk (optional)
#save(matrix2, file = "simulation_results2.RData")

# Plot one sample path
plot(matrix3[1, ], type = "l", main = "Simulated Daily Prices", xlab = "Days", ylab = "Price")
end <- Sys.time()

print(end - start)