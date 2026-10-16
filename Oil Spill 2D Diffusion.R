# Parameters
N <- 50         # Grid size
D <- 0.1       # Diffusion coefficient m2/d
dx <- 1         # Spatial step size in x direction
dy <- 1         # Spatial step size in y direction
dt <- 0.1       # Time step
alpha <- D * dt / (dx^2)  # For stability

sim_time = 365 #Time in days

# Initial condition: Oil spill at the center
C <- matrix(0, nrow = N, ncol = N)
C[N/2, N/2] <- 100  # Initial concentration at the center

# Function to visualize the concentration
plot_concentration <- function(C, title = "Concentration") {
  filled.contour(C, color.palette = terrain.colors, main = title)
}


# Explicit Schemes --------------------------------------------------------

# Explicit Scheme
explicit_diffusion <- function(C, alpha, timesteps) {
  for (t in 1:timesteps) {
    C_new <- C  # Copy current concentration matrix
    
    for (i in 2:(N-1)) {
      for (j in 2:(N-1)) {
        C_new[i, j] <- C[i, j] + alpha * (
          C[i+1, j] + C[i-1, j] + C[i, j+1] + C[i, j-1] - 4 * C[i, j]
        )
      }
    }
    
    C <- C_new
  }
  return(C)
}

# Run and plot explicit scheme
C_explicit <- explicit_diffusion(C, alpha, timesteps = sim_time/dt)
plot_concentration(C_explicit, "Explicit Scheme: Concentration after time")


# Implicit Schemes --------------------------------------------------------
# Implicit
implicit_diffusion <- function(C, alpha, timesteps) {
  I <- diag(N^2)  # Identity matrix
  L <- matrix(0, nrow = N^2, ncol = N^2)  # Laplacian matrix
  
  # Construct Laplacian matrix L for implicit method
  for (i in 1:(N^2)) {
    L[i, i] <- 1 + 4 * alpha
    if (i > 1) L[i, i-1] <- -alpha
    if (i < N^2) L[i, i+1] <- -alpha
    if (i > N) L[i, i-N] <- -alpha
    if (i <= N^2 - N) L[i, i+N] <- -alpha
  }
  
  for (t in 1:timesteps) {
    C_vector <- as.vector(C)  # Flatten C to vector form
    C_new_vector <- solve(L, C_vector)  # Solve system
    C <- matrix(C_new_vector, nrow = N, ncol = N)  # Reshape to matrix
  }
  return(C)
}

# Run and plot implicit scheme
C_implicit <- implicit_diffusion(C, alpha, timesteps = 100)
plot_concentration(C_implicit, "Implicit Scheme")


# Crank_nicolson ----------------------------------------------------------

# Crank-Nicolson Scheme
crank_nicolson_diffusion <- function(C, alpha, timesteps) {
  I <- diag(N^2)
  L <- matrix(0, nrow = N^2, ncol = N^2)
  
  # Build matrix for Crank-Nicolson
  for (i in 1:(N^2)) {
    L[i, i] <- 1 + 2 * alpha
    if (i > 1) L[i, i-1] <- -alpha / 2
    if (i < N^2) L[i, i+1] <- -alpha / 2
    if (i > N) L[i, i-N] <- -alpha / 2
    if (i <= N^2 - N) L[i, i+N] <- -alpha / 2
  }
  
  for (t in 1:timesteps) {
    C_vector <- as.vector(C)
    
    # Right-hand side for Crank-Nicolson
    B <- C_vector + (alpha / 2) * (
      C_vector[c(2:N^2, N^2)] + C_vector[c(1:(N^2-1), 1)] + 
        C_vector[c(N:N^2, 1:(N^2-N))] + C_vector[c(1:N, N^2-N+1:N^2)] - 4 * C_vector
    )
    
    # Solve system
    C_new_vector <- solve(L, B)
    C <- matrix(C_new_vector, nrow = N, ncol = N)
  }
  return(C)
}

# Run and plot Crank-Nicolson scheme
C_crank_nicolson <- crank_nicolson_diffusion(C, alpha, timesteps = 10)
plot_concentration(C_crank_nicolson, "Crank-Nicolson Scheme")





