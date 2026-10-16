rm(list = ls())

library(reshape2)

# Parameters
Lx <- 100   # Length of the river in x-direction (m)
Ly <- 100   # Length of the river in y-direction (m)
T_max <- 50 # Simulation time (s)
u <- 1.0    # Flow velocity in x-direction (m/s)
v <- 0.5    # Flow velocity in y-direction (m/s)
k <- 0.05   # Decay rate (1/s)
Nx <- 101   # Number of spatial points in x-direction
Ny <- 101   # Number of spatial points in y-direction
Nt <- 1000  # Number of time steps
dx <- Lx / (Nx - 1) # Spatial step size in x-direction
dy <- Ly / (Ny - 1) # Spatial step size in y-direction
dt <- T_max / Nt   # Time step size

# Stability condition (CFL condition for 2D: u*dt/dx + v*dt/dy <= 1)
if (u * dt / dx + v * dt / dy > 1) stop("CFL condition not satisfied! Reduce dt or increase dx/dy.")

# Grid
x <- seq(0, Lx, length.out = Nx)
y <- seq(0, Ly, length.out = Ny)
t <- seq(0, T_max, length.out = Nt)

# Initialize solution matrix (C for concentration)
C <- array(0, dim = c(Nt, Nx, Ny))
dimnames(C) <- list(Time = t, Distance_x = x, Distance_y = y)

# Initial condition (Gaussian profile)
for (i in 1:Nx) {
  for (j in 1:Ny) {
    C[1, i, j] <- exp(-0.1 * (x[i]^2 + y[j]^2))
  }
}

# Boundary conditions (constant concentration at x = 0)
C[, 1, ] <- 1.0

# Time stepping using explicit upwind scheme for 2D
for (n in 1:(Nt - 1)) {
  for (i in 2:Nx) {
    for (j in 2:Ny) {
      # Advection and diffusion for both x and y directions
      C[n + 1, i, j] <- C[n, i, j] - (u * dt / dx) * (C[n, i, j] - C[n, i - 1, j]) -
        (v * dt / dy) * (C[n, i, j] - C[n, i, j - 1]) - 
        k * dt * C[n, i, j]
    }
  }
}

# Visualization
library(ggplot2)
library(reshape2)

# Prepare data for plotting (reshape to long format for ggplot)
C_df <- as.data.frame(C[,, round(Ny/2)]) # Take concentration at the middle of the y-axis
C_df$Time <- t
C_long <- melt(C_df, id.vars = "Time", variable.name = "Distance_x", value.name = "Concentration")
C_long$Distance_x <- as.numeric(as.character(C_long$Distance_x))

# Plot results
ggplot(C_long, aes(x = Distance_x, y = Concentration, group = Time, color = Time)) +
  geom_line(alpha = 0.7) +
  scale_color_viridis_c() +
  labs(
    title = "2-D Flood Water Contaminant Transport",
    x = "Distance (m)", y = "Concentration",
    color = "Time (s)"
  ) +
  theme_minimal()