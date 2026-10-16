# Install and load lpSolve
if (!require("lpSolve")) install.packages("lpSolve")
library(lpSolve)

# Coefficients of the objective function
c <- c(25, 25, 25, 20, 20, 20, 5, 5, 5)

# Coefficients of constraints
A <- matrix(c(
  # Transportation constraint
  1, 1, 1, 2, 2, 2, 3, 3, 3,
  # Logistical effort constraint
  1, 1, 1, 2, 2, 2, 1, 1, 1,
  # Shelter capacities for Clean Water
  1, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 1, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 1, 0, 0, 0, 0, 0, 0,
  # Shelter capacities for Food
  0, 0, 0, 1, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 1, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 1, 0, 0, 0,
  # Shelter capacities for Blankets
  0, 0, 0, 0, 0, 0, 1, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 1, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 1
), nrow = 11, byrow = TRUE)

# Right-hand side of constraints
b <- c(200, 150, 30, 40, 50, 20, 30, 40, 15, 25, 35)

# Solve the problem
solution <- lp("max", c, A, "<=", b)

# Output solution
cat("Optimal allocation:", solution$solution, "\n")
cat("Maximum people supported:", solution$objval, "\n")