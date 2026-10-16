rm(list = ls()) 

# Defining the parameters
c0 <- 0
c1 <- 0.002
c2 <- 0.001
Q1 <- 7
Q2 <- 100
v <- 100000000

# Define your calculation time grid
t <- seq(0, 3, by = 0.01)         
length(t) # Time in [Year]
c_bar <- (Q1 * c1 + Q2 * c2) / (Q1 + Q2)
c_bar


# Perform your calculation 
c <- (Q1 * c1 + Q2 * c2 + ((Q1 + Q2) * c0 - Q1 * c1 + Q2 * c2) * exp(((Q1 + Q2) * t) / v)) / (Q1 + Q2)
#par(mfrow = c(2, 1))  ## set the layout to be 3 by 3
par(mfrow=c(1,2))

# Plot the result
plot(t, c, type = "l", xlab = "Time in years", 
     ylab = expression(paste("Concentration (kg/m"^3,")")),
     main = expression(paste("Pollutant Concentration vs Time  (With Baseline)")),
     col = "blue", lwd = 5, col.main = "black", col.lab = "black", cex.lab = 0.8,ylim=c(0.00105,0.0019))

# Add the horizontal line y = c_bar
abline(h= c_bar, col = "red", lty = 2)
text(1,0.00111621 ,expression(paste("Steady State margin (C=0.001065421 kg/"*m^3,")")), cex=1.2, pos=4, offset=0,col='red')


plot(t, c, type = "l", xlab = "Time in years", 
     ylab = expression(paste("Concentration (kg/m"^3,")")),
     main = expression(paste("Pollutant Concentration vs Time (Without Baseline)")),
     col = "blue", lwd = 5, col.main = "black", col.lab = "black", cex.lab = 0.9)

