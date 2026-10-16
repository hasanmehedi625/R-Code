# Taylor Series by a polynomial ------------------------------------------------

rm(list = ls()) 

# Define your independent variable
x = seq(-5, 5, by=0.1)   

# Calculate your dependent variable which is a third order polynomial 
y = x^4-3*x^3-25*x^2-4*x-10  # Concentration in the lake, C     [Kg/m3]

# Plot the result
plot(x,y, type = "l", col="Blue",lwd=2,xlab = 'Time in years', 
     ylab = 'Cocncentration [kg/m3]',
     main = "Plot of Concentraiton Vs Time")

X1 = -2
Y1 = X1^4-3*X1^3-25*X1^2-4*X1-10 
dx = 3

X2 = X1+dx
Y =  X2^4-3*X2^3-25*X2^2-4*X2-10 
points(X1,Y1,pch=9, cex=0.5, col="Red")
points(X2,Y,pch=2,cex=1,col="Black")
print(paste("For X =", X2, "; the exact value of Y =", Y))

# First Derivatives only --------------------------------------------------

d1 = 4*X1^3-9*X1^2-50*X1-4
Y2= Y1 + d1*dx

points(X2,Y2,pch=5, cex=0.5, col="Red")

DD = Y - Y2

cat(paste("For X =", X2, "; the exact value of Y =", Y,
            "and \n the value calculated by the 1st derivatives only =",
            Y2, "\n which differs from the exact value by ", DD))

# Second derivatives ------------------------------------------------------

d2 = 12*X1^2-18*X1-50
Y2= Y1 + d1*dx + d2*(dx^2/factorial(2))
points(X2,Y2,pch=5, cex=0.5, col="Blue")
DD = Y - Y2

cat(paste("For X =", X2, "; the exact value of Y =", Y,
          "and \n the value calculated up to the 2nd derivatives =",
          Y2, "\n which differs from the exact value by ", DD))

# Third derivatives -------------------------------------------------------

d3 = 24*X1-18
Y2= Y1 + d1*dx + d2*(dx^2/factorial(2)) + d3*(dx^3/factorial(3))
points(X2,Y2,pch=5, cex=0.5, col="Green")
DD = Y - Y2

cat(paste("For X =", X2, "; the exact value of Y =", Y,
          "and \n the value calculated up to the 3rd derivatives =",
          Y2, "\n which differs from the exact value by ", DD))


# Fourth Derivatives ------------------------------------------------------

d4 = 24
Y2= Y1 + d1*dx + d2*(dx^2/factorial(2)) + d3*(dx^3/factorial(3)) + 
  d4*(dx^4/factorial(4)) 
points(X2,Y2,pch=5, cex=0.5, col="Cyan")
DD = Y - Y2

cat(paste("For X =", X2, "; the exact value of Y =", Y,
          "and \n the value calculated up to the 4th derivatives =",
          Y2, "\n which differs from the exact value by ", DD,
          "\n which is the exact value, because it was a 4th order polynomial"))

