# Analytical Solution 

 rm(list = ls())

 # Defining the parameters
 C0 = 0     # Initial concentration                                 [kg/m3]
 C1 = 0.2 # Concentration of pollutant in river 1                   [kg/m3]
 C2 = 0.1 # Concentration of pollutant in river 2                   [kg/m3]
 Q1 = 23.0E+6   # Discharge of river 1                              [m3/y]
 Q2 = 15.0E+6 # Discharge of river 2                                [m3/y]
 V = 1.0E+8 # Lake volume                                           [m3]

 #Let's simplify
 a = Q1*C1+Q2*C2
 b = Q1+Q2
 Ce = a/b

# Define your calculation time grid
 t = seq(0, 10, by=0.1)   # Calculation Time                        [Year]

# Perform your calculation 
 C = (a+(b*C0-a)*exp(-b*t/V))/b  # Concentration in the lake, C     [Kg/m3]

 # Plot the result
 plot(t,C, type = "l", col="Blue",lwd=2,xlab = 'Time in years', 
      ylab = 'Cocncentration [kg/m3]',
      main = "Plot of Concentraiton Vs Time")
 

# Numerical Solution ------------------------------------------------------

 dt = 0.1
 tn = seq(0,10,by=dt)
 Cn = rep(0, length(tn))
 Cn [1] = C0
 
 for (i in 2:length(Cn)) {
   Cn [i] = Cn[i-1]+(dt/V)*(a-b*Cn[i-1])
 }
  
 points(tn,Cn,pch=5, cex=0.5, col="Red")
 