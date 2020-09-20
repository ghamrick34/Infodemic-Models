library('deSolve')

# Real-world data from Table 2.2
N = 350
S_data = c(349,254,235,201,153.5,121,108,97,83)
I_data = c(1,7,14.5,22,29,21,8,8,0)
R_data = N - S_data - I_data

# Took August to May to be 9*30 days, Mid-May to be May 15th, and used 4ths of month
time = c(0,270,320,335,351,366,382,397,428)


initial = 3

plot_time = time[-(1:(initial-1))]
plot_S = S_data[-(1:(initial-1))]
plot_I = I_data[-(1:(initial-1))]
plot_R = R_data[-(1:(initial-1))]


# On the last day of data collection, there are 0 infectives, 
# so the number of susceptibles on that day is S(infinity)
Sinf = tail(S_data,1)
S0 = S_data[initial]
I0 = I_data[initial]
R0 = N - S0 - I0
alpha = 1/11
beta = (log(S0/Sinf)/(S0 + I0 - Sinf))*alpha
c = alpha/beta
Imax = -c + c * log(c) + S0 + I0 - c * log(S0)

# time sequence for before initial conditions
t1 <- seq(time[initial], 0, by = -0.01)

# time sequence for after initial conditions
t2 <- seq(time[initial], tail(time,1), by = 0.01)

# parameters: a named vector
parameters <- c(a = alpha, b = beta)

# initial condition: a named vector
state <- c(S = S0, I = I0, R = R0)

# R function to calculate the value of the derivatives at each time value
# Use the names of the variables as defined in the vectors above
SIR_model <- function(t, state, parameters){
  with(as.list(c(state, parameters)), {
    dS = -b * I * S
    dI = b * I * S - a * I
    dR = a * I
    return(list(c(dS, dI, dR)))
  })
}
## Integration with 'ode'
out1 <- ode(y = state, times = t1, func = SIR_model, parms = parameters)
out1.df <- as.data.frame(out1)

out2 <- ode(y = state, times = t2, func = SIR_model, parms = parameters)
out2.df <- as.data.frame(out2)

par(new=F,mar=c(5.1,4.1,4.1,5.1))
plot(time,S_data,
     xlab='Time (days since August 1665)',
     #ylab='Susceptible & Recovered (individuals)',
     ylab='Individuals',
     xlim=c(time[1],430), ylim=c(0,375),
     main='Eyam Plague')
lines(out1.df[c("time","S")], lty=1)
lines(out2.df[c("time","S")], lty=1)

par(new=T)
plot(time,R_data, col='red', 
     ann=F,axes=F, 
     xlim=c(time[1],430), ylim=c(0,375))
lines(out1.df[c("time","R")], col='red', lty=1)
lines(out2.df[c("time","R")], col='red', lty=1)

par(new=T)
plot(time,I_data, col='blue', 
     ann=F,axes=F, 
     #xlim=c(time[1],430), ylim=c(0,max(I_data,Imax)+2))
     xlim=c(time[1],430), ylim=c(0,350))
lines(out1.df[c("time","I")], col='blue', lty=1)
lines(out2.df[c("time","I")], col='blue', lty=1)
#mtext("Infective (individuals)", side=4, line=3)
#axis(4)

legend("top",
       legend=c("Model Susceptible","Susceptible Historical Data","Model Infective","Infective Historical Data","Model Recovered","Recovered Historical Data"),
       lty=c(1,NA,1,NA,1,NA),
       col=c("black","black","blue","blue","red","red"), 
       pch=c(NA,"o",NA,"o",NA,"o"),
       cex=0.75)


