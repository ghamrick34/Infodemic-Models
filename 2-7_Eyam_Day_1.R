library('deSolve')

# Real-world data from Table 2.2
N = 350
mean_recovery = 11
S_data = c(349,254,235,201,153.5,121,108,97,83)
I_data = c(1,7,14.5,22,29,21,8,8,0)
R_data = N - S_data - I_data

# Took August to May to be 9*30 days, Mid-May to be May 15th, and used 4ths of month
time = c(0,270,320,335,351,366,382,397,428)

# On the last day of data collection, there are 0 infectives, so the number of susceptibles on that day is S(infinity)
Sinf = tail(S_data,1)
S0 = S_data[1]
I0 = I_data[1]
R0 = N - S0 - I0
alpha = 1/mean_recovery
beta = (log(S0/Sinf)/(S0 + I0 - Sinf))*alpha
c = alpha/beta
Imax = -c + c * log(c) + S0 + I0 - c * log(S0)

# Time sequence 
t <- seq(time[1], tail(time,1), by = 0.01)

# Parameters
parameters <- c(a = alpha, b = beta)

# Initial conditions
state <- c(S = S0, I = I0, R = R0)

# Define system of differential equations
SIR_model <- function(t, state, parameters){
  with(as.list(c(state, parameters)), {
    dS = -b * I * S
    dI = b * I * S - a * I
    dR = a * I
    return(list(c(dS, dI, dR)))
  })
}

# Integration with 'ode'
out <- ode(y = state, times = t, func = SIR_model, parms = parameters)
out.df <- as.data.frame(out)

# Plot susceptible data and model curve
par(new=F,mar=c(5.1,4.1,4.1,5.1))
plot(time,S_data,
     xlab='Time (days since August 1665)',
     ylab='Susceptibles & Recovered (individuals)',
     xlim=c(time[1],430), ylim=c(0,375),
     main='Eyam Plague')
lines(out.df[c("time","S")], lty=1)

# Plot recovered data and model curve
par(new=T)
plot(time,R_data, col='red', 
     ann=F,axes=F, 
     xlim=c(time[1],430), ylim=c(0,375))
lines(out.df[c("time","R")], col='red', lty=1)

# Plot infective data and model curve
par(new=T)
plot(time,I_data, col='blue', 
     ann=F,axes=F, 
     xlim=c(time[1],430), ylim=c(0,max(I_data,Imax)+2))
lines(out.df[c("time","I")], col='blue', lty=1)
mtext("Infectives (individuals)", side=4, line=3)
axis(4)

# Add legend
legend("topright",
       legend=c("Model Susceptibles","Susceptibles Historical Data","Model Infectives","Infectives Historical Data","Model Recovered","Recovered Historical Data"),
       lty=c(1,NA,1,NA,1,NA),
       col=c("black","black","blue","blue","red","red"), 
       pch=c(NA,"o",NA,"o",NA,"o"),
       cex=0.75)
