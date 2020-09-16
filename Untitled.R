
## Define variable x to be some number
x = 20

## Define a list of numbers
y = c(7, 32, 11, 2, 6, 10, 14, 77, 8, 1, 5, 4, 20000000)  # c() is a protected function, so are T,F,t()

##
plot(x)

##
plot(y)

## plot y on a log scale (vertical)
plot(y,log='y')

## plot y on a log scale (horizontal)
plot(y,log='x')

## plot y on a log scale (both)
plot(y,log='xy')

## how to plot a function
# want to plot the function y = 3x^2
# [1] define x coordinates: min = 0, max = 13
x = seq(0,13,0.1) # increment = 0.1
# [2] define y coordinates based on x
y = 3*x^2
plot(x,y) # defaults to plotting points

# specify plot in 'line'
plot(x,y,type='l')

# axes labels, title, units, color, tick marks, legend, gridlines

# add labels/units (minutes vs. distance) and title (Distance over time)
plot(x,y,type='l',
     xlab='Time (minutes)',
     ylab='Distance(km)',
     main='Distance Over Time')

z = sin(x) # trig as expected | exponential: exp() | ln: log()
plot(x,z,type='l',col='blue') # new plot command erases previous

#adding two curves to same set of axes
plot(x,y,type='l',
     xlab='Time (minutes)',
     ylab='Distance(km)',
     main='Distance Over Time')
lines(x,z,col='blue',lty=2) # doesn't work because of scale difference

## Attempt #3: Add z with its own scale
par(new=F)
plot(x,y,type='l',
     xlab='Time (minutes)',
     ylab='Distance(km)',
     #ann=F,axes=F, 
     xlim=c(0,13), ylim=c(0,500),
     main='Distance Over Time')
#axis(2,at=seq(0,500,100))
par(new=T)
plot(x,z,type='l',col='blue',lty=2,
     ann=F,axes=F, xlim=c(0,13), ylim=c(-1,1))
axis(4)
grid()
legend("top",c("black line","blue line"),lty=c(1,2),col=c("black","blue"))

# solving and plotting solution to a differential equation

# logistic model: r = 0.1; K = 250

# install.packages('pracma') (only once ever)

library('pracma') # load package each time you restart and need it

r = 1
K = 250

f = function(t,i) r*i*(1-i/K)

a = 0 #start time
b = 30 #end time
y0 = 1 #initial population affected


# use rk4 to solve models
sol = rk4(f,a,b,y0,500)


plot(sol$x,sol$y,type='l',xlab='t',ylab='I(t)')

