#install.packages("deSolve")
library(deSolve)
library(dplyr)

###################################################################
####################HOMOGENEOUSLY-MIXED MODEL######################
###################################################################

{parameters1 <- c(b = 0.5, y=0.3)
state1 <- c(S = 0.998, I = 0.002)
Eq1 <- function(t, state1, parameters1){
  with(as.list(c(state1,parameters1)),{
    dS <- -b*S*I
    dI <- b*S*I - y*I
    list(c(dS,dI))
  })
}
times1 <- seq(0, 150, by = 0.01)
out1 <- ode(y=state1, times = times1, 
            func = Eq1, parms = parameters1)}

##1/R_0 is 0.6
timeR<-out1[which(out1[,2] >0.599 & out1[,2]<0.601)]
median(timeR) ##and what time the maximum occurs

par(mfrow=c(1,2))

plot(out1[,1],out1[,2], type = "l", ylim = c(0,1), xlab="Time (Days)", ylab = "Susceptible", col = 1)
title(main = "$\beta$ = 0.5")

plot(out1[,1],out1[,3], type = "l", ylim = c(0,0.1), xlab = "Time (Days)", ylab = "Infected")
title(main = "$\beta$ = 0.5")
abline(v=27.23)

plot(out2[,1],out2[,3] + out2[,2], type="l", col = "blue", xlab = "Time (Days)", ylab = "Total Susceptible")

###################################################################
####################HETEROGENEOUSLY-MIXED MODEL####################
###################################################################
{parameters2<-c(b1 = 0.6, b2 = 0.4, y = 0.3)
state2 <- c(S1 = 0.213, S2 = 0.785, I1 = 0.001, I2 = 0.001)
Eq2 <- function(t, state2, parameters2){
  with(as.list(c(state2,parameters2)),{
    dS1 <- -b1*S1*(I1+I2)
    dS2 <- -b2*S2*(I1+I2)
    dI1 <- b1*S1*(I1+I2) - y*I1
    dI2 <- b2*S2*(I1+I2) - y*I2
    list(c(dS1, dS2, dI1, dI2))
  })
}
times2 <- seq(0, 150, by = 0.01)
out2 <- ode(y=state2, times = times2, 
            func = Eq2, parms = parameters2)}

max(out2[,4]) ##maximum percentage of infected individuals (high)
out2[which(out2[,4]>0.01534906 & out2[,4] < 0.01534908),]
##time is approx 33.23

max(out2[,5]) ##maximum percentage of infected individuals (low)
out2[which(out2[,5]>0.04307889 & out2[,5] < 0.0430790),]
##time is approx 34.50

par(mfrow=c(1,2))

plot(out2[,1],out2[,3], type = "l", ylim = c(0,1), xlab="Time (Days)", ylab = "Susceptible", col = 1)
lines(out2[,1],out2[,2], type="l", col = 2)
lines(out2[,1],out2[,3] + out2[,2], type="l", col = 4)
legend("topright", c("High", "Low", "Total"), col = c(1, 2, 4), lty = c(1,1,1))
title(main ="$\beta_1$ = 0.6,$\beta_2$ = 0.4")

plot(out2[,1],out2[,5], type = "l", ylim = c(0,0.08), xlab = "Time (Days)", ylab = "Infected", xlim=c(0,80))
lines(out2[,1],out2[,4], type="l", col = "red")
lines(out2[,1],out2[,5] + out2[,4], type="l", col = "blue")
legend("topright", c("High", "Low", "Total"), col = c(1, 2, 4), lty = c(1,1,1))
title(main = "$\beta_1$ = 0.9,$\beta_2$ = 0.1")
segments(x0=33.23, y0=0, x1=33.23, y1=0.01534907, col = "red")
segments(x0=34.50, y0=0, x1=34.50, y1=0.0430789)


##Comparing both models

plot(out2[,1],out2[,3] + out2[,2], type = "l", ylim = c(0,1), xlab="Time (Days)", ylab = "Susceptible", col = 1)
lines(out1[,1],out1[,2], type = "l", col = 2)
legend("topright", c("Mixed", "Singular"), col = c(1, 2), lty = c(1,1))
title(main ="$\beta$ = 0.5,$\beta_1$ = 0.6,$\beta_2$ = 0.4")
lines(x=)

plot(out2[,1],out2[,4] + out2[,5], type = "l", ylim = c(0,0.1), xlab="Time (Days)", ylab = "Infected", col = 1)
lines(out1[,1],out1[,3], type = "l", col = 2)
legend("topright", c("Mixed", "Singular"), col = c(1, 2), lty = c(1,1))

plot(out2[,1],out2[,3] + out2[,2], type="l", col = "blue", xlab = "Time (Days)", ylab = "Total Susceptible")

plot(out2[,1],out2[,5] + out2[,4], type = "l", col = "blue", ylim = c(0,0.05), xlab = "Time (Days)", ylab = "Total Infected")

###################################################################
###########################FINAL SIZE#############################
###################################################################
{parameters3<-c(b1 = 0.6, b2 = 0.4, y = 0.3)
state3 <- c(S1 = 0.213, S2 = 0.785, I1 = 0.001, I2 = 0.001)
Eq3 <- function(t, state3, parameters3){
  with(as.list(c(state3,parameters3)),{
    dS1 <- -b1*S1*(I1+I2)
    dS2 <- -b2*S2*(I1+I2)
    dI1 <- b1*S1*(I1+I2)
    dI2 <- b2*S2*(I1+I2)
    list(c(dS1, dS2, dI1, dI2))
  })
}
times3 <- seq(0, 150, by = 0.01)
out3 <- ode(y=state3, times = times3, 
            func = Eq3, parms = parameters3)}

plot(out3[,1], out3[,4], type = "l")
plot(out3[,1], out3[,5], type = "l")