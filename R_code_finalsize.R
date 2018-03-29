rm(list = ls(all = TRUE)) 
SIR <- function(time,state,parameters) {
  with(as.list(c(state,parameters)),{
    dS1 <- -b1*S1*(I1+I2)
    dS2 <- -b2*S2*(I1+I2)
    dI1 <- b1*S1*(I1+I2) - gamma*I1
    dI2 <- b2*S2*(I1+I2) - gamma*I2
    dR <- gamma*(I1+I2)
    return(list(c(dS1,dS2,dI1,dI2,dR)))
  })
}
init <-  c(S1 = 0.213, S2 = 0.785, I1 = 0.001, I2 = 0.001, R=0) #initial conditions for odes
time <- seq(0,150,by=0.01) #time period

betavec1 <- seq(0.1,3,by=0.1)

res1 <- vector(length(betavec1),mode="list")

for (k in seq_along(betavec1)){ #range of values for beta
  res1[[k]] <- ode(y=init,times=time,func=SIR,
                  parms=c(b1=betavec1[k], gamma=0.3, b2=0.54))
}

names(res1) <- betavec1  ## to get beta value incorporated in results
dd1 <- dplyr::bind_rows(lapply(res1,as.data.frame),.id="beta1")
dd1$b1 <- as.numeric(dd1$beta1)

newdata1 <-dd1[ which(dd1$time==150), ]

##final size:R_0= log(1-z)/-z
newdata1$R_0 <- log(1-newdata1$R)/-newdata1$R


##changing beta_2
betavec2 <- seq(0.1,3,by=0.1)

res2 <- vector(length(betavec2),mode="list")

for (k in seq_along(betavec2)){ #range of values for beta
  res2[[k]] <- ode(y=init,times=time,func=SIR,
                   parms=c(b2=betavec2[k], gamma=0.3, b1=0.54))
}

names(res2) <- betavec2  ## to get beta value incorporated in results
dd2 <- dplyr::bind_rows(lapply(res2,as.data.frame),.id="beta2")
dd2$b2 <- as.numeric(dd2$beta2)

newdata2 <-dd2[ which(dd2$time==150), ]

##final size:R_0= log(1-z)/-z
newdata2$R_0 <- log(1-newdata2$R)/-newdata2$R

plot(newdata2$beta2, newdata2$R_0, type = "l", col = "blue", xlab = "\beta", lwd = 2)
lines(newdata1$beta1, newdata1$R_0, type = "l", col = "red", lwd = 2)
legend("bottomright", c("\beta_1 = 0.54","\beta_2 = 0.54"), col = c("red", "blue"),lwd = c(2,2))
plot(newdata1$beta1, newdata1$R_0, type = "l", col = "red")
lines(newdata2$beta2, newdata2$R_0, type = "l", col = "blue")

