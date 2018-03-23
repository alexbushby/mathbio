####################QUESTION 1####################
###############PART A###############
###############PART I###############

londona <- read.csv("meas_uk__lon_1944-94_wk.csv")
liverpoola <- read.csv("meas_uk__lpl_1944-94_wk.csv")

londonb <- londona[-c(1,2,3,4,5,6,7,8,9),] ##cleaning out not needed data
liverpoolb <- liverpoola[-c(1,2,3,4,5,6,7,8,9),] ##cleaning out not needed data

read.ymdc <- function(dat){
  year <- dat[seq(1,length(dat),4)]
  month <-dat[seq(2,length(dat),4)]
  day <-dat[seq(3,length(dat),4)]
  date<-as.Date(paste(year, month, day, sep = "."), format = "%Y.%m.%d")
  count <-dat[seq(4,length(dat),4)]
  count <- as.numeric(as.character(count))
  week <- as.numeric((date - date[1])/7)
  l <- list(date, count, week)
  L <- data.frame(l)
  names(L) <-c("Date", "Counts", "Week")
  return(L)
}

London <- read.ymdc(londonb)
Liverpool <-read.ymdc(liverpoolb)

###############PART II###############
m.average <- function(dat,n){filter(dat[,2],rep(1/(2*n+1),n), sides=2)}

time.plot<-function(dat,add=FALSE, n=20, linetype = "l", colour = "red", maint){
  if(add == TRUE){
    X<-plot(dat$Week, dat$Counts, type= linetype, xlab = "Time (Weeks)", ylab = "Cases of Measles", main = maint)
    lin <- lines(m.average(dat,n), col = colour)
  }
  if(add == FALSE){
    X<-plot(dat$Week, dat$Counts, type = linetype, xlab = "Time (Weeks)", ylab = "Cases of Measles", main = maint)
  }
}

time.plot(London, add = TRUE, n = 10, col = "red", maint = "London")
time.plot(Liverpool, add=TRUE, n=10, col = "red", maint = "Liverpool")

###############PART III###############

periodogram<-function(dat, timemin = 0, timemax = 2660, linetype = "l", colour = "black", maint){
  Uptodate <- dat[dat$Week >= timemin & dat$Week <= timemax , ]
  s<-spectrum(Uptodate$Counts, plot=FALSE)
  per <-  1/(s$freq*52)
  spec <- s$spec/max(s$spec)
  plot(per, spec, type = linetype, xlab = "Period (Years)", ylab = "Power Spectrum", xlim = c(0,5), col = colour, main = maint)
}


###############PART B###############

par(mfrow = c(4,2))

periodogram(London, timemax = 400, maint = "London (1944 - 1951)")
periodogram(Liverpool, timemax = 400, maint = "Liverpool (1944-1951)")
periodogram(London, timemin = 400, timemax = 1250, maint = "London (1951-1967)") ##looks like there is some power at 2.5 years as well - changes from 2 years to 3 years
periodogram(Liverpool, timemin = 400, timemax = 1250, maint = "Liverpool (1951-1967)") ##looks like there is some power at 2.5 years as well - changes from 2 years to 3 years
periodogram(London, timemin = 1250, timemax = 2400, maint = "London (1967 - 1990)")
periodogram(Liverpool, timemin = 1250, timemax = 2400, maint = "Liverpool (1967 - 1990)") ##looks like there is some power at 2.5 years as well - changes from 2 years to 3 years
periodogram(London, timemin = 2400, maint = "London (1990-1994)")
periodogram(Liverpool, timemin = 2400, maint = "Liverpool (1990-1994)")

#The above periodograms were chosen because that's when the data appeared to change on the timeplot. 
#London's periodogram and Liverpool's periodogram are very different. London seems to have 
#to almost always have a period of 1 year, as well as an additional period, such as 2 or 2.5 years.
#Liverpool has a much more interesting and vast plot, but also shows some similarities to London's periodogram.
#For example, Liverpool and London both have high power at 1 year from 1944-1951, high power at 2 years from 1951-1967
## high power at 2.5 years from 1967-1990 and then high power back at 1 year from 1990-1994. What is interesting
##about the periodogram is Liverpool from 1990-1994. It seems as though there is a lot of power at certain intervals between 0 and 1 year.
##

dev.off()

####################QUESTION 2####################
###############PART A###############

##needs to be completed
SI.gillespie <- function(beta, N, I0, tmax){
  t0 <- 0
  times =(t0:tmax)
  x <-c(S=N-I0, I=I0, t=t0)
  
  res <- matrix(nrow=length(t0:tmax),ncol=length(x),
                dimnames = list(times,names(x))) #matrix to store values
  
  for (i in 1:(tmax+1)){
    res[i,] <- x
    rate <- with(as.list(x), beta*S*I) ## calculate current rate
    if(rate<=0) break #rate != 0; t_next would return NaN
    t_next <- rexp(1,rate)  #time to next event
    t0 <- t0 + t_next # update time
    x <- x+c(-1*rate*t_next, 1*rate*t_next, t0) #updates x <- c(S, I, t)
    if(x[1]<0) #if S is for some reason negative, change it to its previous positive
      x <- res[i,]
  }
  cbind(res[,3],res[,2]) #returns cbind(t, I)
}

par(mfrow = c(2,2))

## N=32
plot(0,0,xlim=c(0,10),ylim=c(0,32),
     type="n",xlab="Time (t)",ylab="Prevalence (I)",main = "N = 32", las=1)
for(i in 1:30){
  G.SI <- SI.gillespie(beta=1, N=32, I0=1, tmax=80)
  lines(G.SI, col=i)
}
N <- 32
beta <- 1
I0 <- 1
It <- I0*exp(N*beta*G.SI[,1])/(1+(I0/N)*(exp(N*beta*G.SI[,1])-1))
lines(G.SI[,1],It,lwd=3)

## N=100
plot(0,0,xlim=c(0,10),ylim=c(0,100),
     type="n",xlab="Time (t)",ylab="Prevalence (I)",las=1, main = "N = 100")
for(i in 1:30){
  G.SI <- SI.gillespie(beta=1, N=100, I0=1, tmax=300)
  lines(G.SI, col=i)
}
N <- 100
G.SI <- SI.gillespie(beta=1, N=100, I0=1, tmax=80)
It <- I0*exp(N*beta*G.SI[,1])/(1+(I0/N)*(exp(N*beta*G.SI[,1])-1))
lines(G.SI[,1],It,lwd=3)

## N=1000
plot(0,0,xlim=c(0,10),ylim=c(0,1000),
     type="n",xlab="Time (t)",ylab="Prevalence (I)",las=1, main = "N = 1000")
for(i in 1:30){
  G.SI <- SI.gillespie(beta=1, N=1000, I0=1, tmax=1000)
  lines(G.SI, col=i)
}
N <- 1000
It <- I0*exp(N*beta*G.SI[,1])/(1+(I0/N)*(exp(N*beta*G.SI[,1])-1))
lines(G.SI[,1],It,lwd=3)

## N=10,000
plot(0,0,xlim=c(0,10),ylim=c(0,10000),
     type="n",xlab="Time (t)",ylab="Prevalence (I)",las=1, main = "N = 10,000")
for(i in 1:30){
  G.SI <- SI.gillespie(beta=1, N=10000, I0=1, tmax=10000)
  lines(G.SI, col=i)
}
N <- 10000
It <- I0*exp(N*beta*G.SI[,1])/(1+(I0/N)*(exp(N*beta*G.SI[,1])-1))
lines(G.SI[,1],It,lwd=3)

