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

time.plot<-function(dat,add=FALSE, n=20, linetype = "l", colour = "red"){
  if(add == TRUE){
    X<-plot(dat$Week, dat$Counts, type= linetype, xlab = "Time (Weeks)", ylab = "Cases of Measles")
    lin <- lines(m.average(dat,n), col = colour)
  }
  if(add == FALSE){
    X<-plot(dat$Week, dat$Counts, type = linetype, xlab = "Time (Weeks)", ylab = "Cases of Measles")
  }
}

time.plot(London, add = TRUE, n = 10, col = "red")
time.plot(London, add = FALSE, n = 10, col = "red")


###############PART III###############

periodogram<-function(dat, timemin = 0, timemax = 2660, linetype = "l", colour = "red"){
  Uptodate <- London[London$Week >= timemin & London$Week <= timemax , ]
  s<-spectrum(Uptodate$Counts, plot=FALSE)
  per <-  1/(s$freq*52)
  spec <- s$spec/max(s$spec)
  plot(per, spec, type = linetype, xlab = "Period (Years)", ylab = "Power Spectrum", xlim = c(0,5), col = colour)
}

periodogram(London, col = "blue")

###############PART B###############

##needs to be completed

####################QUESTION 2####################
###############PART A###############

##needs to be completed

SI.Gillepsie<-function(){
  
}

periodogram<-function(dat, timemin=0, timemax=){
  myts<-ts(dat$Counts, start=timemin, end = timemax, frequency = 52)
}

###############PART B###############

##needs to be completed