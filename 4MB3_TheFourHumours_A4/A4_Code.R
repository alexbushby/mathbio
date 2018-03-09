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

time.plot<-function(dat,add){
  if(add == TRUE){
    X<-plot(dat$Week, dat$Counts, type = "l", xlab = "Time (Weeks)", ylab = "Cases of Measles")
    lo <- loess(dat$Counts~dat$Week)
    lin <- lines(predict(lo), col = "red")
  }
  if(add == FALSE){
    X<-plot(dat$Week, dat$Counts, type = "l", xlab = "Time (Weeks)", ylab = "Cases of Measles")
  }
}

time.plot(London, add=TRUE)
time.plot(London, add = FALSE)
time.plot(Liverpool, add=TRUE)
time.plot(Liverpool, add=FALSE)

###############PART III###############

periodogram<-function(dat, timemin = 1944-01-07, timemax = 1994-12-31){
  v<-ts(dat$Counts, start = timemin, end = timemax, frequency = 1)
  s<-spectrum(v, plot=FALSE)
  plot(s$freq, s$spec, type = "l", xlab = "Frequency", ylab = "Power")
}

periodogram(London)
periodogram(Liverpool, 1990-01-01, 1999-01-01)

###############PART B###############

##needs to be completed

####################QUESTION 2####################
###############PART A###############

##needs to be completed

SI.Gillepsie<-function(){
  
}

###############PART B###############

##needs to be completed