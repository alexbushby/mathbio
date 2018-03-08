#####PART I#####

londona <- read.csv("meas_uk__lon_1944-94_wk.csv")
liverpoola <- read.csv("meas_uk__lpl_1944-94_wk.csv")

londonb <- londona[-c(1,2,3,4,5,6,7,8,9),] ##cleaning out not needed data
liverpoolb <- liverpoola[-c(1,2,3,4,5,6,7,8,9),]

read.ymdc <- function(dat){
  year <- dat[seq(1,length(dat),4)]
  month <-dat[seq(2,length(dat),4)]
  day <-dat[seq(3,length(dat),4)]
  date<-as.Date(paste(year, month, day, sep = "."), format = "%Y.%m.%d")
  count <-dat[seq(4,length(dat),4)]
  week <- as.numeric((date - date[1])/7)
  l <- list(date, count, week)
  L <- data.frame(l)
  names(L) <-c("Date", "Counts", "Week")
  return(L)
}

London <- read.ymdc(londonb)
Liverpool <-read.ymdc(liverpoolb)

#####PART II#####

time.plot<-function(dat,smooth){
  X<-plot(dat$Week, dat$Counts, type = "l", xlab = "Time (Weeks)", ylab = "Cases of Measles")
  if(smooth == "TRUE"){
    lo <- loess(dat$Counts~dat$Week)
    line <- lines(predict(lo), col = "red")
    print(X) 
    print(line)
  }
  else{
   print(X) 
  }
}
  
#####PART III#####

periodogram<-function()

