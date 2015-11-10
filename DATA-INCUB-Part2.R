library(data.table)
library(phonTools)
library(stringr)

# Find the useful columns.
test <- read.csv('nyc311calls.csv', nrows=100)
names(test)
var_need <- c(2,4,6,25,51,52)
names(test)[var_need]

# Read the data.
data <- fread('nyc311calls.csv', select=var_need, stringsAsFactors=TRUE)
names(data)
names(data)[3] <- 'Comp_type'
names(data)[1] <- 'Time'

# Question 1: Fraction of 2nd popular agency.
q1 <- factor(data$Agency)
list <- sort(summary(q1), decreasing=TRUE)
list[[2]]/sum(list)

# Question 2: Largest ratio of conditional probability.
comp_type <- factor(data$Comp_type)
borough <- factor(data$Borough)
unc_pro <- table(comp_type)/sum(table(comp_type))
con_pro <- tapply(comp_type, borough, function(a) table(a)/sum(table(a)))
max <- c()
for (i in 1:dim(con_pro)){
    max <- c(max(con_pro[[i]]/unc_pro),max)
}
max(max)

# Question 3: 90% and 10% percentiles difference of latitude.
latitude <- data$Latitude
quant <- quantile(latitude, c(0.1,0.9), na.rm=TRUE)
abs(quant[[1]]-quant[[2]])

# Question 4: Estimate the area.
longitude <- data$Longitude
points <- cbind(latitude, longitude)

# Question 5: Difference of calls between most and least popular hours.
time <- substr(data$Time, 12, 22)
is_am <- substr(time, 10, 11)=='AM'
time_am <- time[is_am]
time_pm <- time[!(is_am)]
hour_am <- as.numeric(substr(time_am,1,2))
hour_pm <- as.numeric(substr(time_pm,1,2))
a <- table(hour_am)
p <- table(hour_pm)
set <- c(a,p)
max(set)-min(set)

# Question 6: Standard deviation in seconds of the time between consecutive calls.
date_pre <- mdy_hms(data$Time)
date <- sort(date_pre)
is_na <- date==date[1]
ndate <- date[!(is_na)]
dif <- c()
for (i in 2:length(ndate)){
    dif <- c(dif, ndate[i]-ndate[i-1])
}
sd(dif)
