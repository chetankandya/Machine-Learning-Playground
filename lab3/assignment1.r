set.seed(1234567890)
#install.packages("geosphere")
library(geosphere)
stations <- read.csv("C:/Users/hp/Desktop/LiU Sem I/Machine Learning/Lab/ML Lab 3/stations.csv")
temps <- read.csv("C:/Users/hp/Desktop/LiU Sem I/Machine Learning/Lab/ML Lab 3/temps50k.csv")
#A join operation on "station_number"
st <- merge(stations,temps,by="station_number")
n = dim(st)[1]

#Kernel weighting factors
#chose a width that gives large kernel values to closer points and small values to distant points
h_distance <- 200000
h_date <- 40
h_time <- 4

#Create a vector of the point of interest
place_of_interest = c(58.3978, 15.5760) #Coordinates for Liu
date_of_interest <- as.Date("2021-12-09") # Using current date
times_of_interest = c("04:00:00", "06:00:00", "08:00:00", "10:00:00", "12:00:00", "14:00:00", "16:00:00", "18:00:00", 
            "20:00:00", "22:00:00", "24:00:00")


#Plotting kernel values as a function of distance

#Kernel values as function of distance
dist = seq(0, 200000, 1)
u = dist/h_distance
plot(dist, exp(-u^2), type="l", main="Plot of kernel values as a function of distances", xlab="Distance")

#Kernel values as function of dates
date = seq(-182, 182, 1)
u = date/h_date
plot(date, exp(-u^2), type="l", main="Plot of kernel values as a function of dates", xlab="Days")

#Kernel values as function of time
time = seq(-12, 12, 1)
u = time/h_time
plot(time, exp(-u^2), type="l", main="Plot of kernel values as a function of time", xlab="Hours")


#Removing data posterior to the daya nad hour of the chosen time
filter_st = function(date, time, data){
  return(data[which(as.numeric(difftime(strptime(paste(date, time, sep=" "), format="%Y-%m-%d %H:%M:%S"),
                                        strptime(paste(data$date, data$time, sep=" "),format="%Y-%m-%d %H:%M:%S")))>0), ])
}

#Gaussian function to calculate the euclidean normal values
gauss_values = function(difference, h){
  
  u = difference/h
  return (exp(-u^2))
}

#A gaussian function to calculate the difference in distance
gauss_dist = function(place, data, h) {
  lat = data$latitude
  long = data$longitude
  points = data.frame(lat,long)
  dist_diff = distHaversine(points, place)
  return (gauss_values(dist_diff, h))
}

#A gaussian function to calculate the difference in days
gauss_day = function(date, data, h){
  c_date = as.Date(data$date)
  diff = as.numeric(date-c_date)
  for (i in 1:length(diff)) {
    if (diff[i] > 365) {
      diff[i] = diff[i] %% 365
      if(diff[i]>182){
        diff[i]=365-diff[i]
      }
    }
  }
  return (gauss_values(diff, h))
}

#A gaussian function to calculate the difference in hours
gauss_hour = function(hour, data, h){
  c_hour = strptime(data$time, format="%H:%M:%S")
  c_hour = as.numeric(format(c_hour, format="%H"))
  hour = strptime(hour, format="%H:%M:%S")
  hour = as.numeric(format(hour, format="%H"))
  diff = abs(hour-c_hour)
  for (i in 1:length(diff)){
    if(diff[i]>12){
      diff[i] = 24-diff[i]
    }
  }
  return (gauss_values(diff, h))
}


kernel_sum = c()
kernel_mult = c()

#Looping through time array to claculate the kernel values
for (time in times_of_interest) {
  filtered_data = filter_st(date_of_interest, time, st)
  kernel_dist = gauss_dist(place_of_interest, filtered_data, h_distance)
  kernel_day = gauss_day(date_of_interest, filtered_data, h_date)
  kernel_time = gauss_hour(time, filtered_data, h_time)
  
  #Adding all the kernel values
  sum_kernel = kernel_dist+kernel_day+kernel_time
  temp_s = sum(sum_kernel * filtered_data$air_temperature)/sum(sum_kernel)
  
  #Multiplying all the kernel values
  mult_kernel = kernel_dist*kernel_day*kernel_time
  temp_m = sum(mult_kernel * filtered_data$air_temperature)/sum(mult_kernel)
  
  kernel_sum = c(kernel_sum, temp_s)
  kernel_mult = c(kernel_mult, temp_m)
}


plot(kernel_sum, type="o", main ="Using Sum Kernel", xlab="Time", 
     ylab="Temperature")
axis(1, at=1:length(times_of_interest), labels=times_of_interest)

plot(kernel_mult, type="o", main="Using Multiplication kernel", xlab="Time",
     ylab="Temperature")
axis(1, at=1:length(times_of_interest), labels=(times_of_interest))