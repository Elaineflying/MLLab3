set.seed(1234567890)
library(geosphere)
#library(ggplot2)

# Read data
stations <- read.csv("stations.csv", fileEncoding = "latin1")
temps <- read.csv("temps50k.csv")
st <- merge(stations, temps, by = "station_number")

# Define the point and date to predict
a <- 58.4274      # Latitude of the point to predict
b <- 14.826       # Longitude of the point to predict
date <- as.Date("2013-11-04")  # Date to predict

# Define the times to predict
times <- seq(from = as.POSIXct("04:00:00", format = "%H:%M:%S"),
             to = as.POSIXct("24:00:00", format = "%H:%M:%S"),
             by = "2 hours")

# Define parameters
h_distance <- 500000  # Smoothing coefficient for physical distance
h_date <- 8000      # Smoothing coefficient for date difference
h_time <- 12       # Smoothing coefficient for time difference

#h_distance <- 300000
#h_date <- 6000
#h_time <- 4

# Initialize the temperature vector
temp_1 <- vector(length = length(times))
temp_2 <- vector(length = length(times))

# Filter out measurements that are posterior to the day and hour of the forecast
st <- st[as.Date(st$date) <= date, ]

# Kernel function
kernel <- function(dist_h, h) {
  exp(-(dist_h^2) / (2 * h^2))
}

# Calculate temperature predictions using sum of three Gaussian kernels and multiply the kernels
for (i in seq_along(times)) {
  # Calculate distances
  dist_lat_lon <- distHaversine(matrix(c(b, a), nrow = 1), matrix(c(st$longitude, st$latitude), nrow = nrow(st)))
  dist_date <- as.numeric(as.Date(st$date) - date)
  dist_time <- as.numeric((as.POSIXct(st$time, format = "%H:%M:%S") - times[i]) / 60)

  # Calculate kernel values
  kernel_distance <- kernel(dist_lat_lon, h_distance)
  kernel_date <- kernel(dist_date, h_date)
  kernel_time <- kernel(dist_time, h_time)

  # Calculate the weights using sum of three Gaussian kernels
  weights_1 <- kernel_distance + kernel_date + kernel_time
  # Calculate the weights using multiply three Gaussian kernels
  weights_2 <- kernel_distance * kernel_date * kernel_time

  # Calculate the predicted temperature
  temp_1[i] <- sum(weights_1 * st$air_temperature) / sum(weights_1)
  temp_2[i] <- sum(weights_2 * st$air_temperature) / sum(weights_2)
}

par(mfrow=c(1,3))
plot(dist_lat_lon, kernel_distance, main = "Kernel for physical distance")
plot(dist_date, kernel_date, main = "Kernel for day")
plot(dist_time, kernel_time, main = "Kernel for hour")

# Plot the predicted temperatures
par(mfrow=c(1,1))
plot(times, temp_1, type = "o", col="red",
     ylim = c(4,9),
     xlab = "Time", ylab = "Temperature", main = "Temperature Forecast", xaxt = "n")
lines(times,temp_2, type = "o", col="purple")
axis.POSIXct(1, at=seq(as.POSIXct("04:00:00", format = "%H:%M:%S"), as.POSIXct("24:00:00", format = "%H:%M:%S"), by = "2 hours"), las=2)
legend(x = "topright",  text.font = 3,
       fill= c("red","purple"),
       legend=c("temperature by sum kernel", "temperature by multiiply kernel"))


#ggplot(data.frame(times = times, temperature_by_sum = temp_1, temperature_by_multiply = temp_2), aes(times)) +
#  geom_line(aes(y = temperature_by_sum, colour = "temperature_by_sum")) +
#  geom_line(aes(y = temperature_by_multiply, colour = "temperature_by_multiply")) +
#  scale_x_datetime(date_labels = "%H:%M", date_breaks = "2 hour",
#                   limits = c(as.POSIXct("04:00:00", format = "%H:%M:%S"),
#                              as.POSIXct("24:00:00", format = "%H:%M:%S")),
#                   expand = c(0,0)) +
#  labs(subtitle="multiply kernels VS sum kernels", title = "Temperature Forecast") +
#  xlab("Time") +
#  ylab("Temperature")
