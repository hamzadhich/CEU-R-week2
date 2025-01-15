library(pairsD3)
library ("GGally")
library(ggplot2)
library(data.table)
library(nycflights13)
dt <- data.table(flights)
dt

#1 Count the number of flights originating from JFK
dt[origin == "JFK", .(number_of_flights_originating_from_JFK = .N)]

#2 Count the number of flights per month.
dt[, .(number_of_flights =.N),by = 'month'][order(month)]

#3 Visualize the number of flights per destination.
library(ggplot2)
ggplot(dt, aes(y = dest)) + geom_bar(fill = "darkblue") +
  labs(x = "Number of Flights", y = "Destination", title = "Number of Flights per Destination") +
  theme_minimal()

#4 Count the number of flights with an arrival delay of more than 100 mins.
dt[arr_delay>100, .(number_of_flights_with_an_arrival_delay_of_more_than_100_mins = .N)]

#5 Visualize the maximum arrival delay per destination.
dt_max_delay <- dt[, .(max_arr_delay = max(arr_delay, na.rm = TRUE)), by = dest]

ggplot(dt_max_delay, aes(x = reorder(dest, max_arr_delay), y = max_arr_delay)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  coord_flip() +
  labs(x = "Destination", y = "Max Arrival Delay (mins)", 
       title = "Maximum Arrival Delay per Destination") +
  theme_bw()

#6 Aggregate the min and max arrival delay per origin.
dt[, .(min_arr_delay = min(arr_delay, na.rm = TRUE), max_arr_delay = max(arr_delay, na.rm = TRUE)), by = origin]

#7 Visualize the distribution of the arrival delay per origin.  
ggplot(dt, aes(x = arr_delay)) + 
  geom_histogram(binwidth = 5, fill = "skyblue", color = "black") +
  facet_wrap(~ origin) +
  labs(x = "Arrival Delay (mins)", y = "Frequency", title = "Distribution of Arrival Delay per Origin") +
  theme_bw()

#8 Visualize the distribution of the arrival delay per destination.
ggplot(dt, aes(x = arr_delay)) + 
  geom_histogram(binwidth = 5, fill = "skyblue", color = "black") +
  facet_wrap(~ dest) +
  labs(x = "Arrival Delay (mins)", y = "Frequency", title = "Distribution of Arrival Delay per Destination") +
  theme_bw()


#9 List the top 5 destinations being the furthest from NYC.
dt[, .(total_distance = sum(distance, na.rm = TRUE)), by = dest][order(-total_distance)][1:5]

  
#10 How many flights were scheduled to depart before 11 am?
dt[sched_dep_time < 1100, .N]



