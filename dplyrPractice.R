library(nycflights13)
library(tidyverse)

summary(flights)

#creating a subset from dataframe
temp <- select(flights, carrier, flight, dep_time)
summary(temp)

# exlcding certain columns
temp <- select(flights, -carrier, flight, -dep_time)

#sort flights in ascending oder of departure delay
# flights[order(dep_delay),]
flights <- arrange(flights, dep_delay)

#sorting last variable by descending variable
# flights[order(dep_delay, -dep_time),]
flights <- arrange(flights, dep_delay, desc(dep_time))

#look only at flights from JetBlue
jb_flights <- filter(flights, carrier == "B6")

# sort flights that are not delayed (time greater than 0)
#ontime_flights <- flights[flights$arr_delay <= 0, ]
ontime_flights <- filter(flights,arr_delay <= 0)

# Flights not from Newark
notewr_flights <- filter(flights, origin != "EWR")

#Flights not cancelled
notcanc_flights <- filter(flights, !is.na(dep_delay))

notcvg_flights <- filter (flights, dest != "CVG")

# delay in hours not minutes, create new column dep_delay_hrs
flights <- mutate(flights, dep_delay_hrs = dep_delay/60)

flights <- mutate(flights, dep_delay_pct = dep_delay/air_time)

# get rid f rows with NAs
nona_flights <- filter(flights, !is.na(dep_delay), !is.na(dep_time))


#Median distance of a flight for ach origin month combination
nona_flights <- group_by(nona_flights, origin, month)

ormnth_dist <- summarize(nona_flights, meddist = median(distance))










