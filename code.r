library(here)
library("tidyverse")
library("ggplot2")
library("lubridate")
library("geosphere")
library("gridExtra") 
library("ggmap") 

tripdata_2020_04 <- read.csv("../input/cyclistic/202004-divvy-tripdata/202004-divvy-tripdata.csv")
tripdata_2020_05 <- read.csv("../input/cyclistic/202005-divvy-tripdata/202005-divvy-tripdata.csv")
tripdata_2020_06 <- read.csv("../input/cyclistic/202006-divvy-tripdata/202006-divvy-tripdata.csv")
tripdata_2020_07 <- read.csv("../input/cyclistic/202007-divvy-tripdata/202007-divvy-tripdata.csv")
tripdata_2020_08 <- read.csv("../input/cyclistic/202008-divvy-tripdata/202008-divvy-tripdata.csv")
tripdata_2020_09 <- read.csv("../input/cyclistic/202009-divvy-tripdata/202009-divvy-tripdata.csv")
tripdata_2020_10 <- read.csv("../input/cyclistic/202010-divvy-tripdata/202010-divvy-tripdata.csv")
tripdata_2020_11 <- read.csv("../input/cyclistic/202011-divvy-tripdata/202011-divvy-tripdata.csv")
tripdata_2020_12 <- read.csv("../input/cyclistic/202012-divvy-tripdata/202012-divvy-tripdata.csv")
tripdata_2021_01 <- read.csv("../input/cyclistic/202101-divvy-tripdata/202101-divvy-tripdata.csv")
tripdata_2021_02 <- read.csv("../input/cyclistic/202102-divvy-tripdata/202102-divvy-tripdata.csv")
tripdata_2021_03 <- read.csv("../input/cyclistic/202103-divvy-tripdata/202103-divvy-tripdata.csv")

print("####### GLIMPSE #######")
glimpse(all_tripdata)
print("####### SUMMARY #######")
summary(all_tripdata)

#drop all NA:
all_tripdata_clean <- drop_na(all_tripdata)

#create new columns.
#separate the dates into month, day, year and day of the week:
all_tripdata_clean$date <- as.Date(all_tripdata_clean$started_at) 
all_tripdata_clean$month <- format(as.Date(all_tripdata_clean$date), "%m")
all_tripdata_clean$day <- format(as.Date(all_tripdata_clean$date), "%d")
all_tripdata_clean$year <- format(as.Date(all_tripdata_clean$date), "%Y")
all_tripdata_clean$day_of_week <- format(as.Date(all_tripdata_clean$date), "%A")

#create columns with the duration of the ride, distance traveled, and speed:
#the ride lenght in seconds:
all_tripdata_clean$ride_length <- difftime(all_tripdata_clean$ended_at,all_tripdata_clean$started_at)
#the ride distance traveled in km
all_tripdata_clean$ride_distance <- distGeo(matrix(c(all_tripdata_clean$start_lng, all_tripdata_clean$start_lat), ncol = 2), matrix(c(all_tripdata_clean$end_lng, all_tripdata_clean$end_lat), ncol = 2))
all_tripdata_clean$ride_distance <- all_tripdata_clean$ride_distance/1000
#the speed in Km/h
all_tripdata_clean$ride_speed = c(all_tripdata_clean$ride_distance)/as.numeric(c(all_tripdata_clean$ride_length), units="hours")

#the dataframe includes a few hundred entries when bikes were taken out of docks and checked for quality by Divvy or ride_length was negative:
all_tripdata_clean <- all_tripdata_clean[!(all_tripdata_clean$start_station_name == "HQ QR" | all_tripdata_clean$ride_length<0),]


#the average distance, distance for both the casual and member type users
userType_means <- all_tripdata_clean %>% group_by(member_casual) %>% summarise(mean_time = mean(ride_length),mean_distance = mean(ride_distance))

membervstime <- ggplot(userType_means) + 
  geom_col(mapping=aes(x=member_casual,y=mean_time,fill=member_casual), show.legend = FALSE)+
  labs(title = "Mean travel time by User type",x="User Type",y="Mean time in sec")

membervsdistance <- ggplot(userType_means) + 
  geom_col(mapping=aes(x=member_casual,y=mean_distance,fill=member_casual), show.legend = FALSE)+
  labs(title = "Mean travel distance by User type",x="User Type",y="Mean distance In Km",caption = "Data by Motivate International Inc")

grid.arrange(membervstime, membervsdistance, ncol = 2)  

#number of rides diferences by weekday
all_tripdata_clean %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length),.groups = 'drop') %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge") +
  labs(title = "Number of rides by User type during the week",x="Days of the week",y="Number of rides",caption = "Data by Motivate International Inc", fill="User type") +
  theme(legend.position="top")

#Create a new data frame with only the rows with info in the "bike type" column
with_bike_type <- all_tripdata_clean %>% filter(rideable_type=="classic_bike" | rideable_type=="electric_bike")

#the bike type usage by user type
with_bike_type %>%
  group_by(member_casual,rideable_type) %>%
  summarise(totals=n(), .groups="drop")  %>%
  
  ggplot()+
  geom_col(aes(x=member_casual,y=totals,fill=rideable_type), position = "dodge") + 
  labs(title = "Bike type usage by user type",x="User type",y=NULL, fill="Bike type") +
  scale_fill_manual(values = c("classic_bike" = "#746F72","electric_bike" = "#FFB100")) +
  theme_minimal() +
  theme(legend.position="top")

#usage by both user types during a week
with_bike_type %>%
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual,rideable_type,weekday) %>%
  summarise(totals=n(), .groups="drop") %>%
  
  ggplot(aes(x=weekday,y=totals, fill=rideable_type)) +
  geom_col(, position = "dodge") + 
  facet_wrap(~member_casual) +
  labs(title = "Bike type usage by user type during a week",x="User type",y=NULL,caption = "Data by Motivate International Inc") +
  scale_fill_manual(values = c("classic_bike" = "#746F72","electric_bike" = "#FFB100")) +
  theme_minimal() +
  theme(legend.position="none")


#table only for the most popular routes (>250 times)
coordinates_table <- all_tripdata_clean %>% 
  filter(start_lng != end_lng & start_lat != end_lat) %>%
  group_by(start_lng, start_lat, end_lng, end_lat, member_casual, rideable_type) %>%
  summarise(total = n(),.groups="drop") %>%
  filter(total > 250)

#create two sub tables for each user type
casual <- coordinates_table %>% filter(member_casual == "casual")
member <- coordinates_table %>% filter(member_casual == "member")

#bounding box coordinates for ggmap
chi_bb <- c(
  left = -87.700424,
  bottom = 41.790769,
  right = -87.554855,
  top = 41.990119
)

#the stamen map of Chicago
chicago_stamen <- get_stamenmap(
  bbox = chi_bb,
  zoom = 12,
  maptype = "toner"
)

#plot data
ggmap(chicago_stamen,darken = c(0.8, "white")) +
  geom_curve(casual, mapping = aes(x = start_lng, y = start_lat, xend = end_lng, yend = end_lat, alpha= total, color=rideable_type), size = 0.5, curvature = .2,arrow = arrow(length=unit(0.2,"cm"), ends="first", type = "closed")) +
  coord_cartesian() +
  labs(title = "Most popular routes by casual users",x=NULL,y=NULL, color="User type", caption = "Data by Motivate International Inc") +
  theme(legend.position="none")

ggmap(chicago_stamen,darken = c(0.8, "white")) +
  geom_curve(member, mapping = aes(x = start_lng, y = start_lat, xend = end_lng, yend = end_lat, alpha= total, color=rideable_type), size = 0.5, curvature = .2,arrow = arrow(length=unit(0.2,"cm"), ends="first", type = "closed")) +  
  coord_cartesian() +
  labs(title = "Most popular routes by annual members",x=NULL,y=NULL, caption = "Data by Motivate International Inc") +
  theme(legend.position="none")