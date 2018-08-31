library(ggplot2)
library(dplyr)
library(lubridate)
library(ggmap)

data <- read.csv('../Data/Cleaned/Filtered.csv', header=TRUE, sep=',' ,na.strings = c("", "Unspecified"))

data$DATE_TIME <- paste(data$DATE,data$TIME)
data$DATE <- mdy(data$DATE)
data$DATE_TIME <-mdy_hm(data$DATE_TIME)
data$day <- wday(data$DATE_TIME,label = T)
data$month <- month(data$DATE_TIME,label = T)
data$hour <- hour(data$DATE_TIME)
data$year <- year(data$DATE)
data$ON.STREET.NAME <- toupper(data$ON.STREET.NAME)
data$OFF.STREET.NAME <- toupper(data$OFF.STREET.NAME)
data$CROSS.STREET.NAME <- toupper(data$CROSS.STREET.NAME)

data_year <- data %>% group_by(year) %>% summarise(n=n())

ggplot(data = data_year, aes(x=year, y=n)) + geom_line() + labs(title='Number of collisions over the year', x='Year', y='Number of collisions')

data_factors <- data %>% filter(!is.na(CONTRIBUTING.FACTOR.VEHICLE.1)) %>% group_by(CONTRIBUTING.FACTOR.VEHICLE.1) %>% summarise(n=n()) %>% na.omit()

data_factors$CONTRIBUTING.FACTOR.VEHICLE.1 <- factor(data_factors$CONTRIBUTING.FACTOR.VEHICLE.1, levels = data_factors$CONTRIBUTING.FACTOR.VEHICLE.1[order(data_factors$n)])

ggplot(data = data_factors, aes(x=data_factors$CONTRIBUTING.FACTOR.VEHICLE.1, y=data_factors$n)) +
  geom_bar(position="dodge",stat = "identity")+ coord_flip() + labs(title = 'Number of accidents by contributing factor') + labs(x='Number of datalisions') + labs(y='Factor')


data_hour <- data %>% group_by(hour) %>% summarise(n=n()) %>% na.omit()


ggplot(data = data_hour, aes(x=data_hour$hour, y=data_hour$n)) + geom_line() + labs(title='Collisions by hour', x='Hour', y='Number of colliisons')+ scale_x_continuous(breaks=data_hour$hour)

data_day <- data %>% group_by(day) %>% summarise(n=n()) %>% na.omit()

ggplot(data = data_day, aes(x=data_day$day, y=data_day$n)) + geom_bar(position = "dodge", stat="identity") + labs(title='Collisions by day', x='Day', y='Number of colliisons')


data_dayhour <- data %>% group_by(day, hour) %>% summarise(n=n()) %>% na.omit()

ggplot(data=data_dayhour, aes(x=hour, y=n, colour=day, group = day)) + geom_line() + labs(title='Number of accidents by day & hour', x='Hour', y='Number of collisions')

data_month <- data %>% group_by(month) %>% summarise(n=n()) %>% na.omit()

ggplot(data=data_month, aes(x=month, y=n,)) + geom_bar(position = "dodge", stat="identity") + labs(title='Number of accidents by month', x='Month', y='Number of collisions')

data_borough <- data %>% filter(BOROUGH!="") %>% group_by(BOROUGH) %>% summarise(n=n()) %>% na.omit()

ggplot(data=data_borough, aes(x=BOROUGH, y=n,)) + geom_bar(position = "dodge", stat="identity") + labs(title='Number of accidents by Borough', x='Borough', y='Number of collisions')

nyc <- get_map("new york",zoom=20)

data_cyclist_map <- data %>% select(LATITUDE, LONGITUDE, NUMBER.OF.CYCLIST.KILLED) %>%filter(NUMBER.OF.CYCLIST.KILLED!=0) %>% group_by(LATITUDE, LONGITUDE) %>% summarise(n=n()) %>% na.omit()


ggmap(nyc)+geom_point(data=data_cyclist_map, 
                      aes(x=LONGITUDE, y=LATITUDE, colour=n),size=5,alpha=0.2) +
  ggtitle("Cyclists Killed")+scale_color_continuous(low = "red",  high = "black")

data_Pedestrian_map <- data %>% select(LATITUDE, LONGITUDE, NUMBER.OF.PEDESTRIANS.KILLED) %>%filter(NUMBER.OF.PEDESTRIANS.KILLED!=0) %>% group_by(LATITUDE, LONGITUDE) %>% summarise(n=n()) %>% na.omit()


ggmap(nyc)+geom_point(data=data_Pedestrian_map, 
                      aes(x=LONGITUDE, y=LATITUDE, colour=n),size=5,alpha=0.2) +
  ggtitle("Pedestrians Killed")+scale_color_continuous(low = "red",  high = "black")


data_motorist_map <- data %>% select(LATITUDE, LONGITUDE, NUMBER.OF.MOTORIST.KILLED) %>%filter(NUMBER.OF.MOTORIST.KILLED!=0) %>% group_by(LATITUDE, LONGITUDE) %>% summarise(n=n()) %>% na.omit()


ggmap(nyc)+geom_point(data=data_motorist_map, 
                      aes(x=LONGITUDE, y=LATITUDE, colour=n),size=5,alpha=0.2) +
  ggtitle("Motorists Killed")+scale_color_continuous(low = "red",  high = "black")


data_on_street_top_10 <- data %>% filter(!is.na(ON.STREET.NAME)) %>% group_by(ON.STREET.NAME) %>% summarise(n=n()) %>% top_n(11) %>% top_n(-10) %>% arrange(desc(n))


data_cyclist_month <- data %>% filter(NUMBER.OF.CYCLIST.KILLED != 0 | NUMBER.OF.CYCLIST.INJURED != 0) %>% group_by(month) %>% summarise(n=n())

ggplot(data=data_cyclist_month, aes(x=month, y=n)) + geom_bar(position = "dodge", stat="identity") + labs(title='Number of Cyclists\' accidents by month', x='Month', y='Number of collisions')


data_alcohol <- data %>% filter(CONTRIBUTING.FACTOR.VEHICLE.1 == 'Alcohol Involvement') %>% group_by(day) %>% summarise(n=n())


data_nov_mar <- data %>% filter(month=='Nov' | month=='Mar')
data_nov_mar$dat <- day(data_nov_mar$DATE_TIME)
data_nov_mar <- data_nov_mar %>% group_by(month, dat) %>% summarise(n=n())

ggplot(data=data_alcohol, aes(x=day, y=n)) + geom_bar(position = "dodge", stat="identity") + labs(title='Accidents due to Alcohol by day', x='Day', y='Number of accidents')

ggplot(data=data_nov_mar, aes(x=dat, y=n, colour=month, group = month)) + geom_line() + labs(title='Number of accidents by date in November and March', x='Date', y='Number of collisions')

data_on_street_top_20 <- data %>% filter(!is.na(ON.STREET.NAME)) %>% group_by(ON.STREET.NAME) %>% summarise(n=n()) %>% na.omit() %>% top_n(21) %>% top_n(-20) %>% arrange(desc(n))

data_for_models <- data %>% select(BOROUGH, ON.STREET.NAME, NUMBER.OF.PERSONS.INJURED, NUMBER.OF.PERSONS.KILLED, NUMBER.OF.PEDESTRIANS.INJURED, NUMBER.OF.PEDESTRIANS.KILLED, NUMBER.OF.CYCLIST.INJURED, NUMBER.OF.CYCLIST.KILLED, NUMBER.OF.MOTORIST.INJURED, NUMBER.OF.MOTORIST.KILLED, day, hour, DATE_TIME) %>% filter(!is.na(BOROUGH) & !is.na(ON.STREET.NAME) & ON.STREET.NAME %in% data_on_street_top_20$ON.STREET.NAME)

data_for_models2 <- data %>% select(ON.STREET.NAME, day, hour, month) %>% filter(!is.na(ON.STREET.NAME) & ON.STREET.NAME %in% data_on_street_top_20$ON.STREET.NAME) %>% group_by(ON.STREET.NAME, month, day, hour) %>% summarise(n=n()) %>% select(ON.STREET.NAME, day, hour, n)

data_for_models2$n_normal = ifelse((data_for_models2$n / max(data_for_models2$n)) < 0.14, 0, 1)

write.csv(data_for_models2, file = '../Data/Cleaned/For_models_normal.csv')
