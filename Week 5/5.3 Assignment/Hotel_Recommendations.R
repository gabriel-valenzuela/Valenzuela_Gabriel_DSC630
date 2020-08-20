

# Import libraries
library(ggplot2)
library(dplyr)
library(tidyr)

# Import Data

train <- read.csv('subset_train.csv')
destinations <- read.csv('destinations.csv')

# Clean NA records
train <- na.omit(train)

apply(train, 2, function(x) any(is.na(x)))



### Formating Variables ###

# Date Time
train$date_time <- as.character(train$date_time) 
train$date_time <- substr(train$date_time,1,nchar(train$date_time)-12)


train <- train %>%
          tidyr::separate(date_time, c("date_year", "date_month"))

train$date_year <- as.integer(train$date_year) 
train$date_month <- as.integer(train$date_month)

# Check in date
train$srch_ci <- as.character(train$srch_ci) 
train$srch_ci <- substr(train$srch_ci,1,nchar(train$srch_ci)-3)


train <- train %>%
          tidyr::separate(srch_ci, c("CheckIn_year", "CheckIn_month"))

train$CheckIn_year <- as.integer(train$CheckIn_year) 
train$CheckIn_month <- as.integer(train$CheckIn_month)

# check out date
train$srch_co <- as.character(train$srch_co) 
train$srch_co <- substr(train$srch_co,1,nchar(train$srch_co)-3)


train <- train %>%
          tidyr::separate(srch_co, c("CheckOut_year", "CheckOut_month"))

train$CheckOut_year <- as.integer(train$CheckOut_year) 
train$CheckOut_month <- as.integer(train$CheckOut_month)

# Orig_distance_distance
train$orig_destination_distance <- as.integer(train$orig_destination_distance)

### EDA ###
train <- na.omit(train)

hotel_clusters <- train$hotel_cluster

str(train)

summary(train)

hist(hotel_clusters)

cor_test <- cor(train$hotel_cluster, train)
round(cor_test, 2)

# Merge the data sets into one ('train' and 'destinations')

merged_hotel <- left_join(train, destinations, 
                          by = c("srch_destination_id" = "srch_destination_id"))

merged_hotel <- na.omit(merged_hotel)
head(merged_hotel)
str(merged_hotel)

# Export to CSV
write.csv(merged_hotel,"C:\\Users\\Gabe\\Documents\\Bellevue University\\Predictive Analytics\\Week 5\\Merged_Prepared_Train.csv")
#, row.names = FALSE