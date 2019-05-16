#jiangshan
library(ggplot2)
library(caTools)
library(tidyverse)
library(mice)
library(lubridate)

#analysis.data
air.bnb <- read.csv('/Users/jiangshan/CU_Course/AA_Framework&Methods/Kaggle/analysisData.csv', stringsAsFactors = FALSE)
summary(air.bnb)

# scoring data
scoring.data <- read.csv('/Users/jiangshan/CU_Course/AA_Framework&Methods/Kaggle/scoringData.csv', stringsAsFactors = FALSE)


#square_feet --> convert NA using mean
air.bnb$square_feet[is.na(air.bnb$square_feet)] <- mean(air.bnb$square_feet, na.rm = TRUE)
summary(air.bnb$square_feet)

#weekly_price --> T or F
air.bnb$weekly_price[is.na(air.bnb$weekly_price)] <- 0
air.bnb$weekly_price <- as.factor(ifelse(air.bnb$weekly_price > 0, T,F))
summary(air.bnb$weekly_price)

#monthly_price -> T or F
air.bnb$monthly_price[is.na(air.bnb$monthly_price)]<- 0
air.bnb$monthly_price <- as.factor(ifelse(air.bnb$monthly_price > 0, T,F))
summary(air.bnb$monthly_price)

#security_deposit security_deposit --> NA convert to  0
air.bnb$security_deposit[is.na(air.bnb$security_deposit)]<- 0
summary(air.bnb$security_deposit)

#cleaning_fee cleaning_fee --> NA convert to  0 
air.bnb$cleaning_fee[is.na(air.bnb$cleaning_fee)]<- 0
summary(air.bnb$cleaning_fee)

#host_reponse_rate
air.bnb$host_response_rate <- as.numeric(gsub('%', '', air.bnb$host_response_rate))
air.bnb$host_response_rate[is.na(air.bnb$host_response_rate)]<- 0
summary(air.bnb$host_response_rate)

#host_is_super_host
air.bnb$host_is_superhost <- as.factor(air.bnb$host_is_superhost)
summary(air.bnb$host_is_superhost)

#has_availability
air.bnb$has_availability <- as.factor(air.bnb$has_availability)
air.bnb$has_availability <- as.numeric(air.bnb$has_availability)
str(air.bnb$has_availability)

#impute beds
air.bnb$beds[is.na(air.bnb$beds)] <- 1
summary(air.bnb$beds)


#is_location_exact
summary(air.bnb$is_location_exact)
air.bnb$is_location_exact <- as.factor(air.bnb$is_location_exact)


#last_review
summary(air.bnb$last_review)
air.bnb$last_review <- as.Date(air.bnb$last_review)
str(air.bnb$last_review)
air.bnb$last_review <- format(as.Date(air.bnb$last_review), "%Y")
summary(air.bnb$last_review)


#property_type
air.bnb$property_type <- as.factor(air.bnb$property_type)
scoring.data$property_type <- as.factor(scoring.data$property_type)


#
air.bnb$host_response_rate<- as.integer(air.bnb$host_response_rate)
air.bnb$host_response_time<- as.factor(air.bnb$host_response_time)
air.bnb$host_has_profile_pic<- as.factor(air.bnb$host_has_profile_pic)
air.bnb$host_identity_verified<- as.factor(air.bnb$host_identity_verified)
air.bnb$neighbourhood_group_cleansed<- as.factor(air.bnb$neighbourhood_group_cleansed)
air.bnb$room_type<- as.factor(air.bnb$room_type)
air.bnb$bed_type<- as.factor(air.bnb$bed_type)
air.bnb$instant_bookable<- as.factor(air.bnb$instant_bookable)
air.bnb$cancellation_policy<- as.factor(air.bnb$cancellation_policy)
air.bnb$last_review <- as.factor(air.bnb$last_review)

air.bnb1 <- na.omit(select(air.bnb,-id
                           ,-listing_url
                           ,-scrape_id
                           ,-last_scraped
                           ,-name
                           ,-summary
                           ,-space
                           ,-description
                           ,-experiences_offered
                           ,-neighborhood_overview
                           ,-notes
                           ,-transit
                           ,-access
                           ,-interaction
                           ,-house_rules
                           ,-thumbnail_url
                           ,-medium_url
                           ,-picture_url
                           ,-xl_picture_url
                           ,-host_id
                           ,-host_url
                           ,-host_since
                           ,-host_location
                           ,-host_about
                           ,-host_acceptance_rate
                           ,-host_thumbnail_url
                           ,-host_picture_url
                           ,-host_neighbourhood
                           ,-host_listings_count
                           ,-host_total_listings_count
                           ,-street
                           ,-neighbourhood
                           ,-neighbourhood_cleansed
                           ,-city
                           ,-state
                           ,-market
                           ,-smart_location
                           ,-country_code
                           ,-country
                           ,-calendar_updated
                           ,-calendar_last_scraped
                           ,-first_review
                           ,-requires_license
                           ,-license
                           ,-jurisdiction_names
                           ,-is_business_travel_ready
                           ,-require_guest_profile_picture
                           ,-require_guest_phone_verification
                           ,-calculated_host_listings_count
                           ,-host_verifications
                           ,-amenities
                           ,-host_name
                           ,-zipcode
))



#SCORING CLEANING

#square_feet --> convert NA using mean
scoring.data$square_feet[is.na(scoring.data$square_feet)] <- mean(scoring.data$square_feet, na.rm = TRUE)
summary(scoring.data$square_feet)

#weekly_price --> T or F
scoring.data$weekly_price[is.na(scoring.data$weekly_price)] <- 0
scoring.data$weekly_price <- as.factor(ifelse(scoring.data$weekly_price > 0, T,F))
summary(air.bnb$weekly_price)

#monthly_price -> T or F
scoring.data$monthly_price[is.na(scoring.data$monthly_price)]<- 0
scoring.data$monthly_price <- as.factor(ifelse(scoring.data$monthly_price > 0, T,F))
summary(scoring.data$monthly_price)

#security_deposit security_deposit --> NA convert to  0
scoring.data$security_deposit[is.na(scoring.data$security_deposit)]<- 0
summary(scoring.data$security_deposit)

#cleaning_fee cleaning_fee --> NA convert to  0 
scoring.data$cleaning_fee[is.na(scoring.data$cleaning_fee)]<- 0
summary(scoring.data$cleaning_fee)

#host_reponse_rate
scoring.data$host_response_rate <- as.numeric(gsub('%', '', scoring.data$host_response_rate))
scoring.data$host_response_rate[is.na(scoring.data$host_response_rate)]<- 0
summary(scoring.data$host_response_rate)

#host_is_super_host
scoring.data$host_is_superhost <- as.factor(scoring.data$host_is_superhost)
summary(scoring.data$host_is_superhost)

#has_availability
scoring.data$has_availability <- as.factor(scoring.data$has_availability)
scoring.data$has_availability <- as.numeric(scoring.data$has_availability)
str(scoring.data$has_availability)

#impute beds
scoring.data$beds[is.na(scoring.data$beds)] <- 1
summary(scoring.data$beds)


#is_location_exact
summary(scoring.data$is_location_exact)
scoring.data$is_location_exact <- as.factor(scoring.data$is_location_exact)


#last_review
summary(scoring.data$last_review)
scoring.data$last_review <- as.Date(scoring.data$last_review)
str(air.bnb$last_review)
scoring.data$last_review <- format(as.Date(scoring.data$last_review), "%Y")


#Levelling
scoring.data$property_type <- factor(scoring.data$property_type, levels = levels(air.bnb$property_type))
scoring.data$property_type [is.na(scoring.data$property_type)]<- "Other"
summary(scoring.data$property_type)

scoring.data$host_name <- factor(scoring.data$host_name, levels = levels(air.bnb$host_name))
summary(scoring.data$host_name)

scoring.data$last_review <- factor(scoring.data$last_review, levels = levels(air.bnb$last_review))
summary(scoring.data$last_review)

#equalize variables
scoring.data$host_response_rate<- as.integer(scoring.data$host_response_rate)
scoring.data$host_response_time<- as.factor(scoring.data$host_response_time)
scoring.data$host_has_profile_pic<- as.factor(scoring.data$host_has_profile_pic)
scoring.data$host_identity_verified<- as.factor(scoring.data$host_identity_verified)
scoring.data$neighbourhood_group_cleansed<- as.factor(scoring.data$neighbourhood_group_cleansed)
scoring.data$room_type<- as.factor(scoring.data$room_type)
scoring.data$bed_type<- as.factor(scoring.data$bed_type)
scoring.data$instant_bookable<- as.factor(scoring.data$instant_bookable)
scoring.data$cancellation_policy<- as.factor(scoring.data$cancellation_policy)
scoring.data$last_review <- as.factor(scoring.data$last_review)


library(randomForest)
library(caret)
Random_Forest <- randomForest(price~., data = air.bnb1, ntree = 500)
summary(Random_Forest)

predForest = predict(Random_Forest,newdata=test)
rmseForest = sqrt(mean((predForest-test$price)^2)); rmseForest


#prediction
pred = predict(Random_Forest,newdata = scoring.data)
summary(pred)

# constuct submission form 
submissionFile = data.frame(id = scoring.data$id, price = pred)
write.csv(submissionFile, 'Random_Forest_Shan_9.csv',row.names = F)


#Check Variables
summary(train$host_response_time)
summary(scoring.data$host_response_time)

summary(train$host_response_rate)
summary(scoring.data$host_response_rate)

summary(train$host_is_superhost)
summary(scoring.data$host_is_superhost)

summary(train$host_has_profile_pic)
summary(scoring.data$host_has_profile_pic)

summary(train$host_identity_verified)
summary(scoring.data$host_identity_verified)

summary(train$neighbourhood_group_cleansed)
summary(scoring.data$neighbourhood_group_cleansed)

summary(train$zipcode)
summary(scoring.data$zipcode)

summary(train$latitude)
summary(scoring.data$latitude)

summary(train$longitude)
summary(scoring.data$longitude)

summary(train$is_location_exact)
summary(scoring.data$is_location_exact)

summary(train$property_type)
summary(scoring.data$property_type)

summary(train$room_type)
summary(scoring.data$room_type)

summary(train$accommodates)
summary(scoring.data$accommodates)

summary(train$bathrooms)
summary(scoring.data$bathrooms)

summary(train$bedrooms)
summary(scoring.data$bedrooms)

summary(train$bed_type)
summary(scoring.data$bed_type)

summary(train$square_feet)
summary(scoring.data$square_feet)


summary(train$weekly_price) #try to replace NA
summary(scoring.data$weekly_price)

summary(train$monthly_price) #try to replace NA
summary(scoring.data$monthly_price)

summary(train$security_deposit)
summary(scoring.data$security_deposit)

summary(train$cleaning_fee)
summary(scoring.data$cleaning_fee)

summary(train$guests_included)
summary(scoring.data$guests_included)

summary(train$minimum_nights)
summary(scoring.data$minimum_nights)

summary(train$maximum_nights)
summary(scoring.data$maximum_nights)

summary(train$has_availability)
summary(scoring.data$has_availability)

summary(train$availability_30)
summary(scoring.data$availability_30)

summary(train$availability_60)
summary(scoring.data$availability_60)

summary(train$availability_90)
summary(scoring.data$availability_90)

summary(train$availability_365)
summary(scoring.data$availability_365)

summary(train$number_of_reviews)
summary(scoring.data$number_of_reviews)

summary(train$last_review)
summary(scoring.data$last_review)

summary(train$review_scores_accuracy)
summary(scoring.data$review_scores_accuracy)

summary(train$review_scores_cleanliness)
summary(scoring.data$review_scores_cleanliness)

summary(train$review_scores_checkin)
summary(scoring.data$review_scores_checkin)

summary(train$review_scores_communication)
summary(scoring.data$review_scores_communication)

summary(train$review_scores_location)
summary(scoring.data$review_scores_location)

summary(train$review_scores_value)
summary(scoring.data$review_scores_value)

summary(train$instant_bookable)
summary(scoring.data$instant_bookable)

summary(train$cancellation_policy)
summary(scoring.data$cancellation_policy)

#next 
#1. Edit last review to become month & year
#2. add host listing count & calendar updated
#3. Amenities (Make Dummy Variables from Words Frequency Table)