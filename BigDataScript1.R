# -------------------------------------
# Script created by Andrew Dumit for work at the UCL Data Science Student Challenge
# Script worked on by team 'Sleep on the Job' Recevied 3rd place in the hackathon
# 
# Purpose of script: Join together 6 sources of data that create one large df of sleep time series data.
# Possible issues: Running the entire script at once will crash a 16GB of RAM machine, not all of the data can be loaded into your workspace at once
# the script must be ran in parts and loaded separately with removes for data that's done being processed
# If trying to run this script on your own, be advised to save current dataframes as rdas often for faster load times
# If interested in running the script, please email andrew.dumit@gmail.com
# Data can be found on the project peach website (not sure where). Was downloaded from here: http://kenji.azurewebsites.net/?p=301, but this was a temporary website for the hackathon
# Apologies for the messy code.
# -------------------------------------

# ----- Read in all of the Q1 data
sleep_dat <- read.csv("/Users/Andrew/Desktop/UCL Stuff/UCLDSSC/PEACHData/BigData/PEACH_sleepQualityLog_1Q2015.csv")
fd_dat <- read.csv("/Users/Andrew/Desktop/UCL Stuff/UCLDSSC/PEACHData/BigData/PEACH_foodDiary_1Q2015.csv")
fd_all_dat <- read.csv("/Users/Andrew/Desktop/UCL Stuff/UCLDSSC/PEACHData/BigData/PEACH_allergyConsumption_1Q2015.csv")
af_dat <- read.csv("/Users/Andrew/Desktop/UCL Stuff/UCLDSSC/PEACHData/BigData/PEACH_atmosphericFactorsLog_1Q2015.csv")
user_dat <- read.csv("/Users/Andrew/Desktop/UCL Stuff/UCLDSSC/PEACHData/up_implicit_data.csv")
fd_sent_dat <- read.csv("/Users/Andrew/Desktop/UCL Stuff/UCLDSSC/PEACHData/BigData/PEACH_sentimentFeedback_1Q2015.csv", 
                        row.names = NULL, col.names = c("UserId", "DateTime", "Feedback", "Symptoms", "Symptoms2", "Symptoms3", "Symptoms4", "Symptoms5"),
                        header = F)
# ------------------------
# ------ Read in the Q2 data when needed
sleep_dat <- read.csv("/Users/Andrew/Desktop/UCL Stuff/UCLDSSC/PEACHData/BigData/PEACH_sleepQualityLog_1Q2015.csv")
fd_dat <- read.csv("/Users/Andrew/Desktop/UCL Stuff/UCLDSSC/PEACHData/BigData/PEACH_foodDiary_1Q2015.csv")
fd_all_dat <- read.csv("/Users/Andrew/Desktop/UCL Stuff/UCLDSSC/PEACHData/BigData/PEACH_allergyConsumption_1Q2015.csv")
af_dat <- read.csv("/Users/Andrew/Desktop/UCL Stuff/UCLDSSC/PEACHData/BigData/PEACH_atmosphericFactorsLog_1Q2015.csv")
user_dat <- read.csv("/Users/Andrew/Desktop/UCL Stuff/UCLDSSC/PEACHData/up_implicit_data.csv")
fd_sent_dat <- read.csv("/Users/Andrew/Desktop/UCL Stuff/UCLDSSC/PEACHData/BigData/PEACH_sentimentFeedback_1Q2015.csv", 
                        row.names = NULL, col.names = c("UserId", "DateTime", "Feedback", "Symptoms", "Symptoms2", "Symptoms3", "Symptoms4", "Symptoms5"),
                        header = F)
# -----------------------------

# ---- Load rda files if they were saved ----
# load("/Users/Andrew/fd_all_dat_BLD_with_meal_allergen_interactions.rda")
# load("/Users/Andrew/sleep_data_full.rda")
# load("/Users/Andrew/fd_dat_sub.rda")
# load("/Users/Andrew/dinner_dat_sub.rda")
# load("/Users/Andrew/af_dat_sub.rda")
# load("/Users/Andrew/Q1_dinners.rda")
# load("/Users/Andrew/sleep_datQ1.rda")
# load("/Users/Andrew/fd_sent_dat_sub.rda")
# load("/Users/Andrew/Q2_dinner_dat_sub.rda")
# --------------------


# --- Preprocess fd_all_dat ---
bad_users <- as.numeric(names(which(table(sleep_dat$UserId) < 91))) # Remove the users that doesn't have 90 days of data
sleep_dat_sub <- subset(sleep_dat, !(UserId %in% bad_users)) # Remove bad users from sleep
fd_all_dat_sub <- subset(fd_all_dat, !(UserId %in% bad_users)) # Remove bad users from fd_dat_all
fd_dat_sub <- subset(fd_dat, !(UserId %in% bad_users)) # Remove bad users from fd_dat
fd_all_dat$DateTimeStr <- as.character(fd_all_dat$DateTime) # Make a string of the DateTime for splitting later
fd_dat_sub$DateTimeStr <- as.character(fd_dat_sub$DateTime)
af_dat_sub <- subset(af_dat, !(ID %in% bad_users))
fd_sent_dat_sub <- subset(fd_sent_dat, !(UserId %in% bad_users))

# Add breakfast lunch and dinner columns and remove type of food columns for joining
fd_dat_sub$Breakfast = rep(c(1,0,0), nrow(sleep_dat_sub))
fd_dat_sub$Lunch = rep(c(0,1,0), nrow(sleep_dat_sub))
fd_dat_sub$Dinner = rep(c(0,0,1), nrow(sleep_dat_sub))
fd_dat_sub_BLD = fd_dat_sub[,-3]

# Add Breakfast, Lunch, and Dinner data to fd_all_dat
library(plyr)
fd_all_dat_BLD <- join(fd_all_dat_sub, fd_dat_sub_BLD, by = c("UserId", "DateTime"))

# Get just the day
fd_all_dat_BLD$DateTimeStr <- as.character(fd_all_dat_BLD$DateTime)
fd_all_dat_BLD$Date <- as.numeric(unlist(strsplit(fd_all_dat_BLD$DateTimeStr, " "))[seq(1, (nrow(fd_all_dat_BLD) * 2), by = 2)])

# Add a column for each factor of allergen
fd_all_dat_BLD$Apple <- 0
fd_all_dat_BLD$Apple[fd_all_dat_BLD$Name_of_Allergen == "Apple"] <- fd_all_dat_BLD$Quantity[fd_all_dat_BLD$Name_of_Allergen == "Apple"]
fd_all_dat_BLD$Beef <- 0
fd_all_dat_BLD$Beef[fd_all_dat_BLD$Name_of_Allergen == "Beef"] <- fd_all_dat_BLD$Quantity[fd_all_dat_BLD$Name_of_Allergen == "Beef"]
fd_all_dat_BLD$Cow_Milk <- 0
fd_all_dat_BLD$Cow_Milk[fd_all_dat_BLD$Name_of_Allergen == "Cow Milk"] <- fd_all_dat_BLD$Quantity[fd_all_dat_BLD$Name_of_Allergen == "Cow Milk"]
fd_all_dat_BLD$Dried_Fruit <- 0
fd_all_dat_BLD$Dried_Fruit[fd_all_dat_BLD$Name_of_Allergen == "Dried Fruit"] <- fd_all_dat_BLD$Quantity[fd_all_dat_BLD$Name_of_Allergen == "Dried Fruit"]
fd_all_dat_BLD$Egg <- 0
fd_all_dat_BLD$Egg[fd_all_dat_BLD$Name_of_Allergen == "Egg"] <- fd_all_dat_BLD$Quantity[fd_all_dat_BLD$Name_of_Allergen == "Egg"]
fd_all_dat_BLD$Fish <- 0
fd_all_dat_BLD$Fish[fd_all_dat_BLD$Name_of_Allergen == "Fish"] <- fd_all_dat_BLD$Quantity[fd_all_dat_BLD$Name_of_Allergen == "Fish"]
fd_all_dat_BLD$Gluten <- 0
fd_all_dat_BLD$Gluten[fd_all_dat_BLD$Name_of_Allergen == "Gluten"] <- fd_all_dat_BLD$Quantity[fd_all_dat_BLD$Name_of_Allergen == "Gluten"]
fd_all_dat_BLD$Peanut <- 0
fd_all_dat_BLD$Peanut[fd_all_dat_BLD$Name_of_Allergen == "Peanut"] <- fd_all_dat_BLD$Quantity[fd_all_dat_BLD$Name_of_Allergen == "Peanut"]
fd_all_dat_BLD$Sesame_Seeds <- 0
fd_all_dat_BLD$Sesame_Seeds[fd_all_dat_BLD$Name_of_Allergen == "Sesame Seeds"] <- fd_all_dat_BLD$Quantity[fd_all_dat_BLD$Name_of_Allergen == "Sesame Seeds"]
fd_all_dat_BLD$Shellfish <- 0
fd_all_dat_BLD$Shellfish[fd_all_dat_BLD$Name_of_Allergen == "Shellfish"] <- fd_all_dat_BLD$Quantity[fd_all_dat_BLD$Name_of_Allergen == "Shellfish"]
fd_all_dat_BLD$Soy <- 0
fd_all_dat_BLD$Soy[fd_all_dat_BLD$Name_of_Allergen == "Soy"] <- fd_all_dat_BLD$Quantity[fd_all_dat_BLD$Name_of_Allergen == "Soy"]
fd_all_dat_BLD$Tree_Nut <- 0
fd_all_dat_BLD$Tree_Nut[fd_all_dat_BLD$Name_of_Allergen == "Tree Nut"] <- fd_all_dat_BLD$Quantity[fd_all_dat_BLD$Name_of_Allergen == "Tree Nut"]
fd_all_dat_BLD$Wheat <- 0
fd_all_dat_BLD$Wheat[fd_all_dat_BLD$Name_of_Allergen == "Wheat"] <- fd_all_dat_BLD$Quantity[fd_all_dat_BLD$Name_of_Allergen == "Wheat"]

# Add time data
fd_all_dat_BLD$Time <- unlist(strsplit(fd_all_dat_BLD$DateTimeStr, " "))[seq(2, (nrow(fd_all_dat_BLD) * 2), by = 2)]
fd_dat_sub$DateTimeStr <- as.character(fd_dat_sub$DateTime)
fd_dat_sub$Time <- unlist(strsplit(fd_dat_sub$DateTimeStr, " "))[seq(2, (nrow(fd_dat_sub) * 2), by = 2)]

# Need to make a new dataframe with one row per day per user
fd_all_dat_BLD_sub <- fd_all_dat_BLD[,-c(2, 3, 4, 5)]
allergens <- c("Apple", "Beef", " Cow_Milk", "Dried_Fruit", "Egg", " Fish", "Gluten", "Peanut", "Sesame_Seeds",
               "Shellfish", "Soy", "Tree_Nut", "Wheat")
meals <- c("Breakfast", "Lunch", "Dinner")
fd_names <- names(fd_all_dat_BLD)
for (i in 10:22) {
  for (j in 6:8) {
    fd_all_dat_BLD[paste0(fd_names[j], "_", fd_names[i])] <- fd_all_dat_BLD[,i] * fd_all_dat_BLD[,j]
  }
}

# Create just a dinner dataframe from the dinner columns
dinner_dat <- fd_all_dat_BLD[,c(1, 9, 23, which(grepl("Dinner", names(fd_all_dat_BLD))))]
# Get only the rows that actually contain dinner info
dinner_dat_sub <- dinner_dat[which(dinner_dat$Dinner == 1),]

# Function to apply column sums to 
col_sum <- function(df) {
  return(colSums(df))
}

# ---- Speed test for 1000 users, 177303 users total in data set, but the time scaling isn't strictly linear
# Should be pretty fast though
# a <- subset(dinner_dat_sub, UserId < 1000)
# a$UserIdDate <- paste0(a$UserId, a$Date)
# system.time(
#   b <- by(a, a$UserId, function(x) colSums(x[,5:17]))
# )
# ------------------
system.time(
  b <- by(dinner_dat_sub, dinner_dat_sub$UserId, function(x) colSums(x[,5:17]))
)

# Get all of the relevant information
Q1_dinners <- matrix(unlist(b), ncol = 13, byrow = T)
Q1_dinners <- as.data.frame(Q1_dinners)
Q1_dinners$UserId <- names(b)
dinner_dat_time_sub <- dinner_dat_sub[,c(1,2,3)]
Q1_dinner_times <- fd_dat_sub$Time[seq(3, (nrow(fd_dat_sub)), by = 3)]

# Add the data to sleep_dat
sleep_dat_sub$Dinner_Time <- Q1_dinner_times
sleep_dat_sub$Dinner_Hour <- as.numeric(substr(sleep_dat_sub$Dinner_Time, 1, 2))
sleep_dat_sub$Dinner_Minute <- as.numeric(substr(sleep_dat_sub$Dinner_Time, 4, 5))
sleep_dat_sub$Dinner_Second <- as.numeric(substr(sleep_dat_sub$Dinner_Time, 7, 8))

# Sort the sleep_data first by dateTime and then by userid
sleep_dat_sub_ordered <- sleep_dat_sub[with(sleep_dat_sub, order(DateTime, UserId)), ]

# Add atmospheric data, now that sleep dat is ordered, the ids match up
# Proof that they line up for first 10, should generalize:
# sleep_dat_sub_ordered$UserId[1:10]
# af_dat$UserId[1:10]
af_dat_uv <- af_dat_sub[seq(1, nrow(af_dat_sub), by = 3),]
sleep_dat_sub_ordered$uv_level <- af_dat_uv$EXPOSURE_LEVEL
sleep_dat_sub_ordered$uv_feedback <- af_dat_uv$USER.S_FEEDBACK
af_dat_aqi <- af_dat_sub[seq(3, nrow(af_dat_sub), by = 3),]
sleep_dat_sub_ordered$aqi_level <- af_dat_aqi$EXPOSURE_LEVEL
sleep_dat_sub_ordered$aqi_feedback <- af_dat_aqi$USER.S_FEEDBACK
af_dat_pollen <- af_dat_sub[seq(2, nrow(af_dat_sub), by = 3),]
sleep_dat_sub_ordered$pollen_level <- af_dat_pollen$EXPOSURE_LEVEL
sleep_dat_sub_ordered$pollen_feedback <- af_dat_pollen$USER.S_FEEDBACK

# Add overall diet data
names(Q1_dinners) <- c(paste0("Dinner_", c("Apple", "Beef", "Cow_Milk", "Dried_Fruit", "Egg", "Fish", "Gluten", "Peanut", "Sesame_Seeds",
                       "Shellfish", "Soy", "Tree_Net", "Wheat")), "UserId")
sleep_dat_sub_full <- join(sleep_dat_sub_ordered, Q1_dinners, type = "inner", by = "UserId")

# Add implicit user data
sleep_data_full <- join(sleep_dat_sub_full, user_dat, by = "UserId")


# ---- Make a map?? ---- No, map is ugly :(
# Wanted to make a map visualization for sleep, but location doesn't matter much. So it was scrapped.
library(RgoogleMaps)
MyMap <- GetMap(center = c(lat = 51, lon = 0), destfile = "londonMap.png", zoom = 10)
ColorMap()
library(ggmap)
map <- get_map(location = 'London', zoom = 4)
mapPoints <- ggmap(map) +
  geom_point(aes(x = Longitude, y = Latitude, color = meanSleep), data = sleep_dat_mapping_data, alpha = .5) +
  scale_colour_gradient2(low = "red", mid = "orange", midpoint = 2.7, high = "blue", guide = "colourbar")
mapPoints
meanSleep <- function(df) {
  return(mean(df$Sleep_Quality_Value))
}
small_sleep_data <- subset(sleep_data_full, Longitude >= -10 & Longitude <= 40 & Latitude >= 35 & Latitude <= 60)
meanSleepTimes <- by(small_sleep_data, small_sleep_data$UserId, function(x) meanSleep(x))
meanSleep <- as.vector(unlist(meanSleepTimes))
sleep_dat_mapping_data <- small_sleep_data[!duplicated(small_sleep_data[,c('UserId')]),]
sleep_dat_mapping_data$meanSleep <- meanSleep
uk_sleep <- subset(sleep_dat_mapping_data, Longitude >= -10 & Longitude <= 3 & Latitude <= 60 & Latitude >= 50)
mean(uk_sleep$meanSleep)
port_sleep <- subset(sleep_dat_mapping_data, Longitude >= -10 & Longitude <= -8 & Latitude <= 45 & Latitude >= 35)
mean(port_sleep$meanSleep)
spain_sleep <- subset(sleep_dat_mapping_data, Longitude >= -8 & Longitude <= -2 & Latitude <= 45 & Latitude >= 35)
mean(spain_sleep$meanSleep)
italy_sleep <- subset(sleep_dat_mapping_data, Longitude >= 6 & Longitude <= 16 & Latitude <= 48 & Latitude >= 35)
mean(italy_sleep$meanSleep)

us_sleep <- subset(sleep_data_full, Longitude >= -125 & Longitude <= -65 & Latitude <= 48 & Latitude >= 25)
meanSleepTimes_us <- by(us_sleep, us_sleep$UserId, function(x) meanSleep(x))
us_sleep$meanSleep <- as.vector(unlist(meanSleepTimes_us))
mean(us_sleep$meanSleep)
# -------- End map ----------



# ---- Join some more data!! ---- Not enough social media data, please do not use
sm_dat <- read.csv("/Users/Andrew/Desktop/UCL Stuff/UCLDSSC/PEACHData/BigData/sm_data.csv")
sm_dat$DateTimeStr <- as.character(sm_dat$DateTime)
sm_dat$Date <- as.numeric(unlist(strsplit(sm_dat$DateTimeStr, " "))[seq(1, (nrow(sm_dat) * 2), by = 2)])

count_tweets <- function(df) {
  return(nrow(df))
}
test_dat <- subset(sm_dat, UserId < 50000)
num_tweets4 <- by(test_dat, interaction(test_dat$UserId, test_dat$Date), function(x) count_tweets(x))

num_tweets_vector <- as.vector(unlist(num_tweets))
num_tweets_vector[is.na(num_tweets_vector)] <- 0
# ---------- End social media data --------

# ---- Add food diary sentiment data ----
fd_sent_dat_dinner <- fd_sent_dat_sub[seq(3, (nrow(fd_sent_dat_sub)), by = 3),]
fd_sent_dat_dinner$DateTimeStr <- as.character(fd_sent_dat_dinner$DateTime)
fd_sent_dat_dinner$Date <- as.numeric(unlist(strsplit(fd_sent_dat_dinner$DateTimeStr, " "))[seq(1, (nrow(fd_sent_dat_dinner) * 2), by = 2)])
fd_sent_dat_dinner$UserId <- as.numeric(as.character(fd_sent_dat_dinner$UserId))
fd_sent_dat_dinner_ordered <- fd_sent_dat_dinner[with(fd_sent_dat_dinner, order(Date, UserId)), ]
fd_sent_dat_dinner_sub <- subset(fd_sent_dat_dinner_ordered, UserId %in% sleep_data_full$UserId)
sleep_data_full$Symptom_Feedback <- fd_sent_dat_dinner_sub$Feedback
# Add a column for each of the types of disease.
sleep_data_full$have_fatigue <- 0
sleep_data_full$have_fatigue[fd_sent_dat_dinner_sub$Feedback == 1 & 
                               (fd_sent_dat_dinner_sub$Symptoms == "Fatigue" | fd_sent_dat_dinner_sub$Symptoms2 == "Fatigue" |
                                  fd_sent_dat_dinner_sub$Symptoms3 == "Fatigue" | fd_sent_dat_dinner_sub$Symptoms4 == "Fatigue" |
                                  fd_sent_dat_dinner_sub$Symptoms5 == "Fatigue")] <- 1
sleep_data_full$have_bloating <- 0
sleep_data_full$have_bloating[fd_sent_dat_dinner_sub$Feedback == 1 & 
                               (fd_sent_dat_dinner_sub$Symptoms == "Bloating" | fd_sent_dat_dinner_sub$Symptoms2 == "Bloating" |
                                  fd_sent_dat_dinner_sub$Symptoms3 == "Bloating" | fd_sent_dat_dinner_sub$Symptoms4 == "Bloating" |
                                  fd_sent_dat_dinner_sub$Symptoms5 == "Bloating")] <- 1
sleep_data_full$have_dizziness <- 0
sleep_data_full$have_dizziness[fd_sent_dat_dinner_sub$Feedback == 1 & 
                                (fd_sent_dat_dinner_sub$Symptoms == "Dizziness" | fd_sent_dat_dinner_sub$Symptoms2 == "Dizziness" |
                                   fd_sent_dat_dinner_sub$Symptoms3 == "Dizziness" | fd_sent_dat_dinner_sub$Symptoms4 == "Dizziness" |
                                   fd_sent_dat_dinner_sub$Symptoms5 == "Dizziness")] <- 1
sleep_data_full$have_headache <- 0
sleep_data_full$have_headache[fd_sent_dat_dinner_sub$Feedback == 1 & 
                                 (fd_sent_dat_dinner_sub$Symptoms == "Headache" | fd_sent_dat_dinner_sub$Symptoms2 == "Headache" |
                                    fd_sent_dat_dinner_sub$Symptoms3 == "Headache" | fd_sent_dat_dinner_sub$Symptoms4 == "Headache" |
                                    fd_sent_dat_dinner_sub$Symptoms5 == "Headache")] <- 1
sleep_data_full$have_congestion <- 0
sleep_data_full$have_congestion[fd_sent_dat_dinner_sub$Feedback == 1 & 
                                (fd_sent_dat_dinner_sub$Symptoms == "Nasal Congestion" | fd_sent_dat_dinner_sub$Symptoms2 == "Nasal Congestion" |
                                   fd_sent_dat_dinner_sub$Symptoms3 == "Nasal Congestion" | fd_sent_dat_dinner_sub$Symptoms4 == "Nasal Congestion" |
                                   fd_sent_dat_dinner_sub$Symptoms5 == "Nasal Congestion")] <- 1
sleep_data_full$have_upset_stomach <- 0
sleep_data_full$have_upset_stomach[fd_sent_dat_dinner_sub$Feedback == 1 & 
                                  (fd_sent_dat_dinner_sub$Symptoms == "Upset Stomach" | fd_sent_dat_dinner_sub$Symptoms2 == "Upset Stomach" |
                                     fd_sent_dat_dinner_sub$Symptoms3 == "Upset Stomach" | fd_sent_dat_dinner_sub$Symptoms4 == "Upset Stomach" |
                                     fd_sent_dat_dinner_sub$Symptoms5 == "Upset Stomach")] <- 1

# --- Fix data format for model training ---- !Code from Federico Fontana!
X = sleep_data_full
X$Sleep_Quality_Value = NULL
X$DateTime = NULL
X$Dinner_Time = NULL
X$Nationality = NULL
X$Current_City_of_Domicile = NULL
X$Dinner_Dried_Fruit = NULL # All values are zeros!
X$Symptom_Feedback = NULL
X$Current_Smoker = as.numeric(X$Current_Smoker) - 1
X$Alcoholic = as.numeric(X$Alcoholic) - 1
X$Outgoing_Social_Life = as.numeric(X$Outgoing_Social_Life) - 1
X$Outdoors_Social_Life = as.numeric(X$Outdoors_Social_Life) - 1
y = sleep_data_full$Sleep_Quality_Value

# City_of_Domicile_Type
City_of_Domicile_Type_UrbanTown = as.numeric(X$City_of_Domicile_Type==0)
City_of_Domicile_Type_SuburbanTown = as.numeric(X$City_of_Domicile_Type==1)
City_of_Domicile_Type_RuralTown = as.numeric(X$City_of_Domicile_Type==2)
X$City_of_Domicile_Type = NULL
X = cbind(X, City_of_Domicile_Type_UrbanTown,
          City_of_Domicile_Type_SuburbanTown,
          City_of_Domicile_Type_RuralTown)
# Income_Bracket
Income_Bracket_Poor = as.numeric(X$Income_Bracket==0)
Income_Bracket_MiddleClass = as.numeric(X$Income_Bracket==1)
Income_Bracket_Wealthy = as.numeric(X$Income_Bracket==2)
X$Income_Bracket = NULL
X = cbind(X, Income_Bracket_Poor, Income_Bracket_MiddleClass, Income_Bracket_Wealthy)
# Education
Education_None = as.numeric(X$Education==0)
Education_Primary = as.numeric(X$Education==1)
Education_Secondary = as.numeric(X$Education==2)
Education_BachelorsDegree = as.numeric(X$Education==3)
Education_PdD = as.numeric(X$Education==4)
X$Education = NULL
X = cbind(X, Education_None, Education_Primary,
          Education_Secondary, Education_BachelorsDegree,
          Education_PdD)
# Type of Work
Type_of_Work_Sedentary = as.numeric(X$Type_of_Work=="sedentary")
Type_of_Work_Active_indoors = as.numeric(X$Type_of_Work=="active_indoors")
Type_of_Work_Active_outdoors = as.numeric(X$Type_of_Work=="active_outdoors")
X$Type_of_Work = NULL
X = cbind(X, Type_of_Work_Sedentary, Type_of_Work_Active_indoors,
          Type_of_Work_Active_outdoors)
sleep_data_full = X
sleep_data_full$Sleep_Quality_Value = y
# --------------- End fix data -------------

# ----- Code to save, load and edit the data for binary classification -----
# Used to change the loaded data, which was saved with missing columns initially.
save(sleep_data_full, file = "Q2_fixed_sleep_data_full.rda")
load("/Users/Andrew/Q2_fixed_sleep_data_full.rda")
sleep_data_full_Q2 <- sleep_data_full
rm(sleep_data_full)
load("/Users/Andrew/fixed_sleep_data_full.rda")
sleep_data_full$binary_sleep_quality <- 0
sleep_data_full$binary_sleep_quality[sleep_data_full$Sleep_Quality_Value > 2.5] <- 1
sleep_data_full$binary_sleep_quality_2_7 <- 0
sleep_data_full$binary_sleep_quality_2_7[sleep_data_full$Sleep_Quality_Value > 2.7] <- 1
sleep_data_full_Q2$Dinner_HourTime <- sleep_data_full_Q2$Dinner_Hour + sleep_data_full_Q2$Dinner_Minute/60
sleep_data_full_Q2 <- sleep_data_full_Q2[,-c(2:4)]
sleep_data_full <- sleep_data_full[,order(names(sleep_data_full))]
sleep_data_full_Q2 <- sleep_data_full_Q2[,order(names(sleep_data_full_Q2))]
# --------- End data editing. --------

# ----- Test glmnet model -----
# Split data into training, validation, and test set or into just training, validation, and Q2 data
# Train on the data, pick best lambda, retrain on all training data, and test on Q2
# Below here may need user editing depending on which lambda is best from the split.
# -----------------------------
library(glmnet)
all_ids <- unique(sleep_data_full$UserId)
validation_ids <- sample(all_ids, length(all_ids)*.1)
no_validation_ids <- all_ids[!(all_ids %in% validation_ids)]
test_ids <- sample(no_validation_ids, length(no_validation_ids)*.15)
train_set <- subset(sleep_data_full, !(UserId %in% test_ids) & !(UserId %in% validation_ids))
validation_set <- subset(sleep_data_full, UserId %in% validation_ids)
test_set <- subset(sleep_data_full, UserId %in% test_ids)

train_set <- subset(sleep_data_full, UserId %in% no_validation_ids)
validation_set <- subset(sleep_data_full, UserId %in% validation_ids)

glmnet.mod <- glmnet(x = as.matrix(train_set[,c(1:4, 8:50, 52:54, 56)]), y = train_set$Sleep_Quality_Value,
                     family = "gaussian", alpha = 1, lambda = seq(0.001, 0.05, by = 0.001))
preds1 <- predict(glmnet.mod, as.matrix(validation_set[,c(1:4, 8:50, 52:54, 56)]))
errors <- c()
for (i in 1:50) {
  errors[i] <- mean(abs(preds1[,i] - validation_set$Sleep_Quality_Value))
}
which(errors == min(errors))
mean(abs(median(train_set$Sleep_Quality_Value) - validation_set$Sleep_Quality_Value))

preds42 <- preds1[,42]
errors42 <- c()
for (i in seq(0.1, 0.9, by = 0.05)) {
  preds_made <- preds42
  preds_made[preds_made > i] <- 1
  preds_made[preds_made <= i] <- 0
  errors42[length(errors42) + 1] <- mean(abs(preds_made - validation_set$binary_sleep_quality))
}
min(errors42)

best_betas <- glmnet.mod$beta[,which(errors == min(errors))]
best_betas[order(best_betas)]

glmnet.mod.final <- glmnet(x = as.matrix(sleep_data_full[,c(1:4, 8:50, 52:54, 56)]), y = sleep_data_full$Sleep_Quality_Value,
                     family = "gaussian", alpha = 1, lambda = 0.01)
preds.final <- predict(glmnet.mod.final, as.matrix(sleep_data_full_Q2[,c(1:4, 8:50, 52:54, 56)]))
mean(abs(sleep_data_full_Q2$Sleep_Quality_Value - preds.final))







