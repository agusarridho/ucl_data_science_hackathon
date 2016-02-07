# Load data
af_data <- read.csv("~/Desktop/PEACH Data/af_data.csv")
af_data = af_data[order(af_data$ID), ]
fd_allergy_data <- read.csv("~/Desktop/PEACH Data/fd_allergy_data.csv")
fd_allergy_data = fd_allergy_data[order(fd_allergy_data$UserId), ]
fd_fooddiary_data <- read.csv("~/Desktop/PEACH Data/fd_fooddiary_data.csv")
fd_fooddiary_data = fd_fooddiary_data[order(fd_fooddiary_data$UserId), ]
fd_sentimentfeedback_data <- read.csv("~/Desktop/PEACH Data/fd_sentimentfeedback_data.csv")
fd_sentimentfeedback_data = fd_sentimentfeedback_data[order(fd_sentimentfeedback_data$UserId), ]
iot_bodytemp_data <- read.csv("~/Desktop/PEACH Data/iot_bodytemp_data.csv")
iot_bodytemp_data = iot_bodytemp_data[order(iot_bodytemp_data$UserId), ]
iot_heartrate_data <- read.csv("~/Desktop/PEACH Data/iot_heartrate_data.csv")
iot_heartrate_data = iot_heartrate_data[order(iot_heartrate_data$UserId), ]
iot_sleepquality_data <- read.csv("~/Desktop/PEACH Data/iot_sleepquality_data.csv")
iot_sleepquality_data = iot_sleepquality_data[order(iot_sleepquality_data$UserId), ]
rs_data <- read.csv("~/Desktop/PEACH Data/rs_data.csv", skip=6)
rs_data = rs_data[order(rs_data$X.f0.fs24..cf0.UserId), ]
sm_data <- read.csv("~/Desktop/PEACH Data/sm_data.csv")
sm_data = sm_data[order(sm_data$UserId), ]
up_explicit_data <- read.csv("~/Desktop/PEACH Data/up_explicit_data.csv")
up_explicit_data = up_explicit_data[order(up_explicit_data$UserId), ]
up_implicit_data <- read.csv("~/Desktop/PEACH Data/up_implicit_data.csv")
up_implicit_data = up_implicit_data[order(up_implicit_data$UserId), ]

# Now I remove the 2nd of Jan since many other data sets doesn't provide info 
# for this date. 200000id
iot_sleepquality_data = iot_sleepquality_data[iot_sleepquality_data$DateTime==20150101, ]
temp = fd_allergy_data
namesAllergens = as.character(unique(fd_allergy_data$Name_of_Allergen))
namesList = c(paste(namesAllergens, "_Breakfast", sep=""),
              paste(namesAllergens, "_Lunch", sep=""),
              paste(namesAllergens, "_Dinner", sep=""))
temp = fd_fooddiary_data
row.names(temp) <- NULL
dinner = temp[as.logical(rep(c(0,0,1)), 200000), ]

dinnerNames = unique(dinner$Name_of_Meal)
sleepInfo = matrix(NaN, nrow=length(dinnerNames)-1, ncol=5)
for (i in 1:(length(dinnerNames)-1)) {
    myDinner = dinnerNames[i] # pork belly buns, this: 199816
    # Now find the id that have eaten myDinner
    IDs = dinner$UserId[dinner$Name_of_Meal==myDinner]
    logical = iot_sleepquality_data$UserId %in% IDs
    sleepQuality = iot_sleepquality_data$Sleep_Quality_Value[logical]
    sleepInfo[i, ] = c(mean(sleepQuality), median(sleepQuality),
                       sd(sleepQuality), skewness(sleepQuality),
                       kurtosis(sleepQuality))
}

plot(up_implicit_data$Hours_of_Sleep_per_Night, iot_sleepquality_data$Sleep_Quality_Value)


