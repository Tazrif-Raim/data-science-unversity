dataset <- read.csv("", header=TRUE, sep=",")
summary(dataset)
View(dataset)
names(dataset)
noMissingDataset <- na.omit(dataset)
summary(noMissingDataset)
#gender

barplot(table(dataset$gender), 
        main = "Gender Distribution", 
        xlab = "Gender", 
        ylab = "Count", 
        col = c("black","cyan", "pink")
)

#age
hist(dataset$age,main="Age Distribution", xlab="Age", xlim = c(0,300),ylim=c(0,30), breaks=30)
boxplot(dataset$age, main = "Age Distribution", ylab = "Age")
summary(dataset$age)
#hypertension
barplot(table(dataset$hypertension), main = "Hypertension Distribution", xlab = "Hypertension", ylab = "Count",ylim=c(0,120), col = c("lightgreen", "pink"))
#heart disease ok
barplot(table(dataset$heart_disease), main = "Heart Disease Distribution", xlab = "Heart Disease", ylab = "Count",ylim=c(0,120), col = c("lightgreen", "pink"))
#smoking_history
barplot(table(dataset$smoking_history), main = "Smoking_History Distribution", xlab = "Smoking History", ylab = "Count",ylim=c(0,120), col = c("black", "pink","red","blue","lightgreen","purple", "yellow"))
#bmi
hist(dataset$bmi,main="BMI Distribution", xlab="BMI", xlim = c(-40,80),ylim=c(0,70), breaks=20)
boxplot(dataset$bmi, main = "BMI Distribution", ylab = "BMI")
summary(dataset$bmi)
#HbA1c_level ok
hist(dataset$HbA1c_level,
     main="HbA1c_level Distribution", 
     xlab="HbA1c_level", 
     xlim = c(3,10),
     ylim=c(0,60), 
     breaks=7
)
boxplot(dataset$HbA1c_level, 
        main = "HbA1c_level Distribution", 
        ylab = "HbA1c_level"
)
#blood_glucose_level ok
hist(dataset$blood_glucose_level,main="blood_glucose_level Distribution", xlab="blood_glucose_level", xlim = c(60,340),ylim=c(0,60), breaks=10)
boxplot(dataset$blood_glucose_level, main = "blood_glucose_level Distribution", ylab = "blood_glucose_level")
summary(dataset$blood_glucose_level)
#diabetes ok
barplot(table(dataset$diabetes), main = "Diabetes Distribution", xlab = "Diabetes", ylab = "Count",ylim=c(0,80), col = c("lightgreen", "pink"))
table(dataset$diabetes)
summary(dataset)
sum(is.na(dataset$age))

sum(dataset$gender == "" | dataset$gender == " ")

table(dataset$gender)

table(dataset$smoking_history)

newDataset <- dataset
most_frequent_gender <- names(sort(table(newDataset$gender), decreasing = TRUE)[1])
most_frequent_gender
newDataset$gender[newDataset$gender == "" | newDataset$gender == " "] <- most_frequent_gender
table(dataset$gender)
table(newDataset$gender)
barplot(table(newDataset$gender), 
        main = "Gender Distribution", 
        xlab = "Gender", 
        ylab = "Count", 
        col = c("cyan", "pink")
)

newDataset <- dataset
newDataset$gender[newDataset$gender == "" | newDataset$gender == " "] <- "Unknown"
table(dataset$gender)
table(newDataset$gender)
barplot(table(newDataset$gender), 
        main = "Gender Distribution", 
        xlab = "Gender", 
        ylab = "Count", 
        col = c("cyan", "pink","black")
)

sum(!(dataset$gender %in% c("Male", "Female")))

newDataset <- dataset[dataset$gender != "" & dataset$gender != " ",]
table(newDataset$gender)
nrow(dataset)
nrow(newDataset)
barplot(table(newDataset$gender), 
        main = "Gender Distribution", 
        xlab = "Gender", 
        ylab = "Count", 
        col = c("cyan", "pink")
)


summary(dataset$age)
sum(is.na(dataset$age))
newDataset <- dataset[!is.na(dataset$age), ]
summary(newDataset$age)
sum(is.na(newDataset$age))
nrow(dataset)
nrow(newDataset)

summary(dataset$age)
hist(dataset$age,main="Age Distribution", xlab="Age", xlim = c(0,300),ylim=c(0,30), breaks=30)
newDataset <- dataset[!(dataset$age>150), ]
summary(newDataset$age)
hist(newDataset$age,main="Age Distribution", xlab="Age", xlim = c(0,100),ylim=c(0,30), breaks=10)

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

newDataset <- dataset[!(dataset$age>150), ]
mean_age <- mean(newDataset$age,na.rm = TRUE)
median_age <- median(newDataset$age, na.rm = TRUE)
mode_age <- getmode(newDataset$age)
mean_age <- round(mean_age, digits = 0)
median_age <- round(median_age, digits = 0)
mode_age <- round(mode_age, digits = 0)
mean_age
median_age
mode_age

newDataset<-dataset

newDataset$age[newDataset$age > 150] <- mean_age
summary(newDataset$age)

newDataset<-dataset

newDataset$age[newDataset$age > 150] <- median_age
summary(newDataset$age)

newDataset<-dataset

newDataset$age[newDataset$age > 150] <- mode_age
summary(newDataset$age)

summary(dataset$age)

newDataset <- dataset
newDataset$age[is.na(newDataset$age)] <- mean_age
summary(newDataset$age)

newDataset <- dataset
newDataset$age[is.na(newDataset$age)] <- median_age
summary(newDataset$age)

newDataset <- dataset
newDataset$age[is.na(newDataset$age)] <- mode_age
summary(newDataset$age)

summary(dataset$bmi)
hist(dataset$bmi,main="BMI Distribution", xlab="BMI", xlim = c(-40,80),ylim=c(0,70), breaks=20)

newDataset <- dataset[!(dataset$bmi<0), ]
summary(newDataset$bmi)
hist(newDataset$bmi,main="BMI Distribution", xlab="BMI", xlim = c(0,80),ylim=c(0,70), breaks=10)

newDataset <- dataset[!(dataset$bmi<0), ]
mean_bmi <- mean(newDataset$bmi)
median_bmi <- median(newDataset$bmi)

mean_bmi <- round(mean_bmi, digits = 2)
median_bmi <- round(median_bmi, digits = 2)

mean_bmi
median_bmi

newDataset<-dataset

newDataset$bmi[newDataset$bmi < 0] <- mean_bmi
summary(newDataset$bmi)

newDataset<-dataset

newDataset$bmi[newDataset$bmi < 0] <- median_bmi
summary(newDataset$bmi)

newDataset <- dataset

sum(newDataset$bmi<0)
newDataset[newDataset$bmi<0,]$bmi
newDataset[newDataset$bmi<0,]$bmi <- -(newDataset[newDataset$bmi<0,]$bmi)
summary(newDataset$bmi)

sum(is.na(dataset$hypertension))
barplot(table(dataset$hypertension), 
        main = "Hypertension Distribution", 
        xlab = "Hypertension", 
        ylab = "Count",
        ylim=c(0,120), 
        col = c("lightgreen", "pink")
)

newDataset <- dataset[!is.na(dataset$hypertension), ]
summary(newDataset$hypertension)
sum(is.na(newDataset$hypertension))
nrow(dataset)
nrow(newDataset)
table(dataset$hypertension)

newDataset <- dataset

most_frequent_hypertension <- names(sort(table(newDataset$hypertension), decreasing = TRUE)[1])
most_frequent_hypertension
newDataset$hypertension[is.na(newDataset$hypertension)] <- most_frequent_hypertension
table(dataset$hypertension)
table(newDataset$hypertension)

dataset <- read.csv("C://Users//Admin//Desktop//IDS//Mid Project//Dataset_MIdterm_sectoin(B).csv", header=TRUE, sep=",")
summary(dataset)

most_frequent_gender <- names(sort(table(dataset$gender), decreasing = TRUE)[1])
most_frequent_gender
dataset$gender[dataset$gender == "" | dataset$gender == " "] <- most_frequent_gender
table(dataset$gender)
barplot(table(dataset$gender), 
        main = "Gender Distribution", 
        xlab = "Gender", 
        ylab = "Count", 
        col = c("cyan", "pink")
)

newDataset <- dataset[!(dataset$age>150), ]
mean_age <- mean(newDataset$age,na.rm = TRUE)
median_age <- median(newDataset$age, na.rm = TRUE)
mean_age <- round(mean_age, digits = 0)
median_age <- round(median_age, digits = 0)
mean_age
median_age

dataset$age[dataset$age > 150] <- mean_age
dataset$age[is.na(dataset$age)] <- mean_age
summary(dataset)

most_frequent_hypertension <- names(sort(table(dataset$hypertension), decreasing = TRUE)[1])
most_frequent_hypertension
dataset$hypertension[is.na(dataset$hypertension)] <- most_frequent_hypertension

most_frequent_smoking_history <- names(sort(table(dataset$smoking_history), decreasing = TRUE)[1])
most_frequent_smoking_history
dataset$smoking_history[dataset$smoking_history == "" | dataset$smoking_history == " "] <- most_frequent_smoking_history

summary(dataset)

sum(dataset$bmi<0)
dataset[dataset$bmi<0,]$bmi
dataset[dataset$bmi<0,]$bmi <- -(dataset[dataset$bmi<0,]$bmi)
summary(dataset$bmi)
