dataset <- read.csv("", header=TRUE, sep=",")

summary(dataset$age)
View(dataset)
names(dataset)
noMissingDataset <- na.omit(dataset)
summary(noMissingDataset)


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
hist(dataset$age,main="Age Distribution", xlab="Age", xlim = c(0,300),ylim=c(0,30), breaks=30)
boxplot(dataset$age, main = "Age Distribution", ylab = "Age")

newDataset <- dataset[!(dataset$age>150), ]
mean_age <- mean(newDataset$age,na.rm = TRUE)
median_age <- median(newDataset$age, na.rm = TRUE)
mean_age <- round(mean_age, digits = 0)
median_age <- round(median_age, digits = 0)
mean_age
median_age

dataset$age[dataset$age > 150] <- mean_age
dataset$age[is.na(dataset$age)] <- mean_age
summary(dataset$age)
hist(dataset$age,main="Age Distribution", xlab="Age", xlim = c(0,100),ylim=c(0,40), breaks=10)
boxplot(dataset$age, main = "Age Distribution", ylab = "Age")




summary(dataset$hypertension)
barplot(table(dataset$hypertension), main = "Hypertension Distribution", xlab = "Hypertension", ylab = "Count",ylim=c(0,120), col = c("lightgreen", "pink"))

most_frequent_hypertension <- names(sort(table(dataset$hypertension), decreasing = TRUE)[1])
most_frequent_hypertension
dataset$hypertension[is.na(dataset$hypertension)] <- most_frequent_hypertension

summary(dataset$hypertension)
barplot(table(dataset$hypertension), main = "Hypertension Distribution", xlab = "Hypertension", ylab = "Count",ylim=c(0,120), col = c("lightgreen", "pink"))

table(dataset$hypertension)





sum(is.na(dataset$heart_disease))
table(dataset$heart_disease)
barplot(table(dataset$heart_disease), main = "Heart_disease Distribution", xlab = "heart_disease", ylab = "Count",ylim=c(0,150), col = c("lightgreen", "pink"))
summary(dataset$diabetes)




sum(dataset$smoking_history == "" | dataset$smoking_history == " ")

table(dataset$smoking_history)

newDataset <- dataset
most_frequent_smoking_history <- names(sort(table(newDataset$smoking_history), decreasing = TRUE)[1])
most_frequent_smoking_history
newDataset$smoking_history[newDataset$smoking_history == "" | newDataset$smoking_history == " "] <- most_frequent_smoking_history
table(dataset$smoking_history)
table(newDataset$smoking_history)
barplot(table(newDataset$smoking_history), 
        main = "smoking_history Distribution", 
        xlab = "smoking_history", 
        ylab = "Count", 
        col = c("pink","red","blue","lightgreen","purple", "yellow")
)

newDataset <- dataset[dataset$smoking_history != "" & dataset$smoking_history != " ",]
table(newDataset$smoking_history)
nrow(dataset)
nrow(newDataset)
barplot(table(newDataset$smoking_history), 
        main = "smoking_history Distribution", 
        xlab = "smoking_history", 
        ylab = "Count", 
        col = c("pink","red","blue","lightgreen","purple", "yellow")
)

newDataset <- dataset
newDataset$smoking_history[newDataset$smoking_history == "" | newDataset$smoking_history == " "] <- "Unknown"
table(dataset$smoking_history)
table(newDataset$smoking_history)
barplot(table(newDataset$smoking_history), 
        main = "smoking_history Distribution", 
        xlab = "smoking_history", 
        ylab = "Count", 
        col = c("pink","red","blue","lightgreen","purple", "yellow","black")
)




summary(dataset$bmi)
sum(dataset$bmi<0)
dataset[dataset$bmi<0,]$bmi
dataset[dataset$bmi<0,]$bmi <- -(dataset[dataset$bmi<0,]$bmi)
summary(dataset$bmi)
hist(dataset$bmi,main="BMI Distribution", xlab="BMI", xlim = c(-40,80),ylim=c(0,70), breaks=20)
boxplot(dataset$bmi, main = "BMI Distribution", ylab = "BMI")




sum(is.na(dataset$HbA1c_level))
table(dataset$HbA1c_level)

hba1c_mean <- mean(dataset$HbA1c_level, na.rm = TRUE)
hba1c_sd <- sd(dataset$HbA1c_level, na.rm = TRUE)
hba1c_range <- range(dataset$HbA1c_level, na.rm = TRUE)
cat("HbA1c Level - Mean:", hba1c_mean, "SD:", hba1c_sd, "Range:", hba1c_range, "\n")

hist(dataset$HbA1c_level,main="HbA1c_level Distribution", xlab="HbA1c_level", xlim = c(3,10),ylim=c(0,60), breaks=7)
boxplot(dataset$HbA1c_level, main = "HbA1c_level Distribution", ylab = "HbA1c_level")



sum(is.na(dataset$blood_glucose_level))
table(dataset$blood_glucose_level)

glucose_mean <- mean(dataset$blood_glucose_level, na.rm = TRUE)
glucose_sd <- sd(dataset$blood_glucose_level, na.rm = TRUE)
glucose_range <- range(dataset$blood_glucose_level, na.rm = TRUE)
cat("Blood Glucose Level - Mean:", glucose_mean, "SD:", glucose_sd, "Range:", glucose_range, "\n")

hist(dataset$blood_glucose_level,main="blood_glucose_level Distribution", xlab="blood_glucose_level", xlim = c(60,340),ylim=c(0,60), breaks=10)
boxplot(dataset$blood_glucose_level, main = "blood_glucose_level Distribution", ylab = "blood_glucose_level")
summary(dataset$blood_glucose_level)



sum(is.na(dataset$diabetes))
table(dataset$diabetes)
barplot(table(dataset$diabetes), main = "Diabetes Distribution", xlab = "Diabetes", ylab = "Count",ylim=c(0,80), col = c("lightgreen", "pink"))
summary(dataset$diabetes)


dataset$gender <- ifelse(dataset$gender == "Female", 0, 1)
smoking_history_map <- c("current"=0, "ever"=1, "former"=2, "never"=3, "No Info"=4, "not current"=5)
dataset$smoking_history <- smoking_history_map[dataset$smoking_history]
View(dataset)
