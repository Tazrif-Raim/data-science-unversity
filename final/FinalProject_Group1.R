dataset <- read.csv("", header=TRUE, sep=",")
summary(dataset)
View(dataset)

sum(is.na(dataset))
sum(dataset == "" | dataset ==" ")

unique_values <- sapply(dataset, unique)
unique_values


catRaised <- cut(dataset$raisedhands, breaks =c(0,30,60,101), labels=c('low', 'mid', 'high'), right=FALSE)
catRaised
barplot(table(catRaised), main = "Raised Hands Distribution", xlab="Raised Hands", ylab = "Num of times Raised Hands",ylim=c(0,200) )

catVisited <- cut(dataset$VisITedResources, breaks =c(0,20,70,101), labels=c('low', 'mid', 'high'), right=FALSE)
catVisited
barplot(table(catVisited), main = "VisITedResources Distribution", ylab = "Num of times go through course content",ylim=c(0,300) )

catAnnoun <- cut(dataset$AnnouncementsView, breaks =c(0,20,60,101), labels=c('low', 'mid', 'high'), right=FALSE)
catAnnoun
barplot(table(catAnnoun), main = "Announcements View Distribution", xlab = "Announcements View",ylim=c(0,300))

catDis <- cut(dataset$Discussion, breaks =c(0,40,101), labels=c('low', 'high'), right=FALSE)
catDis
barplot(table(catDis), main = "Discussion Distribution", xlab = "Discussion View",ylim=c(0,300))


dataset$raisedhands <- catRaised
dataset$VisITedResources <- catVisited
dataset$AnnouncementsView<- catAnnoun
dataset$Discussion <- catDis

summary(dataset)
View(dataset)

copy_dataset <- dataset

dataset <- copy_dataset

chi2 <- function(data_set, pred_class){
  for(c in names(data_set)){
    observed <- table(data_set[[c]], data_set[[pred_class]])
    
    row_totals <- rowSums(observed)
    col_totals <- colSums(observed)
    grand_total <- sum(observed)
    
    expected <- outer(row_totals, col_totals)/grand_total
    
    chi_squared <- sum((observed - expected)^2 / expected)
    
    df <- (nrow(observed) - 1) * (ncol(observed) - 1)
    
    p_value <- 1 - pchisq(chi_squared, df)
    
    if(p_value>0.05){
      data_set[[c]] <- NULL
    }
  }
  return(data_set)
}



naive_bayes_train <- function(prediction_dataset, prediction_class){
  classes <- sapply(prediction_dataset['Class'], unique)
  dataset_row_amount <- nrow(prediction_dataset)
  
  pred_classes <- list()
  d3 <- list(list(list()))
  
  for(c in classes){
    another_dataset <- prediction_dataset[prediction_dataset[prediction_class] == c,]
    row_amount = nrow(another_dataset)
    pred_classes[[c]] = row_amount/dataset_row_amount
    for(i in colnames(another_dataset)){
      unique_val = sapply(another_dataset[i], unique)
      for(x in unique_val){
        d3[[c]][[i]][[x]]=sum(another_dataset[i]==x)/row_amount
      }
    }
  }
  return(list(d3, pred_classes))
}


naive_bayes_test <- function(test_instance, Class, d3, pred_classes){
  t <- test_instance[, !colnames(test_instance) %in% c(Class)]
  a <- test_instance[[Class]]
  p <- pred_classes
  d <- d3
  temp <- list()
  for(c in names(p)){
    copy_t = t
    row_count = nrow(copy_t)
    j=1
    while(j<=row_count){
      ans = p[[c]]
      for(i in colnames(copy_t)){
        x = copy_t[[i]][j]
        ans = ans*(d[[c]][[i]][[x]])
      }
      temp[[as.character(j)]][[c]] = ans
      j = j+1
    }
  }
  largest_values <- list()
  for (i in 1:length(temp)) {
    categories <- names(temp[[i]])
    values1 <- unlist(temp[[i]])
    
    largest_category <- categories[which.max(values1)]
    if(length(largest_category)==0)
    {
      largest_category = "M"
    }
    largest_values[[i]] <- largest_category
  }
  m = length(a)
  mi = 1
  count1 = 0
  while(mi<=m){
    if(largest_values[mi]==a[mi]){
      count1 = count1+1
    }
    mi = mi+1
  }
  m = count1/m
  return(list(m, largest_values))
}

pre_processed_dataset <- chi2(dataset, "Class")
summary(pre_processed_dataset)

View(pre_processed_dataset)

set.seed(4)
shuffled_dataset <- pre_processed_dataset[sample(nrow(pre_processed_dataset)), ]

split_index <- round(nrow(shuffled_dataset) * 0.8)
training_set <- shuffled_dataset[1:split_index, ]
test_set <- shuffled_dataset[(split_index + 1):nrow(shuffled_dataset), ]
cat("Training set rows:", nrow(training_set), "\n")
cat("Test set rows:", nrow(test_set), "\n")

train_result <- naive_bayes_train(training_set, "Class")

d3 <- train_result[[1]]
pc <- train_result[[2]]
View(test_set)
accuracy <- naive_bayes_test(test_set, "Class", d3, pc)
accuracy

accuracy_value<- accuracy [[1]]
instance_value<- accuracy [[2]]
View(instance_value)
cat("Accuracy: ", accuracy_value, "\n")

for (i in 1:length(instance_value)) {
  cat("Instance", i, ": ", instance_value[[i]], "\n")
}


naive_bayes_test_ten <- function(test_instance, Class, d3, pred_classes){
  t <- test_instance[, !colnames(test_instance) %in% c(Class)]
  a <- test_instance[[Class]]
  p <- pred_classes
  d <- d3
  temp <- list()
  for(c in names(p)){
    copy_t = t
    row_count = nrow(copy_t)
    j=1
    while(j<=row_count){
      ans = p[[c]]
      for(i in colnames(copy_t)){
        x = copy_t[[i]][j]
        ans = ans*(d[[c]][[i]][[x]])
      }
      temp[[as.character(j)]][[c]] = ans
      j = j+1
    }
  }
  largest_values <- list()
  for (i in 1:length(temp)) {
    categories <- names(temp[[i]])
    values1 <- unlist(temp[[i]])
    
    largest_category <- categories[which.max(values1)]
    
    largest_values[[i]] <- largest_category
  }
  m = length(a)
  mi = 1
  count1 = 0
  while(mi<=m){
    if(largest_values[mi]==a[mi]){
      count1 = count1+1
    }
    mi = mi+1
  }
  m = count1/m
  return(m)
}

pre_processed_dataset <- chi2(dataset, "Class")
summary(pre_processed_dataset)

View(pre_processed_dataset)

set.seed(4)
shuffled_dataset <- pre_processed_dataset[sample(nrow(pre_processed_dataset)), ]

split_size <- nrow(shuffled_dataset) / 10

t_acc = 0

for (i in 1:10) {
  
  test_indices <- ((i - 1) * split_size + 1):(i * split_size)
  
  test_data <- shuffled_dataset[test_indices, ]
  train_data <- shuffled_dataset[-test_indices, ]
  
  tr <- naive_bayes_train(train_data, "Class")
  d3<- tr[[1]]
  pc <- tr[[2]]
  t_acc = t_acc+naive_bayes_test_ten(test_data, "Class", d3, pc)
}

accuracy <- t_acc/10
accuracy


evaluate_classifier <- function(actual, predicted, positive_class) {
  confusion_matrix <- table(actual, predicted)
  
  TP <- confusion_matrix[positive_class, positive_class]
  TN <- sum(diag(confusion_matrix)) - TP
  FP <- sum(confusion_matrix[positive_class, ]) - TP
  FN <- sum(confusion_matrix[, positive_class]) - TP
  
  recall <- TP / (TP + FN)
  precision <- TP / (TP + FP)
  f_measure <- 2 * (precision * recall) / (precision + recall)
  
  result <- list(
    confusion_matrix = confusion_matrix,
    recall = recall,
    precision = precision,
    f_measure = f_measure
  )
  
  return(result)
}

set.seed(4)
actual_labels <- test_set$Class

predicted_values <- unlist(instance_value)
predicted_values <- as.character(predicted_values)

positive_class <- "M"

evaluation_result <- evaluate_classifier(actual_labels, predicted_values, positive_class)

cat("Confusion Matrix:\n")
print(evaluation_result$confusion_matrix)

cat("\nRecall:", evaluation_result$recall, "\n")
cat("Precision:", evaluation_result$precision, "\n")
cat("F-measure:", evaluation_result$f_measure, "\n")

positive_class <- "L"

evaluation_result <- evaluate_classifier(actual_labels, predicted_values, positive_class)

cat("Confusion Matrix:\n")
print(evaluation_result$confusion_matrix)

cat("\nRecall:", evaluation_result$recall, "\n")
cat("Precision:", evaluation_result$precision, "\n")
cat("F-measure:", evaluation_result$f_measure, "\n")

positive_class <- "H"

evaluation_result <- evaluate_classifier(actual_labels, predicted_values, positive_class)

cat("Confusion Matrix:\n")
print(evaluation_result$confusion_matrix)

cat("\nRecall:", evaluation_result$recall, "\n")
cat("Precision:", evaluation_result$precision, "\n")
cat("F-measure:", evaluation_result$f_measure, "\n")