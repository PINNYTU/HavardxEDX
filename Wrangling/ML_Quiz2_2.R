library(caret)
data(iris)
iris <- iris[-which(iris$Species=='setosa'),]
y <- iris$Species
head(iris)
#Q7
 set.seed(2) # if using R 3.5 or earlier
# line of code
test_index <- createDataPartition(y,times=1,p=0.5,list=FALSE) 
test <- iris[test_index,]
train <- iris[-test_index,]

#Q8 Using only the train iris dataset,
#for each feature, perform a simple search to find the cutoff that produces the highest accuracy, 
#predicting virginica if greater than the cutoff and versicolor otherwise.
#Use the seq function over the range of each feature by intervals of 0.1 for this search.
x1<- iris$Sepal.Length 
x2<- iris$Sepal.Width
x3<- iris$Petal.Length
x4<- iris$Petal.Width

cutoff <- seq(min(x1),max(x1),by=0.1)
accuracy <- map_dbl(cutoff, function(x1){
  y_hat <- ifelse(train$Sepal.Length > x1, "virginica", "versicolor") %>% 
    factor(levels = levels(test$Species)) 
  mean(y_hat == train$Species)
})

data.frame(cutoff, accuracy) %>% 
  ggplot(aes(cutoff, accuracy)) + 
  geom_point() + 
  geom_line() 
max(accuracy)#0.7

best_cutoff <- cutoff[which.max(accuracy)]
best_cutoff
#------------------------------------
cutoff2 <- seq(min(x2),max(x2),by=0.1)
accuracy2 <- map_dbl(cutoff2, function(x2){
  y_hat <- ifelse(train$Sepal.Width > x2, "virginica", "versicolor") %>% 
    factor(levels = levels(test$Species)) 
  mean(y_hat == train$Species)
})

data.frame(cutoff2, accuracy2) %>% 
  ggplot(aes(cutoff2, accuracy2)) + 
  geom_point() + 
  geom_line() 
max(accuracy2)#0.62

cutoff[which.max(accuracy2)]

#------------------------------------
cutoff3 <- seq(min(x3),max(x3),by=0.1)
accuracy3 <- map_dbl(cutoff3, function(x3){
  y_hat <- ifelse(train$Petal.Length > x3, "virginica", "versicolor") 
  mean(y_hat == train$Species)
})

data.frame(cutoff3, accuracy3) %>% 
  ggplot(aes(cutoff3, accuracy3)) + 
  geom_point() + 
  geom_line() 
max(accuracy3)#0.96

#------------------------------------
cutoff4 <- seq(min(x4),max(x4),by=0.1)
accuracy4 <- map_dbl(cutoff4, function(x4){
  y_hat <- ifelse(train$Petal.Width > x4, "virginica", "versicolor") 
  mean(y_hat == train$Species)
})

data.frame(cutoff4, accuracy4) %>% 
  ggplot(aes(cutoff4, accuracy4)) + 
  geom_point() + 
  geom_line() 
max(accuracy4)# 0.94

######################################
#Combine function
app_data<- function(x){
    cutoff <- seq(min(x),max(x),by=0.1)
    map_dbl(cutoff, function(i){
      y_hat <- ifelse(x > i, "virginica", "versicolor")  
      mean(y_hat == train$Species)
    })
}
#select all
all<-sapply(train[,-5],app_data)
lapply(all, FUN=max )

######################################
#Q9
cutoff4 <- seq(min(x4),max(x4),by=0.1)
accuracy4<-  map_dbl(cutoff4, function(x4){
    y_hat <- ifelse(train$Petal.Length > x4, "virginica", "versicolor") %>%
      factor(levels = levels(test$Species))
    mean(y_hat == train$Species)
  })


best_cutoff<-cutoff[which.max(accuracy4)]

#change data to test
y_hat <- ifelse(test$Petal.Length  > best_cutoff, "virginica", "versicolor") %>% 
  factor(levels = levels(test$Species))
mean(y_hat == test$Species)

#Q10
#overall accuracy greater than 96% in the training data, 
#but the overall accuracy was lower in the test data. 
#This can happen often if we overtrain.
#t could be the case that a single feature is not the best choice.

#Combine function test data
app_data<- function(x){
  cutoff <- seq(min(x),max(x),by=0.1)
  map_dbl(cutoff, function(i){
    y_hat <- ifelse(x > i, "virginica", "versicolor")  
    mean(y_hat == test$Species)
  })
}
#select all
all<-sapply(test[,-5],app_data)
lapply(all, FUN=max )

#Q11
plot(iris,pch=21,bg=iris$Species)
#Optimize the the cutoffs for Petal.Length and Petal.Width separately in the train dataset by using the seq function with increments of 0.1.
  cutoff1 <- seq(min(train$Petal.Length),max(train$Petal.Length),by=0.1)
  cutoff2 <- seq(min(train$Petal.Width),max(train$Petal.Width),by=0.1)

 des<- function(x,y){
    y_hat <- ifelse(test$Petal.Length>length_cutoff | test$Petal.Width>width_cutoff, "virginica", "versicolor")  
    mean(y_hat == test$Species)
 }
 max(mapply(des, cutoff1, cutoff2))
 
 #ans
  
  length_predictions <- sapply(cutoff1,function(i){
    y_hat <- ifelse(train$Petal.Length>i,'virginica','versicolor')
    mean(y_hat==train$Species)
  })
  length_cutoff <- cutoff1[which.max(length_predictions)] # 4.7
  
  width_predictions <- sapply(cutoff2,function(i){
    y_hat <- ifelse(train$Petal.Width>i,'virginica','versicolor')
    mean(y_hat==train$Species)
  })
  width_cutoff <- cutoff2[which.max(width_predictions)] # 1.5
  
  #combine two dataset in the end
  y_hat <- ifelse(test$Petal.Length>length_cutoff | test$Petal.Width>width_cutoff,'virginica','versicolor')
  mean(y_hat==test$Species)


  