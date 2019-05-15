
library(DMwR)
population_dataset<-knnImputation(population_dataset, k=179, meth = "median")
sum(is.na(population_dataset))

population_subset = subset(population_dataset, 
                           -select = -c(qualification, country,occupation,
                                        working_sector))

dummy_obj = dummyVars(~., population_dataset, fullRank = T) 
#Dumify all categorical rows at once in std_data dataframe
dummy_data = as.data.frame(predict(dummy_obj, population_dataset))
names(dummy_data)






library(caret)
dummy_std_obj = preProcess( x = dummy_data[, !colnames(dummy_data) %in% ("target")],  
                      method = c("center", "scale")) #leaving the column target on which have to predict






table(population_dataset$financial_weight)


#Train/Test Split
#Split the data 70/30 into train and test sets by using Stratified Sampling and setting the seed as "786"
#Split the data using stratified sampling, we can do that by using the createDataPartition() function from the caret package

install.packages("caret")
library(caret)
set.seed(11)
train_rows <- createDataPartition(population_dataset$target, p = 0.7, list = F)
train_data <- population_dataset[train_rows,] #training data
test_data <- population_dataset[-train_rows,] #test data
dim(population_dataset)
dim(train_data)
dim(test_data)
table(train_data$target)
table(test_data$target)

#Basic Logistic Regression Model
#Build a model using all the variables, excluding the response variable, in the dataset

log_reg <- glm(target~., data = train_data, family = binomial) #AIC: 14057
summary(log_reg)

#qualification, years_of_education,ethnicity



### Exploratory Data Analysis - Scatterplot

#Look for relationships.
pairs(population_dataset)
cor(population_dataset[sapply(population_dataset, is.numeric)])


