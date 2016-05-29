#[] The report describes a machine learning algorithm to predict activity quality from activity monitors.
#[] The report is 2, 000 words or less.
#[] The number of figures in the document 5 or less.
#[] The report explains how the model was built.
#[] The report explains how cross - validation was used to estimate the out of sample error rate.
#[] The report lists the key decisions made during the analysis, and explains why these decisions were made.
#[] The report reviews the accuracy of the selected machine learning algorithm in predicting the 20 unknown test cases.
#[] The submission includes a github repository, and a link to the repository as part of the Coursera submission.
#[] The submission includes a compiled HTML document, the output .md markdown file, and any graphics required to correctly view them within the markdown file on github.
#[] If a github pages branch is provided, is the HTML file accessible at https: / / username.github.io / reponame.

install.packages("YaleToolkit")
library(YaleToolkit)
library(caret)

# Six young health participants were asked to perform one set of 10 repetitions of the Unilateral 
# Dumbbell Biceps Curl in five different fashions:
# exactly according to the specification(Class A)
# throwing the elbows to the front(Class B)
# lifting the dumbbell only halfway(Class C)
# lowering the dumbbell only halfway(Class D) 
# and throwing the hips to the front(Class E) 

# What you should submit

# The goal of your project is to predict the manner in which 
# they did the exercise. This is the "classe" variable in the 
# training set. You may use any of the other variables to predict with. 
# You should create a report describing how you built your model, 
# how you used cross validation, what you think the expected out 
# of sample error is, and why you made the choices you did. 
# You will also use your prediction model to predict 20 different test cases.

#Load data
trainRaw <- read.csv("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv")
testRaw <- read.csv("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv")

#Examine data
whatis(trainRaw)


#Remove columns that have no values in the test set
trainNaomit <- trainRaw[, colSums(is.na(testRaw)) < nrow(testRaw)]
testNaomit <- testRaw[, colSums(is.na(testRaw)) < nrow(testRaw)]

#Remove columns that have no prediction power or are not of particular interest
trainClean <- trainNaomit[, - c(1, 3, 4, 5, 6, 7)]
testClean <- testNaomit[, - c(1, 3, 4, 5, 6, 7)]


#Split data for train and validate
inTrain = createDataPartition(trainClean$classe, p = 3 / 4)[[1]]
training = trainClean[inTrain,]
validate = trainClean[ - inTrain,]

#train model
set.seed(112)
controlRf <- trainControl(method = "cv", 5)
mod1 <- train(classe ~ ., data = training, method = "rf", trControl = controlRf, ntree = 100)


#Predict on validate
pred1 <- predict(mod1, validate)
confusionMatrix(validate$classe, pred1)


#Predict on test
prediction  <- predict(mod1, testClean[, - 54])
result <- cbind(testClean[, 54], prediction)
result

#Match in plot
qplot(pred1, pred2, colour = y, data = vowel.test)





prediction
[1,] 1 2
[2,] 2 1
[3,] 3 2
[4,] 4 1
[5,] 5 1
[6,] 6 5
[7,] 7 4
[8,] 8 2
[9,] 9 1
[10,] 10 1
[11,] 11 2
[12,] 12 3
[13,] 13 2
[14,] 14 1
[15,] 15 5
[16,] 16 5
[17,] 17 1
[18,] 18 2
[19,] 19 2
[20,] 20 2