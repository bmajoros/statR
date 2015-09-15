#!/usr/bin/Rscript --vanilla

args <- commandArgs(TRUE)
if(length(args)!=2) {
   cat("usage: <train.txt> <test.txt>\n");
   q(status=1)
}
trainFile <- args[1];
testFile <- args[2];
trainData <- read.table(trainFile);
testData <- read.table(testFile);

# Fit model to training data
model <- glm(trainData,family=binomial(logit));

# Do prediction on test data
predictions <- predict(model,testData,type="response");
cbind(testData[,1],predictions);


