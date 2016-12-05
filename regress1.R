#!/bin/env Rscript

# Generate a random model with given coefficient and intercept
beta <- rnorm(1,0,5)
intercept <- rnorm(1,0,5)
sd <- 10 # noise variance
cat(paste("Y=",beta,"X+",intercept))

# Simulate data from the model
Xs <- runif(1000,0,100)
Ys <- beta*Xs+intercept+rnorm(1,0,sd)
matrix <- cbind(Xs,Ys)
colnames(matrix) <- c("X","Y")

# See how well we can re-learn the model from the data
model <- lm(Y~X,data=as.data.frame(matrix))
summary(model)

# Plot points
pdf("plot.pdf")
plot(matrix)
dev.off()
