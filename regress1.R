#!/bin/env Rscript

# Generate a random model with given coefficient and intercept
beta <- rnorm(1,0,5)
intercept <- rnorm(1,0,5)
sd <- 100 # noise variance
N <- 100 # sample size
cat(paste("Y=",beta,"X+",intercept))

# Simulate data from the model
Xs <- runif(N,0,100)
Ys <- beta*Xs+intercept+rnorm(N,0,sd)
matrix <- cbind(Xs,Ys)
colnames(matrix) <- c("X","Y")

# See how well we can re-learn the model from the data
model <- lm(Y~X,data=as.data.frame(matrix))
summary(model)
interceptEst <- model$coefficients[[1]]
betaEst <- model$coefficients[[2]]

# Plot points
pdf("plot.pdf")
plot(matrix,pch=19)
line1 <- matrix(c(0,100,intercept,beta*100+intercept),nrow=2,ncol=2)
lines(line1,col="blue",lwd=3)
line2 <- matrix(c(0,100,interceptEst,betaEst*100+interceptEst),nrow=2,ncol=2)
lines(line2,col="red",lty=2,lwd=3)
dev.off()
