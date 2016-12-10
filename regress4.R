#!/bin/env Rscript

# Regressing on purely random data

NUM_TRIALS <- 10000
ALPHA <- 0.05
sd <- 100 # noise variance

for(N in seq(10,400,10)) {
    numSignificant <- 0
    for(i in 1:NUM_TRIALS) {
        # Generate purely random data
        Xs <- runif(N,0,100)
        Ys <- runif(N,0,100)
        matrix <- cbind(Xs,Ys)
        colnames(matrix) <- c("X","Y")

        # See how well we can re-learn the model from the data
        model <- lm(Y~X,data=as.data.frame(matrix))
        s <- summary(model)
        adjRSquared <- s$adj.r.squared
        rSquared <- s$r.squared
        df1 <- s$df[[1]]
        df2 <- s$df[[2]]
        f <- s$fstatistic[[1]]
        interceptEst <- model$coefficients[[1]]
        betaEst <- model$coefficients[[2]]
        p <- pf(f,df1-1,df2,lower.tail=FALSE)
        if(p<=ALPHA) {
            numSignificant <- numSignificant+1
        }
    }
    sig <- numSignificant/NUM_TRIALS
    cat(paste(N,sig,"\n"))
}

