#!/bin/env Rscript

# As beta decreases, the ability to detect a significant relationship
# relationship decreases?

NUM_TRIALS <- 10000
ALPHA <- 0.05
sd <- 100 # noise variance
N=400

cat("beta\tproportion\n")
for(beta in c(1,0.9,0.8,0.7,0.6,0.55,0.5,0.45,0.4,0.35,0.3,0.25,0.2,0.15,0.1,0.05,0.0001)) {
    numSignificant <- 0
    for(i in 1:NUM_TRIALS) {
        # Generate a random model with given coefficient and intercept
        intercept <- rnorm(1,0,5)

        # Simulate data from the model
        Xs <- runif(N,0,100)
        Ys <- beta*Xs+intercept+rnorm(N,0,sd)
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
    cat(paste(beta,sig,"\n"))
}

