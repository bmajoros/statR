#!/bin/env Rscript

# Regressing on random and nonrandom data

NUM_TRIALS <- 1000
ALPHA <- 0.05
regressPvalue <- function(X,Y) {
    matrix <- cbind(X,Y)
    colnames(matrix) <- c("X","Y")
    model <- lm(Y~X,data=as.data.frame(matrix))
    s <- summary(model)
    df1 <- s$df[[1]]
    df2 <- s$df[[2]]
    f <- s$fstatistic[[1]]
    p <- pf(f,df1-1,df2,lower.tail=FALSE)
    return(p) }
randomModel <- function(N) {
    X <- rnorm(N,0,100)
    Y <- rnorm(N,0,100)
    p <- regressPvalue(X,Y)
    return(p) }
nonrandomModel <- function(N) {
    beta <- rnorm(1,0,5)
    intercept <- rnorm(1,0,5)
    X <- rnorm(N,0,100)
    Y <- beta*X+intercept+rnorm(N,0,100)
    p <- regressPvalue(X,Y)
    return(p) }
countSignificant <- function(pvalues,alpha) {
    num <- 0
    for(i in 1:length(pvalues)) {
        if(pvalues[i]<=ALPHA) { num <- num+1 } }
    return(num) }
positives <- array() ; negatives <- array()
for(N in seq(10,400,10)) {
    for(i in 1:NUM_TRIALS) {
        negatives[[i]] <- randomModel(N)
        positives[[i]] <- nonrandomModel(N)
    }
    TPrate <- countSignificant(positives,ALPHA)/NUM_TRIALS
    FPrate <- countSignificant(negatives,ALPHA)/NUM_TRIALS
    positives <- p.adjust(positives,"BH")
    negatives <- p.adjust(negatives,"BH")
    adjTPrate <- countSignificant(positives,ALPHA)/NUM_TRIALS
    adjFPrate <- countSignificant(negatives,ALPHA)/NUM_TRIALS
    cat(paste(N,FPrate,TPrate,adjFPrate,adjTPrate,"\n"))
}


