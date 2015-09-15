#!/usr/bin/Rscript --vanilla

args <- commandArgs(TRUE)
if(length(args)!=1) {
   cat("usage: PCA.R data.txt #components  >  outfile\n");
   q(status=1)
}
infile <- args[1];
numFactors <- as.numeric(args[2]);

# Load data
data <- read.table(infile);

# Analyze each class separately



fit <- factanal(data, numFactors, rotation="varimax")
print(fit, digits=2, cutoff=.3, sort=TRUE)
# plot factor 1 by factor 2 
load <- fit$loadings[,1:2] 
plot(load,type="n") # set up plot 
text(load,labels=names(mydata),cex=.7) # add variable names




