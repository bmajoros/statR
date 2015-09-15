#!/usr/bin/Rscript --vanilla
library(gtools); # needed for dirichlet distribution

#errSD=10; # 1 gives medium dispersion, 5 looks totally random
           # 0.1 looks almost perfectly linear

args <- commandArgs(TRUE)
if(length(args)!=8) {
   cat("usage: <#observables> <#latents> <#classes> <#individuals> <err-sd> <out.model> <out.observable> <out.latent>\n");
   q(status=1)
}
numObservables <- as.numeric(args[1]);
numLatent <- as.numeric(args[2]);
numClasses <- as.numeric(args[3]);
numIndiv <- as.numeric(args[4]);
errSD <- as.numeric(args[5]);
modelFile <- args[6];
dataFile <- args[7];
latentFile <- args[8];

# Generate latent variable distributions
latentMeans <- matrix(nrow=numClasses,ncol=numLatent); # [class][variable]
for (i in 1:numClasses)
   latentMeans[i,] <- rnorm(numLatent,sd=10);
latentSDs <- rep(5,numLatent); #abs(rnorm(numLatent,sd=5));

# Generate class proportions
#classProportions <- rdirichlet(1,rep(1,numClasses));# potentially unbalanced
classProportions <- rep(1/numClasses,numClasses);    # balanced data

# Generate factor loadings and constant offsets and error variances
obsOffsets <- rnorm(numObservables,sd=10);
errSDs <- rep(errSD,numObservables);  # abs(rnorm(numObservables,sd=errSD));
allLoadings <- matrix(nrow=numObservables,ncol=numLatent,byrow=TRUE);
for (i in 1:numObservables) {
   #numLoadings <- floor(runif(1,min=0,max=numLatent+1));
   #if(numLoadings>numLatent) { numLoadings <- numLatent }
   loadings <- rdirichlet(1,rep(1,numLatent));
   allLoadings[i,] <- loadings;
}

# Save model to file
cat("observables: ",numObservables,"\nlatent: ",numLatent,"\n",
    "classes: ",numClasses,"\nindividuals: ",numIndiv,"\n",
    file=modelFile,append=F,sep="");
cat("latent means:\n",file=modelFile,append=T);
cat(latentMeans,"\n",file=modelFile,append=T);
cat("latent SDs:\n",file=modelFile,append=T);
cat(latentSDs,"\n",file=modelFile,append=T);
cat("class proportions:\n",file=modelFile,append=T);
cat(classProportions,"\n",file=modelFile,append=T);
cat("factor loadings:\n",file=modelFile,append=T);
for (i in 1:numObservables) {
   cat(allLoadings[i,],"\n",file=modelFile,append=T);
}
cat("constants:\n",file=modelFile,append=T);
cat(obsOffsets,"\n",file=modelFile,append=T);
cat("error SDs:\n",file=modelFile,append=T);
cat(errSDs,"\n",file=modelFile,append=T);

# Generate individuals
f <- file.create(latentFile);
f <- file.create(dataFile);
for (i in 1:numIndiv) {
   # Select a class for the individual
   classes <- rmultinom(1,1,classProportions);
   class <- 0;
   for(j in 1:numClasses) { if(classes[j]==1) {class=j}}

   # Generate latent values
   latent <- rnorm(numLatent,latentMeans[class,],latentSDs);
   cat(class-1,latent,"\n",sep="\t",file=latentFile,append=T);

   # Generate observable values
   error <- rnorm(numObservables,0,errSDs);
   obs <- obsOffsets + error + allLoadings %*% latent;
   cat(class-1,obs,"\n",sep="\t",file=dataFile,append=T);
}



