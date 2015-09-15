#!/usr/bin/perl
use strict;

my ($numObs,$numLatent,$numClasses,$numIndiv,@latentMeans,@latentSDs,
    @classProportions,@factorLoadings,@constants,@errorSDs);
open(IN,"1.model") || die;
while(<IN>) {
  chomp;
  if(/observables:\s*(\d+)/) { $numObs=$1 }
  elsif(/latent:\s*(\d+)/) { $numLatent=$1 }
  elsif(/classes:\s*(\d+)/) { $numClasses=$1 }
  elsif(/individuals:\s*(\d+)/) { $numIndiv=$1 }
  elsif(/latent means:/) { @latentMeans=split/\s+/,<IN> }
  elsif(/latent SDs:/) { @latentSDs=split/\s+/,<IN> }
  elsif(/class proportions:/) { @classProportions=split/\s+/,<IN> }
  elsif(/constants:/) { @constants=split/\s+/,<IN> }
  elsif(/error SDs:/) { @errorSDs=split/\s+/,<IN> }
  elsif(/factor loadings:/) {
    for(my $i=0 ; $i<$numObs ; ++$i) {
      my @array=split/\s+/,<IN>;
      my $loadings=[];
      @$loadings=@array;
      push @factorLoadings,$loadings;
    }
  }
}
close(IN);

my @indiv;
open(IN,"1.latent") || die;
while(<IN>) {
  chomp;
  my @fields=split;
  my $class=shift @fields;
  my $latent=[];
  @$latent=@fields;
  my $indiv={class=>$class,latent=>$latent};
  push @indiv,$indiv;
}
close(IN);

my $i=0;
open(IN,"1.data") || die;
while(<IN>) {
  chomp;
  my @fields=split;
  shift @fields;
  my $obs=[];
  @$obs=@fields;
  $indiv[$i]->{obs}=$obs;
  ++$i;
}
close(IN);

for(my $i=0 ; $i<$numObs ; ++$i) {
  for(my $j=0 ; $j<$numIndiv ; ++$j) {
    my $indiv=$indiv[$j];
    my $obs=$indiv->{obs}->[$i]; my $latent=$indiv->{latent};
    my $pred=$constants[$i];
    for(my $k=0 ; $k<$numLatent ; ++$k) {
      $pred+=$factorLoadings[$i]->[$k]*$latent->[$k];
    }
    print "$pred\t$obs\n";
  }
    exit
  print "\n";
}

