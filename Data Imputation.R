# Single Imputation: replace each missing value with the genotype that the most frequent in the sampe
set.seed(1234)
round(apply(is.na(breast_cancer),2,summ)/dim(breast_cancer)[1],3)
data<-na.rougnfix(breast_cancer)
