# Principle Component Analysis can actually be used as one method of reduction
# But in this case, we can see the plot are concentrated
# So we prefer not to use it as it shows little effective
set.seed(1234)
par(mfow=c(1,2))
library(ade4) 
# dependency of adegenet, whether variables should be scaled to uit varianes
library(adegenet)
pca=dudi.pca(X,scale=FALSE)
summary(pca)

s.lable(pca$li,sub="PCA-PC 1 and 2")
D<-dist(pca$li[,1:5])^2
clust<-hclust(D,method="complete")
temp<-as.data.frame(as.matrix(D))
temp<-t(as.matrix(D))
temp<-temp[,ncol(temp):1]
par(mar=c(1,5,5,1))

# heatmap
image(x=1:120,y=1:120,temp,col=rev(heat.colors(nlevels(as.factor(D)))),xaxt="n",yaxt="n",xlab="",ylab="")
asix(side=2,at=1:150,lab=colnames(X)),lax=2,cex.axis=0.5)
asix(side=3,at=1:150,lab=colnames(X)),las=2,cex.axis=0.5)
title("Genetic distances between isolates",outer=TRUE,line=-1)
plot(clust,main="Clustering (complete linkage) based on the first 5 PCs",cex=0.4)

# seperation analysis
pop<-factor(cutree(clust,k=5))
s.class(pca$li,fac=pop,col=transp(funky(5),cpoint=1,sub="PCA-axes 1 and 2")
s.class(pca$li,fac=y.factor,col=transp(c("blue","red")),cpoint=1,sub="PCA-axes 1 and 2")
