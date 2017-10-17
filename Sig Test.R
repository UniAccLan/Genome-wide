################ Function #################
fhat<-function(D){
	D<-na.roughfix(D)
	misclass.error<-matrix(0,3,2)
	X<-as.matrix(D[,2:151])
	y<-D$Status

	# tests code
	beta=seq(length=0)
	pval=seq(length=0)
	for (i in 2:151){
		fit=glm(y~data[,i],family=binomial(link="logit"))
		beta=c(pval,summary(fit)$coef[2,4])
		pval=c(pval,summary(fit)$coef[2,1])
	}
	Logit.order=order(abx(beta),decreasing=T)[1:12]
	Sigdata=data[c(1,Logit.order+1)]
	Logit.sig<-glm(Sigdata$Status~.,family=binomial(link="logit"),data=Sigdata)
	Logit.sig<-step(Logit.sig)
	Sig.cv=predict(Logit.sig,type="response")
	misclass.error[1,1]<-1-sum(diag(table(round(Sig.cv),data$Status))/sum(table(round(Sig.cv),data$Status)))
	lda=lda(Logit.sig$formula,Sigdata)
	lda.P=predict(lda)$class
	misclass.error[1,2]<-1-sum(diag(table(lda.P,Sigdata$Status))/sum(table(lda.P,Sigdata$Status)))

	#Lasso for significant
	Lasso=glmnet(X,y,family="binomial",alpha=1)
	Lasso.order=(1:151)[Lasso$beta[,19]!=0]
	Lassodata=D[c(1,Lasso.order+1)]
	Logit.lasso<-glm(Lassodata$Status~.,Lassodata,family=binomial(link="logit"))
	Logit.lasso<-step(Logit.lasso)
	Lasso.cv=predict(Logit.lasso,type="response")
	misclass.error[2,1]<-1-sum(diag(table(round(Lasso.cv),Lassodata$Status))/sum(table(round(Lasso.cv),Lassodata$Status)))
	lda=lda(Logit.lasso$formula,Lassodata)
	lda.P=predict(lda)$class
	misclass.error[2,2]<-1-sum(diag(table(lda.P,Lassodata$Status))/sum(table(lda.P,Lassodata$Status)))

	#Elastic Net with alpha=0.01
	Elastic=glmnet(X,y,family="binomial",alpha=0.01)
	Elastic.order=(1:151)[Elastic$beta[,22]!=0]
	Elasticdata=D[c(1,Elastic.order+1)]
	Logit.elastic<-glm(Elasticdata$Status~.,Elasticdata,family=binomial(link="logit"))
	Logit.elastic<-step(Logit.elastic)
	Elastic.cv=predict(Logit.elastic,type="response")
	misclass.error[3,1]<-1-sum(diag(table(round(Elastic.cv),Elasticdata$Status))/sum(table(round(Elastic.cv),Elasticdata$Status)))
	lda=lda(Logit.elastic$formula,Elasticdata)
	lda.P=predict(lda)$class
	misclass.error[3,2]<-1-sum(diag(table(lda.P,Elasticdata$Status))/sum(table(lda.P,Elasticdata$Status)))

	#misclasssification error
	colnames(misclass.error)<-c("Logit Error","Lda Error")
	rownames(misclass.error)<-c("SigTest","Lasso","Elastic")
	return(misclass.error)
}