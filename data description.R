dim(data) 
# checking the dimension of data
# the breast cancer case give 120 x 151

y<-data$Status
y.factor<-as.factor(data$Status)
X<-as.matrix(data[,2:151])
# X as predictor matrix and y as response vector
# notice the cancer test give bivariate response

# we can check for the classifcaiton table
Y.0=as.numeric(data$Status=="0")
Y=cbind(Y.0,1-Y.0)
table(1-Y.0,data$Status)
# give following result
#      0    1
# 0   100   0
# 1    0    20
