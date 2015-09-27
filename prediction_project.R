library(ggplot2)
library(kernlab)
library(caret)
library(randomForest)
library(devtools)
library(plyr)

#function writes the content of vector x into text files
pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

#reading the training set
training<-read.csv("pml-training.csv", sep=",")
training<-training[,-(1:7)]

y<-function(x){sum(is.na(x))==0}

z<-sapply(training, y)
data1<-training[,z]

y<-function(x){is.numeric(x)}
data1<-data1[,sapply(data1[,-ncol(data1)],y)]


data1_pca<-prcomp(data1[,-ncol(data1)], center=TRUE, scale=TRUE)
summary (data1_pca)

#selection of appropriate variables
x<-substr(names(training), 1,5)=="accel"
y<-substr(names(training), 1,5)=="total"
z<-substr(names(training), 1,4)=="roll"
w<-substr(names(training), 1,5)=="pitch"

#subsetting the data
data1<-(cbind(training[,x], training[,y], training[,z], training[,w],training[,160]))
names(data1)[ncol(data1)]='Classe'

M<-abs(cor(data1[,-ncol(data1)]))
diag(M)<-0
which(M>0.8, arr.ind=TRUE)

data1_pca<-prcomp(data1[,-ncol(data1)], center=TRUE, scale=TRUE)
summary (data1_pca)
#install_github("ggbiplot", "vqv")

featurePlot(x=data1[,-ncol(data1)], y=factor(data1$Classe), plot="pairs")
#g <- ggplot(data1, aes(x=total_accel_belt, y=total_accel_dumbbell))
#g <- g+geom_point(aes(color=factor(Classe)), size=3)
#g
#creating training and testing subsets for cross-validation
set.seed(1342)
in_train<-createDataPartition(y=data1$Classe, p=0.75, list=FALSE)
tr<-data1[in_train,]
testing<-data1[-in_train,]

#building a predictive model
modFit<-train(factor(tr$Classe)~., method="rf", preProcess="pca", data=tr)

#results of cross-validation
confusionMatrix(testing$Classe, predict(modFit, testing))

#application of the predictive model to a new set of data
validation<-read.csv("pml-testing.csv", sep=",")
results<-predict(modFit, validation)
pml_write_files(results)
#printing the results on screen
print(results)
