 business<-read.csv("/Users/yananchen/Downloads/CSV_pan/business.csv",header=T)
 business2<-business
 business2$city<-NULL
 business2$full_address<-NULL
 business2$state<-NULL
 business2$name<-NULL
 business2$review_count<-NULL
 business2$type<-NULL
 business2$open<-NULL
 business2$business_id<-NULL
 library(ggvis)
 business3<-business2[,c(1,3,2)]
 print(business3)
 (kmeans.result<-kmeans(business3,2))
 plot(business3$latitude,business3$stars)
 plot(business3$longitude,business3$stars)
 library(ggvis)
 business3 %>% ggvis(~longitude,~latitude,fill=~stars) %>% layer_points()
 table(business3$stars)
 round(prop.table(table(business3$stars))*100,digits=1)
 summary(business3)
 library(class)
 normalize <- function(x) {
num <- x - min(x)
denom <- max(x) - min(x)
return (num/denom)
}
business3[complete.cases(business3),]
 business_norm<-as.data.frame(lapply(business3[1:2],normalize))
 set.seed(12)
ind<-sample(2,nrow(business_norm),replace=TRUE,prob=c(0.7,0.3))
business.training<-business_norm[ind==1,1:2]
business.test<-business_norm[ind==2,1:2]
 business.trainLabels<-business3[ind==1,3]
business.testLabels<-business3[ind==2,3]
business_pred<-knn(train=business.training,test=business.test,cl=business.trainLabels,k=3)
library(gmodels)
CrossTable(x=business.testLabels,y=business_pred,prop.chisq=FALSE)
tab<-table(x=business.testLabels,y=business_pred)
1-sum(diag(tab))/sum(tab)


