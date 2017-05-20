company<-read.csv("/Users/yananchen/Desktop/companyReview.csv",header=T)
print(company)
company2$user_id<-NULL
(kmeans.result<-kmeans(company2,3))
table(company$data,kmeans.result$cluster)
 plot(company[c("data","stars")],col=kmeans.result$cluster)
points(kmeans.result$centers[c("data","stars")],col=1:3,pch=8,cex=2)