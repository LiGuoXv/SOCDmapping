library(ggplot2)
library(lattice)
library(ranger)
library(caret)
library(lattice)
library(SpatialML)
library(xlsx)

setwd("C:/scientific_research/SOC/data/workplace/R_code")
mydata<-read.xlsx("sentinel_1_2_RF.xlsx",1)

n = dim(mydata)[1]
train_num = floor(n*0.8)
idx_all <-c()
result_final_all<-c()
for(j in 1:11){
  result_all <-c()
  for (i in 1:100) {
    idx<-sample(1:n,n,replace = F)
    i_char = as.character(i)
    idx_name=paste("idx_10_.",i_char,sep = "")
    if (i==10){
      idx_name=paste("idx_10_",i_char,sep = "")
    }
    
    #idx<-sample(1:n,n,replace = F)
    idx_all <- cbind(idx_all,idx)
    
    s_train=idx[1:train_num]
    #s_train<-as.numeric(mydata[1:train_num,c(idx_name)])
    mydata_train=mydata[c(s_train),]
    
    s_test=idx[(train_num+1):n]
    #s_test<-as.numeric(mydata[(train_num+1):n,c(idx_name)])
    mydata_test=mydata[c(s_test),]
    Coords<-mydata_train[,c("lon","lat")]
    grf <- grf(Y ~ band_1+band_2+band_3+band_4+band_5+band_6+band_7+band_8+band_9, 
               dframe=mydata_train, bw=2.5,
               kernel="fixed", coords=Coords,ntree=700,mtry=5,weighted = TRUE)
    Y_pre=predict.grf(grf, mydata_test, x.var.name="lon", y.var.name="lat", local.w=(1-((j-1)*0.1) ), global.w=((j-1)*0.1))
    Y_test<-as.numeric(mydata_test[,c('Y')])
    RMSE=(sum((Y_test-Y_pre)^2)/length(Y_pre))^0.5
    SSR=sum((Y_pre-mean(Y_test))^2);
    SSE=sum((Y_test-Y_pre)^2);
    SST=sum((Y_test-mean(Y_test))^2);
    R2=1-SSE/SST;
    MAE = sum( abs(Y_test-Y_pre) )/length(Y_pre)
    MRE = sum( (  abs(Y_test-Y_pre)/Y_test  ) )/length(Y_pre)
    result <-c(R2,RMSE,MAE)
    result_all <- cbind(result_all,result)
  }
  #idx_max = b[91:100]
  #result = result_all[,idx_max]
  result_final = rowSums(result_all)/100
  result_final_all <- cbind(result_final_all,result_final)
}


R2_all=result_all[1,]
a=sort(R2_all)
b = order(R2_all)
idx_max = b[491:500]
R2_all[idx_max]
idx_10 = idx_all[,idx_max]
result = result_all[,idx_max]


for (i in 1:50) {
  idx<-sample(1:n,n,replace = F)
  i_char = as.character(i)
  idx_name=paste("idx_10_.",i_char,sep = "")
  if (i==10){
    idx_name=paste("idx_10_",i_char,sep = "")
  }
  
  #idx<-sample(1:n,n,replace = F)
  idx_all <- cbind(idx_all,idx)

  s_train=idx[1:train_num]
  #s_train<-as.numeric(mydata[1:train_num,c(idx_name)])
  mydata_train=mydata[c(s_train),]

  s_test=idx[(train_num+1):n]
  #s_test<-as.numeric(mydata[(train_num+1):n,c(idx_name)])
  mydata_test=mydata[c(s_test),]
  Coords<-mydata_train[,c("lon","lat")]
  grf <- grf(Y ~ band_1+band_2+band_3+band_4+band_5+band_6+band_7+band_8+band_9, 
             dframe=mydata_train, bw=2.5,
             kernel="fixed", coords=Coords,ntree=700,mtry=5,weighted = TRUE)
  Y_pre=predict.grf(grf, mydata_test, x.var.name="lon", y.var.name="lat", local.w=1, global.w=0)
  Y_test<-as.numeric(mydata_test[,c('Y')])
  RMSE=(sum((Y_test-Y_pre)^2)/length(Y_pre))^0.5
  SSR=sum((Y_pre-mean(Y_test))^2);
  SSE=sum((Y_test-Y_pre)^2);
  SST=sum((Y_test-mean(Y_test))^2);
  R2=1-SSE/SST;
  MAE = sum( abs(Y_test-Y_pre) )/length(Y_pre)
  MRE = sum( (  abs(Y_test-Y_pre)/Y_test  ) )/length(Y_pre)
  result <-c(R2,RMSE,MAE)
  result_all <- cbind(result_all,result)
  print('111111111111111111111111111111111111111')
  print(i)
}
  result_final = rowSums( result_all)/10
  result_final
  
  
  
  R2_all=result_all[1,]
  a=sort(R2_all)
  b = order(R2_all)
  idx_max = b[491:500]
  R2_all[idx_max]
  idx_10 = idx_all[,idx_max]
  result = result_all[,idx_max]
  
  grf <- grf(Y ~ band_1+band_2+band_3+band_4+band_5+band_6+band_7+band_8+band_9, 
             dframe=mydata_train, bw=0.27,
             kernel="fixed", coords=Coords,ntree=700,mtry=5,weighted = TRUE)
  
bwe <-grf.bw(Y ~ band_1+band_2+band_3+band_4+band_5+band_6+band_7+band_8+band_9,
             mydata_train, kernel="fixed",
             coords=Coords, bw.min = 0.25, bw.max = 0.5, step = 0.01,
             forests = FALSE, weighted = TRUE,trees=700, mtry=5)

grf <- grf(Y ~ band_1+band_2+band_3+band_4+band_5+band_6+band_7+band_8+band_9,,dframe=mydata_train, bw=bwe$Best.BW,
           kernel="fixed", coords=Coords)

