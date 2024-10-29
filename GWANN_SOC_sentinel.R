options(java.parameters="-Xmx8g")
library(viridisLite)
library(viridis)
library(rJava)
library(gwann)
library(ggplot2)
library(xlsx)
setwd("C:/scientific_research/光谱/SOC/data/workplace/R_code")
mydata<-read.xlsx("sentinel_1_2_RF.xlsx",1)
n = dim(mydata)[1]
train_num = floor(n*0.8)


x<-as.matrix(mydata[,c(1:9)])

y<-as.numeric(mydata[,c('Y')] )

dm<-as.matrix(dist(mydata[,c("lon","lat")])  )

s_test<-as.numeric(mydata[(train_num+1):n,c('idx_10_.1')])



nrHi=seq(5,by=5,length.out=20)
batSize=seq(10,by=10,length.out=19)
lr1 = seq(0.0005,by=0.0005,length.out=10)
nr=55
ba=10
lr2=0.0045
for (nr in nrHi) {
  for(ba in batSize) {
    for(lr2 in lr1) {

      r<-gwann(x_train=x[-s_test,],y_train=y[-s_test],w_train=dm[-s_test,-s_test],
               x_pred=x[s_test,],w_pred=dm[-s_test,s_test],
               nrHidden=nr,batchSize=ba,lr=lr2,
               adaptive=F,
               bwSearch="goldenSection",bwMin=min(dm)/4, bwMax=max(dm)/4,
               threads=8
      )
      Y_pre<-ddiag(r$predictions)
      Y_test = y[s_test]
      RMSE=(sum((Y_test-Y_pre)^2)/length(Y_pre))^0.5
      SSR=sum((Y_pre-mean(Y_test))^2);
      SSE=sum((Y_test-Y_pre)^2);
      SST=sum((Y_test-mean(Y_test))^2);
      R2=1-SSE/SST;
      MAE = sum( abs(Y_test-Y_pre) )/length(Y_pre)
      MRE = sum( (  abs(Y_test-Y_pre)/Y_test  ) )/length(Y_pre)
      
      
      parameter<-data.frame(nrHidden=nr,batchSize=ba,lr=lr2,RMSE=RMSE,R2=R2);
      write.table(parameter,file = 'SOC_acc_all.txt',sep = ',',append = TRUE,eol = "\n",
                  row.names = FALSE,col.names = FALSE)
      
    }
  }
}

##############  重复  55  10  0.0045
setwd("C:/scientific_research/SOC/data/workplace/R_code")
mydata<-read.xlsx("sentinel_1_2_RF.xlsx",1)
x<-as.matrix(mydata[,c(1:9)])
y<-as.numeric(mydata[,c('Y')] )
dm<-as.matrix(dist(mydata[,c("lon","lat")])  )
n = dim(mydata)[1]
train_num = floor(n*0.8)
result_all <-c()
idx_all <-c()
nr=55
ba=10
lr2=0.0045
for (i in 1:100) {
  idx<-sample(1:n,n,replace = F)
  idx_all <- cbind(idx_all,idx)
  s_test<-idx[(train_num+1):n]
  
  
  r<-gwann(x_train=x[-s_test,],y_train=y[-s_test],w_train=dm[-s_test,-s_test],
           x_pred=x[s_test,],w_pred=dm[-s_test,s_test],
           nrHidden=nr,batchSize=ba,lr=lr2,
           adaptive=F,
           bwSearch="goldenSection",bwMin=min(dm)/4, bwMax=max(dm)/4,
           threads=8
  )
  Y_pre<-diag(r$predictions)
  Y_test = y[s_test]
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
R2_all=result_all[1,]
a=sort(R2_all)
b = order(R2_all)
idx_max = b[91:100]
R2_all[idx_max]
idx_10 = idx_all[,idx_max]
result = result_all[,idx_max]
result_final = rowSums( result)/10
result_final 


###############
##############  出图
setwd("C:/scientific_research/SOC/data/workplace/R_code")
mydata<-read.xlsx("sentinel_1_2_RF.xlsx",1)
x<-as.matrix(mydata[,c(1:9)])
y<-as.numeric(mydata[,c('Y')] )
dm<-as.matrix(dist(mydata[,c("lon","lat")])  )
n = dim(mydata)[1]
train_num = floor(n*0.8)
result_all <-c()
idx_all <-c()
nr=55
ba=10
lr2=0.0045
y_contrast<-c()
for (i in 1:10) {
  i_char = as.character(i)
  idx_name=paste("idx_10_.",i_char,sep = "")
  if (i==10){
    idx_name=paste("idx_10_",i_char,sep = "")
  }
  
  s_test<-mydata[(train_num+1):n,c(idx_name)]
  
  
  r<-gwann(x_train=x[-s_test,],y_train=y[-s_test],w_train=dm[-s_test,-s_test],
           x_pred=x[s_test,],w_pred=dm[-s_test,s_test],
           nrHidden=nr,batchSize=ba,lr=lr2,
           adaptive=F,
           bwSearch="goldenSection",bwMin=min(dm)/4, bwMax=max(dm)/4,
           threads=8
  )
  Y_pre<-diag(r$predictions)
  Y_test = y[s_test]
  RMSE=(sum((Y_test-Y_pre)^2)/length(Y_pre))^0.5
  SSR=sum((Y_pre-mean(Y_test))^2);
  SSE=sum((Y_test-Y_pre)^2);
  SST=sum((Y_test-mean(Y_test))^2);
  R2=1-SSE/SST;
  MAE = sum( abs(Y_test-Y_pre) )/length(Y_pre)
  MRE = sum( (  abs(Y_test-Y_pre)/Y_test  ) )/length(Y_pre)
  result <-c(R2,RMSE,MAE)
  result_all <- cbind(result_all,result)
  y_contrast<- cbind(y_contrast,Y_test,Y_pre)
}

result_final = rowSums( result_all)/10
result_final 
write.table(y_contrast,file = 'y_contrast.txt',sep = ',',append = FALSE,eol = "\n",
            row.names = FALSE,col.names = TRUE)

###############













print(paste("RMSE: ",sqrt(mean((Y_pre-y[s_test])^2))))
print(paste("Iterations: ",r$iterations))
print(paste("Bandwidth: ",r$bandwidth))
# plot predictions
s<-cbind( Prediction=p, toy4[s_test,c("lon","lat")] )
ggplot(s,aes(lon,lat,fill=Prediction)) + geom_raster() + scale_fill_viridis() + coord_fixed()



