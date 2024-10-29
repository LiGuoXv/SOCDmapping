options(java.parameters="-Xmx8g")
library(viridisLite)
library(viridis)
library(rJava)
library(gwann)
library(ggplot2)
library(xlsx)
library(data.table)
library(foreach)
library(doParallel)
library(iterators)
library(parallel)

setwd("C:/scientific_research/SOC/data/workplace/R_code")
data <- fread("sentinel_2023_1_2_area_bareland.txt",sep = ",",header =T )


N_this= dim(data)[1];
interval=3000000
num_list = seq(from=1,to=N_this,by=interval)
num = num_list[7]
if(  (num+interval)>N_this  ){
    data = data[num:N_this,]
}
if(  (num+interval)<N_this  ){
    data = data[num:(num-1+interval),]
}

mydata<-read.xlsx("sentinel_1_2_RF.xlsx",1)
x<-as.matrix(mydata[,c(1:9)])
y<-as.numeric(mydata[,c('Y')] )
n = dim(mydata)[1]
train_num = floor(n*0.8)


N= dim(data)[1];
interval=1000
num_list = seq(from=1,to=N,by=interval)


nr=55
ba=10
lr2=0.0045

for(num in num_list) {
  print(num)
  if(  (num+interval)>N  ){
    pre_data = data[num:N,]
  }
  else{
    pre_data = data[num:(num-1+interval),]
  }
  band<-as.matrix(pre_data[,1:9])
  band=band*0.0001;
  Meanreflectance = rowSums(band)/9;
  x_pred = band/Meanreflectance;
  #for (i in 1:10) {
    i=10
    print(i)
    i_char = as.character(i)
    idx_name=paste("idx_10_.",i_char,sep = "")
    if (i==10){
      idx_name=paste("idx_10_",i_char,sep = "")
    }
    s_test<-mydata[(train_num+1):n,c(idx_name)]
    pos2 = pre_data[,c("X","Y")]
    pos1 = mydata[-s_test,c("lon","lat")]
    colnames(pos1) <- c("X", "Y")
    pos<-rbind(pos1,pos2)
    dm<-as.matrix(dist( pos )  )
    n_all =  dim(pos)[1]
    r<-gwann(x_train=x[-s_test,],y_train=y[-s_test],w_train=dm[1:train_num,1:train_num],
             x_pred=x_pred,w_pred=dm[1:train_num,(train_num+1): n_all],
             nrHidden=nr,batchSize=ba,lr=lr2,
             adaptive=F,
             bwSearch="goldenSection",bwMin=min(dm[1:train_num,1:train_num])/4, 
             bwMax=max(dm[1:train_num,1:train_num])/4,
             threads=8
    )
    SOCD<-diag(r$predictions)
    result<-data.frame(SOCD,pos2);
    file_name=paste("SOC_area_7_",i_char,".txt",sep = "")
    write.table(result,file =file_name,sep = ',',append = TRUE,eol = "\n",
                row.names = FALSE,col.names = FALSE)
  #}
}

#test<-matrix(sample(1:200,50),nrow=10)
#write.table(test, file = "test.txt",col.names = F,row.names = F,quote = F)



####  并行
setwd("C:/scientific_research/SOC/data/workplace/R_code")

cl <- makeCluster(2)
registerDoParallel(cl)
#
data <- fread("sentinel_2023_1_2_area.txt",sep = ",",header =T )
data<- data[,-c(1:3)]
N= dim(data)[1];
interval=1000
num_list = seq(from=1,to=N,by=interval)
nr=55
ba=10
lr2=0.0045
N_num_list= length(num_list);
#
mydata<-read.xlsx("sentinel_1_2_RF.xlsx",1)
x<-as.matrix(mydata[,c(1:9)])
y<-as.numeric(mydata[,c('Y')] )
n = dim(mydata)[1]
train_num = floor(n*0.8)




#for(num in num_list)
foreach(j = 1:N_num_list, .combine = c) %dopar%{
  num =num_list[j]
  print(num)
  if(  (num+interval)>N  ){
    pre_data = data[num:N,]
  }
  else{
    pre_data = data[num:(num-1+interval),]
  }
  band<-as.matrix(pre_data[,1:9])
  band=band*0.0001;
  Meanreflectance = rowSums(band)/9;
  x_pred = band/Meanreflectance;
  for (i in 1:1) {
    print(i)
    i_char = as.character(i)
    idx_name=paste("idx_10_.",i_char,sep = "")
    if (i==10){
      idx_name=paste("idx_10_",i_char,sep = "")
    }
    s_test<-mydata[(train_num+1):n,c(idx_name)]
    pos2 = pre_data[,c("X","Y")]
    pos1 = mydata[-s_test,c("lon","lat")]
    colnames(pos1) <- c("X", "Y")
    pos<-rbind(pos1,pos2)
    dm<-as.matrix(dist( pos )  )
    n_all =  dim(pos)[1]
    r<-gwann(x_train=x[-s_test,],y_train=y[-s_test],w_train=dm[1:train_num,1:train_num],
             x_pred=x_pred,w_pred=dm[1:train_num,(train_num+1): n_all],
             nrHidden=nr,batchSize=ba,lr=lr2,
             adaptive=F,
             bwSearch="goldenSection",bwMin=min(dm[1:train_num,1:train_num])/4, 
             bwMax=max(dm[1:train_num,1:train_num])/4,
             threads=8
    )
    SOCD<-diag(r$predictions)
    result<-data.frame(SOCD,pos2);
    file_name=paste("SOC_area_text",i_char,".txt",sep = "")
    write.table(result,file =file_name,sep = ',',append = TRUE,eol = "\n",
                row.names = FALSE,col.names = FALSE)
  }
}
stopImplicitCluster()
stopCluster(cl)
#test<-matrix(sample(1:200,50),nrow=10)
#write.table(test, file = "test.txt",col.names = F,row.names = F,quote = F)
















