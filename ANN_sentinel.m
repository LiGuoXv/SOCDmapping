clc
clear
load('idx_1-2_10.mat')
data = importdata('sentinel_8_9.txt');
Y=data.data(:,6);
band = data.data(:,7:end);
band=band*0.0001;
Meanreflectance = sum(band,2)./9;
B_Normalize = band./Meanreflectance;
n=length(Y);
n_train = floor(n*0.8);
X=B_Normalize;
%[max_acc,idx_max]=sort(result(:,1),'descend') 
%idx_10 = idx_all(:,idx_max(1:10));
%%
%band = data.data(:,15:21);
%band=band*2.75e-05-0.2;
%%{'SAVI','RVI','NDVI','EVI','DVI'}
NIR =band(:,7);
RED =band(:,3);
BLUE =band(:,2);
EVI = 2.5.* ((NIR - RED) ./ (NIR + 6 .* RED - 7.5.* BLUE + 1));
NDVI = (NIR - RED)./(NIR + RED);
DVI = NIR - RED;
RVI = NIR ./ RED;
SAVI =(NIR - RED) .* (1 + 0.5)./(NIR + RED + 0.5);
NDWI = (band(:,3) - NIR)./ (band(:,3) + NIR);
X=[band,SAVI,RVI,NDVI,EVI,DVI];
%%
result=[];
idx_all = [];
for i=1:100
      %idx = idx_10(:,i); 
      idx=randperm(n);
      idx=idx';
      idx_all = [idx_all,idx];
      
      %idx = idx_10(:,i); 
      X_train = X(idx(1:n_train ),:);
      Y_train = Y(idx(1:n_train ));
      X_test = X(idx(n_train +1:end),:);
      Y_test = Y(idx(n_train +1:end));
      
      X_test =X_test';
      X_train = X_train';
      Y_train =Y_train';
      N = size(X_test,2);

      % 数据归一化
      [p_train, ps_input] = mapminmax(X_train,0,1);
      p_test = mapminmax('apply',X_test,ps_input);
      [t_train, ps_output] = mapminmax(Y_train,0,1);
      net = newff(p_train,t_train,5);
      net.trainParam.epochs = 100000;%,batchSize
      net.trainParam.goal = 1e-3;
      net.trainParam.lr = 1e-3;
      net = train(net,p_train,t_train);
       % 4. 仿真测试
      t_test_pre = sim(net,p_test);
      % 5. 数据反归一化
      P_test_pre = mapminmax('reverse',t_test_pre,ps_output);
      Y_pre =  P_test_pre';   
     
      RMSE=(sum((Y_test-Y_pre).^2)/size(Y_pre,1))^0.5;
      [R,P] = corrcoef(Y_test,Y_pre);
      SSR=sum((Y_pre-mean(Y_test)).^2);
      SSE=sum((Y_test-Y_pre).^2);
      SST=sum((Y_test-mean(Y_test)).^2);
      R2=1-SSE/SST;
      MAE = sum( abs(Y_test-Y_pre) )/length(Y_pre);
      MRE = sum( (  abs(Y_test-Y_pre)./Y_test  ) )/length(Y_pre);
      result_acc= [R2,RMSE,MAE];
      result = [result;result_acc];
end
result_acc= sum(result,1)./100