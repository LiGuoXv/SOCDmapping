clc
clear
load('idx_1-2_10.mat')
data = importdata('sentinel_1_2.txt');
Y=data.data(:,6);
band = data.data(:,7:end);
band=band*0.0001;
Meanreflectance = sum(band,2)./9;
B_Normalize = band./Meanreflectance;
n=length(Y);
n_train = floor(n*0.8);
X=B_Normalize;
% X=band;
idx_all = [];

[h,p] = kstest(Y);
%%
%%{'SAVI','RVI','NDVI','EV
NIR =band(:,7);
RED =band(:,3);
BLUE =band(:,1);
EVI = 2.5.* ((NIR - RED) ./ (NIR + 6 .* RED - 7.5.* BLUE + 1));
NDVI = (NIR - RED)./(NIR + RED);
DVI = NIR - RED;
RVI = NIR ./ RED;
SAVI =(NIR - RED) .* (1 + 0.5)./(NIR + RED + 0.5);
NDWI = (band(:,2) - NIR)./ (band(:,2) + NIR);
X=[B_Normalize,SAVI,RVI,NDVI,EVI,DVI];

%%
data = importdata('SOC_acc_all.txt');
[a,b]=max(data(:,5));
[55,10,0.00450000000000000,1.72631687373153,0.540413497968274]
%T = table(band,SAVI,RVI,NDVI,EVI,DVI,LON,LAT,Y,idx_10);
lon = data.textdata(2:end,3); lon = str2double(lon);
lat = data.textdata(2:end,4);lat = str2double(lat);
T = table(band,lon,lat,Y,idx_10);
writetable(T,'sentinel_1_2_ANN.xlsx','WriteRowNames',true);  
[max_acc,idx_max]=sort(result(:,1),'descend') ;
idx_10 = idx_all(:,idx_max(1:10));
mean(result(idx_max(1:10),:))
%%
result=[];
y_contrast_RF=[];
  for i=1:10
%     idx=randperm(n);
%     idx=idx';
%     idx_all = [idx_all,idx];
    idx=idx_10(:,i);
    var_name = {};
    var_acc = [];
    X_train = X(idx(1:n_train),:);
    Y_train = Y(idx(1:n_train));
    nTree=700;
    nLeaf=5;
    RFModel=TreeBagger(nTree,X_train,Y_train,...
         'Method','regression','OOBPredictorImportance','on', 'MinLeafSize',nLeaf);
   [RFPredictYield,RFPredictConfidenceInterval]=predict(RFModel,X);
    %save(['RF_sentinel_model_2023_1_2_',num2str(i),'.mat'],'RFModel')
    X_test = X(idx(n_train+1:end),:);
    Y_test = Y(idx(n_train+1:end));
    y_contrast_RF =[y_contrast_RF,Y_test];
    Y_pre = predict(RFModel,X_test);
    y_contrast_RF =[y_contrast_RF,Y_pre];
    RMSE=(sum((Y_test-Y_pre).^2)/size(Y_pre,1))^0.5;
    [R,P] = corrcoef(Y_test,Y_pre);
    SSR=sum((Y_pre-mean(Y_test)).^2);
    SSE=sum((Y_test-Y_pre).^2);
    SST=sum((Y_test-mean(Y_test)).^2);
    R2=1-SSE/SST;
    MAE = sum( abs(Y_test-Y_pre) )./length(Y_pre);
    MRE = sum( (  abs(Y_test-Y_pre)./Y_test  ) )/length(Y_pre);
    %result_acc = [R2,RMSE,R(2,1),P(2,1)];
    result_acc = [R2,RMSE,MAE];
    result = [result;result_acc];
    %result_contrast=[Y_test,Y_pre];
  end
result_acc= sum(result,1)./10
%% SOC2  TN 6
final_name = var_name{b};
Y = data.data(:,2:4);
X = data.data(:,8:23);
name_X = name(8:23);
name_X = name_X';
X_final = [];
for i=1:length(final_name)
    for j=1:length( name_X)
        if string(cell2mat(name_X(j)))==string(cell2mat(final_name(i)))
            break
        end
    end 
    X_final=[X_final,X(:,j)];
end


%%
      idx=randperm(277);
      X_train = X_final(idx(1:195),:);
      Y_train = Y(idx(1:195),y_var);
      X_train = X_final;
      Y_train = Y(:,y_var);
      nTree=600;
      nLeaf=5;
      RFModel=TreeBagger(nTree,X_train,Y_train,...
         'Method','regression','OOBPredictorImportance','on', 'MinLeafSize',nLeaf);
      [RFPredictYield,RFPredictConfidenceInterval]=predict(RFModel, X_final);


[importance,index]=sort(RFModel.OOBPermutedPredictorDeltaError,'descend');
VariableImportanceX=final_name(index);
figure('Name','Variable Importance Contrast');
VariableImportanceX=categorical(VariableImportanceX);
bar(VariableImportanceX,importance)
xtickangle(45);
% set(gca, 'XDir','normal')
xlabel('Factor');
ylabel('Importance');

data = importdata('SOC_acc_all.txt');
[a b]=max(data(:,end));


