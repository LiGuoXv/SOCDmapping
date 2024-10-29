%%  原始光谱
% [B, R]=readgeoraster('ALL_Band3.8.tif');   
clc
clear
load('sentinel2_map2.mat')
clearvars -except RFModel


[B, R1]=readgeoraster('C:\scientific_research\SOC\data\sentienal\1_2_area.tif'); 
[class, R2]=readgeoraster('C:\scientific_research\SOC\data\class\bareland1.tif'); 
fid=fopen('sentinel_2023_1_2_area_bareland.txt','W');
fprintf(fid,'b1,b2,b3,b4,b5,b6,b7,b8,b9,X,Y\n');
[M N]=size(class);
for i=1:M
    i
    for j=1:N
        if(class(i,j)==1)
            [lat, lon] = pix2latlon(R1,i,j);
            band = B(i,j,:);
            band = reshape(band,[1,9]);
            fprintf(fid,'%d,%d,%d,%d,%d,%d,%d,%d,%d,%f,%f\n',band,lon,lat);
        end
    end
end
fclose(fid);
data = importdata('sentinel_2023_1_2_area_bareland.txt');


info=geotiffinfo('C:\scientific_research\SOC\data\sentienal\1_2_area.tif');   





B(:,:,8)=[];
%%
i=1; 
[M,N,~]=size(B);
for a=1:M
    for b=1:N
        B_1W(:,i)=B(a,b,:); i=i+1;
    end
end

%% rf
clc
clear
[B, R1]=readgeoraster('D:\scientific_research\SOC\SOC\data\sentienal\1_2_area.tif'); 
[class, R2]=readgeoraster('D:\scientific_research\SOC\SOC\data\class\bareland1.tif'); 
info=geotiffinfo('D:\scientific_research\SOC\SOC\data\sentienal\1_2_area.tif');   
[M N]=size(class);
B_1W=[];
i=1;
for a=1:M
    for b=1:N
       B_1W(:,i)=B(a,b,:);
        i=i+1;
    end
end

B_1W_T=B_1W';
clearvars -except R2 N M info B_1W_T
for num=6:10
load(['RF_sentinel_model_2023_1_2_',num2str(num),'.mat'])
Rf_pre=zeros(size(B_1W_T,1),1);
for i=1:100000:size(B_1W_T,1)
    if i==197000001
        band=B_1W_T(i:end,:);
        band(band<0)=nan;
        band=band*0.0001;
        Meanreflectance = sum(band,2)./9;
        B_Normalize = band./Meanreflectance;
%         NIR =band(:,7);
%         RED =band(:,3);
%         BLUE =band(:,1);
%         EVI = 2.5.* ((NIR - RED) ./ (NIR + 6 .* RED - 7.5.* BLUE + 1));
%         NDVI = (NIR - RED)./(NIR + RED);
%         DVI = NIR - RED;
%         RVI = NIR ./ RED;
%         SAVI =(NIR - RED) .* (1 + 0.5)./(NIR + RED + 0.5);
%         NDWI = (band(:,2) - NIR)./ (band(:,2) + NIR);
        X=B_Normalize;
        [Y_pre,~]=predict(RFModel,X);
        Y_pre(isnan(band(:,1)))=nan;
        Rf_pre(i:end)=Y_pre;
    else
        band=B_1W_T(i:(i+100000-1),:);
        band(band<0)=nan;
        band=band*0.0001;
        Meanreflectance = sum(band,2)./9;
        B_Normalize = band./Meanreflectance;
%         NIR =band(:,7);
%         RED =band(:,3);
%         BLUE =band(:,1);
%         EVI = 2.5.* ((NIR - RED) ./ (NIR + 6 .* RED - 7.5.* BLUE + 1));
%         NDVI = (NIR - RED)./(NIR + RED);
%         DVI = NIR - RED;
%         RVI = NIR ./ RED;
%         SAVI =(NIR - RED) .* (1 + 0.5)./(NIR + RED + 0.5);
%         NDWI = (band(:,2) - NIR)./ (band(:,2) + NIR);
        X=B_Normalize;
        [Y_pre,~]=predict(RFModel,X);
        Y_pre(isnan(band(:,1)))=nan;
        Rf_pre(i:(i+100000-1))=Y_pre;
    end
    i            
end

SOC_rf=zeros(M,N);
i=1;
for a=1:M
    for b=1:N
       SOC_rf(a,b)= Rf_pre(i);
        i=i+1;
    end
end
geotiffwrite(['SOC_map_2023_1_2_',num2str(num)],SOC_rf, R2, 'GeoKeyDirectoryTag', ...
    info.GeoTIFFTags.GeoKeyDirectoryTag)
end


% clearvars -except R N M info HH_1w HH_1w_T  Rf_pre Rf_pre
%%
load('svm');
SVM_pre=predict(net,HH_1w_T);
Cd_svm=zeros(M,N);
i=1;
for a=1:M
    for b=1:N
       Cd_svm(a,b)= SVM_pre(i);
        i=i+1;
    end
end
Cd_svm=exp(Cd_svm);
Cd_svm(isnan(Cd_svm))=0;
geotiffwrite('Cd_svm',Cd_svm, R, 'GeoKeyDirectoryTag', ...
    info.GeoTIFFTags.GeoKeyDirectoryTag);
clearvars -except R N M info HH_1w HH_1w_T