%%
%Load raw data from doric recording system
data_path = '.csv';
num=csvread(data_path,2,2);


%Raw data pre analysis channel1
[a,b] = find(num(:,4)==0);
raw_data_all = num(:,4);
raw_data_405_digital = num(:,10);
raw_data_475_digital = num(:,11);
raw_data_405_digital(raw_data_all==0)=[];
raw_data_475_digital(raw_data_all==0)=[];
raw_data_all(raw_data_all==0)=[];
%Find 0.5 value point
[a,b] = find(raw_data_405_digital==0.5);
[c,d] = find(raw_data_475_digital==0.5); 
g = intersect(a,c);
%Delete 0.5 value
raw_data_405_digital(raw_data_475_digital==0.5) = [];
raw_data_all(raw_data_475_digital==0.5) =[];
raw_data_475_digital(raw_data_475_digital==0.5) = [];
raw_data_475_digital(raw_data_405_digital==0.5) = [];
raw_data_all(raw_data_405_digital==0.5) = [];
raw_data_405_digital(raw_data_405_digital==0.5) = [];


%Rawdata_pre_analysis,channel2
[a,b] = find(num(:,9)==0);
raw_data_all = num(:,9);
raw_data_405_digital = num(:,12);
raw_data_475_digital = num(:,13);
raw_data_405_digital(raw_data_all==0)=[];
raw_data_475_digital(raw_data_all==0)=[];
raw_data_all(raw_data_all==0)=[];
%Find 0.5 value point
[a,b] = find(raw_data_405_digital==0.5);
[c,d] = find(raw_data_475_digital==0.5); 
g = intersect(a,c);
%Delete 0.5 value
raw_data_405_digital(raw_data_475_digital==0.5) = [];
raw_data_all(raw_data_475_digital==0.5) =[];
raw_data_475_digital(raw_data_475_digital==0.5) = [];
raw_data_475_digital(raw_data_405_digital==0.5) = [];
raw_data_all(raw_data_405_digital==0.5) = [];
raw_data_405_digital(raw_data_405_digital==0.5) = [];


se = [1;1;1];
%405nm channel data process
raw_data_405_digital_e = imdilate(raw_data_405_digital, se);
raw_data_405_digital_e = imerode(raw_data_405_digital_e, se);
raw_data_405_digital_e = imerode(raw_data_405_digital_e, se);
raw_data_405_digital_e = imdilate(raw_data_405_digital_e, se);
%475nm channel data process
raw_data_475_digital_e = imdilate(raw_data_475_digital, se);
raw_data_475_digital_e = imerode(raw_data_475_digital_e, se);
raw_data_475_digital_e = imerode(raw_data_475_digital_e, se);
raw_data_475_digital_e = imdilate(raw_data_475_digital_e, se);


%Find the start position of 405nm and 475nm channel signal, as m_405 and m_475
raw_data_475_digital_2 = [raw_data_475_digital_e;0];
raw_data_475_digital_2(1,:)=[];
raw_data_475_digital_3 = raw_data_475_digital_2 - raw_data_475_digital_e;
[m_475,n_475]=find(raw_data_475_digital_3 ==1);
m_475 = m_475 + 1;
raw_data_405_digital_2 = [0;raw_data_405_digital_e];
raw_data_405_digital_2(end,:)=[];
raw_data_405_digital_3 = raw_data_405_digital_e - raw_data_405_digital_2;
[m_405,n_405]=find(raw_data_405_digital_3 ==1);



%Extract each small piece of data into a small piece of the target and splicing
N = numel(m_475);
for i = 1:N-1
data_475_segment(:,i) = raw_data_all ((m_475(i,:)+15):(m_475(i,:)+24),:);
end
for i = 1:N-1
data_405_segment(:,i) = raw_data_all ((m_405(i,:)+15):(m_405(i,:)+24),:);
end



%Each 600 column is a short record of 30s, a total of 144 30s records, the data of each 30s is integrated into one column, and merged into data_475/405_timeseries
c = size(data_405_segment,2)
d = floor(c/600)
data_475_timeseries = ones(6000, d);
for i = 1:d
    for j = 1:600
        data_475_timeseries((j-1)*10+1:j*10,i) = data_475_segment(:,(i-1)*600+j);
    end
end
data_405_timeseries = ones(6000, d);
for i = 1:d
    for j = 1:600
        data_405_timeseries((j-1)*10+1:j*10,i) = data_405_segment(:,(i-1)*600+j);
    end
end


%Remove the first 1000 rows of data to remove the sharp rise in signal caused when the laser is just turned on
data_475_timeseries_1000 = data_475_timeseries (1001:end,:);
data_475_1000 = data_475_timeseries_1000(:);
data_405_timeseries_1000 = data_405_timeseries (1001:end,:);
data_405_1000 = data_405_timeseries_1000(:);
data_475_405_1000 = data_475_1000 - data_405_1000;
plot(data_475_405_1000)
for i = 1:numel(data_475_405_1000)
    delta_f_f(i,:) = data_475_405_1000(i,:)/data_405_1000(i,:);
end
plot(delta_f_f)


%Take the average value for a short period of every 30s
for i = 1:d
    data_475_time_mean(i,:) = mean(data_475_timeseries_1000(:,i));
end
for i = 1:d
    data_405_time_mean(i,:) = mean(data_405_timeseries_1000(:,i));
end
data_475_405_time_mean = data_475_time_mean - data_405_time_mean;
for i = 1:numel(data_475_405_time_mean)
    delta_f_f_475_405_time_mean(i,:) = data_475_405_time_mean(i,:)/data_405_time_mean(i,:);
end
bar(delta_f_f_475_405_time_mean)