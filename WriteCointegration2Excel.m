function [ Date, TabName ] = WriteCointegration2Excel(Cutoff, RollingCoIntegrationTest, ColumnName,FileName )
%WRITEREGRESSIONRESULTTOEXCEL writes to excel, where Each tab is one series of rolling regression

PairName=fieldnames(RollingCoIntegrationTest);
NumofPair=rows(PairName);
NumofRows=rows(RollingCoIntegrationTest.(char(PairName(1,1))));
data4Excel=zeros(NumofRows,10);
NewColumnName=[ColumnName(1,1:4),'Constant','Trend','Beta','RMSE','EndResidual'];
Date=RollingCoIntegrationTest.(char(PairName(1,1)));
data4Excel(:,1)=Date; %date
TabName=cell(NumofPair,1);
TabName(1,1)=PairName(1,1);
% Write cointegration test result for each period
for i= 2: NumofPair
    for j=1:NumofRows
    data4Excel(j,2)=RollingCoIntegrationTest.(char(PairName(i,1))){j,1};
    data4Excel(j,3)=RollingCoIntegrationTest.(char(PairName(i,1))){j,2};
    data4Excel(j,4)=RollingCoIntegrationTest.(char(PairName(i,1))){j,3};
    data4Excel(j,5)=RollingCoIntegrationTest.(char(PairName(i,1))){j,4};
    data4Excel(j,6:8)=RollingCoIntegrationTest.(char(PairName(i,1))){j,5}.coeff(:,1);
    data4Excel(j,9)=RollingCoIntegrationTest.(char(PairName(i,1))){j,5}.RMSE;
    data4Excel(j,10)=RollingCoIntegrationTest.(char(PairName(i,1))){j,5}.res(end,1);
    end
    
   % if mean(data4Excel(:,2))> Cutoff % Only write when cointegration is present for more than 30% time in history
    xlswrite(FileName, NewColumnName, (char(PairName(i,1))),'B1'); %write column name to tab
    xlswrite(FileName, data4Excel, (char(PairName(i,1))),'A2'); % write results to tab
    TabName(i,1)=PairName(i,1);
   % end
    
    data4Excel(:,2:10)=zeros(NumofRows,9); % Reset
end

end




