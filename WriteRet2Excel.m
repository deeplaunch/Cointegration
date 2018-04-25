function [ output_args ] = WriteRet2Excel( OutputDate,TabName,AccumulatedReturn,FileName,StartColumn)
%WRITEFWDRET2EXCEL writes cumulative daily returns to the specified
%Write current return of y and x for each period
NumofTab=rows(TabName);

for i=2:NumofTab
    variable=strsplit(char(TabName(i,1)),'_');
    yName=variable(1,1);
    xName=variable(1,2);
    [C,index]=intersect(AccumulatedReturn.DataDate,OutputDate); %find common date
    yReturn=zeros(size(index));
    xReturn=zeros(size(index));
    yReturn(:,1)=AccumulatedReturn.(char(yName))(index(:,1));
    xReturn(:,1)=AccumulatedReturn.(char(xName))(index(:,1));
    xlswrite(FileName, [yReturn,xReturn], (char(TabName(i,1))),horzcat(StartColumn,'2'));
    xlswrite(FileName, [yName,xName], (char(TabName(i,1))),horzcat(StartColumn,'1'));
end

end

