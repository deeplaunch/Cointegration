function [ RollingCoIntegrationTest, ColumnName ] = runRollingCointegrationTest( AccumulatedReturn, PairName, LookBack, Alpha )
%RUNROLLINGREGRESSION Summary of this function goes here
%   Detailed explanation goes here

%%Define Variable

RollingCoIntegrationTest=struct();

%%Calculate static variables for loop
FieldName=fieldnames(AccumulatedReturn);
NumofPeriod=rows(AccumulatedReturn.(char(FieldName(1,1)))); %Total size of each series
Constant=ones(LookBack,1); %Constant used in linear regression
MultipleResultYX=cell(NumofPeriod-LookBack+1,6); %Total size of each rolling regression results
PairNo=rows(PairName);
% MultipleResultXY=cell(NumofPeriod-LookBack+1,6);
SingleResult=cell(1,6); %used to store results from each individual regression run
sumhYX=0;
sumhXY=0;

%%Set Date for Rolling cointegration test
RollingCoIntegrationTest.(char(FieldName(1,1)))=AccumulatedReturn.(char(FieldName(1,1)))(LookBack:NumofPeriod,1);

%%Run rolling cointegration test
for i = 1: PairNo       
        y= AccumulatedReturn.(char(PairName(i,1)));
        x= AccumulatedReturn.(char(PairName(i,2)));
        sumhYX=0;
        sumhXY=0;
        
        for k = LookBack: (NumofPeriod)  
            % Calculate log of cumulative return, 
            y1=log(cumprod(1+y((k-LookBack+1):k,1)));
            x1=log(cumprod(1+x((k-LookBack+1):k,1)));       
            [h,pValue,stat,cValue,reg1,reg2] = egcitest([y1,x1],'creg','ct','alpha',Alpha); % Cointegration test with confidence level of 100*(1-alpha)%
            SingleResult={h,pValue,stat,cValue,reg1,reg2}; %
            MultipleResultYX(k-LookBack+1,:)=SingleResult;
            sumhYX=sumhYX+h;
         %   if h==1
         %       disp('stop');
         %   end
            %repeat the process switching X and Y
%             [h,pValue,stat,cValue,reg1,reg2] = egcitest([x1,y1],'creg','ct','alpha',Alpha); % Cointegration test with confidence level of 100*(1-alpha)%
%             SingleResult={h,pValue,stat,cValue,reg1,reg2}; %
%             sumhXY=sumhXY+h;        
%             MultipleResultXY(k-LookBack+1,:)=SingleResult;
%             SingleResult=cell(1,6); %Reset
        end
        
        %select the pair with larger h on average i.e. more often to show
        %cointegration
        
%         if sumhYX>sumhXY
        RollingCoIntegrationTest.([char(PairName(i,1)),'_',char(PairName(i,2))])=MultipleResultYX;
%         else
%         RollingCoIntegrationTest.([char(FieldName(j,1)),'_',char(FieldName(i,1))])=MultipleResultXY;
%         end
        
        MultipleResultYX=cell(NumofPeriod-LookBack+1,6); %Reset
%         MultipleResultXY=cell(NumofPeriod-LookBack+1,6); 
 
end

ColumnName={'h'  'pValue' 'stat' 'cValue' 'reg1' 'reg2'};

end

