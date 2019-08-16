function [ output_args ] = UpdateCointegrationAnalysis( StartDate, Frequency, LookBack, Alpha,Cutoff,OutputFile )
%%Run rolling cross-asset regression and reports outliers
%  Freqeuncy: number of days over which daily return is accumulated(5= Weekly return)
%  LookBack: number of data points in each regression
%  Alpha confidence interval= 100*(1-Alpha)%  
% Example: 
% UpdateCointegrationAnalysis('20000101',5,52,0.1,0.3,'\\acntnyc037\eqres\quant\data\SurpriseExcel_GMP\Cointegration Analysis\Cointegration_Template.xls')

%% Load asset daily returns
DailyReturn=LoadAssetReturnsForBetaAnalysis( StartDate ); %Fist Column is date

%% Accumulate asset returns based on freqeuency
AccumulatedReturn=Daily2Accumulated(DailyReturn, Frequency);

%%Load pairnames to run

PairName=LoadPairsName();

%% Run rolling cointegration test
[RollingCoIntegrationTest, ColumnName]=runRollingCointegrationTest(AccumulatedReturn, PairName, LookBack, Alpha );

%%Wriet test result to excel
[OutputDate,Tabname]=WriteCointegration2Excel(Cutoff,RollingCoIntegrationTest, ColumnName, OutputFile);

%%Write weekly returns to excel

WriteRet2Excel(OutputDate,Tabname,AccumulatedReturn,OutputFile,'K');

end

