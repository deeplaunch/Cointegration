function [ PairName ] = LoadPairsName()
%LOADPAIRS loads pair names that are identified previously as significant from Excel
%file
[~,~,PairName]=xlsread('Cointegration Pairs.xls', 'Sheet1');

end

