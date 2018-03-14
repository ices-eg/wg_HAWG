clear all
close all
clc

pathData = 'D:\git\wg_HAWG\NSAS\benchmark\results\miscellaneous\';  

A = readtable(strcat(pathData, 'time_series_all.csv'), 'Delimiter', ',');

% varHERAS = A(strcmp(A.cname, 'HERAS'),:);
% varcanum = A(strcmp(A.cname, 'canum'),:);

age = 2;

for age = 1:6

currentA = A(str2double(A.age) == age,:);
currentA = currentA(~strcmp(currentA.cname, 'canum'),:);

surveys = unique(currentA.cname);

years = unique(currentA.year);

% for idxSurvey = 1:length(surveys)
%     currentYearVec = currentA.year(str2double(currentA.age) == age & strcmp(currentA.cname,surveys(idxSurvey)));
%     lengthYear(idxSurvey) = length(currentA.year(str2double(currentA.age) == age & strcmp(currentA.cname,surveys(idxSurvey))));
%     surveys(idxSurvey) = erase(surveys(idxSurvey),'-');
% end

Y = NaN(length(years), length(surveys));

for idxSurvey = 1:length(surveys)
    tempYear = currentA.year(str2double(currentA.age) == age & strcmp(currentA.cname,surveys(idxSurvey)));
    tempData = currentA.data(str2double(currentA.age) == age & strcmp(currentA.cname,surveys(idxSurvey)));
    [~,~,idxYear] = intersect(tempYear,years);
    Y(idxYear, idxSurvey) = tempData;
end

Y = flipud(Y);
Y = array2table(Y, 'VariableNames',erase(surveys, '-'));

% Y = Y(:,1:2);
% 
% plot(table2array(Y(:,1)), table2array(Y(:,2)), 'or')
% 
% plotyy(1:28, table2array(Y(1:28,1)), 1:28, table2array(Y(1:28,2)))
% plot()
% hold on
% plot(table2array(Y(1:28,2)))
% 
% x1 = varcanum.year(str2double(varcanum.age) == age);
% y1 = varcanum.data(str2double(varcanum.age) == age);
% 
% x2 = varHERAS.year(str2double(varHERAS.age) == age);
% y2 = varHERAS.data(str2double(varHERAS.age) == age);
% 
% x3 = varHERAS.year(str2double(varHERAS.age) == age);
% y3 = varHERAS.data(str2double(varHERAS.age) == age);
% 
% plotyy(x1,(y1),x2,(y2))
% 
% [C,i1,i2] = intersect(x1, x2);
% 
% [RHO,PVAL] = corr(y1(i1),y2(i2))
% 
% Y2 = array2table([y1(i1),y2(i2)], 'VariableNames',{'canum', 'HERAS'});

corrplot(Y)
hfig = gcf;
haxes = findobj(hfig, 'Type', 'Axes');
title(haxes, 'salut')
get(haxes, 'title')
set(hfig ,'Name', 'age 1', 'NumberTitle', 'on');

set(haxes, 'title', 'salut')
end