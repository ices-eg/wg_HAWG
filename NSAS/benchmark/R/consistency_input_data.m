clear all
close all
clc

pathData = 'D:\git\wg_HAWG\NSAS\benchmark\results\miscellaneous\';

A = readtable(strcat(pathData, 'stat_res.csv'), 'Delimiter', ',');

age = str2double(A.age_comp);

age_comp = unique(age);

matRes = struct();

boundCorr = 1;
alpha = 0.05;

for idxAge = 1:length(age_comp)

    currentAgeIdx = find(age == age_comp(idxAge));

    surveyName = A.survey_name(currentAgeIdx);
    surveyCompName = A.survey_name_comp(currentAgeIdx);
    corr = A.corr(currentAgeIdx);
    pvalue = A.pvalue(currentAgeIdx);

    uniqueSurvey = unique(surveyName);

    I = zeros(length(unique(surveyName)),length(uniqueSurvey));
    Iplot = zeros(length(unique(surveyName)),length(uniqueSurvey));
    
    corrCurrent = [];
    pvalueCurrent = [];
    for idxSurveyName = 1:length(uniqueSurvey)
        Index = find(~contains(surveyCompName,uniqueSurvey(idxSurveyName)));

        surveyNameCurrent = uniqueSurvey(idxSurveyName);
        a = 1:length(uniqueSurvey);
        surveyCompNameCurrent = uniqueSurvey(a(a~=idxSurveyName));

        for idx1 = 1:length(surveyCompNameCurrent)
            corrCurrent(idx1) = corr(strcmp(surveyName, surveyNameCurrent) & strcmp(surveyCompName, surveyCompNameCurrent(idx1)));
            pvalueCurrent(idx1) = pvalue(strcmp(surveyName, surveyNameCurrent) & strcmp(surveyCompName, surveyCompNameCurrent(idx1)));
        end

        I(idxSurveyName,a(a~=idxSurveyName)) = corrCurrent;
        I(a(a~=idxSurveyName),idxSurveyName) = corrCurrent;
%         I(a(a~=idxSurveyName),idxSurveyName) = pvalueCurrent;
    end
%     L = tril(I)/boundCorr;
% 	U = triu(I);
%     U = U/alpha;
%     U(U>1) = 1;
%     U = abs(U-1);
%     U = triu(U);
%     Iplot = U+L;
%     Iplot = Iplot - diag(diag(Iplot));
    
    matRes(idxAge).I = I;
%     matRes(idxAge).Iplot = Iplot;
    matRes(idxAge).Iplot = I;
    matRes(idxAge).names = uniqueSurvey;
    matRes(idxAge).age = age_comp(idxAge);
    
    [myColorMap]=buildcmap('ryg');

    lowThreshold = 0;
    highThreshold = 1;
    dThreshold = [lowThreshold highThreshold];

    ctBot = 1;
    ctTop = length(myColorMap);
    ctRng = ctTop - ctBot;
    data =  uint8(ceil((matRes(idxAge).Iplot - dThreshold(1)) / (dThreshold(2) - ...
            dThreshold(1)) * ctRng) + ctBot);

    ih = image(1:size(data,1), 1:size(data,2), data);
    
    xt = 1:size(data,1);
    yt = 1:size(data,1);
    xticks(xt)
    xticklabels(uniqueSurvey)
    yticks(yt)
    yticklabels(uniqueSurvey)

    colormap(myColorMap);
    
    for k = 1:length(data)
        for l = 1:length(data)
            txt1 = strcat(num2str(I(l,k)));
            text(xt(k),yt(l),txt1)
        end
    end
    title(strcat('age', num2str(age_comp(idxAge))))
    
    set(gcf, 'Units', 'centimeters');
    set(gcf, 'PaperOrientation', 'portrait ');  
    
    set(gcf, 'Units', 'centimeters');
    set(gcf, 'PaperOrientation', 'portrait ');

    % we set the position and dimension of the figure ON THE SCREEN
    %
    % NOTE: measurement units refer to the previous settings!
    afFigurePosition = [1 1 11 8]; % [pos_x pos_y width_x width_y]
    set(gcf, 'Position', afFigurePosition); % [left bottom width height]
    print('-dpng','-r300',strcat(pathData,'consistency input date - ', num2str(age_comp(idxAge))))
    close(gcf)
end