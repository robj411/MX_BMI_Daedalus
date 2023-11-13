input_path = '../matlab_inputs/';
dae_path = 'functions';
country_file = fullfile('input_data.mat');
load(country_file,'data');
addpath(dae_path);

data_to_fit = readtable(fullfile(input_path,'hospital_occupancy.csv'));
% data_to_fit = data_to_fit((data_to_fit.day<399|data_to_fit.day>625)&data_to_fit.day<733,:);
keeprows = ~isnan(data_to_fit.Hospitalised);
day_count3 = data_to_fit.day(find(keeprows)); %;
ydata3 = data_to_fit.Hospitalised(find(keeprows)) .* 1; 

hospital_parameters = fit_hesitancy(day_count3,ydata3,data,[1,5],data.conSector);

fig = gcf;
close(fig)

data_to_fit = readtable(fullfile(input_path,'hospital_admissions.csv'));
data_to_fit = data_to_fit(data_to_fit.day<733,:);
keeprows = 1 - strcmp(data_to_fit.admissions,'NA'); %admissions
day_count1 = data_to_fit.day(find(keeprows));
day_count1 = data_to_fit.day;
ydata1 = str2double(data_to_fit.admissions(find(keeprows))) ; %admissions
ydata1 = data_to_fit.N .* 1./0.7/0.96; %admissions
columns = cell2mat(cellfun(@(a) strmatch(a, data_to_fit.Properties.VariableNames),...
    {'age0to4','age5to19','age20to64','age65plus'},'uniform',false));
ydatamat = table2array(data_to_fit(:,columns)) .* 1./data.rates.rr' .* [10 10 1 1];
ydata1 = reshape(ydatamat,length(day_count1)*length(columns),1);
day_count1 = repmat(day_count1,length(columns),1);

data_to_fit = readtable(fullfile(input_path,'death_data.csv'));
data_to_fit = data_to_fit(data_to_fit.days<50,:);
day_count2 = data_to_fit.days - 14;
ydata2 = data_to_fit.ydata .* 1./0.7;

data_to_fit = readtable(fullfile(input_path,'hospital_occupancy.csv'));
data_to_fit = data_to_fit(data_to_fit.day<50,:);
keeprows = ~isnan(data_to_fit.Hospitalised);
day_count3 = data_to_fit.day(find(keeprows)); %;
ydata3 = data_to_fit.Hospitalised(find(keeprows)) .* 1; 

poptim = fitEpi20(day_count1,ydata1,day_count2,ydata2,day_count3,ydata3,...
    data,'Hospital admissions','Deaths','Hospital occupancy',hospital_parameters);

poptim.hosp1 = hospital_parameters(1);
poptim.hosp2 = hospital_parameters(2);
poptim.hospthresh = hospital_parameters(3);

save(fullfile('optimised_parameters.mat'),'poptim')

% fig1 = gcf;
% set(fig1, 'PaperUnits', 'inches');
% set(fig1, 'PaperPosition', [0 0 x_width y_width]);
% saveas(fig1,'store/modelfit_hosp.jpg')
% close(fig1)
% 
% fig2 = gcf;
% set(fig2, 'PaperUnits', 'inches');
% x_width=7.25 ;y_width=5;
% set(fig2, 'PaperPosition', [0 0 x_width y_width]); 
% saveas(fig2,'store/modelfit_ad.jpg')
% close(fig2)
% 
% fig3 = gcf;
% set(fig3, 'PaperUnits', 'inches');
% set(fig3, 'PaperPosition', [0 0 x_width y_width]);
% saveas(fig3,'store/modelfit_deaths.jpg')
% close(fig3)


ages = {'0 to 4','5 to 19','20 to 64','65 plus'};
agecats = categorical(ages(2:4));
figure; 
tiledlayout(2,2, 'Padding', 'compact', 'TileSpacing', 'compact');
nexttile
heatmap(ages,ages,round(data.CM,2))
title('Before')
nexttile
heatmap(ages,ages,round(scale_community_matrix(data.CM, poptim.community_scalar),2))
title('After')
nexttile
bar(reordercats(agecats,ages(2:4)),[0.5 1 1.5]'.*[1 1 1; poptim.consumer_scalar]')
xtickangle(0)
legend({'Before','After'},'location','northwest')
ylabel('Hospitality contacts')
nexttile
agecats = {'0 to 4','5 to 19' 'C'};
bar(reordercats(categorical(agecats),agecats),[data.schoolA1 data.schoolA2 sum(data.Cmat(data.edSector,:))]'.*[1 1 1; poptim.school_scalar]')
ylabel('School contacts')


