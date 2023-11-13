
data = struct;

% set path for inputs
input_path = '../matlab_inputs/';
% data_path = '../../data/';

%% sectors
% read in multi inputs: summary of sectors
sector_summary = readtable(fullfile(input_path,'sector_summary.csv'));
nSectors = size(sector_summary,1);

% B
data.B = sector_summary.B';

% C
% data.C = sector_summary.C';
columns = cell2mat(cellfun(@(a) strmatch(a, sector_summary.Properties.VariableNames),...
    {'age0to4','age5to19','age20to64','age65plus'},'uniform',false));
data.Cmat = table2array(sector_summary(:,columns));
data.Emat = table2array(readtable(fullfile(input_path,'worker_to_customer.csv')));

% wfhAv
data.wfhAv = sector_summary.WFH';

% obj
data.obj = sector_summary.GVApm;

% IO
data.IO = table2array(readtable(fullfile(input_path,'IO.csv')));

% final demand fraction
data.final_demand_consumption_frac = sector_summary.consumption_frac_of_final_demand;


%% population
% population sizes of 24 groups
NN = readtable(fullfile(input_path,'population_by_sector.csv'));
four_community_groups = NN.number(nSectors+(1:4));
data.NN = [sector_summary.Workforce; four_community_groups];

% population size by age
% Npop = readtable(fullfile(input_path,'population_by_age.csv')).Population;
% data.Npop = Npop;

% contact matrix
data.CM = table2array(readtable(fullfile(input_path,'contact_matrix.csv')));

ylltab = readtable(fullfile(input_path,'yll.csv'));
data.ylltab = ylltab;

%% vaccinations
vaccine_struct = struct;
vaccine_struct.antigenic_drift = 0;
vaccine_struct.waned_infected = table2array(readtable(fullfile(input_path,'waned_infected_1.csv')));
% vaccine_struct.waned_infected_half= table2array(readtable(fullfile(input_path,'waned_infected_half.csv')));
vaccine_struct.scenarios = {};
for i = 1:7
    vaccine_struct.scenarios{i} = struct;
    vaccine_struct.scenarios{i}.waned_infected =...
        table2array(readtable(fullfile(input_path,sprintf('waned_infected_%d.csv',i))));
    vaccination_rates = ...
        readtable(fullfile(input_path,sprintf('second_vaccinations_%d.csv',i)));
    vaccine_struct.scenarios{i}.total = vaccination_rates.total;
    vaccine_struct.scenarios{i}.vaccination_proportions = ...
        table2array(vaccination_rates(:,3:6));
end

% vaccination rates
vaccination_rates = readtable(fullfile(input_path,'second_vaccinations_1.csv'));
atimes = vaccination_rates.day;
arates = vaccination_rates.total;
vaccination_proportions = table2array(vaccination_rates(:,3:6));
% vaccination_rates = readtable(fullfile(input_path,'second_vaccinations_half.csv'));
% vaccine_struct.arates_half = vaccination_rates.total;

% booster rates
% booster_rates = readtable(fullfile(input_path,'third_vaccinations.csv'));
% vaccine_struct.booster_rates0 = readtable(fullfile(input_path,'third_vaccinations_booster0.csv'));
% vaccine_struct.booster_rates1 = readtable(fullfile(input_path,'third_vaccinations_booster1.csv'));
% vaccine_struct.booster_rates2 = readtable(fullfile(input_path,'third_vaccinations_booster2.csv'));
% a2rates = booster_rates.total;
% booster_proportions = table2array(booster_rates(:,3:7));


vaccine_struct.vaccination_policy_change_times = [atimes; Inf];
vaccine_struct.vaccines_per_day = [0; arates];
vaccine_struct.proportions = [zeros(1,size(vaccination_proportions,2)); vaccination_proportions];
% vaccine_struct.boosters_per_day = [0; a2rates];
% vaccine_struct.booster_proportions = [zeros(1,size(booster_proportions,2)); booster_proportions];

%%!! manually map sectors
colnames = vaccination_rates.Properties.VariableNames;
% healthworker_index = find(strcmp(colnames,'Health_workers')) - 2;
infant_index = find(strcmp(colnames,'children')) - 2;
youth_index = find(strcmp(colnames,'X12_to_17_years_old')) - 2;
adult_index = find(strcmp(colnames,'X20_to_64_years_old')) - 2;
oldest_index = find(strcmp(colnames,'X65_plus')) - 2;
vaccine_struct.map = repmat(adult_index,1,length(data.NN));
% vaccine_struct.map(34) = healthworker_index;
vaccine_struct.map(nSectors+1) = infant_index;
vaccine_struct.map(nSectors+2) = youth_index;
vaccine_struct.map(nSectors+4) = oldest_index;

%% vaccination parameters
vaccine_profiles = readtable(fullfile(input_path,'vaccine_profiles.csv'));
param_names = vaccine_profiles.Parameter;
vaccine_1 = vaccine_profiles.Average;
% vaccine_2 = vaccine_profiles.mRNA1273;

%Vaccine 1 (two doses)
vaccine_struct.hrv1 = 1/vaccine_1(strcmp(param_names,'Expected time to seroconversion')); %time to develop v-acquired immunity (AstraZeneca)
vaccine_struct.scv1 = vaccine_1(strcmp(param_names,'Acquisition')); %infection-blocking efficacy
vaccine_struct.p1v1 = vaccine_1(strcmp(param_names,'Symptomatic given infection'));  %disease-blocking efficacy
vaccine_struct.hv1 = vaccine_1(strcmp(param_names,'Hospitalisation given symptomatic'));  %severe-disease-blocking efficacy
vaccine_struct.dv1 = vaccine_1(strcmp(param_names,'Death given hospitalisation'));  %death-blocking efficacy
vaccine_struct.trv1 = vaccine_1(strcmp(param_names,'Transmission'));  %transmission-blocking efficacy
vaccine_struct.nuv1 = 1/vaccine_1(strcmp(param_names,'Expected time to wane'));  %duration of v-acquired immunity
vaccine_struct.maturation_rate = 1/vaccine_1(strcmp(param_names,'Expected time before waning'));  

%Vaccine 2 (booster)
% vaccine_struct.hrv2 = 1/vaccine_2(strcmp(param_names,'Expected time to seroconversion')); %time to develop v-acquired immunity (AstraZeneca)
% vaccine_struct.scv2 = vaccine_2(strcmp(param_names,'Acquisition')); %infection-blocking efficacy
% vaccine_struct.p1v2 = vaccine_2(strcmp(param_names,'Symptomatic given infection'));  %disease-blocking efficacy
% vaccine_struct.hv2 = vaccine_2(strcmp(param_names,'Hospitalisation given symptomatic'));  %severe-disease-blocking efficacy
% vaccine_struct.dv2 = vaccine_2(strcmp(param_names,'Death given hospitalisation'));  %death-blocking efficacy
% vaccine_struct.trv2 = vaccine_2(strcmp(param_names,'Transmission'));  %transmission-blocking efficacy
% vaccine_struct.nuv2 = 1/vaccine_2(strcmp(param_names,'Expected time to wane'));  %duration of v-acquired immunity
% vaccine_struct.maturation_rate2 = 1/vaccine_2(strcmp(param_names,'Expected time before waning'));  

%% economic

x_values = table2array(readtable(fullfile(input_path,'x_values.csv')));
data.x_values= x_values;

mandate = table2array(readtable(fullfile(input_path,'mandate.csv')));
data.mandate = mandate(2:end,:);
data.tvec = mandate(1,:);

gva_max = table2array(readtable(fullfile(input_path,'gva_max.csv')));
data.gva_max = gva_max;

data.gdp_growth = table2array(readtable(fullfile(input_path,'gdp_growth.csv')));

data.mobility = table2array(readtable(fullfile(input_path,'mobility.csv')));

%%
% miscellaneous parameters:
param_struct = struct;
parameters = readtable(fullfile(input_path,'parameters.csv'));
param_names = parameters.Parameter;
param_vals = parameters.Value;

Delta = struct;
Omicron = struct;
for i = 1:numel(param_names)
    this_name = param_names{i};
    if contains(param_names{i},'Delta')
        Delta.(this_name) = param_vals(i);
    elseif contains(param_names{i},'Omicron')
        Omicron.(this_name) = param_vals(i);
    else 
        param_struct.(this_name) = param_vals(i);
    end
    
end

param_struct.p1 = ones(nSectors+4,1).*param_struct.p1;

%Recovery Rates (proportion and rate combined)
param_struct.g1=1/param_struct.Ta;
param_struct = rmfield(param_struct,'Ta');

%Immunity Loss
% Ti = 180; %365;%from Global
param_struct.nu = 1/param_struct.Ti;
param_struct = rmfield(param_struct,'Ti');

param_struct.Delta = Delta;
param_struct.Omicron = Omicron;

%
data.edSector = param_struct.edSector;
param_struct = rmfield(param_struct,'edSector');
con = dec2base(param_struct.conSector,10) - '0';
data.conSector = nSectors - length(con) + find(con==1);
param_struct = rmfield(param_struct,'conSector');

data.hospA2 = param_struct.hospA2;
data.hospA3 = param_struct.hospA3;
data.hospA4 = param_struct.hospA4;
data.travelA3 = param_struct.travelA3;
data.schoolA1 = param_struct.schoolA1;
data.schoolA2 = param_struct.schoolA2;
param_struct = rmfield(param_struct,'hospA2');
param_struct = rmfield(param_struct,'hospA3');
param_struct = rmfield(param_struct,'hospA4');
param_struct = rmfield(param_struct,'schoolA1');
param_struct = rmfield(param_struct,'schoolA2');
param_struct = rmfield(param_struct,'travelA3');

param_struct.alpha = 1;
data.alpha = param_struct.alpha;

param_struct = rmfield(param_struct,'alpha');
param_struct = rmfield(param_struct,'q1');
param_struct = rmfield(param_struct,'q2');
param_struct = rmfield(param_struct,'Hmax');

parametersbyage = readtable(fullfile(input_path,'parameters_by_age.csv'),'Format','%f%f%f%f');
rates_struct = struct;
for i = 1:numel(parametersbyage.Properties.VariableNames)
    this_name = parametersbyage.Properties.VariableNames{i};
    rates_struct.(this_name) = parametersbyage.(parametersbyage.Properties.VariableNames{i});
end

data.rates = rates_struct;

param_struct.vx = vaccine_struct;

data.fit_routine = 0;

data.ageindex.adInd = 3;
data.ageindex.community = nSectors + [1:4];
data.ageindex.age0to4 = nSectors + 1;
data.ageindex.age5to19 = nSectors + 2;
data.ageindex.age20to64 = [1:nSectors, nSectors + 3];
data.ageindex.age65plus = nSectors + 4;

data.pr = param_struct;

%% school

xs = data.x_values;
month_starts = datetime([2020 1 1]) + calmonths(0:size(xs,2));
month_tvec = daysact(datetime([2019 12 31]),month_starts);
school_opening = xs(data.edSector,:);
new_school_opening = data.tvec;
end_times = [data.tvec(2:end) max(data.tvec(2:end))+14];
for i = 1:length(new_school_opening)
    start_time = data.tvec(i);
    end_time = end_times(i);
    new_duration = end_time - start_time;
    old_indices = find(month_tvec<=start_time,1,'Last'):find(month_tvec>=end_time,1,'First');
    weights = diff(sort([start_time month_tvec(old_indices) end_time]));
    new_school_opening(i) = sum(school_opening(old_indices(1:(end-1))) .* weights(2:(end-1))) ./ sum(weights(2:(end-1)));
end
new_school_opening(3) = 0;
data.mandate(data.edSector,:) = new_school_opening;

%%

% save('xmins_IDN.mat','xmins')
save('input_data.mat','data')

