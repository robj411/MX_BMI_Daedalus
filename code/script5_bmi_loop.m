dae_path = 'functions';
addpath(dae_path);

load('optimised_parameters.mat','poptim')
load('input_data.mat','data');
data.x_values = data.x_values(:,1:22);
[ns,nt] = size(data.x_values);

nNewMonths = length(poptim.map_parameters_to_modifier(1:nt)) - nt;% + 6;
xs = [data.x_values, repmat(1-.5*(1-data.x_values(:,14)),1,nNewMonths)];
xs(data.edSector,(nt+1):(nt+nNewMonths)) = 1;
xit = reshape(min(xs,1),(nt+nNewMonths)*ns,1);

nSectors = length(xit)/nt;

data = add_params_to_data(data, poptim);

mod = poptim.modifier(poptim.map_parameters_to_modifier);
rng(0)
mod(nt+[1:nNewMonths]) = randsample(mod(4:nt),nNewMonths,true);

month_starts = datetime([2020 1 1]) + calmonths(0:(nt+nNewMonths));
tvec = [poptim.seedtime, daysact(datetime([2019 12 31]),month_starts)];
data.xit = xit;
data.tvec = tvec;
   
%%
[f,g,data] = heRunCovid19(data,poptim.R0,mod);
f0 = f;
orig_beta = data.pr.beta;
tout0 = f0.restart_initial_conditions.toutAll;

tout = tout0;

%% 
objFun = struct;
objFun.tvec = tvec;
objFun.nSectors = nSectors;
objFun.pr = data.pr;
objFun.NN = data.NN;
objFun.data = data;
objFun.eval_period = 1:22;
        
max_gdp = sum(sum(data.gva_max(:,objFun.eval_period),1));
gdp = sum(sum(data.x_values(:,objFun.eval_period).*data.gva_max(:,objFun.eval_period),1));
workers = f.restart_initial_conditions.workersout;
%[gdp, gdp_by_year, new_x] = compute_revenue(objFun,xs,tout,workers);
%gdploss1 = (1-gdp/max_gdp)*100;
        
death_by_age = f.Doutbyage(end,:);
deaths1 = f.Dout(end);
new_tout = 1:max(tout);
infections1 = sum(sum(f.restart_initial_conditions.InewbyageAll'));
hosp1 = sum(interp1(tout,f.Hout,new_tout));
byage1 = f.Doutbyage(end,:);
yll1 = sum(data.ylltab.yll'.*byage1);

results = [{'Infections','YLLs','Total deaths',...
    'Deaths age 0-4','Deaths age 5-19','Deaths age 20-64','Deaths age 65+'...
    'Cumulative hospital days'}', ...
    num2cell([...
    %round(gdp/1e6,1),...
    %round(gdploss1,1),...
    infections1,...
    yll1,...
    deaths1,...
    death_by_age,...
    hosp1]')];

writecell(results,'store/baseline.csv')
disp(f.restart_initial_conditions.Iout((end-3):end))

%% bmi: acquisition scenario

orig_p1 = data.pr.p1;
orig_ihr = data.rates.ihr;
orig_ifr = data.rates.ifr;

    
input_path = '../matlab_inputs/';
nSamples = 8192;
scenario_labels = {'Labelling','Double tax','Triple tax','Quadruple tax','2000','2006','2012','No obesity'};

for sample = 1:nSamples
    rrs = readtable(fullfile(input_path,sprintf('relativerisks\\sample%d.csv',sample)));

    for i = 1:(size(rrs,2)-1)/2
        diag_scen{i} = table2array(rrs(1,1+[1:2]+(i-1)*2))/100;
        hosp_scen{i} = table2array(rrs(2,1+[1:2]+(i-1)*2))/100;
        death_scen{i} = table2array(rrs(3,1+[1:2]+(i-1)*2))/100;
    end



    for i = 1:length(scenario_labels)

        diagnoses = diag_scen{i};
        hospitalisations = hosp_scen{i};
        deathrates = death_scen{i};


        %%!! NB: need to run above (baseline) scenario first to get beta
        symptomatic_rr = ones(4,1);
        hospitalisation_rr = ones(4,1);
        fatality_rr = ones(4,1);
        symptomatic_rr(data.ageindex.adInd) = diagnoses(1);
        symptomatic_rr(data.ageindex.age65plus-nSectors) = diagnoses(2);
        hospitalisation_rr(data.ageindex.adInd) = hospitalisations(1);
        hospitalisation_rr(data.ageindex.age65plus-nSectors) = hospitalisations(2);
        fatality_rr(data.ageindex.adInd) = deathrates(1);
        fatality_rr(data.ageindex.age65plus-nSectors) = deathrates(2);

        data.rates.ihr = hospitalisation_rr.*orig_ihr./symptomatic_rr;
        data.rates.ifr = fatality_rr.*orig_ifr./symptomatic_rr;


        data.pr.p1 = orig_p1;

        data.pr.phi1 = ones(length(data.NN),1);
        data.pr.phi1(data.ageindex.age20to64) = diagnoses(1);
        data.pr.phi1(data.ageindex.age65plus) = diagnoses(2);

        [f,g,data] = heRunCovid19(data,poptim.R0,mod);

        tout = f.restart_initial_conditions.toutAll;
        new_tout = 1:max(tout);
        infections3 = sum(sum(f.restart_initial_conditions.InewbyageAll'));
        hosp3 = sum(interp1(tout,f.Hout,new_tout));
        deaths3 = f.Dout(end);% - f.Dout(find(abs(tout-365*2)==min(abs(tout-365*2))));
        byage3 = f.Doutbyage(end,:);
        yll3 = sum(data.ylltab.yll'.*byage3);
        workers = f.restart_initial_conditions.workersout;
%         [gdp, gdp_by_year, new_x] = compute_revenue(objFun,xs,tout,workers);
%         gdploss3 = (1-gdp/max_gdp)*100;

        %plotMultiOutd_sr(f,xit,tvec,data,data.pr,breach);

        jf=java.text.DecimalFormat; 
        tosave{i} = sprintf('%s, %s, %s, %s, %s, %s, %s, %s, %s',...
            scenario_labels{i},...
            jf.format(round(-100*(infections1-infections3)/infections1,1)),...
            jf.format(round(-100*(hosp1-hosp3)/hosp1,1)),...
            jf.format(round(-100*(yll1-yll3)./yll1,1)),...
            jf.format(-100*(deaths1-deaths3)/deaths1),...
            jf.format(round(-100*(byage1(1)-byage3(1))./byage1(1),1)),...
            jf.format(round(-100*(byage1(2)-byage3(2))./byage1(2),1)),...
            jf.format(round(-100*(byage1(3)-byage3(3))./byage1(3),1)),...
            jf.format(round(-100*(byage1(4)-byage3(4))./byage1(4),1)));%,...
    %         jf.format(round(100*(gdploss1-gdploss3)/gdploss1,1)))
        if i==1
            writematrix(tosave{i},sprintf('store/results/sample%d.csv',sample),'QuoteStrings',0,'WriteMode','overwrite')
        else
            writematrix(tosave{i},sprintf('store/results/sample%d.csv',sample),'QuoteStrings',0,'WriteMode','append')
        end
    %     round(f.restart_initial_conditions.Dout(find(abs(tout-580)==min(abs(tout-580))),21)./f0.restart_initial_conditions.Dout(find(abs(tout0-580)==min(abs(tout0-580))),21)*100)
    %     figure; plot(tout,f.restart_initial_conditions.Dout(:,21))
    end
close all
end


%% bmi: sympomaticity scenario

% symptomatic_rr = ones(length(data.NN),1);
% symptomatic_rr(data.ageindex.age20to64) = diagnoses(1);
% symptomatic_rr(data.ageindex.age65plus) = diagnoses(2);
% data.pr.p1 = symptomatic_rr.*orig_p1;
% 
% tout = f.restart_initial_conditions.toutAll;
% new_tout = 1:max(tout);
% infections2 = sum(interp1(tout,f.restart_initial_conditions.Iout,new_tout));
% hosp2 = sum(interp1(tout,f.Hout,new_tout));
% deaths2 = f.Dout(end);% - f.Dout(find(abs(tout-365*2)==min(abs(tout-365*2))));
% byage2 = f.Doutbyage(end,:);
% yll2 = sum(data.ylltab.yll'.*byage2);
% workers = f.restart_initial_conditions.workersout;
% [gdp, gdp_by_year, new_x] = compute_revenue(objFun,xs,tout,workers);
% gdploss2 = (1-gdp/max_gdp)*100;
% [f,g,data] = heRunCovid19(data,poptim.R0,mod);
% 
% plotMultiOutd_sr(f,xit,tvec,data,data.pr,breach);
% 
% 
% round([(deaths1-deaths2)/deaths1 (byage1-byage2)./byage1 (yll1-yll2)./yll1...
%     (hosp1-hosp2)/hosp1 (infections1-infections2)/infections1 (gdploss1-gdploss2)/gdploss1;...
%     (deaths1-deaths3)/deaths1 (byage1-byage3)./byage1 (yll1-yll3)./yll1...
%     (hosp1-hosp3)/hosp1 (infections1-infections3)/infections1 (gdploss1-gdploss3)/gdploss1]*100)
% 
% 
% 
