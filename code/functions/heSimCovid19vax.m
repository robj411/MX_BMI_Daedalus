function [f,g,tvec]=heSimCovid19vax(data)
%% PARAMETERS:


NNvec = data.NNvec;
tvec = data.tvec;
Dvec = data.Dvec;
S0 = NNvec(:,1);
XitMat = data.XitMat;
pr = data.pr;
adInd = data.ageindex.adInd;
nGroups = length(pr.ph);
nSectors = nGroups - length(data.ageindex.community);
NNbar = NNvec(:,1);
age20to64 = data.ageindex.age20to64;

nc = pr.nCompartments;


    
lt = length(tvec);
    
%% initial conditions
%%!!
if sum(strcmp(fieldnames(data), 'restart_initial_conditions')) == 0
    zn = zeros(nGroups,1);
    first_time_index = 1;
    t0 = tvec(first_time_index);
    y0 = [S0;repmat(zn,nc-1,1)];
    y0([1:nGroups] + nGroups*(data.pr.compindex.x_index-1)) = 1./nGroups;

    toutAll = t0;
    Sout = S0';
    Iout = sum(pr.seedvec');
    Hout = zn';
    HnewAll = 0;
    HnewbyageAll = zeros(1,length(data.ageindex.community));
    InewbyageAll = 0;
    Dout = zn';
    workersout = S0(1:nSectors)';
    studentsout = S0(data.ageindex.age5to19);
    Rout = 0;
    preomicron = 0;
    behaviour_change = 0;
    consumption_behaviour = 1;
else
    restart_initial_conditions = data.restart_initial_conditions;
    first_time_index = restart_initial_conditions.first_time_index;
    t0 = tvec(first_time_index);

    returnObject = restart_initial_conditions.returnObject;
    % gives us
    % y0 = [S0;repmat(zn,nc-1,1)]; 

    toutAll = restart_initial_conditions.toutAll; 
    Sout = restart_initial_conditions.Sout; 
    Iout = restart_initial_conditions.Iout; 
    Hout = restart_initial_conditions.Hout; 
    Rout = restart_initial_conditions.Rout; 
    HnewAll = restart_initial_conditions.HnewAll; 
    HnewbyageAll = restart_initial_conditions.HnewbyageAll; 
    InewbyageAll = restart_initial_conditions.InewbyageAll; 
    Dout = restart_initial_conditions.Dout; 
    workersout = restart_initial_conditions.workersout; 
    studentsout = restart_initial_conditions.studentsout; 
    preomicron = restart_initial_conditions.preomicron;
    behaviour_change = restart_initial_conditions.behaviour_change;
    consumption_behaviour = restart_initial_conditions.consumption_behaviour;

end

%% iterate over months
%%!! first time point
for i = first_time_index:lt-1
    if i>1
        pasthosp = max(sum(returnObject.Hclass,2));
        Dvec(:,:,i) = data.betamod(i)*heMakeDs(NNvec(:,i),XitMat(:,i-1),data,data.wfh_tab(i,:),pasthosp);
    end

    tend = tvec(i+1);
    NNfeed = NNvec(:,i);
    NNfeed(NNfeed==0) = 1;
    D = Dvec(:,:,i);

%% move positions for closures
    if i>1
        y0 = returnObject.y0new;
        Xh2w = NNvec(1:nSectors,i) - NNvec(1:nSectors,i-1);%Addition to each wp next intervention step

        Xw2h = -Xh2w; 
        Xw2h(Xw2h<0) = 0;
        Xw2h = Xw2h./NNvec(1:nSectors,i-1);
        Xw2h(NNvec(1:nSectors,i-1)==0) = 0;

        if NNvec(nSectors+adInd,i-1)>0%when would this not be the case?
            Xh2w(Xh2w<0) = 0;
            Xh2w = Xh2w/NNvec(nSectors+adInd,i-1);
        else
            Xh2w = 0;
        end

        %Move all infection statuses:

        y0 = reshape(y0,[nGroups,nc]);%IC
        y0w2h = y0(1:nSectors,:).*repmat(Xw2h,1,nc);%IC%number of people to be put at home (+)
        y0w2h = [-y0w2h;sum(y0w2h,1)];

        y0h2w = y0(nSectors+adInd,:);
        y0h2w = kron(y0h2w,Xh2w);
        y0h2w = [y0h2w;-sum(y0h2w,1)];
        y0(age20to64,:) = y0(age20to64,:)+y0w2h+y0h2w;

        y0 = reshape(y0,nGroups*nc,1);

    end

%% solve
%         [tout,Sclass,Hclass,Dclass,DEcum,Itot,y0,Hnew,Hnewbyage,Vclass,SVclass,sol,notabsent,Rcum]=integr8(pr,beta,nGroups,NNfeed,D,phi1,seedvec,t0,tend,y0);

    returnObject = integr8(data,nGroups,NNfeed,D,t0,tend,y0,preomicron,behaviour_change);

%% save
    toutAll = [toutAll; returnObject.tout(2:end)];
    Sout = [Sout; returnObject.Sclass(2:end,:)];
    Iout = [Iout; returnObject.Itot(2:end)];
    Hout = [Hout; returnObject.Hclass(2:end,:)];
    Rout = [Rout; returnObject.Rcum(2:end)];
    HnewAll = [HnewAll; returnObject.Hnew(2:end)];
    hnby = returnObject.Hnew_by_age(:,data.ageindex.community);
    hnby(:,adInd) = sum(returnObject.Hnew_by_age(:,data.ageindex.age20to64)');
    HnewbyageAll = [HnewbyageAll; hnby(2:end,:)];
    inby = returnObject.Inew_by_age(data.ageindex.community,:);
    inby(adInd,:) = sum(returnObject.Inew_by_age(data.ageindex.age20to64,:));
    InewbyageAll = InewbyageAll + inby;
    Dout = [Dout; returnObject.Dclass(2:end,:)];
    workersout = [workersout; returnObject.notabsent(2:end,1:nSectors)];
    studentsout = [studentsout; returnObject.notabsent(2:end,data.ageindex.age5to19)];
    if tend < (pr.Omicron.Omicron_start+7)
        time_index = find(returnObject.tout<(pr.Omicron.Omicron_start+7),1,'last');
        preomicron = returnObject.all_acquired(time_index);
    end
    behaviour_change = returnObject.behaviour_change;
    consumption_behaviour = [consumption_behaviour; returnObject.consumption_behaviour(2:end)];

    t0 = tend;
%     if i<lt-1
%         y0 = returnObject.y0new;
%         Xh2w = NNvec(1:nSectors,i+1) - NNvec(1:nSectors,i);%Addition to each wp next intervention step
% 
%         Xw2h = -Xh2w; 
%         Xw2h(Xw2h<0) = 0;
%         Xw2h = Xw2h./NNvec(1:nSectors,i);
%         Xw2h(NNvec(1:nSectors,i)==0) = 0;
% 
%         if NNvec(nSectors+adInd,i)>0%when would this not be the case?
%             Xh2w(Xh2w<0) = 0;
%             Xh2w = Xh2w/NNvec(nSectors+adInd,i);
%         else
%             Xh2w = 0;
%         end
% 
%         %Move all infection statuses:
% 
%         y0 = reshape(y0,[nGroups,nc]);%IC
%         y0w2h = y0(1:nSectors,:).*repmat(Xw2h,1,nc);%IC%number of people to be put at home (+)
%         y0w2h = [-y0w2h;sum(y0w2h,1)];
% 
%         y0h2w = y0(nSectors+adInd,:);
%         y0h2w = kron(y0h2w,Xh2w);
%         y0h2w = [y0h2w;-sum(y0h2w,1)];
%         y0(age20to64,:) = y0(age20to64,:)+y0w2h+y0h2w;
% 
%         y0 = reshape(y0,nGroups*nc,1);
% 
%     end
end


%% OUTPUTS:  

restart_initial_conditions = struct;
restart_initial_conditions.first_time_index = lt;
restart_initial_conditions.returnObject = returnObject;
restart_initial_conditions.toutAll = toutAll; 
restart_initial_conditions.Sout = Sout; 
restart_initial_conditions.Iout = Iout; 
restart_initial_conditions.Hout = Hout; 
restart_initial_conditions.Rout = Rout; 
restart_initial_conditions.HnewAll = HnewAll; 
restart_initial_conditions.HnewbyageAll = HnewbyageAll; 
restart_initial_conditions.InewbyageAll = InewbyageAll; 
restart_initial_conditions.Dout = Dout; 
restart_initial_conditions.workersout = workersout; 
restart_initial_conditions.studentsout = studentsout; 
restart_initial_conditions.preomicron = preomicron;
restart_initial_conditions.consumption_behaviour = consumption_behaviour;

%%RJ
Doutbyage = Dout(:,data.ageindex.community);
Doutbyage(:,adInd) = Doutbyage(:,adInd) + sum(Dout(:,1:nSectors),2);
f = struct;
f.Sout = sum(Sout,2);
f.Hout =    sum(Hout,2);
f.Dout =    sum(Dout,2);
f.Doutbyage =    Doutbyage;
% f.SVout =    SVout;

f.restart_initial_conditions = restart_initial_conditions;

g = max(sum(Hout(toutAll>=tvec(1),:),2));   

end

%%

function returnObject=integr8(data,nGroups,NN0,D,t0,tend,y0,preomicron,behaviour_change)
%% CALL:

pr = data.pr;
seed = pr.seedvec./NN0;
compindex = pr.compindex;
vx = pr.vx;
Delta = pr.Delta;
Omicron = pr.Omicron;
pr = rmfield(pr,'compindex');
pr = rmfield(pr,'vx');
pr = rmfield(pr,'Delta');
pr = rmfield(pr,'Omicron');

start_times = [75 350 545 710];%[197 385 596 755];
time_index = find(start_times>t0,1,'first');
mandate = [1 .5 0 .5];
next_change_time = start_times(time_index);
next_mandate = mandate(time_index);
if behaviour_change==1
    next_mandate = mandate(time_index-1);
end
% disp([t0 next_change_time behaviour_change]);

fun=@(t,y)integr8covid(t,y,pr,compindex,vx,Delta,Omicron,nGroups,NN0,D,seed,preomicron,next_change_time,behaviour_change,next_mandate);
options = odeset('RelTol',1e-6,'AbsTol',1e-6);
[tout,yout]=ode23(fun,[t0 tend],y0);

returnObject = struct;
returnObject.tout = tout;
returnObject.Hnew = zeros(length(tout),1);
returnObject.Hnew_by_age = zeros(length(tout),nGroups);
if data.fit_routine==1
    for i = 1:length(tout)
        [~,Hnewi,Hnewi_by_age] = fun(tout(i),yout(i,:)');
        returnObject.Hnew(i) = Hnewi;
        returnObject.Hnew_by_age(i,:) = Hnewi_by_age;
    end
end

nStates = 4;
inew_by_age_t = zeros(length(tout),nGroups,nStates);
for i = 1:length(tout)
    [~,~,~,Inewi_by_age,behaviour_change] = fun(tout(i),yout(i,:)');
    inew_by_age_t(i,:,:) = Inewi_by_age;
end
days = t0:(tend-1);
interp_cases = zeros(nGroups,nStates);
for i = 1:nGroups
    for j = 1:nStates
        interp_cases(i,j) = sum(interp1(tout,inew_by_age_t(:,i,j),days));
    end
end
returnObject.Inew_by_age = interp_cases;
returnObject.behaviour_change = behaviour_change;

%% EC:
y_mat_out = reshape(yout,length(tout),nGroups,pr.nCompartments); 

returnObject.Sclass=     y_mat_out(:,:,compindex.S_index(1)); 
returnObject.SVclass=     y_mat_out(:,:,compindex.S_index(3)); 
% Sv1class=   sum(y_mat_out(:,:,compindex.S_index(2:3)),3); 
returnObject.Itot=   sum(sum(y_mat_out(:,:,compindex.I_index),3),2); 
returnObject.Hclass=     sum(y_mat_out(:,:,compindex.H_index),3); 
returnObject.Rcum=   sum(sum(y_mat_out(:,:,compindex.R_index),3),2); 
returnObject.Dclass=     y_mat_out(:,:,compindex.D_index); 
returnObject.DEcum=      y_mat_out(end,:,compindex.D_index); 
%%!!
returnObject.Vclass=     y_mat_out(:,:,compindex.V_index(1:2)); 
returnObject.y0new=      yout(end,:)';
returnObject.notabsent = sum(y_mat_out(:,:,[compindex.S_index compindex.E_index compindex.R_index compindex.I_index([1,3])]),3);
returnObject.all_acquired = sum(y_mat_out(:,:,compindex.A_index),2);
returnObject.consumption_behaviour = sum(y_mat_out(:,:,compindex.x_index),2);
end

%%

function [f,hosp_admissions,hosp_admissions_by_age,new_infections,behaviour_change]=...
    integr8covid(t,y,pr,compindex,vx,Delta,Omicron,nGroups,NN0,D,seed,preomicron,next_change_time,behaviour_change,next_mandate)

% names = fieldnames(pr);
% for i = 1:numel(names)
%     assignin('base', names{i}, pr.(names{i}));
% end

%% state variables

y_mat = reshape(y,nGroups,pr.nCompartments); 
% compindex = pr.compindex;
S_index = compindex.S_index;
E_index = compindex.E_index;
I_index = compindex.I_index;
H_index = compindex.H_index;
R_index = compindex.R_index;
V_index = compindex.V_index;
A_index = compindex.A_index;
x_index = compindex.x_index;

S=      y_mat(:,S_index(1)); 
E=      y_mat(:,E_index(1)); 
Ia=    y_mat(:,I_index(1)); 
Is=    y_mat(:,I_index(2)); 
H=      y_mat(:,H_index(1)); 
R=      y_mat(:,R_index(1)); 

Shv1=   y_mat(:,S_index(2)); 
Sv1=    y_mat(:,S_index(3)); 
Ev1=    y_mat(:,E_index(2)); 
Iav1=  y_mat(:,I_index(3)); 
Isv1=  y_mat(:,I_index(4));
Hv1=    y_mat(:,H_index(2)); 
Rv1=    y_mat(:,R_index(2)); 

% Sv2=    y_mat(:,S_index(4)); 
% Ev2=    y_mat(:,E_index(3));
% Iav2=  y_mat(:,I_index(5)); 
% Isv2=  y_mat(:,I_index(6)); 
% Hv2=    y_mat(:,H_index(3)); 
% Rv2=    y_mat(:,R_index(3)); 
% 
% DE=     y_mat(:,pr.D_index); 
V_1 =      y_mat(:,V_index(1)); 
V_2 =      y_mat(:,V_index(2));
% Vb_1 =      y_mat(:,V_index(3)); 
% Vb_2 =      y_mat(:,V_index(4));
%%!!
A_1 =      y_mat(:,A_index(1)); 

Sv1_1 = V_1;
Sv1_2 = Sv1 - Sv1_1;
current_vaccine_pp = V_2./Sv1_2;
current_vaccine_pp(Sv1_2 < 1e-10) = 1;
% Svb1_1 = Vb_1;
% Svb1_2 = Sv2 - Svb1_1;
% current_booster_pp = Vb_2./Svb1_2;
% current_booster_pp(Svb1_2 < 1e-10) = 1;

%% parameters:

p1 = pr.p1;
g1 = pr.g1;
ph = pr.ph;
Threc = pr.Threc;
Thd = pr.Thd;
Ts = pr.Ts;
Tsh = pr.Tsh;
pd = pr.pd;
nu = pr.nu;
odds = pr.odds;
sigma = pr.sigma;
% Hmin = pr.Hmin;
% Hmax = pr.Hmax;
% surge_rate = pr.surge_rate;
asymp_reduction = pr.red;
beta = pr.beta;

% vx = pr.vx;
map = vx.map;
seroconversion_rate0 = vx.hrv1;
waning0 = vx.nuv1;
pr_asymptomatic0 = vx.p1v1;
pr_transmission0 = vx.trv1;
pr_hospital0 = vx.hv1;
pr_death0 = vx.dv1;
% waning0b = vx.nuv2;
% pr_asymptomatic0b = vx.p1v2;
% pr_transmission0b = vx.trv2;
% pr_hospital0b = vx.hv2;
% pr_death0b = vx.dv2;
maturation_rate = vx.maturation_rate;
% maturation_rate2 = vx.maturation_rate2;
vaccination_policy_change_times = vx.vaccination_policy_change_times;
% boosters_per_day = vx.boosters_per_day;
waned_infected = vx.waned_infected;
% waned_infected_booster = vx.waned_infected_booster;


%% time

day_index = max(1,round(t));
month_index = find(t<=vaccination_policy_change_times,1);


p3 = pr.p3;
p4 = pr.p4;
if t <= 80
    p3 = 0;
    p4 = 0;
end
Ina = (1-p3).*Ia;
Ins = (1-p4).*Is;
Inav1 = (1-p3).*Iav1;
Insv1 = (1-p4).*Isv1;
% Inav2 = (1-p3).*Iav2;
% Insv2 = (1-p4).*Isv2;

%% time-dependent parameters

Delta_start = Delta.Delta_start;
resistance0 = vx.scv1;
if t > Delta_start
    antigenic_drift = (max(0,day_index-611))/365 * vx.antigenic_drift;
    faded_effect = 1 - antigenic_drift;
    resistance0 = resistance0 * faded_effect;
    % resistance0b = vx.scv2 * faded_effect;
    
%     Delta = pr.Delta;
    Delta_hfr = Delta.Delta_hfr;
    Delta_hr = Delta.Delta_hr;
    Delta_interp = min(max((t-Delta_start)./(Delta.Delta_end-Delta_start),0),1);
    beta = (1+Delta.Delta_rate.*Delta_interp).*beta;
    ph = min((1+Delta_hr.*Delta_interp).*ph, 0.9);
    pd = min((1+Delta_hfr.*Delta_interp).*pd, 0.9);
    
    Omicron_start = Omicron.Omicron_start;
    if t>Omicron_start
%         Omicron = pr.Omicron;
        Omicron_end = Omicron.Omicron_end;
        Omicron_interp = min(max((t-Omicron_start)./(Omicron_end-Omicron_start),0),1);
        % needs to match process_inputs.R
        if t>(Omicron_start+7)
            Omicron_reinfection_hfr = Omicron.Omicron_reinfection_hfr;
            Omicron_reinfection_hospital = Omicron.Omicron_reinfection_hospital;
            Omicron_reinfection = Omicron.Omicron_reinfection;
            all_susc = sum(S+Shv1+Sv1);
            recent_acquired = sum(A_1);
            frac_omicron_infected = Omicron_reinfection /(1+Omicron_reinfection );
            omicron_reinfected = recent_acquired * frac_omicron_infected;
            preomicron_left = preomicron - omicron_reinfected;
            preomicron_frac = preomicron_left/all_susc;
            hosp_frac = preomicron_frac*Omicron_reinfection./((1-preomicron_frac) + preomicron_frac*Omicron_reinfection);
            death_frac = hosp_frac*Omicron_reinfection_hospital./((1-hosp_frac) + hosp_frac*Omicron_reinfection_hospital);
            % decreased rates for prior exposure
            beta = (1-preomicron_frac)*beta + Omicron_reinfection*preomicron_frac.*beta;
            ph = ((1-hosp_frac) + Omicron_reinfection_hospital*hosp_frac).*ph;
            pd = (1-death_frac)*pd + Omicron_reinfection_hfr*death_frac.*pd;
        elseif t<(Omicron_start+7)
            nu = Omicron.Omicron_immune_escape;
        end

        % update variant-dependent parameters
        % vaccine escape
        resistance0 = resistance0.*(1-(1-Omicron.Omicron_ve).*Omicron_interp);
        pr_hospital0 = pr_hospital0.*(1-(1-Omicron.Omicron_ve_hospitalisation).*Omicron_interp);
        pr_death0 = pr_death0.*(1-(1-Omicron.Omicron_ve_death).*Omicron_interp);
        % decreased rates for omicron
        ph = (1-(1-Omicron.Omicron_hr).*Omicron_interp).*ph;
        pd = (1-(1-Omicron.Omicron_hfr).*Omicron_interp).*pd;
    end

end

%% vaccine delay time and product proportions
k_mature = maturation_rate;
% k_mature2 = maturation_rate2;

% for administration
SplusR = S+R+1e-16;
Sfrac = S./SplusR;
Rfrac = R./SplusR;

%% VACCINATION:
waned_by_group = waned_infected(day_index,map+1)'; 
% waned_by_group_booster = waned_infected_booster(day_index,map+1)'; 
% waned_by_group_past = waned_infected(max(1,day_index-21),map+1)'; 
% waned_by_group_past_booster = waned_infected_booster(max(1,day_index-21),map+1)'; 

%% New vaccines

proportions = vx.proportions(month_index,map); % allocation to groups as a function of time
% distribute equally among each group
for i = 1:max(map)
    SRi = SplusR(map==i);
    proportions(map==i) = proportions(map==i).* SRi'./sum(SRi);
end
vaccines_that_day = min(vx.vaccines_per_day(month_index), sum(SplusR));
vaccinated_by_group = min(vaccines_that_day .* proportions', SplusR); 

% vaccines divided proportionally among recovered and susceptible
v1rates = vaccinated_by_group.*Sfrac;
v1rater = vaccinated_by_group.*Rfrac;

%% Booster vaccines
% SVplusRV = Sv1 + Rv1 + 1e-16;
% SVfrac = Sv1./SVplusRV;
% RVfrac = Rv1./SVplusRV;
% booster_proportions = vx.booster_proportions(month_index,map); % allocation to groups as a function of time
% % distribute equally among each group
% for i = 1:max(map)
%     SRi = SVplusRV(map==i);
%     booster_proportions(map==i) = booster_proportions(map==i).* SRi'./sum(SRi);
% end
% vaccines_that_day = min(boosters_per_day(month_index), sum(SVplusRV));
% vaccinated_by_group = min(vaccines_that_day .* booster_proportions', SVplusRV); 
% 
% % vaccines divided proportionally among recovered and susceptible
% v2rates = vaccinated_by_group.*SVfrac;
% v2rater = vaccinated_by_group.*RVfrac;


%% change rates based on waned vaccine

seroconversion_rate = repmat(seroconversion_rate0,nGroups,1);
waning = repmat(waning0,nGroups,1);
resistance_1 = repmat(resistance0,nGroups,1);
resistance = min(1,resistance0.*current_vaccine_pp);
pr_asymptomatic = min(1,pr_asymptomatic0.*current_vaccine_pp);
pr_transmission = min(1,pr_transmission0.*current_vaccine_pp);
pr_hospital = min(1,pr_hospital0.*current_vaccine_pp);
pr_death = min(1,pr_death0.*current_vaccine_pp);

% seroconversion_rate2 = repmat(seroconversion_rate0,nGroups,1);
%%!!
% waning = repmat(waning0,nGroups,1);
% waning2 = repmat(waning0b,nGroups,1);
% resistance_1b = repmat(resistance0b,nGroups,1);
% resistance2 = min(1,resistance0b.*current_booster_pp);
% pr_asymptomatic2 = min(1,pr_asymptomatic0b.*past_booster_pp);
% pr_transmission2 = min(1,pr_transmission0b.*past_booster_pp);
% pr_hospital2 = min(1,pr_hospital0b.*past_booster_pp);
% pr_death2 = min(1,pr_death0b.*past_booster_pp);

%% updated hospitalisation and death rates

% initial CFR
expected_hospital_duration = (1-pd).*Threc + pd.*Thd;
hospital_CFR = pd./expected_hospital_duration;
hosp_recover = (1-pd)./expected_hospital_duration;

v_pd = (1-pr_death).*pd;
v_expected_hospital_duration = (1-v_pd).*Threc + v_pd.*Thd;
v_hospital_CFR = v_pd./v_expected_hospital_duration;
v_hosp_recover = (1-v_pd)./v_expected_hospital_duration;

% v2_pd = (1-pr_death2).*pd;
% v2_expected_hospital_duration = (1-v2_pd).*Threc + v2_pd.*Thd;
% v2_hospital_CFR = v2_pd./v2_expected_hospital_duration;
% v2_hosp_recover = (1-v2_pd)./v2_expected_hospital_duration;

% update based on past hospital occupancy
% Hm21 = Z_mat(:,H_index(1));
% HVm21 = Z_mat(:,H_index(2));
% past_occupancy = sum(H) + sum(Hv1);
% if past_occupancy > Hmin
%     interp = max((past_occupancy - Hmin)./(Hmax-Hmin),0);
%     CFR_factor = 1 + surge_rate.*interp;
% %     hospital_CFR = CFR_factor.*hospital_CFR;
% %     v_hospital_CFR = CFR_factor.*v_hospital_CFR;
% %     v2_hospital_CFR = CFR_factor.*v2_hospital_CFR;
% end

expected_infectious_duration = (1-ph).*Ts + ph.*Tsh;
g2 = (1-ph)./expected_infectious_duration;
h = ph./expected_infectious_duration;

v_ph = (1-pr_hospital).*ph;
v_expected_infectious_duration = (1-v_ph).*Ts + v_ph.*Tsh;
I_recover_rate =   (1-v_ph)./v_expected_infectious_duration;
hospital_rate = v_ph./v_expected_infectious_duration;

% v2_ph = (1-pr_hospital2).*ph;
% v2_expected_infectious_duration = (1-v2_ph).*Ts + v2_ph.*Tsh;
% I_recover_rate2 =   (1-v2_ph)./v2_expected_infectious_duration;
% hospital_rate2 = v2_ph./v2_expected_infectious_duration;

%% hospital rate of change

k6  = h.*Is;
k6v = hospital_rate.*Isv1;
% k6v2 = hospital_rate2.*Isv2;
k7  = hosp_recover.*H;
k7v = v_hosp_recover.*Hv1;
% k7v2 = v2_hosp_recover.*Hv2;
k8  = hospital_CFR.*H;
k8v = v_hospital_CFR.*Hv1;
% k8v2 = v2_hospital_CFR.*Hv2;

Hdot=    k6   -k7 - k8 ;
Hv1dot=  k6v  -k7v - k8v;

if t>next_change_time
    behaviour_change = 1;
%     disp([t behaviour_change sum(Hdot+Hv1dot)]);
end
if behaviour_change==1 & sum(Hdot+Hv1dot)<0
    behaviour_change = sum(Hdot+Hv1dot)>0 ;
end
consumption = y_mat(1,x_index(1));
if t<75
    xdot = zeros(nGroups,1);
elseif behaviour_change==1
    xdot = - (Hdot+Hv1dot)./1500./nGroups*next_mandate;
else
    xdot = ones(nGroups,1)./450./nGroups;
end

%% foi and rates

I=(asymp_reduction*Ina+Ins)  + (1-pr_transmission).*(asymp_reduction*Inav1+Insv1); % + ...
%      + (1-pr_transmission2).*(asymp_reduction*Inav2+Insv2);%Only non-self-isolating compartments

foi = beta*(D*(I./NN0));

k1  = pr.phi1 .* S.*(foi+seed);
k1v_1 = Sv1_1.*(1-resistance_1).*foi;
k1v_2 = Sv1_2.*(1-resistance).*foi;
k1v = k1v_1 + k1v_2;
% k1vb_1 = Svb1_1.*(1-resistance_1b).*foi;
% k1vb_2 = Svb1_2.*(1-resistance2).*foi;
% k1v2 = k1vb_1 + k1vb_2;
k12 = Shv1.*foi;

%% all other rates

Eflux = sigma.*E;
Evflux = sigma.*Ev1;
% Ev2flux = sigma.*Ev2;
k2  = (1-p1)  .*Eflux;
k2v = (1-p1.*(1-pr_asymptomatic)).*Evflux;
% k2v2 = (1-p1.*(1-pr_asymptomatic2)).*Ev2flux;
k3  = g1*(1+odds) .*Ia;
k3v = g1*(1+odds) .*Iav1;
% k3v2 = g1*(1+odds) .*Iav2;
k4  = p1      .*Eflux;
k4v = (1-pr_asymptomatic).*p1    .*Evflux;
% k4v2 = (1-pr_asymptomatic2).*p1    .*Ev2flux;
k5  = g2.*Is;
k5v = I_recover_rate.*Isv1;
% k5v2 = I_recover_rate2.*Isv2;
k10 = seroconversion_rate.*Shv1;
k11 = waning.*V_2;
% k11b = waning2.*Vb_2;
k13 = nu.*R ;
k13v = nu.*Rv1 ;

Sdot=  - k1 + k13 -v1rates;
Edot=    k1  +k12  -Eflux;
Iadot=   k2      -k3;
Isdot=   k4      -k5 - k6;
Rdot=    k3+k5  + k7     -k13   -v1rater;

Shv1dot= v1rates     -k10   -k12;
Sv1dot=               k10   - k1v  + k13v ; %- v2rates
Ev1dot=                              k1v  -Evflux;
Iav1dot= k2v       -k3v;
Isv1dot= k4v       -k5v - k6v;
Rv1dot=  k3v   +k5v+ k7v +v1rater - k13v ; %- v2rater

Adot = k3v + k5v + k7v + k3 + k5 + k7;

% Sv2dot=               v2rates   - k1v2  ;  
% Ev2dot=                              k1v2  -Ev2flux;
% Iav2dot= k2v2       -k3v2;
% Isv2dot= k4v2       -k5v2 - k6v2;
% Hv2dot=  k6v2  - k7v2 - k8v2;
% Rv2dot=  k3v2   + k5v2 + k7v2 + v2rater ;

DEdot=   k8 + k8v;   %   + k8v2

% v2rates_waned = min(v2rates,waned_by_group.*V_2);
% v2rates_unwaned = v2rates - v2rates_waned;
V1dot = k10 - k1v_1 - k_mature.*V_1; % - v2rates_unwaned
V2dot = k_mature.*V_1  - k11 + current_vaccine_pp.* (k13v - waned_by_group.*k1v_2 ); % - v2rates_waned
% Vb1dot = v2rates - k1vb_1 - k_mature2.*Vb_1; 
% Vb2dot = k_mature2.*Vb_1  - k11b + current_booster_pp.* ( - waned_by_group_booster.*k1vb_2 ); 

%% return

f= [Sdot;Edot;...
    Iadot;Isdot;...%Insdot;Issdot;...
    Hdot;Rdot;...
    Shv1dot;Sv1dot;Ev1dot;...
    Iav1dot;Isv1dot;...%Insv1dot;Issv1dot;...
    Hv1dot;Rv1dot;...
    DEdot;V1dot;V2dot;...
    Adot;xdot...
%     Sv2dot;Ev2dot;...
%     Iav2dot;Isv2dot;...%Insv1dot;Issv1dot;...
%     Hv2dot;Rv2dot;Vb1dot;Vb2dot
    ];
hosp_admissions = sum(k6v + k6);
hosp_admissions_by_age = k6v + k6;

new_infections = [k2 k2v k4 k4v];

end


%%
