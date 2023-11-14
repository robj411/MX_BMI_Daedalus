function [data] = hePrepCovid19(data,R0,modifier)

%data.NN - column vector of population
%Possible generalsiation to within-sector heterogeneity - one column per subsector


pr = data.pr;
numInt = length(modifier);
%% contact rates

% data.CM = scale_community_matrix(data.CM, data.community_scalar);

%% POPULATION PARAMETERS:

%Population Density
[n,na] = size(data.NN);
nGroups = n*na;
NNbar=reshape(data.NN,nGroups,1);
data.nGroups = nGroups;
data.n = n;
data.na = na;

%% contact matrices
%Age-Sector Breakdown
ageindex = data.ageindex;
lt = length(data.tvec);
lc = length(ageindex.community);
lx = length(data.NN)-lc;
D = heMakeDs(data.NN,ones(lx,1),data,zeros(1,lx),0);
Dout = D;
data.Dout = Dout;
Dvec = repmat(D,[1,1,lt-1]);
data.Dvec = Dvec;

%% opening
Xit = data.xit;
XitMat = reshape(Xit,lx,lt-2);
WitMat = XitMat.^(1./data.alpha);
WitMat(data.edSector,:) = XitMat(data.edSector,:);
NNvec = repmat(NNbar(1:lx),1,lt-2).*WitMat;%Assumes pre-lockdown=fully open
NNworkSum = sum(NNvec,1);
NNvec(ageindex.community,1:lt-2) = repmat(NNbar(ageindex.community),1,lt-2);
NNvec(lx+ageindex.adInd,:) = sum(NNbar(ageindex.age20to64))-NNworkSum;
NNvec = [NNbar,NNvec];
data.NNbar = NNbar;
data.NNvec = NNvec;
data.XitMat = XitMat;


%% compartments

compindex = struct;

compindex.S_index = [1,7,8];
compindex.E_index = [2,9];
compindex.I_index = [3,4,10,11];
compindex.H_index = [5,12];
compindex.R_index = [6,13];
compindex.D_index = [14];
compindex.V_index = [15,16];
compindex.A_index = 17;
compindex.x_index = 18;
pr.compindex = compindex;
pr.nCompartments = max(struct2array(compindex));
%%!! parameter for number of zeros
data.wfh_tab = [zeros(3,length(data.IO));repmat(data.wfhAv,numInt-2,1)];


%% DISEASE PARAMETERS:

%from Knock et al. (2020) unless indicated otherwise

%Latency and Onset
pr.sigma=1/pr.Text;

%Case Pathways
ihr =    data.rates.ihr;
ph =   ihr; 
ifr=    data.rates.ifr;
pd =   ifr./ihr;
% [ph,pd,~,~] = heParamsAge(data);
pr.ph = [repmat(ph(data.ageindex.adInd),lx,1);ph];
pd = [repmat(pd(data.ageindex.adInd),lx,1);pd];
pr.pd = pd;

expected_infectious_duration = (1-pr.ph).*pr.Ts + pr.ph.*pr.Tsh;

%%

Deff = Dout.*repmat(NNbar,1,n*na)./repmat(NNbar',n*na,1);
onesn = ones(nGroups,1);

F = zeros(3*nGroups,3*nGroups);
%F=zeros(4*nGroups,4*nGroups);
F(1:nGroups,nGroups+1:end) = [pr.red*Deff,Deff];
%F(1:nGroups,nGroups+1:end)=[pr.red*Deff,repmat(Deff,1,2)];

vvec = [pr.sigma.*onesn;      pr.g1.*onesn;       1./expected_infectious_duration.*onesn];%g2 and h are vectors
V = diag(vvec);
V(nGroups+1:2*nGroups,1:nGroups)=    diag(-(1-pr.p1) .*pr.sigma  .*onesn);
V(2*nGroups+1:3*nGroups,1:nGroups)=  diag(-pr.p1     .*pr.sigma  .*onesn);
%V(2*nGroups+1:3*nGroups,1:nGroups)=  diag(-pr.p1     .*(1-pr.p2) .*pr.sigma  .*onesn);
%V(3*nGroups+1:4*nGroups,1:nGroups)=  diag(-pr.p1     .*pr.p2     .*pr.sigma  .*onesn);

GD = F/V;

d = eigs(GD,1);%largest in magnitude (+/-) 
R0a = max(d); 

if ~strcmp(fieldnames(pr),'beta')
    pr.beta = R0/R0a;%beta scales actual R0 to fitted R0
end

% fun= @(s) s-exp((beta/pr.g2(1))*D*(s-1));
% sinf=fsolve(fun,0.3*ones(size(NN)));
% s=100000*sum(sinf.*NN)/sum(data.NN);

%% PREPAREDNESS PARAMETERS:

%see country parameter file

%Adherence to NPIs
data.betamod=[1,modifier];

% pdoc=heParamsAge_oc(data,pr.p1);
% pdoc=[repmat(pdoc(data.ageindex.adInd),lx,1);pdoc];

%% VACCINATION PARAMETERS:

vx=pr.vx;%from RTM unless indicated otherwise

%Vaccine 1 (two doses)
% vx.hrv1=    1/14;                       %time to develop v-acquired immunity (AstraZeneca)
% vx.scv1=    0.3;                       %infection-blocking efficacy
% vx.p1v1=    0.3;                          %disease-blocking efficacy          
% vx.hv1=     0.5;   %severe-disease-blocking efficacy
% vx.dv1=     0.5;                          %death-blocking efficacy
% vx.trv1=    0.5;                       %transmission-blocking efficacy
% vx.nuv1=    1/150;                      %duration of v-acquired immunity

% %Vaccine 1 (one dose)
% vx.hrv1=    1/21;                       %time to develop v-acquired immunity (AstraZeneca)
% vx.scv1=    0.33;                       %infection-blocking efficacy
% vx.p1v1=    0;                          %disease-blocking efficacy          
% vx.hv1=     1-((1-0.75)/(1-vx.scv1));   %severe-disease-blocking efficacy
% vx.dv1=     0;                          %death-blocking efficacy
% vx.trv1=    0.40;                       %transmission-blocking efficacy
% vx.nuv1=    1/Inf;                      %duration of v-acquired immunity

% vx.mu_ocv1= 1.19*vx.mu_v1;%Wilde et al. (2021)
% vx.g3_ocv1= (1-1.19*(1-vx.dv1)*pd)/Threc;

% vx.hrv2=    1/14; 
% vx.scv2=    0.9;                       %infection-blocking efficacy
% vx.p1v2 = 0.5; % disease
% vx.hv2 = 0.5; % hospitalisation
% vx.dv2 = 0.5; % death
% vx.trv2 = 0.5; % transmission
% vx.nuv2 = 1/180; % 1/duration

% %Vaccine 2
% vx.hrv2=    1/21;                       %time to develop v-acquired immunity (AstraZeneca)
% vx.scv2=    0.65;                       %infection-blocking efficacy
% vx.p1v2=    0;                          %disease-blocking efficacy          
% vx.hv2=     1-((1-0.80)/(1-vx.scv1));   %severe-disease-blocking efficacy
% vx.dv2=     0;                          %death-blocking efficacy
% vx.trv2=    0;                          %transmission-blocking efficacy
% vx.nuv2=    1/Inf;                      %duration of v-acquired immunity

%%RJ
% vx.vaccination_policy_change_times = [vx.atimes; Inf];
% vx.vaccines_per_day = [0; vx.arates];
% % vx.boosters_per_day = [0; vx.a2rates];
% vx.proportions = [zeros(1,size(vx.vaccination_proportions,2)); vx.vaccination_proportions];
% vx.booster_proportions = [zeros(1,size(vx.booster_proportions,2)); vx.booster_proportions];
% vx = rmfield(vx,'atimes');
% vx = rmfield(vx,'arates');
% % vx = rmfield(vx,'a2rates');
% vx = rmfield(vx,'vaccination_proportions');

pr.vx = vx;

% pr.sw=0;%switching off

%% seeding

numseed = 7;

if ~strcmp(fieldnames(pr),'phi1')
    pr.phi1 = 1; 
end

seed = 10^-(numseed);
pr.seedvec = seed*ones(nGroups,1); 

data.pr = pr;

end

%%

function [phgs,pdgh,Threc,Thd]=heParamsAge(datax)
%%

%nn=[3463,3726,3538,3260,3693,4022,4011,3926,3586,3919,4129,3890,3308,2982,2960,2069,1531+933+414+117+12];%England and Wales, mid-2019 estimate (ONS)
nn=datax.Npop';
nn=[nn(1:16),sum(nn(17:end))];%last age range in Knock et al. (2020) is 80+

ranges=[1,3,9,4];
nntot=[nn(1),sum(nn(2:4)),sum(nn(5:13)),sum(nn(14:end))];
nntot=repelem(nntot,ranges);
nnprop=nn./nntot;

subs=1:4;
subs=repelem(subs,ranges);

%%

ihr=    datax.rates.ihr;%[0.030000 0.002600 0.000840	0.000420 0.000800 0.002600 0.004000	0.006300 0.012000 0.019000 0.023000	0.040000 0.096000 0.100000 0.240000	0.500000 0.500000];
phgs=   ihr; %./datax.pr.ps;
% phgs=   accumarray(subs',phgs'.*nnprop);

ifr=    datax.rates.ifr;%[0.000310 0.000030 0.000010 0.000000 0.000010 0.000040 0.000060 0.000130 0.000310 0.000700 0.001160 0.002760 0.008670 0.012150 0.035120 0.084300 0.096960];
pdgh=   ifr./ihr;

% nnh = nn.*ihr';
% nnhtot=[nnh(1),sum(nnh(2:4)),sum(nnh(5:13)),sum(nnh(14:end))];
% nnhtot=repelem(nnhtot,ranges);
% nnhprop=nnh./nnhtot;
% pdgh=   accumarray(subs',pdgh'.*nnhprop);

%%

% Tgr=    10.7;
% 
% Ttr=    2.5; 
% Ticur=  15.6; 
% Tsdr=   12.2;
% 
% Tgd=    10.3;
% 
% %Ttr=    2.5; 
% Ticud=  11.8;
% 
% %Ttr=    2.5; 
% Ticusd= 7; 
% Tsdd=   8.1;
% 
% Threc=  Tgr*(pg*nn'/sum(nn))+(Ttr+Ticur+Tsdr)*(picu*nn'/sum(nn));
% Thd=    Tgd*(pg*nn'/sum(nn))+(Ttr+Ticud)*((picu.*pdicu)*nn'/sum(nn))+(Ttr+Ticusd+Tsdd)*((picu.*psd)*nn'/sum(nn));

Threc=  NaN;
Thd=    NaN;

end

function pdgh=heParamsAge_oc(datax,ps)
%%

%nn=[3463,3726,3538,3260,3693,4022,4011,3926,3586,3919,4129,3890,3308,2982,2960,2069,1531+933+414+117+12];%England and Wales, mid-2019 estimate (ONS)
nn=datax.Npop';
nn=[nn(1:16),sum(nn(17:end))];%last age range in Knock et al. (2020) is 80+

ranges=[1,3,9,4];
nntot=[nn(1),sum(nn(2:4)),sum(nn(5:13)),sum(nn(14:end))];
nntot=repelem(nntot,ranges);
nnprop=nn./nntot;

subs=1:4;
subs=repelem(subs,ranges);

%%

% ps=     0.6;
ihr=    datax.pr.ihr;%[0.030000 0.002600 0.000840	0.000420 0.000800 0.002600 0.004000	0.006300 0.012000 0.019000 0.023000	0.040000 0.096000 0.100000 0.240000	0.500000 0.500000];

%ifr=    4*[0.000310 0.000030 0.000010 0.000000 0.000010 0.000040 0.000060 0.000130 0.000310 0.000700 0.001160 0.002760 0.008670 0.012150 0.035120 0.084300 0.096960];
ifr=    datax.pr.ifr;%[0.0003 0.0003 0.0002 0.0002 0.0003 0.0003 0.0009 0.0009 0.0034 0.0034 0.0073 0.0073 0.0253 0.0253 0.064 0.064 0.1325];
pdgh=   ifr./ihr;

pdgh=   accumarray(subs',pdgh.*nnprop);

end