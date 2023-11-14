function poptim_struct=fitEpi20(day_count1,ydata1,day_count2,ydata2,day_count3,ydata3,...
    data,ylab1,ylab2,ylab3,hospital_parameters)
%% start

x_values = min(data.x_values,1);
[nSectors,nMonths] = size(x_values);

%% parameters
map_parameters_to_modifier = ...%[3:21 22*ones(1,5)];%...
    [1:24]; % june to end
del0 = [1.0867    1.0825    1.0743    1.0490    0.9432    0.9104    0.8601    0.8501    0.9648    0.9981    1.0993    1.1547    1.1248    0.9077    1.0898    1.0458    1.0872    1.1016   1.1909    0.8636    0.5934    0.4915    0.3850    0.6032];
    
CM0 = ones(1,10);
CM0(end) = 4;
CM0 = [1.2718 1.3349 0.4083 1.7737 1.7345 1.6118 1.0118 0.7332 1.0000 1.4441 1.7496 2.3489 2.2718 0.7134 1.3980 0.1180];
CM0([1,(end-3):end]) = 4;
hosp0 = [3.3877 1 2.8212]; %ones(1,3); %[2.9656 0.1112 3.2953];
sch0 = [1.7333 1.1490 1.753]; %ones(1,3); %[1.7067 0.7856 1.7966];
lens = struct;
lens.CM = length(CM0);
lens.hosp = length(hosp0);
lens.sch = length(sch0);
maxCM = 7*ones(1,lens.CM);
maxCM([1,(end-3):end]) = 40;

ub=[2, 3.5, 4, maxCM, 4*ones(1,lens.hosp), [2 2 2].*ones(1,lens.sch)*2, 2*ones(1,length(del0))];
x0=[1, 3,   .5, CM0, hosp0, sch0, del0            ];
lb=[0, 2,   0, ones(1,lens.CM)/4, ones(1,lens.hosp)/4, [1 1 5].*ones(1,lens.sch)/5, zeros(1,length(del0))];

modifier_start = length(x0) - length(del0) + 1;
 
newNMonths = length(map_parameters_to_modifier);
nNewMonths = newNMonths - nMonths;

X=[x_values, repmat(x_values(:,end),1,nNewMonths)];
X=reshape(min(X,1),nSectors*newNMonths,1);

%% data

xdata1 = day_count1;
xdata2 = day_count2;
xdata3 = day_count3;
xdata_append = [xdata1;xdata2;xdata3];
ydata_append = [ydata1;ydata2;ydata3];
xdata_append = xdata1;
ydata_append = ydata1;
x1length = length(xdata1);
x2length = length(xdata2);

xdata = min(day_count1):max(day_count1);
xdata = min(xdata_append):max(xdata_append);

%% compute

data.fit_routine = 1;

fun=@(params,xdata)sim2fit(params,data,xdata,X,map_parameters_to_modifier,...
    x1length,lens,modifier_start,hospital_parameters);

tic
rng default;%for reproducibility

options=optimoptions(@lsqcurvefit,'MaxFunctionEvaluations',500000,...
    'MaxIterations',500000,'OptimalityTolerance',1e-5,'StepTolerance',1e-5);
problem=createOptimProblem('lsqcurvefit','x0',x0,'objective',fun,...
    'xdata',xdata_append,'ydata',ydata_append,'lb',lb,'ub',ub,'options',options);
ms=MultiStart;
poptim=run(ms,problem,1);
toc

ymod = sim2fit(poptim,data,xdata_append,X,map_parameters_to_modifier,...
    x1length,lens,modifier_start,hospital_parameters);

plotxdata = datetime([2019 12 31]) + days(xdata);
plotxdata1 = datetime([2019 12 31]) + days(xdata1);
plotxdata2 = datetime([2019 12 31]) + days(xdata2);
plotxdata3 = datetime([2019 12 31]) + days(xdata3);

xdatamat = reshape(plotxdata1,length(xdata1)/4,4);
ydatamat = reshape(ydata1,length(ydata1)/4,4) .* [1/10 1/10 1 1];
ymodmat = reshape(ymod,length(ydata1)/4,4) .* [1/10 1/10 1 1];

f=figure('Units','centimeters','Position',[0 0 20 20]);
set(f,'defaulttextInterpreter','latex');
set(f,'defaultAxesTickLabelInterpreter','latex');
set(f,'defaultLegendInterpreter','latex');
set(f,'DefaultAxesFontSize',15);
titles = {'0 to 4','5 to 19','20 to 64','65 plus'};
for i = 1:4
    subplot(2,2,i)
    hold on;
    bar(xdatamat(:,i),ydatamat(:,i));
    plot(xdatamat(:,i),ymodmat(:,i),'linewidth',2.5,'color','red');
    xlim([plotxdata1(1),plotxdata1(end)]);
    % ylim([0,1.25*max(ydata_append)]);
    box on;
    grid on;
    xlabel('');
    ylabel(ylab1);
    xtickangle(45)
    title(titles{i});
end

% f=figure('Units','centimeters','Position',[0 0 20 20]);
% set(f,'defaulttextInterpreter','latex');
% set(f,'defaultAxesTickLabelInterpreter','latex');
% set(f,'defaultLegendInterpreter','latex');
% set(f,'DefaultAxesFontSize',15);
% hold on;
% bar(plotxdata2,ydata2);
% plot(plotxdata2,ymod((1+x1length):(x2length+x1length)),'linewidth',2.5,'color','red');
% xlim([plotxdata(1),plotxdata(end)]);
% % ylim([0,1.25*max(ydata_append)]);
% box on;
% grid on;
% xlabel('');
% ylabel(ylab2);
% xtickangle(-45)
% title('');
% 
% f=figure('Units','centimeters','Position',[0 0 20 20]);
% set(f,'defaulttextInterpreter','latex');
% set(f,'defaultAxesTickLabelInterpreter','latex');
% set(f,'defaultLegendInterpreter','latex');
% set(f,'DefaultAxesFontSize',15);
% hold on;
% bar(plotxdata3,ydata3);
% plot(plotxdata3,ymod((1+x2length+x1length):end),'linewidth',2.5,'color','red');
% xlim([plotxdata(1),plotxdata(end)]);
% % ylim([0,1.25*max(ydata_append)]);
% box on;
% grid on;
% xlabel('');
% ylabel(ylab3);
% xtickangle(-45)
% title('');

poptim_struct = struct;
poptim_struct.seedtime = poptim(1)*-50;
poptim_struct.R0 = poptim(2);
poptim_struct.surge_rate = 0;%poptim(3);
% poptim_struct.Delta_hfr = .1;%poptim(4);
poptim_struct.Hmin = 20000;
% poptim_struct.Delta_hr = 1;%poptim(3);
poptim_struct.community_scalar = poptim(3+[1:lens.CM]);
poptim_struct.consumer_scalar = poptim(3+lens.CM+[1:lens.hosp]);
poptim_struct.school_scalar = poptim(3+lens.CM+lens.hosp+[1:lens.sch]);
poptim_struct.map_parameters_to_modifier = map_parameters_to_modifier;
poptim_struct.modifier = poptim(modifier_start:end);


% [poptim,resid,Jacobian,varcovar,~,~]=nlinfit(xdata_append,ydata_append,fun,poptim);
% conf=nlparci(poptim,R,'jacobian',J);
% [Ypred,delta]=nlpredci(fun,xdata,poptim,R,'Covar',CovB);
% f=poptim;
% g=conf;
% poptim_struct.varcovar = varcovar;
% poptim_struct.resid = resid;
% poptim_struct.Jacobian = Jacobian;

end


