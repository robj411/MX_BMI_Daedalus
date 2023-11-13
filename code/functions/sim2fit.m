function f=sim2fit(params,data,xdata,X,map_parameters_to_modifier,...
    x1length,lens,modifier_start,hospital_parameters)
    
    numInt=length(X)/length(data.IO);
    month_starts = datetime([2020 1 1]) + calmonths(0:numInt);
    tvec = [params(1)*-50, daysact(datetime([2019 12 31]),month_starts)];

    data.hosp1 = hospital_parameters(1);
    data.hosp2 = hospital_parameters(2);
    data.hospthresh = hospital_parameters(3);
%     data.community_scalar = params(3+[1:lens.CM]);
    data.hospA2 = data.hospA2*params(3+lens.CM+1);
    data.hospA3 = data.hospA3*params(3+lens.CM+2);
    data.hospA4 = data.hospA4*params(3+lens.CM+3);
    data.schoolA1 = data.schoolA1*params(3+lens.CM+lens.hosp+1);
    data.schoolA2 = data.schoolA2*params(3+lens.CM+lens.hosp+2);
    data.Cmat(data.edSector,:) = data.Cmat(data.edSector,:)*params(3+lens.CM+lens.hosp+3);
    data.CM = scale_community_matrix(data.CM, params(3+[1:lens.CM]));
    data.xit = X;
    data.tvec = tvec;

    mod = params(map_parameters_to_modifier+modifier_start-1);
    %%
    [simu,~,~] = heRunCovid19(data,params(2),mod);

%     [pr,NN,n,ntot,na,NNbar,NNrep,Dout,beta]=...
%         hePrepCovid19(data,numInt,params(2),...
%         params(map_parameters_to_modifier+modifier_start-1));
% 
%     [simu,~,~]=heRunCovid19(pr,n,ntot,na,NN,NNbar,NNrep,Dout,beta,X,tvec,0,data);

    t = simu.restart_initial_conditions.toutAll';
    h = simu.Hout';
    deaths = simu.Dout';
    new_admissions = simu.restart_initial_conditions.HnewAll';
    
    [unique_t, unique_index] = unique(t);
 
%     interp_new_admissions = interp1(unique_t,new_admissions(unique_index),xdata(1:x1length)); 
%     interp_deaths = interp1(unique_t,deaths(unique_index),xdata((x1length+1):(x1length+x2length))); 
%     interp_h = interp1(unique_t,h(unique_index),xdata((x1length+x2length+1):end));
%     f = [interp_new_admissions; interp_deaths; interp_h]; %

    new_admissions_by_age = simu.restart_initial_conditions.HnewbyageAll .* [10 10 1 1];
    nAges = size(new_admissions_by_age,2);
    nTimes = length(xdata)/nAges;
    interp_admissions = zeros(nTimes, nAges);
    for i = 1:nAges
        interp_admissions(:,i) = interp1(unique_t,new_admissions_by_age(unique_index,i),xdata(1:nTimes));
    end
    
    f = reshape(interp_admissions,length(xdata),1);

end