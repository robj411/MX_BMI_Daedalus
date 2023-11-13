function [f,g,data_new]=heRunCovid19(data,R0,mod)

%Inputs up to beta are outputs from hePrepCovid19
%Xit - column vector with proportion of each sector open at each
%intervention point. 
%tvec - vector of time points including tvev(1)=t0, tvec(2)=lockdown start,
%tvec(end)=end of simulation. 
%plotTau=1 to plot output. 
%Note: this is set to 0 in any optimisation protocol to avoid a crash due
%to rendering loads of images!

%%
data_new = hePrepCovid19(data,R0,mod);


[DEout,Rout,~]=heSimCovid19vax(data_new);
f = DEout;
g = Rout;


end
