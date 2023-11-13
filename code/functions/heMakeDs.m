function f=heMakeDs(NN,x,data,wfh,past_hosp)


%NN - vector of populations including non-working.
%x - proportion of each sector open - not including non-working.
%The contact matrices are entirely determined by 'economics', both sector closures and working-from-home
%However, the succeptability of children may be included

% workforce present is by nonlinear production function, or by school
% opening
w = x.^(1./data.alpha);
w(data.edSector) = x(data.edSector);

%% COMMUNITY-COMMUNITY MATRIX:

childrenLessSus=0;

lx = length(x);%Number of sectors%make this standard
ln = length(NN);

ageindex = data.ageindex;
CM = data.CM;
CworkRow = CM(ageindex.adInd,:);

%%change NN here based on wfh

NNrep = repmat(NN'/sum(NN),ln,1);%population proportion matrix
NNrel = NN(ageindex.age20to64)/sum(NN(ageindex.age20to64));%proportion of adult population in each sector as vector

%Make A:
matA = zeros(ln,ln);
matA(ageindex.community, ageindex.community) = CM; % community contacts in bottom-right corner
matA(1:lx,ageindex.community) = repmat(CworkRow,lx,1); % worker to community (RHS)
matA(:,ageindex.age20to64)=repmat(matA(:,lx+ageindex.adInd),1,lx+1).*repmat(NNrel',ln,1); % everyone to all adults

%% Modify depending on x:

%Education:
matA(ageindex.age0to4,ageindex.age0to4)=    matA(ageindex.age0to4,ageindex.age0to4)+    x(data.edSector).^2*  data.schoolA1;%mixing within age groups only
matA(ageindex.age5to19,ageindex.age5to19)=    matA(ageindex.age5to19,ageindex.age5to19)+    x(data.edSector).^2*  data.schoolA2;

%Hospitality:

hesitancy = hesitancy_function([data.hosp1,data.hosp2],past_hosp,data.hospthresh);
% extend to sectors 7, 8, 9, 10? apply x_con to final demand?
sects = data.conSector;
x_con = hesitant_consumption(hesitancy,x,data);
% x(sects) = x_con;
psub=NN(sects);
psub=sum(psub.*x(sects))/sum(psub);%constant from 0-1, weighted measure of how much sectors are open
matA(ageindex.age20to64,:) =    matA(ageindex.age20to64,:) +    psub.^2*   data.hospA3*    NNrep(ageindex.age20to64,:);%mixing between all age groups, including pre-school
matA(ageindex.age5to19,:)  =    matA(ageindex.age5to19,:)  +    psub.^2*   data.hospA2*    NNrep(ageindex.age5to19,:);
matA(ageindex.age65plus,:) =    matA(ageindex.age65plus,:) +    psub.^2*   data.hospA4*    NNrep(ageindex.age65plus,:);



%Transport:
matA(1:lx,1:lx)=    matA(1:lx,1:lx)+    repmat(w',lx,1).*   data.travelA3.*  NNrep(1:lx,1:lx).*  repmat(1-wfh,lx,1).*repmat(1-wfh',1,lx);%home-working has a compound effect

%% WORKER-WORKER AND COMMUNITY-WORKER MATRICES:

%Make B and C:
worker_to_worker = data.B;
worker_to_worker = worker_to_worker.*(1-wfh).*(1-wfh);%home-working has a compound effect
worker_to_worker(ageindex.community) = 0;
w(ageindex.community) = 0;
matB = diag(w.*worker_to_worker');

x(ageindex.community) = 0;
customer_to_worker = data.Cmat;
customer_to_worker = customer_to_worker.*repmat(1-wfh',1,4);
customer_to_worker(ageindex.community,:) = 0;
matC = zeros(ln);
matC(:,ageindex.community) = repmat(x,1,4).*customer_to_worker;
matC(:,ageindex.age20to64) = repmat(x.*customer_to_worker(:,ageindex.adInd),1,lx+1).*NNrel';

worker_to_customer = data.Emat;
worker_to_customer = worker_to_customer.*repmat(1-wfh,4,1);
worker_to_customer(:,ageindex.community) = 0;
matE = zeros(ln);
matE(ageindex.community,:) = repmat(x',4,1).*worker_to_customer;
matE(ageindex.age20to64,:) = repmat(x'.*worker_to_customer(ageindex.adInd,:),lx+1,1).*NNrel;


%%

D = matA+matB+matC+matE;

%%

if childrenLessSus==1
    %D(end-3,:)=.5*D(end-3,:);
    %D(end-2,:)=9/15*D(end-2,:);
    D(end-3,:)=(1-0.5)*D(end-3,:);
    D(end-2,:)=(1-(0.5*11/15))*D(end-2,:);
end

f=D;

end