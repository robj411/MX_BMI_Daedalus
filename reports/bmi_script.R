require(data.table,quietly=T)
library(splines)
library(ggplot2)
library(stringr)
library(stringi)
library(lubridate)
library(dplyr)
require(readxl,quietly=T)
library(parallelsugar)
library(pander)
library(bw)
setwd('~/../OneDrive - Imperial College London/Mexico/reports/')
set.seed(0)

country <- 'Mexico'
country_abbr <- 'MEX'
country_abbr_2 <- 'MX'
out_path <- '../matlab_inputs'
inputs_path <- '../matlab_inputs'
multi_country_inputs_path <- #'../../data'
country_inputs_path <- '../country_data'
template_path <- '.'
contact_path <- file.path(multi_country_inputs_path,'synthetic-contact-matrices/generate_synthetic_matrices/output/syntheticcontactmatrices2020/overall/')
store_path <- '../code/store'

readxlsx <- function(...) suppressMessages(read_xlsx(...))

datafile <- file.path(country_inputs_path,'MEXcomodata.Rds')


p_hosp <- c(.132,.491,.648)
p_not_hosp <- 1 - p_hosp
p_death_given_hosp <- c(0.285,0.509,0.601)
p_death_given_not_hosp <- c(0.006,0.052,0.121)
# p(d) =  p(d | h) p(h) + p(d | not h) p(not h). p(not h | d) p(d) = p(d | not h) p(not h)
# pdirect = p(not h | d)
pdirect <- p_not_hosp*p_death_given_not_hosp/(p_not_hosp*p_death_given_not_hosp + p_hosp*p_death_given_hosp)


## relative risk functions ######################

bmi_data <- c(16,19,22,25,28,31,34,37,40,43,47,50)
diagnosismn <- c(.8,.89,1,1.1,1.18,1.23,1.26,1.29,1.32,1.35,1.4,1.43,
               .97,.98,1,1.02,1.03,1.05,1.07,1.08,1.1,1.12,1.14,1.16,
               1.23,1.11,1,.91,.9,.98,1.13,1.31,1.52,1.76,2.15,2.49)
diagnosislq <- c(.77,.88,1,1.09,1.16,1.2,1.23,1.26,1.28,1.3,1.33,1.35,
                 .95,.97,1,1,1.01,1.01,1.02,1.02,1.03,1.03,1.04,1.04,
                 1.11,1.05,1,.87,.83,.91,1.04,1.18,1.33,1.49,1.74,1.95)
diagnosisuq <- c(.82,.91,1,1.12,1.2,1.25,1.29,1.32,1.37,1.41,1.47,1.52,
                 .99,1,1,1.03,1.06,1.08,1.11,1.15,1.18,1.21,1.25,1.29,
                 1.36,1.17,1,.96,.97,1.06,1.23,1.46,1.74,2.08,2.66,3.19)
# hosp <- c(.39,.64,1,1.49,2.12,2.88,3.74,4.64,5.5,6.24,6.87,7.01,
#           .79,.89,1,1.12,1.26,1.42,1.6,1.8,2.02,2.27,2.65,2.98,
#           .84,.91,1,1.09,1.2,1.31,1.43,1.56,1.71,1.87,2.11,2.3)
diagnosis_to_hospmn <- c(.44,.67,1,1.45,1.91,2.26,2.55,2.84,3.17,3.54,4.09,4.56,
                       .81,.9,1,1.11,1.24,1.37,1.53,1.7,1.88,2.09,2.41,2.68,
                       .91,.95,1,1.05,1.1,1.16,1.22,1.28,1.34,1.41,1.51,1.58)
diagnosis_to_hosplq <- c(.38,.62,1,1.36,1.72,2.02,2.28,2.54,2.8,3.07,3.45,3.76,
                       .77,.88,1,1.08,1.18,1.27,1.38,1.5,1.62,1.76,1.96,2.13,
                       .83,.91,1,1,1.01,1.01,1.01,1.02,1.02,1.02,1.03,1.03)
diagnosis_to_hospuq <- c(.52,.72,1,1.55,2.12,2.53,2.85,3.18,3.59,4.08,4.85,5.55,
                       .85,.92,1,1.14,1.3,1.48,1.68,1.92,2.19,2.49,2.96,3.38,
                       .99,1,1,1.1,1.21,1.33,1.46,1.61,1.77,1.94,2.2,2.42)
diagnosis_to_deathmn <- c(6.37,2.52,1,.47,.36,.46,.77,1.33,2.32,4.04,8.46,14.72,
                        1.95,1.4,1.,.72,.59,.61,.74,.96,1.23,1.58,2.2,2.83,
                        1.05,1.03,1,.97,.95,.93,.9,.88,.86,.84,.81,.79)
diagnosis_to_deathlq <- c(2.22,1.49,1,.3,.18,.22,.37,.65,1.08,1.75,3.18,4.86,
                        1.39,1.18,1.,.61,.44,.44,.54,.68,.82,.96,1.17,1.35,
                        1.14,1.07,1,1.02,1.03,1.05,1.07,1.08,1.1,1.12,1.14,1.16)
diagnosis_to_deathuq <- c(18.23,4.26,1,.74,.73,.98,1.58,2.75,4.97,9.33,22.52,44.52,
                        2.73,1.65,1.,.85,.78,.84,1.02,1.35,1.85,2.59,4.13,5.91,
                        1.14,1.07,1,1.02,1.03,1.05,1.07,1.08,1.1,1.12,1.14,1.16)
hosp_to_deathmn <- c(.64,.8,1,1.25,1.55,1.93,2.41,3,3.74,4.66,6.24,7.77,
                   1.27,1.1,1,.95,.95,1,1.1,1.27,1.53,1.95,2.88,4.08,
                   .98,.99,1,1.01,1.02,1.03,1.05,1.06,1.07,1.08,1.1,1.11)
hosp_to_deathlq <- c(.52,.72,1,1.12,1.25,1.4,1.57,1.76,1.97,2.2,2.56,2.87,
                   .99,.98,1,.87,.81,.81,.86,.97,1.15,1.42,1.95,2.53,
                   .88,.94,1,.96,.92,.88,.84,.81,.77,.74,.7,.67)
hosp_to_deathuq <- c(.8,.89,1,1.39,1.92,2.66,3.69,5.12,7.09,9.83,15.2,21.06,
                   1.63,1.23,1,1.04,1.12,1.23,1.4,1.65,2.04,2.66,4.24,6.6,
                   1.09,1.04,1,1.07,1.14,1.22,1.3,1.39,1.48,1.58,1.73,1.85)
outcomes <- c('Diagnosis','Hospitalisation','Mortality')
agegroups <- c('18 to 59','60 to 79','80 plus')
nOutcomes <- length(outcomes)
nAgegroups <- length(agegroups)
# hospitalisation:direct to death is 136:5.
##!! assuming probability to be hospitalised given death independent of obesity
mixing_weights <- rep(pdirect,each=length(bmi_data))

diagnosissd <- (log(diagnosisuq)-log(diagnosislq))/3.92
diagnosis_to_hospsd <- (log(diagnosis_to_hospuq)-log(diagnosis_to_hosplq))/3.92
diagnosis_to_deathsd <- (log(diagnosis_to_deathuq)-log(diagnosis_to_deathlq))/3.92
hosp_to_deathsd <- (log(hosp_to_deathuq)-log(hosp_to_deathlq))/3.92

get_rr <- function(agegroup,bmi,hrs,outcome='Hospitalisation'){
  age_in_years <- (agegroup-1)*5
  if(age_in_years<60){
    agelab <- '18 to 59'
    # if(age_in_years<40){
    #   agelab <- '18 to 39'
    # }else if(age_in_years<60){
    #   agelab <- '40 to 59'
  }else if(age_in_years<80){
    agelab <- '60 to 79'
  }else{
    agelab <- '80 plus'
  }
  use_bmi <- pmax(pmin(bmi,max(hrs$bmi)),min(hrs$bmi))
  approx(x=bmi_data,y=subset(hrs,ages==agelab&measure==outcome)$hr,xout=use_bmi)$y/min(subset(hrs,ages==agelab&measure==outcome)$hr)
}

bmi_cut <- 29.9

## population data #######################

age_groups <- c('0 to 4','5 to 9','10 to 14','15 to 19','20 to 24','25 to 29','30 to 34','35 to 39','40 to 44',
                '45 to 49','50 to 54','55 to 59','60 to 64','65 to 69','70 to 74','75 to 79','80 to 84','85 to 89','90 to 94','95 to 120')
twoages <- list(5:13,14:length(age_groups))
ageind <- 5:length(age_groups)

pop_file <- read.csv(file.path(multi_country_inputs_path,'IHME_GBD_2019_POP_2019_Y2020M10D15.csv'),stringsAs=F)
country_pop <- subset(pop_file,location_name==country&sex_name=='both')
if(!'0 to 4'%in%country_pop$age_group_name)
  country_pop$age_group_name[country_pop$age_group_name=='Under 5'] <- '0 to 4'
if(!'95 to 120'%in%country_pop$age_group_name)
  country_pop$age_group_name[country_pop$age_group_name=='95 plus'] <- '95 to 120'
population <- country_pop$val[match(age_groups,country_pop$age_group_name)]
population[max(ageind)] <- sum(population[max(ageind):length(population)])
population <- population[ageind]

## datasets ##########################

if(!file.exists(datafile)){
  
  filenames <- list.files(country_inputs_path)
  fulldata <- read.csv(file.path(country_inputs_path,tail(filenames[grepl('COVID19MEXICO',filenames)],1)),stringsAs=F)
  
  setDT(fulldata)
  fulldata[,admitted:=TIPO_PACIENTE==2]
  fulldata[,died:=FECHA_DEF!='9999-99-99']
  fulldata[,admission_date:=FECHA_INGRESO]
  fulldata[,age:=EDAD]
  fulldata[,COPD:=EPOC]
  fulldata[,diabetes:=DIABETES]
  fulldata[,asthma:=ASMA]
  fulldata[,immunosuppressed:=INMUSUPR]
  fulldata[,hypertension:=HIPERTENSION]
  fulldata[,cardiovascular:=CARDIOVASCULAR]
  fulldata[,obesity:=OBESIDAD]
  fulldata[,CKD:=RENAL_CRONICA]
  fulldata[,smoker:=TABAQUISMO]
  fulldata[,case:=CLASIFICACION_FINAL%in%1:3]
  
  outcome_data <- fulldata[case==T,.(admitted,died,admission_date,age,diabetes,COPD,asthma,immunosuppressed,hypertension,cardiovascular,obesity,CKD,smoker)]
  saveRDS(outcome_data,datafile)
  
}else{
  outcome_data <- readRDS(datafile)
}
outcome_data <- subset(outcome_data,age<120)
outcome_data[,agegroup99:=floor(age/5)+1]
agenames <- sapply(ageind,function(x)paste0((x-1)*5,' to ',x*5-1))
agenames[length(agenames)] <- paste0(strsplit(agenames[length(agenames)],' to ')[[1]][1],' plus')
first_death_admission <- min(subset(outcome_data,died)$admission_date)
outcome_data <- subset(outcome_data,admission_date<=as.Date(first_death_admission)+days(365)&agegroup99%in%ageind)
surveillance_rate <- sum(outcome_data$died)/nrow(outcome_data)*100
target_rate <- .47
fraction_cases_captured <- target_rate/surveillance_rate

allyears <- haven::read_dta(file.path(country_inputs_path,'IMC_adultos_2000_2018.dta'))  
years <- unique(allyears$survey)
bmidatasets <- list()
for(i in years) {
  bmidatasets[[as.character(i)]] <- 
    setDT(subset(allyears,survey==i)[,colnames(allyears)%in%c('id_sujeto','sexo','edad','peso','talla','imc_valid',"ponderador_alt",'conglome','estrato')])
  bmidatasets[[as.character(i)]][,edad99:=as.numeric(edad)]
  bmidatasets[[as.character(i)]][edad99>99,edad:=99]
  bmidatasets[[as.character(i)]][,agegroup99:=floor(edad99/5)+1]
  bmidatasets[[as.character(i)]][,sexo:=as.numeric(sexo)]
}

ses <- setDT(subset(allyears,survey==2018)[,colnames(allyears)%in%c('edad','imc_valid','ponderador_alt','nsef_propia_sinpond')])
ses[,age:=cut(edad,c(0,39,59,Inf),labels=c('20-39','40-59','60+'))]
ses[,weight:=cut(imc_valid,c(0,18,25,30,Inf),labels=c('Underweight','Healthy','Overweight','Obesity'))]
ses[,edad:=NULL]
ses[,imc_valid:=NULL]
sestab <- ses[,sum(ponderador_alt),by=.(weight,age,nsef_propia_sinpond)]
sestab[,denom:=sum(V1),by=.(age,weight)]
sestab[,frac:=V1/denom]
colnames(sestab)[colnames(sestab)=='nsef_propia_sinpond'] <- 'ses'
sestab[,V1:=NULL]
sestab[,denom:=NULL]

taxes <- as.data.frame(readxlsx(file.path(country_inputs_path,'Caloric reduction_taxes.xlsx'),skip=4))[1:36,c(1:9)]
colnames(taxes) <- c('ses','age','weight','beverage','beverage_sd','food','food_sd','beverage_pc','food_pc')
taxes$beverage_reduction <-  taxes$beverage * taxes$beverage_pc
taxes$food_reduction <- taxes$food * taxes$food_pc
taxes$beverage_reduction_sd <- taxes$beverage_sd * taxes$beverage_pc
taxes$food_reduction_sd <- taxes$food_sd * taxes$food_pc
taxes$reduction <- taxes$food_reduction + taxes$beverage_reduction
# taxes$reduction_sd <- taxes$food_reduction_sd + taxes$beverage_reduction_sd # sqrt(taxes$food_reduction_sd^2 + taxes$beverage_reduction_sd^2)
coef_of_var <- data.frame(ses=c('Highest','Middle','Lowest'),cv=c(2.3/2.3,1.84/5.8,3.31/10.2))
taxes$cv <- coef_of_var$cv[match(taxes$ses,coef_of_var$ses)]
taxes$ses[taxes$ses=='Highest'] <- 3
taxes$ses[taxes$ses=='Middle'] <- 2
taxes$ses[taxes$ses=='Lowest'] <- 1
taxes$weight[taxes$weight=='Normal'] <- 'Healthy'
taxes$weight[taxes$weight=='Overweigth'] <- 'Overweight'
taxes$reduction_sd <- taxes$cv * taxes$reduction
reductions_tax0 <- setDT(merge(taxes,sestab,on=.(weight,age,ses)))
reductions_tax <-  reductions_tax0[,.(calorie=sum(reduction*frac),
                                      # caloriesd=sqrt(sum(reduction_sd^2*(frac/sum(frac))^2)),
                                      caloriesd=sum(reduction_sd*frac)
                                      ),by=.(age,weight)]
setkey(reductions_tax,'age','weight')
write.csv(reductions_tax,file.path(country_inputs_path,'reductions_tax.csv'),row.names=F)

reductions <- as.data.frame(readxlsx(file.path(country_inputs_path,'Caloric reduction_taxes.xlsx'),skip=4))[1:12,13:18]
colnames(reductions) <- c('age','weight','calorie','caloriesd','sodium','sodiumsd')
reductions$weight[reductions$weight=='Normal'] <- 'Healthy'
reductions$calorie <- as.numeric(reductions$calorie)
reductions$caloriesd <- as.numeric(reductions$caloriesd)
write.csv(reductions,file.path(country_inputs_path,'reductions.csv'),row.names=F)

# https://ensanut.insp.mx/encuestas/ensanut2018/descargas.php
healthdata <- haven::read_dta(file.path(country_inputs_path,'CN_ANTROPOMETRIA_ADULTOS_2018.dta'))
# “pregnancy_lactating” (0=no 1=yes 99=does not know) 
# healthdata[,colnames(healthdata)%in%c("EDAD","UPM_DIS","EST_DIS","F_ANTROP_INSP")]
# The weights to account for the sample design are these:
# Primary sample unit: UPM_DIS
# Strata: EST_DIS
# Weigth: F_ANTROP_INSP
bmidata <- healthdata[,colnames(healthdata)%in%c("imc_valid","peso_f","talla_f","EDAD","SEXO")]

setDT(bmidata)
# bmidata[,bmi:=imc_valid]
bmidata[,EDAD99:=as.numeric(EDAD)]
bmidata[EDAD99>99,EDAD99:=99]
bmidata[,agegroup99:=floor(EDAD99/5)+1]
bmidata[,SEXO:=as.numeric(SEXO)]

outcome_data[agegroup99>max(bmidata$agegroup99),agegroup99:=max(bmidata$agegroup99)]
outcome_data <- subset(outcome_data,agegroup99%in%unique(bmidata$agegroup99))

set2018 <- bmidatasets[['2018']]
set2018[,reductionagegroup:=cut(edad99,c(19,39,59,Inf),labels=c('20-39','40-59','60+'))]
set2018[,weightgroup:=cut(imc_valid,c(10,18,24.9,bmi_cut,Inf),labels=c('Underweight','Healthy','Overweight','Obesity'))]



## for bw

total_days <- 365*2
dt <- 1
dtdays <- round(total_days/dt)


## start loop #########################

numcores <- 4
nsamples <- 8192

set.seed(0)
diagquants <- runif(nsamples)
set.seed(1)
dhquants <- runif(nsamples)
set.seed(2)
ddquants <- runif(nsamples)
set.seed(3)
hdquants <- runif(nsamples)

caloriesquants <- sodiumquants <- matrix(0,nrow=nsamples,ncol=nrow(reductions))
taxquants <- matrix(0,nrow=nsamples,ncol=nrow(reductions_tax))
for(i in 1:nsamples){
  set.seed(i+3)
  caloriesquants[i,] <- runif(nrow(reductions))
  sodiumquants[i,] <- runif(nrow(reductions))
  taxquants[i,] <- runif(nrow(reductions_tax))
}

# save variables for voi
write.table(diagquants,file.path(store_path,'diagquants.csv'),row.names=F,col.names=F)
write.table(dhquants,file.path(store_path,'dhquants.csv'),row.names=F,col.names=F)
write.table(ddquants,file.path(store_path,'ddquants.csv'),row.names=F,col.names=F)
write.table(hdquants,file.path(store_path,'hdquants.csv'),row.names=F,col.names=F)
write.table(caloriesquants,file.path(store_path,'caloriesquants.csv'),row.names=F,col.names=F,sep=',')
write.table(sodiumquants,file.path(store_path,'sodiumquants.csv'),row.names=F,col.names=F,sep=',')
write.table(taxquants,file.path(store_path,'taxquants.csv'),row.names=F,col.names=F,sep=',')

system.time(numbers <- mclapply(1:nsamples,mc.cores=numcores,FUN=function(samplenumber) 
  {
  
  diagquant <- diagquants[samplenumber]
  dhquant <- dhquants[samplenumber]
  ddquant <- ddquants[samplenumber]
  hdquant <- hdquants[samplenumber]
  
  diagnosis <- qlnorm(diagquant,log(diagnosismn),diagnosissd)
  diagnosis_to_hosp <- qlnorm(dhquant,log(diagnosis_to_hospmn),diagnosis_to_hospsd)
  diagnosis_to_death <- qlnorm(ddquant,log(diagnosis_to_deathmn),diagnosis_to_deathsd)
  hosp_to_death <- qlnorm(hdquant,log(hosp_to_deathmn),hosp_to_deathsd)
  hrs <- data.frame(hr=c(diagnosis,diagnosis*diagnosis_to_hosp,
                         diagnosis*((1-mixing_weights)*diagnosis_to_hosp*hosp_to_death + mixing_weights*diagnosis_to_death)),
                    measure=rep(outcomes,each=nAgegroups*length(bmi_data)),
                    bmi=rep(bmi_data,nOutcomes*nAgegroups),
                    ages=rep(rep(agegroups,each=length(bmi_data)),nOutcomes))
  
  
  # labelling scenario
  reductions$caloriesample <- with(reductions,qnorm(caloriesquants[samplenumber,],calorie,caloriesd))
  reductions$sodiumsample <- with(reductions,qnorm(sodiumquants[samplenumber,],sodium ,sodiumsd ))
  set2018[,EIchange:=subset(reductions,age==reductionagegroup&weight==weightgroup)$caloriesample,by=.(reductionagegroup,weightgroup)]
  set2018[,NAchange:=subset(reductions,age==reductionagegroup&weight==weightgroup)$sodiumsample,by=.(reductionagegroup,weightgroup)]
  # set2018[,EIchange:=rnorm(nrow(set2018),EIchangemean,1)]
  # set2018[,NAchange:=rnorm(nrow(set2018),NAchangemean,1)]
  

  EIchangemat <- -t(pracma::repmat(set2018$EIchange,dtdays,1)) #-36.8 
  NAchangemat <- -t(pracma::repmat(set2018$NAchange,dtdays,1)) # 30.2
  set2018[,scenbmi:=bw::adult_weight(bw=peso, ht=talla/100, age=edad99, sex=c('male','female')[sexo], 
                                     EIchange = EIchangemat, 
                                     NAchange = NAchangemat,
                                     days = dtdays*dt, dt = dt, checkValues = TRUE)$Body_Mass_Index[,dtdays]]
  
  # tax scenarios
  reductions_tax[,caloriesample:=qnorm(taxquants[samplenumber,],calorie,caloriesd)]
  set2018[,EIchangetax:=subset(reductions_tax,age==reductionagegroup&weight==weightgroup)$caloriesample,by=.(reductionagegroup,weightgroup)]
  # set2018[,EIchangetax:=rnorm(nrow(set2018),EIchangetaxmean,1)]
  set2018[,NAchangetax:=0]
  EIchangemattax <- -t(pracma::repmat(set2018$EIchangetax,dtdays,1)) #-36.8 
  NAchangemattax <- -t(pracma::repmat(set2018$NAchangetax,dtdays,1)) # 30.2
  for(i in 1:3) set2018[,paste0('taxbmi',i):=bw::adult_weight(bw=peso, ht=talla/100, age=edad99, sex=c('male','female')[sexo], 
                                                              EIchange = EIchangemattax*i, 
                                                              NAchange = NAchangemattax*i,
                                                              days = dtdays*dt, dt = dt, checkValues = TRUE)$Body_Mass_Index[,dtdays]]



  # resample BMI
  
  for(x in unique(bmidata$agegroup99)){
    for(y in 1:2){
      subsam <- subset(set2018,agegroup99==x&!is.na(imc_valid)&sexo==y)
      sz <- sum(bmidata$agegroup99==x&bmidata$SEXO==y)
      indices <- sample(1:nrow(subsam),size=sz,replace=T,prob=subsam$ponderador_alt)
      bmidata[agegroup99==x&SEXO==y,bmi:=subsam$imc_valid[indices]]
      bmidata[agegroup99==x&SEXO==y,scenbmi:=subsam$scenbmi[indices]]
      for(i in 1:3) bmidata[agegroup99==x&SEXO==y,paste0('taxbmi',i):=subsam[[paste0('taxbmi',i)]][indices]]
    }
  }
  
  
  dat <- subset(bmidata,agegroup99>4&!is.na(bmi))[,grepl('agegroup|bmi|SEXO|EDAD',colnames(bmidata)),with=F]
  popsizesbmidata <- dat[,.N,by=agegroup99]
  dat[,ageweight:=population[agegroup99-4]/popsizesbmidata$N[popsizesbmidata$agegroup99==agegroup99],by=agegroup99]


  ## join #####################################
  
  dat[,hrr:=get_rr(agegroup99,bmi,hrs),by=.(agegroup99)]
  dat[,drr:=get_rr(agegroup99,bmi,hrs,outcome='Mortality'),by=.(agegroup99)]
  dat[,crr:=get_rr(agegroup99,bmi,hrs,outcome='Diagnosis'),by=.(agegroup99)]
  dat[,bmi_cat:=cut(bmi,c(0,bmi_cut,100))]
  
  admission_rate <- outcome_data[,.(rate=sum(admitted)*fraction_cases_captured/.N,
                                    death_rate=sum(died)*fraction_cases_captured/.N,
                                    diagnosis_rate=.N*fraction_cases_captured/.N),by=agegroup99]
  



  bmipop <- dat[,.(sm_drr=sum(drr*ageweight),
                   sm_hrr=sum(hrr*ageweight),
                   sm_crr=sum(crr*ageweight),
                   N=sum(ageweight)),by=agegroup99]
  setkey(bmipop)
  joined <- bmipop[admission_rate, on=.(agegroup99)]
  # joined[,expected_admissions:=rate*N]
  # joined[,expected_deaths:=death_rate*N]
  joined[,ad_per_rr:=rate*N/sm_hrr]
  joined[,death_per_rr:=death_rate*N/sm_drr]
  joined[,diagnosis_per_rr:=diagnosis_rate*N/sm_crr]
  dat <- dat[joined[,.(agegroup99,ad_per_rr,death_per_rr,diagnosis_per_rr)],on=.(agegroup99)]




  dat[,hrr:=get_rr(agegroup99,bmi,hrs),by=.(agegroup99)]
  dat[,drr:=get_rr(agegroup99,bmi,hrs,outcome='Mortality'),by=.(agegroup99)]
  dat[,crr:=get_rr(agegroup99,bmi,hrs,outcome='Diagnosis'),by=.(agegroup99)]
  dat[,prob_admitted:=hrr*ad_per_rr]
  dat[,prob_died:=drr*death_per_rr]
  dat[,prob_diagnosed:=crr*diagnosis_per_rr]
  
  dat[,hrr_rel:=prob_admitted/min(prob_admitted)]
  dat[,drr_rel:=prob_died/min(prob_died)]
  dat[,crr_rel:=prob_diagnosed/min(prob_diagnosed)]
  dat[,hrr_cat:=mean(hrr_rel),by=.(bmi_cat,agegroup99)]
  dat[,drr_cat:=mean(drr_rel),by=.(bmi_cat,agegroup99)]
  dat[,crr_cat:=mean(crr_rel),by=.(bmi_cat,agegroup99)]

  hospbmipop <- dat[,.(sm_hrr=sum(hrr_cat*ageweight),min_hrr=min(hrr_cat)*sum(ageweight),weight=mean(ageweight)),by=agegroup99]
  deathbmipop <- dat[,.(sm_drr=sum(drr_cat*ageweight),min_drr=min(drr_cat)*sum(ageweight),weight=mean(ageweight)),by=agegroup99]
  diagnosisbmipop <- dat[,.(sm_crr=sum(crr_cat*ageweight),min_crr=min(crr_cat)*sum(ageweight),weight=mean(ageweight)),by=agegroup99]
  setkey(deathbmipop)
  setkey(hospbmipop)
  setkey(diagnosisbmipop)
  dpaf <- 1-deathbmipop[,sum(min_drr*weight)/sum(sm_drr*weight)]
  dpaf1 <- 1-deathbmipop[agegroup99<14,sum(min_drr*weight)/sum(sm_drr*weight)]
  dpaf2 <- 1-deathbmipop[agegroup99>13,sum(min_drr*weight)/sum(sm_drr*weight)]
  hpaf <- 1-hospbmipop[,sum(min_hrr*weight)/sum(sm_hrr*weight)]
  hpaf1 <- 1-hospbmipop[agegroup99<14,sum(min_hrr*weight)/sum(sm_hrr*weight)]
  hpaf2 <- 1-hospbmipop[agegroup99>13,sum(min_hrr*weight)/sum(sm_hrr*weight)]
  cpaf <- 1-diagnosisbmipop[,sum(min_crr*weight)/sum(sm_crr*weight)]
  cpaf1 <- 1-diagnosisbmipop[agegroup99<14,sum(min_crr*weight)/sum(sm_crr*weight)]
  cpaf2 <- 1-diagnosisbmipop[agegroup99>13,sum(min_crr*weight)/sum(sm_crr*weight)]

  
  baseline <- sapply(ageind,function(x)with(subset(dat,agegroup99==x),sum(prob_admitted*ageweight)))
  deathbaseline <- sapply(ageind,function(x)with(subset(dat,agegroup99==x),sum(prob_died*ageweight)))
  diagnosisbaseline <- sapply(ageind,function(x)with(subset(dat,agegroup99==x),sum(prob_diagnosed*ageweight)))


## historic scenarios ########################

  pastscen <- list()
  admissions <- list()
  deaths <- list()
  diagnoses <- list()
  for(i in 1:(length(bmidatasets)-2)){
    pastscen[[i]] <- copy(dat)
    for(x in unique(pastscen[[i]]$agegroup99)){
      for(y in 1:2){
        subsam <- subset(bmidatasets[[i]],agegroup99==x&!is.na(imc_valid)&sexo==y)
        sz <- sum(pastscen[[i]]$agegroup99==x&pastscen[[i]]$SEXO==y)
        pastscen[[i]][agegroup99==x&SEXO==y,bmi:=sample(subsam$imc_valid,size=sz,replace=T,prob=subsam$ponderador_alt)]
      }
    }
    pastscen[[i]][,rr:=get_rr(agegroup99,bmi,hrs),by=.(agegroup99)]
    pastscen[[i]][,drr:=get_rr(agegroup99,bmi,hrs,outcome='Mortality'),by=.(agegroup99)]
    pastscen[[i]][,crr:=get_rr(agegroup99,bmi,hrs,outcome='Diagnosis'),by=.(agegroup99)]
    pastscen[[i]][,prob_admitted:=rr*ad_per_rr]
    pastscen[[i]][,prob_died:=drr*death_per_rr]
    pastscen[[i]][,prob_diagnosed:=crr*diagnosis_per_rr]
    # these use age weight as resampling should be representative, so we correct only for representation of age groups
    admissions[[i+1+3]] <- sapply(ageind,function(x)with(subset(pastscen[[i]],agegroup99==x),sum(prob_admitted*ageweight)))
    deaths[[i+1+3]] <- sapply(ageind,function(x)with(subset(pastscen[[i]],agegroup99==x),sum(prob_died*ageweight)))
    diagnoses[[i+1+3]] <- sapply(ageind,function(x)with(subset(pastscen[[i]],agegroup99==x),sum(prob_diagnosed*ageweight)))
    
  }


## tax scenarios #################################

  taxscen <- list()
  meanbmis <- c()
  obprevs <- c()
  for(i in 1:3){
    taxscen[[i]] <- copy(dat)
    taxscen[[i]][,bmi:=get(paste0('taxbmi',i))]
    meanbmis <- c(meanbmis,with(taxscen[[i]],sum(bmi*ageweight)/sum(ageweight)))
    obprevs <- c(obprevs,with(taxscen[[i]],sum(as.numeric(bmi>bmi_cut)*ageweight)/sum(ageweight)))
    
    taxscen[[i]][,rr:=get_rr(agegroup99,bmi,hrs),by=.(agegroup99)]
    taxscen[[i]][,drr:=get_rr(agegroup99,bmi,hrs,outcome='Mortality'),by=.(agegroup99)]
    taxscen[[i]][,crr:=get_rr(agegroup99,bmi,hrs,outcome='Diagnosis'),by=.(agegroup99)]
    taxscen[[i]][,prob_admitted:=rr*ad_per_rr]
    taxscen[[i]][,prob_died:=drr*death_per_rr]
    taxscen[[i]][,prob_diagnosed:=crr*diagnosis_per_rr]
    # these use age weight as resampling should be representative, so we correct only for representation of age groups
    admissions[[i+1]] <- sapply(ageind,function(x)with(subset(taxscen[[i]],agegroup99==x),sum(prob_admitted*ageweight)))
    deaths[[i+1]] <- sapply(ageind,function(x)with(subset(taxscen[[i]],agegroup99==x),sum(prob_died*ageweight)))
    diagnoses[[i+1]] <- sapply(ageind,function(x)with(subset(taxscen[[i]],agegroup99==x),sum(prob_diagnosed*ageweight)))
    
  }

  ## labelling scenario #################################
  
  dat[,scenrr:=get_rr(agegroup99,scenbmi,hrs),by=.(agegroup99)]
  dat[,scendrr:=get_rr(agegroup99,scenbmi,hrs,outcome='Mortality'),by=.(agegroup99)]
  dat[,scencrr:=get_rr(agegroup99,scenbmi,hrs,outcome='Diagnosis'),by=.(agegroup99)]
  dat[,scenprob_admitted:=scenrr*ad_per_rr]
  dat[,scenprob_died:=scendrr*death_per_rr]
  dat[,scenprob_diagnosed:=scencrr*diagnosis_per_rr]
  admissions[[1]] <- sapply(ageind,function(x)with(subset(dat,agegroup99==x),sum(scenprob_admitted*ageweight)))
  deaths[[1]] <- sapply(ageind,function(x)with(subset(dat,agegroup99==x),sum(scenprob_died*ageweight)))
  diagnoses[[1]] <- sapply(ageind,function(x)with(subset(dat,agegroup99==x),sum(scenprob_diagnosed*ageweight)))

  ## no obesity scenario #################################
  
  dat[,pafbmi:=bmi]
  for(x in unique(dat$agegroup99)){
    for(y in 1:2){
      subsam <- subset(dat,agegroup99==x&!is.na(bmi)&SEXO==y&bmi<=bmi_cut)
      sz <- sum(dat$agegroup99==x&dat$SEXO==y&dat$bmi>bmi_cut)
      dat[agegroup99==x&SEXO==y&bmi>bmi_cut,pafbmi:=sample(subsam$bmi,size=sz,replace=T)]
    }
  }
  dat[,pafrr:=get_rr(agegroup99,pafbmi,hrs),by=.(agegroup99)]
  dat[,pafdrr:=get_rr(agegroup99,pafbmi,hrs,outcome='Mortality'),by=.(agegroup99)]
  dat[,pafcrr:=get_rr(agegroup99,pafbmi,hrs,outcome='Diagnosis'),by=.(agegroup99)]
  dat[,pafprob_admitted:=pafrr*ad_per_rr]
  dat[,pafprob_died:=pafdrr*death_per_rr]
  dat[,pafprob_diagnosed:=pafcrr*diagnosis_per_rr]
  # these use age weight as resampling should be representative, so we correct only for representation of age groups
  admissions[[length(admissions)+1]] <- sapply(ageind,function(x)with(subset(dat,agegroup99==x),sum(pafprob_admitted*ageweight)))
  deaths[[length(deaths)+1]] <- sapply(ageind,function(x)with(subset(dat,agegroup99==x),sum(pafprob_died*ageweight)))
  diagnoses[[length(diagnoses)+1]] <- sapply(ageind,function(x)with(subset(dat,agegroup99==x),sum(pafprob_diagnosed*ageweight)))
  

  ## average bmi and prevalence #########################
  
  meanbmis <- c(meanbmis,t(sapply(c(1,2,3),function(y){
    with(pastscen[[y]],sum(bmi*ageweight)/sum(ageweight))
  })))
  meanbmis <- c(meanbmis,with(dat,sum(pafbmi*ageweight)/sum(ageweight)))
  obprevs <- c(obprevs,t(sapply(c(1,2,3),function(y){
    with(pastscen[[y]],sum(as.numeric(bmi>bmi_cut)*ageweight)/sum(ageweight))
  })))
  obprevs <- c(obprevs,with(dat,sum(as.numeric(pafbmi>bmi_cut)*ageweight)/sum(ageweight)))
  




  mean_bmi <- as.numeric(dat[,.(sum(as.numeric(bmi)*ageweight)/sum(ageweight))])
  scen_mean_bmi <- as.numeric(dat[,.(sum(as.numeric(scenbmi)*ageweight)/sum(ageweight))])
  meanbmis <- c(mean_bmi,scen_mean_bmi,meanbmis)
  obesity_pc <- as.numeric(dat[,.(sum(as.numeric(bmi>bmi_cut)*ageweight)/sum(ageweight))])
  scen_obesity_pc <- as.numeric(dat[,.(sum(as.numeric(scenbmi>bmi_cut)*ageweight)/sum(ageweight))])
  obprevs <- c(obesity_pc,scen_obesity_pc,obprevs)
  
  ## results #########################
  
  finalpc <- sapply(twoages,function(x)sum(admissions[[1]][x-4])/sum(baseline[x-4])*100)
  deathpc <- sapply(twoages,function(x)sum(deaths[[1]][x-4])/sum(deathbaseline[x-4])*100)
  diagnosispc <- sapply(twoages,function(x)sum(diagnoses[[1]][x-4])/sum(diagnosisbaseline[x-4])*100)
  
  

  scen14rr <- do.call(cbind,lapply(1:4,function(i){
    finalpc <- sapply(twoages,function(x)sum(admissions[[i]][x-4])/sum(baseline[x-4])*100)
    deathpc <- sapply(twoages,function(x)sum(deaths[[i]][x-4])/sum(deathbaseline[x-4])*100)
    diagnosispc <- sapply(twoages,function(x)sum(diagnoses[[i]][x-4])/sum(diagnosisbaseline[x-4])*100)
    res <- as.data.frame(rbind(diagnosispc,finalpc,deathpc))
    res
  }))
  
  rownames(scen14rr) <- c("Diagnoses","Hospitalisations", "Deaths")
  
  scen58rr <- do.call(cbind,lapply(5:length(admissions),function(i){
    finalpc <- sapply(twoages,function(x)sum(admissions[[i]][x-4])/sum(baseline[x-4])*100)
    deathpc <- sapply(twoages,function(x)sum(deaths[[i]][x-4])/sum(deathbaseline[x-4])*100)
    diagnosispc <- sapply(twoages,function(x)sum(diagnoses[[i]][x-4])/sum(diagnosisbaseline[x-4])*100)
    res2 <- as.data.frame(rbind(diagnosispc,finalpc,deathpc))
    res2
  }))
  
  rownames(scen58rr) <- c("Diagnoses","Hospitalisations", "Deaths")
  
  write.csv(cbind(scen14rr,scen58rr),file.path(inputs_path,paste0('/relativerisks/sample',samplenumber,'.csv')))
  
  plotdat <- data.frame(obs=obprevs,bmi=meanbmis)
  write.csv(plotdat,file.path(inputs_path,paste0('/meanbmiobprev/sample',samplenumber,'.csv')))

  return(samplenumber)
}))
