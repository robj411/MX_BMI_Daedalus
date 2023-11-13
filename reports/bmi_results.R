library(data.table)
library(earth)
library(xtable)
library(tidyr)
library(ggplot2)
setwd('~/../OneDrive - Imperial College London/Mexico/reports/')
store_path <- '../code/store'
inputs_path <- '../matlab_inputs'
country_inputs_path <- '../country_data'

## read in tables for reference #####################

reductions_tax <- read.csv(file.path(country_inputs_path,'reductions_tax.csv'))
reductions <- read.csv(file.path(country_inputs_path,'reductions.csv'))

## results in terms of percentages #################

reslist <- list()
baseline <- c('Reference',rep(0,8))
nsamples <- 8192
for(i in 1:nsamples)
  reslist[[i]] <- rbind(baseline,read.csv(file.path(store_path,paste0('results/sample',i,'.csv')),header=F))
results <- do.call(rbind,reslist)
colnames(results) <- c('Scenario','Infections','Hospital days','YLL','Total','0 to 4','5 to 19','20 to 64','65 plus')


## get mean obesity prevalences and bmis for samples #############

bmilist <- list()
for(i in 1:nsamples)
  bmilist[[i]] <- read.csv(file.path(inputs_path,paste0('meanbmiobprev/sample',i,'.csv')),header=T)
bmiresults <- do.call(rbind,bmilist)
results$bmi <- bmiresults$bmi
results$obs <- bmiresults$obs

for(i in 2:9) results[[i]] <- -as.numeric(results[[i]])

## scale to absolute numbers ####################

##!! hardcoded!!
bau_inf <- 6.9553e7
bau_yll <- 1.0019e+07
bau_total <- 4.5470e+05
bau_0to4 <- 403
bau_5to19 <- 1.2130e+03
bau_20to64 <- 2.4638e+05
bau_65plus <- 2.0670e+05
bau_hosp <- 8.5703e6

absresults <- copy(results)
absresults$Infections <- bau_inf*absresults$Infections/100/1e6
absresults$`Hospital days` <- bau_hosp*absresults$`Hospital days`/100/1e6
absresults$YLL <- bau_yll*absresults$YLL/100/1e6
absresults$Total <- bau_total*absresults$Total/100/1e3
absresults$`0 to 4` <- bau_0to4*absresults$`0 to 4`/100
absresults$`5 to 19` <- bau_5to19*absresults$`5 to 19`/100
absresults$`20 to 64` <- bau_20to64*absresults$`20 to 64`/100/1e3
absresults$`65 plus` <- bau_65plus*absresults$`65 plus`/100/1e3
colnames(absresults)[1:9] <- c('Scenario','Infections (millions)','Hospital days (millions)','YLL (millions)','Total (thousands)','0 to 4','5 to 19','20 to 64 (thousands)','65 plus (thousands)')

## print results ########################

meanres <- setDT(results)[,.(averted=mean(Total),bmi=mean(bmi),obs=mean(obs*100),
                             avertedq1=quantile(Total,0.025),bmiq1=quantile(bmi,0.025),obsq1=quantile(obs*100,0.025),
                             avertedq2=quantile(Total,0.975),bmiq2=quantile(bmi,0.975),obsq2=quantile(obs*100,0.975)),by=Scenario]
meanres$bmidst <- paste0(round(meanres$bmi,1),' (',round(meanres$bmiq1,1),', ',round(meanres$bmiq2,1),')')
meanres$obsdst <- paste0(round(meanres$obs,1),' (',round(meanres$obsq1,1),', ',round(meanres$obsq2,1),')')
print(xtable(meanres[,.(Scenario,bmidst,obsdst)],type='latex'),include.rownames=F)


results$obs <- NULL
longresults <- data.table::melt(setDT(results),id.vars=c('Scenario','bmi'))
qres <- setDT(longresults)[,.(mn=mean(value),
                              q1=quantile(value,0.025),
                              q2=quantile(value,0.975)),by=.(Scenario,variable)]
qres[,print:=paste0(round(mn,1),' (',round(q1,1),', ',round(q2,1),')')]
dcastqres <- dcast(qres,formula = Scenario~variable,value.var = 'print')
print(xtable(dcastqres[match(c("No obesity","2000","2006","2012","Labelling","Double tax","Triple tax","Quadruple tax"),dcastqres$Scenario),],type='latex'),include.rownames=F)

absresults$obs <- NULL
absresults$bmi <- NULL
longresults2 <- data.table::melt(setDT(absresults),id.vars=c('Scenario'))
qres <- setDT(longresults2)[,.(mn=mean(value),
                              q1=quantile(value,0.025),
                              q2=quantile(value,0.975)),by=.(Scenario,variable)]
qres[,print:=paste0(round(mn,1),' (',round(q1,1),', ',round(q2,1),')')]
dcastqres <- dcast(qres,formula = Scenario~variable,value.var = 'print')
print(xtable(dcastqres[match(c("No obesity","2000","2006","2012","Labelling","Double tax","Triple tax","Quadruple tax"),dcastqres$Scenario),],type='latex'),include.rownames=F)

# noobrow <- which(results$V1=='No obesity')

## plot % deaths averted ##########################

resfig <- ggplot(results) + 
  geom_point(aes(x=bmi,y=Total),colour='grey',alpha=.2) +
  geom_point(data=meanres,aes(x=bmi,y=averted),colour='navyblue') +
  theme_bw(base_size = 15) + 
  labs(x=bquote('Population mean BMI,'~kg/m^2),y='Deaths averted, %') + 
  geom_text(data=meanres,aes(x=bmi,y=averted,label=Scenario),colour='navyblue',
            nudge_y=c(2,0,0,0,0,0,0,0,2),nudge_x=c(0,-0.05,-0.05,-0.05,-.05,.05,.05,.05,0),
            hjust=c(1,2,2,2,2,0,0,0,1)/2) + scale_x_continuous(limits=range(results$bmi)+c(-.1,.2))
ggsave(resfig,filename='resultsfig-1.png',height = 6, width=6)

## print relative risks ###################################

rrlist <- list()
for(i in 1:nsamples){
  tab <- melt(read.csv(file.path(inputs_path,paste0('relativerisks/sample',i,'.csv'))))
  tab$Scenario <- rep(meanres$Scenario[-1],each=3*2)
  tab$`Age group` <- rep(c('20 to 64','65 plus'),each=3)
  tab$variable <- NULL
  rrlist[[i]] <- tab
}
rrres <- do.call(rbind,rrlist)
rrdist <- setDT(rrres)[,.(rr=mean(value),
                             rrq1=quantile(value,0.025),
                             rrq2=quantile(value,0.975)),by=.(Scenario,X,`Age group`)]

ggplot(rrdist,aes(x=factor(Scenario,levels=c('No obesity','2000','2006','2012',
                                             'Labelling',
                                             'Quadruple tax','Triple tax','Double tax')))) +
  geom_point(aes(y=rr),colour='navyblue',size=3,shape=18) +
  geom_errorbar(aes(ymin=rrq1,ymax=rrq2),colour='navyblue',linewidth=1.2, width = 0.5) +
  facet_grid( `Age group` ~ factor(X,levels=c('Diagnoses','Hospitalisations','Deaths')),
              scales='free') +
  theme_bw(base_size=15) +
  labs(y='Relative risk, %',x='') +
  theme(axis.text.x = element_text(angle = 45,  hjust=1,vjust=1),
        strip.background = element_blank(),
        strip.placement='outside') -> rrp

ggsave(rrp,filename='rrplot.png')

rrdist$rrdst <- paste0(round(rrdist$rr,1),' (',round(rrdist$rrq1,1),', ',round(rrdist$rrq2,1),')')
rrtab <- reshape2::dcast(rrdist,Scenario+`Age group`~X,value.var = 'rrdst')
rrtab <- rrtab[,c(1,2,4,5,3)]
print(xtable(rrtab,type='latex'),include.rownames=F)

ggplot(subset(rrres,Scenario=='Labelling')) + 
  geom_histogram(aes(x=value,y=..density..),colour='navyblue',fill='navyblue') +
  facet_grid( `Age group` ~ X,scales='free') +
  theme_bw(base_size=15) +
  labs(x='Relative risk',y='Density')

results$sample <- rep(1:nsamples,each=9)
rrres$sample <- rep(1:nsamples,each=3*2*8)

## pairs plots ##########################

rrcast <- dcast(rrres,formula = sample+Scenario~X+`Age group`,value.var = 'value')


library(dplyr)
library(GGally)
allres <- left_join(rrcast,results,by=c('sample','Scenario'))
ggplot(subset(allres,Scenario=='Double tax')) + geom_point(aes(x=bmi,y=Total))
ggplot(subset(allres,Scenario=='Double tax')) + geom_point(aes(x=bmi,y=`Deaths_20 to 64`))
ggplot(subset(allres,Scenario=='Double tax')) + geom_point(aes(y=Total,x=`Deaths_20 to 64`))

reductions_tax <- read.csv(file.path(country_inputs_path,'reductions_tax.csv'))
rowno <- which(reductions_tax$age=='60+'&reductions_tax$weight=='Overweight')
mn <- reductions_tax$calorie[rowno]
std <- reductions_tax$caloriesd[rowno]
allres[,taxred:=qnorm(taxquants[sample,rowno],mn,std),by=sample]
rowno <- which(reductions_tax$age=='20-39'&reductions_tax$weight=='Obesity')
mn <- reductions_tax$calorie[rowno]
std <- reductions_tax$caloriesd[rowno]
allres[,taxred2:=qnorm(taxquants[sample,rowno],mn,std),by=sample]
rowno <- which(reductions_tax$age=='40-59'&reductions_tax$weight=='Obesity')
mn <- reductions_tax$calorie[rowno]
std <- reductions_tax$caloriesd[rowno]
allres[,taxred3:=qnorm(taxquants[sample,rowno],mn,std),by=sample]
pairplot <- subset(allres,Scenario=='Double tax')[,colnames(allres)%in%c('taxred','Deaths_65 plus','65 plus'),with=F]
colnames(pairplot) <- c('Death relative risk','Averted deaths, 65+','Calorie reduction')
ggpairs(pairplot[,c(3,1,2)], axisLabels = "internal",upper=list(continuous='points'),lower=NULL,aes(colour='col')) + 
  theme_bw(base_size=15) + 
  scale_colour_manual(values = c(col='navyblue')) + 
  theme(panel.grid = element_blank()) -> p
ggsave(p,filename='pairs.png')



ggplot(subset(allres,Scenario=='Double tax')) + 
  geom_point(aes(x=obs*100,y=(1-Total/100)*bau_total/130e6*1e5),colour='navyblue') +
  theme_bw(base_size = 15) +
  labs(x='Obesity prevalence (%)',y='Deaths per 100,000 population') -> p
ggsave(p,filename='obsdeaths.png',height=6)


ggplot(subset(allres,Scenario=='Double tax')) + 
  geom_point(aes(x=taxred2,y=taxred3,colour=`Deaths_20 to 64`),alpha=1) +
  theme_bw(base_size = 15) +
  labs(x='Calorie reduction 20-39',y='Calorie reduction 40-59') ->p
ggsave(p,filename='obsdeaths.png',height=6)

ggplot(subset(allres,Scenario=='No obesity')) + 
  geom_point(aes(x=`Diagnoses_20 to 64`,y=`5 to 19`),colour='navyblue') +
  theme_bw(base_size = 15) +
  labs(x='',y='')

ggplot(melt(allres[Scenario=='Double tax',
                   c('Diagnoses_65 plus','Hospitalisations_65 plus','Deaths_65 plus','65 plus')],
            id.vars='65 plus')) + 
  geom_point(aes(y=`65 plus`,x=value/100),colour='navyblue') +
  facet_wrap(~factor(variable,levels=c('Diagnoses_65 plus','Hospitalisations_65 plus','Deaths_65 plus'),labels=c('Infection','Hospitalisation','Death')),ncol=1,scale='free') +
  theme_bw(base_size = 15) + 
  labs(x='Relative risk',y='Deaths averted, %') -> p
ggsave(p,filename='rrdeaths.png',width=3)

ggplot(melt(allres[Scenario=='Double tax',
                   c('Diagnoses_65 plus','Hospitalisations_65 plus','Deaths_65 plus','65 plus')],
            id.vars='65 plus')) + 
  geom_density(aes(x=value/100),colour='navyblue',size=2) +
  facet_wrap(~factor(variable,levels=c('Diagnoses_65 plus','Hospitalisations_65 plus','Deaths_65 plus'),labels=c('Infection','Hospitalisation','Death')),ncol=1,scale='free') +
  theme_bw(base_size = 15) + 
  labs(x='Relative risk',y='Density') -> p
ggsave(p,filename='rrdists.png',width=3)


allres[,diagrr:=dhquants[sample],by=sample]
allres[,deathrr:=hdquants[sample],by=sample]
ggplot(melt(allres[Scenario=='No obesity',
                   c('deathrr','diagrr','taxred','20 to 64')],
            id.vars='20 to 64')) + 
  geom_point(aes(y=`20 to 64`,x=value),colour='navyblue') +
  facet_wrap(~factor(variable,levels=c('deathrr','diagrr','taxred'),labels=c('Mortality RR quantile','Diagnosis RR quantile','Tax reduction')),ncol=3,scale='free') +
  theme_bw(base_size = 15) + 
  labs(x='',y='Deaths averted, %') -> p
ggsave(p,filename='scatter.png',height=4,width=10)



## rr as a function of bmi ###################

library(splines)
library(dplyr)
rrbmi <- left_join(setDT(results)[,.(Scenario,bmi,sample)],dcast(rrres,Scenario+`Age group` + sample ~ X),by=c('Scenario','sample'))
refbmi <- subset(rrbmi,is.na(Deaths))
refbmi[,Deaths:=100]
refbmi[,Diagnoses:=100]
refbmi[,Hospitalisations:=100]
rrbmi2 <- rbind(subset(rrbmi,!is.na(Deaths)&Scenario!='No obesity'),
               refbmi %>% mutate(`Age group`='20 to 64'),
               refbmi %>% mutate(`Age group`='65 plus'))
ggplot(rrbmi2,aes(x=bmi,y=Diagnoses)) +
  geom_point(colour='navyblue') +
  facet_wrap(~`Age group`) +
  geom_smooth(formula=y~ns(x,df=1),method=glm) +
  theme_bw(base_size = 15)

ggplot(rrbmi2,aes(x=bmi,y=Hospitalisations/Diagnoses)) +
  geom_point(colour='navyblue') +
  facet_wrap(~`Age group`) +
  geom_smooth(formula=y~ns(x,df=1),method=glm) +
  theme_bw(base_size = 15)

ggplot(rrbmi2,aes(x=bmi,y=Deaths/Hospitalisations)) +
  geom_point(colour='navyblue') +
  facet_wrap(~`Age group`) +
  geom_smooth(formula=y~ns(x,df=1),method=glm) +
  theme_bw(base_size = 15)

library(brms)

dat <- rrbmi2
colnames(dat)[colnames(dat)=='Age group'] <- 'agegroup'
attr(ns(dat$bmi,df=2), "knots")
attr(ns(dat$bmi,df=2), "Boundary.knots")
range(rrbmi2$bmi)

diagmodel <- glm(Diagnoses/100~-1+bmi:agegroup+agegroup,data=dat,family=gaussian())
summary(diagmodel)
x <- '65 plus'
plot(dat$Diagnoses[dat$agegroup==x]/100,diagmodel$fitted.values[dat$agegroup==x])
sapply(unique(dat$agegroup),function(x)sd(diagmodel$residuals[dat$agegroup==x]))

hospmodel <- glm(Hospitalisations/Diagnoses~-1+bmi:agegroup+agegroup,data=dat,family=gaussian())
summary(hospmodel)
x <- '65 plus'
plot(with(dat,Hospitalisations/Diagnoses)[dat$agegroup==x],hospmodel$fitted.values[dat$agegroup==x])
sapply(unique(dat$agegroup),function(x)sd(hospmodel$residuals[dat$agegroup==x]))

deathmodel <- glm(Deaths/Hospitalisations~-1+bmi:agegroup+agegroup,data=dat,family=gaussian())
summary(deathmodel)
x <- '65 plus'
plot(with(dat,Deaths/Hospitalisations)[dat$agegroup==x],deathmodel$fitted.values[dat$agegroup==x])
sapply(unique(dat$agegroup),function(x)sd(deathmodel$residuals[dat$agegroup==x]))




## pafs and rrs ######################

rrdeath <- setDT(rrres)[X=='Deaths',.(mn=100-mean(value),
                          q1=100-quantile(value,0.025),
                          q2=100-quantile(value,0.975)),by=.(Scenario,`Age group`)]
rrdeath$type <- 'Static'

dyndeath <- setDT(longresults)[variable%in%c('20 to 64','65 plus'),.(mn=mean(value),
                              q1=quantile(value,0.025),
                              q2=quantile(value,0.975)),by=.(Scenario,variable)]
colnames(dyndeath)[2] <- 'Age group'
dyndeath$type <- 'Dynamic'

pafs <- rbind(rrdeath,dyndeath)
setorder(pafs,-mn)
pafs$Scenario <- factor(pafs$Scenario,levels=c('No obesity','2000','2006','2012','Labelling','Double tax','Triple tax','Quadruple tax','Business as usual'))

vals <- c(Static='navyblue',Dynamic='darkorange')
shapevals <- c(Static=16,Dynamic=15)
dodge <- position_dodge(width=0.5)  
ggplot(data=subset(pafs,Scenario!='Business as usual')) + 
  geom_point(aes(x=Scenario,y=mn,colour=type,shape=type),size=3,position=dodge) + 
  geom_errorbar(aes(x=Scenario,ymin=q1, ymax=q2,colour=type), width=.5,size=1,position=dodge) +
  facet_wrap(~`Age group`,nrow=1, scale='free_y') +
  theme_bw(base_size=15) +
  theme(axis.text.x = element_text(angle = 40,  hjust=1,vjust=1),legend.position = 'top') +
  scale_colour_manual(values=vals) +
  scale_shape_manual(values=shapevals) +
  labs(x='',y='% deaths averted',colour='',shape='') -> p
ggsave(p,filename='pafs.png',height=5,width=10)


## voi #####################################

# assemble results
deathresults <- sapply(results[,.(Scenario,Total)] %>% pivot_wider(names_from = 'Scenario', values_from = 'Total',values_fn = list),unlist)

# read variables
diagquants <- read.csv(file.path(store_path,'diagquants.csv'),header=F)[1:nsamples,]
dhquants <- read.csv(file.path(store_path,'dhquants.csv'),header=F)[1:nsamples,]
ddquants <- read.csv(file.path(store_path,'ddquants.csv'),header=F)[1:nsamples,]
hdquants <- read.csv(file.path(store_path,'hdquants.csv'),header=F)[1:nsamples,]
caloriesquants <- read.csv(file.path(store_path,'caloriesquants.csv'),header=F,sep=',')[1:nsamples,]
sodiumquants <- read.csv(file.path(store_path,'sodiumquants.csv'),header=F,sep=',')[1:nsamples,]
taxquants <- read.csv(file.path(store_path,'taxquants.csv'),header=F,sep=',')[1:nsamples,]


sourcemat <- cbind(diagquants,dhquants,ddquants,hdquants)
red_ages <- unique(reductions$age)
redtax_ages <- unique(reductions_tax$age)
sourcelist <- list()
for(i in 1:length(red_ages)){
  sourcelist[[i]] <- caloriesquants[,reductions$age==red_ages[i]]
  sourcelist[[i+length(red_ages)]] <- sodiumquants[,reductions$age==red_ages[i]]
  sourcelist[[i+2*length(red_ages)]] <- taxquants[,reductions_tax$age==redtax_ages[i]]
}
# sourcelist <- list(caloriesquants,sodiumquants,taxquants)
voilist <- list()

for(i in 2:ncol(deathresults)){
  voi <- c()
  y <- deathresults[,i]
  vary <- var(y) 
  for(j in 1:ncol(sourcemat)){
    # model outcome as a function of input(s)
    sourcesj <- sourcemat[,j]
    max_degree <- ifelse(is.vector(sourcesj),1,ncol(sourcesj))
    model <- earth(y ~ sourcesj, degree=min(4,max_degree))  
    # compute evppi as percentage
    voi[j] <- (vary - mean((y - model$fitted) ^ 2)) / vary * 100
  }
  for(j in 1:length(sourcelist)){
    sourcesj <- sourcelist[[j]]
    voi[j+ncol(sourcemat)] <- voi::evppivar(y,sourcesj,pars=colnames(sourcesj))[2]/vary*100
  }
  voilist[[i]] <- voi
  print(voi)
}

voitab <- do.call(rbind,voilist)
colnames(voitab) <- c('Diagnosis',
                      'Hospitalisation given diagnosis',
                      'Death given diagnosis',
                      'Death given hospitalisation',
                      paste0('Labelling on calories (',red_ages,')'),
                      paste0('Labelling on sodium (',red_ages,')'),
                      paste0('Tax on calories (',redtax_ages,')'))
rownames(voitab) <- colnames(deathresults)[-1]

nscen <- nrow(voitab)
nparam <- ncol(voitab)
brks <- c(-1,1,10,30,50,70,101)
discxs <- matrix(cut(as.numeric(voitab),breaks=brks,labels=1:(length(brks)-1)),
                 nrow=nscen,ncol=nparam)
df <- data.frame(x=rep(1:nparam,each=nscen),y=rep(1:nscen,nparam),xs=c(discxs[nscen:1,]))

options(repr.plot.width = 1, repr.plot.height = 0.75)
lbls <- c('<1','1-10','10-30','30-50','50-70','70-100')
p <- ggplot(df, aes(x, y, fill = factor(xs,levels=1:(length(brks)-1),labels=lbls))) +
  geom_tile() +
  scale_fill_manual(values = colorRampPalette(c("midnightblue","royalblue","lightskyblue","white"))(length(brks)-1)) +   
  theme_bw(base_size=12) +                                   # Minimal theme
  labs(title = "") +
  scale_x_continuous(name='',breaks=1:nparam,labels=colnames(voitab),expand=c(0,0))+
  theme(axis.text.x = element_text(angle = 30,  hjust=1,vjust=1)) +
  scale_y_continuous(name='',breaks=nscen:1,labels=rownames(voitab),expand=c(0,0))+
  theme(
    plot.title = element_text(hjust = 1),        
    legend.position="right", legend.spacing.x = unit(0.15, 'cm'),legend.box.spacing = unit(0.0, 'cm')) +               
  guides(fill = guide_legend(#nrow = 1,
                             title.theme = element_text(size=10),title.vjust = .8,title.hjust = 1,#title.position = "left",      
                             #label.position="bottom", 
                             label.hjust = 0, #label.vjust = 0.3, 
                             #label.theme = element_text(size=8,angle = 45),
                             title = 'EVPPI, % ',override.aes=list(colour='black')))
ggsave(p,filename='evppi.png',width=7,height=4)
