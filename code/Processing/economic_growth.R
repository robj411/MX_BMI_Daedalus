
# GVA: maximum values

## Summary

# - Maximum values per sector per quarter are extrapolated from values we have for Q1 2019 to Q4 2019.
# - Growth rates are based on observed growth from years 2012 to 2019, extrapolated beyond 2019.
# - GVA per quarter is extrapolated to GVA per month based on monthly stringency indices.


## Method

### Long-term growth trends ###################################

# From https://www.inegi.org.mx/temas/pib/#Tabulados:



mat <- setDT(gva_to_plot)
# mat$growth_2020 <- 1
# for(i in 2:5) mat$growth_2020[mat$quarter==i+4] <- mat$value[mat$quarter==i+4]/mat$value[mat$quarter==i]
# for(i in 2:3) mat$growth_2020[mat$quarter==i+8] <- mat$value[mat$quarter==i+8]/mat$value[mat$quarter==i+4]


# https://www.inegi.org.mx/temas/pib/#Tabulados from http://en.www.inegi.org.mx/mapasitio/
rev_path <- file.path(country_inputs_path,'tabulados_pibt/PIBT_5.xlsx')
gvapq <- as.data.frame(readxlsx(rev_path))
gvapq <- gvapq[!is.na(gvapq[,1]),]
gvapq <- gvapq[,c(!is.na(gvapq[2,]))&grepl('T|S',gvapq[2,])]
for(i in 3:ncol(gvapq)) if(is.na(gvapq[1,i])) gvapq[1,i] <- gvapq[1,i-1]
colnames(gvapq)[-1] <- paste0(gvapq[1,-1],'-',gvapq[2,-1])
gvapq <- gvapq[-c(1:2),]
usectors <- unique(gvapq[,1])
# "quarterly" values are for one year. to get to quarterly, divide by 4.
new_gvapq <- data.frame(label=usectors) #data.frame(Sector=sector_summary$Sector)
for(i in 2:ncol(gvapq)) new_gvapq[[i]] <- sapply(new_gvapq$label,function(x)
  sum(as.numeric(gvapq[[i]])[grepl(x,gvapq[,1])]))*1/4
new_gvapq$label <- usectors # sapply(usectors,function(x)strsplit(x,',')[[1]][1])

first_year <- as.numeric(strsplit(colnames(gvapq)[2],'-')[[1]][1])
mgva <- setDT(reshape2::melt(new_gvapq,id='label'))
bysector <- mgva[,sum(value),by=.(variable,label)]
setnames(bysector,'V1','obsValue')
bysector[,x:=as.numeric(as.factor(variable))]
bysector[,year:=ceiling(x/4)+first_year-1]
bysector[,quarter:=(x-1)%%4 + 1]
xm <- 76
label_data <- data.frame(label=sapply(usectors,function(x)strsplit(x,',')[[1]][1]),
                         yl=subset(bysector,x==xm+1)$obsValue,
                         yr=subset(bysector,x==max(bysector$x))$obsValue,
                         x=1,row.names = NULL)
label_data$x[as.numeric(factor(label_data$yl))%%2==1] <- 10
label_data3 <- label_data
quarterly_gva_over_time_plot <- ggplot(subset(bysector,x>xm)) + geom_line(aes(x=x,y=log(obsValue),colour=label)) + theme_bw(base_size=15) + 
  scale_x_continuous(breaks=seq(xm+1,120,by=8),labels=seq(xm+4,120,by=8)/4+1992,limits = c(xm-20,max(bysector$x)+40)) +
  theme(legend.position='none',axis.text.x = element_text(angle = -45, vjust = 0.5, hjust=0)) + labs(x='',y='log GVA')+
  geom_text(data=subset(label_data,x==10),aes(x=max(bysector$x),y=log(yr),colour=label,label=label),hjust=0,size=3)+
  geom_text(data=subset(label_data,x==1),aes(x=xm+1,y=log(yl),colour=label,label=label),hjust='right',size=3)



### Extrapolating growth rates #####################################

# To create "counterfactual maximum values", of which the observed GVA was some fraction, we extrapolate growth rates based on recorded growth rates per sector in the years 2012 to 2019.

bysector12 <- subset(bysector,x>xm)
bysector12[,x:=x-xm]
# firstyear <- 2012

# bysector12[,annualdiff:=c(diff(obsValue),NA),by=.(quarter,label)]
# label_data3$yl2 <- subset(bysector12,x==1)$annualdiff

# bysector12[,annualgrowth:=annualdiff/obsValue]

# growthmodel <- glm(annualgrowth~ns(x,df=1)*label+label:as.factor(quarter),data=subset(bysector12,(label!='Mining'|year>2014)&x<(4*(2020-firstyear-1))))
# bysector12[,fade:=x<(4*(2020-firstyear-1)+1)]
# bysector12[,pred:=predict(growthmodel,newdata = bysector12)]
# label_data3$yl4 <- subset(bysector12,x==1)$pred
# label_data3$yl3 <- subset(bysector12,x==1)$annualgrowth
# model_growth_rate_plot <- ggplot(bysector12,aes(x=x,colour=label,group=label))+
#   annotate("rect", xmin = 4*(2020-firstyear-1)+1, xmax = 4*(2020-firstyear-1)+5, ymin = -Inf, ymax = Inf,
#            alpha = .15,fill='hotpink') + geom_line(aes(y=pred)) +
#   annotate("rect", xmin = 4*(2020-firstyear-1)+5, xmax = 4*(2020-firstyear-1)+9, ymin = -Inf, ymax = Inf,
#            alpha = .15,fill='navyblue') + geom_line(aes(y=pred)) +
#   geom_point(aes(y=annualgrowth,pch=factor(fade))) + scale_shape_manual(values=c(1,16)) +
#   theme_bw(base_size=15)+ylab('Annual GVA growth')+
#   scale_x_continuous('',breaks=c(1:(nrow(bysector12)/nrow(label_data3)))[seq(1,(nrow(bysector12)/nrow(label_data3)),by=4)],
#                      limits=c(-26,max(bysector12$x)),
#                      labels=unique(bysector12$year))+
#   theme(axis.text.x = element_text(size=15, angle=-45,hjust=-0),legend.position = "none")+
#   geom_text(data=subset(label_data3,x>0),aes(x=1,y=(yl4),colour=label,label=label),hjust='right')





### Counterfactual quarterly maxima ###############################################

# We use the pre-pandemic growth rate trends to create "maximum counterfactual values" over time for all sectors. We have 40 quarters; the pre-pandemic ones (2012 Q1 to 2019 Q4) are as they are in the data. The next eight (2020 Q1 to 2020 Q4) have "2019 vintage" maximum value equal to their value one year before multiplied by the expected growth per quarter for its parent sector in the years 2012 to 2019 (Figure \@ref(fig:growth)). 

joinsector <- bysector12[year>=2018,.(x,label,quarter,obsValue)]
setnames(joinsector,'label','variable')
setnames(joinsector,'obsValue','value')
joinsector[,quarter:=x-min(x)+1]
joinsector$x <- NULL
mat1 <- joinsector # left_join(mat0,joinsector,by=c('label','quarter'))
# mat1[,counter_growth:=1+pred]
# mat1[quarter>8,counter_growth:=1+pred]#*growth_acceleration]

# mat1[,counter_value:=value]
# for(i in 1:4) mat1$counter_value[mat1$quarter==i+4] <- mat1$value[mat1$quarter==i]*mat1$counter_growth[mat1$quarter==i]
# for(i in 1:8) mat1$counter_value[mat1$quarter==i+8] <- mat1$counter_value[mat1$quarter==i+4]*mat1$counter_growth[mat1$quarter==i+8]


## seas ##########################
library(seasonal)
library(zoo)

seas_obj <- seas_past <- list()

for(sec in sectors){
  GVA_full <- subset(bysector,label==sec)
  ts_tmp <- ts(GVA_full$obsValue[GVA_full$year<2020],start=c(1993,1,1),frequency = 4)  
  seas_past[[sec]]        <- seas(ts_tmp, forecast.save = "forecasts")#, arima.model = "(1 1 1)(1 1 1)")#,transform.function='none')
  seas_past[[sec]]$sector <- sec
  
  ts_tmp <- ts(GVA_full$obsValue,start=c(1993,1,1),frequency = 4)  
  seas_obj[[sec]]        <- seas(ts_tmp, forecast.save = "forecasts")#,transform.function='none')
  seas_obj[[sec]]$sector <- sec
}

suppressWarnings(sectorcounter   <- data.frame())
for(sec_ind in 1:length(seas_obj)){
  
  sec_obj <- seas_obj[[sec_ind]]
  data        <- as_tibble(sec_obj$data)
  data$date   <- index(sec_obj$data)
  data$sector <- sec_obj$sector
  
  # compute quarterly growth of seasonaly adjusted time series from 2015-19
  # trd_grwth <-  ( data %>% filter(date==2019.75) %>% pull(final) / data %>% filter(date==2015) %>% pull(final) ) ^ (1/20) - 1 
  
  sec_past <- seas_past[[sec_ind]]
  pp_forecast0 <- series(sec_past, c('seats.trendfcstdecomp','seats.seriesfcstdecomp','seats.seasonalfcstdecomp'))
  if(is.null(pp_forecast0)) pp_forecast0 <- series(sec_past, c('seats.trendfcstdecomp','seats.seriesfcstdecomp','seats.seasonalfcstdecomp'))
  GVA_full <- subset(bysector,label==sectors[sec_ind])
  if(is.null(pp_forecast0)){
    ts_tmp <- ts(GVA_full$obsValue[GVA_full$year<2020],start=c(1993,1,1),frequency = 4)  
    sec_past        <- seas(ts_tmp, forecast.save = "forecasts", arima.model = "(1 1 1)(1 1 1)")#,transform.function='none')
    sec_past$sector <- sectors[sec_ind]
    pp_forecast0 <- series(sec_past, c('seats.trendfcstdecomp','seats.seriesfcstdecomp','seats.seasonalfcstdecomp'))
  }
  
  if(summary(sec_past)$transform.function=='none'){
    pp_forecast <- data.frame(pptrend = c(pp_forecast0[,'tfd']),counter_value_2020 = c(pp_forecast0[,'ofd']))
  }else{
    pp_forecast <- data.frame(pptrend = c(pp_forecast0[,'tfd']),counter_value_2020 = c(pp_forecast0[,'ofd']))
  }
  pp_forecast$date <- seq(2020,2022.75,by=0.25)
  data <- left_join(data,pp_forecast,by='date')
  data$counter_value_2020[is.na(data$counter_value_2020)] <- GVA_full$obsValue[is.na(data$pptrend)]
  data$pptrend[is.na(data$pptrend)] <- data$trend[is.na(data$pptrend)]
  data$trd_grwth <- with(data,c(NA,trend[2:length(trend)]/trend[2:length(trend)-1])) - 1
  trd_grwth <-  with(subset(data, date<=2019.75& date>=2014.75), prod(trd_grwth + 1,na.rm=T) ^ (1/(length(trend)-1))) - 1 
  data$pptrd_grwth <- with(data,c(NA,pptrend[2:length(pptrend)]/pptrend[2:length(pptrend)-1])) - 1
  
  data$growth <- trd_grwth
  if(summary(sec_obj)$transform.function=='log'){
    data$trendandseason <- data$trend * data$seasonal
    data$season_scalar <- data$seasonal
  }else{
    data$trendandseason <- data$trend + data$seasonal
    data$season_scalar <- data$trendandseason / data$trend
  }
  data$past_seasonality <- sapply(data$date-floor(data$date),function(x)mean(subset(data,date>=2014.75&date<2020&date-floor(date)==x)$season_scalar))
  data$ppseasonal <- with(data,counter_value_2020/pptrend)
  
  data$counterfactual                 <- NA
  data$counterfactual[data$date<2020] <- data$final[data$date<2020]
  
  tmp     <- data$counterfactual
  final_q <- tail(na.omit(tmp),1)
  len_na  <- sum(is.na(tmp))
  data$counterfactual[is.na(data$counterfactual)] <- final_q*(1+trd_grwth)^seq(1,len_na)
  
  savedata <- subset(data, date>2017.75)
  sectorcounter <- bind_rows(sectorcounter,savedata)
  
}

ggplot(sectorcounter) + geom_line(aes(x=date,y=counter_value_2020,colour=sector)) + 
  theme_bw(base_size=15) + theme(legend.position = 'none') + 
  scale_x_continuous(breaks=seq(2018,2022.5,by=0.5),labels=unlist(lapply(2018:2022,function(x)sapply(c(1,3),function(y)paste0(x,' Q',y)))))

#########################################

mat1[,date:=2018+quarter/4-0.25]
mat2 <- mat1[setDT(sectorcounter[,c('date','counter_value_2020','sector')]),on=c("date","variable"="sector")]

mat2[,shortfall:=1-(counter_value_2020-value)/counter_value_2020]

label_data$yr2 <- subset(mat2,quarter==16)$counter_value_2020
label_data$yl <- subset(mat2,quarter==1)$counter_value_2020
label_data$shortlabel <- sapply(label_data$label,function(x)strsplit(x,', |; | and | with ')[[1]][1])
counterfactual_plot <- ggplot(mat2,aes(x=quarter,y=log(counter_value_2020),colour=variable))+
  annotate("rect", xmin = 8, xmax = 9, ymin = 9.8, ymax = 14.2,
           alpha = .25,fill='grey')+
  scale_y_continuous('GVA, log pesos',limits=c(9.8,14.2),expand=c(0,0))+
  geom_line()+ 
  theme_bw(base_size=15)+
  scale_x_continuous('',breaks=seq(1,(nrow(mat2)/nrow(label_data)),by=2), 
                     labels=unlist(lapply(2019:2022,function(x)paste0(paste0('Q',c(1,3)),' ',x))),limits=c(-16,28))+
  theme(axis.text.x = element_text(size=12, angle=55,hjust=1,vjust=1),legend.position = "none")+
  geom_text(data=subset(label_data,x==10),aes(x=16,y=log(yr2),colour=label,label=shortlabel),hjust='left')+
  geom_text(data=subset(label_data,x==1),aes(x=x,y=log(yl),colour=label,label=shortlabel),hjust='right')
ggsave(counterfactual_plot,filename = 'reports/countergvapq.png')

### Application to 20-sector GVA data #######################################

# Our processed values for GVA per quarter by sector from the National Accounts are shown in Figure \@ref(fig:gva):


gva_per_quarter_plot <- ggplot(gva_to_plot,aes(x=quarter,y=log(value/4),colour=variable))+geom_line()+
  theme_bw(base_size=15)+ylab('log GVA')+
  scale_x_continuous('',breaks=1:length(unique(gva_to_plot$quarter)),
                     labels=unlist(lapply(2018:2021,function(x)paste0(paste0('Q',1:4),' ',x))),limits=c(-8,22))+
  theme(axis.text.x = element_text(size=12, angle=-55,hjust=-0),legend.position = "none")+
  geom_text(data=subset(label_data,x==10),aes(x=16,y=log(yr),colour=label,label=label),hjust=0)+
  geom_text(data=subset(label_data,x==1),aes(x=x,y=log(yl),colour=label,label=label),hjust='right')

label_datax <- label_data
label_datax$yr <- subset(mat2,quarter==16)$shortfall
quarterly_x_with_growth_plot <- ggplot(subset(mat2,quarter>4),aes(x=quarter,y=(shortfall),colour=variable)) +
  geom_hline(yintercept=1, color='grey', size=1)+geom_line()+ 
  theme_bw(base_size=15)+ylab('x')+
  scale_x_continuous('',breaks=1:(nrow(mat2)/nrow(label_datax)), 
                     labels=unlist(lapply(2018:2021,function(x)paste0(paste0('Q',1:4),' ',x))),limits=4+c(1,17))+
  theme(axis.text.x = element_text(size=14, angle=-55,hjust=-0),legend.position = "none")+
  geom_text(data=label_datax,aes(x=16,y=(yr),colour=label,label=label),hjust=0)#+

### Monthly x values ###########################################################

# Monthly x values are computed so that their loss relative to the counterfactual is linearly related to the stringency index for the month, and the losses for the three months in each quarter must add up to the loss for the quarter.

nMonths <- 24
index_file <- file.path(country_inputs_path,'index.xlsx')
y2q <-   readxlsx(index_file,sheet=2)
y2q$date <- as.Date(as.character(y2q$date),format='%Y%m%d')
y2q$quarter <- (year(y2q$date)-2020)*4 + ceiling(month(y2q$date)/3)
all_dates <- seq.Date(from=as.Date('2020-01-01'),to=as.Date('2022-12-01'),by='month')
missing_dates <- all_dates[!all_dates%in%y2q$date]
if(length(missing_dates)==0){
  y2qaug <- y2q
}else{
  y2qaug <- rbind(y2q, data.frame(date=missing_dates,economicclosures=0,averagebyquarter=0,
                             percentvariationaroundQaverage=0,percentdifferenceinstringencycomparedtoQvalue=0,
                             quarter=subset(y2q,date%in%(missing_dates-years(1)))$quarter+4))
}
mat4 <- subset(mat2,quarter>8)
mat4[,date:=NULL]
mat4[,shortfall:=NULL]
mat4[,value:=NULL]
mat4[,quarter:=quarter-8]
# triplicate
setDT(gva_pm_melt)
gva_pm_melt[,quarter:=ceiling(month/3)]
xindex <- mat4[setDT(subset(gva_pm_melt,quarter<=max(mat4$quarter))),,on=c('quarter','variable'),allow.cartesian=T]

# xindex <- mat4[setDT(subset(y2qaug,quarter<=max(mat4$quarter))),,on='quarter',allow.cartesian=T]
# 
# xindex[,sumclosure:=sum(economicclosures),by=.(quarter,variable)]
# xindex[,qloss:=counter_value_2020-value]
# xindex[,zsumclosure:=sum(sumclosure-economicclosures),by=.(quarter,variable)]
# xindex[,mloss:=qloss*economicclosures/sumclosure]
# xindex[qloss<0,mloss:=qloss*(sumclosure-economicclosures)/zsumclosure]
xindex[,mmax:=counter_value_2020/3]
# xindex[,mvalue:=value]
xindex[,mshortfall:=value/mmax]
x_mat <- matrix(0,nrow=nSectors,ncol=nMonths)
nMaxMonths <- max(xindex$quarter)*3
gva_max <- matrix(0,nrow=nSectors,ncol=nMaxMonths)
sectors <- unique(xindex$variable)

for(i in 1:nSectors){
  gva_max[i,] <- subset(xindex,variable==sectors[i])$mmax
  x_mat[i,] <- subset(xindex,variable==sectors[i])$mshortfall[1:nMonths]
  # x_mat[i,] <- gva_max[i,] - gva_pm[[sectors[i]]]
}
newxs <- x_mat
write.csv(x_mat,file.path(out_path,'x_values_growth.csv'),row.names=F)
write.csv(gva_max,file.path(out_path,'gva_max.csv'),row.names=F)

x_mat <- data.frame(x_mat)
x_mat$sector <- label_datax$label
mat3 <- reshape2::melt(x_mat,id.vars='sector')
label_datax$yr <- subset(mat3,variable=='X24')$value
monthly_x_plot <- ggplot(mat3,aes(x=as.numeric(variable),y=(value),colour=sector)) +
  geom_hline(yintercept=1, color='grey', size=1)+geom_line()+ 
  theme_bw(base_size=15)+ylab('x')+
  scale_x_continuous('',breaks=1:24, 
                     labels=format(as.Date(unlist(lapply(2020:2021,function(x)paste0(x,'-',1:12,'-',1)))),'%b %y'),limits=c(1,30))+
  theme(axis.text.x = element_text(size=14, angle=-55,hjust=-0),legend.position = "none")+
  geom_text(data=label_datax,aes(x=24,y=(yr),colour=label,label=label),hjust=0)

# View(subset(xindex,variable=='Real estate services and tangible and intangible goods rental and leasing'))

write.csv(newxs,file.path(out_path,'mandate.csv'),row.names=F)
discxs <- matrix(cut(newxs*100,breaks=c(-1,25,50,75,90,95,100-1e-6,200),labels=1:7),
  nrow=nSectors,ncol=nMonths)
df <- data.frame(x=rep(1:nMonths,each=nSectors),y=rep(1:nSectors,nMonths),xs=c(discxs[nSectors:1,]))

options(repr.plot.width = 1, repr.plot.height = 0.75)
monthly_x_heat_plot <- ggplot(df, aes(x, y, fill = factor(xs,levels=1:7,labels=c('0-25','25-50','50-75','75-90','90-95','95-100','100')))) +
  geom_tile() +
  scale_fill_manual(values = colorRampPalette(c("black","red","yellow","white"))(7)) +   
  theme_bw(base_size=9) +                                   # Minimal theme
  labs(title = "") +
  scale_x_continuous(name='',breaks=1:nMonths,labels=format(as.Date(unlist(lapply(2020:2021,function(x)paste0(x,'-',1:12,'-',1)))),'%b %y'),expand=c(0,0))+
  theme(axis.text.x = element_text(angle = -45, vjust = 0.5, hjust=0)) +
  scale_y_continuous(name='',breaks=nSectors:1,labels=label_datax$label,expand=c(0,0))+
  theme(
    plot.title = element_text(hjust = 1),        
    legend.position="bottom", legend.spacing.x = unit(00, 'cm'),legend.box.spacing = unit(0.0, 'cm')) +               
  guides(fill = guide_legend(nrow = 1,
    title.theme = element_text(size=10),title.vjust = .8,title.hjust = 8,title.position = "left",      
    label.position="bottom", label.hjust = 0, label.vjust = 0.3, 
    label.theme = element_text(size=8,angle = -45),
    title = 