
# dl <- read.csv('dates_and_locations.tsv',sep='\t',stringsAsFactors=F)
md <- read.csv(gisaid_file,sep='\t',stringsAsFactors=F)
# vs <- read.csv('variant_surveillance.tsv',sep='\t',stringsAsFactors=F)

weektodate <- function(x){
  format(ymd('2019-12-28')+7*x,'%d-%b-%Y')
}

epiwrap <- function(x){
  out <- c()
  for(i in 1:length(x)) out[i] <- ceiling(difftime(x[i],'2019-12-28',units='weeks')[[1]])
  out
}

vsdt <- as.data.table(md)
country_string <- paste0('/ ',country,' | / ',country,'$')
vsdt[,country_flag:=grepl(country_string,Location),by=Location]
vsdt[country_flag==T,]
unique(vsdt[country_flag==T,Location])
vsdti <- vsdt[country_flag==T,]

vsdti[,epiweek:=epiwrap(ymd(Collection.date)),by=Collection.date]


vsdti[,Lineage:=Pango.lineage]
vsdti[grepl('BA\\.|B\\.1\\.1\\.529|B\\.1\\.1\\.519|B\\.1\\.617\\.2|AY\\.',Pango.lineage)==F,Lineage:='Other']
vsdti[grepl('B\\.1\\.617\\.2|AY\\.',Pango.lineage)==T,Lineage:='B.1.617.2 / AY.']
vsdti[grepl('B\\.1\\.1\\.529|BA\\.',Pango.lineage)==T,Lineage:='B.1.1.529 / BA.']
#vsdti[grepl('P\\.1',Pango.lineage)==T,Lineage:='P.1']
sort(with(vsdti,table(Lineage)))
sort(with(subset(vsdti,Lineage=='Other'&Submission.date>'2022-01-01'),table(Pango.lineage)))
vsdti[,region:=str_trim(strsplit(Location,'/')[[1]][3]),by=Location]
vsdti[region=='Chandigarh',region:='Chhattisgarh']
vsdti[region=='Harayana',region:='Haryana']
vsdti[region=='Maharastra',region:='Maharashtra']

saveRDS(vsdti,file.path(country_inputs_path,'lineages.Rds'))

xtabr <- as.data.table(reshape2::melt(xtabs(data=vsdti,~epiweek+Lineage+region)))
xtabr[,total:=sum(value),by=.(epiweek,region)]
p <- ggplot(subset(xtabr,value>0&epiweek>60),aes(x=epiweek,y=value/total,colour=Lineage)) + geom_point() +
  theme_bw(base_size=15) + facet_wrap(~region) + theme(axis.text.x = element_text(angle = 90)) + labs(x='',y='Proportion') +
  scale_x_continuous(breaks = c(62,65,68), labels = format(ymd('2019-12-28')+7*c(62,65,68),'%d-%b'))
p
# ggsave(filename='lineagebystateprop.png',p,width=10,height=10)
p <- ggplot(subset(xtabr,value>0&epiweek>60),aes(x=epiweek,y=value,colour=Lineage)) + geom_point() +
  theme_bw(base_size=15) + facet_wrap(~region) + theme(axis.text.x = element_text(angle = 90)) + labs(x='',y='Count') +
  scale_x_continuous(breaks = c(62,75,88), labels = format(ymd('2019-12-28')+7*c(62,75,88),'%d-%b'))
p
# ggsave(filename='lineagebystate.png',p,width=10,height=10)


xtab <- as.data.table(reshape2::melt(xtabs(data=vsdti,~epiweek+Lineage)))
xtab[,total:=sum(value),by=.(epiweek)]
write.csv(xtab,lineage_file,row.names = F)
daterange <- seq(60,105,by=3)
p <- ggplot(subset(xtab,value>0&epiweek>40),aes(x=as.Date(weektodate(epiweek),format='%d-%b-%Y'),y=value/total,colour=Lineage)) + geom_point(size=3) +
  theme_bw(base_size=15) + theme(legend.position='top',axis.text.x = element_text(angle = -45,hjust=0)) + labs(x='',y='Proportion') +
  scale_x_date(date_breaks ='months', date_labels ='%b %y')
p
ggsave(filename='reports/lineageprop.png',p,width=8,height=5)
p <- ggplot(subset(xtab,value>0&epiweek>60),aes(x=epiweek,y=value,colour=Lineage)) + geom_point() +
  theme_bw(base_size=15) + theme(axis.text.x = element_text(angle = 90)) + labs(x='',y='Count') +
  scale_x_continuous(breaks =daterange, labels = format(ymd('2019-12-28')+7*daterange,'%d-%b'))
p
# ggsave(filename='lineage.png',p,width=10,height=10)


