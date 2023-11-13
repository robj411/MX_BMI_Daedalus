suppressMessages({
  require(pwt10,quietly=T) ##NB should be loaded before OECD
  require(data.table,quietly=T)
  require(dplyr,quietly=T)
  require(lubridate,quietly=T)
  require(OECD,quietly=T)
  require(readODS,quietly=T)
  require(readxl,quietly=T)
  require(xlsx,quietly=T)
  require(stringi,quietly=T)
  require(tidyverse,quietly=T)
  require(splines,quietly=T)
})

## 0 variables ####################################

country <- 'Mexico'
country_abbr <- 'MEX'
country_abbr_2 <- 'MX'
out_path <- 'matlab_inputs'
multi_country_inputs_path <- '../data'
country_inputs_path <- 'country_data'
template_path <- '.'
contact_path <- file.path(multi_country_inputs_path,'synthetic-contact-matrices/generate_synthetic_matrices/output/syntheticcontactmatrices2020/overall/')
processing_code_path <- 'code/Processing'

nSectors <- 20

args = commandArgs(trailingOnly=TRUE)
if(length(args)>0){
  country <- args[1]
  country_abbr <- args[2]
  print(args)
}

epiwrap <- function(x){
  out <- c()
  for(i in 1:length(x)) out[i] <- ceiling(difftime(x[i],'2019-12-28',units='weeks')[[1]])
  out
}

get_month_day <- function(x){
  out <- c()
  for(i in 1:length(x)) out[i] <- ceiling(difftime(paste0(x[i],'-01'),'2019-12-31',units='days')[[1]])
  out
}
get_day_from_jan1 <- function(x){
  out <- c()
  for(i in 1:length(x)) out[i] <- ceiling(difftime(x[i],'2019-12-31',units='days')[[1]])
  out
} 

get_day_from_week <- function(x){
  as.Date('2019-12-31') + weeks(x)
}

readxlsx <- function(...) suppressMessages(read_xlsx(...))

population_source_data <- file.path(multi_country_inputs_path,'Population.xlsx')
# pop_sheet1 <- as.data.frame(readxlsx(population_source_data,sheet=1),stringsAsFactors=F)
pop_sheet2 <- as.data.frame(readxlsx(population_source_data,sheet=2))

owid <- read.csv('https://covid.ourworldindata.org/data/owid-covid-data.csv',stringsAsFactors = F)
owidc <- subset(owid,iso_code==country_abbr)
owidi <- subset(owid,iso_code=='IDN')
rm(owid)

## 1 GVA pm (obj) ############################################
cat('GVA pm\n')

# http://en.www.inegi.org.mx/programas/mip/2013/

# io <- read_ods('IO_IDN.ods')
# search_dataset('input')
# struct <- OECD::get_data_structure('IOTSI4_2018')
# # dataset is matrix in long form (as the csv extracted from the OECD online tool)
# io_long <- OECD::get_dataset('IOTSI4_2018',filter=list('TTL',country_abbr),start_time=2015,end_time=2015)
# # retain only row label, col label, and value
# io_in <- io_long[,colnames(io_long)%in%c('ROW','COL','obsValue')]
# # store correct orders
# roworder <- unique(io_in$ROW)
# colorder <- unique(io_in$COL)
# # reshape to matrix
# disordered_mat <- reshape2::dcast(io_in,ROW~COL,value.var='obsValue')
# # reorder matrix
# ordered_io_mat <- disordered_mat[match(roworder,disordered_mat$ROW),match(colorder,colnames(disordered_mat))]
# # name using code-to-description map
# struct$ROW$label <- gsub('ccomodation','ccommodation',struct$ROW$label)
# struct$COL$label <- gsub('ccomodation','ccommodation',struct$COL$label)
# rownames(ordered_io_mat) <- struct$ROW$label[match(roworder,struct$ROW$id)]
# colnames(ordered_io_mat) <- struct$COL$label[match(colorder,struct$COL$id)]

io <- readxlsx(file.path(country_inputs_path,'MIP_4.xlsx'),sheet=1)
first_numeric_row <- 6 #which(sapply(io[,1],function(x)grepl('Agriculture, forestry and fishing',x)))
value_added_row <- which(sapply(io[,1],function(x)grepl('B.1bP - Producto interno bruto',x)))
first_numeric_col <- 4 #which(sapply(io[first_numeric_row-2,],function(x)grepl('Agriculture, forestry and fishing',x)))
numeric_cols <- first_numeric_col + 1:nSectors - 1
# data.obj is value-added row divided by 12
gva <- as.numeric(io[value_added_row,numeric_cols])

x <- readxlsx(file.path(country_inputs_path,'ECON.xlsx'),sheet=3)
sectors <- unname(sapply(x$`Source: Instituto Nacional de Estadística y Geografía (INEGI).`[6:25],
                  function(x)strsplit(x,' - ')[[1]][2]))
edSector <- which(grepl('education',sectors,ignore.case=T))
healthSector <- which(grepl('health',sectors,ignore.case=T))
conSector <- as.numeric(paste(as.numeric(grepl('accommodation|beverage|recreation',sectors,ignore.case=T)),collapse=''))

# io <- apply(x[6:25,4:23],2,function(xx)as.numeric(xx) %>% replace(is.na(.), 0))*1  
# obj <- as.numeric(unlist(x[6:25,2]))/12
# colnames(io) <- sectors
# rownames(io) <- sectors

## 2 IO ##############################################
cat('IO\n')


# find rows/columns corresponding to output and changes
numeric_rows <- 1:nSectors + first_numeric_row - 1
output_row <- which(sapply(io[,1],function(x)grepl('__eP.1 - Producción',x)))
total_output <- as.numeric(io[output_row,numeric_cols])
external_cols <- which(grepl('Consumo|Formación|Variación|Exportaciones',(io[first_numeric_row - 1,])))
ext_io <- io[numeric_rows,external_cols]
for(i in 1:ncol(ext_io)) {ext_io[,i] <- as.numeric(unlist(ext_io[,i])); ext_io[is.na(ext_io[,i]),i] <- 0}
demand <- apply(ext_io,1,sum)
private_consumption <- as.numeric(unlist(io[numeric_rows,which(grepl('Consumo Privado',(io[first_numeric_row - 1,])))]))
private_consumption[is.na(private_consumption)] <- 0
consumption_frac_of_final_demand <- private_consumption / demand
consumption_frac_of_final_demand[is.na(consumption_frac_of_final_demand)] <- 0
imp_col <- which(grepl('Importaciones',(io[first_numeric_row - 1,])))
imports <- - as.numeric(unlist(io[numeric_rows,imp_col]))
imports[is.na(imports)] <- 0
io_mat <- matrix(apply(io[numeric_rows,numeric_cols],2,function(x)as.numeric(x)*1  ),
                 nrow=length(numeric_rows),ncol=length(numeric_rows),byrow=F)
for(i in 1:ncol(io_mat)) {io_mat[is.na(io_mat[,i]),i] <- 0;  }


# prepare matrices to subtract
total_mat <- matrix(0,length(numeric_rows),length(numeric_rows))
diag(total_mat) <- total_output

# data.G is IO matrix - diag(output - changes)
for(i in 1:ncol(io_mat)) {io_mat[,i] <- as.numeric(unlist(io_mat[,i])); io_mat[is.na(io_mat[,i]),i] <- 0}
old_io_mat <- io_mat/12 # (io_mat - total_mat) / 12



## 3 population ##############################
cat('population\n')

# http://ghdx.healthdata.org/record/ihme-data/gbd-2019-population-estimates-1950-2019

# popstruct <- OECD::get_data_structure('HISTPOP')
# agepops <- popstruct$AGE
# colnames(agepops)[1] <- 'AGE'
# agepops$label2 <- agepops$label
# agepops$label2[agepops$label=='85 and over'] <- '85 to 120'
# age_groups <- agepops$label2[2:19]
# print(age_groups)
age_groups <- c('0 to 4','5 to 9','10 to 14','15 to 19','20 to 24','25 to 29','30 to 34','35 to 39','40 to 44',
                '45 to 49','50 to 54','55 to 59','60 to 64','65 to 69','70 to 74','75 to 79','80 to 84','85 to 120')
# if(country_abbr%in%popstruct$LOCATION$id){
#   # dataset is matrix in long form (as the csv extracted from the OECD online tool)
#   hp <- OECD::get_dataset('HISTPOP',filter=list(country_abbr,'T'),start_time=2015,end_time=2015)
#   hp <- left_join(hp,agepops,by='AGE')
#   population <- hp$obsValue[match(age_groups,hp$label2)]
# }else{
  pop_file <- read.csv(file.path(multi_country_inputs_path,'IHME_GBD_2019_POP_2019_Y2020M10D15.csv'),stringsAs=F)
  country_pop <- subset(pop_file,location_name==country&sex_name=='both')
  if(!'0 to 4'%in%country_pop$age_group_name)
    country_pop$age_group_name[country_pop$age_group_name=='Under 5'] <- '0 to 4'
  if(!'85 to 120'%in%country_pop$age_group_name)
    country_pop$age_group_name[country_pop$age_group_name=='85 plus'] <- '85 to 120'
  population <- country_pop$val[match(age_groups,country_pop$age_group_name)]
# }

age_range <- sapply(age_groups,function(x){
  as.numeric(strsplit(x,' to ')[[1]])
  })

new_age_groups <- c('0 to 4','5 to 19','20 to 64','65 to 120')
# new_age_groups <- c('0 to 4','5 to 19','20 to 64','65 to 120')
new_age_range <- sapply(new_age_groups,function(x){
  as.numeric(strsplit(x,' to ')[[1]])
})

map_17_to_4_ages <- c()
for(i in 1:length(age_groups)) 
  map_17_to_4_ages[i] <- which(age_range[1,i]>=new_age_range[1,]&age_range[2,i]<=new_age_range[2,])

population_size <- c()
for(i in 1:length(new_age_groups))
  population_size[i] <- sum(as.numeric(population[map_17_to_4_ages==i]))

pop_by_age <- data.frame(Age_group=age_groups,Population=as.numeric(unlist(population)))
write.table(pop_by_age,file.path(out_path,'population_by_age.csv'),col.names=T,row.names=F,sep=',')


## 4 population by sector ##############################
cat('population by sector\n')


if(country_abbr=='MEX'){
  secpopsheet <- readxlsx(file.path(country_inputs_path,'ECON.xlsx'),sheet=5)
  workforce <- as.numeric(unname(unlist(secpopsheet[10+1:nSectors,2])))
  sec_names <- sectors
}else if(country_abbr!='PHL'){
  popstruct <- OECD::get_data_structure('TIM_2019_MAIN')
  industries <- popstruct$IND
  colnames(industries)[1] <- 'IND'
  # dataset is matrix in long form (as the csv extracted from the OECD online tool)
  tim <- OECD::get_dataset('TIM_2019_MAIN',filter=list('EMPN',country_abbr),start_time=2015,end_time=2015)
  tim <- left_join(tim,industries,by='IND')
  mapping <- match(colnames(old_io_mat),tim$label)
  workforce <- tim$obsValue[mapping]*10^as.numeric(tim$POWERCODE[mapping])
  sec_names <- colnames(old_io_mat)
}else{
  pop_by_sector <- read_xls(file.path(country_inputs_path,'TABLE 4   Percent Distribution of  Employed Persons by Major Industry Group APR 2019 and APR 2020.xls'))
  total_number <- as.numeric(pop_by_sector[6,3])
  percentages <- unlist(pop_by_sector[12:36,3])
  clean_perc <- as.numeric(percentages[-c(3,4,8,11,12)])
  workforce <- clean_perc*total_number/100
  workplace_contact_filename <- file.path(multi_country_inputs_path,'Workplace Contact Rates.xlsx')
  sec_names <- as.data.frame(readxlsx(workplace_contact_filename,sheet='36to21',col_names = F))[1:20,3]
}

not_working <- population_size[3] - sum(workforce)
community_populations <- population_size
community_populations[3] <- not_working
community_populations <- c(community_populations, not_working/population_size[3], population_size[3])
community_names <- c(new_age_groups,'Inactive/Pop',new_age_groups[3])
community_names[3] <- 'Inactive'
pop_by_sector <- data.frame(sector=c(sec_names,community_names),
                            number=c(workforce,community_populations))

write.csv(pop_by_sector,file.path(out_path,'population_by_sector.csv'),row.names = F)

## 5 B and C values #####################
cat('B and C values\n')


workplace_contact_filename <- file.path(multi_country_inputs_path,'Workplace Contact Rates.xlsx')
sector_map_filename <- file.path(multi_country_inputs_path,'sector_map.ods')

sec_sheet0 <- as.data.table(read_ods(sector_map_filename))
sec_sheet1 <- unique(sec_sheet0[,.(x64,x20NAICS)])
map_64_to_unique <- as.numeric(factor(sec_sheet1$x20NAICS,levels=sectors))
map_unique_to_sectors <- 1:nSectors

unique_sectors <- sort(unique(as.numeric(map_64_to_unique)))

B_input <- as.data.frame(readxlsx(workplace_contact_filename,sheet='B36',col_names = F))
uk_population <- B_input[,5]
uk_population[is.na(uk_population)] <- 0
C_values <- as.data.frame(readxlsx(workplace_contact_filename,sheet='C36',col_names = F))[,1]
B_values <- B_input[,1]

unique_Bs <- sapply(unique_sectors,function(x){
  ind <- map_64_to_unique==x
  sum(B_values[ind]*uk_population[ind])/sum(uk_population[ind])
  })
B_sector <- unique_Bs[map_unique_to_sectors[1:nSectors]]

unique_Cs <- sapply(unique_sectors,function(x){
  ind <- map_64_to_unique==x
  sum(C_values[ind]*uk_population[ind])/sum(uk_population[ind])
})
C_sector <- unique_Cs[map_unique_to_sectors[1:nSectors]]



## 6 WFH ############################################
cat('WFH\n')


# sec_sheet3 <- as.data.frame(readxlsx(workplace_contact_filename,sheet='36to35',col_names = F))
# colnames(sec_sheet3) <- c('X36_sectors','map_unique_to_36','X35_sectors','map_35_to_unique')

wfh_values <- as.numeric(as.data.frame(readxlsx(file.path(multi_country_inputs_path,'WFH.xlsx'),sheet='35 Sectors',col_names = F))[1:64,1])

unique_wfhs <- sapply(unique_sectors,function(x){
  ind <- map_64_to_unique==x
  sum(wfh_values[ind]*uk_population[ind])/sum(uk_population[ind])
})
wfh_uk <- unique_wfhs[map_unique_to_sectors[1:nSectors]]

# map to country value
wfh_by_country <- read.csv(file.path(multi_country_inputs_path,'WFH.csv'),stringsAs=F)
wfh_rural <- subset(wfh_by_country,country==country_abbr&ind_sex=='all'&ind_age=='all'&
                      ind_educ=='all'&ind_work=='all'&urban=='rural')$wfh_share
wfh_urban <- subset(wfh_by_country,country==country_abbr&ind_sex=='all'&ind_age=='all'&
                      ind_educ=='all'&ind_work=='all'&urban=='urban')$wfh_share

rural_pop_average <- sum(wfh_uk[1:2]*workforce[1:2])/sum(workforce[1:2])
urban_pop_average <- sum(wfh_uk[-c(1:2)]*workforce[-c(1:2)])/sum(workforce[-c(1:2)])

wfh <- wfh_uk
wfh[1:2] <- wfh[1:2]/rural_pop_average*wfh_rural
wfh[-c(1:2)] <- wfh[-c(1:2)]/urban_pop_average*wfh_urban


## 7 contact matrices ################################
cat('contact matrices\n')

# identify age mappings
age1 <- which(map_17_to_4_ages==1)
age2 <- which(map_17_to_4_ages==2)
age3 <- which(map_17_to_4_ages==3)
age4 <- which(map_17_to_4_ages[1:16]==4)
listages <- list(age1,age2,age3,age4)

# file locations
all_locations_sheets <- file.path(contact_path,'contact_all.rdata')
work_sheets <- file.path(contact_path,'contact_work_conditional.rdata')
consumer_sheets <- file.path(contact_path,'contact_consumer.rdata')
school_sheets <- file.path(contact_path,'contact_school.rdata')
school_mix_sheets <- file.path(contact_path,'contact_school_mix.rdata')
home_sheets <- file.path(contact_path,'contact_home.rdata')
other_sheets <- file.path(contact_path,'contact_others.rdata')

# resize population by age to length 16
pop <- c(pop_by_age$Population[1:15], sum(pop_by_age$Population[16:17]))

# get community matrix from all_locations
all_locations <- get(load(all_locations_sheets))
country_all_mat <- all_locations[[country_abbr]]
country_all_mat <- sapply(listages,function(x) apply(as.matrix(country_all_mat[,x]),1,sum))
# map to four ages
country_all_mat <- sapply(1:4,function(x) 
  sapply(listages,function(y)
    sum(pop[y]*country_all_mat[y,x])/sum(pop[y])))
# get average
Cav <- sum(rowSums(country_all_mat) * population_size) / sum(population_size)

# read in each matrix
country_school_mat <- get(load(school_sheets))[[country_abbr]]#read.xlsx(school_sheets[country_sheet],sheetName=country,header = country_sheet==1)
country_school_mix_mat <- get(load(school_mix_sheets))[[country_abbr]]#read.xlsx(school_sheets[country_sheet],sheetName=country,header = country_sheet==1)
country_home_mat <- get(load(home_sheets))[[country_abbr]]#read.xlsx(home_sheets[country_sheet],sheetName=country,header = country_sheet==1)
country_other_mat <- get(load(other_sheets))[[country_abbr]]#read.xlsx(other_sheets[country_sheet],sheetName=country,header = country_sheet==1)
country_work_mat <- get(load(work_sheets))[[country_abbr]]#read.xlsx(other_sheets[country_sheet],sheetName=country,header = country_sheet==1)
country_consumer_mat <- get(load(consumer_sheets))[[country_abbr]]#read.xlsx(other_sheets[country_sheet],sheetName=country,header = country_sheet==1)

new_params <- list()

# weighted average using only relevant age groups
average_contacts <- function(contact_matrix,pop){
  pop_prop <- pop / sum(pop)
  contacts <- pop_prop %*% as.matrix(contact_matrix) %*% rep(1,ncol(as.matrix(contact_matrix)))
  contacts
}

# teachers
teacher_contacts <- sapply(listages,function(x) average_contacts(country_school_mix_mat[age3,x,drop=F],pop[age3]))
# worker to consumer
worker_contacts <- sapply(listages,function(x) average_contacts(country_consumer_mat[age3,x,drop=F],pop[age3]))
# other contacts
#sapply(listages,function(x) average_contacts(country_other_mat[x,x,drop=F],pop[x]))
# preschool
new_params$schoolA1 <- average_contacts(country_school_mat[age1,age1],pop[age1]) 
# school
new_params$schoolA2 <- average_contacts(country_school_mat[age2,age2],pop[age2]) 
# workforce travel
new_params$travelA3 <- average_contacts(country_other_mat[age3,age3],pop[age3]) 
# workforce at work
work_contact_average <- average_contacts(country_work_mat[age3,age3],pop[age3]) 
# everyone
comm <- average_contacts(country_home_mat,pop) 

country_all_mat = c(comm)*(country_all_mat/Cav);
write.table(country_all_mat,file.path(out_path,'contact_matrix.csv'),row.names=F,col.names=F,sep=',')

# rescale B contacts
sector_pops <- pop_by_sector$number[c(1:nSectors)]
B_average <- sum(sector_pops*B_sector)/sum(sector_pops)
work_contact_scalar <- work_contact_average/B_average
B_sector <- B_sector*work_contact_scalar

# rescale C contacts
sector_pops <- pop_by_sector$number[c(1:nSectors)[-edSector]]
C_contacts <- matrix(rep(C_sector,4),ncol=4)
C_sector_df <- sapply(1:4,function(x)
  C_contacts[,x]*worker_contacts[x]/(sum(sector_pops*C_contacts[-edSector,x])/sum(sector_pops))
)
C_sector_df[edSector,] <- teacher_contacts
C_sector_df <- as.data.frame(C_sector_df)
colnames(C_sector_df) <- c('age0to4','age5to19','age20to64','age65plus')

# find reciprocal contacts
total_contacts_made <- apply(C_sector_df,2,function(x)x*pop_by_sector$number[1:nSectors])
contacts_per_customer <- apply(total_contacts_made,1,function(x)x/population_size)
rownames(contacts_per_customer) <- NULL
write.table(contacts_per_customer,file.path(out_path,'worker_to_customer.csv'),row.names=F,col.names=F,sep=',')

## 8 vaccination ##############################
cat('vaccination\n')

vaccines_by_age <- setDT(readxlsx(file.path(country_inputs_path,'Vaccines with Ensanut 2021.xlsx'),skip=5))
vaccines_by_age[,age:=rep(c('X20_to_64_years_old','X65_plus'),each=nrow(vaccines_by_age)/2)]
vaccines_by_age <- vaccines_by_age[-c(c(0,nrow(vaccines_by_age)/2)+1),]
colnames(vaccines_by_age)[1] <- 'day'
for(i in 2:9){
  for(j in 0:1){
    totals <- vaccines_by_age[1:11+j*nrow(vaccines_by_age)/2,..i]
    toadd <- totals/sum(totals) * as.numeric(vaccines_by_age[12+j*nrow(vaccines_by_age)/2,..i])
    vaccines_by_age[1:11+j*nrow(vaccines_by_age)/2,i] <- vaccines_by_age[1:11+j*nrow(vaccines_by_age)/2,..i] + toadd
  }
}
vaccines_by_age <- vaccines_by_age[-c(nrow(vaccines_by_age)*c(1/2,1)),]
vaccines_by_age[,day:=get_day_from_jan1(openxlsx::convertToDate(day))]
unkcol <- which(colnames(vaccines_by_age)=='Unknown')
for(i in 1:nrow(vaccines_by_age)){
  totals <- vaccines_by_age[i,2:(unkcol-1)]
  toadd <- totals/sum(totals) * as.numeric(vaccines_by_age[i,..unkcol])
  vaccines_by_age[i,2:(unkcol-1)] <- vaccines_by_age[i,2:(unkcol-1)] + toadd
}
vaccines_by_age[,Unknown:=NULL]
products <- colSums(vaccines_by_age[,2:8])
vaxschedule <- dcast(melt(vaccines_by_age,id.vars=c('day','age'),variable.name='product')[,sum(value),by=.(age,day)],day~age)
vaxschedule[,days:=diff(c(day,670))]
vaxschedule[,total:=X20_to_64_years_old  + X65_plus]

health_vaccines <- readxlsx(file.path(country_inputs_path,'healthcare workers doses.xlsx'))
hwv <- na.omit(diff(c(0,health_vaccines$`Healthcare workers (Cumulative doses)`)))
hwv <- hwv/sum(hwv)*workforce[healthSector]

# vaxschedule[,Health_workers:=0]
# vaxschedule$Health_workers[1:length(hwv)] <- hwv
vaxschedule[,X20_to_64_years_old:=X20_to_64_years_old/total]
vaxschedule[,X65_plus:=X65_plus/total]
vaxschedule[,total:=total/days/2]
vaxschedule <- vaxschedule[,.(day,total,X20_to_64_years_old, X65_plus)]
vaxschedule$X12_to_17_years_old <- 0;
vaxschedule$children <- 0;
vaccine_out_file <- file.path(out_path,'second_vaccinations.csv')
write.csv(vaxschedule,vaccine_out_file,row.names=F)

# moveages <- '60 to 64'
# if(14%in%age3) moveages <- c('60 to 64','65 to 69')
# age_60_to_64 <- pop_by_age$Population[pop_by_age$Age_group%in%moveages]
# prop_60 <- age_60_to_64/(population_size[4]+age_60_to_64)
# 
# vax_table <- readxlsx(file.path(country_inputs_path,'EPI.xlsx'),sheet=6)
# colnames(vax_table)[3:7] <- paste0('X',gsub(' ','_',vax_table[1,3:7]))
# vax_table <- vax_table[-1,]
# for(i in 3:7) vax_table[[i]] <- as.numeric(vax_table[[i]])
# vax_table$day <- get_day_from_jan1(get_day_from_week(vax_table$`DATE (Epi week 2021)` + 52-1))
# colnames(vax_table) <- gsub(' ','_',colnames(vax_table))
# vax_total <- sum(vax_table$TOTAL)
# vax_incomplete <- subset(vax_table,COMPLETE_VACCINATION_SCHEDULE=='False')
# vax_table <- subset(vax_table,COMPLETE_VACCINATION_SCHEDULE=='True')
# vaccines <- unique(vax_table$VACCINE)
# setDT(vax_table)
# vax_table[,cumulative:=cumsum(TOTAL),by=VACCINE]
# # add 60 to 64
# vax_table[,X20_to_64yo:=XLess_than_30yo+X30_to_39yo+X40_to_49yo+X50_to_59yo + prop_60*X60_yo_and_more]
# vax_table[,X60_yo_and_more:=X60_yo_and_more-prop_60*X60_yo_and_more]
# vax_table[,X20_to_64yo_cumulative:=cumsum(X20_to_64yo),by=VACCINE]
# vax_table[,X60_yo_and_more_cumulative:=cumsum(X60_yo_and_more),by=VACCINE]
# vax_table[,X20_to_64_years_old:=X20_to_64yo/(X20_to_64yo+X60_yo_and_more_cumulative)]
# p <- ggplot(vax_table) + geom_line(aes(x=day,y=cumulative,colour=VACCINE))
# # p
# # ggplot(vax_table) + geom_line(aes(x=day,y=X20_to_64yo_cumulative,colour=VACCINE))
# # ggplot(vax_table) + geom_line(aes(x=day,y=X60_yo_and_more_cumulative,colour=VACCINE))
# vax_total <- unique((vax_table[,total:=sum(cumulative),by=day])[,.(day,total)])
# # ggplot(vax_total) + geom_line(aes(x=day,y=total))
# 
# vax_table[,X20_to_64_years_old:=sum(X20_to_64yo)/sum(X20_to_64yo+X60_yo_and_more),by=day]
# vax2 <- unique((vax_table[,total:=sum(TOTAL)/7,by=day])[,.(day,total,X20_to_64_years_old)])
# vax2[,Over_60s:=1-X20_to_64_years_old]
# vax2$X12_to_17_years_old <- 0;
# vax2$children <- 0;
# vaccine_out_file <- file.path(out_path,'second_vaccinations.csv')
# write.csv(vax2,vaccine_out_file,row.names=F)

vax_melt2 <- melt(vaccines_by_age,id.vars=c('day','age'),variable.name='VACCINE')
colnames(vax_melt2)[colnames(vax_melt2)=='age'] <- 'variable'
vax_melt2[,value:=value/2]
# vax_melt <- melt(vax_table,id.vars=c('day','VACCINE'),measure.vars=c('X20_to_64yo_cumulative','X60_yo_and_more_cumulative'))
saveRDS(vax_melt2,file.path(country_inputs_path,'vaccines.Rds'))


# booster_doses <- readxlsx(file.path(country_inputs_path,'Booster doses.xlsx'),sheet=1)
# colnames(booster_doses) <- c('Date',booster_doses[1,-1])
# booster_doses <- booster_doses[-c(1:2),]
# booster_doses$day <- get_day_from_jan1(booster_doses$Date)
# update_doses <- subset(booster_doses,!is.na(Total)&Total>10)
# eligibility_dates  <- subset(booster_doses,is.na(Total))
# update_doses$X60_yo_and_more_cumulative <- (1-prop_60)*as.numeric(update_doses$`60 and older`)
# update_doses$X20_to_64yo_cumulative <- as.numeric(update_doses$Total) - update_doses$X60_yo_and_more_cumulative
# 
# start_days <- sort(eligibility_dates$day)
# boosters <- rbind(na.omit(setDT(update_doses)[day%%7==4,.(day,X60_yo_and_more_cumulative,X20_to_64yo_cumulative)]), list(start_days[1:2],0,0))
# setkey(boosters,day)
# boosters[day==start_days[2],X60_yo_and_more_cumulative:=min(update_doses$X60_yo_and_more_cumulative,na.rm=T)]
# boosters[,X60_yo_and_more_per_day:=c(diff(X60_yo_and_more_cumulative)/diff(day),NA)]
# boosters[,X20_to_64yo_per_day:=c(diff(X20_to_64yo_cumulative)/diff(day),NA)]
# boosters[,total:=X60_yo_and_more_per_day+X20_to_64yo_per_day]
# boosters[,X20_to_64_years_old:=X20_to_64yo_per_day/(X20_to_64yo_per_day+X60_yo_and_more_per_day)]
# boosters[,X65_plus:=1-X20_to_64_years_old]
# boosters[,X12_to_17_years_old:=0]
# boosters[,children:=0]
# 
# booster_out_file <- file.path(out_path,'third_vaccinations.csv')
# write.csv(boosters[,.(day,total,X20_to_64_years_old,X65_plus,X12_to_17_years_old,children)],booster_out_file,row.names=F)

setDT(owidc)
setnames(owidc,'date','Date')
# sum(vax_incomplete$TOTAL)
# subset(owidc,Date=='2021-10-26')$people_vaccinated-subset(owidc,Date=='2021-10-26')$people_fully_vaccinated
owidc[,day:=get_day_from_jan1(Date)]
owidc[,yearmonth:=format(as.Date(Date),'%Y-%m')]
owidc[,firstofmonth:=get_month_day(yearmonth)]
owidc[,daysinmonth:=.N,by=yearmonth]
owidc[,finalsecond:=max(people_fully_vaccinated,na.rm=T),by=yearmonth]
owidc[,finalboost:=max(total_boosters,na.rm=T),by=yearmonth]

# v1 <- dcast(data=monthly_vaccines[Data_category=='Vaccinated_first',],formula=day+total~Vaccine_group,value.var='proportion')
# v1$children <- 0;
# write.csv(v1,file.path(out_path,'first_vaccinations.csv'),row.names=F)

# v2 <- owidc[day==firstofmonth&finalsecond>0,]
# v2[,vax_per_m:=diff(c(0,finalsecond))]
# v2[,total:=vax_per_m/daysinmonth]
# v2 <- v2[,.(day,total)]
# v2$`12 to 17 years old` <- 0;
# v2$`18 to 59 years old` <- c(rep(0,4),rep(1,nrow(v2)-4));
# v2$`Health workers` <- c(1,1,0.3,0.3,rep(0,nrow(v2)-4));
# v2$`Over 60s` <- c(0,0,0.7,0.7,rep(0,nrow(v2)-4));
# v2$children <- 0;
# colnames(v2) <- gsub(' ','_',colnames(v2))
owidc$people_fully_vaccinated[1] <- 0
for(i in 2:nrow(owidc)) if(is.na(owidc$people_fully_vaccinated[i]))
  owidc$people_fully_vaccinated[i] <- owidc$people_fully_vaccinated[i-1]
owidc[,V1:=c(0,diff(people_fully_vaccinated))]
all_second_doses <- owidc[people_fully_vaccinated>0,colnames(owidc)%in%c('Date','V1'),with=F]
write.csv(all_second_doses,file.path(out_path,'all_second_vaccinations.csv'),row.names=F)

owidc[,week:=round(day/7)]
owidc[,weektotal:=mean(V1),by=week]
# x11(); with(subset(owidc,week==day/7),plot(day,weektotal,typ='l',xlim=c(350,750)))
# lines(vax_table$day,vax_table$total,col='red')

# v3 <- owidc[day==firstofmonth&finalsecond>0,]
# v3[is.na(finalboost)==T|is.finite(finalboost)==F,finalboost:=0]
# v3[,vax_per_m:=diff(c(0,finalboost))]
# v3[,total:=vax_per_m/daysinmonth]
# v3 <- v3[,.(day,total)]
# v3$`12 to 17 years old` <- 0;
# v3$`18 to 59 years old` <- 0.2;
# v3$`Health workers` <- 0.1;
# v3$`Over 60s` <- 0.7;
# v3$children <- 0;
# write.csv(v3,file.path(out_path,'third_vaccinations.csv'),row.names=F)


# vaccine effects

VEs <- readxlsx(file.path(multi_country_inputs_path,'strain_vaccine_profiles.xlsx'))[1:10,]
uu <- function(x) unname(unlist(x))
nuu <- function(x) as.numeric(unname(unlist(x)))
VEdf <- data.frame(Parameter=uu(VEs[1,2:9]),
                   ChAdOx1=nuu(VEs[VEs[,1]=='ChAdOx1',2:9]),
                   Sinovac=nuu(VEs[VEs[,1]=='Sinovac',2:9]),
                   BNT162b2=nuu(VEs[VEs[,1]=='BNT162b2',2:9]),stringsAsFactors = F)


# total_vs <- subset(vax_table[,sum(TOTAL),by=VACCINE],VACCINE%in%c('Astra Zeneca','Pfizer','Sinovac'))
total_v2s <- subset(vax_melt2[,sum(value),by=VACCINE],VACCINE%in%c('AstraZeneca','Pfizer','Sinovac'))
total_v2s$props <- total_v2s$V1/sum(total_v2s$V1)
VEdf$Average <- VEdf$ChAdOx1*subset(total_v2s,VACCINE%in%c('AstraZeneca'))$props +
  VEdf$BNT162b2*subset(total_v2s,VACCINE%in%c('Pfizer'))$props +
  VEdf$Sinovac*subset(total_v2s,VACCINE%in%c('Sinovac'))$props

write.csv(VEdf,file.path(out_path,'vaccine_profiles.csv'),row.names=F)

## 9 stringency index ##################################
cat('indices\n')



index_file <- file.path(country_inputs_path,'index.xlsx')

## if only indices are needed:
# library(oxcgrt)
# x <- get_json_time(from = "2020-01-02", to = Sys.Date())
# raw_ox_ts <- get_data(x)

make_file <- !file.exists(index_file) || difftime( today(),file.info(index_file)$mtime ,units='days') > 1
if(make_file){
  ## template
  # read in template and rename to match covid-policy-tracker
  # template_file <- file.path(template_path,'Blavatnik index Sri Lanka.xlsx')
  # temp_sheet1 <- readxlsx(template_file,sheet=1)
  # colnames(temp_sheet1)[1] <- 'date'
  # colnames(temp_sheet1) <- sapply(colnames(temp_sheet1), function(x) gsub(' ','',x))
  # colnames(temp_sheet1) <- sapply(colnames(temp_sheet1), function(x) gsub('\\(','',x))
  # colnames(temp_sheet1) <- sapply(colnames(temp_sheet1), function(x) gsub(')','',x))
  # setnames(temp_sheet1,"c8internationaltravel","c8internationaltravelcontrols")
  # temp_sheet2 <- readxlsx(template_file,sheet=2)
  # colnames(temp_sheet2)[1] <- 'date'
  # colnames(temp_sheet2) <- sapply(colnames(temp_sheet2), function(x) gsub(' ','',x))
  # colnames(temp_sheet2) <- sapply(colnames(temp_sheet2), function(x) gsub('%','percent',x))
  # temp_sheet3 <- readxlsx(template_file,sheet=3)
  # colnames(temp_sheet3)[1] <- 'date'
  # colnames(temp_sheet3) <- sapply(colnames(temp_sheet3), function(x) gsub(' ','',x))
  # setnames(temp_sheet3,"c7movementrestrictions","c7restrictionsoninternalmovement")
  # setnames(temp_sheet3,"c8internationaltravel","c8internationaltravelcontrols")
  # setnames(temp_sheet3,"e2debtrelief","e2debtcontractrelief")
  # setnames(temp_sheet3,"h6facialcovering","h6facialcoverings")
  # temp_sheet4 <- readxlsx(template_file,sheet=4)
  
  ## read in OxCRGT
  
  ##!! is there a static link for this (not github)?
  raw_ox <- read.csv('https://raw.githubusercontent.com/OxCGRT/covid-policy-tracker/master/data/OxCGRT_nat_latest.csv',stringsAsFactors = F)
  
  country_data <- raw_ox[raw_ox$CountryName==country,]
  
  # rename to match template
  colnames(country_data) <- tolower(colnames(country_data))
  colnames(country_data) <- sapply(colnames(country_data), function(x) gsub('_','',x))
  colnames(country_data) <- sapply(colnames(country_data), function(x) gsub('\\.','',x))
  colnames(country_data) <- sapply(colnames(country_data), function(x) gsub(' ','',x))
  
  ## get daily index and average over month
  
  workplaceclosing <- grepl('workplaceclosing',colnames(country_data))
  stayathomerequirements <- grepl('stayathomerequirements',colnames(country_data))
  if(!(sum(workplaceclosing)==1&sum(stayathomerequirements)==1)) cat('check indices\n')
  country_data <- setDT(country_data)
  country_data$econclosureindexday <- rowSums(country_data[,workplaceclosing|stayathomerequirements,with=F])
  country_data[,monthyear:=format(ymd(date), "%Y-%m")]
  country_data[,month:=format(ymd(date), "%m")]
  country_data[,year:=format(ymd(date), "%Y")]
  country_data[,day:=format(ymd(date), "%d")]
  country_data[,econclosureindexmonth:=mean(econclosureindexday,na.rm=T),by=monthyear]
  
  ## extract monthly data
  # subset to one day per month (the first)
  month_data <- country_data[day=='01',]
  # round month -> quarter
  month_data[,quarter:=ceiling(as.numeric(month)/3)]
  month_data[,economicclosures:=econclosureindexmonth]
  month_data[,averagebyquarter:=mean(economicclosures,na.rm=T),by=.(year,quarter)]
  month_data[,percentvariationaroundQaverage:=economicclosures/averagebyquarter*100]
  month_data[,percentdifferenceinstringencycomparedtoQvalue:=percentvariationaroundQaverage-100]
  
  ## save
  
  # extract columns matching template
  c2flag <- grepl('c2',colnames(country_data))
  internationaltravelcontrols <- grepl('internationaltravelcontrols',colnames(country_data))
  colnamestemp_sheet1 <- c("date",colnames(country_data)[c2flag|internationaltravelcontrols], "econclosureindexday","econclosureindexmonth")
  colnamestemp_sheet2 <- c("date","economicclosures","averagebyquarter","percentvariationaroundQaverage", "percentdifferenceinstringencycomparedtoQvalue")
  y1 <- country_data[,colnamestemp_sheet1,with=F]
  y2 <- month_data[,colnamestemp_sheet2,with=F]
  y3 <- country_data#[,colnames(temp_sheet3),with=F]
  
  # write
  write.xlsx(y1,index_file,sheetName='Day',col.names=T,row.names=F)
  write.xlsx(y2,index_file,sheetName='Month',col.names=T,row.names=F,append=T)
  write.xlsx(y3,index_file,sheetName='All',col.names=T,row.names=F,append=T)
  #write.xlsx(temp_sheet4,index_file,sheetName='Definitions',col.names=T,row.names=F,append=T)
  
  peak_infection_day <- which.max(diff(y3$confirmedcases))
  days <- get_day_from_jan1(ymd(y3$date))#[1:peak_infection_day]
  data <- data.frame(days=days,ydata=y3$confirmeddeaths)#[1:peak_infection_day])
  data <- data[!is.na(data[,2]),]
  write.csv(data,file.path(out_path,'death_data.csv'),row.names = F)
  
  newcases <- diff(y3$confirmedcases)
  # plot(y3$stringencyindex[1+1:400],newcases[7+1:400])
  
  ## other file that might or might not be needed?
  
  # month_data[,quarter_year:=paste0(quarter,'-',year)]
  # quarter_data <- unique(month_data[,.(quarter_year,averagebyquarter)])
  # 
  # entry1 <- t(quarter_data)
  # entry2 <- t(month_data[,.(monthyear,econclosureindexmonth)])
  # df <- matrix('',nrow=5,ncol=1+max(ncol(entry1),ncol(entry2)))
  # df[2,1] <- 'average closure stringency by quarter'
  # df[5,1] <- 'economic closures by month'
  # df[1,1+1:ncol(entry1)] <- entry1[1,]
  # df[2,1+1:ncol(entry1)] <- entry1[2,]
  # df[4,1+1:ncol(entry2)] <- entry2[1,]
  # df[5,1+1:ncol(entry2)] <- entry2[2,]
  
  
  #write.xlsx(df,file.path(out_path,'Xmin_IDN.xlsx'),sheetName='economic closure stringency',col.names=F,row.names=F)
  
  rm(raw_ox)
  rm(temp_sheet1)
  rm(temp_sheet3)
}

## 10 hospital capacity ######################################
cat('hospital capacity\n')

# https://data.worldbank.org/indicator/SH.MED.BEDS.ZS?locations=ID
# 1.04 per 1000 people, 2017

# search_dataset('health')
# health_struct <- get_data_structure('HEALTH_REAC')
# beds <- get_dataset('HEALTH_REAC',filter=list(list('HOPITBED','HOPILICS'),'',country_abbr),start_time = 2017)
hospital_sheet <- read.csv(file.path(multi_country_inputs_path,'hospital_beds.csv'),stringsAs=F,skip = 4)
country_row <- unlist(hospital_sheet[hospital_sheet$Country.Name==country,])
per1000 <- as.numeric(tail(country_row[!is.na(country_row)],1))
idn_bed_ratio <- max(owidc$hospital_beds_per_thousand)/max(owidi$hospital_beds_per_thousand)
idn_pop_ratio <- sum(pop_by_age$Population) / 270000000
new_params$Hmax <-  90000 * idn_bed_ratio * idn_pop_ratio
# per1000 <- beds$obsValue[beds$UNIT=='RTOINPNB']
# IDN_ICU <- 13854 + 6644
# IDN_for_covid <- 85000

# scale like Sri Lanka?
# SR_ICU <- 164
# SR_Hmax <- 2017
# SR_beds_for_covid <- 11880
# w <- (SR_Hmax-SR_beds_for_covid)/(SR_ICU-SR_beds_for_covid)
# Hmax <- w*IDN_ICU+(1-w)*IDN_for_covid 

# new_params$Hmin <- 60000
# new_params$Hmax <- 90000

## 11 alpha ################################################
cat('alpha\n')


country_pwt <- subset(pwt10.0,isocode==country_abbr&!is.na(labsh))
most_recent <- max(country_pwt$year,na.rm=T)
new_params$alpha <- subset(country_pwt,year==most_recent)$labsh




## 12 gva pm ####################################
cat('gva pm\n')



# read in sheets
gva_pq_sheet <- readxlsx(file.path(country_inputs_path,'ECON.xlsx'),sheet=9,col_names=F)
# crop data sheet
numeric_row <- 10:32 #which(gva_pq_sheet$ID==1)
Qrow <- 6
annual_cols <- which(grepl('Annual',gva_pq_sheet[Qrow,]))
obj_col <- max(annual_cols[annual_cols<which(grepl('2020',gva_pq_sheet[Qrow-1,]))])
year_cols <- gva_pq_sheet[Qrow-1,grepl('20',gva_pq_sheet[Qrow-1,])]
q_cols <- which(grepl('Q',gva_pq_sheet[Qrow,]))
extract_rows <- numeric_row[sapply(gva_pq_sheet[numeric_row,1],function(x)grepl('^[[:digit:]]+',x))]
# the column is the average of four quarters. to get monthly value, divide by 3.
obj <- as.numeric(unlist(gva_pq_sheet[extract_rows,obj_col]))/12

gva_pq_tab <- gva_pq_sheet[extract_rows,q_cols]
colnames(gva_pq_tab) <- unlist(lapply(year_cols,function(x)paste0(x,'-',paste0('Q',1:4))))

# save to plot
gva_to_plot <- as.data.frame(t(gva_pq_tab),row.names=F)
colnames(gva_to_plot) <- sectors
gva_to_plot <- reshape2::melt(gva_to_plot,id.vars=NULL)
gva_to_plot$value <- as.numeric(gva_to_plot$value)
gva_to_plot$quarter <- rep(unlist(lapply(2018:2021,function(x)paste0(paste0('Q',1:4),' ',x))),nrow(gva_pq_tab))
gva_to_plot$quarter <- rep(1:ncol(gva_pq_tab),nrow(gva_pq_tab))
sector_short_name <- sapply(as.character(sectors),function(x)strsplit(x,', |; | and | with ')[[1]][1])
label_data <- data.frame(x=16,yr=gva_to_plot$value[gva_to_plot$quarter==16],yl=gva_to_plot$value[gva_to_plot$quarter==1],
                         label=sector_short_name,row.names=NULL)
label_data$x[order(label_data$yr)[seq(1,nrow(label_data),by=2)]] <- 1
saveRDS(list(gva_to_plot,label_data),file.path(country_inputs_path,'gva_pq.Rds'))
# colnames(mapped_gva_pq) <- unlist(lapply(2018:2021,function(x)paste0(paste0('Q',1:4),' ',x)))
# rownames(mapped_gva_pq) <- rev4_sectors$`Gross Value Added (GVA) by economic activity (ADB sectors)`
# write.csv(mapped_gva_pq,file.path(country_inputs_path,'gva_pq.csv'))

## 13 mobility ####################################################################
cat('mobility\n')

y2q <- readxlsx(index_file,sheet=2)
y2q$date <- as.Date(as.character(y2q$date),format='%Y%m%d')
y2q$Q <- (year(y2q$date)-2020)*4 + ceiling(month(y2q$date)/3)

smooth_counts <- function(x,window=3){
  y <- x
  for(i in 1:length(x)){
    ind <- max(1,i-window) : min(length(x),i+window)
    y[i] <- mean(x[ind],na.rm=T)
  }
  y
}

temp <- tempfile()
download.file("https://www.gstatic.com/covid19/mobility/Region_Mobility_Report_CSVs.zip",temp)
fle <- unzip(temp,exdir=file.path(multi_country_inputs_path,'mobility'))
country_mob_files <- fle[grepl(country_abbr_2,fle)]
mob_dta <- c()
for(i in 1:length(country_mob_files))
  mob_dta <- rbind(mob_dta,subset(read.csv(country_mob_files[i],stringsAs=F),sub_region_1==''))
unlink(temp)
mob_dta$smooth_transit <- smooth_counts(mob_dta$transit_stations_percent_change_from_baseline,7)
mob_dta$smooth_retail <- smooth_counts(mob_dta$retail_and_recreation_percent_change_from_baseline,7)
mob_dta$smooth_work <- smooth_counts(mob_dta$workplaces_percent_change_from_baseline,7)
mob_dta$smooth_grocery <- smooth_counts(mob_dta$grocery_and_pharmacy_percent_change_from_baseline,7)
mob_dta$smooth_parks <- smooth_counts(mob_dta$parks_percent_change_from_baseline,7)
mob_dta$smooth_home <- smooth_counts(mob_dta$residential_percent_change_from_baseline,7)
saveRDS(mob_dta[,colnames(mob_dta)%in%c("date","smooth_transit","smooth_retail",
                                        "smooth_work","smooth_grocery","smooth_parks","smooth_home")],
        file.path(country_inputs_path,'mobility.Rds'))
ggplot(mob_dta) + geom_line(aes(x=as.Date(date),y=smooth_transit,colour='Transit')) +
  geom_line(aes(x=as.Date(date),y=smooth_retail,colour='Retail')) +
  geom_line(aes(x=as.Date(date),y=smooth_work,colour='Workplaces')) +
  geom_line(aes(x=as.Date(date),y=smooth_grocery,colour='Grocery')) +
  geom_line(aes(x=as.Date(date),y=smooth_parks,colour='Parks')) +
  geom_line(aes(x=as.Date(date),y=smooth_home,colour='Home')) +
  theme_bw(base_size=15) + labs(x='',y='Percent change',colour='')


# get month averages

mob_dta$yearmonth <- format.Date(mob_dta$date,'%b-%y')
setDT(mob_dta)
mob_dta[,meanretail:=mean(retail_and_recreation_percent_change_from_baseline),by=yearmonth]
mob_dta[,meanwork:=mean(workplaces_percent_change_from_baseline),by=yearmonth]
mob_dta[,meantransit:=mean(transit_stations_percent_change_from_baseline),by=yearmonth]
mob_dta$quarter <- ceiling(as.numeric(format.Date(mob_dta$date,'%m'))/3)
mob_dta$year <- as.numeric(format.Date(mob_dta$date,'%Y'))
modeldata <- unique(mob_dta[date<'2022-06-01',.(quarter,yearmonth,meanretail,meanwork,meantransit,year)])
modeldata$econ <- y2q$economicclosures[y2q$date>'2020-01-01'&y2q$date<'2022-06-01']
quarterdata <- unique(modeldata[,list(meanretail=mean(meanretail),meanwork=mean(meanwork),
                                      meantransit=mean(meantransit),econ=mean(econ)),by=.(year,quarter)])

ggplot(modeldata) + 
  geom_line(aes(x=as.Date(paste0('1-',yearmonth),tryFormats=c('%d-%b-%y')),y=meanretail),colour='navyblue')+ 
  geom_line(aes(x=as.Date(paste0('1-',yearmonth),tryFormats=c('%d-%b-%y')),y=meanwork),colour='grey')+ 
  geom_line(aes(x=as.Date(paste0('1-',yearmonth),tryFormats=c('%d-%b-%y')),y=meantransit),colour='hotpink')+ 
  geom_line(aes(x=as.Date(paste0('1-',yearmonth),tryFormats=c('%d-%b-%y')),y=-econ*10),colour='darkorange')+
  scale_x_date(date_breaks = '2 month',date_labels='%b-%y')

cor(modeldata[,3:6])

## 14 gvas #####################################################
cat('gvas\n')

gva_pan <- gva_pq_tab[,grepl('2020|2021',colnames(gva_pq_tab))]
gva_pm <- data.frame(month=1:(3*ncol(gva_pan)))
for(i in 1:nrow(gva_pan)){
  secrow <- as.numeric(gva_pan[i,])
  subdat <- subset(quarterdata,year<2022)
  pcormat <- abs(ppcor::pcor(as.data.frame(cbind(subdat[,3:5],secrow)))$estimate)[-4,4]
  form1 <- paste0('(secrow)~factor(quarter)+ns(econ)+',paste(paste0('ns(',names(sort(pcormat,decreasing=T)[1]),',df=2)'),collapse='+'))
  mod1 <- glm(as.formula(form1),data=subdat)
  pred1 <- (predict(mod1,newdata = subset(modeldata,year<2022),type = 'response'))
  form2 <- paste0('(secrow)~factor(quarter)+ns(econ)+',paste(paste0('ns(',names(sort(pcormat,decreasing=T)[1]),',df=1)'),collapse='+'))
  mod2 <- glm(as.formula(form2),data=subdat)
  pred2 <- (predict(mod2,newdata = subset(modeldata,year<2022),type = 'response'))
  pred <- (mod1$deviance*pred1+mod2$deviance*pred2)/(mod1$deviance+mod2$deviance)
  # add in Jan 2020
  pred <- c(pred[1],pred)
  #x11(); plot(secrow,(mod$fitted.values),main=sec_names[i])
  for(j in 1:ncol(gva_pan)){
    pred[1:3+(j-1)*3] <- secrow[j] * pred[1:3+(j-1)*3] / sum(pred[1:3+(j-1)*3]) / 4
  }
  gva_pm[[sec_names[i]]] <- pred
}
gva_pm_melt <- reshape2::melt(gva_pm,id.vars=c('month'))
ggplot(gva_pm_melt) + geom_line(aes(x=month,y=log(value),colour=variable)) +
  theme(legend.position='none')

## 15 xs #####################################################
cat('xs\n')


# ref_2019 <- gva_pq_tab[,grepl('19',colnames(gva_pq_tab))]
# xpq <- gva_pan
# for(i in 1:ncol(xpq)){
#   qval <- strsplit(colnames(xpq),'-')[[1]][2]
#   ref_col <- which(grepl(qval,colnames(ref_2019)))
#   xpq[[i]] <- as.numeric(gva_pan[[i]])/as.numeric(ref_2019[[ref_col]])
# }
# 
# xpqt <- data.frame(t(as.matrix(xpq)))
# xpqt$Q <- as.numeric(grepl('2021',rownames(xpqt)))*4 + 
#   as.numeric(sapply(rownames(xpqt),function(x)strsplit(x,'Q')[[1]][2]))
# xindex <- left_join(xpqt,y2q,by='Q')
# x_mat <- matrix(0,nrow=nSectors,ncol=24)
# 
# for(i in 1:nSectors){
#   xcol <- colnames(xindex)[i]
#   build_data <- unique(setDT(xindex[,colnames(xindex)%in%c(xcol,'averagebyquarter')]))
#   colnames(build_data) <- c('x','economicclosures')
#   index_model <- glm(x~economicclosures,data=build_data)
#   x_mat[i,] <- pmin(1,predict(index_model,newdata=xindex))
# }

## 16 update io #################################################
gva_pq <- readRDS('country_data/gva_pq.Rds')[[1]]
gva_2019 <- sapply(unique(gva_pq$variable),function(x)mean(subset(gva_pq,variable==x&quarter%in%5:8)$value))/12
ones <- rep(1,nSectors)
old_i <- old_io_mat%*%ones # intermediate_demand
old_j <- t(old_io_mat)%*%ones # intermediate_supply
old_g <- ones*0 # max_inputs
old_r <- old_j + old_g
old_f <- ones*demand/12
old_q <- old_i + old_f # total demand
old_gva <- old_q - old_r
old_q <- old_i + old_f # total demand
old_gva <- old_q - old_r
target_gva <- gva_2019/sum(gva_2019)*sum(old_gva) # openness*old_gva # 
source(file.path(processing_code_path,'io_ras.R'))
ras_list <- ras_target(target_gva,io_mat=old_io_mat,gva=old_gva,q=old_q)

recent_io_mat <- ras_list$io_mat
obj <- ras_list$gva

write.table(recent_io_mat,file.path(out_path,paste0('IO.csv')),col.names=F,row.names=F,sep=',')


## 
## 17 hospitalisations ##################################
cat('hospitalisations\n')


tableau_name <- 'hospitalisation_tableau.csv'
raw_hospital_data <- file.path(country_inputs_path,tableau_name)
if(!file.exists(raw_hospital_data)){
  hosp_data_epi <- readxlsx(file.path(country_inputs_path,'Epi-Info.xlsx'),sheet=1)
  hosp_data_epi$occupancy <- hosp_data_epi$`OVERALL HOSPITAL OCCUPANCY` * hosp_data_epi$`TOTAL BEDS`
  predict_occupancy <- glm(`TOTAL BEDS`~`GENERAL BEDS OCCUPANCY`,data=hosp_data_epi)
  hosp_data_epi$pred <- predict(predict_occupancy,newdata=hosp_data_epi)
  hosp_data_epi$Hospitalised <- with(hosp_data_epi,pred*`GENERAL BEDS OCCUPANCY`)
  ggplot(hosp_data_epi,aes(x=DATE)) + geom_line(aes(y=Hospitalised,colour='Predict')) + geom_line(aes(y=occupancy,colour='Occupancy')) + geom_line(aes(y=`TOTAL BEDS`,colour='Capacity'))
  hosp_data_epi$day <- get_day_from_jan1(hosp_data_epi$DATE)
  write.csv(hosp_data_epi,file.path(out_path,'hospital_occupancy.csv'),row.names = F)
  
  hosp_data_epi$Date <- as.character(hosp_data_epi$DATE)
  # hosp_data_epi$pc <- as.numeric(hosp_data_epi$`HOSPITAL OCCUPANCY (%)`)
  # hosp_jnd <- left_join(hosp_data,hosp_data_epi,by='Date')
  # hosp_jnd$capacity <- hosp_jnd$Hospitalised/hosp_jnd$pc*100
  # ggplot(hosp_jnd) + geom_path(aes(x=day,y=capacity))
  hosp_data_ad <- readxlsx(file.path(country_inputs_path,'EPI.xlsx'),sheet=1)
  hosp_data_ad$Date <- as.character(hosp_data_ad$DATE)
  hosp_data_ad$day <- get_day_from_jan1(hosp_data_ad$Date)
  hosp_ad <- hosp_data_ad[,colnames(hosp_data_ad)%in%c('Date','day','HOSPITAL ADMISSIONS')]
  colnames(hosp_ad) <- c('admissions','Date','day')
  admissions <- readRDS(file.path(country_inputs_path,'admissions.Rds'))
  hosp_ad <- left_join(admissions,hosp_ad,by='day')
  admissions_by_age <- readRDS(file.path(country_inputs_path,'admissions_by_age.Rds'))
  hosp_ad <- left_join(admissions_by_age,hosp_ad,by='day')
  write.csv(hosp_ad,file.path(out_path,'hospital_admissions.csv'),row.names = F)
  rm(hospref0)
  rm(hospref1)
}else{

  if(file.exists(file.path('~/Downloads',tableau_name)))
    system(paste0('mv ',file.path('~/Downloads',tableau_name),' ',raw_hospital_data))

  source(file.path(processing_code_path,'hospitalisations.R'))
  file_list <- list.files(country_inputs_path)
  file_path <- file.path(country_inputs_path,file_list[grepl('hospitalisations_daily_',file_list)])
  hosp_data <- read.csv(file_path,stringsAs=F)
  hosp_data$day <- get_day_from_jan1(hosp_data$Date)
  write.csv(hosp_data[,c('day','Hospitalised','Deaths','Date')],file.path(out_path,'hospital_occupancy.csv'),row.names = F)
}

## 18 lineages ##############################################
cat('lineages\n')

# https://www.gisaid.org/ rj411 downloads metadata
gisaid_meta_file <- file.path(multi_country_inputs_path,'metadata.tsv')
lineage_file <- file.path(country_inputs_path,'lineage.csv')
if(file.exists(file.path('~/Downloads','metadata.tsv')))
  system(paste0('mv ',file.path('~/Downloads','metadata.tsv'),' ',gisaid_meta_file))
make_file <- !file.exists(lineage_file) || file.info(lineage_file)$mtime < file.info(gisaid_meta_file)$mtime

# process lineage data

if(make_file){
  command <- paste0("grep -w -F '",country,"' ",gisaid_meta_file," > country_data/gisaid.tsv")
  gisaid_file <- "country_data/gisaid.tsv"
  system(command)
  md <- read.csv(gisaid_file,sep='\t',stringsAsFactors=F)
  mdcolnames <- read.csv(gisaid_meta_file,sep='\t',stringsAsFactors=F,nrows = 1)
  colnames(md) <- colnames(mdcolnames)
  
  weektodate <- function(x){
    format(ymd('2019-12-28')+7*x,'%d-%b')
  }
  
  vsdt <- as.data.table(md)
  rm(md)
  gc()
  country_string <- paste0('/ ',country,' | / ',country,'$')
  vsdt[,country_flag:=grepl(country_string,Location),by=Location]
  vsdt[country_flag==T,]
  vsdti <- vsdt[country_flag==T,]
  
  vsdti[,epiweek:=epiwrap(ymd(Collection.date)),by=Collection.date]
  
  vsdti[,Lineage:=Pango.lineage]
  vsdti[grepl('BA\\.|B\\.1\\.1\\.529|B\\.1\\.1\\.519|B\\.1\\.617\\.2|AY\\.',Pango.lineage)==F,Lineage:='Other']
  vsdti[grepl('B\\.1\\.617\\.2|AY\\.',Pango.lineage)==T,Lineage:='B.1.617.2 / AY.']
  vsdti[grepl('B\\.1\\.1\\.529|BA\\.',Pango.lineage)==T,Lineage:='B.1.1.529 / BA.']
  
  xtab <- as.data.table(reshape2::melt(xtabs(data=vsdti,~epiweek+Lineage)))
  xtab[,total:=sum(value),by=.(epiweek)]
  write.csv(xtab,lineage_file,row.names = F)
}

xtab <- read.csv(file.path(country_inputs_path,'lineage.csv'),stringsAs=F)
xtab <- subset(xtab,total>5)
delta <- subset(xtab,Lineage=='B.1.617.2 / AY.')
delta$prop <- delta$value/delta$total
delta$day <- get_day_from_jan1(ymd('2019-12-28')+7*delta$epiweek)
fn <- function(param){
  cp1 <- param[1]
  cp2 <- param[2]
  grad <- 1/(cp2-cp1)
  y <- rep(0,nrow(delta))
  x <- delta$day
  y[x>cp1] <- pmin(1,grad*(x[x>cp1]-cp1))
  sum(abs(y-delta$prop))
}
params <- optim(c(400,500),fn)
new_params$Delta_start <- round(params$par[1])
new_params$Delta_end <- round(params$par[2])

omicron <- subset(xtab,Lineage=='B.1.1.529 / BA.')
omicron$prop <- omicron$value/omicron$total
omicron$day <- get_day_from_jan1(ymd('2019-12-28')+7*omicron$epiweek)
fn <- function(param){
  cp1 <- param[1]
  cp2 <- param[2]
  grad <- 1/(cp2-cp1)
  y <- rep(0,nrow(omicron))
  x <- omicron$day
  y[x>cp1] <- pmin(1,grad*(x[x>cp1]-cp1))
  sum(abs(y-omicron$prop))
}
params <- optim(c(630,660),fn)
new_params$Omicron_start <- round(params$par[1])
new_params$Omicron_end <- round(params$par[2])
# new_params$Omicron_immune_escape <- log(0.2)/(new_params$Omicron_start-new_params$Omicron_end)
# needs to be matched in heSimCovid19vax
new_params$Omicron_immune_escape <- log(0.01)/-7


## 19 update parameters #######################################
cat('update parameters\n')


parameter_file_in <- file.path(multi_country_inputs_path,'parameters.ods')
parameter_file_out <- file.path(out_path,'parameters.csv')
parameters <- read_ods(parameter_file_in)

days_a_infectious <- as.numeric(parameters$Value[parameters$Parameter=='Ta'])
days_s_infectious <- as.numeric(parameters$Value[parameters$Parameter=='Ts'])
days_incubation <- as.numeric(parameters$Value[parameters$Parameter=='Text'])
isolation_by_day_overall <- c(31.9,1,.6,1.2,.3,1.9,0,4.1,58.9)/100
isolation_by_day_sym <- c(22.9,1.4,0.8,1.2,.3,2.3,0,5.4,65.6)/100
isolation_by_day_asym <- c(56.5,0,0,1.4,0,.8,0,.5,40.7)/100
# Assuming a delay from contact to notification
days_incubation_left <- days_incubation - 1.6
days_si <- days_incubation_left + 0:days_a_infectious
si_asym <- 0
for(i in days_si[-length(days_si)]) si_asym <- si_asym + sum(isolation_by_day_asym[i:length(isolation_by_day_asym)])
si_asym <- si_asym + (days_incubation_left + days_a_infectious - floor(max(days_si))) * sum(isolation_by_day_asym[floor(max(days_si)):length(isolation_by_day_asym)])
si_asym <- si_asym/days_a_infectious
new_params$p3 <- si_asym

days_si <- days_incubation_left + 0:days_s_infectious
si_sym <- 0
for(i in days_si[-length(days_si)]) si_sym <- si_sym + sum(isolation_by_day_sym[i:length(isolation_by_day_sym)])
si_sym <- si_sym + (days_incubation_left + days_s_infectious - floor(max(days_si))) * sum(isolation_by_day_sym[floor(max(days_si)):length(isolation_by_day_sym)])
si_sym <- si_sym/days_s_infectious
new_params$p4 <- si_sym

new_params$Threc <- 10.24 #(10.17, 10.31)
new_params$Thd <- 8.41 #(8.37, 8.46)
new_params$edSector <- edSector
new_params$conSector <- conSector
new_params$Omicron_ve <- 0.1
new_params$Omicron_reinfection <- 0.8
new_params$Omicron_reinfection_hospital <- 0.5
new_params$Omicron_reinfection_hfr <- 0.75
for(i in 1:length(new_params))
  parameters$Value[parameters$Parameter==names(new_params)[i]] <- new_params[[i]]

write.table(parameters,parameter_file_out,row.names = F,quote=F,sep='\t')


# hospitalised <- read.csv(file.path(multi_country_inputs_path,'owid-covid-data.csv'),stringsAs=F)
# hospitalised <- subset(hospitalised,location==country)
# hospitalised$weekly_hosp_admissions





## 20 waning ####################################################
cat('waning\n')

source('code/Processing/get_waning_profile.R')

vaccine_rollout_schedule <- read.csv(file.path(out_path,'second_vaccinations.csv'),stringsAs=F)
# for now, extend 0 vaccines up to end of year
vaccine_rollout_schedule <- rbind(vaccine_rollout_schedule,vaccine_rollout_schedule[nrow(vaccine_rollout_schedule),])
vaccine_rollout_schedule[nrow(vaccine_rollout_schedule),1:2] <- c(max(vaccine_rollout_schedule$day)+7,0)
booster_rollout_schedule <- read.csv(file.path(out_path,'third_vaccinations.csv'),stringsAs=F)
vaccine_effect_parameter <- read.csv(file.path(out_path,'vaccine_profiles.csv'),stringsAs=F)
infants <- vaccine_rollout_schedule$children
teens <- vaccine_rollout_schedule$X12_to_17_years_old
adults <- vaccine_rollout_schedule$X20_to_64_years_old
elders <- vaccine_rollout_schedule$X65_plus

days <- c(diff(vaccine_rollout_schedule$day),30)
totals <- vaccine_rollout_schedule$total
nTeens <- population_size[2]
nAdults <- population_size[3]
nElders <- population_size[4]

vaccinated_frac_teens <- sum(days*totals*teens)/nTeens
vaccinated_frac_adults <- sum(days*totals*adults)/nAdults
vaccinated_frac_elders <- sum(days*totals*elders)/nElders

teen_target <- (.9-vaccinated_frac_teens)*nTeens
adult_target <- (.9-vaccinated_frac_adults)*nAdults
elder_target <- (.9-vaccinated_frac_elders)*nElders
half_teen_target <- teen_target/2

nVax_max <- teen_target+adult_target+elder_target
nVax <- c(nVax_max, nVax_max-half_teen_target, nVax_max-teen_target)
nVaxPerDay <- nVax / 180

write.csv(data.frame(Vaccines=rep(nVaxPerDay,2)/rep(1:2,each=3),
                     Adults=c(rep(.9,3),rep(vaccinated_frac_adults+(.9-vaccinated_frac_adults)/2,3))*100,
                     Elders=c(rep(.9,3),rep(vaccinated_frac_elders+(.9-vaccinated_frac_elders)/2,3))*100,
                     Adolescents=rep(seq(.9,0,length=3),2)/rep(1:2,each=3)*100),'reports/scenarios.csv',row.names = F)

proportions <- list(c(nVaxPerDay[1],adult_target/nVax[1],elder_target/nVax[1],teen_target/nVax[1],0),
                    c(nVaxPerDay[2],adult_target/nVax[2],elder_target/nVax[2],half_teen_target/nVax[2],0),
                    c(nVaxPerDay[3],adult_target/nVax[3],elder_target/nVax[3],0,0),
                    c(nVaxPerDay[1]/2,adult_target/nVax[1],elder_target/nVax[1],teen_target/nVax[1],0),
                    c(nVaxPerDay[2]/2,adult_target/nVax[2],elder_target/nVax[2],half_teen_target/nVax[2],0),
                    c(nVaxPerDay[3]/2,adult_target/nVax[3],elder_target/nVax[3],0,0),
                    ##!! not used for scenario 7 as time horizon finishes sooner (2021)
                    c(nVaxPerDay[3]/2,adult_target/nVax[3],elder_target/nVax[3],0,0))
file_lab <- paste0('_',1:length(proportions))

start_day <- vaccine_rollout_schedule$day[1]
initial_VE <- vaccine_effect_parameter$Average[vaccine_effect_parameter$Parameter=='Acquisition']
wane_time <- vaccine_effect_parameter$Average[vaccine_effect_parameter$Parameter=='Expected time to wane']
sero_time <- vaccine_effect_parameter$Average[vaccine_effect_parameter$Parameter=='Expected time to seroconversion']
mat_time <- 28
global_max_day <- 2*365+250
age_indices <- c(3,4,5,6)
scenario7 <- vaccine_rollout_schedule
scenario7$total <- c(seq(0,2000000/7,length=10),rep(2000000/7,nrow(vaccine_rollout_schedule)-10))
adult_only <- with(scenario7,which(cumsum(total*7*X65_plus)>nElders))
scenario7$X20_to_64_years_old[adult_only] <- 1
scenario7$X65_plus[adult_only] <- 0
schedules <- list(vaccine_rollout_schedule,vaccine_rollout_schedule,
                  vaccine_rollout_schedule,vaccine_rollout_schedule,
                  vaccine_rollout_schedule,vaccine_rollout_schedule,
                  scenario7)

make_file <- T
for(j in 1:length(proportions)){
  rollout_schedule <- schedules[[j]]
  rollout_schedule[1+nrow(rollout_schedule),] <- c(365*2,proportions[[j]])
  write.csv(rollout_schedule,file.path(out_path,paste0('second_vaccinations',file_lab[j],'.csv')),row.names=F)
  if(make_file){
    gamma_ratios <- data.frame(day=1:global_max_day)
    for(i in 1:length(age_indices)) gamma_ratios[[colnames(vaccine_rollout_schedule)[age_indices[i]]]] <- 1
    for(i in 1:length(age_indices)){
      age_rollout <- rollout_schedule[,age_indices[i]]
      age_start_ind <-  age_rollout > 0
      if(sum(age_start_ind)>0){
        periods <- diff(c(rollout_schedule$day[age_start_ind],global_max_day))
        number_vaccinated_per_day <- rep(rollout_schedule$total[age_start_ind]*age_rollout[age_start_ind],periods)
        local_start_day <- min(rollout_schedule$day[age_start_ind])
        local_max_day <- global_max_day - local_start_day 
        
        profile1 <- get_vaccine_effect_profile(number_vaccinated_per_day=number_vaccinated_per_day,wane_time=wane_time, initial_VE=initial_VE, 
                                               max_day=local_max_day,sero_time=sero_time,mat_time=mat_time,one_compartment = F)
        for(ii in 1:length(profile1)) assign(names(profile1)[ii],profile1[[ii]])
        
        gamma_ratios[[colnames(vaccine_rollout_schedule)[age_indices[i]]]][(local_start_day+1):global_max_day] <- gamma_ratio
      }
        # cx <- 1.7
      # x11()
      # par(mfrow=c(3,1),mar=c(5,7,1,1))
      # plot(day_vector,residual_V_effect,ylim=0:1,typ='l',col='turquoise',lwd=4,xlab='Days since vaccination',ylab='Relative vaccine effect\n (one person)',cex.lab=cx,cex.axis=cx,frame=F)
      # 
      # plot(day_vector,number_vaccinated_per_day,typ='l',col='slateblue3',lwd=4,xlab='Days since rollout started',ylab='People vaccinated per day',cex.lab=cx,cex.axis=cx,frame=F)
      # 
      # plot(day_vector[seq(10,max_day,by=10)]/10,lwd=4,ylim=0:1,frame=F,cex.axis=cx,cex.lab=cx,xlab='Days since rollout started',ylab='Relative vaccine effect\n (all vaccinated people)',
      #      medians,col=alpha('navyblue',1),typ='l')
      # lines(day_vector[seq(10,max_day,by=10)]/10,lwd=2,
      #       quants[1,],col=alpha('navyblue',1),pch=4)
      # lines(day_vector[seq(10,max_day,by=10)]/10,lwd=2,
      #       quants[2,],col=alpha('navyblue',1),pch=4)
      # x11()
      # par(mfrow=c(1,1),mar=c(5,5,1,1))
      # plot(mean_effect,fgammahat,typ='l',col='navyblue',lwd=4,xlab=TeX('$\\hat{\\gamma}(t)$'),ylab=TeX('$f(\\hat{\\gamma},t)$'),cex.lab=1.2,cex.axis=1.2,frame=F)
      # text(c(TeX('$x=y$')),x=0.7,y=0.725,col=c('grey'),srt=45,cex=1.2)
      # lines(range(mean_effect),range(mean_effect),col='grey',lwd=2,lty=2)
    }
    write.csv(gamma_ratios,file.path(out_path,paste0('waned_infected',file_lab[j],'.csv')),row.names = F)
  }
}

## 21 schools and gvapm again #########################################
cat('schools\n')


temp <- tempfile()
download.file("https://en.unesco.org/sites/default/files/covid_impact_education_full.csv.zip",temp, mode="wb")
fle <- unzip(temp,exdir=multi_country_inputs_path)
school_dta <- read.csv(fle,stringsAs=F)
unlink(temp)

cschool <- subset(school_dta,ISO==country_abbr)
change_times <- which(cschool$Status[-1]!=cschool$Status[-nrow(cschool)])
schedule <- cschool[1+c(0,change_times),]
schedule$Date <- as.Date(schedule$Date,"%d/%m/%Y")
schedule$end <- c(schedule$Date[-1],today())

saveRDS(schedule,file.path(country_inputs_path,"schools.Rds"))

## mexico school data

schoolsheet <- readxlsx(file.path(country_inputs_path,'School attendance MX.xlsx'),sheet=1)
colnames(schoolsheet) <- schoolsheet[1,]
schoolsheet <- schoolsheet[-1,]
for(i in 2:5){
  x <- schoolsheet[,i]
  x[x=='BREAK'] <- '0'
  schoolsheet[,i] <- as.numeric(unlist(x))
}
##!! could be weighted
schoolx <- apply(schoolsheet[,2:5],1,mean)
# *Schools were fully open until March 23rd (public)
schoolx[1] <- schoolx[1]*23/31
# *Schools were officially re-opened in Sept 2021, but there has been a gradual return to fully open schools depending on private vs. public and geographical aspects.

source('code/Processing/economic_growth.R')
# growth <- T
# if(growth){
#   x_mat_old <- x_mat
  x_mat <- read.csv(file.path(out_path,'x_values_growth.csv'),stringsAs=F)
  # x_mat[,22:24] <- x_mat_old[,22:24]
# }
## add to x_mat
write.csv(x_mat,file.path(country_inputs_path,'x_values_raw.csv'),row.names=F)
x_mat[edSector,] <- 1
x_mat[edSector,-c(1:2)] <- schoolx
write.csv(x_mat,file.path(out_path,'x_values.csv'),row.names=F)

## 22 save sectors ###########################################
cat('save sectors\n')

xm <- apply(x_mat,1,min)

sector_summary <- cbind(data.frame(Sector=sec_names,
                                   Workforce=workforce,
                                   GVApm=obj,
                                   x_min=xm,
                                   WFH=wfh,
                                   consumption_frac_of_final_demand=consumption_frac_of_final_demand,
                                   B=B_sector),C_sector_df)
write.csv(sector_summary,file.path(out_path,'sector_summary.csv'),row.names = F)




## 23 age-dependent rates ###########################################
cat('age-dependent rates\n')

datafile <- file.path(country_inputs_path,'MEXsurveillancedata.Rds')
if(!file.exists(datafile)){
  # https://www.gob.mx/salud/documentos/datos-abiertos-152127
  filenames <- list.files(country_inputs_path)
  fulldata <- read.csv(file.path(country_inputs_path,filenames[grepl('COVID19MEXICO',filenames)]),stringsAs=F)
  
  setDT(fulldata)
  fulldata[,admitted:=TIPO_PACIENTE==2]
  fulldata[,died:=FECHA_DEF!='9999-99-99']
  fulldata[,admission_date:=FECHA_INGRESO]
  fulldata[,age:=EDAD]
  fulldata[,case:=CLASIFICACION_FINAL%in%1:3]
  
  workingdata <- fulldata[case==T,.(admitted,died,admission_date,age)]
  workingdata[,day:=get_day_from_jan1(admission_date),by=admission_date]
  workingdata[,week:=floor(day/7)+1]
  workingdata[,agegroup:=floor(age/10)+1]
  saveRDS(workingdata,datafile)
  
}else{
  workingdata <- readRDS(datafile)
}

workingdata[,age_group:=cut(age,breaks=c(-1,4,19,64,100),labels=c('age0to4','age5to19','age20to64','age65plus')),by=age]
admissions <- workingdata[admitted==T,.N,by=day]
admissions_by_age <- workingdata[admitted==T,.N,by=.(day,age_group)]
saveRDS(dcast(admissions_by_age[!is.na(age_group),],day~age_group,fill=0),file.path(country_inputs_path,'admissions_by_age.Rds'))
setkey(admissions)
saveRDS(admissions,file.path(country_inputs_path,'admissions.Rds'))

m_deaths <- c(77,146,1244,4396,11796,21402,25103,19251,10310)
f_deaths <- c(69,141,624,1752,5088,10482,14738,12183,7173)
surv_m_deaths <- c(73,81,690,2683,7445,13592,16267,12625,6507)
surv_f_deaths <- c(55,82,405,1219,3406,7048,9880,7864,4295)
m_cases <- c(1748672, 2939712, 3391185, 1868550, 2648950, 1325576, 834357,468058,175471)
f_cases <- c(2180160, 2410149, 2917249, 2598867, 2570133, 1530997, 1031111, 350392,115163)
IFR <- (m_deaths+f_deaths)/(m_deaths+f_deaths+m_cases+f_cases)
reportingrate <- (surv_m_deaths+surv_f_deaths)/(m_deaths+f_deaths)
rrdf <- data.frame(x=1:9,rr=reportingrate)
newrrdf <- data.frame(x=seq(7.5,90,by=5)/10)
suppressMessages(mod <- glm(rr~ns(x,df=7),data=rrdf,family=binomial))
newrrdf$rr <- predict(mod,newdata=newrrdf,type = 'response')

ageseries4 <- workingdata[day<450&age<100,.N,by=.(admitted,died,age_group)]
ageseries4[,total:=sum(N),by=age_group]
ageseries4[admitted==T,total_admitted:=sum(N),by=.(age_group)]
ageseries4[admitted==T,total_notadmitted:=total-total_admitted]
ageseries4[died==T,total_died:=sum(N),by=age_group]
ageseries4[died==T&admitted==T,total_diednotadmitted:=total_died-N,by=age_group]
ageseries4[,pAgivenS:=total_admitted/total]
ageseries4[died==T&admitted==T,pFgivenAS:=N/total_admitted]
ageseries4[died==T&admitted==T,pFgivenAbarS:=total_diednotadmitted/total_notadmitted]
ageseries4[died==T&admitted==T,pFgivenS:=total_died/total] ##!! includes non-admitted deaths
AgivenN <- with(subset(ageseries4,died==T&admitted==T),sum(total_admitted)/sum(total_admitted+total_diednotadmitted))

setkey(ageseries4)
probtable4 <- ageseries4[died==T&admitted==T,.(age_group,pAgivenS,pFgivenAS,pFgivenAbarS,pFgivenS)]
setkey(probtable4)
probtable4[,pNgivenS:=pAgivenS+pFgivenAbarS*(1-pAgivenS)]
IFR4 <- sapply(list(1,1:2,3:8,8:9),function(x)sum(m_deaths[x]+f_deaths[x])/sum(m_cases[x]+f_cases[x]))
probtable4$pF <- IFR4
probtable4[,pSgivenF:=c(newrrdf$rr[1],mean(newrrdf$rr[2:4]),mean(newrrdf$rr[5:13]),mean(newrrdf$rr[14:17]))]#.7]
probtable4[,pS:=pSgivenF*pF/pFgivenS]
probtable4[,pN:=pF*pNgivenS/pFgivenS]
probtable4[,pFgivenN:=pF/pN]
pSymptomatic <- as.numeric(parameters$Value[parameters$Parameter=='p1'])
parameters_by_age <- data.frame(ihr=probtable4$pN/pSymptomatic,ifr=probtable4$pF/pSymptomatic)
parameters_by_age$hfr <- round(parameters_by_age$ifr/parameters_by_age$ihr,3)
parameters_by_age$rr <- round(probtable4$pSgivenF,3)
write.csv(parameters_by_age,file.path(out_path,'parameters_by_age.csv'),row.names=F)
rownames(parameters_by_age) <- c('0 to 4', '5 to 19', '20 to 64', '65 plus')
colnames(parameters_by_age) <- c('sCHR','sCFR','HFR','Reporting rate')

# ihr <- readxlsx(file.path(country_inputs_path,'EPI.xlsx'),sheet=2)
# ifr <- readxlsx(file.path(country_inputs_path,'EPI.xlsx'),sheet=3)
# parameters_by_age <- read.csv(file.path(multi_country_inputs_path,'parameters_by_age.csv'),stringsAs=F)
# age_groups <- pop_by_age$Age_group
# age_groups17 <- age_groups[1:17]
# age_groups17[17] <- paste0(strsplit(age_groups[17],' ')[[1]][1],' to ',strsplit(age_groups[18],' ')[[1]][3])

# parameters_by_age$ihr <- ihr$SCHRpc/100 # as.numeric(sapply(ihr$`SYMPTOMATIC CASE HOSPITALISATION RATE (%)`,function(x)strsplit(x,' \\(')[[1]][1]))/100
# parameters_by_age$ifr <- ifr$SCFRpc/100 # as.numeric(sapply(ifr$`SYMPTOMATIC CASE FAT