source('code/Processing/get_waning_profile.R')

profile1 <- get_vaccine_effect_profile(number_vaccinated_per_day=NULL,wane_time=365, initial_VE=0.8, max_day=400,one_compartment = F)
for(i in 1:length(profile1)) assign(names(profile1)[i],profile1[[i]])


cx <- 1.7
par(mfrow=c(3,1),mar=c(5,7,1,1))
plot(day_vector,residual_V_effect,ylim=0:1,typ='l',col='turquoise',lwd=4,xlab='Days since vaccination',ylab='Relative vaccine effect\n (one person)',cex.lab=cx,cex.axis=cx,frame=F)

plot(day_vector,number_vaccinated_per_day,typ='l',col='slateblue3',lwd=4,xlab='Days since rollout started',ylab='People vaccinated per day',cex.lab=cx,cex.axis=cx,frame=F)

plot(day_vector[seq(10,max_day,by=10)]/10,lwd=4,ylim=0:1,frame=F,cex.axis=cx,cex.lab=cx,xlab='Days since rollout started',ylab='Relative vaccine effect\n (all vaccinated people)',
     medians,col=alpha('navyblue',1),typ='l')
lines(day_vector[seq(10,max_day,by=10)]/10,lwd=2,
      quants[1,],col=alpha('navyblue',1),pch=4)
lines(day_vector[seq(10,max_day,by=10)]/10,lwd=2,
      quants[2,],col=alpha('navyblue',1),pch=4)

par(mfrow=c(1,1),mar=c(5,5,1,1))
plot(mean_effect,fgammahat,typ='l',col='navyblue',lwd=4,xlab=TeX('$\\hat{\\gamma}(t)$'),ylab=TeX('$f(\\hat{\\gamma},t)$'),cex.lab=1.2,cex.axis=1.2,frame=F)
text(c(TeX('$x=y$')),x=0.7,y=0.725,col=c('grey'),srt=45,cex=1.2)
lines(range(mean_effect),range(mean_effect),col='grey',lwd=2,lty=2)

gamma_ratio

max_day <- 500
profile2 <- get_vaccine_effect_profile(number_vaccinated_per_day=c(1:100,rep(100,100),rep(00,200),rep(100,max_day-400)),
                                       wane_time=100, initial_VE=0.8, max_day=max_day, one_compartment = T,simple_approx=T)
for(i in 1:length(profile2)) assign(names(profile2)[i],profile2[[i]])


rng <- range(c(mean_effect))
ratmean_effect <- min(fgammahat/mean_effect)

cx <- 2
par(mfrow=c(3,2),mar=c(5,7,1,1))
plot(day_vector,residual_V_effect,ylim=0:1,typ='l',col='turquoise',lwd=4,xlab='Days since vaccination',ylab='Relative vaccine effect\n (one person)',cex.lab=cx,cex.axis=cx,frame=F)
plot(day_vector,number_vaccinated_per_day,typ='l',col='slateblue3',lwd=4,xlab='Days since rollout started',ylab='People vaccinated per day',cex.lab=cx,cex.axis=cx,frame=F)
plot(day_vector,expectedA/vaccinees,ylim=0:1,typ='l',col='navyblue',lwd=4,xlab='Days since rollout started',ylab='Mean vaccine effect\n (all vaccinated people)',cex.lab=cx,cex.axis=cx,frame=F)
boxplot(VEs~days,ylim=0:1,border='grey',col='white',lwd=1.5,frame=F,cex.axis=cx,cex.lab=cx,xlab='Days since rollout started',ylab='Relative vaccine effect\n (all vaccinated people)')
lines(day_vector[seq(10,max_day,by=10)]/10,lwd=4,
      medians,col=alpha('navyblue',1))

lines(day_vector[seq(10,max_day,by=10)]/10,lwd=2,
      quants[1,],col=alpha('navyblue',1),pch=4)
lines(day_vector[seq(10,max_day,by=10)]/10,lwd=2,
      quants[2,],col=alpha('navyblue',1),pch=4)
plot(mean_effect,fgammahat,typ='l',col='navyblue',lwd=4,xlab=TeX('$\\hat{\\gamma}(t)$'),ylab=TeX('$f(\\hat{\\gamma},t)$'),cex.lab=cx,cex.axis=cx,frame=F)


text(c(TeX('$x=y$')),x=0.6,y=0.65,col=c('grey'),srt=45,cex=2)
lines(rng,rng,col='grey',lwd=2,lty=2)
gamma_ratio


## Indonesia #########################

vaccine_rollout_schedule <- read.csv(file.path(out_path,'second_vaccinations.csv'),stringsAs=F)
booster_rollout_schedule <- read.csv(file.path(out_path,'third_vaccinations.csv'),stringsAs=F)
vaccine_effect_parameter <- read.csv(file.path(out_path,'vaccine_profiles.csv'),stringsAs=F)
days <- diff(c(vaccine_rollout_schedule$day))
totals <- vaccine_rollout_schedule$total[-1]
infants <- vaccine_rollout_schedule$children[-1]
teens <- vaccine_rollout_schedule$X12.to.17.years.old[-1]
adults <- vaccine_rollout_schedule$X18.to.59.years.old[-1]
elders <- vaccine_rollout_schedule$Over.60s[-1]
sum(days*totals*infants)/22000000
sum(days*totals*teens)/67000000
sum(days*totals*adults)/187000000
sum(days*totals*elders)/14500000

vaccine_rollout_schedule[nrow(vaccine_rollout_schedule),] <- c(640,500000,0.33,0.62,0,0.05,0.0)

days <- diff(c(vaccine_rollout_schedule$day,610-1+365))
totals <- vaccine_rollout_schedule$total
infants <- vaccine_rollout_schedule$children
teens <- vaccine_rollout_schedule$X12.to.17.years.old
adults <- vaccine_rollout_schedule$X18.to.59.years.old
elders <- vaccine_rollout_schedule$Over.60s
sum(days*totals*infants)/22000000
sum(days*totals*teens)/67000000
sum(days*totals*adults)/187000000
sum(days*totals*elders)/14500000


start_day <- vaccine_rollout_schedule$day[1]
initial_VE <- vaccine_effect_parameter$Sinovac[vaccine_effect_parameter$Parameter=='Acquisition']
wane_time <- vaccine_effect_parameter$Sinovac[vaccine_effect_parameter$Parameter=='Expected time to wane']
sero_time <- vaccine_effect_parameter$Sinovac[vaccine_effect_parameter$Parameter=='Expected time to seroconversion']
mat_time <- 28
global_max_day <- max(vaccine_rollout_schedule$day)+610-1+365
age_indices <- c(3,4,5,6,7)
gamma_ratios <- data.frame(day=1:global_max_day)
for(i in 1:length(age_indices)) gamma_ratios[[colnames(vaccine_rollout_schedule)[age_indices[i]]]] <- 1


for(i in 1:length(age_indices)){
  age_rollout <- vaccine_rollout_schedule[,age_indices[i]]
  age_start_ind <-  age_rollout > 0
  if(sum(age_start_ind)>0){
    periods <- diff(c(vaccine_rollout_schedule$day[age_start_ind],global_max_day))
    number_vaccinated_per_day <- rep(vaccine_rollout_schedule$total[age_start_ind]*age_rollout[age_start_ind],periods)
    local_start_day <- min(vaccine_rollout_schedule$day[age_start_ind])
    local_max_day <- global_max_day - local_start_day 
    
    profile1 <- get_vaccine_effect_profile(number_vaccinated_per_day=number_vaccinated_per_day,wane_time=wane_time, initial_VE=initial_VE, 
                                           max_day=local_max_day,sero_time=sero_time,mat_time=mat_time,one_compartment = F)
    for(ii in 1:length(profile1)) assign(names(profile1)[ii],profile1[[ii]])
    
    gamma_ratios[[colnames(vaccine_rollout_schedule)[age_indices[i]]]][(local_start_day+1):global_max_day] <- gamma_ratio
    
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
}

write.csv(gamma_ratios,file.path(out_path,'waned_infected.csv'),row.names = F)
