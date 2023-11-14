get_vaccine_effect_profile <- function(number_vaccinated_per_day=NULL,wane_time=365, initial_VE=0.8, max_day=400, 
                                       sero_time=14, mat_time=28,one_compartment=T,simple_approx=F){
  
  # rate parameters
  kw <- 1/wane_time
  km <- 1/mat_time
  ks <- 1/sero_time
  
  # vector of all days
  day_vector <- 1:max_day
  
  # if missing, lay out rollout
  if(is.null(number_vaccinated_per_day))
    number_vaccinated_per_day <- c(1:100,rep(100,100),rep(100,max_day-200))
  # get cumulative number vaccinated
  vaccinees <- cumsum(number_vaccinated_per_day)
  # get probability to be vaccinated per day
  p_time <- number_vaccinated_per_day/sum(number_vaccinated_per_day)
  
  ## first approximation ##################################
  expectedA <- NULL
  VEs <- NULL
  days <- NULL
  # remaining vaccine effect is deterministic function of number of days of waning
  residual_V_effect <- exp(-kw*day_vector)
  if(simple_approx){
    days <- unlist(sapply(seq(10,max_day,by=10),function(x)rep(day_vector[x],vaccinees[x])))
    # basic expectation assuming simple waning
    expectedA <- sapply(day_vector,function(y)
      #y is the time we are on
      #x is the day they were vaccinated
      #y-x is the number of days of waning
      sum(sapply(1:y,function(x)number_vaccinated_per_day[x]*residual_V_effect[y-x+1])))
    
    states_per_day <- lapply(day_vector,function(y)
      unlist(sapply(1:y,function(x)rep(residual_V_effect[y-x+1],round(number_vaccinated_per_day[x])))))
    
    VEs <- unlist(sapply(seq(10,max_day,by=10),function(x)states_per_day[[x]]))
  }
  
  ## functions ################################
  # function p(gamma,t) assuming simple model
  p_Veffectt_1c <- function(tval) {
    end_day <- which(day_vector==round(tval))
    # probability vaccinated on day, given vaccine rollout up to that day
    p_time_so_far <- p_time[1:end_day]/sum(p_time[1:end_day])
    possible_gamma_values <- sapply(1:end_day,function(tv){
      exp(-kw*(day_vector[end_day]-day_vector[tv]))
    })
    # probability to have gamma is simple function of time of vaccination, = probability vaccinated that number of days ago
    cbind(possible_gamma_values,p_time_so_far)
  }
  
  # function p(gamma,t) assuming more complicated model
  p_Veffectt_2c <- function(tval) {
    end_day <- which(day_vector==round(tval))
    gamma_values <- rep(0,end_day)
    for(tv in 1:end_day)
      gamma_values[tv] <- exp(-kw*(day_vector[end_day]-day_vector[tv]))
    # make table of all people vaccinated on day i and mature on day j
    p_vax_day <- p_time[1:end_day]
    norm_p_vax_day <- p_vax_day/sum(p_vax_day)
    mattab <- duration_mat[1:end_day,1:end_day,drop=F]
    for(i in 1:end_day)
      mattab[i,] <- mattab[i,] * norm_p_vax_day[i]
    # normalise
    norm_mattab <- (mattab+1e-16)/sum(mattab+1e-16)
    gamma_probs <- colSums(norm_mattab)
    # return
    cbind(gamma_values,gamma_probs)
  }
  
  # choose which model for p(gamma,t)
  if(one_compartment==T){
    p_Veffectt <- p_Veffectt_1c
  }else{
    p_Veffectt <- p_Veffectt_2c
  } 
  duration_mat <- matrix(0,nrow=length(day_vector),ncol=length(day_vector))
  for(i in 1:length(day_vector)){
    for(j in i:length(day_vector)){
      mat_time <- day_vector[j] - day_vector[i]
      p_duration <- km*ks/(km-ks) * (exp(-ks*mat_time) - exp(-km*mat_time))
      duration_mat[i,j] <- p_duration 
    }
  }
  p_V <- list()
  for(i in 1:length(day_vector))
    p_V[[i]] <- p_Veffectt(day_vector[i])
  
  # function for expected value of gamma
  E_Veffectt <- function(tval) {
    day_index <- which(day_vector==round(tval))
    p <- p_V[[day_index]]
    sum(p[,1]*p[,2])
  }
  # function for quantiles of gamma
  q_Veffectt <- function(tval) {
    day_index <- which(day_vector==round(tval))
    p <- p_V[[day_index]]
    quant <- cumsum(p[,2])
    q1 <- which.min(abs(quant-0.05))
    q2 <- which.min(abs(quant-0.95))
    p[c(q1,q2),1]
  }
  # function for median of gamma
  med_Veffectt <- function(tval) {
    day_index <- which(day_vector==round(tval))
    p <- p_V[[day_index]]
    quant <- cumsum(p[,2])
    q1 <- which.min(abs(quant-0.5))
    p[q1,1]
  }
  
  ## computations ##############################################
  
  # get p(gamma) for all days
  p_Veffect <- p_V # lapply(day_vector,p_Veffectt)
  
  # get mean, median, quantiles of gamma
  medians <- sapply(seq(10,max_day,by=10),function(x) med_Veffectt(x))
  quants <- sapply(seq(10,max_day,by=10),function(x) q_Veffectt(x))
  mean_effect <- sapply(day_vector, E_Veffectt)

  # get pI(gamma) for all days
  p_infected <- lapply(p_Veffect,function(x){
    gammas <- x[,1]
    initial_probs <- x[,2]
    tmp <- initial_probs * (1 - gammas*initial_VE) + 1e-16
    probs <- tmp/sum(tmp)
    cbind(gammas,probs)
  })
  
  # get f(gamma)
  fgammahat <- sapply(p_infected,function(x){
    gammas <- x[,1]
    probs <- x[,2]
    sum(gammas*probs)
  })
  
  # get f(gamma)/mean(gamma) for all t
  gamma_ratio <- fgammahat/mean_effect
  
  list(day_vector=day_vector,
       residual_V_effect=residual_V_effect,
       number_vaccinated_per_day=number_vaccinated_per_day,
       max_day=max_day,
       medians=medians,
       quants=quants,
       mean_effect=mean_effect,
       fgammahat=fgammahat,
       expectedA=expectedA,
       vaccinees=vaccinees,
       VEs=VEs,
       days=days,
       gamma_ratio=gamma_ratio)
}