#### New results covidestim STATES ####
#### For counties: rerun covidestim with all counties!
### functions ####
solveOR <- function(p_dx_and_inf,p_vac,OR){
  t1 = 1 + OR*p_dx_and_inf - p_dx_and_inf - OR*p_vac - p_vac
  t2 = OR - p_vac*OR
  b = (-t1 + (t1^2 + 4*p_vac*t2)^0.5) / (2*t2)
  o_inf = b*OR
  cp_vac_not_inf = b/(1+b)
  cp_vac_inf = o_inf/(1+o_inf)
  p_vac_not_inf = cp_vac_not_inf*(1-p_dx_and_inf)
  p_vac_inf = cp_vac_inf*p_dx_and_inf
  p_not_vac_not_inf = (1-cp_vac_not_inf)*(1-p_dx_and_inf)
  p_not_vac_inf = (1-cp_vac_inf)*p_dx_and_inf
  pct_imm = p_vac_inf + p_vac_not_inf + p_not_vac_inf
  d_inf_vac = p_dx_and_inf - p_vac
  pct_imm
}

### vacinf
solveVacInf <- function(cuminf, vaccinated, immune){
  (cuminf + vaccinated - immune) / cuminf
}

## waning
waningFunc <- function(obs, 
                       prob = FALSE,
                       waning){
  out <- rep(NA,length(obs))
  
  for(i in 1:length(obs)){
    if(length(prob) == 1) {
      out[i] <- obs[1:i] %*% waning[i:1]
    } else{
      out[i] <- (obs[1:i] * (1-min(max(prob[i],0),1))) %*% waning[i:1]
    }
  }
  out
}
## waning
waningFuncB <- function(obs, 
                       prob = FALSE,
                       waning,
                       boost){
  nobs <- length(obs)
  out <- rep(NA,nobs)
  
  if(length(prob) == 1)    prob = rep(0, nobs)
  if(length(boost) == 1)   boost = rep(0, boost)
  
  for(i in 1:nobs){
      out[i] <- (obs[1:i] * (1-min(max(prob[i],0),1)) * (1-boost[i])) %*% waning[i:1]
  }
  out
}

### noPeaks function
noPeaks <- function(x){
  revx <- rev(x)
  mono_bw <- revx
  mono_fw <- x
  
  #create a monotonic timeseries going forward and backward
  for(i in 2:length(x)){
    if(mono_bw[i] > mono_bw[i-1]) {mono_bw[i] <- mono_bw[i-1]}
    if(mono_fw[i] < mono_fw[i-1]) {mono_fw[i] <- mono_fw[i-1]}
  }
  
  mono_fw_rev <- rev(mono_fw)
  # moving backwards from last observation, 
  # selecting the maximum of the monotonic backwards
  # and monotonic forwards series -- if they are smaller
  # than the previous observation.
  for(i in 2:length(x)){
    revx[i] <- ifelse(mono_bw[i] <= revx[i-1],
                      ifelse(mono_fw_rev[i] <= revx[i-1],
                             max(mono_bw[i],mono_fw_rev[i])[1],
                             mono_bw[i]),
                      mono_fw_rev[i])
  }
  rev(revx)
}

## pivot a data frame into longer (with interval variable; if state)
pivotDF <- function(infD, state = FALSE){
  if(state == TRUE){
    infD %>%
      pivot_longer(-c(starts_with("state"),starts_with("date"))) %>% 
      mutate(interval = if_else(str_detect(name, '.hi'), 
                                "high", 
                                if_else(str_detect(name, '.lo'), 
                                        "low",
                                        "median")
      ),
      type = str_remove(str_remove(name, '.lo'), '.hi')) %>% 
      select(-name) %>% 
      pivot_wider(names_from = type) -> infD1
  } else{
    infD %>% 
      mutate(interval = "median") -> infD1
  }
  return(infD1)
}

pivotOdds <- function(oddsD, state = FALSE){
  if(state == TRUE){
    oddsD %>% transmute(
      state = state,
      odds.hi = exp(qnorm(.025, mu, sd)),
      odds = exp(qnorm(.5, mu, sd)),
      odds.lo = exp(qnorm(.975, mu, sd))) -> oddsD1
  } else {
    oddsD %>% transmute(
      fips = fips, 
      odds = exp(qnorm(.5, mu, sd))
    ) -> oddsD1
  }
  return(pivotDF(oddsD1, state = state))
}
