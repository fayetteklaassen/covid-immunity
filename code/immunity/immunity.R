calcImmunity <- function(vaxD, # location date vaccinated
                      infD, # location date cum.incidence nterval
                      boostD, # location date boost
                      oddsD, # location date odds interval
                      popD, # location pop
                      pop12D, # locataion propunder12
                      state = FALSE,
                      maxDate = as.Date("2021-12-01"),
                      waneScn,
                      scn.omi.df){
  
  location <- if(state == TRUE) "state" else "fips"

    ## Merge data files
    infD %>%
    left_join(popD, by = location) %>%
    left_join(pop12D, by = location) %>%
    left_join(oddsD, by = c(location, "interval")) %>%
    left_join(vaxD, by = c(location, "date")) %>%
    mutate(vaccinated = if_else(is.na(vaccinated),
                                0,
                                vaccinated)) %>%
    left_join(boostD, by = c(location, "date")) %>%
    filter(date <= maxDate) %>%
    rename_all(recode, fips = "location", state = "location") %>%

    # Replace NA booster data by zero
    mutate(boost = if_else(is.na(boost),
                           0,
                           boost/100)) %>%
    group_by(location, interval) %>%
    arrange(date) %>%
    # recalculate vaccinated percentages
    mutate(vaccinated12 = vaccinated / ( 1- propunder12),
           vaccinated12 = if_else(vaccinated12 > .999,
                                  .999,
                                  vaccinated12),
           vaccinated = vaccinated12 * (1 - propunder12),
           boost = if_else(boost > vaccinated,
                           vaccinated,
                           boost),
           vac.p = c(vaccinated[1], diff(vaccinated))) %>%
    ungroup() %>%
    mutate(
      cum.inf.p = cum.incidence / pop,
      immune12p = solveOR(cum.inf.p, vaccinated12, odds),
      immune = immune12p * (1 - propunder12) + cum.inf.p * propunder12,
      pvacinf = solveVacInf(cum.inf.p, vaccinated12, immune12p),
      pvacinf = if_else(pvacinf < 0,
                        0,
                        if_else(pvacinf > 1,
                                1,
                                pvacinf)),
      both.cum12 = vaccinated12 + cum.inf.p - immune12p,
      both.cum12 = if_else(both.cum12 < 0,
                           0, 
                           both.cum12 ),
      both.cum = both.cum12 * (1 - propunder12),
      vac.only.cum12 = vaccinated12 - both.cum12,
      vac.only.cum = if_else(vac.only.cum12 < 0,
                             0,
                             vac.only.cum12 * (1 - propunder12)),
      boost.vac = if_else( boost > vaccinated,
                           1,
                           if_else(vaccinated <= 0,
                                   0,
                                   boost / vaccinated)),
    ) %>%
    group_by(location, interval) %>%
    arrange(date) %>%
    mutate(
      inf.p = c(cum.inf.p[1], diff(cum.inf.p)),
      both.p = c(both.cum[1], diff(both.cum)),
      vac.only = c(vac.only.cum[1], diff(vac.only.cum)),
    ) %>%
    ungroup() %>%
      # create empty placeholder variables
    mutate(
      inf.only = NA,
      vac.only = NA,
      vac.boost = NA,
      both.only = NA,
      both.boost = NA,
      immune.waning.inf = 0,
      immune.waning.vac = 0,
      immune.waning.both = 0,
      immune.waning.vacboost = 0,
      immune.waning.bothboost = 0,
      immune.waning = 0,
      immune.severe.waning.inf = 0,
      immune.severe.waning.vac = 0,
      immune.severe.waning.both = 0,
      immune.severe.waning.vacboost = 0,
      immune.severe.waning.bothboost = 0,
      immune.severe.waning = 0,
    ) -> dat
    
  
  allLoc <- unique(dat$location)
  interval <- unique(dat$interval)  

  res <- vector("list", length(waneScn))
  
    for(i in 1:length(waneScn)){ #i = 1
    waneI <- waneScn[[i]][["wanei"]]
    waneIboth <- waneScn[[i]][["waneiboth"]]
    waneS <- waneScn[[i]][["wanes"]]
    waneSboth <- waneScn[[i]][["wanesboth"]]
    
    for(j in 1:length(allLoc)){ # j = 8
      for(k in 1:length(unique(dat$interval))) {
      
      inf.p <- dat$inf.p[which(dat$location == allLoc[j] & dat$interval == interval[k])]
      vac.p <- dat$vac.p[which(dat$location == allLoc[j] & dat$interval == interval[k])]
      pvacinf <- dat$pvacinf[which(dat$location == allLoc[j] & dat$interval == interval[k])]
      cum.inf.p <- dat$cum.inf.p[which(dat$location == allLoc[j] & dat$interval == interval[k])]
      both.p <- dat$both.p[which(dat$location == allLoc[j] & dat$interval == interval[k])]
      boost.p <- dat$boost.vac[which(dat$location == allLoc[j] & dat$interval == interval[k])]
      
      dat$inf.only[which(dat$location == allLoc[j] & dat$interval == interval[k])] = waningFunc(inf.p, pvacinf, rep(1, 1000))
      dat$vac.only[which(dat$location == allLoc[j] & dat$interval == interval[k])] = waningFuncB(vac.p, cum.inf.p, rep(1,1000), boost.p)
      dat$vac.boost[which(dat$location == allLoc[j] & dat$interval == interval[k])] = waningFuncB(vac.p, cum.inf.p, rep(1,1000), 1-boost.p)
      dat$both.only[which(dat$location == allLoc[j] & dat$interval == interval[k])] = waningFuncB(both.p, FALSE, rep(1,1000), boost.p)
      dat$both.boost[which(dat$location == allLoc[j] & dat$interval == interval[k])] = waningFuncB(both.p, FALSE, rep(1,1000), 1- boost.p)
      
      dat$immune.waning.inf[which(dat$location == allLoc[j] & dat$interval == interval[k])] = waningFunc(inf.p, pvacinf, waneI)
      dat$immune.waning.vac[which(dat$location == allLoc[j] & dat$interval == interval[k])] = waningFuncB(vac.p, cum.inf.p, waneI, boost.p)
      dat$immune.waning.vacboost[which(dat$location == allLoc[j] & dat$interval == interval[k])] = waningFuncB(vac.p, cum.inf.p, waneIboth, 1-boost.p)
      dat$immune.waning.both[which(dat$location == allLoc[j] & dat$interval == interval[k])] = waningFuncB(both.p, FALSE, waneIboth, boost.p)
      dat$immune.waning.bothboost[which(dat$location == allLoc[j] & dat$interval == interval[k])] = waningFuncB(both.p, FALSE, waneIboth, 1-boost.p)
      dat$immune.waning[which(dat$location == allLoc[j] & dat$interval == interval[k])] = dat$immune.waning.inf[which(dat$location == allLoc[j] & dat$interval == interval[k])] + 
        dat$immune.waning.vac[which(dat$location == allLoc[j] & dat$interval == interval[k])] + 
        dat$immune.waning.vacboost[which(dat$location == allLoc[j] & dat$interval == interval[k])] + 
        dat$immune.waning.both[which(dat$location == allLoc[j] & dat$interval == interval[k])] + 
        dat$immune.waning.bothboost[which(dat$location == allLoc[j] & dat$interval == interval[k])] 
      
      dat$immune.severe.waning.inf[which(dat$location == allLoc[j] & dat$interval == interval[k])] = waningFunc(inf.p, pvacinf, waneS)
      dat$immune.severe.waning.vac[which(dat$location == allLoc[j] & dat$interval == interval[k])] = waningFuncB(vac.p, cum.inf.p, waneS, boost.p)
      dat$immune.severe.waning.vacboost[which(dat$location == allLoc[j] & dat$interval == interval[k])] = waningFuncB(vac.p, cum.inf.p, waneSboth, 1-boost.p)
      dat$immune.severe.waning.both[which(dat$location == allLoc[j] & dat$interval == interval[k])] = waningFuncB(both.p, FALSE, waneSboth, boost.p)
      dat$immune.severe.waning.bothboost[which(dat$location == allLoc[j] & dat$interval == interval[k])] = waningFuncB(both.p, FALSE, waneSboth, 1- boost.p)
      dat$immune.severe.waning[which(dat$location == allLoc[j] & dat$interval == interval[k])] = dat$immune.severe.waning.inf[which(dat$location == allLoc[j] & dat$interval == interval[k])] + 
        dat$immune.severe.waning.vac[which(dat$location == allLoc[j] & dat$interval == interval[k])] + 
        dat$immune.severe.waning.vacboost[which(dat$location == allLoc[j] & dat$interval == interval[k])] + 
        dat$immune.severe.waning.both[which(dat$location == allLoc[j] & dat$interval == interval[k])] +
        dat$immune.severe.waning.bothboost[which(dat$location == allLoc[j] & dat$interval == interval[k])] 
      }
    }
    dat$scn.waning <- names(waneScn)[i]
    res[[i]] <- dat
    }
  
  df <- bind_rows(res)
  
    df %>% 
      group_by(location, date) %>%
      summarize() -> locdate
    
    scn.omi.dat <- data.frame(scn.omi.df,
                              location= rep(locdate$location, each = 3),
                              date = rep(locdate$date, each = 3))
    
    df %>% 
      left_join(scn.omi.dat, by = c("location", "date")) %>%
      mutate(immune.waning = if_else(immune.waning <= 0,
                                     0,
                                     immune.waning)) %>%
      mutate(immune.waning.omi = 
                  immune.waning.vac * (1-vac.ev) + 
                  immune.waning.vacboost * (1-boost.ev) + 
                  immune.waning.inf *(1-inf.ev) + 
                  immune.waning.both *(1-both.ev) +
                  immune.waning.bothboost *(1-boost.ev),
                immune.severe.waning.omi = 
                  immune.severe.waning.vac * (1-vac.evs) + 
                  immune.severe.waning.vacboost * (1-boost.evs) + 
                  immune.severe.waning.inf *(1-inf.evs) + 
                  immune.severe.waning.both *(1-both.evs)+
                  immune.severe.waning.bothboost *(1-boost.evs)
                
      ) -> res2
    
    return(res2)

}