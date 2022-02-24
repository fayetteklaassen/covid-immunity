library(tidyverse)
setwd("data/raw/")

#### Collect data files ####
nfiles <- length(list.files("HHPS", pattern = ".csv"))

dat_lst <- list()

for(i in 1:nfiles){
  dat_lst[[i]] <- data.table::fread(paste0("HHPS/pulse2021_puf_",23+i,".csv"))
}

### Subset and rbind data
var_save <- c("WEEK","EST_ST","EST_MSA","PWEIGHT","RECVDVACC","HADCOVID")
for(i in 1:nfiles){
  dat_lst[[i]] <- as.data.frame(dat_lst[[i]])[,var_save]
}

dat <- dat_lst[[1]] 
for(i in 2:nfiles){
  dat <- rbind(dat,dat_lst[[i]])
}

#### Filter and save raw data ####
### Remove observations without clear answer to key variables
dat2 <- dat[dat$RECVDVACC%in%(1:2) & dat$HADCOVID%in%(1:2),]

setwd("../analyzed")
saveRDS(dat, "HHPS/HHPS-raw.RDS") 
saveRDS(dat2, "HHPS/HHPS-filtered.RDS")

# recode of relevant variables
dat2$RECVDVACC <- ifelse(dat2$RECVDVACC==1,1,0)
dat2$HADCOVID <- ifelse(dat2$HADCOVID==1,1,0)
dat2$loc <- dat2$EST_ST
dat2$loc <- factor(dat2$loc ,levels = unique(dat2$loc), labels = 1:length(unique(dat2$loc)))
dat2$week <- factor(dat2$WEEK, levels = unique(dat2$WEEK), labels = 1:length(unique(dat2$WEEK)))

# check the location specific odds ratios
dat2 %>% group_by(loc) %>% summarize(pvac_if_inf = sum(PWEIGHT[HADCOVID == 1 & RECVDVACC == 1])/
                                       sum(PWEIGHT[HADCOVID ==1]),
                                     pvac_no_inf = sum(PWEIGHT[HADCOVID == 0 & RECVDVACC == 1])/
                                       sum(PWEIGHT[HADCOVID == 0]),
                                     o_vifi = pvac_if_inf/(1-pvac_if_inf),
                                     o_vnoi = pvac_no_inf/(1-pvac_no_inf),
                                     or = o_vifi / o_vnoi)


dat2 %>% 
  arrange(EST_ST) %>% 
  # compute the effective sample size
  mutate(effn = sum(PWEIGHT)^2 / sum(PWEIGHT^2),
         obsn = sum(PWEIGHT),
         effW = PWEIGHT/obsn*effn) %>%
  group_by(EST_ST, loc, week, HADCOVID) %>%
  summarize(y0 = sum(PWEIGHT[RECVDVACC == 0]),
            y1 = sum(PWEIGHT[RECVDVACC == 1]),
            u = sum(PWEIGHT), 
            effy1 = sum(effW[RECVDVACC == 1]),
            effu =sum(effW),
            .groups= 'drop')%>%
  ungroup() %>% 
  mutate(odds = y1/y0,
         oddseff = effy1 / (effu-effy1)) -> HHPS

# check results
HHPS %>% group_by(loc, week) %>% 
  summarize(OR = odds[HADCOVID == 1]/odds[HADCOVID ==0])

#### Write results ####
saveRDS(HHPS, "HHPS/HHPS-eff-ss.RDS")
setwd("../..")
