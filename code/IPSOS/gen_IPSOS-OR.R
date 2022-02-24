library(foreign)
library(tidyverse)
library(rstan)
setwd("data/raw/IPSOS/")

dat2 <- read.spss("Axios-Ipsos Wave 41.sav",to.data.frame = T)

table(dat2$Q107_1,dat2$Q21)

# calculate national odds ratio
o_vax_n_inf <- 141/250
o_vax_y_inf <- 13/50

o_vax_y_inf/o_vax_n_inf

##########
# read the IPSOS data 
dat <- list()
for(i in 34:48){ # i=34
  dat[[i-33]] <- read.spss(paste0("Axios-Ipsos Wave ",i,".sav"),to.data.frame = T)
}

setwd("../../..")


## recode / recalculate variables
vax_y_inf_y <- vax_n_inf_y <- vax_y_inf_n <- vax_n_inf_n <- wt <- effW <- list()
y0in <- y0iy <- y1in <- y1iy <- rep(0, 15)
effn <- obsn <- u0 <- u1 <- effy1in <- effy1iy <-effy0in <- effy0iy <- rep(0, 15)
for(i in 1:15){ # i=1
  vax_y_inf_y[[i]] <- dat[[i]]$Q107_1=="Yes, I have received the vaccine" & dat[[i]]$Q21=="Positive for coronavirus"
  vax_n_inf_y[[i]] <- dat[[i]]$Q107_1=="Skipped" & dat[[i]]$Q21=="Positive for coronavirus"
  vax_y_inf_n[[i]] <- dat[[i]]$Q107_1=="Yes, I have received the vaccine" & dat[[i]]$Q21=="Negative for coronavirus"
  vax_n_inf_n[[i]] <- dat[[i]]$Q107_1=="Skipped" & dat[[i]]$Q21=="Negative for coronavirus"
  wt[[i]]          <- dat[[i]]$wt_final
  y0in[i]          <- sum(wt[[i]][vax_n_inf_n[[i]]], na.rm = TRUE)
  y0iy[i]          <- sum(wt[[i]][vax_n_inf_y[[i]]], na.rm = TRUE)
  y1in[i]          <- sum(wt[[i]][vax_y_inf_n[[i]]], na.rm = TRUE)
  y1iy[i]          <- sum(wt[[i]][vax_y_inf_y[[i]]], na.rm = TRUE)
effn[i] <- sum(wt[[i]], na.rm = TRUE)^2 / sum(wt[[i]]^2, na.rm = TRUE)
obsn[i] <- sum(wt[[i]], na.rm = TRUE)
effW[[i]] <- wt[[i]]/obsn[i]*effn[i]
effy1in[i] <- sum(effW[[i]][vax_y_inf_n[[i]]], na.rm = TRUE)
effy1iy[i] <- sum(effW[[i]][vax_y_inf_y[[i]]], na.rm = TRUE)
effy0in[i] <- sum(effW[[i]][vax_n_inf_n[[i]]], na.rm = TRUE)
effy0iy[i] <- sum(effW[[i]][vax_n_inf_y[[i]]], na.rm = TRUE)
u0[i] <- sum(y0in[i], y0iy[i])
u1[i] <- sum(y1in[i], y1iy[i])
}


df <- data.frame("week" = rep(1:15, 2))
df %>% mutate(HADCOVID = rep(0:1, each = 15),
              y0 = c(y0in, y0iy),
              y1 = c(y1in, y1iy),
              u  = c(u0, u1),
              effy1 = c(effy1in, effy1iy),
              effu = c(effy1in + effy0in, effy1iy + effy0iy),
              week = factor(as.numeric(week))) -> df2
dat1 <- list("N" = nrow(df2),
             "W" = length(unique(df2$week)),
             "xweek" = model.matrix(~0+df2$week),
             "xcovid" = df2$HADCOVID,
             "y" = round(df2$effy1,0),
             "u" =round(df2$effu,0))


fit1 <- stan(
  file = "code/IPSOS/ORipsos.stan",  # Stan program
  data = dat1,    # named list of data
  chains = 3,          # number of Markov chains
  warmup = 1000,         # number of warmup iterations per chain
  iter = 2000,           # total number of iterations per chain
  cores = 1,
  refresh = 0 # number of cores (could use one per chain)
)

resall <- summary(fit1, pars = c("b_covid"), probs = c(.025,.975) )
saveRDS(resall, "data/analyzed/IPSOS/out-stan-Ipsos.RDS")


## Weekly odds ratios
o_vax_n_inf <- o_vax_y_inf  <- o_vax_n_inf_w <- o_vax_y_inf_w <- rep(NA,15)

for(i in 1:15){ # i=1
  o_vax_n_inf[i] <- sum(vax_y_inf_n[[i]],na.rm=T)/sum(vax_n_inf_n[[i]],na.rm=T)
  o_vax_y_inf[i] <- sum(vax_y_inf_y[[i]],na.rm=T)/sum(vax_n_inf_y[[i]],na.rm=T)
  o_vax_n_inf_w[i] <- sum(vax_y_inf_n[[i]]*wt[[i]],na.rm=T)/sum(vax_n_inf_n[[i]]*wt[[i]],na.rm=T)
  o_vax_y_inf_w[i] <- sum(vax_y_inf_y[[i]]*wt[[i]],na.rm=T)/sum(vax_n_inf_y[[i]]*wt[[i]],na.rm=T)
}

or_vax_y_inf <- o_vax_y_inf/o_vax_n_inf
or_vax_y_inf_w <- o_vax_y_inf_w/o_vax_n_inf_w

range(or_vax_y_inf_w[-(1:2)])




