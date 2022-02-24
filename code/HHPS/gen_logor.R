####################
# takes about 30 minutes! 
####################
library(tidyverse)
library(rstan)

### Load data ####  
fipsstate <- read_csv("data-sources/fipsstate.csv")
## Generated through 
## - code/HHPS/gen_HHPS.R
## - data/raw/HHPS/<.csv>
HHPS <- readRDS("data/analyzed/HHPS/HHPS-eff-ss.RDS")

HHPS <- HHPS %>% filter(week != "13") %>% mutate(week = factor(as.numeric(week)))
dat1 <- list("N" = nrow(HHPS),
             "L" = length(unique(HHPS$loc)),
             "W" = length(unique(HHPS$week)),
             "xweek" = model.matrix(~0+HHPS$week),
             "xloc" = model.matrix(~0+HHPS$loc),
             "xloccov" = model.matrix(~HHPS$loc:HHPS$HADCOVID-1),
             "xcovid" = HHPS$HADCOVID,
             "y" = round(HHPS$effy1,0),
             "u" =round(HHPS$effu,0))

#### Run Stan model ####
# takes approx 15-30 minutes!
t0 <- Sys.time()
fit1 <- stan(
  file = "code/HHPS/ORstan.stan",  # Stan program
  data = dat1,    # named list of data
  chains = 3,          # number of Markov chains
  warmup = 1000,         # number of warmup iterations per chain
  iter = 2000,           # total number of iterations per chain
  cores = 1,
  refresh = 0 # number of cores (could use one per chain)
)
print(Sys.time() - t0)

resall <- summary(fit1, pars = c("beta", "b_covid"), probs = c(.025,.975) )
saveRDS(resall, "data/analyzed/log-OR/out-stan-OR.RDS")

res1 <- summary(fit1, pars = c("beta"), probs = .5) 

# Plot output
plot(density(rnorm(100000,
                   res1$summary[1,1], 
                   res1$summary[1,3])), 
     xlim = c(-1,0),
     ylim = c(0,10),
     xlab = "Log odds ratio",
     main = "State specific prior distributions of log OR")
for(i in 2:52){
  lines(density(rnorm(100000, 
                      res1$summary[i,1], 
                      res1$summary[i,3])))
} 

#### Map results to location names ####
HHPS %>% mutate(loc2 = as.numeric(loc)) %>%
  group_by(EST_ST, loc2) %>%
  summarize(.groups = 'drop') %>%
  rename(loc = loc2) -> locmap

allstates <- c(state.name[1:8], "District of Columbia", state.name[9:50])

stateOR <- as.data.frame(res1$summary ) %>%
  mutate(loc = 1:51) %>%
  left_join(locmap, by = "loc") %>% 
  arrange(EST_ST) %>%
  mutate(state = allstates) %>%
  rename(mu = mean) %>%
  arrange(mu) %>%
  select(state, mu, sd)

rownames(stateOR) <- NULL

manFips <- data.frame("fips" = c("02158", "46102"), 
                      "state" = c("Alaska", "South Dakota"))

allFips <- rbind(fipsstate, manFips)

fipsOR <- stateOR %>% 
  left_join(allFips, by = "state") %>%
  select(fips,mu,sd) 

#### Write results ####
write_csv(stateOR, "data/analyzed/log-OR/logor-vac-state.csv")
write_csv(fipsOR, "data/analyzed/log-OR/logor-vac-fips.csv")

