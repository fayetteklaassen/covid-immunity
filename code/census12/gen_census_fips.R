#### COUNTY ####

# read in the vaccineAdjust Census data (which has agegroups)
load("https://github.com/covidestim/vaccineAdjust/raw/master/R/sysdata.rda")

Census %>% 
  group_by(FIPS) %>%
  mutate(total = sum(census_age0to18_ct, 
                     census_age18to64_ct, 
                     census_age65to99_ct),
         propunder12 = census_age0to12_ct/total) %>%
  ungroup() %>%
  rename(fips = FIPS) %>% 
  select(fips, propunder12) -> fipsCensusByAge

write_csv(fipsCensusByAge, "data/data-sources/fipspop-age.csv")
