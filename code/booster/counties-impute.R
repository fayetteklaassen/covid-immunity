### Impute county booster data relative to the state data.
## Generates file data/analyzed/booster/cdc-boost-counties-imputed.csv
## Requires the CDC booster data; to be downloaded at:
# https://data.cdc.gov/Vaccinations/COVID-19-Vaccinations-in-the-United-States-County/8xkx-amqh (data/raw/immunity/cdc-boost-counties.csv)
# https://data.cdc.gov/Vaccinations/COVID-19-Vaccinations-in-the-United-States-Jurisdi/unsk-b7fc (data/raw/immunity/cdc-boost-states.csv)

library(tidyverse)
stt_cnt <- read_csv("data/data-sources/fipsstate.csv")

cnt_boost <- read_csv("data/raw/immunity/cdc-boost-counties.csv",
                      col_types = cols(
                        Date = col_date(format = "%m/%d/%Y"),
                        FIPS = col_character(),
                        .default = col_number()
                      )) %>% 
  filter(Date == as.Date("2021-12-16")) %>%
  transmute(fips = FIPS,
         date = Date,
         boost = Booster_Doses_Vax_Pct) 

stt_boost <- read_csv("data/raw/immunity/cdc-boost-states.csv",
                      col_types = cols(
                        Date = col_date(format = "%m/%d/%Y"),
                        Location = col_character(),
                        .default = col_number()
                      )) %>% mutate(
                        state = abbr2state(Location),
                        boost.stt = Additional_Doses_Vax_Pct
                      ) %>%
  select(state, Date, boost.stt) %>%
  filter(Date >= as.Date("2021-10-20") &
           Date <= as.Date("2021-12-16")) %>%
  filter(!is.na(state)) %>%
  rename(date = Date)

allFipsDates <- cnt_boost %>%
  group_by(fips) %>%
  summarize(date = seq.Date(as.Date("2021-10-20"), as.Date("2021-12-16"), 1),
            .groups = 'drop') %>%
  ungroup()


cnt_boost %>% 
  full_join(allFipsDates, by = c("fips", "date")) %>%
  left_join(stt_cnt, by = "fips") %>%
  right_join(stt_boost, by = c("state", "date")) %>%
  group_by(fips) %>%
  mutate(rr = if_else(state == "Hawaii", # Hawaii counties are all missing; impute with state estimates
                      1,
                      if_else(max(boost, na.rm = TRUE) == 0,
                              1,
                              max(boost, na.rm = TRUE) / max(boost.stt)))) %>%
  ungroup() %>%
  mutate(boost.imp = if_else(is.na(boost),
                             boost.stt * rr,
                             if_else(boost == 0,
                                     boost.stt,
                                     boost))) -> cnt_boost_imp

write_csv(cnt_boost_imp, "data/analyzed/booster/cdc-boost-counties-imputed.csv")
