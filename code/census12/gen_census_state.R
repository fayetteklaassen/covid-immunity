##### STATE ####

## Download state census by single year of age
tmp = tempfile(fileext = ".xlsx")
download.file(url = "https://www2.census.gov/programs-surveys/popest/tables/2010-2019/state/asrh/sc-est2019-syasex-01.xlsx", 
              destfile = tmp, 
              mode = "wb")

stateCensus  <- readxl::read_excel(tmp, 
                                   range = "AI7:AI92", # confirm columns
                                   col_names = FALSE)
stateCensus  <- data.frame(t(stateCensus), "StateName" = state.name[1])

stateName_DC <- c(state.name[1:8], 
                  "District of Columbia", 
                  state.name[9:50]) # add DC to R state.name object

validN <- c(2,4:6,8:13,15:42,44:51,53:56) # these are the state fips IDs

for(i in 1:length(validN)){
  if(validN[i] < 10){
    download.file(url = paste0("https://www2.census.gov/programs-surveys/popest/tables/2010-2019/state/asrh/sc-est2019-syasex-0",
                               validN[i],
                               ".xlsx"), 
                  destfile = tmp,
                  mode = "wb")
  } else {
    download.file(url = paste0("https://www2.census.gov/programs-surveys/popest/tables/2010-2019/state/asrh/sc-est2019-syasex-",
                               validN[i],
                               ".xlsx"), 
                  destfile = tmp,
                  mode = "wb")
  }
  
  tempState  <- readxl::read_excel(tmp, 
                                   range = "AI7:AI92", # confirm
                                   col_names = FALSE)
  tempState  <- data.frame(t(tempState), "StateName" = stateName_DC[i+1])
  stateCensus  <- rbind(stateCensus, tempState)
}

stateCensus$under12 <- rowSums(stateCensus[,1:12])
stateCensus$total <- rowSums(stateCensus[,1:86])
stateCensus$propunder12 <- stateCensus$under12/stateCensus$total
row.names(stateCensus) <- NULL

stateCensusByAge <- stateCensus %>% 
  rename(state = StateName) %>% 
  select(state, propunder12)

write_csv(stateCensusByAge, "data/data-sources/statepop-age.csv")



