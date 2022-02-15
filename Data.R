library(tidyverse)
library(readxl)
library(stringr)
library(udpipe)
library(rscorecard)

APR <- read_xlsx("All_APR.xlsx")

summary(APR)

APR <- APR %>% rename("Multi-Year APR" = `Multi-Year Rate`) %>%
  filter(Sport == "Men's Soccer" | Sport == "Women's Soccer" | Sport == "Baseball" | Sport == "Softball" | Sport == "Men's Basketball" | Sport == "Women's Basketball" | 
           Sport == "Football" | Sport == "Men's Ice Hockey" | Sport == "Women's Ice Hockey")

unique(APR$School)

GSR <- read_xlsx("All_GSR.xlsx")

summary(GSR)

GSR <- GSR %>% mutate("Start Year" = `Cohort Year` + 5,
                      "End Year" = `Cohort Year` + 6)

GSR$`Academic Year` <- str_c(GSR$`Start Year`, "-", GSR$`End Year`)

GSR <- GSR %>%
  select(-`Start Year`, -`End Year`, -`GSR Report`, -`FGR Report`)

Baseball <- read_xlsx("Baseball.xlsx")

Baseball <- Baseball %>% mutate(Sport = "Baseball", `FBS or FCS` = NA) %>%
  rename("Pct" = PCT)

Football <- read_xlsx("Football.xlsx")

Football <- Football %>% mutate(Sport = "Football") %>%
  rename("Pct" = Pct)

Men_Basketball <- read_xlsx("Men_Basketball.xlsx")

Men_Basketball <- Men_Basketball %>% mutate(Sport = "Men's Basketball", `T` = NA, `FBS or FCS` = NA) %>%
  rename("Pct" = Pct) %>%
  mutate(Pct = Pct/100)

Men_Soccer <- read_xlsx("Men_Soccer.xlsx")

Men_Soccer<- Men_Soccer %>% mutate(Sport = "Men's Soccer", `FBS or FCS` = NA) %>%
  rename("Pct" = Pct., `W` = Won, `L` = Loss, `T` = Tied)

Women_Basketball <- read_xlsx("Women_Basketball.xlsx")

Women_Basketball <- Women_Basketball %>% mutate(Sport = "Women's Basketball", `T` = NA, `FBS or FCS` = NA) %>%
  rename("Pct" = Pct) %>%
  mutate(Pct = Pct/100)

Softball <- read_xlsx("Softball.xlsx")

Softball <- Softball %>% mutate(Sport = "Softball", `FBS or FCS` = NA) %>%
  rename("Pct" = PCT)

Women_Soccer <- read_xlsx("Women_Soccer.xlsx")

Women_Soccer <- Women_Soccer %>% mutate(Sport = "Women's Soccer", `FBS or FCS` = NA) %>%
  rename("Pct" = Pct., `W` = Won, `L` = Lost, `T` = Tied)

Men_IceHockey <- read_xlsx("Men_IceHockey.xlsx")

Men_IceHockey <- Men_IceHockey %>% mutate(Sport = "Men's Ice Hockey", `FBS or FCS` = NA) %>%
  rename("Pct" = Pct., `W` = `Goalie Won`, `L` = `Goalie Loss`, `T` = `Goalie Tied`)

Women_IceHockey <- read_xlsx("Women_IceHockey.xlsx")

Women_IceHockey <- Women_IceHockey %>% mutate(Sport = "Women's Ice Hockey", `FBS or FCS` = NA) %>%
  rename("Pct" = Pct.)

Records <- rbind(Baseball, Football)

Records <- Records %>% rbind(Records, Men_Basketball) %>% 
  rbind(Records, Men_IceHockey) %>% 
  rbind(Records, Men_Soccer) %>% 
  rbind(Records, Softball) %>% 
  rbind(Records, Women_Basketball) %>% 
  rbind(Records, Women_IceHockey) %>% 
  rbind(Records, Women_Soccer)

glimpse(Records)

unique(Records$Team)

Crosswalk <- read_xlsx("Crosswalk.xlsx")

Records <- Records %>% inner_join(Crosswalk, by = "Team")

Full <- inner_join(Records, APR, by = c("School", "Sport", "Academic Year"))

Full <- Full %>% distinct()

summary(Full)

Full <- Full %>% select(-`FBS or FCS`) %>%
  separate(`Academic Year`, c("Start.Year", "End.Year")) %>%
  mutate(Start.Year = as.numeric(Start.Year)) %>%
  mutate(End.Year = as.numeric(End.Year)) %>%
  mutate(Pct = Pct * 100) %>%
  mutate(centeredAPR = `Multi-Year APR` - mean(`Multi-Year APR`, na.rm = TRUE)) %>% 
  mutate(Years10 = Start.Year - 2010) %>%
  mutate(program.id = unique_identifier(Full, fields = c("School", "Sport"))) %>%
  select(-Team, - End.Year) %>%
  select(Rank, School, Sport, program.id, Start.Year, State, everything())


sc_key("1nNjY0a42ZIbyxlydCf2jqiNVqe9Nojz6qcc6KF6")

df10 <- sc_init() %>% 
  sc_select(unitid, instnm, region, pcip09, ugds, ugds_white, ugds_men, costt4_a, age_entry) %>%
  sc_year(2010) %>%
  sc_get()

summary(df10)

df11 <- sc_init() %>% 
  sc_select(unitid, instnm, region, pcip09, ugds, ugds_white, ugds_men, costt4_a, age_entry) %>%
  sc_year(2011) %>%
  sc_get()

df12 <- sc_init() %>% 
  sc_select(unitid, instnm, region, pcip09, ugds, ugds_white, ugds_men, costt4_a, age_entry) %>%
  sc_year(2012) %>%
  sc_get()

df13 <- sc_init() %>% 
  sc_select(unitid, instnm, region, pcip09, ugds, ugds_white, ugds_men, costt4_a, age_entry) %>%
  sc_year(2013) %>%
  sc_get()

df14 <- sc_init() %>% 
  sc_select(unitid, instnm, region, pcip09, ugds, ugds_white, ugds_men, costt4_a, age_entry) %>%
  sc_year(2014) %>%
  sc_get()

df15 <- sc_init() %>% 
  sc_select(unitid, instnm, region, pcip09, ugds, ugds_white, ugds_men, costt4_a, age_entry) %>%
  sc_year(2015) %>%
  sc_get()

df16 <- sc_init() %>% 
  sc_select(unitid, instnm, region, pcip09, ugds, ugds_white, ugds_men, costt4_a, age_entry) %>%
  sc_year(2016) %>%
  sc_get()

df17 <- sc_init() %>% 
  sc_select(unitid, instnm, region, pcip09, ugds, ugds_white, ugds_men, costt4_a, age_entry) %>%
  sc_year(2017) %>%
  sc_get()

df18 <- sc_init() %>% 
  sc_select(unitid, instnm, region, pcip09, ugds, ugds_white, ugds_men, costt4_a, age_entry) %>%
  sc_year(2018) %>%
  sc_get()

Scorecard <- rbind(df10, df11)

Scorecard <- Scorecard %>% rbind(Scorecard, df12) %>% 
  rbind(Scorecard, df13) %>% 
  rbind(Scorecard, df14) %>% 
  rbind(Scorecard, df15) %>% 
  rbind(Scorecard, df16) %>% 
  rbind(Scorecard, df17) %>% 
  rbind(Scorecard, df18)

ScorecardX <- read_xlsx("ScorecardSchools.xlsx")

Full <- Full %>% inner_join(ScorecardX, by = "School")

Scorecard <- Scorecard %>% distinct()

Full <- Full %>% inner_join(Scorecard, by = c("unitid","Start.Year"="year"))

Full <- Full %>% select(-instnm, -Penalties, -Postseason) %>%
  rename("School_ID" = unitid) %>%
  mutate(pcip09 = pcip09 * 100,
         ugds_men = ugds_men * 100,
         ugds_white = ugds_white * 100,
         Region = as.factor(region)) %>%
  select(-region, -W, -L, -T, -Rank)

summary(Full)


write.csv(Full,"C:\\Users\\stick\\Documents\\R\\SeniorProject\\Full_Data.csv", row.names = FALSE)