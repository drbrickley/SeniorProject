library(tidyverse)
library("readxl")
library(stringr)

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

write.csv(Full,"C:\\Users\\stick\\Documents\\R\\SeniorProject\\Full_Data.csv", row.names = FALSE)