library(tidyverse)
library("readxl")
library(stringr)

APR <- read_xlsx("All_APR.xlsx")

summary(APR)

APR <- APR %>% rename("Multi-Year APR" = `Multi-Year Rate`) %>%
  filter(Sport == "Men's Soccer" | Sport == "Women's Soccer" | Sport == "Baseball" | Sport == "Softball" | Sport == "Men's Basketball" | Sport == "Women's Basketball" | 
           Sport == "Football" | Sport == "Men's Ice Hockey" | Sport == "Women's Ice Hockey")

GSR <- read_xlsx("All_GSR.xlsx")

summary(GSR)

GSR <- GSR %>% mutate("Start Year" = `Cohort Year` + 5,
                      "End Year" = `Cohort Year` + 6)

GSR$`Academic Year` <- str_c(GSR$`Start Year`, "-", GSR$`End Year`)

GSR <- GSR %>%
  select(-`Start Year`, -`End Year`, -`GSR Report`, -`FGR Report`)

Baseball <- read_xlsx("Baseball.xlsx")

Baseball <- Baseball %>% mutate(Sport = "Baseball") %>%
  rename("Pct" = PCT)

Football <- read_xlsx("Football.xlsx")

Football <- Football %>% mutate(Sport = "Football") %>%
  rename("Pct" = Pct)

Men_Basketball <- read_xlsx("Men_Basketball.xlsx")

Men_Basketball <- Men_Basketball %>% mutate(Sport = "Men's Basketball") %>%
  rename("Pct" = Pct)

Men_Soccer <- read_xlsx("Men_Soccer.xlsx")

Men_Soccer<- Men_Soccer %>% mutate(Sport = "Men's Soccer") %>%
  rename("Pct" = Pct.)

Women_Basketball <- read_xlsx("Women_Basketball.xlsx")

Women_Basketball <- Women_Basketball %>% mutate(Sport = "Women's Basketball") %>%
  rename("Pct" = Pct)

Softball <- read_xlsx("Softball.xlsx")

Softball <- Softball %>% mutate(Sport = "Softball") %>%
  rename("Pct" = PCT)

Women_Soccer <- read_xlsx("Women_Soccer.xlsx")

Women_Soccer <- Women_Soccer %>% mutate(Sport = "Women's Soccer") %>%
  rename("Pct" = Pct.)

Men_IceHockey <- read_xlsx("Men_IceHockey.xlsx")

Men_IceHockey <- Men_IceHockey %>% mutate(Sport = "Men's Ice Hockey") %>%
  rename("Pct" = Pct.)

Women_IceHockey <- read_xlsx("Women_IceHockey.xlsx")

Women_IceHockey <- Women_IceHockey %>% mutate(Sport = "Women's Ice Hockey") %>%
  rename("Pct" = Pct.)

