library(tidyverse)
library("readxl")
library(stringr)

APR <- read_xlsx("All_APR.xlsx")

summary(APR)

APR <- APR %>% rename("Multi-Year APR" = `Multi-Year Rate`)

GSR <- read_xlsx("All_GSR.xlsx")

summary(GSR)

GSR <- GSR %>% mutate("Start Year" = `Cohort Year` + 5,
                      "End Year" = `Cohort Year` + 6)

GSR$`Academic Year` <- str_c(GSR$`Start Year`, "-", GSR$`End Year`)

Academics <- GSR %>% inner_join(APR, by = c("School", "Sport", "State", "Academic Year")) %>%
  select(-`Start Year`, -`End Year`, -`GSR Report`, -`FGR Report`)

unique(Academics$Sport)

unique(Academics$`Academic Year`)

sum(is.na(Academics$`Academic Year`))
