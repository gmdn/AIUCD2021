library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(tibble)
library(dplyr)
library(readxl)

dataset <- as_tibble(readRDS("./data/my_df_extended.rds"))

trimed <- read_xlsx("~/Downloads/schede complete.xlsx", sheet = 1)

trimed$lowercase <- tolower(trimed$Term)

english_eu <- dataset %>% 
  filter(language == "EN") %>%
  select(technical, popular, codes)

en_tech <- english_eu %>% 
  inner_join(trimed, by = c("technical" = "lowercase")) %>%
  select(technical, codes) %>% 
  distinct()

en_pop <- english_eu %>% 
  inner_join(trimed, by = c("popular" = "lowercase")) %>%
  select(popular, codes) %>% 
  distinct()

en_tech %>% full_join(en_pop)

#### ITALIAN

trimed <- read_xlsx("~/Downloads/schede complete.xlsx", sheet = 2 )

trimed$lowercase <- tolower(trimed$Term)

italian_eu <- dataset %>% 
  filter(language == "IT") %>%
  select(technical, popular, codes)

it_tech <- italian_eu %>%
  inner_join(trimed, by = c("technical" = "lowercase")) %>%
  select(technical, codes) %>% 
  distinct()

it_pop <- italian_eu %>% 
  inner_join(trimed, by = c("popular" = "lowercase")) %>%
  select(popular, codes) %>% 
  distinct()

it_tech %>% full_join(it_pop)

#### FRENCH

trimed <- read_xlsx("~/Downloads/schede complete.xlsx", sheet = 3)

trimed$lowercase <- tolower(trimed$Term)

french_eu <- dataset %>% 
  filter(language == "FR") %>%
  select(technical, popular, codes)
