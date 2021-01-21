library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(tibble)
library(dplyr)
library(readr)

source("create_tbx.R")

dataset <- as_tibble(readRDS("./data/my_df_extended_extra.rds"))

dataset <- dataset %>%
  distinct(codes, language, technical, popular) %>%
  mutate(search = paste0(technical, " - ", popular, " - ", codes))

languages <- unique(dataset$language)
