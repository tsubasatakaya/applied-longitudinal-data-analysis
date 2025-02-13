library(tidyverse)
library(haven)
library(survival)

data <- read_dta("data/ESS9e03_2.dta") |> 
  as_tibble()


