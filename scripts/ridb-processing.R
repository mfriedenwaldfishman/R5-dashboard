# setup ----
library(vroom)
library(here)
library(tidyverse)

# read in data ----
# 2018 ridb data
fp_ridb2018 <- here("data/ridb/raw/reservations2018.csv")
raw_ridb2018 <- vroom(fp_ridb2018)

# 2019 ridb data
fp_ridb2019 <- here("data/ridb/raw/reservations2019.csv")
raw_ridb2019 <- vroom(fp_ridb2019)