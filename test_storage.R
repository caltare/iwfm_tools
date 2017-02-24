library(dssrip)
library(tidyverse)
library(broom)
library(scales)
library(forcats)

minYear <- 1989
maxYear <- 2009
zone <- 33 

SRI <- read_csv("U:\\River.Indices.csv")
SRI$index.classification <- as.factor(SRI$index.classification) %>%
  fct_relevel("wet", "above.normal", "below.normal", "dry", "critical")


zbDSS <- opendss("U:\\C2VSim_CG_1972IC_R374_Model\\C2VSim_CG_1972IC_R374\\Results\\zbudget_GWBasins_All.dss")

getDSS <- function(dssFile, zone, component){
  pth <- paste0("/*/ZONE:",zone,"/*/*/*/",component,"/")
  pths <- getPaths(dssFile, pth)
  x <- getFullTSC(dssFile, pths) %>%
    tidy(x) %>%
    mutate(index=index-1)
}

wtr_yr <- function(dates, start_month=10) {
  # Convert dates into POSIXlt
  dates.posix = as.POSIXlt(dates)
  # Year offset
  offset = ifelse(dates.posix$mon >= start_month - 1, 1, 0)
  # Water year
  adj.year = dates.posix$year + 1900 + offset
  # Return the water year
  as.integer(adj.year)
}


storage.in <- getDSS(zbDSS ,zone, "GW STORAGE_IN")
storage.out <- getDSS(zbDSS, zone, "GW STORAGE_OUT")
storage.net <- data.frame(index=storage.in$index,
                          component="STORAGE_NET",
                          value=storage.out$value - storage.in$value)
storage.net.WY <- storage.net %>%
  mutate(water.year=wtr_yr(index)) %>%
  group_by(component,water.year) %>%
  summarise(value = sum(value)) %>%
  filter(water.year >= minYear, water.year <= maxYear)

cumul.storage.net.WY <- tibble(component = factor('CUMULATIVE_CHANGE_STORAGE'),
                               water.year = storage.net.WY$water.year,
                               value = cumsum(storage.net.WY$value))