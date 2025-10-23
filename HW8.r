###################
### ZOO800: HW8 ###
###################

# Authors: Evan Peepo & Rebekkah LaBlue
# Project: Online data repositories and exploratory data analysis
# Due: October 27, 2025

##############
### SET UP ###
#############

library(tidyr)
library(dplyr)
library(magrittr)
library(readr)
library(stringr)
library(lubridate)
library(ggplot2)
library(EDIutils)


###################
### OBJECTIVE 1 ###
###################

package_id <- 'knb-lter-sev.1.18'

package_entities <- list_data_entities(package_id)
package_entities

data_entity <- read_data_entity(
  packageId = package_id,
  entityId = "65056476604ff22cb44dcc3d3bebfd2a"
)

sev_2020_2023 <- read_csv(data_entity)