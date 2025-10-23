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
library(viridis)
library(EDIutils)


###################
### OBJECTIVE 1 ###
###################

# Access data remotely

package_id <- 'knb-lter-sev.1.18'

package_entities <- list_data_entities(package_id)
package_entities

data_entity <- read_data_entity(
  packageId = package_id,
  entityId = "65056476604ff22cb44dcc3d3bebfd2a"
)

sev_df <- read_csv(data_entity)
colnames(sev_df)


###################
### OBJECTIVE 2 ###
##################


### --- Histogram --- ###



### --- Box and Whisker Plot --- ###

temps_all <- sev_df %>%
  dplyr::select(StationID, Temp_C, Max_Temp_C, Min_Temp_C) %>%
  mutate(Group = "All Stations")
  
temps_no42 <- sev_df %>%
  filter(StationID != 42) %>%
  dplyr::select(StationID, Temp_C, Max_Temp_C, Min_Temp_C) %>%
  mutate(Group = "Without Station 42")

temps_comp <- bind_rows(temps_all, temps_no42)

temps_comp_long <- temps_comp %>%
  pivot_longer(
    cols = c(Temp_C, Max_Temp_C, Min_Temp_C),
    names_to = "Variable",
    values_to = "Value"
  )

ggplot(temps_comp_long, aes(x = Variable, y = Value, fill = Variable)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.8) +  # hide outlier dots to emphasize whiskers
  facet_wrap(~Group, ncol = 2) +
  scale_fill_viridis_d(option = "magma", end = 0.8) +
  labs(
    x = "",
    y = "Temperature (°C)",
    caption = "Figure 1. Side-by-side boxplots of average, minimum, and maximum temperatures at Sevilleta NWR in New Mexico, USA. \nData collected from Station 42 is considered outlying and potentially erroneous."
  ) +
  theme_bw() +
  theme(
    plot.caption = element_text(size = 9),
    strip.text = element_text(face = "bold"),
    legend.position = "none"
  )













### --- Scatter Plot --- ###




### --- Q-Q Plot --- ###


