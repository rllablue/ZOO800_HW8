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

package_id <- 'knb-lter-sev.1.18' #found on EDI website

package_entities <- list_data_entities(package_id)
package_entities

data_entity <- read_data_entity(
  packageId = package_id,
  entityId = "65056476604ff22cb44dcc3d3bebfd2a" #2020-2023 weather data
)

sev_2020_2023 <- read_csv(data_entity) #add as dataframe

###################
### OBJECTIVE 2 ###
###################

#Histogram. Large peak at -40 and long left tail. Needs further investigation
ggplot(sev_2020_2023, aes(x = Temp_C)) +
  geom_histogram(fill = 'blue', color = 'black' ) +
  theme_minimal(base_size = 15)

#Check which stations low values come from
sev_neg20 <- sev_2020_2023 %>% 
  filter(Temp_C < -20) %>% 
  group_by(StationID) %>% 
  summarise(
    n=n()
  )
sev_neg20

#Check which stations have -40 values 
sev_neg40 <- sev_2020_2023 %>% 
  filter(Temp_C == -40) %>% 
  group_by(StationID) %>% 
  summarise(
    n=n()
  )
sev_neg40 #Almost all from station 42

#Many low temps from station 44. Check months.
sev_44_low_temps <- sev_2020_2023 %>% 
  filter(Temp_C < -20, StationID == 44) %>% 
  group_by(Month) %>% 
  summarise(
    n=n()
  )
sev_44_low_temps #unusual amount of low temps in May-October

#Check station 45
sev_45_low_temps <- sev_2020_2023 %>% 
  filter(Temp_C < -20, StationID == 45) %>% 
  group_by(Month) %>% 
  summarise(
    n=n()
  )
sev_45_low_temps #All from November, which is plausible 

#Average temp by month
monthly_averages <- sev_2020_2023 %>% 
  group_by(Year, Month, StationID) %>% 
  summarise(
    mean_temp_c = mean(Temp_C, na.rm = TRUE),
    mean_precip = mean(Precipitation, na.rm = TRUE)
  )

#Station 42 and 44 seem to have erroneous data, so we will remove to look at monthly average temp for 2020-2023
monthly_avg_no_42_44 <- monthly_averages %>% 
  filter(StationID != 42, StationID != 44) %>% 
  group_by(Month) %>% 
  summarise(mean_temp_c = mean(mean_temp_c, na.rm = TRUE))

#Plot average monthly temperature, excluding station 42, 44 data
ggplot(monthly_avg_no_42_44, aes(x = Month, y = mean_temp_c)) +
  geom_line() +
  scale_x_continuous(breaks = seq(0, 12, 1)) +
  scale_y_continuous(breaks = seq(0, 30, 3)) +
  theme_minimal(base_size = 15) +
  labs(
    y = 'Average Temperature (C)'
  ) +
  theme(
    axis.line = element_line(linewidth = .5, color = 'black')
  )
