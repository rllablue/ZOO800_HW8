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
library(car)
library(broom)
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

# EDI data for the Sevilleta NWR long-term weather monitoring dataset

package_id <- 'knb-lter-sev.1.18' # found on EDI website

package_entities <- list_data_entities(package_id)
package_entities

data_entity <- read_data_entity(
  packageId = package_id,
  entityId = "65056476604ff22cb44dcc3d3bebfd2a" # 2020-2023 weather data
)

sev_2020_2023 <- read_csv(data_entity) # add as dataframe

sev_2020_2023 <- sev_2020_2023 %>%
  mutate(StationID = as.factor(StationID)) # Make sure this is read as cat., not cont.


###################
### OBJECTIVE 2 ###
###################

### --- INIATIAL PLOTS --- ###
# Exploring temperature variables: Temp_C, Max_Temp_C, and Min_Temp_C

# HISTOGRAM (Temp_C) #
ggplot(sev_2020_2023, aes(x = Temp_C)) +
  geom_histogram(fill = 'blue', color = 'black' ) +
  theme_minimal(base_size = 15)
# Large peak at -40 and long left tail need further investigation


# Q-Q PLOT (Temp_C) #
ggplot(sev_2020_2023, aes(sample = Temp_C)) +
  stat_qq(color = "steelblue") +
  stat_qq_line(color = "red", linetype = "dashed") +
  labs(
    title = "QQ Plot of Temperature (°C)",
    x = "Theoretical Quantiles",
    y = "Sample Quantiles"
  ) +
  theme_minimal(base_size = 15)
# Normality assessment suggests highly non-normal distributrion of values as well
# as many potential outliers (call returns Warning Message indicating 23428 values
# outside the scale range)


### --- STATS TESTS --- ###
# Check whether extreme points are actually influential/outliers

# FIT LM #
monthtemp_mod <- lm(Temp_C ~ Month + StationID, data = sev_2020_2023)


# COOK'S DISTANCE #
# Check for points with potentially high influence on model coefficients
# Note: High influence not necessarily significant influence
cooks_df <- broom::augment(monthtemp_mod) %>%
  mutate(high_influence = .cooksd > (4 / n()))

ggplot(cooks_df, aes(x = seq_along(.cooksd), y = .cooksd)) +
  geom_point(aes(color = high_influence), alpha = 0.7) +
  geom_hline(yintercept = 4 / nrow(cooks_df), color = "red", linetype = "dashed") +
  scale_color_manual(values = c("FALSE" = "steelblue", "TRUE" = "orange")) +
  labs(
    title = "Cook's Distance: Temp_C ~ Month + StationID",
    x = "Observation Index",
    y = "Cook's Distance",
    caption = "Orange points = potential influential observations (D > 4/n). 
    Red dashed line = 4/n cutoff."
  ) +
  theme_minimal(base_size = 15)
# Indicates indeed that lots of Temp_C valeus have power to influence modeling results


# OUTLIER TEST #
# Find points that are potentially outliers based on (studentized) residuals
# ie. poorly fit model
car::outlierTest(monthtemp_mod)
# Indicates only a single observation's residuals are outliers after correction
# for this model, but let's diver deeper into the observations themselves


### --- EXPLORE OBSERVATIONS --- ###

# Check which stations low values come from
sev_neg20 <- sev_2020_2023 %>% 
  filter(Temp_C < -20) %>% 
  group_by(StationID) %>% 
  summarise(
    n=n()
  )
sev_neg20

# Check which stations have -40 values 
sev_neg40 <- sev_2020_2023 %>% 
  filter(Temp_C == -40) %>% 
  group_by(StationID) %>% 
  summarise(
    n=n()
  )
sev_neg40 # Almost all from station 42

# Many low temps from station 44. Check months.
sev_44_low_temps <- sev_2020_2023 %>% 
  filter(Temp_C < -20, StationID == 44) %>% 
  group_by(Month) %>% 
  summarise(
    n=n()
  )
sev_44_low_temps # Unusual amount of low temps in May-October?

#Check station 45
sev_45_low_temps <- sev_2020_2023 %>% 
  filter(Temp_C < -20, StationID == 45) %>% 
  group_by(Month) %>% 
  summarise(
    n=n()
  )
sev_45_low_temps # All from November, which is plausible 

# Average temp by month
monthly_averages <- sev_2020_2023 %>% 
  group_by(Year, Month, StationID) %>% 
  summarise(
    mean_temp_c = mean(Temp_C, na.rm = TRUE),
    mean_precip = mean(Precipitation, na.rm = TRUE)
  )
monthly_averages

# Station 42 and 44 seem to have erroneous data
# We will remove to look at monthly average temp for 2020-2023
monthly_avg_no_42_44 <- monthly_averages %>% 
  filter(StationID != 42, StationID != 44) %>% 
  group_by(Month) %>% 
  summarise(mean_temp_c = mean(mean_temp_c, na.rm = TRUE))
monthly_avg_no_42_44

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

# Reveals systematic bias of certain statiosn toward clusters of extreme values--
# would consider removing these in additional analysis, and at minimum
# investigate possibility of recording unit malfunction.
