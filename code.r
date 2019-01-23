# Cyberball Physiological Analysis Code

# This code was created by Sam Ashcroft to import, clean, manipulate and summarise over 80 million rows of physiological data
# The 5.5 million rows of data are summarised in a 60 x 26 cell Excel sheet as output
# The output is formatted so that it is ready to be imported into SPSS or used with R or Python easily as needed

# install packages I usually use
library(dplyr)
library(beepr)
library(tidyr)
library(ez)
library(ggplot2)
library(car)
library(psych)
library(Publish)

# setwd to your raw data folder ready for importing
setwd("~/Documents/PhD Resources/R/Cyberball/Cyberball Analysis/Raw")

# list all files ending in .txt
df_name <- list.files(pattern = "\\.txt$")
# check the names of all files to see whether something has gone wrong
df_name

## make a list of all the .txt files in order
df_list <- lapply(df_name,
                  read.csv, sep = "\t", header = TRUE, skip = 9
)

## bind all the csvs to make a big csv with all participant data
df <- data.table::rbindlist(df_list,
                            use.names = TRUE,
                            idcol = TRUE,
                            fill = TRUE)

# look at all variables
names(df)

# remove weird "X" column
df <- df[,1:5] 

# look at all variables to check deletion of "X" variable
names(df)

# count how many rows there are
str(df)

# look at max values to see obvious outlier
summary(df)

# delete all rows with NA
df <- na.omit(df)

# count how many rows there are again
str(df)

# look at max values to see the removal of obvious outlier
summary(df)

# set wd to folder containing all the pings that mark each important event during physiological recording
setwd("~/Documents/PhD Resources/R/Cyberball/Cyberball Analysis/Pings")

# find pings file
pings_file <- list.files(pattern = "\\.csv$")

# import pings
pings <- read.csv(pings_file)

# join pings and raw dataframes
pings_and_phys <- full_join(pings, df, by = c("SS" = ".id"))

# check structure and summary to see obvious outliers
str(pings_and_phys)
summary(pings_and_phys)

# remove NA rows which also obviously contain outliers
pings_and_phys <- na.omit(pings_and_phys)

# check structure and summary to see removal obvious outliers
str(pings_and_phys)
summary(pings_and_phys)

# set dir to output for writing a csv later
setwd("~/Documents/PhD Resources/R/Cyberball/Cyberball Analysis/Output")

# begin extracting mins, maxs, means, sds

# Baseline stage

# Channel One physiological calculations

baseline_CH1_min <- pings_and_phys %>% group_by(SS) %>%
  summarise(baseline_min_CH1 = min(CH1[min > BaselineStart & min < BaselineEnd]))

baseline_CH1_max <- pings_and_phys %>% group_by(SS) %>%
  summarise(baseline_max_CH1 = max(CH1[min > BaselineStart & min < BaselineEnd]))

baseline_CH1_mean <- pings_and_phys %>% group_by(SS) %>%
  summarise(baseline_mean_CH1 = mean(CH1[min > BaselineStart & min < BaselineEnd]))

baseline_CH1_sd <- pings_and_phys %>% group_by(SS) %>%
  summarise(baseline_sd_CH1 = sd(CH1[min > BaselineStart & min < BaselineEnd]))

# Channel Two physiological calculations

baseline_CH2_min <- pings_and_phys %>% group_by(SS) %>%
  summarise(baseline_min_CH2 = min(CH2[min > BaselineStart & min < BaselineEnd]))

baseline_CH2_max <- pings_and_phys %>% group_by(SS) %>%
  summarise(baseline_max_CH2 = max(CH2[min > BaselineStart & min < BaselineEnd]))

baseline_CH2_mean <- pings_and_phys %>% group_by(SS) %>%
  summarise(baseline_mean_CH2 = mean(CH2[min > BaselineStart & min < BaselineEnd]))

baseline_CH2_sd <- pings_and_phys %>% group_by(SS) %>%
  summarise(baseline_sd_CH2 = sd(CH2[min > BaselineStart & min < BaselineEnd]))

# Channel Forty-Three physiological calculations

baseline_CH43_min <- pings_and_phys %>% group_by(SS) %>%
  summarise(baseline_min_CH43 = min(CH43[min > BaselineStart & min < BaselineEnd]))

baseline_CH43_max <- pings_and_phys %>% group_by(SS) %>%
  summarise(baseline_max_CH43 = max(CH43[min > BaselineStart & min < BaselineEnd]))

baseline_CH43_mean <- pings_and_phys %>% group_by(SS) %>%
  summarise(baseline_mean_CH43 = mean(CH43[min > BaselineStart & min < BaselineEnd]))

baseline_CH43_sd <- pings_and_phys %>% group_by(SS) %>%
  summarise(baseline_sd_CH43 = sd(CH43[min > BaselineStart & min < BaselineEnd]))


# Test

# Channel One physiological calculations

test_CH1_min <- pings_and_phys %>% group_by(SS) %>%
  summarise(test_min_CH1 = min(CH1[min > TestStart & min < TestEnd]))

test_CH1_max <- pings_and_phys %>% group_by(SS) %>%
  summarise(test_max_CH1 = max(CH1[min > TestStart & min < TestEnd]))

test_CH1_mean <- pings_and_phys %>% group_by(SS) %>%
  summarise(test_mean_CH1 = mean(CH1[min > TestStart & min < TestEnd]))

test_CH1_sd <- pings_and_phys %>% group_by(SS) %>%
  summarise(test_sd_CH1 = sd(CH1[min > TestStart & min < TestEnd]))

# Channel Two physiological calculations

test_CH2_min <- pings_and_phys %>% group_by(SS) %>%
  summarise(test_min_CH2 = min(CH2[min > TestStart & min < TestEnd]))

test_CH2_max <- pings_and_phys %>% group_by(SS) %>%
  summarise(test_max_CH2 = max(CH2[min > TestStart & min < TestEnd]))

test_CH2_mean <- pings_and_phys %>% group_by(SS) %>%
  summarise(test_mean_CH2 = mean(CH2[min > TestStart & min < TestEnd]))

test_CH2_sd <- pings_and_phys %>% group_by(SS) %>%
  summarise(test_sd_CH2 = sd(CH2[min > TestStart & min < TestEnd]))

# Channel Forty-Three physiological calculations

test_CH43_min <- pings_and_phys %>% group_by(SS) %>%
  summarise(test_min_CH43 = min(CH43[min > TestStart & min < TestEnd]))

test_CH43_max <- pings_and_phys %>% group_by(SS) %>%
  summarise(test_max_CH43 = max(CH43[min > TestStart & min < TestEnd]))

test_CH43_mean <- pings_and_phys %>% group_by(SS) %>%
  summarise(test_mean_CH43 = mean(CH43[min > TestStart & min < TestEnd]))

test_CH43_sd <- pings_and_phys %>% group_by(SS) %>%
  summarise(test_sd_CH43 = sd(CH43[min > TestStart & min < TestEnd]))


# Join all dataframes I just created

full_df <- baseline_CH1_min %>%
  full_join(baseline_CH1_max, by = "SS") %>%
  full_join(baseline_CH1_mean, by = "SS") %>%
  full_join(baseline_CH1_sd, by = "SS") %>%
  full_join(baseline_CH2_min, by = "SS") %>%
  full_join(baseline_CH2_max, by = "SS") %>%
  full_join(baseline_CH2_mean, by = "SS") %>%
  full_join(baseline_CH2_sd, by = "SS") %>%
  full_join(baseline_CH43_min, by = "SS") %>%
  full_join(baseline_CH43_max, by = "SS") %>%
  full_join(baseline_CH43_mean, by = "SS") %>%
  full_join(baseline_CH43_sd, by = "SS") %>%
  full_join(test_CH1_min, by = "SS") %>%
  full_join(test_CH1_max, by = "SS") %>%
  full_join(test_CH1_mean, by = "SS") %>%
  full_join(test_CH1_sd, by = "SS") %>%
  full_join(test_CH2_min, by = "SS") %>%
  full_join(test_CH2_max, by = "SS") %>%
  full_join(test_CH2_mean, by = "SS") %>%
  full_join(test_CH2_sd, by = "SS") %>%
  full_join(test_CH43_min, by = "SS") %>%
  full_join(test_CH43_max, by = "SS") %>%
  full_join(test_CH43_mean, by = "SS") %>%
  full_join(test_CH43_sd, by = "SS")


# write a csv of the full dataframe
write.csv(full_df, "Cyberball Phys Data Summarised.csv")

# play a sound to let me know it's done (takes a few minutes given how much data there is)
beep(3)
