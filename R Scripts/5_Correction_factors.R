
set.seed(8) # to have reproducible results
library(tidyverse)
setwd("/home/charlotte/Documents/R/PAM-compatibility-solutions/R Scripts")
source("fun_for_bats.R")

#### OPTION ####

# Define species
Species = "Pippip" # "Pippip", "Plesp", "Pippyg", "Nyclei", "Pipkuh", "Rhifer"

# Define time interval
Time_interval = "day" # "5_seconds", "minute", "5_minutes", "10_minutes", "hour", "day"

# Define recorder
Recorder = "Batlogger" # "SM4BAT", "Audiomoth", "Batcorder", "Batlogger"

# Define trigger
# Audiomoth: in dB (e.g. -40)
# Batcorder: in dB (e.g. -27)
# SM4BAT: in dB and only in positive (e.g. 12)
# Batlogger: in crest factor (e.g. 5)
Trigger = 5

# Define Date
DateNight = as.Date("05/05/2025", format = "%d/%m/%Y")

#### SCALE TRIGGER ####
# Load table
Scaled_triggers = read_delim("/home/charlotte/Documents/Post-Doc/Stages/Laureen et Nathan/Analyse/Courbe de sensibilité/Models/Scaled_triggers.csv")
Scaled_triggers_select = Scaled_triggers %>% 
  select(-TriggerLevel) %>% 
  filter(Recorder == .env$Recorder) %>% 
  mutate(TriggerLevel_adjusted = as.numeric(TriggerLevel_adjusted))

# Inverse trigger value to correspond to the direction of the effect of the sensitivity (higher sensitivity = more triggered recordings)
# Put trigger value in the same scale
TriggerLevel_scaled = scale_trigger(-Trigger, Scaled_triggers_select, Recorder)

#### PREDICT RELATIVE ACTIVITY ####

# Load model
Model_name = paste0(Species, "_", Time_interval, ".rds")
Model = readRDS(paste0("/home/charlotte/Documents/Post-Doc/Stages/Laureen et Nathan/Analyse/Courbe de sensibilité/Models/", Model_name))

# Define variables
Newdata_Batcorder_Low = data.frame(TriggerLevel_scaled = 0,
                            Recorder = "Batcorder",
                            Site = NA,
                            DateNight = DateNight)
Newdata_target = data.frame(TriggerLevel_scaled = TriggerLevel_scaled,
                     Recorder = Recorder,
                     Site = NA,
                     DateNight = DateNight)

# Predict
Predicted_activity_Batcorder_low = predict(Model, Newdata_Batcorder_Low, allow_new_levels = T)
Predicted_activity_Target = predict(Model, Newdata_target, allow_new_levels = T)

# Calculate correction factor
Correction_factor = Predicted_activity_Target / Predicted_activity_Batcorder_low
Correction_factor

