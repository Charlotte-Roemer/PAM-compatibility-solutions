
library(tidyverse)

#### Load files and define options ####

setwd("/home/charlotte/Documents/R/PAM-compatibility-solutions/R Scripts")
source("fun_for_bats.R")

# Filter probabilities by...
Filter_proba = c(0.5, 0.9)

# Time interval: "5 seconds", "minute", "5 minutes", "10 minutes", "hour", "day"
Time_interval = "1 day"

# Load Observation results
Obs = read_delim("/home/charlotte/Documents/Post-Doc/Stages/Laureen et Nathan/Analyse/Obs_Stage_Laureen_Nathan_2025-10-27.csv")

# Load and bind Metadata files
Chemin_Metadata = "/home/charlotte/Documents/Post-Doc/MIGRATION/IN2P3/Scripts_IRODS/Dossiers traités"
TagMetadata = "Stage_Laureen_Nathan"
ListFilesMeta = list.files(paste0(Chemin_Metadata), full.names = T, recursive = TRUE, pattern = TagMetadata)
Metadata_All = data.frame()
for(i in 1:length(ListFilesMeta)){
  Metadata_Temp = read_delim(ListFilesMeta[i], show_col_types=F)
  Metadata_All = rbind(Metadata_All, Metadata_Temp)
}

# Load Distance information
Distance_veg = read_delim("/home/charlotte/Documents/Post-Doc/Stages/Laureen et Nathan/Analyse/Distance.csv")

#### Action ####

# Add date of night
Obs = Obs %>% 
  mutate(DateNight = DateNightFun(donnee))

# Aggregate data if time interval is higher than 5 seconds
if(Time_interval != "5 seconds") {
  Obs = aggregate_detections (Obs, Time_interval)
}

# Create Activity file
Activity = data.frame()
for(i in 1:length(Filter_proba)){
  Activity_temp = Activity_summary(Obs, Filter_proba[i])
  Activity = rbind(Activity, Activity_temp)
}

# Add 0 for absent species
Activity_complet <- Activity %>%
  unite(temp_key, DateNight, participation, probability_filter, sep = "_") %>%
  complete(temp_key, espece, fill = list(nb_triggered_files = 0)) %>%
  separate(temp_key, into = c("DateNight", "participation", "probability_filter"), sep = "_") %>%
  select(DateNight, participation, espece, probability_filter, nb_triggered_files)

Activity_file = Metadata_All %>% 
  select(Site, Recorder, TriggerLevel, idparticipation) %>% 
  right_join(Activity_complet, by = join_by(idparticipation == participation)) %>% 
  mutate(Recorder = Recorder_rename(Recorder)) %>% 
  left_join(Distance_veg)

# Check that there is only one night per participation
Activity_file %>% 
  group_by(Site, DateNight) %>% 
  distinct(Site, DateNight) %>% 
  as.data.frame()

# Filter nights that should not be here
Activity_file = Activity_file %>% 
  filter(!(Site == "L1" & DateNight == "2025-05-24")) %>% 
  filter(!(Site == "L1" & DateNight == "2025-05-25")) 

write_csv(Activity_file, 
          paste0("/home/charlotte/Documents/Post-Doc/Stages/Laureen et Nathan/Analyse/Activity_file_", 
                 str_replace(Time_interval, " ", "_"), ".csv"))



