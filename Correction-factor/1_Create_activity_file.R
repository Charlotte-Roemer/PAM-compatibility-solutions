
library(tidyverse)

setwd("/home/charlotte/Documents/R/PAM-compatibility-solutions/Correction-factor")
source("fun_for_bats.R")

#### !!!!!!!!!! Need to add the 0 for each species !!!!!!!!!!!! #####


# Filter probabilities by...
Filter_proba = c(0.5, 0.9)

# Load Observation results
Obs = read_delim("/home/charlotte/Documents/Post-Doc/Stages/Laureen et Nathan/Analyse/Obs_Stage_Laureen_Nathan_2025-09-29.csv")

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

# Create Activity file
Activity = data.frame()
for(i in 1:length(Filter_proba)){
  Activity_temp = Activity_summary(Obs, Filter_proba[i])
  Activity = rbind(Activity, Activity_temp)
}

Activity_file = Metadata_All %>% 
  select(Site, Recorder, TriggerLevel, idparticipation) %>% 
  right_join(Activity, by = join_by(idparticipation == participation)) %>% 
  mutate(Recorder = Recorder_rename(Recorder)) %>% 
  left_join(Distance_veg)

write_csv(Activity_file, "/home/charlotte/Documents/Post-Doc/Stages/Laureen et Nathan/Analyse/Activity_file.csv")

