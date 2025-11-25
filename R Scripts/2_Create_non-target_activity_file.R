
library(tidyverse)

setwd("/home/charlotte/Documents/R/PAM-compatibility-solutions/Correction-factor")
source("fun_for_bats.R")

# Filter probabilities by...
Filter_proba = c(0.5, 0.9)

# Load Observation results
Obs = read_delim("/home/charlotte/Documents/Post-Doc/Stages/Laureen et Nathan/Analyse/Obs_Stage_Laureen_Nathan_2025-10-27.csv")

# Calculate activity of species that were recorded at 0s
Activity_0s = Species_activity_at_0(Obs)

# Group insect activity
List_insects = c("Antsp","Barfis","Confus","Cympud","Decalb","Epheph","Eupsp",
                 "gril","Inssp7","Isopyr","Lamsp.","Leppun","Phanan","Phofem",
                 "Phogri","Plaaff","Plaalb","Plaint","Rhasp","Roeroe","Rusnit",
                 "Sepsep","Testes","Tetarg","Tetvir","Tyllil","Urosp","Yerray"
)

insect_rows = Activity_0s %>% 
  group_by(participation, DateNight, probability_filter) %>% 
  summarise(
    nb_triggered_files = sum(nb_triggered_files[espece %in% paste0(List_insects, "_0s")]),
    .groups = "drop") %>%
  mutate(espece = "Insect_0s")

Activity_0s = bind_rows(Activity_0s, insect_rows) %>%
  arrange(participation, DateNight, probability_filter, espece)

# Load and rbind Metadata files
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

# Add 0 for absent species
Activity_complet <- Activity_0s %>%
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

write_csv(Activity_file, "/home/charlotte/Documents/Post-Doc/Stages/Laureen et Nathan/Analyse/Activity_file_non-target_0s.csv")




