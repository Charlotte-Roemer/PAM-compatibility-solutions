
# Plot confidence index

library(tidyverse)
library(viridis)

Species_list = c("Pippip", "Plesp", "Pippyg", "Nyclei", "Pipkuh", "Rhifer")

setwd("/home/charlotte/Documents/R/PAM-compatibility-solutions/R Scripts")
source("fun_for_bats.R")

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

# Add date of night
Obs = Obs %>% 
  mutate(DateNight = DateNightFun(donnee))

Activity_file = Metadata_All %>% 
  select(Site, Recorder, TriggerLevel, idparticipation) %>% 
  right_join(Obs, by = join_by(idparticipation == participation)) %>% 
  mutate(Recorder = Recorder_rename(Recorder))

# Check that there is only one night per participation
Activity_file %>% 
  group_by(Site, DateNight) %>% 
  distinct(Site, DateNight) %>% 
  as.data.frame()

# Filter nights that should not be here
Activity_file = Activity_file %>% 
  filter(!(Site == "L1" & DateNight == "2025-05-24")) %>% 
  filter(!(Site == "L1" & DateNight == "2025-05-25")) 

# Consider Batlogger A and Batlogger A+ the same machine
Activity_file$Recorder = gsub("^Batlogger.*", "Batlogger", Activity_file$Recorder)

# Add ID for Recorder + Sensitivity Level
Activity_file$ID = paste0(Activity_file$Recorder, "_", Activity_file$TriggerLevel)

# Remove Site CE_0 because it is another design
Activity_file = Activity_file %>% 
  filter(Site != "CE_0")

# Adapt Trigger Level so that it goes in the right direction
Activity_file$TriggerLevel_adjusted = Activity_file$TriggerLevel
Activity_file$TriggerLevel_adjusted = ifelse(Activity_file$Recorder!="Audiomoth",-Activity_file$TriggerLevel, Activity_file$TriggerLevel_adjusted)
Activity_file$TriggerLevel_adjusted = ifelse(Activity_file$Recorder=="Audiomoth" & Activity_file$TriggerLevel==0, 100, Activity_file$TriggerLevel_adjusted)
Activity_file$TriggerLevel_adjusted = ifelse(Activity_file$Recorder=="Audiomoth" & Activity_file$TriggerLevel==5, 26, Activity_file$TriggerLevel_adjusted)

# Create short names
Activity_file$TriggerLevel_Short = Activity_file$TriggerLevel_adjusted
Activity_file$TriggerLevel_Short = ifelse(Activity_file$TriggerLevel_adjusted==0, "High", Activity_file$TriggerLevel_Short)
Activity_file$TriggerLevel_Short = ifelse(Activity_file$TriggerLevel_adjusted==-24, "Low", Activity_file$TriggerLevel_Short)
Activity_file$TriggerLevel_Short = ifelse(Activity_file$TriggerLevel_adjusted==-10, "Low", Activity_file$TriggerLevel_Short)
Activity_file$TriggerLevel_Short = ifelse(Activity_file$TriggerLevel_adjusted==26, "Low", Activity_file$TriggerLevel_Short)
Activity_file$TriggerLevel_Short = ifelse(Activity_file$TriggerLevel_adjusted==27, "Medium", Activity_file$TriggerLevel_Short)
Activity_file$TriggerLevel_Short = ifelse(Activity_file$TriggerLevel_adjusted==100, "High", Activity_file$TriggerLevel_Short)

# Add ID for Recorder + Sensitivity Level
Activity_file$ID = paste0(Activity_file$Recorder, "_", Activity_file$TriggerLevel_Short)

Activity_file_for_plot <- Activity_file %>%
  mutate(espece = ifelse((espece == "Pleaur" | espece== "Pleaus" | espece== "Plemac"), "Plesp", espece)) %>% 
  filter(espece %in% Species_list)

# Plot probability
setwd("/home/charlotte/Documents/Post-Doc/Stages/Laureen et Nathan/Analyse/Effet environnement")
png(filename="Tadarida.png", height=1500, width=1800,res=150)
plot1 = Activity_file_for_plot %>% 
  ggplot(aes(y=tadarida_probabilite, x=ID, colour = espece)) +
  geom_boxplot() +
  facet_grid(vars(espece)) +
  labs(y = "Probability of identification",
       x = "") +
  guides(colour="none") +
  theme_minimal(base_size = 15)
print(plot1)
dev.off()



# Plot number of triggered recordings by Recorder/trigger
colour_manual <- c("dodgerblue2", "#E31A1C", "green4", "#6A3D9A", "#FF7F00", "black", "gold1")
Activity_file_for_plot2 = Activity_file_for_plot %>% 
  filter(!(Site %in% c("B1", "L3", "M3", "M4", "Z1"))) %>% 
  group_by(Site, ID, espece) %>%
  summarise(n = n())
Activity_file_for_plot2$ID = factor(Activity_file_for_plot2$ID, levels = c("SM4BAT_High", "Audiomoth_High", "Batlogger_High", 
                                                                           "Audiomoth_Low", "SM4BAT_Low", "Batlogger_Low", 
                                                                           "Batcorder_Medium"))

setwd("/home/charlotte/Documents/Post-Doc/Stages/Laureen et Nathan/Analyse/Effet environnement")
png(filename="Nb_triggered_recordings_filter0.png", height=1000, width=1200,res=150)
plot2 = Activity_file_for_plot2 %>% 
  ggplot(aes(y = n, x = espece, fill=ID)) +
  geom_boxplot() +
  scale_y_log10()+
  #ylim(0, 1000) +
  scale_fill_manual(values=colour_manual) +
  labs(y = "Number of triggered recordings per night",
       x = "") +
  theme_minimal(base_size = 15)
print(plot2)
dev.off()

Activity_file_for_plot3 = Activity_file_for_plot %>% 
  filter(!(Site %in% c("B1", "L3", "M3", "M4", "Z1")),
         tadarida_probabilite > 0.5) %>% 
  group_by(Site, ID, espece) %>%
  summarise(n = n())
Activity_file_for_plot3$ID = factor(Activity_file_for_plot3$ID, levels = c("SM4BAT_High", "Audiomoth_High", "Batlogger_High", 
                                                                           "Audiomoth_Low", "SM4BAT_Low", "Batlogger_Low", 
                                                                           "Batcorder_Medium"))
setwd("/home/charlotte/Documents/Post-Doc/Stages/Laureen et Nathan/Analyse/Effet environnement")
png(filename="Nb_triggered_recordings_filter50.png", height=1000, width=1200,res=150)
plot2 = Activity_file_for_plot3 %>% 
  ggplot(aes(y = n, x = espece, fill=ID)) +
  geom_boxplot() +
  scale_y_log10()+
  #ylim(0, 1000) +
  scale_fill_manual(values=colour_manual) +
  labs(y = "Number of triggered recordings per night",
       x = "") +
  theme_minimal(base_size = 15)
print(plot2)
dev.off()





