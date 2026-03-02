
set.seed(8) # to have reproducible results
setwd("/home/charlotte/Documents/R/PAM-compatibility-solutions/R Scripts")
source("fun_for_bats.R")

library(tidyverse)
library(glmmTMB)
library(ggeffects)
library(ggplot2)
library(viridis)
library(DHARMa)
library(ordbetareg)
library(loo)
library(parameters)
library(beepr)

Sp_select = "Pippyg"
Proba = 0.5
Time_interval = "5_seconds" # "5_seconds", "minute", "5_minutes", "10_minutes", "hour", "day"

Activity_file <- read_delim(paste0("/home/charlotte/Documents/Post-Doc/Stages/Laureen et Nathan/Analyse/Activity_file_", 
                                   str_replace(Time_interval, " ", "_"), ".csv"))
Non_target_activity_file = read_delim("/home/charlotte/Documents/Post-Doc/Stages/Laureen et Nathan/Analyse/Activity_file_non-target_0s.csv")

# Display most common species
Activity_file %>% 
  filter(Recorder == "SM4BAT", probability_filter == Proba) %>% 
  group_by(espece) %>% 
  summarise(n=sum(nb_triggered_files)) %>% 
  as.data.frame() %>% 
  arrange(n)

# Merge all Plecotus rows
Activity_file = Activity_file %>% 
  mutate(espece = ifelse((espece == "Pleaur" | espece== "Pleaus" | espece== "Plemac"), "Plesp", espece)) %>% 
  group_by(across(-nb_triggered_files)) %>% 
  summarise(nb_triggered_files = sum(nb_triggered_files), .groups = "drop")

# # Remove false result
# Activity_file = Activity_file %>%
#   filter(!(Site=="B1" & TriggerLevel == 5))

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

# Calculate insect and noise and other bat activity
List_bats_0s = c("Barbar", "Eptnil", "Eptser", "Hypsav", "Minsch", "Myoalc", "Myobec", "Myobly",
                 "Myobra", "Myocap", "Myodas", "Myodau", "Myoema", "MyoGT", "Myomyo", "Myomys",
                 "Myonat", "Myopun", "MyospA", "Nyclas", "Nyclei", "Nycnoc", "Pipkuh", "Pipnat",
                 "Pippip", "Pippyg", "Pleaur", "Pleaus", "Plemac", "Rhieur", "Rhifer", "Rhihip",
                 "Tadten", "Vesmur")
List_bats_0s = paste0(List_bats_0s, "_0s")

Activity_non_target = Non_target_activity_file %>%
  filter(probability_filter == Proba) %>% 
  group_by(DateNight, idparticipation) %>%
  summarise(nb_triggered_files_insects = sum(nb_triggered_files[espece == "Insect_0s"]),
            nb_triggered_files_noise = sum(nb_triggered_files[espece == "noise_0s"]),
            nb_triggered_files_bats = sum(nb_triggered_files[espece %in% List_bats_0s]))

# Calculate Relative activity to make results comparable
# Select species and keep only 0.5 confidence
Activity_file_for_model <- Activity_file %>%
  filter(espece == Sp_select, probability_filter == Proba) %>% 
  group_by(DateNight) %>%
  mutate(sm4bat_0_activity = nb_triggered_files[ID == "SM4BAT_High"]) %>% 
  mutate(Relative_activity = nb_triggered_files / sm4bat_0_activity) %>%
  mutate(Relative_activity = ifelse(sm4bat_0_activity==0, NA, Relative_activity)) %>% #if SM4BAT_0 did not pick the species, then the species is considered absent on the site
  ungroup() %>% 
  as.data.frame()

# Force relative activity at 1 even if it is a little bit above 1
Activity_file_for_model$Relative_activity = ifelse(Activity_file_for_model$Relative_activity>1, 
                                                   1, 
                                                   Activity_file_for_model$Relative_activity)

# Add insect + noise + bat activity
Activity_file_for_model = Activity_file_for_model %>%
  left_join(Activity_non_target) %>%
  group_by(DateNight) %>%
  mutate(sm4bat_0_insect_activity = nb_triggered_files_insects[ID == "SM4BAT_High"],
         sm4bat_0_noise_and_insect_activity = nb_triggered_files_insects[ID == "SM4BAT_High"] + nb_triggered_files_noise[ID == "SM4BAT_High"],
         sm4bat_0_noise_insect_bat_activity = nb_triggered_files_insects[ID == "SM4BAT_High"] + nb_triggered_files_noise[ID == "SM4BAT_High"] + nb_triggered_files_bats[ID == "SM4BAT_High"]) %>%
  ungroup() %>%
  as.data.frame()

Activity_file_for_model$Recorder = as.factor(Activity_file_for_model$Recorder)
Activity_file_for_model$ID = as.factor(Activity_file_for_model$ID)
Activity_file_for_model$TriggerLevel_adjusted = as.factor(Activity_file_for_model$TriggerLevel_adjusted)

# Show % files triggered by insects or bats or noise
for(i in 1:length(names(table(Activity_file_for_model$ID)))){
  ID_i = names(table(Activity_file_for_model$ID))[i]
  
  setwd("/home/charlotte/Documents/Post-Doc/Stages/Laureen et Nathan/Analyse/Effet environnement")
  png(filename=paste(ID_i,"_", "Triggering.png",sep=""), height=700, width=1000,res=150)
  plot0 = Activity_file_for_model %>% 
    filter(ID == ID_i) %>% 
    mutate(Bats = nb_triggered_files_bats/(nb_triggered_files_bats + nb_triggered_files_insects + nb_triggered_files_noise),
           Noise = nb_triggered_files_noise/(nb_triggered_files_bats + nb_triggered_files_insects + nb_triggered_files_noise),
           Insects = nb_triggered_files_insects/(nb_triggered_files_bats + nb_triggered_files_insects + nb_triggered_files_noise),
           month = month(DateNight)) %>% 
    select(Bats, Noise, Insects, DateNight, month) %>% 
    pivot_longer(cols = c(Bats, Noise, Insects),
                 names_to = "Triggered_by",
                 values_to = "n") %>% 
    ggplot(aes(y = n, x = Triggered_by, colour = Triggered_by)) +
    geom_boxplot() +
    facet_grid(vars(month)) +
    guides(colour = "none")+
    theme_bw()
  print(plot0)
  dev.off()
}
