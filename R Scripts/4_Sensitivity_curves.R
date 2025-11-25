
set.seed(8) # to have reproducible results
setwd("/home/charlotte/Documents/R/PAM-compatibility-solutions/Correction-factor")
source("fun_for_bats.R")

library(ggplot2)
library(tidygam)
library(mgcv)
library(tidyverse)
library(MASS)
library(DHARMa)
library(glmmTMB)
library(ordbetareg)
library(loo)
library(parameters)
library(legendry)
library(beepr)

mycol <- c("Audiomoth"= "#F8766D","Batcorder"="#7CAE00","Batlogger"="#00BFC4","SM4BAT"="#C77CFF")

Sp_select = "Pippip"
Proba = 0.5

Activity_file <- read_delim("/home/charlotte/Documents/Post-Doc/Stages/Laureen et Nathan/Analyse/Activity_file.csv")
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

# Consider Batlogger A and Batlogger A+ the same machine
Activity_file$Recorder = gsub("^Batlogger.*", "Batlogger", Activity_file$Recorder)

# # Keep only Site CE_0 (for sensititivity curve)
# Activity_file = Activity_file %>% 
#   filter(Site == "CE_0")

# Remove -48dB Trigger for SM4BAT because it cannot fit on the curve
Activity_file = Activity_file %>% 
  filter(!(Recorder == "SM4BAT" & TriggerLevel == -42))

# Adapt Trigger Level so that it goes in the right direction
Activity_file$TriggerLevel_adjusted = Activity_file$TriggerLevel
Activity_file$TriggerLevel_adjusted = ifelse(Activity_file$Recorder!="Audiomoth",-Activity_file$TriggerLevel, Activity_file$TriggerLevel_adjusted)
Activity_file$TriggerLevel_adjusted = ifelse(Activity_file$Recorder=="Audiomoth" & Activity_file$TriggerLevel==0, 100, Activity_file$TriggerLevel_adjusted)
Activity_file$TriggerLevel_adjusted = ifelse(Activity_file$Recorder=="Audiomoth" & Activity_file$TriggerLevel==1, 40, Activity_file$TriggerLevel_adjusted)
Activity_file$TriggerLevel_adjusted = ifelse(Activity_file$Recorder=="Audiomoth" & Activity_file$TriggerLevel==2, 32, Activity_file$TriggerLevel_adjusted)
Activity_file$TriggerLevel_adjusted = ifelse(Activity_file$Recorder=="Audiomoth" & Activity_file$TriggerLevel==3, 30, Activity_file$TriggerLevel_adjusted)
Activity_file$TriggerLevel_adjusted = ifelse(Activity_file$Recorder=="Audiomoth" & Activity_file$TriggerLevel==5, 26, Activity_file$TriggerLevel_adjusted)
Activity_file$TriggerLevel_adjusted = ifelse(Activity_file$Recorder=="Audiomoth" & Activity_file$TriggerLevel==7, 22, Activity_file$TriggerLevel_adjusted)
Activity_file$TriggerLevel_adjusted = ifelse(Activity_file$Recorder=="Audiomoth" & Activity_file$TriggerLevel==8, 22, Activity_file$TriggerLevel_adjusted)
Activity_file$TriggerLevel_adjusted = ifelse(Activity_file$Recorder=="Audiomoth" & Activity_file$TriggerLevel==9, 20, Activity_file$TriggerLevel_adjusted)

# Add ID for Recorder + Sensitivity Level
Activity_file$ID = paste0(Activity_file$Recorder, "_", Activity_file$TriggerLevel)

# Calculate insect and noise and other bat activity
List_bats_0s = c("Barbar", "Eptnil", "Eptser", "Hypsav", "Minsch", "Myoalc", "Myobec", "Myobly",
                 "Myobra", "Myocap", "Myodas", "Myodau", "Myoema", "MyoGT", "Myomyo", "Myomys",
                 "Myonat", "Myopun", "MyospA", "Nyclas", "Nyclei", "Nycnoc", "Pipkuh", "Pipnat",
                 "Pippip", "Pippyg", "Pleaur", "Pleaus", "Plemac", "Rhieur", "Rhifer", "Rhihip",
                 "Tadten", "Vesmur")
List_bats_0s = List_bats_0s[! List_bats_0s %in% Sp_select]
List_bats_0s = paste0(List_bats_0s, "_0s")

Activity_non_target = Non_target_activity_file %>%
  group_by(DateNight, idparticipation) %>%
  summarise(nb_triggered_files_insects = sum(nb_triggered_files[espece == "Insect_0s"]),
            nb_triggered_files_noise = sum(nb_triggered_files[espece == "Noise_0s"]),
            nb_triggered_files_bats = sum(nb_triggered_files[espece %in% List_bats_0s]))

# Prepare data for analysis (factors)
Activity_file$Recorder = as.factor(Activity_file$Recorder)

# Calculate Relative activity to make results comparable
# Keep only selected species for 0.5 confidence
Activity_file_for_model <- Activity_file %>%
  filter(espece == Sp_select, probability_filter == Proba) %>% 
  group_by(DateNight) %>%
  mutate(sm4bat_0_activity = nb_triggered_files[ID == "SM4BAT_0"]) %>% 
  mutate(Relative_activity = nb_triggered_files / sm4bat_0_activity) %>%
  mutate(Relative_activity = ifelse(sm4bat_0_activity==0, NA, Relative_activity)) %>% #if SM4BAT_0 did not pick the species, then the species is considered absent on the site
  ungroup() %>% 
  as.data.frame()

# Confirm this decision with Christian!!!
# Put Relative activity at 1 even if it is above 1
Activity_file_for_model$Relative_activity = ifelse(Activity_file_for_model$Relative_activity>1, 
                                                   1, 
                                                   Activity_file_for_model$Relative_activity)

# Add insect + noise + bat activity
Activity_file_for_model = Activity_file_for_model %>%
  left_join(Activity_non_target) %>%
  group_by(DateNight) %>%
  mutate(sm4bat_0_insect_activity = nb_triggered_files_insects[ID == "SM4BAT_0"],
         sm4bat_0_noise_and_insect_activity = nb_triggered_files_insects[ID == "SM4BAT_0"] + nb_triggered_files_noise[ID == "SM4BAT_0"],
         sm4bat_0_noise_insect_bat_activity = nb_triggered_files_insects[ID == "SM4BAT_0"] + nb_triggered_files_noise[ID == "SM4BAT_0"] + nb_triggered_files_bats[ID == "SM4BAT_0"]) %>%
  ungroup() %>%
  as.data.frame()

#scaling sensitivities to 0;1
Activity_file_for_model <- Activity_file_for_model %>%
  group_by(Recorder) %>%
  mutate(TriggerLevel_scaled=(TriggerLevel_adjusted-min(TriggerLevel_adjusted))/(max(TriggerLevel_adjusted)-min(TriggerLevel_adjusted))) %>%
  ungroup()

# Plot activity look
hist(Activity_file_for_model$Relative_activity)

#### MODEL ####
OBR_model_1 = ordbetareg(bf(Relative_activity ~ TriggerLevel_scaled * Recorder + 
                              I(TriggerLevel_scaled^2) * Recorder +
                              (1 | Site) ), 
                         data = Activity_file_for_model, 
                         chains = 3, iter = 2000)

OBR_model_2 = ordbetareg(bf(Relative_activity ~ TriggerLevel_scaled * Recorder + 
                              I(TriggerLevel_scaled^2) * Recorder +
                              sm4bat_0_noise_insect_bat_activity +
                              (1 | Site) ), 
                         data = Activity_file_for_model, 
                         chains = 3, iter = 2000) # There were 408 transitions after warmup that exceeded the maximum treedepth.

beep(2)

# Diagnostic
OBR_model
#plot(OBR_model, variable = "^b_", regex = TRUE)
pp_check(OBR_model_1)

# Model comparison
loo_comp <- loo_compare(list(Recorder_inter_Trigger = loo(OBR_model_1), 
                             Recorder_inter_Trigger_plus_nontargets = loo(OBR_model_2)
))

model_parameters(loo_comp)

# Predict model 1
newdat <- expand.grid(
  TriggerLevel_scaled = seq(min(Activity_file_for_model$TriggerLevel_scaled),
                            max(Activity_file_for_model$TriggerLevel_scaled),
                            length.out = 100),
  Recorder = unique(Activity_file_for_model$Recorder),
  Site="B1"
)
preds <- fitted(OBR_model_1, newdata = newdat, summary = TRUE)
newdat$Estimate <- preds[,"Estimate"]
newdat$Q2.5     <- preds[,"Q2.5"]
newdat$Q97.5    <- preds[,"Q97.5"]

# Plot 
# Create parallel axes
Breaks_values_SM4BAT = Activity_file_for_model %>%
  filter(Recorder=="SM4BAT") %>%
  mutate(TriggerLevel_scaled=(TriggerLevel_adjusted-min(TriggerLevel_adjusted))/(max(TriggerLevel_adjusted)-min(TriggerLevel_adjusted)))
Breaks_values_SM4BAT = names(table(Breaks_values_SM4BAT$TriggerLevel_scaled))
Breaks_values_Audiomoth = Activity_file_for_model %>%
  filter(Recorder=="Audiomoth") %>%
  mutate(TriggerLevel_scaled=(TriggerLevel_adjusted-min(TriggerLevel_adjusted))/(max(TriggerLevel_adjusted)-min(TriggerLevel_adjusted)))
Breaks_values_Audiomoth = names(table(Breaks_values_Audiomoth$TriggerLevel_scaled))
Breaks_values_Batlogger = Activity_file_for_model %>%
  filter(Recorder=="Batlogger") %>%
  mutate(TriggerLevel_scaled=(TriggerLevel_adjusted-min(TriggerLevel_adjusted))/(max(TriggerLevel_adjusted)-min(TriggerLevel_adjusted)))
Breaks_values_Batlogger = names(table(Breaks_values_Batlogger$TriggerLevel_scaled))
Breaks_values_Batcorder = Activity_file_for_model %>%
  filter(Recorder=="Batcorder") %>%
  mutate(TriggerLevel_scaled=(TriggerLevel_adjusted-min(TriggerLevel_adjusted))/(max(TriggerLevel_adjusted)-min(TriggerLevel_adjusted)))
Breaks_values_Batcorder = names(table(Breaks_values_Batcorder$TriggerLevel_scaled))
key_SM4BAT <- key_auto()(
  scale_x_continuous(
    limits = range(Activity_file_for_model$TriggerLevel_adjusted[Activity_file_for_model$Recorder == "SM4BAT"]),
    breaks = Breaks_values_SM4BAT,
    labels = as.character(-as.numeric(names(table(Activity_file_for_model$TriggerLevel_adjusted[Activity_file_for_model$Recorder == "SM4BAT"]))))
  )
)
key_Audiomoth <- key_auto()(
  scale_x_continuous(
    limits = range(Activity_file_for_model$TriggerLevel_adjusted[Activity_file_for_model$Recorder == "Audiomoth"]),
    breaks = Breaks_values_Audiomoth,
    labels = as.character(-as.numeric(names(table(Activity_file_for_model$TriggerLevel_adjusted[Activity_file_for_model$Recorder == "Audiomoth"]))))
  )
)
key_Batlogger <- key_auto()(
  scale_x_continuous(
    limits = range(Activity_file_for_model$TriggerLevel_adjusted[Activity_file_for_model$Recorder == "Batlogger"]),
    breaks = Breaks_values_Batlogger,
    labels = as.character(-as.numeric(names(table(Activity_file_for_model$TriggerLevel_adjusted[Activity_file_for_model$Recorder == "Batlogger"]))))
  )
)
key_Batcorder <- key_auto()(
  scale_x_continuous(
    limits = range(Activity_file_for_model$TriggerLevel_adjusted[Activity_file_for_model$Recorder == "Batcorder"]),
    breaks = Breaks_values_Batcorder,
    labels = as.character(-as.numeric(names(table(Activity_file_for_model$TriggerLevel_adjusted[Activity_file_for_model$Recorder == "Batcorder"]))))
  )
)

setwd("/home/charlotte/Documents/Post-Doc/Stages/Laureen et Nathan/Analyse/Courbe de sensibilité")
png(filename=paste(Sp_select,"_", "Sensitivity_curve_50.png",sep=""), height=1000, width=1500,res=150)

plot1 = ggplot(newdat, aes(x = TriggerLevel_scaled, y = Estimate, color = Recorder)) +
  geom_line(linewidth = 1) +
  geom_ribbon(aes(ymin = Q2.5, ymax = Q97.5, fill = Recorder), alpha = 0.2, color = NA) +
  labs(y = paste0("Predicted Relative Activity for ", Sp_select)) +
  theme_minimal() +
  theme(legend.position = "right",
    legend.position.inside = c(.025, .975),
    legend.justification.inside = c(0, 1),
    axis.line.y = element_line(),
    axis.title.y.right = element_text(
      vjust = .5, angle = 90)) +
  guides(x = compose_stack(
    compose_stack(
      "axis",
      primitive_title("Scaled Trigger"),
      theme = theme_guide(spacing = unit(0, "pt"))),
    compose_stack(
      guide_axis_base(key = key_Audiomoth),
      primitive_title("Audiomoth Threshold (dB)"),
      theme = theme(axis.title.x = element_text(color=mycol[[1]], size=10, face="bold"))),
    compose_stack(
      guide_axis_base(key = key_Batcorder),
      primitive_title("Batcorder Threshold (dB)"),
      theme = theme(axis.title.x = element_text(color=mycol[[2]], size=10, face="bold"))),
    compose_stack(
      guide_axis_base(key = key_Batlogger),
      primitive_title("Batlogger Min. Crest factor"),
      theme = theme(axis.title.x = element_text(color=mycol[[3]], size=10, face="bold"))),
    compose_stack(
      guide_axis_base(key = key_SM4BAT),
      primitive_title("SM4BAT Relative Trigger Level (dB)"),
      theme = theme(axis.title.x = element_text(color=mycol[[4]], size=10, face="bold"))),
    title = NULL,
    position = "bottom",
    theme = theme_guide(
      spacing = unit(7, "pt"))))

print(plot1)
dev.off()

# Predict model 2
newdat1 <- expand.grid(
  TriggerLevel_scaled = seq(min(Activity_file_for_model$TriggerLevel_scaled),
                            max(Activity_file_for_model$TriggerLevel_scaled),
                            length.out = 100),
  sm4bat_0_noise_insect_bat_activity = min(Activity_file_for_model$sm4bat_0_noise_insect_bat_activity),
  Type = "Low non-target activity",
  Recorder = unique(Activity_file_for_model$Recorder),
  Site="B1"
)
newdat2 <- expand.grid(
  TriggerLevel_scaled = seq(min(Activity_file_for_model$TriggerLevel_scaled),
                            max(Activity_file_for_model$TriggerLevel_scaled),
                            length.out = 100),
  sm4bat_0_noise_insect_bat_activity = mean(Activity_file_for_model$sm4bat_0_noise_insect_bat_activity),
  Type = "Mid non-target activity",
  Recorder = unique(Activity_file_for_model$Recorder),
  Site="B1"
)
newdat3 <- expand.grid(
  TriggerLevel_scaled = seq(min(Activity_file_for_model$TriggerLevel_scaled),
                            max(Activity_file_for_model$TriggerLevel_scaled),
                            length.out = 100),
  sm4bat_0_noise_insect_bat_activity = max(Activity_file_for_model$sm4bat_0_noise_insect_bat_activity),
  Type = "High non-target activity",
  Recorder = unique(Activity_file_for_model$Recorder),
  Site="B1"
)
newdat = rbind(newdat1, newdat2, newdat3)
preds <- fitted(OBR_model_2, newdata = newdat, summary = TRUE)
newdat$Estimate <- preds[,"Estimate"]
newdat$Q2.5     <- preds[,"Q2.5"]
newdat$Q97.5    <- preds[,"Q97.5"]

# Plot 
setwd("/home/charlotte/Documents/Post-Doc/Stages/Laureen et Nathan/Analyse/Courbe de sensibilité")
png(filename=paste(Sp_select,"_", "Sensitivity_curve_50_non-targets.png",sep=""), height=700, width=1500,res=150)

plot2 = ggplot(newdat, aes(x = TriggerLevel_scaled, y = Estimate, color = Recorder)) +
  facet_grid(cols = vars(Type)) +
  geom_line(linewidth = 1) +
  geom_ribbon(aes(ymin = Q2.5, ymax = Q97.5, fill = Recorder), alpha = 0.2, color = NA) +
  labs(y = paste0("Predicted Relative Activity for ", Sp_select)) +
  theme_minimal(base_size = 18)

print(plot2)
dev.off()
