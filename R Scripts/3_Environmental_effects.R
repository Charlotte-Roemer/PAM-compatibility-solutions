
library(tidyverse)
library(glmmTMB)
library(ggeffects)
# library(emmeans)
library(ggplot2)
library(viridis)
library(DHARMa)
library(ordbetareg)
# library(multcompView)
# library(multcomp)
library(loo)
library(parameters)
library(beepr)

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
List_bats_0s = List_bats_0s[! List_bats_0s %in% Sp_select] # Removes focus species
List_bats_0s = paste0(List_bats_0s, "_0s")

Activity_non_target = Non_target_activity_file %>%
  group_by(DateNight, idparticipation) %>%
  summarise(nb_triggered_files_insects = sum(nb_triggered_files[espece == "Insect_0s"]),
            nb_triggered_files_noise = sum(nb_triggered_files[espece == "Noise_0s"]),
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

# Confirm this decision with Christian!!!
# Put Relative activity at 1 even if it is above 1
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
#mycol <- c("Audiomoth"= "#E69F00","Batcorder"="#56B4E9","Batlogger"="#009E73","SM4BAT"="#D55E00")

setwd("/home/charlotte/Documents/Post-Doc/Stages/Laureen et Nathan/Analyse/Effet environnement")
png(filename=paste(Sp_select,"_", "Material_Effect_test_Vegetation.png",sep=""), height=700, width=1000,res=150)

plot1 = ggplot(Activity_file_for_model, aes(x = Recorder, y = Relative_activity)) +
  geom_boxplot(aes(fill = TriggerLevel_Short), color = "grey25", linewidth = 0.5, coef = 1.5) +
  #scale_fill_manual(values = mycol) +
  theme_minimal()

print(plot1)
dev.off()

#### MODEL ENVIRONMENT EFFECT ####
# Question: is it necessary to account for the distance to vegetation (DV), and for the insect/noise activity (IA)?
# i.e. is there an interaction between recorder sensitivity and DV or IA?

# # Comment to keep the Audiomoths
# Activity_file_for_model = Activity_file_for_model %>% 
#   filter(Recorder != "Audiomoth") %>% 
#   droplevels()

# ID alone
OBR_model_1 = ordbetareg(bf(Relative_activity ~ ID +
                              (1 | Site) ), 
                         data=Activity_file_for_model, 
                         chains=3,iter=2000)

# With distance to vegetation but no interaction
OBR_model_2 = ordbetareg(bf(Relative_activity ~ Distance_vegetation + ID +
                              (1 | Site) ),
                         data=Activity_file_for_model,
                         chains=3,iter=2000)
# 
# # With distance to vegetation as quadratic effect but no interaction
# OBR_model_3 = ordbetareg(bf(Relative_activity ~ Distance_vegetation + I(Distance_vegetation^2) + ID +
#                               (1 | Site) ), 
#                          data=Activity_file_for_model, 
#                          chains=3,iter=2000)
# 
# Interaction with distance to vegetation
OBR_model_4 = ordbetareg(bf(Relative_activity ~ Distance_vegetation * ID +
                              (1 | Site) ),
                         data=Activity_file_for_model,
                         chains=3,iter=2000)
# 
# # Interaction with distance to vegetation and quadratic effect
# OBR_model_5 = ordbetareg(bf(Relative_activity ~ Distance_vegetation * ID + 
#                             I(Distance_vegetation^2) * ID +
#                             (1 | Site) ), 
#                        data=Activity_file_for_model, 
#                        chains=3,iter=2000)

# # With insect activity without interaction
# OBR_model_6 = ordbetareg(bf(Relative_activity ~ sm4bat_0_insect_activity + ID +
#                               (1 | Site) ), 
#                          data=Activity_file_for_model, 
#                          chains=3,iter=2000)

# # With insect activity as quadratic effect but no interaction
# OBR_model_7 = ordbetareg(bf(Relative_activity ~ sm4bat_0_insect_activity + I(sm4bat_0_insect_activity^2) + ID +
#                               (1 | Site) ), 
#                          data=Activity_file_for_model, 
#                          chains=3,iter=2000)

# # Interaction with insect activity
# OBR_model_8 = ordbetareg(bf(Relative_activity ~ sm4bat_0_insect_activity * ID +
#                               (1 | Site) ), 
#                          data=Activity_file_for_model, 
#                          chains=3,iter=2000)

# # Interaction with insect activity as quadratic effect
# OBR_model_9 = ordbetareg(bf(Relative_activity ~ sm4bat_0_insect_activity * ID +
#                               I(sm4bat_0_insect_activity^2) * ID +
#                               (1 | Site) ),
#                          data=Activity_file_for_model,
#                          chains=3,iter=2000)

# # With insect+noise activity without interaction
# OBR_model_10 = ordbetareg(bf(Relative_activity ~ sm4bat_0_noise_and_insect_activity + ID +
#                               (1 | Site) ), 
#                          data=Activity_file_for_model, 
#                          chains=3,iter=2000)

# # With insect+noise activity as quadratic effect but no interaction
# OBR_model_11 = ordbetareg(bf(Relative_activity ~ sm4bat_0_noise_and_insect_activity + I(sm4bat_0_noise_and_insect_activity^2) + ID +
#                               (1 | Site) ), 
#                          data=Activity_file_for_model, 
#                          chains=3,iter=2000)

# # Interaction with insect+noise activity
# OBR_model_12 = ordbetareg(bf(Relative_activity ~ sm4bat_0_noise_and_insect_activity * ID +
#                               (1 | Site) ), 
#                          data=Activity_file_for_model, 
#                          chains=3,iter=2000)

# # Interaction with insect+noise activity as quadratic effect
# OBR_model_13 = ordbetareg(bf(Relative_activity ~ sm4bat_0_noise_and_insect_activity * ID +
#                               I(sm4bat_0_noise_and_insect_activity^2) * ID +
#                               (1 | Site) ),
#                          data=Activity_file_for_model,
#                          chains=3,iter=2000)

# With insect+noise+bat activity without interaction
OBR_model_14 = ordbetareg(bf(Relative_activity ~ sm4bat_0_noise_insect_bat_activity + ID +
                               (1 | Site) ), 
                          data=Activity_file_for_model, 
                          chains=3,iter=2000)

# Interaction with insect+noise+bat activity
OBR_model_15 = ordbetareg(bf(Relative_activity ~ sm4bat_0_noise_insect_bat_activity * ID +
                               (1 | Site) ), 
                          data=Activity_file_for_model, 
                          chains=3,iter=2000)

beep(2)

# # Diagnostic
# OBR_model_1
# OBR_model_2
# OBR_model_3
# OBR_model_4
# plot(OBR_model_1, variable = "^b_", regex = TRUE)
# plot(OBR_model_2, variable = "^b_", regex = TRUE)
# plot(OBR_model_3, variable = "^b_", regex = TRUE)

# Model comparison
loo_comp <- loo_compare(list(ID = loo(OBR_model_1), 
                             ID_veg = loo(OBR_model_2),
                             # ID_veg2 = loo(OBR_model_3), 
                             ID_veg_int = loo(OBR_model_4), 
                             # ID_veg2_int = loo(OBR_model_5), # warning about exceeded the maximum treedepth
                             #ID_insect = loo(OBR_model_6), # warning about exceeded the maximum treedepth
                             #ID_insect_int = loo(OBR_model_8), # warning about exceeded the maximum treedepth
                             #ID_insect_noise = loo(OBR_model_10), # warning about exceeded the maximum treedepth
                             #ID_insect_noise_int = loo(OBR_model_12), # warning about exceeded the maximum treedepth
                             ID_insect_noise_bat = loo(OBR_model_14), # warning about exceeded the maximum treedepth
                             ID_insect_noise_bat_int = loo(OBR_model_15) # warning about exceeded the maximum treedepth
                             ))

model_parameters(loo_comp)
# model_parameters(loo_comp, include_IC = FALSE, include_ENP = TRUE)


# Predict
newdat <- expand.grid(
  Distance_vegetation = seq(min(Activity_file_for_model$Distance_vegetation),
                            max(Activity_file_for_model$Distance_vegetation),
                            length.out = 100),
  ID = unique(Activity_file_for_model$ID),
  Site="L1"
)
preds <- fitted(OBR_model_4, newdata = newdat, summary = TRUE)
newdat$Estimate <- preds[,"Estimate"]
newdat$Q2.5     <- preds[,"Q2.5"]
newdat$Q97.5    <- preds[,"Q97.5"]

setwd("/home/charlotte/Documents/Post-Doc/Stages/Laureen et Nathan/Analyse/Effet environnement")
png(filename=paste(Sp_select,"_", "Distance_vegetation_bayesian.png",sep=""), height=700, width=1000,res=150)

plot1 = ggplot(newdat, aes(x = Distance_vegetation, y = Estimate, color = ID)) +
  geom_line(linewidth = 1) +
  geom_ribbon(aes(ymin = Q2.5, ymax = Q97.5, fill = ID), alpha = 0.2, color = NA) +
  labs(y = paste0("Predicted Relative Activity for ", Sp_select)) +
  theme_minimal()

print(plot1)
dev.off()

# Activity
# bat activity follows a negative binomial distribution
glm1 <- glmmTMB(nb_triggered_files~ID + 
                  (1|Site), 
                data = Activity_file_for_model, family = nbinom2)

# interaction with distance to vegetation:
glm2 = glmmTMB(nb_triggered_files ~ Distance_vegetation * ID +
                 (1|Site),
               data=Activity_file_for_model, family = nbinom2)

# interaction with distance to vegetation as a quadratic effect :
glm3 = glmmTMB(nb_triggered_files ~ Distance_vegetation * ID + 
                       I(Distance_vegetation^2) * ID +
                       (1|Site),
                     data=Activity_file_for_model, family = nbinom2)

# interaction with insect activity:
glm4 = glmmTMB(nb_triggered_files ~ nb_triggered_files_insects * ID +
                 (1|Site),
               data=Activity_file_for_model, family = nbinom2)

# interaction with distance to vegetation + insect activity :
glm5 = glmmTMB(nb_triggered_files ~ Distance_vegetation * ID + 
                 log10(nb_triggered_files_insects+1) +
                 (1|Site),
               data=Activity_file_for_model, family = nbinom2)

# interaction with distance to vegetation + interaction with insect activity :
glm6 = glmmTMB(nb_triggered_files ~ Distance_vegetation * ID + 
                 nb_triggered_files_insects * ID +
                 (1|Site),
               data=Activity_file_for_model, family = nbinom2)

simulateResiduals(glm1, plot = TRUE) #everything seems okay
simulateResiduals(glm2, plot = TRUE) #need more sites?
simulateResiduals(glm3, plot = TRUE) #everything seems okay
simulateResiduals(glm4, plot = TRUE) #everything seems okay
simulateResiduals(glm5, plot = TRUE) #need more sites?
simulateResiduals(glm6, plot = TRUE)

# Model comparison :
AIC(glm1,glm2,glm3, glm4, glm5, glm6)

summary(glm2)

# # we continue with post-hoc :
# 
# phoc <- emmeans(glm2, pairwise ~ ID, adjust = "tukey") #very adapted in our situation
# phoc
# plot(phoc$emmeans) + labs(title = "Estimated marginal means (GLMM)",
#                           x = "Log-scale estimated mean", y = "Recorder + Sensitivity (ID)")
# #+ theme_minimal()

#### PLOT ####

ggPredict(glm2,se=TRUE,interactive=TRUE)

pr1 = predict_response(glm2, terms = c("Distance_vegetation", "ID"), bias_correction = TRUE)
plot(pr1)

setwd("/home/charlotte/Documents/Post-Doc/Stages/Laureen et Nathan/Analyse/Effet environnement")
png(filename=paste(Sp_select,"_", "Distance_vegetation_GLMM.png",sep=""), height=700, width=1000,res=150)

plot1 = ggplot(pr1, aes(x = x, y = predicted, colour = group)) +
  geom_line(linewidth = 1) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.2, color = NA) +
  labs(y = paste0("Predicted Activity for ", Sp_select),
       x = "Distance to vegetation (m)",
       color = "", fill = "") +
  scale_y_log10() +
  theme_minimal()

print(plot1)
dev.off()



