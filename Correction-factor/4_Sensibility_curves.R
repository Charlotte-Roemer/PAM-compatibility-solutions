
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
mycol <- c("Audiomoth"= "#E69F00","Batcorder"="#56B4E9","Batlogger"="#009E73","SM4BAT"="#D55E00")

Activity_file <- read_delim("/home/charlotte/Documents/Post-Doc/Stages/Laureen et Nathan/Analyse/Activity_file.csv")

# Keep only common pipistrelle for 0.5 confidence
Activity_file = Activity_file %>% 
  filter(espece == "Pippip", probability_filter == 0.5)

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

# Prepare data for analysis (factors)
Activity_file$Recorder = as.factor(Activity_file$Recorder)
#Activity_file$TriggerLevel = as.factor(Activity_file$TriggerLevel)
#Activity_file$ID = as.factor(Activity_file$ID)

# Calculate Relative activity to make results comparable
Activity_file <- Activity_file %>%
  group_by(DateNight) %>%
  mutate(
    sm4bat_0_activity = nb_triggered_files[ID == "SM4BAT_0"],
    Relative_activity = nb_triggered_files / sm4bat_0_activity
  ) %>%
  ungroup() %>% 
  as.data.frame()

#scaling sensitivities to 0;1
Activity_file <- Activity_file %>%
  group_by(Recorder) %>%
  mutate(TriggerLevel_scaled=(TriggerLevel_adjusted-min(TriggerLevel_adjusted))/(max(TriggerLevel_adjusted)-min(TriggerLevel_adjusted))) %>%
  ungroup()

#### MODEL ####
OBR_model = ordbetareg(bf(Relative_activity ~ TriggerLevel_scaled * Recorder + 
                            I(TriggerLevel_scaled^2) * Recorder), 
                       data=Activity_file, 
                       chains=1,iter=2000,refresh=0)

newdat <- expand.grid(
  TriggerLevel_scaled = seq(min(Activity_file$TriggerLevel_scaled),
                            max(Activity_file$TriggerLevel_scaled),
                            length.out = 100),
  Recorder = unique(Activity_file$Recorder)
)
preds <- fitted(OBR_model, newdata = newdat, summary = TRUE)
newdat$Estimate <- preds[,"Estimate"]
newdat$Q2.5     <- preds[,"Q2.5"]
newdat$Q97.5    <- preds[,"Q97.5"]

ggplot(newdat, aes(x = TriggerLevel_scaled, y = Estimate, color = Recorder)) +
  geom_line(linewidth = 1) +
  geom_ribbon(aes(ymin = Q2.5, ymax = Q97.5, fill = Recorder), alpha = 0.2, color = NA) +
  labs(y = "Predicted Relative Activity") +
  theme_minimal()




#gam_model <- gam(Relative_activity~s(TriggerLevel_scaled, by=Recorder), data=Activity_file, fx=T, family = betar()) #k may change according to the real number of settings
#model_p <- predict_gam(gam_model)
#model_p

# #scaling sensitivities to 0;1
# model_p <- model_p %>%
#   group_by(Recorder) %>%
#   mutate(TriggerLevel_scaled=(TriggerLevel-min(TriggerLevel))/(max(TriggerLevel)-min(TriggerLevel))) %>%
#   ungroup() 

# p_gam <- ggplot(model_p, aes(x = TriggerLevel_scaled, y = Relative_activity, color = Recorder)) +
#   geom_line(linewidth = 1.25) +
#   geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, fill = Recorder), alpha = 0.15, color = NA) +
#   scale_color_manual(values = mycol) +
#   scale_fill_manual(values = mycol) +
#   labs(x = "Scaled Trigger Level",
#        y = "Relative number of triggered files per night",
#        color = "Recorders :",
#        fill = "Recorders :") +
#   theme_minimal() +
#   theme(axis.text = element_text(size = 10),
#         axis.title = element_text(size = 11),
#         axis.title.x = element_text(margin = margin(t = 15)),
#         axis.title.y = element_text(vjust = 2),
#         panel.grid.major.y = element_line(linewidth = 0.8))
# p_gam

#### EQUATION EXTRACTION 

#gam model showed linear relations, so we can make simple glm :
glm_addit_poiss <- glm(relative_contacts~recorder+sensi_val, data = design_tcs, family = poisson)
glm_inter_poiss <- glm(relative_contacts~recorder*sensi_val, data = design_tcs, family = poisson)
glm_addit_nb <- glm.nb(relative_contacts~recorder+sensi_val, data = design_tcs)
glm_inter_nb <- glm.nb(relative_contacts~recorder*sensi_val, data = design_tcs)

AIC(glm_addit_nb, glm_inter_nb)
AIC(glm_addit_poiss, glm_inter_poiss)

#additive + negativebinom model has better AIC

summary(glm_addit_nb)
simulateResiduals(glm_addit_nb, plot = T) #conditions of application

#gathering coeff :

coef(summary(glm_addit_nb))
coefs <- coef(glm_addit_nb)
recorder <- c("adm", "bcd", "blg", "sm4")
sapply(recorder, get_eq) #FUN GIVING GLM EQUATION cf: fun_for_bats.R

#### PLOT ####

a <- coefs["sensi_val"]
#dataframe with intercepts per recorder :
equations <- data.frame(recorder=recorder, intercept=sapply(recorder, eq.df), slope=a) #application of eq.fr, cf : fun_for_bats.R

#sensi values grid
sensi_vals <- seq(0, 1.5, length.out = 100)

#glm curves construction
curve_data <- equations %>%
  group_by(recorder) %>%
  do({data.frame(sensi_val = sensi_vals,
                 contacts = exp(.$slope * sensi_vals + .$intercept))})

#rescaling on 0;1
curve_data <- curve_data %>%
  group_by(recorder) %>%
  mutate(sensi_scaled=(sensi_val-min(sensi_val))/(max(sensi_val)-min(sensi_val))) %>%
  ungroup()

p_eq <- ggplot(curve_data, aes(x = sensi_scaled, y = contacts, color = recorder)) +
  geom_line(linewidth = 1.3) +
  scale_color_manual(values = mycol) +
  labs(title = "Sensibility curves", x = "Sensibility", y = "Number of relative contacts", color = "Recorders :") +
  theme_minimal() +
  theme(axis.title = element_text(size = 12), axis.text = element_text(size = 10))
p_eq

#######
######################## CURVES ADJUSTMENT #############################
#######

design <- read.csv2("design.csv")
design$recorder <- as.factor(design$recorder)

#### Guarantee for relative contacts
ref_value <- mean(design$contacts[design$ID == "sm4_high"], na.rm = TRUE)
design$relative_contacts <- design$contacts / ref_value

#### GLMM for environment influence
glmm_tie <- glmmTMB(relative_contacts~ID+dist+(1|site), family = nbinom2, data = design)
summary(glmm_tie)

# Keeping only low coeffs
coefs <- fixef(glmm_tie)$cond
coefs_low <- coefs[grep("ID.*_low", names(coefs))]

# Df cleaned
df_low <- data.frame(
  recorder = gsub("ID|_low", "", names(coefs_low)),
  estimate = unname(coefs_low))

# Intercept of glm_addit_nb model for each recorder
model_means <- equations %>%
  mutate(mean_model = exp(intercept + slope * mean(sensi_vals))) %>%
  dplyr::select(recorder, mean_model)

# Merger with means observed by taking envirpnment into account --> difference
adjustments <- left_join(model_means, df_low, by = "recorder") %>%
  mutate(adj_factor = estimate - mean_model)

#### UPDATING EQUATIONS

# Adding adjustment to a new df
equations_adj <- left_join(equations, adjustments, by = "recorder")

# Adjusted curves
curve_data_adj <- equations_adj %>%
  group_by(recorder) %>%
  do({data.frame(sensi_val = sensi_vals,
                 contacts = exp(.$slope * sensi_vals + .$intercept) + .$adj_factor #adjusment here !!
  )}) %>%
  ungroup()

# Rescaling on 0;1
curve_data_adj <- curve_data_adj %>%
  group_by(recorder) %>%
  mutate(sensi_scaled=(sensi_val-min(sensi_val))/(max(sensi_val)-min(sensi_val))) %>%
  ungroup()

#### PLOT ####
p_eq_adj <- ggplot(curve_data_adj, aes(x = sensi_scaled, y = contacts, color = recorder)) +
  geom_line(linewidth = 1.3) +
  scale_color_manual(values = mycol) +
  labs(title = "Adjusted sensibility curves", x = "Sensibility", y = "Number of relative contacts", color = "Recorders :") +
  theme_minimal() +
  theme(axis.title = element_text(size = 12), axis.text = element_text(size = 10))
p_eq_adj

