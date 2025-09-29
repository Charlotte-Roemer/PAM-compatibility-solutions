source("fun_for_bats.R")

library(ggplot2)
library(tidygam)
library(mgcv)
library(dplyr)
library(MASS)
library(DHARMa)
library(glmmTMB)
mycol <- c("adm"= "#E69F00","bcd"="#56B4E9","blg"="#009E73","sm4"="#D55E00")

# upload data
design_tcs <- read.csv2("design_tcs.csv")
design_tcs$sensi <- as.factor(design_tcs$sensi)
design_tcs$recorder <- as.factor(design_tcs$recorder)
design_tcs$ID <- as.factor(design_tcs$ID)
summary(design_tcs)
design_tcs <- design_tcs %>%
  group_by(recorder) %>%
  mutate(relative_contacts=(contacts-min(contacts))/(max(contacts)-min(contacts))) #guarantee for relative contacts

#### GAM MODEL ####
gam_model <- gam(relative_contacts~s(sensi_val, by=recorder, k=11), data=design_tcs, fx=T) #k may change accorind to the real number of settings

model_p <- predict_gam(gam_model)
model_p

#scaling sensibilities to 0;1
model_p <- model_p %>%
  group_by(recorder) %>%
  mutate(sensi_scaled=(sensi_val-min(sensi_val))/(max(sensi_val)-min(sensi_val))) %>%
  ungroup() 

p_gam <- ggplot(model_p, aes(x = sensi_scaled, y = relative_contacts, color = recorder)) +
  geom_line(linewidth = 1.25) +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, fill = recorder), alpha = 0.15, color = NA) +
  scale_color_manual(values = mycol) +
  scale_fill_manual(values = mycol) +
  labs(x = "Sensibilities",
       y = "Relative contacts",
       color = "Recorders :",
       fill = "Recorders :") +
  theme_minimal() +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 11),
        axis.title.x = element_text(margin = margin(t = 15)),
        axis.title.y = element_text(vjust = 2),
        panel.grid.major.y = element_line(linewidth = 0.8))
p_gam

# Now we see the real shape of the curves for each recorder

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

