library(glmmTMB)
library(emmeans)
library(ggplot2)
library(DHARMa)
library(multcompView)
library(multcomp)

design <- read.csv2("design.csv")

#### GLMM ####

glmm_contacts <- glmmTMB(contacts~dist+ID+(1|site), family=nbinom2, data=(design))
summary(glmm_contacts)

#### Chart representation (ggplot) ####

mycol <- c("adm"= "#E69F00","bcd"="#56B4E9","blg"="#009E73","sm4"="#D55E00") 

phoc <- emmeans(glmm_contacts, pairwise ~ ID, adjust = "tukey") #very adapted in our situation
phoc
plot(phoc$emmeans) + labs(title = "Estimated marginal means (GLMM)",
                          x = "Log-scale estimated mean", y = "Recorder + Sensitivity (ID)") 
  + theme_minimal()

#### PLOT : ####

# associating labels to significant pairs (a=0.05) :
phoc_labels <- cld(phoc$emmeans, Letters = letters, alpha = 0.05, adjust = "tukey")
summary(phoc_labels)
phoc_labels$ID <- as.factor(phoc_labels$ID)
phoc_labels$.group <- gsub(" ", "", phoc_labels$.group)

p3 <- ggplot(design, aes(x = ID, y = contacts, fill = recorder)) + 
  geom_boxplot(outlier.shape = NA, color = "grey25") + 
  geom_jitter(width = 0.2, shape = 21, color = "white", alpha = 0.4, stroke = 0.4) +
  scale_fill_manual(values = mycol, 
                    labels = c("adm" = "Audiomoth", "bcd" = "Batcorder",
                               "blg" = "Batlogger", "sm4" = "SM4BAT")) +
  theme_minimal() + 
  labs(x = "Recorder/Settings associations",
       y = "Number of bat pass per night", fill = "Recorders :") +
  theme(
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 11),
    axis.title.x = element_text(margin = margin(t=15)),
    axis.title.y = element_text(vjust = 2),
    panel.grid.major.y = element_line(linewidth = 0.80)) +
  scale_x_discrete(labels = c(
    "adm_low" = "Low",
    "adm_high" = "High",
    "bcd_low" = "Low",
    "bcd_high" = "High",
    "blg_low" = "Low",
    "blg_high" = "High",
    "sm4_low" = "Low",
    "sm4_high" = "High"))

p4 <- p3 + geom_text(data = phoc_labels, inherit.aes = F, #adding letters
                     aes(x = ID, y = max(design$contacts)-50, label = .group),
                     vjust = 0, size = 5)
p4

#### GLMM dataframe ####

# Coeffs extraction
estim <- summary(glmm_contacts)$coeff$cond

# transform into a df
tab_estim <- data.frame(
  param = rownames(estim),
  estim_glmm = estim[, "Estimate"])

tab_estim$param <- gsub("(Intercept)", "adm_high", tab_estim$param)
tab_estim$param <- gsub("[()]", "", tab_estim$param)
tab_estim$param <- gsub("^ID", "", tab_estim$param)

# Bring back estimates' values according to estimates

ref_value <- tab_estim$estim_glmm[1]

# Subtraction of : intercept - estimates of other recorders

tab_estim$custom <- ref_value + tab_estim$estim_glmm
tab_estim$custom[1] <- ref_value # add intercept value in the first cell

############ POST HOC TEST ################ 

emm_ID <- emmeans(glmm_contacts, ~ ID) # Getting means
summary(emm_ID)

post_hoc <- pairs(emm_ID) # Effectuate paired comparisons between IDs
summary(post_hoc)

#### Post hoc dataframe ####

tab_post_h<-as.data.frame(post_hoc)

# Splitting "contrast" in two columns and adding it to df
contrasts_split <- do.call(rbind, strsplit(as.character(tab_post_h$contrast), " - "))
tab_post_h$ID_A <- contrasts_split[,1]
tab_post_h$ID_B <- contrasts_split[,2]

tab_post_h <- tab_post_h[,-c(2,3,4,5)] #cleaning

############# CORRECTION FACTOR ################

# Creation of a column with factors for each recorder without the weakest one (here batcorder low) in the aime to refer to it
design$estimate_glmm<-tab_estim$custom[match(design$ID, tab_estim$param)]

# Extract lines for ID == "bcd_low"
bcd_vals <- design[design$ID == "bcd_low", c("site", "estimate_glmm")]

# Insure that the vector is named by sites
bcd_per_site <- setNames(bcd_vals$estimate_glmm, bcd_vals$site)

# Adding to design
design$estimate_bcd_low <- bcd_per_site[as.character(design$site)]

# Division of recorders' estimates by bcd_low's estimate
design$factors<-exp(design$estimate_glmm)/exp(design$estimate_bcd_low)
# exponential of new estimate to get the right value (glmm in log) and getting a correction factor to apply on contacts

#Reset the bcd_low estimate to 1 so as not to apply a correction factor to the reference recorder
design$factors[design$ID == "bcd_low"] <- 1

#### Application of the correction factor ####

design$cts_with_f <-design$contacts/design$factors
design$cts_with_f <-round(design$cts_with_f)

#### CHECKING ####  

glmm_verif <- glmmTMB(cts_with_f~dist+ID+(1|site), family=nbinom2, data=(design))
summary(glmm_verif)
f_post_hoc <- emmeans(glmm_verif, pairwise ~ ID, adjust= "tukey") 
summary(f_post_hoc)

#### PLOT ####

max_y_value <- max(design$cts_with_f, na.rm = TRUE)
p5 <- ggplot(design, aes(x = ID, y = cts_with_f, fill = recorder)) + 
  geom_boxplot(outlier.shape = NA, color = "grey25") + 
  geom_jitter(width = 0.2, shape = 21, color = "white", alpha = 0.4, stroke = 0.4) +
  scale_fill_manual(values = mycol, 
                    labels = c("adm" = "Audiomoth", "bcd" = "Batcorder",
                               "blg" = "Batlogger", "sm4" = "SM4BAT")) +
  theme_minimal() + 
  labs(x = "Recorder/Settings associations",
       y = "Number of batpass per night", fill = "Recorders :") +
  theme(
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 11),
    axis.title.x = element_text(margin = margin(t=15)),
    axis.title.y = element_text(vjust = 2),
    panel.grid.major.y = element_line(linewidth = 0.80)) +
  scale_x_discrete(labels = c(
    "adm_low" = "Low",
    "adm_high" = "High",
    "bcd_low" = "Low",
    "bcd_high" = "High",
    "blg_low" = "Low",
    "blg_high" = "High",
    "sm4_low" = "Low",
    "sm4_high" = "High"))
p5

#associating labels to significant pairs (a=0.05) :
phoc_labels2 <- cld(f_post_hoc$emmeans, Letters = letters, alpha = 0.05, adjust = "tukey")
summary(phoc_labels2)
#clean up:
phoc_labels2$ID <- as.factor(phoc_labels2$ID)
phoc_labels2$.group <- gsub(" ", "", phoc_labels2$.group)

p6 <- p5 + geom_text(data = phoc_labels2, inherit.aes = F,
                     aes(x = ID, y = max(design$cts_with_f)-8, label = .group),
                     vjust = 0, size = 5)
p6

ggsave("non_siginificant_ID_groups_adjusted.jpeg", plot = p6, width = 20, height = 20, units = "cm")
#########Exporter les data frame en csv
dir.create("data_output")
write.csv2(design, "adj_design.csv", row.names = FALSE ) 
