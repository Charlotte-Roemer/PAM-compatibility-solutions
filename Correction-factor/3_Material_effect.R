
library(tidyverse)
library(glmmTMB)
library(emmeans)
library(ggplot2)
library(viridis)
library(DHARMa)
#library(multcompView)
#library(multcomp)

Activity_file <- read_delim("/home/charlotte/Documents/Post-Doc/Stages/Laureen et Nathan/Analyse/Activity_file.csv")

# Keep only common pipistrelle for 0.5 confidence
Activity_file = Activity_file %>% 
  filter(espece == "Pippip", probability_filter == 0.5)

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

# Change TriggerLevel names

Activity_file$Recorder = as.factor(Activity_file$Recorder)
Activity_file$TriggerLevel = as.factor(Activity_file$TriggerLevel)
#mycol <- c("Audiomoth"= "#E69F00","Batcorder"="#56B4E9","Batlogger"="#009E73","SM4BAT"="#D55E00")
ggplot(Activity_file, aes(x = Recorder, y = nb_triggered_files)) +
  geom_boxplot(aes(fill = TriggerLevel), color = "grey25", linewidth = 0.2, coef = 1.5) +
  #scale_fill_manual(values = mycol) +
  theme_minimal()

####ADAPT CODE ACCORDING TO YOUR RESULTS :

with(Activity_file, shapiro.test(nb_triggered_files))
#nb_triggered_files are significantly different from a normal distribution
with(Activity_file, tapply(nb_triggered_files, Recorder, shapiro.test))
#same for all recorders too
# 16 sample for each recorder, less than 20 --> no normality assumed
# 4 recorders (=modes) + unpaired samplings + no normality --> Anova with Kruskal-Wallis 
kruskal.test(nb_triggered_files~Recorder, data=Activity_file)
#p<0.01 --> there is a difference between AT LEAST two recorders
#Let's see for each (post-hoc) :
pairwise.wilcox.test(Activity_file$nb_triggered_files, Activity_file$Recorder, p.adjust.method = "BH") #BH method to control false discovery rate
#batcorder is significantly different from all other recorders
#batlogger is not significantly different from sm4bat and audiomoth
#sm4bat and audiomoth are significantly differents

#we will now take sites effect into consideration to be more representative :
#we already now that we do not have a normal distribution, in fact it is known that bats follow a negative binomial distribution : 
glm1 <- glmmTMB(nb_triggered_files~Recorder+(1|Site), data = Activity_file, family = nbinom2)
summary(glm1)
#bcd and sm4 significantly different from adm, blg just have a tendancy to increase contacts
simulateResiduals(glm1, plot = TRUE)
#everything seems okay

#now we add distances to vegetation to the model :
glm2 <- glmmTMB(nb_triggered_files~Recorder+Distance_vegetation+(1|Site), data = Activity_file, family = nbinom2)
summary(glm2)

#comparison of both :
AIC(glm1,glm2)

# #distance has a significant effect (p<0.05), so we keep it in the model
# exp(-0.02530) # =0.975 --> each meter farer from vegetation gives 2.5% less contacts

simulateResiduals(glm2, plot = TRUE)

#now adding sensitivity : (ID = recorder + sensi ; for instance adm_low or _high)
glm3 <- glmmTMB(nb_triggered_files~ID+Distance_vegetation+(1|Site), data = Activity_file, family = nbinom2)
summary(glm3)

#comparison of models :
AIC(glm1, glm2, glm3)

#sensitivity has a significant effect on contacts (p<0.01), so we keep it in the model

simulateResiduals(glm3, plot = TRUE)

#still okay, we continue with post-hoc :

phoc <- emmeans(glm3, pairwise ~ ID, adjust = "tukey") #very adapted in our situation
phoc
plot(phoc$emmeans) + labs(title = "Estimated marginal means (GLMM)",
                          x = "Log-scale estimated mean", y = "Recorder + Sensitivity (ID)")
  #+ theme_minimal()

####PLOT :

#associating labels to significant pairs (a=0.05) :
phoc_labels <- cld(phoc$emmeans, Letters = letters, alpha = 0.05, adjust = "tukey")
summary(phoc_labels)

#clean up:
phoc_labels$ID <- as.factor(phoc_labels$ID)
phoc_labels$.group <- gsub(" ", "", phoc_labels$.group)

#plot itself: 
p <- ggplot(Activity_file, aes(x = ID, y = nb_triggered_files, fill = Recorder)) + 
  geom_boxplot(outlier.shape = NA, color = "grey25") + 
  geom_jitter(width = 0.2, shape = 21, color = "white", alpha = 0.4, stroke = 0.4) +
  scale_fill_manual(values = mycol, 
                    labels = c("adm" = "Audiomoth", "bcd" = "Batcorder",
                               "blg" = "Batlogger", "sm4" = "SM4BAT")) +
  theme_minimal() + 
  labs(x = "Recorder and settings association",
       y = "Number of contacts", fill = "Recorders :") +
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

p2 <- p + geom_text(data = phoc_labels, inherit.aes = F,
                    aes(x = ID, y = max(design$contacts)-8, label = .group),
                    vjust = 0, size = 5)
p2

###saving the plot :
ggsave("siginificant_ID_groups.jpeg", plot = p2, width = 20, height = 20, units = "cm")
