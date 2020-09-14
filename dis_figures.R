library(readxl)
library(dplyr)
library(ggplot2)
library(effects)
library(magrittr)

theme_set(theme_classic())

dat <- read_xlsx("./Dissertation Data/dissertationdataCOMPLETE.xlsx")

#### Figure 4.1 ####

mod1 <- lm(orgdehum ~ sblm*soe, data = dat)

inter.sd <- effect(c("sblm*soe"), mod1,
                   xlevels = list(sblm = c(1:7),
                                  soe = c(mean(dat$soe)-sd(dat$soe), 
                                          mean(dat$soe)+sd(dat$soe))))
inter.sd <- as.data.frame(inter.sd)

# Create factors of the moderator

inter.sd$soe <- factor(inter.sd$soe,
                       levels = c(mean(dat$soe)-sd(dat$soe), mean(dat$soe)+sd(dat$soec)),
                       labels = c("-1 SD SOE", "+1 SD SOE"))

# Plot 
fig4.1 <- ggplot(data = inter.sd, aes(x = sblm, y = fit, group = soe, linetype = soe)) +
  geom_line(size = 1) +
  scale_linetype_manual(values = c("dashed", "solid")) +
  scale_x_continuous("Supervisor Bottom-Line Mentality", limits = c(1, 7), breaks = c(1:7)) +
  scale_y_continuous("Organizational Dehumanization", limits = c(1, 7), breaks = c(1:7)) +
  guides(shape = guide_legend(title = NULL, reverse = TRUE, keywidth = 5),
         linetype = guide_legend(title = NULL, reverse = TRUE, keywidth = 5)) +
  theme(text = element_text(size = 13))

ggsave("./Output/Figure 4.1.jpg", width = 7, height = 4, fig4.1, dpi = 300)



#####################
#### Figure 4.2 #####
#####################

mod2 <- lm(exploitatt ~ sblm*soe, data = dat)
inter.sd <- effect(c("sblm*soe"), mod2,
                   xlevels = list(sblm = c(1:7),
                                  soe = c(mean(dat$soe)-sd(dat$soe), 
                                          mean(dat$soe)+sd(dat$soe))))
inter.sd <- as.data.frame(inter.sd)

# Create factors of the moderator

inter.sd$soe <- factor(inter.sd$soe,
                       levels = c(mean(dat$soe)-sd(dat$soe), mean(dat$soe)+sd(dat$soec)),
                       labels = c("-1 SD SOE", "+1 SD SOE"))

# Plot 
fig4.2 <- ggplot(data = inter.sd, aes(x = sblm, y = fit, group = soe, linetype = soe)) +
  geom_line(size = 1) +
  scale_linetype_manual(values = c("dashed", "solid")) +
  scale_x_continuous("Supervisor Bottom-Line Mentality", limits = c(1, 7), breaks = c(1:7)) +
  scale_y_continuous("Organization-Centric HR Attributions", limits = c(1, 7), breaks = c(1:7)) +
  guides(shape = guide_legend(title = NULL, reverse = TRUE, keywidth = 5),
         linetype = guide_legend(title = NULL, reverse = TRUE, keywidth = 5)) +
  theme(text = element_text(size = 13))

ggsave("./Output/Figure 4.2.jpg", width = 7, height = 4, fig4.2, dpi = 300)

#### Figure 4.3 ####

mod3 <- lm(wbatt ~ sblm*soe, data = dat)
inter.sd <- effect(c("sblm*soe"), mod3,
                   xlevels = list(sblm = c(1:7),
                                  soe = c(mean(dat$soe)-sd(dat$soe), 
                                          mean(dat$soe)+sd(dat$soe))))
inter.sd <- as.data.frame(inter.sd)

# Create factors of the moderator

inter.sd$soe <- factor(inter.sd$soe,
                       levels = c(mean(dat$soe)-sd(dat$soe), mean(dat$soe)+sd(dat$soec)),
                       labels = c("-1 SD SOE", "+1 SD SOE"))

# Plot 
fig4.3 <- ggplot(data = inter.sd, aes(x = sblm, y = fit, group = soe, linetype = soe)) +
  geom_line(size = 1) +
  scale_linetype_manual(values = c("dashed", "solid")) +
  scale_x_continuous("Supervisor Bottom-Line Mentality", limits = c(1, 7), breaks = c(1:7)) +
  scale_y_continuous("Employee-Centric HR Attributions", limits = c(1, 7), breaks = c(1:7)) +
  guides(shape = guide_legend(title = NULL, reverse = TRUE, keywidth = 5),
         linetype = guide_legend(title = NULL, reverse = TRUE, keywidth = 5)) +
  theme(text = element_text(size = 13))

ggsave("./Output/Figure 4.3.jpg", width = 7, height = 4, fig4.3, dpi = 300)

##################################################################################################
#### Testing More Levels ####

#### Figure 4.1 ####

mod1 <- lm(orgdehum ~ sblm*soe, data = dat)

inter.sd <- effect(c("sblm*soe"), mod1,
                   xlevels = list(sblm = c(1:7),
                                  soe = c(mean(dat$soe)-2*sd(dat$soe), 
                                          mean(dat$soe)-sd(dat$soe),
                                          mean(dat$soe),
                                          mean(dat$soe)+sd(dat$soe),
                                          mean(dat$soe)+2*sd(dat$soe))))
inter.sd <- as.data.frame(inter.sd)

# Create factors of the moderator

inter.sd$soe <- factor(inter.sd$soe,
                       levels = c(mean(dat$soe)-2*sd(dat$soe), 
                                  mean(dat$soe)-sd(dat$soe),
                                  mean(dat$soe),
                                  mean(dat$soe)+sd(dat$soe),
                                  mean(dat$soe)+2*sd(dat$soe)),
                       labels = c("-2 SD, SOE", "-1 SD SOE", "Mean SOE", "+1 SD SOE", "+2 SD SOE"))

# Plot 
ggplot(data = inter.sd, aes(x = sblm, y = fit, group = soe, linetype = soe)) +
  geom_line(size = 1) +
  scale_linetype_manual(values = c("dotted", "dotdash", "dashed", "longdash", "solid")) +
  scale_x_continuous("Supervisor Bottom-Line Mentality", limits = c(1, 7), breaks = c(1:7)) +
  scale_y_continuous("Organizational Dehumanization", limits = c(1, 7), breaks = c(1:7)) +
  guides(shape = guide_legend(title = NULL, reverse = TRUE, keywidth = 5),
         linetype = guide_legend(title = NULL, reverse = TRUE, keywidth = 5)) +
  theme(text = element_text(size = 13))

#################################################################################################
#### Visualizing Performance Ceiling Effect ####

fig4.4 <- ggplot(data = dat, aes(x = orgdehum, y = perf)) + geom_point(size = .5) +
  scale_x_continuous("Organizational Dehumanization", breaks = c(1:7)) +
  scale_y_continuous("Task Performance", limits = c(1, 7), breaks = c(1:7)) +
  theme(text = element_text(size = 13))

ggsave("./Output/Figure 4.4.jpg", width = 7, height = 4, fig4.4, dpi = 300)

fig4.5 <- ggplot(data = dat, aes(x = perf)) + 
  geom_histogram(binwidth = 1, color = "black", fill = "gray") +
  scale_x_continuous("Task Performance", limits = c(.5, 7.5), breaks = c(1:7)) +
  scale_y_continuous("Frequency", expand = c(0, 0)) +
  theme(text = element_text(size = 13))  

ggsave("./Output/Figure 4.5.jpg", width = 7, height = 4, fig4.5, dpi = 300)

sum(dat$perf > 4)

#### Visualizing CWB Floor Effect ####

fig4.6 <- ggplot(data = dat, aes(x = orgdehum, y = cwb)) + geom_point(size = .5) +
  scale_x_continuous("Organizational Dehumanization", breaks = c(1:7)) +
  scale_y_continuous("Counterproductive Work Behaviors", limits = c(1, 5), breaks = c(1:5)) +
  theme(text = element_text(size = 13))

ggsave("./Output/Figure 4.6.jpg", width = 7, height = 4, fig4.6, dpi = 300)

fig4.7 <- ggplot(data = dat, aes(x = cwb)) + 
  geom_histogram(binwidth = 1, color = "black", fill = "gray") +
  scale_x_continuous("Counterproductive Work Behaviors", limits = c(.5, 5.5), breaks = c(1:5)) +
  scale_y_continuous("Frequency", expand = c(0, 0), limits = c(0, 300), breaks = seq(0, 300, by = 50)) +
  theme(text = element_text(size = 13))  

ggsave("./Output/Figure 4.7.jpg", width = 7, height = 4, fig4.7, dpi = 300)

#################################################################################################
#### Demographics #####

### Org Type ###

# New version of dplyr interferes with summarySE, so load and unload
library(dplyr)
dat %<>%
  mutate(orgtype = recode_factor(orgtype, `1` = "For-profit",
                                 `0` = "Nonprofit"))
detach("package:dplyr", unload = TRUE)

source("summarySE.R")
fig4.8 <- dat %>%
  summarySE(., measurevar = "orgdehum", groupvars = "orgtype", na.rm = TRUE) %>%
  ggplot(aes(x = orgtype, y = orgdehum, group = orgtype, fill = orgtype)) +
  geom_bar(stat = "identity", width = 0.3) +
  scale_fill_grey(start = .4, end = .8) +
  geom_errorbar(aes(ymin = orgdehum - ci, ymax = orgdehum + ci), size = 0.5, width = 0.07) +
  scale_y_continuous(expand = c(0,0), limits = c(0,7), breaks = c(1:7)) +
  labs(x = "Organization Type", y = "Organizational Dehumanization") +
  theme(text = element_text(size = 13)) + guides(fill = FALSE)

ggsave("./Output/Figure 4.8.jpg", width = 6, height = 5, fig4.8, dpi = 300)

source("summarySE.R")
fig4.9 <- dat %>%
  summarySE(., measurevar = "exploitatt", groupvars = "orgtype", na.rm = TRUE) %>%
  ggplot(aes(x = orgtype, y = exploitatt, group = orgtype, fill = orgtype)) +
  geom_bar(stat = "identity", width = 0.3) +
  scale_fill_grey(start = .4, end = .8) +
  geom_errorbar(aes(ymin = exploitatt - ci, ymax = exploitatt + ci), size = 0.5, width = 0.07) +
  scale_y_continuous(expand = c(0,0), limits = c(0,7), breaks = c(1:7)) +
  labs(x = "Organization Type", y = "Organization-Centric HR Attributions") +
  theme(text = element_text(size = 13)) + guides(fill = FALSE)

ggsave("./Output/Figure 4.9.jpg", width = 6, height = 5, fig4.9, dpi = 300)

source("summarySE.R")
fig4.10 <- dat %>%
  summarySE(., measurevar = "sblm", groupvars = "orgtype", na.rm = TRUE) %>%
  ggplot(aes(x = orgtype, y = sblm, group = orgtype, fill = orgtype)) +
  geom_bar(stat = "identity", width = 0.3) +
  scale_fill_grey(start = .4, end = .8) +
  geom_errorbar(aes(ymin = sblm - ci, ymax = sblm + ci), size = 0.5, width = 0.07) +
  scale_y_continuous(expand = c(0,0), limits = c(0,7), breaks = c(1:7)) +
  labs(x = "Organization Type", y = "Supervisor Bottom-line Mentality") +
  theme(text = element_text(size = 13)) + guides(fill = FALSE)

ggsave("./Output/Figure 4.10.jpg", width = 6, height = 5, fig4.10, dpi = 300)

### Responsibility for Direct Reports ###
library(dplyr)
dat %<>%
  mutate(dirreport = recode(dirreport, `1` = "Yes",
                          `0` = "No"))
detach("package:dplyr", unload = TRUE)


source("summarySE.R")
fig4.11 <- dat %>%
  summarySE(., measurevar = "orgdehum", groupvars = "dirreport", na.rm = TRUE) %>%
  ggplot(aes(x = dirreport, y = orgdehum, group = dirreport, fill = dirreport)) +
  geom_bar(stat = "identity", width = 0.3) +
  scale_fill_grey(start = .4, end = .8) +
  geom_errorbar(aes(ymin = orgdehum - ci, ymax = orgdehum + ci), size = 0.5, width = 0.07) +
  scale_y_continuous(expand = c(0,0), limits = c(0,7), breaks = c(1:7)) +
  labs(x = "Responsibility for Direct Reports", y = "Organizational Dehumanization") +
  theme(text = element_text(size = 13)) + guides(fill = FALSE)

ggsave("./Output/Figure 4.11.jpg", width = 6, height = 5, fig4.11, dpi = 300)

source("summarySE.R")
fig4.12 <- dat %>%
  summarySE(., measurevar = "wbatt", groupvars = "dirreport", na.rm = TRUE) %>%
  ggplot(aes(x = dirreport, y = wbatt, group = dirreport, fill = dirreport)) +
  geom_bar(stat = "identity", width = 0.3) +
  scale_fill_grey(start = .4, end = .8) +
  geom_errorbar(aes(ymin = wbatt - ci, ymax = wbatt + ci), size = 0.5, width = 0.07) +
  scale_y_continuous(expand = c(0,0), limits = c(0,7), breaks = c(1:7)) +
  labs(x = "Responsibility for Direct Reports", y = "Employee-Centric HR Attributions") +
  theme(text = element_text(size = 13)) + guides(fill = FALSE)

ggsave("./Output/Figure 4.12.jpg", width = 6, height = 5, fig4.12, dpi = 300)

#################################################################################################

library(dplyr)
fig4.13 <- dat %>%
  filter(industry %in% c("arts, entertainment, recreation", "accommodation, food services")) %>%
  summarySE(., measurevar = "orgdehum", groupvars = "industry", na.rm = TRUE) %>%
  ggplot(aes(x = industry, y = orgdehum, group = industry, fill = industry)) +
  geom_bar(stat = "identity", width = 0.3) +
  scale_fill_grey(start = .4, end = .8) +
  geom_errorbar(aes(ymin = orgdehum - ci, ymax = orgdehum + ci), size = 0.5, width = 0.07) +
  scale_x_discrete(labels = c("Accommodation, Food Services", "Arts, Entertainment, Recreation")) +
  scale_y_continuous(expand = c(0,0), limits = c(0,7), breaks = c(1:7)) +
  labs(x = "Industry", y = "Organizational Dehumanization") +
  theme(text = element_text(size = 13)) + guides(fill = FALSE)

ggsave("./Output/Figure 4.13.jpg", width = 6, height = 5, fig4.13, dpi = 300)

fig4.14 <- dat %>%
  filter(industry %in% c("finance, insurance", "accommodation, food services")) %>%
  summarySE(., measurevar = "orgdehum", groupvars = "industry", na.rm = TRUE) %>%
  ggplot(aes(x = industry, y = orgdehum, group = industry, fill = industry)) +
  scale_fill_grey(start = .4, end = .8) +
  geom_bar(stat = "identity", width = 0.3) +
  geom_errorbar(aes(ymin = orgdehum - ci, ymax = orgdehum + ci), size = 0.5, width = 0.07) +
  scale_x_discrete(labels = c("Accommodation, Food Services", "Finance, Insurance")) +  
  scale_y_continuous(expand = c(0,0), limits = c(0,7), breaks = c(1:7)) +
  labs(x = "Industry", y = "Organizational Dehumanization") +
  theme(text = element_text(size = 13)) + guides(fill = FALSE)

ggsave("./Output/Figure 4.14.jpg", width = 6, height = 5, fig4.14, dpi = 300)

fig4.15 <- dat %>%
  filter(industry %in% c("retail trade", "arts, entertainment, recreation")) %>%
  summarySE(., measurevar = "orgdehum", groupvars = "industry", na.rm = TRUE) %>%
  ggplot(aes(x = industry, y = orgdehum, group = industry, fill = industry)) +
  geom_bar(stat = "identity", width = 0.3) +
  scale_fill_grey(start = .4, end = .8) +
  geom_errorbar(aes(ymin = orgdehum - ci, ymax = orgdehum + ci), size = 0.5, width = 0.07) +
  scale_x_discrete(labels = c("Arts, Entertainment, Recreation", "Retail Trade")) +
  scale_y_continuous(expand = c(0,0), limits = c(0,7), breaks = c(1:7)) +
  labs(x = "Industry", y = "Organizational Dehumanization") +
  theme(text = element_text(size = 13)) + guides(fill = FALSE)

ggsave("./Output/Figure 4.15.jpg", width = 6, height = 5, fig4.15, dpi = 300)

fig4.16 <- dat %>%
  filter(industry %in% c("retail trade", "educational services")) %>%
  summarySE(., measurevar = "orgdehum", groupvars = "industry", na.rm = TRUE) %>%
  ggplot(aes(x = industry, y = orgdehum, group = industry, fill = industry)) +
  geom_bar(stat = "identity", width = 0.3) +
  scale_fill_grey(start = .4, end = .8) +
  geom_errorbar(aes(ymin = orgdehum - ci, ymax = orgdehum + ci), size = 0.5, width = 0.07) +
  scale_x_discrete(labels = c("Educational Services", "Retail Trade")) +
  scale_y_continuous(expand = c(0,0), limits = c(0,7), breaks = c(1:7)) +
  labs(x = "Industry", y = "Organizational Dehumanization") +
  theme(text = element_text(size = 13)) + guides(fill = FALSE)

ggsave("./Output/Figure 4.16.jpg", width = 6, height = 5, fig4.16, dpi = 300)

fig4.17 <- dat %>%
  filter(industry %in% c("retail trade", "finance, insurance")) %>%
  summarySE(., measurevar = "orgdehum", groupvars = "industry", na.rm = TRUE) %>%
  ggplot(aes(x = industry, y = orgdehum, group = industry, fill = industry)) +
  geom_bar(stat = "identity", width = 0.3) +
  scale_fill_grey(start = .4, end = .8) +
  geom_errorbar(aes(ymin = orgdehum - ci, ymax = orgdehum + ci), size = 0.5, width = 0.07) +
  scale_x_discrete(labels = c("Finance, Insurance", "Retail Trade")) +
  scale_y_continuous(expand = c(0,0), limits = c(0,7), breaks = c(1:7)) +
  labs(x = "Industry", y = "Organizational Dehumanization") +
  theme(text = element_text(size = 13)) + guides(fill = FALSE)

ggsave("./Output/Figure 4.17.jpg", width = 6, height = 5, fig4.17, dpi = 300)

fig4.18 <- dat %>%
  filter(industry %in% c("educational services", "accommodation, food services")) %>%
  summarySE(., measurevar = "sblm", groupvars = "industry", na.rm = TRUE) %>%
  ggplot(aes(x = industry, y = sblm, group = industry, fill = industry)) +
  geom_bar(stat = "identity", width = 0.3) +
  scale_fill_grey(start = .4, end = .8) +
  geom_errorbar(aes(ymin = sblm - ci, ymax = sblm + ci), size = 0.5, width = 0.07) +
  scale_x_discrete(labels = c("Accommodation, Food Services", "Educational Services")) +
  scale_y_continuous(expand = c(0,0), limits = c(0,7), breaks = c(1:7)) +
  labs(x = "Industry", y = "Supervisor Bottom-Line Mentality") +
  theme(text = element_text(size = 13)) + guides(fill = FALSE)

ggsave("./Output/Figure 4.18.jpg", width = 6, height = 5, fig4.18, dpi = 300)

fig4.19 <- dat %>%
  filter(industry %in% c("finance, insurance", "accommodation, food services")) %>%
  summarySE(., measurevar = "sblm", groupvars = "industry", na.rm = TRUE) %>%
  ggplot(aes(x = industry, y = sblm, group = industry, fill = industry)) +
  geom_bar(stat = "identity", width = 0.3) +
  scale_fill_grey(start = .4, end = .8) +
  geom_errorbar(aes(ymin = sblm - ci, ymax = sblm + ci), size = 0.5, width = 0.07) +
  scale_x_discrete(labels = c("Accommodation, Food Services", "Finance, Insurance")) +
  scale_y_continuous(expand = c(0,0), limits = c(0,7), breaks = c(1:7)) +
  labs(x = "Industry", y = "Supervisor Bottom-Line Mentality") +
  theme(text = element_text(size = 13)) + guides(fill = FALSE)

ggsave("./Output/Figure 4.19.jpg", width = 6, height = 5, fig4.19, dpi = 300)

fig4.20 <- dat %>%
  filter(industry %in% c("retail trade", "educational services")) %>%
  summarySE(., measurevar = "sblm", groupvars = "industry", na.rm = TRUE) %>%
  ggplot(aes(x = industry, y = sblm, group = industry, fill = industry)) +
  geom_bar(stat = "identity", width = 0.3) +
  scale_fill_grey(start = .4, end = .8) +
  geom_errorbar(aes(ymin = sblm - ci, ymax = sblm + ci), size = 0.5, width = 0.07) +
  scale_x_discrete(labels = c("Educational Services", "Retail Trade")) +
  scale_y_continuous(expand = c(0,0), limits = c(0,7), breaks = c(1:7)) +
  labs(x = "Industry", y = "Supervisor Bottom-Line Mentality") +
  theme(text = element_text(size = 13)) + guides(fill = FALSE)

ggsave("./Output/Figure 4.20.jpg", width = 6, height = 5, fig4.20, dpi = 300)

fig4.21 <- dat %>%
  filter(industry %in% c("retail trade", "finance, insurance")) %>%
  summarySE(., measurevar = "sblm", groupvars = "industry", na.rm = TRUE) %>%
  ggplot(aes(x = industry, y = sblm, group = industry, fill = industry)) +
  geom_bar(stat = "identity", width = 0.3) +
  scale_fill_grey(start = .4, end = .8) +
  geom_errorbar(aes(ymin = sblm - ci, ymax = sblm + ci), size = 0.5, width = 0.07) +
  scale_x_discrete(labels = c("Finance, Insurance", "Retail Trade")) +
  scale_y_continuous(expand = c(0,0), limits = c(0,7), breaks = c(1:7)) +
  labs(x = "Industry", y = "Supervisor Bottom-Line Mentality") +
  theme(text = element_text(size = 13)) + guides(fill = FALSE)

ggsave("./Output/Figure 4.21.jpg", width = 6, height = 5, fig4.21, dpi = 300)

#######################################################################################
### Appendix Figures

summary(dat$orgtenure)

figa.1 <- ggplot(data = dat, aes(x = orgtenure)) + 
  geom_histogram(binwidth = 1, color = "black", fill = "gray") +
  scale_x_continuous("Organization Tenure (Years)", limits = c(0, 42), breaks = c(0:42)) +
  scale_y_continuous("Frequency", expand = c(0, 0), limits = c(0, 60), breaks = seq(0, 60, by = 10)) +
  theme(text = element_text(size = 13), axis.text.x = element_text(size = 7, angle = 30))  

ggsave("./Output/Figure A.1.jpg", width = 7, height = 4, figa.1, dpi = 300)

summary(dat$postenure)

figa.2 <- ggplot(data = dat, aes(x = postenure)) + 
  geom_histogram(binwidth = 1, color = "black", fill = "gray") +
  scale_x_continuous("Job Tenure (Years)", limits = c(0, 30), breaks = c(0:30)) +
  scale_y_continuous("Frequency", expand = c(0, 0), limits = c(0, 80), breaks = seq(0, 80, by = 20)) +
  theme(text = element_text(size = 13), axis.text.x = element_text(size = 7, angle = 30))  

ggsave("./Output/Figure A.2.jpg", width = 7, height = 4, figa.2, dpi = 300)

summary(dat$suptenure)

figa.3 <- ggplot(data = dat, aes(x = suptenure)) + 
  geom_histogram(binwidth = 1, color = "black", fill = "gray") +
  scale_x_continuous("Tenure with Supervisor (Years)", limits = c(0, 30), breaks = c(0:30)) +
  scale_y_continuous("Frequency", expand = c(0, 0), limits = c(0, 100), breaks = seq(0, 100, by = 20)) +
  theme(text = element_text(size = 13), axis.text.x = element_text(size = 7, angle = 30))  

ggsave("./Output/Figure A.3.jpg", width = 7, height = 4, figa.3, dpi = 300)

summary(dat$weekhours)

figa.4 <- ggplot(data = dat, aes(x = weekhours)) + 
  geom_histogram(binwidth = 5, color = "black", fill = "gray") +
  scale_x_continuous("Weekly Work Hours", limits = c(.5, 75.5), breaks= seq(0, 75, by = 5)) +
  scale_y_continuous("Frequency", expand = c(0, 0), limits = c(0, 250), breaks = seq(0, 250, by = 50)) +
  theme(text = element_text(size = 13))  

ggsave("./Output/Figure A.4.jpg", width = 7, height = 4, figa.4, dpi = 300)


