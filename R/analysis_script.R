library(ggpubr)
library(tidyverse)
library(rstatix)
library(lubridate)

# write raw data file
all_data <- read_csv("data/raw_data.csv")

# find any duplicates
n_occur <- data.frame(table(all_data$prolific_id))
n_occur[n_occur$Freq > 1,]
duplicates <- all_data[all_data$prolific_id %in% n_occur$Var1[n_occur$Freq > 1],]
to_remove <- c(as.numeric(rownames(duplicates)))
all_data <- all_data[-c(to_remove),]

# find missing values along dependent variables
sum(is.na(all_data$urn))
sum(is.na(all_data$vaccination))
remove <- which(is.na(all_data$vaccination))
all_data <- all_data[-remove,]

# comprehension
all_data$comprehension <- NA
all_data$comprehension[all_data$condition=="external" & 
                         all_data$manipulation_check=="By the actual proportion of black balls in the urn for the first question, and by the numbers reported on the official UK Government website for data on Coronavirus on February 1 for the second question."] <- 1
all_data$comprehension[all_data$condition=="external" & 
                         all_data$manipulation_check=="By the mean answer given by participants in this study for each of the two questions."] <- 0
all_data$comprehension[all_data$condition=="self_res" & 
                         all_data$manipulation_check=="By the actual proportion of black balls in the urn for the first question, and by the numbers reported on the official UK Government website for data on Coronavirus on February 1 for the second question."] <- 0
all_data$comprehension[all_data$condition=="self_res" & 
                         all_data$manipulation_check=="By the mean answer given by participants in this study for each of the two questions."] <- 1

prop.table(table("comprehension" = all_data$comprehension, "condition" = all_data$condition),2)

# did those showing greater comprehension spend more time?
library(lubridate)
all_data %>% 
  group_by(comprehension) %>% 
  summarise(median = seconds_to_period(median(time_taken, na.rm=T)))

time_comp <- na.omit(all_data$time_taken[all_data$comprehension==1])
time_no_comp <- na.omit(all_data$time_taken[all_data$comprehension==0])

# very similar distributions
all_data %>% 
  filter(comprehension %in% c(0,1), time_taken < 2000) %>% 
  ggplot() +
  aes(x = time_taken, fill = factor(comprehension)) +
  geom_density(alpha = 0.5)

# not significantly different
wilcox.test(time_comp,time_no_comp)

# remove non-comprehending subjects
all_data$comprehension[all_data$condition=="no_res"] <- 1
table(all_data$comprehension, all_data$condition)
all_data <- subset(all_data, comprehension==1)
table(all_data$condition)

# randomisation check
prop.table(table(all_data$age_cat,all_data$condition),2)
prop.table(table(all_data$gender,all_data$condition),2)
prop.table(table(all_data$education,all_data$condition),2)

# calculate time taken by condition
sum(is.na(all_data$time_taken))

table(all_data$condition)
time_control <- all_data$time_taken[all_data$condition=="no_res"]
time_self_res <- all_data$time_taken[all_data$condition=="self_res"]
time_external <- all_data$time_taken[all_data$condition=="external"]

seconds_to_period(median(time_control, na.rm = TRUE))
seconds_to_period(median(time_self_res, na.rm = TRUE))
seconds_to_period(median(time_external, na.rm = TRUE))

all_data %>% 
  filter(time_taken < 1000) %>% 
  ggplot() +
  aes(x = time_taken, fill = condition) +
  geom_density(alpha = 0.5)

wilcox_external <- wilcox.test(time_control,time_external)
wilcox_selfres <- wilcox.test(time_control,time_self_res)

wilcox_external
wilcox_selfres

# look at distribution by condition
sample_gender <- table(all_data$gender,all_data$condition)
sample_age <- table(all_data$age_cat,all_data$condition)
sample_education <- table(all_data$education,all_data$condition)

table(all_data$condition)

sample_df <- data.frame(rbind(sample_gender,
                              sample_age,
                              sample_education,
                              c(length(all_data$prolific_id[all_data$condition=="external"]),
                                length(all_data$prolific_id[all_data$condition=="no_res"]),
                                length(all_data$prolific_id[all_data$condition=="self_res"]))))

row.names(sample_df) <- c("Female",
                          "Male",
                          "Gender not disclosed",
                          "Age: 18-24",
                          "Age: 25-34",
                          "Age: 35-44",
                          "Age: 45-54",
                          "Age: 55-64",
                          "Age: Over 65",
                          "Age not disclosed",
                          "Education: A-level",
                          "Education: GCSE",
                          "Education: None",
                          "Education: Postgraduate",
                          "Education: Undergraduate",
                          "N")

sample_df <- sample_df[, c(1,3,2)]
sample_df <- sample_df[c(1:10,12,11,15,14,13),]
colnames(sample_df) <- c("External resolution","Self-resolution","Control")
sample_df

# means and sd by condition
all_data %>% 
  group_by(condition) %>% 
  summarise(mean = mean(urn),
            sd = sd(urn))

# visualise estimate distribution
all_data %>% 
  mutate(condition = dplyr::recode(condition,
                            "external" = "External resolution",
                            "no_res" = "Control",
                            "self_res" = "Self-resolution")) %>% 
  ggplot() +
  aes(x = condition, y = urn, fill = condition) +
  geom_violin() +
  geom_jitter(alpha = 0.2) +
  xlab("Condition") +
  ylab("Estimate (%)") +
  theme_minimal() +
  theme(legend.position = "none")

all_data %>% 
  mutate(condition = dplyr::recode(condition,
                                   "external" = "External resolution",
                                   "no_res" = "Control",
                                   "self_res" = "Self-resolution")) %>% 
  ggplot() +
  aes(x = condition, y = vaccination, fill = condition) +
  geom_violin() +
  geom_jitter(alpha = 0.2) +
  xlab("Condition") +
  ylab("Estimate (%)") +
  theme_minimal() +
  theme(legend.position = "none")

# significantly different?
all_data$condition <- as.factor(all_data$condition)
all_data <- within(all_data, condition <- relevel(condition, ref = "no_res"))
m_urn <- lm(urn ~ condition,
   data = all_data)
summary(m_urn)

library(lmtest)
library(sandwich)
m_urn_vcov <- vcovHC(m_urn)
coeftest(m_urn, vcov = m_urn_vcov)

library(car)
library(multcomp)
m_urn.multcomp <- glht(m_urn, linfct = mcp(condition = "Tukey") , vcov = m_urn_vcov)
summary(m_urn.multcomp,test = adjusted("holm"))

m_vaccination <- lm(vaccination ~ condition,
            data = all_data)
summary(m_vaccination)

library(lmtest)
library(sandwich)
m_vaccination_vcov <- vcovHC(m_vaccination)
coeftest(m_vaccination, vcov = m_vaccination_vcov)

library(car)
library(multcomp)
m_vaccination.multcomp <- glht(m_vaccination, linfct = mcp(condition = "Tukey") , vcov = m_vaccination_vcov)
summary(m_vaccination.multcomp,test = adjusted("holm"))

# mean error by condition
all_data$urn_error <- abs(all_data$urn - 70)

all_data %>% 
  group_by(condition) %>% 
  summarise(mean_error = mean(urn_error),
            sd_error = sd(urn_error))

all_data %>% 
  ggplot() +
  aes(x = condition, y = urn_error) +
  geom_violin()  +
  geom_jitter(alpha = 0.2)

# clear that some went for 50%, principle of indifference
# but about same proportion across conditions, so can ignore
test <- all_data %>%
  mutate(princ_of_diff = ifelse(urn == 50, 1, 0)) %>%
  group_by(condition) %>% 
  count(princ_of_diff) %>% 
  mutate(prop = n/sum(n)) %>% 
  filter(princ_of_diff == 1)
  
# UK population: https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates
# Accessed 1 Feb 2021
vacc_correct <- (9296367/66796807) * 100
all_data$vacc_error <- abs(all_data$vaccination - vacc_correct)

all_data %>% 
  group_by(condition) %>% 
  summarise(mean_error = mean(vacc_error),
            sd_error = sd(vacc_error))

all_data %>% 
  ggplot() +
  aes(x = condition, y = vacc_error) +
  geom_violin() +
  geom_jitter(alpha = 0.2)

# table with mean estimates and errors
df_errors <- data.frame(tapply(all_data$urn, all_data$condition, mean),
                        tapply(all_data$urn, all_data$condition, sd),
                        tapply(all_data$urn_error, all_data$condition, mean),
                        tapply(all_data$urn_error, all_data$condition, sd),
                        tapply(all_data$vaccination, all_data$condition, mean),
                        tapply(all_data$vaccination, all_data$condition, sd),
                        tapply(all_data$vacc_error, all_data$condition, mean),
                        tapply(all_data$vacc_error, all_data$condition, sd))
df_errors <- t(df_errors)

colnames(df_errors) <- c("Control","External resolution","Self-resolution")
df_errors <- df_errors[,c(2,3,1)]
df_errors <- round(df_errors,2)

df_errors_1 <- df_errors[c(1,3,5,7),]
df_errors_2 <- df_errors[c(2,4,6,8),]

two_tables_into_one <- as.data.frame(do.call(cbind, lapply(1:ncol(df_errors_1), function(i) paste0(df_errors_1[ , i], " (", df_errors_2[ , i], ")"  ) )))
names(two_tables_into_one) <- names(df_errors_1)
two_tables_into_one

row.names(two_tables_into_one) <- c("Urn: Mean estimate",
                                    "Urn: Mean error",
                                    "Vaccination: Mean estimate",
                                    "Vaccination: Mean error")

# significantly different?
m_urn_accuracy <- lm(urn_error ~ condition,
            data = all_data)
summary(m_urn_accuracy)
# plot(m_urn_accuracy)

library(lmtest)
library(sandwich)
m_urn_accuracy_vcov <- vcovHC(m_urn_accuracy)
coeftest(m_urn_accuracy, vcov = m_urn_accuracy_vcov)
confint(m_urn_accuracy, vcov = m_urn_accuracy_vcov)

urn_error_plot <- data.frame("condition" = c("External", "Self-resolution"),
                             "coef" = c(coeftest(m_urn_accuracy, vcov = m_urn_accuracy_vcov)[2],
                                        coeftest(m_urn_accuracy, vcov = m_urn_accuracy_vcov)[3]),
                             "upr" = c(confint(m_urn_accuracy, vcov = m_urn_accuracy_vcov)[2,2],
                                       confint(m_urn_accuracy, vcov = m_urn_accuracy_vcov)[3,2]),
                             "lwr" = c(confint(m_urn_accuracy, vcov = m_urn_accuracy_vcov)[2,1],
                                       confint(m_urn_accuracy, vcov = m_urn_accuracy_vcov)[3,1]))

ggplot(urn_error_plot, aes(x=condition, y=coef, group=condition, color=condition)) + 
  geom_pointrange(aes(ymin=lwr, ymax=upr)) + # green
  geom_hline(yintercept=0, linetype="11", color = "black") +
  xlab("Condition") +
  ylab("Difference in error (percentage points)") +
  ggtitle("Estimation task: Urn") +
  labs(subtitle = "Estimated differences in error compared to no incentive (OLS with robust standard errors)") +
  theme(plot.title = element_text(face = "bold")) +
  coord_flip() +
  theme_minimal() +
  theme(legend.position = "none") +
  theme(panel.grid.major = element_line(linetype = "dashed")) +
  theme(panel.grid.minor = element_line(linetype = "dashed")) +
  theme(plot.title = element_text(face = "bold"))

m_vaccination_accuracy <- lm(vacc_error ~ condition,
                     data = all_data)
summary(m_vaccination_accuracy)
#plot(m_vaccination_accuracy)

library(lmtest)
library(sandwich)
m_vaccination_accuracy_vcov <- vcovHC(m_vaccination_accuracy)
coeftest(m_vaccination_accuracy, vcov = m_vaccination_accuracy_vcov)
confint(m_vaccination_accuracy, vcov = m_vaccination_accuracy_vcov)

vaccination_error_plot <- data.frame("condition" = c("External", "Self-resolution"),
                             "coef" = c(coeftest(m_vaccination_accuracy, vcov = m_vaccination_accuracy_vcov)[2],
                                        coeftest(m_vaccination_accuracy, vcov = m_vaccination_accuracy_vcov)[3]),
                             "upr" = c(confint(m_vaccination_accuracy, vcov = m_vaccination_accuracy_vcov)[2,2],
                                       confint(m_vaccination_accuracy, vcov = m_vaccination_accuracy_vcov)[3,2]),
                             "lwr" = c(confint(m_vaccination_accuracy, vcov = m_vaccination_accuracy_vcov)[2,1],
                                       confint(m_vaccination_accuracy, vcov = m_vaccination_accuracy_vcov)[3,1]))

ggplot(vaccination_error_plot, aes(x=condition, y=coef, group=condition, color=condition)) + 
  geom_pointrange(aes(ymin=lwr, ymax=upr)) + # green
  geom_hline(yintercept=0, linetype="11", color = "black") +
  xlab("Condition") +
  ylab("Difference in error (percentage points)") +
  ggtitle("Estimation task: Vaccination") +
  labs(subtitle = "Estimated differences in error compared to no incentive (OLS with robust standard errors)") +
  theme(plot.title = element_text(face = "bold")) +
  coord_flip() +
  theme_minimal() +
  theme(legend.position = "none") +
  theme(panel.grid.major = element_line(linetype = "dashed")) +
  theme(panel.grid.minor = element_line(linetype = "dashed")) +
  theme(plot.title = element_text(face = "bold"))

# wilcoxon
wilcox.test(all_data$urn_error[all_data$condition=="no_res"],
            all_data$urn_error[all_data$condition=="self_res"])
wilcox.test(all_data$urn_error[all_data$condition=="no_res"],
            all_data$urn_error[all_data$condition=="external"])

wilcox.test(all_data$vacc_error[all_data$condition=="no_res"],
            all_data$vacc_error[all_data$condition=="self_res"])
wilcox.test(all_data$vacc_error[all_data$condition=="no_res"],
            all_data$vacc_error[all_data$condition=="external"])

# who gets incentive?
set.seed(100)

library(dplyr)
correct_urn_ext <- subset(all_data, urn==70 & condition=="external")
winners <- sample_n(correct_urn_ext, 1)

correct_vacc_ext <- subset(all_data, vaccination==14 & condition=="external")
winners <- rbind(winners, sample_n(correct_vacc_ext, 1))

mean(all_data$urn[all_data$condition=="self_res"])
correct_urn_self <- subset(all_data, urn==64 & condition=="self_res") # none

mean(all_data$vaccination[all_data$condition=="self_res"])
correct_vacc_self <- subset(all_data, vaccination==15 & condition=="self_res")
winners <- rbind(winners, sample_n(correct_vacc_self, 1))

# distribution
all_data %>%
  ggplot( aes(x=urn, fill=condition)) +
  geom_density(alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("green", "red", "blue")) +
  labs(fill="") +
  facet_wrap(~ condition, ncol = 1)

all_data %>%
  ggplot( aes(x=vaccination, fill=condition)) +
  geom_density(alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("green", "red", "blue")) +
  labs(fill="")  +
  facet_wrap(~ condition, ncol = 1)

# outliers
all_data[c(2,6)] %>%
  group_by(condition) %>%
  identify_outliers(urn)

all_data[c(3,6)] %>%
  group_by(condition) %>%
  identify_outliers(vaccination)

# normality
# https://stats.stackexchange.com/questions/9573/t-test-for-non-normal-when-n50#:~:text=For%20a%20t%2Dtest%20to,samples%20from%20non%2Dnormal%20distributions.
all_data %>%
  group_by(condition) %>%
  shapiro_test(urn)

ggqqplot(all_data, x = "urn", facet.by = "condition")

all_data %>%
  group_by(condition) %>%
  shapiro_test(vaccination)

ggqqplot(all_data, x = "vaccination", facet.by = "condition")

# equality of variance
# irrelevant since using Welsh test
bartlett.test(list(all_data$urn[all_data$condition=="no_res"],
                   all_data$urn[all_data$condition=="external"],
                   all_data$urn[all_data$condition=="self_res"]))

bartlett.test(list(all_data$vaccination[all_data$condition=="no_res"],
                   all_data$vaccination[all_data$condition=="external"],
                   all_data$vaccination[all_data$condition=="self_res"]))

# equivalency testing
# using cohen's d
alpha <- 0.025
cohen <- 0.2
library(TOSTER)
tost1_urn <- TOSTtwo(m1=mean(all_data$urn_error[all_data$condition=="external"]), 
                     m2=mean(all_data$urn_error[all_data$condition=="no_res"]), 
                     sd1=sd(all_data$urn_error[all_data$condition=="external"]), 
                     sd2=sd(all_data$urn_error[all_data$condition=="no_res"]), 
                     n1=length(all_data$urn_error[all_data$condition=="external"]), 
                     n2=length(all_data$urn_error[all_data$condition=="no_res"]), 
                     low_eqbound_d=-cohen, high_eqbound_d=cohen, alpha = alpha,var.equal = FALSE)

tost1_vacc <- TOSTtwo(m1=mean(all_data$vacc_error[all_data$condition=="external"]), 
                     m2=mean(all_data$vacc_error[all_data$condition=="no_res"]), 
                     sd1=sd(all_data$vacc_error[all_data$condition=="external"]), 
                     sd2=sd(all_data$vacc_error[all_data$condition=="no_res"]), 
                     n1=length(all_data$vacc_error[all_data$condition=="external"]), 
                     n2=length(all_data$vacc_error[all_data$condition=="no_res"]), 
                     low_eqbound_d=-cohen, high_eqbound_d=cohen, alpha = alpha,var.equal = FALSE)

tost2_urn <- TOSTtwo(m1=mean(all_data$urn_error[all_data$condition=="self_res"]), 
                     m2=mean(all_data$urn_error[all_data$condition=="no_res"]), 
                     sd1=sd(all_data$urn_error[all_data$condition=="self_res"]), 
                     sd2=sd(all_data$urn_error[all_data$condition=="no_res"]), 
                     n1=length(all_data$urn_error[all_data$condition=="self_res"]), 
                     n2=length(all_data$urn_error[all_data$condition=="no_res"]), 
                     low_eqbound_d=-cohen, high_eqbound_d=cohen, alpha = alpha,var.equal = FALSE)

tost2_vacc <- TOSTtwo(m1=mean(all_data$vacc_error[all_data$condition=="self_res"]), 
                     m2=mean(all_data$vacc_error[all_data$condition=="no_res"]), 
                     sd1=sd(all_data$vacc_error[all_data$condition=="self_res"]), 
                     sd2=sd(all_data$vacc_error[all_data$condition=="no_res"]), 
                     n1=length(all_data$vacc_error[all_data$condition=="self_res"]), 
                     n2=length(all_data$vacc_error[all_data$condition=="no_res"]), 
                     low_eqbound_d=-cohen, high_eqbound_d=cohen, alpha = alpha,var.equal = FALSE)

adj_ps_urn <- p.adjust(c(tost1_urn$TOST_p1,tost1_urn$TOST_p2,
                         tost2_urn$TOST_p1,tost2_urn$TOST_p2),
                       method="holm")

print(paste("No resolution vs. External resolution:", adj_ps_urn[1], "and", adj_ps_urn[2]))
print(paste("No resolution vs. Self-resolution:", adj_ps_urn[3], "and", adj_ps_urn[4]))

adj_ps_vacc <- p.adjust(c(tost1_vacc$TOST_p1,tost1_vacc$TOST_p2,
                          tost2_vacc$TOST_p1,tost2_vacc$TOST_p2),
                   method="holm")

print(paste("No resolution vs. External resolution:", adj_ps_vacc[1], "and", adj_ps_vacc[2]))
print(paste("No resolution vs. Self-resolution:", adj_ps_vacc[3], "and", adj_ps_vacc[4]))

# using raw scales
bound <- 5
tost1_urn <- TOSTtwo.raw(m1=mean(all_data$urn_error[all_data$condition=="external"]), 
                     m2=mean(all_data$urn_error[all_data$condition=="no_res"]), 
                     sd1=sd(all_data$urn_error[all_data$condition=="external"]), 
                     sd2=sd(all_data$urn_error[all_data$condition=="no_res"]), 
                     n1=length(all_data$urn_error[all_data$condition=="external"]), 
                     n2=length(all_data$urn_error[all_data$condition=="no_res"]), 
                     low_eqbound=-bound, high_eqbound=bound, alpha = 0.025,var.equal = FALSE)

tost2_urn <- TOSTtwo.raw(m1=mean(all_data$urn_error[all_data$condition=="self_res"]), 
                         m2=mean(all_data$urn_error[all_data$condition=="no_res"]), 
                         sd1=sd(all_data$urn_error[all_data$condition=="self_res"]), 
                         sd2=sd(all_data$urn_error[all_data$condition=="no_res"]), 
                         n1=length(all_data$urn_error[all_data$condition=="self_res"]), 
                         n2=length(all_data$urn_error[all_data$condition=="no_res"]), 
                         low_eqbound=-bound, high_eqbound=bound, alpha = 0.025,var.equal = FALSE)

tost1_vacc <- TOSTtwo.raw(m1=mean(all_data$vacc_error[all_data$condition=="external"]), 
                      m2=mean(all_data$vacc_error[all_data$condition=="no_res"]), 
                      sd1=sd(all_data$vacc_error[all_data$condition=="external"]), 
                      sd2=sd(all_data$vacc_error[all_data$condition=="no_res"]), 
                      n1=length(all_data$vacc_error[all_data$condition=="external"]), 
                      n2=length(all_data$vacc_error[all_data$condition=="no_res"]), 
                      low_eqbound=-bound, high_eqbound=bound, alpha = 0.025,var.equal = FALSE)

tost2_vacc <- TOSTtwo.raw(m1=mean(all_data$vacc_error[all_data$condition=="self_res"]), 
                      m2=mean(all_data$vacc_error[all_data$condition=="no_res"]), 
                      sd1=sd(all_data$vacc_error[all_data$condition=="self_res"]), 
                      sd2=sd(all_data$vacc_error[all_data$condition=="no_res"]), 
                      n1=length(all_data$vacc_error[all_data$condition=="self_res"]), 
                      n2=length(all_data$vacc_error[all_data$condition=="no_res"]), 
                      low_eqbound=-bound, high_eqbound=bound, alpha = 0.025,var.equal = FALSE)

p_urn <- c(tost1_urn$TOST_p1,tost1_urn$TOST_p2,
           tost2_urn$TOST_p1,tost2_urn$TOST_p2)
adj_ps_urn <- p.adjust(p_urn, method="holm")
round(p_urn,5)
round(adj_ps_urn,5)

print(paste("No resolution vs. External resolution:", round(adj_ps_urn[1],5), "and", round(adj_ps_urn[2],5)))
print(paste("No resolution vs. Self-resolution:", round(adj_ps_urn[3],5), "and", round(adj_ps_urn[4],5)))

p_vacc <- c(tost1_vacc$TOST_p1,tost1_vacc$TOST_p2,
            tost2_vacc$TOST_p1,tost2_vacc$TOST_p2)
adj_ps_vacc <- p.adjust(p_vacc, method="holm")
round(p_vacc,5)
round(adj_ps_vacc,5)

print(paste("No resolution vs. External resolution:", round(adj_ps_vacc[1],5), "and", round(adj_ps_vacc[2],5)))
print(paste("No resolution vs. Self-resolution:", round(adj_ps_vacc[3],5), "and", round(adj_ps_vacc[4],5)))

# ggplots
plot_tosts <- data.frame("task" = c("urn","urn","vaccine","vaccine"),
                         "condition" = c("external","self-resolution","external","self-resolution"),
                         "difference" = c(tost1_urn$diff,
                                          tost2_urn$diff,
                                          tost1_vacc$diff,
                                          tost2_vacc$diff),
                         "upr" = c(tost1_urn$UL_CI_TOST,
                                   tost2_urn$UL_CI_TOST,
                                   tost1_vacc$UL_CI_TOST,
                                   tost2_vacc$UL_CI_TOST),
                         "lwr" = c(tost1_urn$LL_CI_TOST,
                                   tost2_urn$LL_CI_TOST,
                                   tost1_vacc$LL_CI_TOST,
                                   tost2_vacc$LL_CI_TOST))

ggplot(plot_tosts[1:2,], aes(x=condition, y=difference, group=condition, color=condition)) + 
  geom_pointrange(aes(ymin=lwr, ymax=upr)) + # green
  geom_hline(yintercept=0, linetype="11", color = "grey") +
  geom_hline(yintercept=-bound, linetype="11", color = "black") +
  geom_hline(yintercept=bound, linetype="11", color = "black") +
  xlab("Condition") +
  ylab("Mean difference in error (percentage points)") +
  ggtitle("Estimation task: Urn") +
  labs(subtitle = "Equivalency tests with equivalency bounds of +/-5 percentage points") +
  theme(plot.title = element_text(face = "bold")) +
  coord_flip() +
  theme_minimal() +
  theme(legend.position = "none") +
  theme(panel.grid.major = element_line(linetype = "dashed")) +
  theme(panel.grid.minor = element_line(linetype = "dashed")) +
  theme(plot.title = element_text(face = "bold"))

ggplot(plot_tosts[3:4,], aes(x=condition, y=difference, group=condition, color=condition)) + 
  geom_pointrange(aes(ymin=lwr, ymax=upr)) + # green
  geom_hline(yintercept=0, linetype="11", color = "grey") +
  geom_hline(yintercept=-bound, linetype="11", color = "black") +
  geom_hline(yintercept=bound, linetype="11", color = "black") +
  xlab("Condition") +
  ylab("Mean difference in error (percentage points)") +
  ggtitle("Estimation task: Vaccination") +
  labs(subtitle = "Equivalency tests with equivalency bounds of +/-5 percentage points") +
  theme(plot.title = element_text(face = "bold")) +
  coord_flip() +
  theme_minimal() +
  theme(legend.position = "none") +
  theme(panel.grid.major = element_line(linetype = "dashed")) +
  theme(panel.grid.minor = element_line(linetype = "dashed")) +
  theme(plot.title = element_text(face = "bold"))

write_csv(all_data, "data/all_data.csv")
