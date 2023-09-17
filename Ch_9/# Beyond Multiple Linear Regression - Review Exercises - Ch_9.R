# Beyond Multiple Linear Regression - Review Exercises - Ch.9 - Two Level Longitudinal Data



library(GGally)
library(data.table)
library(Hmisc)
library(mice)
library(lattice)
library(nlme)
library(reshape2)
library(MASS)
library(mnormt)
library(lme4)
library(gridExtra)
library(knitr)
library(kableExtra)
library(broom)
library(tidyverse)
library(car)
library(tidyr)


set.seed(10)

chart.wide = read_csv(file = "C:/Users/Saint/Documents/School_2017_onward/2021-2022/STA303/BMLR_Review/Ch_9/chart_wide_condense.csv")

table2chp9 <- md.pattern(x = chart.wide[c(5,9,10,11)], plot=FALSE) #table to analyze the missing data points

chart.long = pivot_longer(data = chart.wide, cols = 9:11, names_to = "year08" ,values_to = "MathAvgScr")
chart.long = separate_wider_delim(data = chart.long, col = year08, delim = regex(pattern = "\\."), names = c("name", "year08"))
chart.long = chart.long %>% select(2:11)
chart.long = chart.long %>% select(-name)
chart.long = chart.long %>% arrange(schoolid, year08) %>% mutate(year08 = as.numeric(year08))


# Exploratory Data Analysis

theme.1 <- theme(axis.title.x = element_text(size = 14),
                 axis.title.y = element_text(size = 14),
                 plot.title=element_text(hjust=.9,face="italic",size=12))

chart.means = chart.long %>% group_by(schoolid) %>%  summarise(mean3yr = mean(MathAvgScr, na.rm = TRUE))

chart.wide <- chart.wide %>%
  mutate(urban0 = ifelse(urban==1, "urban", "rural"),
         charter0 = ifelse(charter==1, "charter",
                           "public non-charter")) %>%
  left_join(chart.means, by="schoolid")

chart.wide %>% ggplot(mapping = aes(x = mean3yr)) + geom_histogram(binwidth = 5, color = "black", fill = "white") + theme.1 +
  xlab("Mean Math Scores by School") + ylab("Frequency")


# Conceptual Exercises

#part 9

# Did some data exploration to see how to categrize variable
chart.wide %>% filter(schPctnonw > 0.75) %>% nrow()
chart.wide %>% filter(schPctnonw <= 0.75 & schPctnonw > 0.5) %>% nrow()
chart.wide %>% filter(schPctnonw <= 0.5 & schPctnonw > 0.25) %>% nrow()
chart.wide %>% filter(schPctnonw <= 0.25)  %>% nrow()

chart.wide %>% ggplot(mapping = aes(x = schPctnonw)) + geom_histogram(bins = 6)
chart.wide %>% ggplot(mapping = aes(x = schPctsped)) + geom_histogram(bins = 4)

medpctonw = median(chart.wide$schPctnonw)

chart.wide = chart.wide %>%
  mutate(highpctnonw = ifelse(test = schPctnonw > medpctonw, yes = "High Pct NonW", no = "Low Pct NonW" ))

chart.wide %>%  ggplot(mapping = aes(x = schPctsped, y = highpctnonw)) + geom_boxplot() + theme.1 + xlab("Percent Special Ed") +
  ylab("Percent NonW")

# Guided Exercises

# Question 2

alcohol = read_csv(file = "C:/Users/Saint/Documents/School_2017_onward/2021-2022/STA303/BMLR_Review/Ch_9/alcohol.csv")


alcohol = alcohol %>% select(2:7) #removed first column
alcohol = alcohol %>% mutate(ch_alc = ifelse(test = coa == 1, yes = 'Yes', no = "No"),
                             sex = ifelse(test = male == 1, yes = "Male", no = "Female")) #turned to understandable factors

# part b

table(alcohol$alcuse) #getting idea of how many data points are in each "box" to split histogram better

alcuse_hist1 = alcohol %>% ggplot(mapping = aes(x = alcuse, color = "red")) + geom_histogram(bins = 6)
alcuse_hist2 = alcohol %>% ggplot(mapping = aes(x = alcuse, color = "red")) + geom_histogram(bins = 7)

alcuse_hist1
alcuse_hist2

overall_mean_alc_peer = mean(alcohol$peer)
overall_mean_alcuse = mean(alcohol$alcuse)

individ_alc_use_means = alcohol %>% group_by(id) %>%  dplyr::summarize(mean_alc_3yr_use = mean(alcuse))
individ_alc_peer_means = alcohol %>% group_by(id) %>%  dplyr::summarize(mean_peer_3yr_use = mean(peer))
alcohol_indv_means = individ_alc_use_means %>% left_join(y = individ_alc_peer_means, by = 'id') # to merge with alc_means for plotting

alcuse_means_hist = alcohol_indv_means %>% ggplot(mapping = aes(x = mean_alc_3yr_use, color = 'red')) + geom_histogram(bins = 6)
alcuse_means_hist

alcuse_ch_alc_box = alcohol %>% ggplot(mapping = aes(x = ch_alc, y = alcuse)) + geom_boxplot() + theme.1 +
  xlab("Child of Alc") + ylab("Alcohol Use") + coord_flip()
alcuse_ch_alc_box

alcuse_sex_box = alcohol %>% ggplot(mapping = aes(x = sex, y = alcuse)) + geom_boxplot() + theme.1 +
  xlab("Sex") + ylab("Alcohol Use") + coord_flip()
alcuse_sex_box

alcuse_peer_scat = alcohol_indv_means %>%  ggplot(mapping = aes(x = mean_peer_3yr_use, y = mean_alc_3yr_use)) + geom_point(color = "red") +
  theme.1 + geom_smooth(se = FALSE, method = 'lm') + xlab("Mean Peer 3yr Use") + ylab("Mean Alc 3yr Use")
alcuse_peer_scat

# part c

alcohol %>% ggplot(mapping = aes(x = age, y = alcuse)) + geom_point() + geom_line() + facet_wrap(facets = vars(id)) +
  scale_x_continuous(limits = c(14,16), breaks = c(14,15,16)) + theme.1 + labs(x = "Age", y = "Alc Use")

# part d

spaghetti_coa = alcohol %>% ggplot(mapping = aes(x = age, y = alcuse)) + geom_line(mapping = aes(group = id)) +
  geom_smooth(mapping = aes(group = 1), method = 'lm', se = FALSE) + facet_wrap(facets = vars(ch_alc))
spaghetti_coa

spaghetti_sex = alcohol %>% ggplot(mapping = aes(x = age, y = alcuse)) + geom_line(mapping = aes(group = id)) +
  geom_smooth(mapping = aes(group = 1), method = 'lm', se = FALSE) + facet_wrap(facets = vars(sex))
spaghetti_sex

alcohol %>% ggplot(mapping = aes(x = peer), color = 'red') + geom_histogram(bins = 10) # used histogram to get idea how to split into a binary, fiddled w/ number of bins

#the median and mode were also used to find how to split peer
median(alcohol$peer)
find_mode <- function(x) {
  u <- unique(x)
  tab <- tabulate(match(x, u))
  u[tab == max(tab)]
}

find_mode(alcohol$peer)

alcohol = alcohol %>%  mutate(above_mode = ifelse(test = peer > 0.90, yes = 'above', no = 'below'))

spaghetti_peer = alcohol %>% ggplot(mapping = aes(x = age, y = alcuse)) + geom_line(mapping = aes(group = id)) +
  geom_smooth(mapping = aes(group = 1), method = 'lm', se = FALSE) + facet_wrap(facets = vars(above_mode))
spaghetti_peer

# part e) come back to finish with more tools

regressions = alcohol %>% nest_by(id) %>% mutate(fit = list(lm(alcuse ~ age, data = alcohol)))
regressions

lm_info1 = regressions %>% tidy(fit) %>%  ungroup() %>%  select(id, term, estimate)


#part f) finish in conjunction with part e


#part h)

model.A = lmer(formula = alcuse ~ 1 + (1|id), REML = TRUE, data = alcohol)
summary(model.A)

print(VarCorr(model.A), comp=c("Variance","Std.Dev"))

intrclass.coor.A = 0.57313/(0.57313+0.56175)
intrclass.coor.A


#part i)

alcohol = alcohol %>% mutate(age_relevel = ifelse(age == 14, yes = 0, no = ifelse(test = age == 15, yes = 1, no = 2))) #needed to relevel to have a base of 0

model.B = lmer(formula = alcuse ~ age_relevel + (age_relevel|id), REML = TRUE, data = alcohol)
summary(model.B)

pseudo_R_squared_a_b = (0.56175 - 0.3373)/0.56175
pseudo_R_squared_a_b


#part j)

model.C = lmer(formula = alcuse ~ age_relevel + coa + peer + coa:age_relevel +
                 peer:age_relevel + (age_relevel|id), REML = TRUE, data = alcohol)

summary(model.C)


#part k)

model.D = lmer(formula = alcuse ~ age_relevel + coa + peer +
                 peer:age_relevel + (age_relevel|id), REML = TRUE, data = alcohol)

summary(model.D)
