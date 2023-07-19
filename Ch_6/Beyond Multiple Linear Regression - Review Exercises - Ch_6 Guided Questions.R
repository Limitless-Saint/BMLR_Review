# Beyond Multiple Linear Regression - Review Exercises - Ch.6 - Logistic Regression - Guided Questions

experimental_palette = brewer.pal(n = 11, name = "Spectral")
experimental_palette


# Exercise 1

goals_table = tibble("Status" = c("Behind", "Tied", "Ahead"), "Saved" = c(20,19,2), "Scored" = c(55, 71, 18)) %>%
  mutate("Shots" = Saved + Scored, "Prop_Scored" = Scored/Shots) #Created table of data

# part a

# i)

goals_table = goals_table %>%  mutate("Odds_Scored" = Prop_Scored/(1-Prop_Scored))
goals_table

#ii # Created an odds_ratio table for practice with programming observe use of subsetting and vectorized functions

odds_ratios = list()

for (i in 1:3){
  vec = vector(mode = "double", length = 3) #preallocate a vector to put ratios into
  for(j in 1:3){
  vec[[j]] = goals_table[[j, 6]]/goals_table[[i,6]]
  }
  odds_ratios[[i]] = vec
}

df = data.frame(matrix(data = NA, nrow = 3, ncol = 3))

for(i in 1:3){
  for(j in 1:3){
    df[[i,j]] = odds_ratios[[i]][[j]]
  }
}

colnames(df) = c("Behind", "Tied", "Ahead")
rownames(df) = colnames(df)

# Using a vectorized function as alternative
df_2 = do.call(what = "rbind", odds_ratios)
df_2


# part b)

scored = glm(data = goals_table, formula = cbind(Scored, Shots - Scored) ~ Status, family = binomial)
summary(scored)

exp(coef(scored))



# Exercise 4

birdkeeping = read_csv(file = "C:/Users/Saint/Documents/School_2017_onward/2021-2022/STA303/BMLR_Review/Ch_6/birdkeeping.csv")

# converted qualitative variables into variables with names to make for better understanding when conducting study, also
# turned years smoking into a factor to make for simpler analysis.

birdkeeping = birdkeeping %>% mutate(sex = ifelse(female == 1, "Female", "Male"),
                                     socioecon_status = ifelse(highstatus == 1,
                                                               "High", "Low"),
                                     keep_bird = ifelse(bird == 1, "Keep Bird", "No Bird"),
                                     lung_cancer = ifelse(cancer == 1, "Cancer",
                                                          "No_Cancer")) %>%
  mutate(years_factor = cut(yrsmoke,
                            breaks = c(-Inf, 0, 25, Inf),
                            labels = c("No smoking", "1-25 years",
                                       "Over 25 years")))


# Needed to convert qualitative variables needed to convert to factors

cols = c(2,4,7:13)

birdkeeping[cols] = map(birdkeeping[cols], as_factor)



#alt version to setting factors, uses up to date Tidyverse syntax

birdkeeping = birdkeeping %>%  mutate(across(all_of(cols), as_factor))

# part a)

# each variable is explored against the response variable

# quantitative variables
cd_plot_age = cdplot(formula = lung_cancer ~ age, data = birdkeeping)
cd_plot_age

boxplot_age = birdkeeping %>% ggplot(mapping = aes(x = lung_cancer, y = age)) + geom_boxplot()
boxplot_age

mean_age = birdkeeping  %>% summarise(mean_age = mean(age))


cd_plot_yrs_smoke = cdplot(formula = lung_cancer ~ yrsmoke, data = birdkeeping)
cd_plot_yrs_smoke

boxplot_yrs_smoke = birdkeeping %>% ggplot(mapping = aes(x = lung_cancer, y = yrsmoke)) + geom_boxplot()
boxplot_yrs_smoke

summary_stats_yrs_smoke = birdkeeping %>% group_by(lung_cancer) %>% summarise(mean = mean(yrsmoke), var = var(yrsmoke), sd = sd(yrsmoke))


cd_plot_cigsday = cdplot(formula = lung_cancer ~ cigsday, data = birdkeeping)
cd_plot_cigsday

boxplot_cigsday = birdkeeping %>% ggplot(mapping = aes(x = lung_cancer, y = cigsday)) + geom_boxplot()
boxplot_cigsday

summary_stats_cigsday = birdkeeping %>% group_by(lung_cancer) %>% summarise(mean = mean(cigsday), var = var(cigsday), sd = sd(cigsday))


# qualitative variables

sex_bar_chart = birdkeeping %>% ggplot(mapping = aes(x = sex, fill = lung_cancer)) +
  geom_bar() + scale_fill_brewer(palette = "Spectral")
sex_bar_chart

sex_cross_tab = birdkeeping %>% group_by(sex, lung_cancer) %>%
  tally() %>% pivot_wider(id_cols = sex, names_from = lung_cancer, values_from =  n) %>%
  mutate(total_cases = sum(Cancer + No_Cancer), prop_cancer = Cancer/total_cases, prop_no_cancer = No_Cancer/total_cases)
sex_cross_tab




socio_bar_chart = birdkeeping %>% ggplot(mapping = aes(x = socioecon_status, fill = lung_cancer)) +
  geom_bar()
socio_bar_chart

socio_cross_tab = birdkeeping %>% group_by(socioecon_status, lung_cancer) %>%
  tally() %>% pivot_wider(id_cols = socioecon_status, names_from = lung_cancer, values_from =  n) %>%
  mutate(total_cases = sum(Cancer + No_Cancer), prop_cancer = Cancer/total_cases, prop_no_cancer = No_Cancer/total_cases)
socio_cross_tab



keep_bird_bar_chart = birdkeeping %>% ggplot(mapping = aes(x = keep_bird, fill = lung_cancer)) +
  geom_bar()
keep_bird_bar_chart

keep_bird_cross_tab = birdkeeping %>% group_by(keep_bird, lung_cancer) %>%
  tally() %>% pivot_wider(id_cols = keep_bird, names_from = lung_cancer, values_from =  n) %>%
  mutate(total_cases = sum(Cancer + No_Cancer), prop_cancer = Cancer/total_cases, prop_no_cancer = No_Cancer/total_cases)
keep_bird_cross_tab

# part c

keep_bird_cross_tab = keep_bird_cross_tab %>% mutate(odds = prop_cancer/prop_no_cancer)

keep_birds_odds_ratio =  keep_bird_cross_tab[[1,7]]/keep_bird_cross_tab[[2,7]]
keep_birds_odds_ratio

keep_birds_relative_risk = keep_bird_cross_tab[[1,5]]/keep_bird_cross_tab[[2,5]]
keep_birds_relative_risk

# part d

#added odds to socio cross tab

socio_cross_tab = socio_cross_tab %>%  mutate(odds = prop_cancer/prop_no_cancer)

#Create cross table to calculate empirical logits

yrs_smoked_factor_cross_tab = birdkeeping %>%  group_by(years_factor, lung_cancer) %>% tally() %>%
  pivot_wider(id_cols = years_factor, names_from = lung_cancer, values_from = n) %>%
  mutate(total_cases = sum(Cancer + No_Cancer), prop_cancer = Cancer/total_cases, prop_no_cancer = No_Cancer/total_cases)

yrs_smoked_factor_cross_tab = yrs_smoked_factor_cross_tab %>% mutate(odds = prop_cancer/prop_no_cancer, emp_log = log(odds))

# created elogits
elogit_yrs_smoked_plot = yrs_smoked_factor_cross_tab %>% ggplot(mapping = aes(x = years_factor, y = emp_log)) +
  geom_point() + geom_smooth(method = lm, se = FALSE) + labs(x = "Yrs_Smoked", y = "Emp_Logit")
elogit_yrs_smoked_plot


#alternative version of above but used yrssmoke instead of the years_factor version to be able to apply geom_smooth properly
# needed to be continuous variable.

yrs_smoked_cross_tab_non_factor = birdkeeping %>%  group_by(yrsmoke, lung_cancer) %>% tally() %>%
  pivot_wider(id_cols = yrsmoke, names_from = lung_cancer, values_from = n)

  yrs_smoked_cross_tab_non_factor$Cancer[is.na(yrs_smoked_cross_tab_non_factor$Cancer) == TRUE] = 0
  yrs_smoked_cross_tab_non_factor$No_Cancer[is.na(yrs_smoked_cross_tab_non_factor$No_Cancer) == TRUE] = 0

  yrs_smoked_cross_tab_non_factor = yrs_smoked_cross_tab_non_factor %>%  mutate(total_cases = sum(Cancer, No_Cancer))

  yrs_smoked_cross_tab_non_factor =  yrs_smoked_cross_tab_non_factor %>%
    mutate(prop_cancer = (Cancer + 0.5)/(total_cases+1), prop_no_cancer = (No_Cancer + 0.5)/(total_cases + 1)) # adjusted to have no log(0) values

  yrs_smoked_cross_tab_non_factor = yrs_smoked_cross_tab_non_factor %>%
    mutate(odds = prop_cancer/prop_no_cancer, emp_log = log(odds))

  elogit_yrs_smoked_plot_non_factor = yrs_smoked_cross_tab_non_factor %>% ggplot(mapping = aes(x = yrsmoke, y = emp_log)) +
    geom_point() + geom_smooth(method = lm, se = FALSE) + labs(x = "Yrs_Smoked", y = "Emp_Logit")
  elogit_yrs_smoked_plot_non_factor


# part e)

 interaction_plot_yrsmoke_keep = birdkeeping %>% ggplot(mapping = aes(x = yrsmoke, y = lung_cancer, color = keep_bird)) +
   geom_point()
 interaction_plot_yrsmoke_keep


 yrs_smoked_cross_interaction = birdkeeping %>%  group_by(yrsmoke, keep_bird) %>% tally() %>%
   pivot_wider(id_cols = yrsmoke, names_from = keep_bird, values_from = n)

  yrs_smoked_cross_interaction =  rename(yrs_smoked_cross_interaction, keep_bird = "Keep Bird") #had to add underscore to make column names workable
  yrs_smoked_cross_interaction = rename(yrs_smoked_cross_interaction, no_bird = "No Bird")

 yrs_smoked_cross_interaction$keep_bird[is.na(yrs_smoked_cross_interaction$keep_bird) == TRUE] = 0
 yrs_smoked_cross_interaction$no_bird[is.na(yrs_smoked_cross_interaction$no_bird) == TRUE] = 0

 yrs_smoked_cross_interaction = yrs_smoked_cross_interaction %>%  mutate(total_cases = sum(keep_bird, no_bird))

 yrs_smoked_cross_interaction =  yrs_smoked_cross_interaction %>%
   mutate(prop_bird = (keep_bird + 0.5)/(total_cases+1), prop_no_bird = (no_bird + 0.5)/(total_cases + 1)) # adjusted to have no log(0) values

 yrs_smoked_cross_interaction = yrs_smoked_cross_interaction %>%
   mutate(odds = prop_bird/prop_no_bird, emp_log = log(odds))

 elogit_yrs_smoked_cross_interaction = yrs_smoked_cross_interaction %>% ggplot(mapping = aes(x = yrsmoke, y = emp_log)) +
   geom_point() + geom_smooth(method = lm, se = FALSE) + labs(x = "Yrs_Smoked", y = "Emp_Logit")
 elogit_yrs_smoked_cross_interaction



 yrs_smoked_interaction_alt = birdkeeping %>%  group_by(yrsmoke, keep_bird, lung_cancer) %>% tally() %>%
   pivot_wider(id_cols = c(yrsmoke, keep_bird), names_from = lung_cancer, values_from = n)

 yrs_smoked_interaction_alt$Cancer[is.na(yrs_smoked_interaction_alt$Cancer) == TRUE] = 0
 yrs_smoked_interaction_alt$No_Cancer[is.na(yrs_smoked_interaction_alt$No_Cancer) == TRUE] = 0

 yrs_smoked_interaction_alt = yrs_smoked_interaction_alt %>%  mutate(total_cases = sum(Cancer, No_Cancer))

 yrs_smoked_interaction_alt =  yrs_smoked_interaction_alt %>%
   mutate(prop_cancer = (Cancer + 0.5)/(total_cases+1), prop_no_cancer = (No_Cancer + 0.5)/(total_cases + 1)) # adjusted to have no log(0) values

 yrs_smoked_interaction_alt = yrs_smoked_interaction_alt %>%
   mutate(odds = prop_cancer/prop_no_cancer, emp_log = log(odds))

 elogit_yrs_smoked_interaction_alt = yrs_smoked_interaction_alt %>% ggplot(mapping = aes(x = yrsmoke, y = emp_log, colour = keep_bird)) +
   geom_point() + geom_smooth(method = lm, se = FALSE) + labs(x = "Yrs_Smoked", y = "Emp_Logit")
 elogit_yrs_smoked_interaction_alt


 # part f

 birdkeeping$lung_cancer =  fct_relevel(birdkeeping$lung_cancer, c("No_Cancer", "Cancer")) # had to relevel factors because "cancer" was the
 #first level and in running glm, the first level is seen as "failures"
 birdkeeping$keep_bird = fct_relevel(birdkeeping$keep_bird, c("No Bird", "Keep Bird"))

 model1 = glm(data = birdkeeping, formula = lung_cancer ~ age + yrsmoke + cigsday + sex + socioecon_status + keep_bird, family = binomial)
 model2 = glm(data = birdkeeping, formula = lung_cancer ~ yrsmoke + cigsday + socioecon_status + keep_bird, family = binomial)
 model4 = glm(data = birdkeeping, formula = lung_cancer ~ yrsmoke + keep_bird, family = binomial)

 birdkeeping = birdkeeping %>% mutate(yrsmoke_sq = yrsmoke^2)# squared term for factor makes no sense

 model5 = glm(data = birdkeeping, formula = lung_cancer ~ yrsmoke + keep_bird + yrsmoke:keep_bird + yrsmoke_sq, family = binomial)
 model6 = glm(data = birdkeeping, formula = lung_cancer ~ yrsmoke + keep_bird + yrsmoke:keep_bird, family = binomial)

summary(model1)
summary(model2)

drop_in_deviance_1_2 = anova(model2, model1, test = "Chisq")
drop_in_deviance_1_2


# part g

drop_in_deviance_4_5 = anova(model4, model5, test = "Chisq")
drop_in_deviance_4_5


#part h)

summary(model6)
exp(coef(model6))


#part i)

birdkeeping = birdkeeping %>% mutate(mean_centr_yrsmoke = yrsmoke - mean(yrsmoke))

model6alt = glm(data = birdkeeping, formula = lung_cancer ~ mean_centr_yrsmoke + keep_bird + mean_centr_yrsmoke:keep_bird,
                family = binomial)
summary(model6alt)
exp(coef(model6alt))

# part j)

exp(confint(model4))

# part k
summary(model4)
exp(coef(model4))

# part l)


model4a = glm(data = birdkeeping, formula = lung_cancer ~ years_factor + keep_bird, family = binomial)
summary(model4a)
exp(coef(model4a))

summary(model4)


# part m)

