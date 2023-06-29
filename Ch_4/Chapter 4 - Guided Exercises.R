# Chapter 4 - Guided Exercises

# Exer - 1 - part c

# make data frame of possible values

possible_robs = 0:50
dens_robs = dpois(x = possible_robs, lambda = 5)
robs.model = tibble('possible_robs' = possible_robs, 'rob_prob' = dens_robs)

#plot the values

robs_model = ggplot(data = robs.model, mapping = aes(x = possible_robs, y = rob_prob)) + geom_bar(stat = 'identity') +
  labs(x = 'Number of Burglaries', y = 'Probability') + ggtitle('Poisson Model of Burglaries') + coord_cartesian(ylim = c(0, 0.5))

robs_model

# verify mean and variance are equal

verify_pois = rpois(n = 40, lambda = 5)
verify_pois

mean(verify_pois)
var(verify_pois)


# part d

# make data frame of possible values

possible_robs_2 = 0:50
dens_robs_2 = dpois(x = possible_robs_2, lambda = 20)
robs.model_2 = tibble('possible_robs' = possible_robs_2, 'rob_prob' = dens_robs_2)

#plot the values

robs_model = ggplot(data = robs.model_2, mapping = aes(x = possible_robs, y = rob_prob)) + geom_bar(stat = 'identity') +
  labs(x = 'Number of Burglaries', y = 'Probability') + ggtitle('Poisson Model of Burglaries') + coord_cartesian(ylim = c(0, 0.5))

robs_model

#alternative way to plot without using feom_bar(stat = 'identity')
robs_model_beta = ggplot(data = robs.model_2, mapping = aes(x = possible_robs, y = rob_prob)) + geom_col() +
  labs(x = 'Number of Burglaries', y = 'Probability') + ggtitle('Poisson Model of Burglaries') + coord_cartesian(ylim = c(0, 0.5))

robs_model_beta


# verify mean and variance are equal

verify_pois_2 = rpois(n = 100, lambda = 20)
verify_pois_2

mean(verify_pois_2)
var(verify_pois_2)



# Exer 2

elephants = read_csv(file = "C:/Users/Saint/Documents/School_2017_onward/2021-2022/STA303/BMLR_Review/Ch_4/elephant.csv")

# part a

elephants %>% ggplot(mapping = aes(x = MATINGS)) + geom_bar(fill = "blue") + labs(x = 'number of matings', y = 'counts')
summary_matings = elephants %>% summarise(mean_matings = mean(MATINGS), var_matings = var(MATINGS))

# part b

elephants %>% ggplot(mapping = aes(x = AGE, y = MATINGS)) + geom_point() + geom_smooth(method = "loess", size = 1.5) + 
  labs(x = "Age", y = "Number of Matings")

# part c

mean_mate_by_age = elephants %>% group_by(AGE) %>%  summarize(mean = mean(MATINGS), var = var(MATINGS), 
                                                                                log_matings = log(mean))

log_mean_mate_plot = mean_mate_by_age %>% ggplot(mapping = aes(x = AGE, y = log_matings)) + geom_point() + 
  geom_smooth(method = 'loess', linewidth = 1.5) + labs(x = "Age", y = "Number of matings_Logged")

log_mean_mate_plot


# Attempt at using stat_summary()
# log_mean_age_func = function(x){
#   tibble(y = log(mean(x)))
# }
# 
# elephants %>% ggplot(mapping = aes(x = AGE, y = MATINGS)) + geom_point() + 
#   stat_summary(fun.data = log_mean_age_func, geom = 'smooth', fun.args = list(method = 'loess'))

# part d

regress_elep_1 = glm(data = elephants, formula = MATINGS ~ AGE, family = poisson)
summary(regress_elep_1)

exp(coef(regress_elep_1))

# part e

exp(confint(regress_elep_1))

# part f

library(aod) #to be able to perform wald test outside of summary()

wald.test(Sigma = vcov(regress_elep_1), b = coef(regress_elep_1), Terms = 2)

null_model_elep = glm(data = elephants, formula = MATINGS ~ 1, family = poisson)
drop_in_dev_elep = anova(null_model_elep, regress_elep_1, test = 'Chisq')
drop_in_dev_elep

# part g
elephants = elephants %>% mutate(AGE2 = AGE^2) #adding quadratic term to df

regress_elep_2 = glm(data = elephants, formula = MATINGS ~ AGE + AGE2, family = poisson)
summary(regress_elep_2)

drop_in_dev_elep2 = anova(regress_elep_1, regress_elep_2, test = 'Chisq')
drop_in_dev_elep2

# part h

1 - pchisq(q = regress_elep_1$deviance, df = regress_elep_1$df.residual) #GOF test

counts_by_AGE = elephants %>%  group_by(AGE) %>%  summarise(n = n())


# part i

regress_elep_od = glm(data = elephants, formula = MATINGS ~ AGE, family = quasipoisson)
summary(regress_elep_od)

coef(regress_elep_1)
coef(regress_elep_od)

drop_in_dev_od = anova(regress_elep_od, regress_elep_1, test = 'F')
drop_in_dev_od