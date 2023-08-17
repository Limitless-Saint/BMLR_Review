# Beyond Multiple Linear Regression - Review Exercises - Ch_7

library(gridExtra)
library(knitr)
library(kableExtra)
library(lme4)
library(ICC)
library(knitr)
library(tidyverse)


#### one large table
tScenario <- c("1a", "", "1b", "", "",
               "Scenario","2a", "", "2b", "")

tModel <- c("Binomial", "Quasibinomial", "Binomial", "Quasibinomial", "",
            "Model", "Binomial", "Quasibinomial", "Binomial", "Quasibinomial")

#tModelName <- ifelse(knitr::is_html_output(),
#                     c("fit_1a_binom", "fit_1a_quasi", #"fit_1b_binom", "fit_1b_quasi", "", "Model Name",
#                       "fit_2a_binom","fit_2a_quasi", #"fit_2b_binom", "fit_2b_quasi"),
#                     c("fit\\_1a\\_binom", #"fit\\_1a\\_quasi","fit\\_1b\\_binom","fit\\_1b\\_quasi", "",
#                       "Model Name", #"fit\\_2a\\_binom","fit\\_2a\\_quasi", "fit\\_2b\\_binom", #"fit\\_2b\\_quasi"))

tModelName <- c("fit\\_1a\\_binom", "fit\\_1a\\_quasi", "fit\\_1b\\_binom", "fit\\_1b\\_quasi", " ",
                "Model Name", "fit\\_2a\\_binom","fit\\_2a\\_quasi", "fit\\_2b\\_binom", "fit\\_2b\\_quasi")

tBeta <- c("0.067","0.07","0.27","0.27","",
           "$\\hat{\\beta}_1$", "1.26","1.26","1.46","1.46")
tSEBeta <- c("0.13","0.12","0.13","0.34","",
             "SE $\\hat{\\beta}_1$", "0.16","0.18","0.18","0.25")
tTStat <- c("0.52","0.55","2.059","0.786","",
            "$t$", "7.725","6.866","8.11","5.84")
tPVal <- c("0.61","0.59","0.039","0.44","",
           "p value", "1.12e-14","6.77e-07","5.02e-16","7.1e-06")
tPhi <- c("1","0.89","1","6.86","",
          "$\\phi$", "1","1.27","1","1.93")
tEst <- c("0.52","0.52","0.57","0.57","",
          "Est odds ratio", "3.54","3.54","4.31","4.31")
tCI <- c("0.45 - 0.58","0.46 - 0.58","0.50 - 0.63","0.40 - 0.72","",
         "CI odds ratio", "2.61 - 4.96","2.51 - 5.19","3.09 - 6.27","2.74 - 7.35")
tMeanCount <- c("5.17","X","5.67","X","",
                "Mean count Dose=1", "3.17","X","3.50","X")
tSDCount <- c("1.49","X","4.10","X","",
              "SD count Dose=1", "1.835","X","2.881","X")
tGOFP <- c("","X","","X","",
           "GOF p value", "","X","","X")

scenarioSimTab <- tibble(tScenario, tModel, tModelName, tBeta, tSEBeta, tTStat, tPVal, tPhi, tEst, tCI, tMeanCount, tSDCount, tGOFP)
colnames(scenarioSimTab) <- c("Scenario", "Model", "Model Name", "$\\hat{\\beta}_0$", "SE $\\hat{\\beta}_0$", "$t$", "p value", "$\\phi$", "Est prob", "CI prob", "Mean count", "SD count", "GOF p value")

kable(scenarioSimTab, booktabs=T, caption="Summary of simulations for Dams and Pups case study.", escape=F) %>%
  kable_styling(latex_options = "scale_down", font_size = 9) %>%
  row_spec(6, bold=T) %>%
  column_spec(9:13, width = "5em")


set.seed(2)  # to get the same simulated results as reported here


pi_1a <- rep(0.5, 24)
count_1a <- rbinom(24, 10, pi_1a)

pi_1b <- rbeta(24,.5,.5)
count_1b <- rbinom(24, 10, pi_1b)


scenario_1 <- tibble(pi_1a, count_1a, pi_1b, count_1b) %>%
  mutate(phat_1a = count_1a / 10, phat_1b = count_1b / 10)

scenario_1 %>%
  summarise(mean_1a = mean(count_1a), sd_1a = sd(count_1a),
            mean_1b = mean(count_1b), sd_1b = sd(count_1b) )

fit_1a_binom <- glm(phat_1a ~ 1, family=binomial, weight=rep(10,24), data = scenario_1)
summary(fit_1a_binom)
# estimated odds of deformity
exp(coef(fit_1a_binom))
# estimated prob of deformity
exp(coef(fit_1a_binom)) / (1+exp(coef(fit_1a_binom)))

confint(fit_1a_binom)
exp(confint(fit_1a_binom))
exp(confint(fit_1a_binom)) / (1 + exp(confint(fit_1a_binom)))


fit_1a_quasi = glm(phat_1a ~ 1, family=quasibinomial, weight=rep(10,24), data=scenario_1)
summary(fit_1a_quasi)
# estimated odds of deformity
exp(coef(fit_1a_quasi))
# estimated prob of deformity
exp(coef(fit_1a_quasi)) / (1+exp(coef(fit_1a_quasi)))

confint(fit_1a_quasi)
exp(confint(fit_1a_quasi))
exp(confint(fit_1a_quasi)) / (1 + exp(confint(fit_1a_quasi)))


fit_1b_binom <- glm(phat_1b ~ 1, family=binomial, weight=rep(10,24), data = scenario_1)
summary(fit_1b_binom)
# estimated odds of deformity
exp(coef(fit_1b_binom))
# estimated prob of deformity
exp(coef(fit_1b_binom)) / (1+exp(coef(fit_1b_binom)))

confint(fit_1b_binom)
exp(confint(fit_1b_binom))
exp(confint(fit_1b_binom)) / (1 + exp(confint(fit_1b_binom)))


fit_1b_quasi = glm(phat_1b ~ 1, family=quasibinomial, weight=rep(10,24), data=scenario_1)
summary(fit_1b_quasi)
# estimated odds of deformity
exp(coef(fit_1b_quasi))
# estimated prob of deformity
exp(coef(fit_1b_quasi)) / (1+exp(coef(fit_1b_quasi)))

confint(fit_1b_quasi)
exp(confint(fit_1b_quasi))
exp(confint(fit_1b_quasi)) / (1 + exp(confint(fit_1b_quasi)))



x <- 0:3
p_2 <- exp(-2+4/3*x)/(1+exp(-2+4/3*x))
p_2

set.seed(1)
dose <- c(rep(0,6),rep(1,6),rep(2,6),rep(3,6))
pi_2a <- exp(-2+4/3*dose)/(1+exp(-2+4/3*dose))
count_2a <- rbinom(24, 10, pi_2a)

b <- 2
a <- b*pi_2a / (1-pi_2a)
pi_2b <- rbeta(24, a, b)
count_2b <- rbinom(24, 10, pi_2b)

scenario_2 <- tibble(dose, pi_2a, count_2a, pi_2b, count_2b)

scenario_2 %>%
  summarise(mean_2a = mean(count_2a), sd_2a = sd(count_2a),
            mean_2b = mean(count_2b), sd_2b = sd(count_2b) )

theoretical_pi <- tibble(x = 1:50000,
                         p1 = rbeta(x, shape1 = 2*p_2[1]/(1-p_2[1]), shape2 = 2),
                         p2 = rbeta(x, shape1 = 2*p_2[2]/(1-p_2[2]), shape2 = 2),
                         p3 = rbeta(x, shape1 = 2*p_2[3]/(1-p_2[3]), shape2 = 2),
                         p4 = rbeta(x, shape1 = 2*p_2[4]/(1-p_2[4]), shape2 = 2))



hist1 <- ggplot() +
  geom_histogram(data = scenario_2[1:6,], bins = 5,
                 aes(x = pi_2b, y = ..density..),
                 color = "black", fill = "white") +
  coord_cartesian(xlim = c(0,1)) +
  geom_density(data = theoretical_pi, aes(x = p1),
               bw = 0.05, linetype = 3) +
  #stat_function(fun = dbeta,
  #              args = list(shape1 = 2*0.119/(1-0.119), shape2 = 2),
  #              xlim = c(0.01,1)) +
  geom_vline(xintercept = p_2[1], color = "black", lwd = 2) +
  labs(title = "Dosage = 0 mg", x = "Probability of Deformity")

hist1


scenario2Tab <- scenario_2 %>%
  group_by(dose) %>%
  summarise(mean_2a_pi = round(mean(pi_2a),3), sd_2a_pi = round(sd(pi_2a),3),
            mean_2a_cnt = round(mean(count_2a),3), sd_2a_cnt = round(sd(count_2a),3),
            mean_2b_pi = round(mean(pi_2b),3), sd_2b_pi = round(sd(pi_2b),3),
            mean_2b_cnt = round(mean(count_2b),3), sd_2b_cnt = round(sd(count_2b),3)) %>%
  as.data.frame()
colnames(scenario2Tab) <- c("Dosage","Mean p", "SD p",
                            "Mean Count", "SD Count", "Mean p", "SD p",
                            "Mean Count", "SD Count")
kable(scenario2Tab, booktabs = T,
      caption="Summary statistics of Scenario 2 by dose.") %>%
  add_header_above(c(" " = 1, "Scenario 2a" = 4,
                     "Scenario 2b" = 4)) %>%
  kable_styling(latex_options = "scale_down") %>%
  column_spec(c(4:5,8:9), width = "1cm")



scenario_2 <- scenario_2 %>%
  mutate(dose = dose,
         phat_2a = count_2a / 10, phat_2b = count_2b / 10,
         logit_2a = log ((count_2a + 0.5) / (10 - count_2a + 0.5)),
         logit_2b = log ((count_2b + 0.5) / (10 - count_2b + 0.5)) )


fit_2a_binom = glm(phat_2a ~ dose, family=binomial, weight=rep(10,24), data=scenario_2)
summary(fit_2a_binom)
exp(coef(fit_2a_binom))
exp(confint(fit_2a_binom))

fit_2a_quasi = glm(phat_2a ~ dose, family=quasibinomial, weight=rep(10,24),
                   data=scenario_2)
summary(fit_2a_quasi)
exp(coef(fit_2a_quasi))
exp(confint(fit_2a_quasi))


fit_2b_binom = glm(phat_2b ~ dose, family=binomial, weight=rep(10,24), data=scenario_2)
summary(fit_2b_binom)
exp(coef(fit_2b_binom))
exp(confint(fit_2b_binom))


fit_2b_quasi = glm(phat_2b ~ dose, family=quasibinomial,
                   weight=rep(10,24), data=scenario_2)
summary(fit_2b_quasi)
exp(coef(fit_2b_quasi))
exp(confint(fit_2b_quasi))



treetubes_yr1 <- read_csv("C:/Users/Saint/Documents/School_2017_onward/2021-2022/STA303/BMLR_Review/Ch_7/treetube.csv") %>%
  filter(YEAR == 1990 | YEAR == 1991) %>%
  spread(key = YEAR, value = HEIGHT) %>%
  mutate(growth_yr1 = `1991` - `1990`) %>%
  filter(!is.na(growth_yr1)) %>%
  rename(tubes = TUBEX, id = ID, transect = TRANSECT,
         species = SPECIES,
         height91 = `1991`, height90 = `1990`) %>%
  dplyr::select(id, transect, species, tubes, height91,
                height90, growth_yr1)


tube_linear <- lm(growth_yr1 ~ tubes, data = treetubes_yr1)

tube_multi1 <- lmer(growth_yr1 ~ tubes + (1|transect),
                    data = treetubes_yr1)
summary(tube_multi1)

#Exercise 1

set.seed(202)
# inputs for the beta distribution must be between 0 and 1
p <- seq(0,1,by=.05)

# To plot a beta density use dbeta; here I selected a=5, b=1
density1 <- dbeta(x = p,shape1 = 0.5, shape2 = 0.5)
density2 <- dbeta(x = p,shape1 = 1, shape2 = 1)
density3 <- dbeta(x = p,shape1 = 3, shape2 = 1)
density4 <- dbeta(x = p,shape1 = 1, shape2 = 3)
density5 <- dbeta(x = p,shape1 = 1, shape2 = 5)
density6 <- dbeta(x = p,shape1 = 2, shape2 = 2)
density7 <- dbeta(x = p,shape1 = 0.5, shape2 = 0.5)
density8 <- dbeta(x = p,shape1 = 2, shape2 = 5)
density9 <- dbeta(x = p,shape1 = 2, shape2 = 2.5)


plot(p, density1, type = "l")

beta_data_set = tibble(x = p, y1 = density1, y2 = density2, y3 = density3, y4 = density4, y5 = density5,
                       y6 = density6, y7 = density7, y8 = density8, y9 = density9)


ggplot(data = beta_data_set, mapping = aes(x = x)) + geom_smooth(mapping = aes(y = y1), se = FALSE, color = 'red') +
  geom_smooth(mapping = aes(y = y2), se = FALSE, color = 'blue') + geom_smooth(mapping = aes(y = y3), se = FALSE, color = 'green') +
  geom_smooth(mapping = aes(y = y4), se = FALSE, color = 'turquoise') + geom_smooth(mapping = aes(y = y5), se = FALSE, color = 'purple') +
  geom_smooth(mapping = aes(y = y6), se = FALSE, color = 'yellow') + geom_smooth(mapping = aes(y = y7), se = FALSE, color = 'steelblue') +
  geom_smooth(mapping = aes(y = y8), se = FALSE, color = 'sienna') + geom_smooth(mapping = aes(y = y9), se = FALSE, color = 'salmon')

