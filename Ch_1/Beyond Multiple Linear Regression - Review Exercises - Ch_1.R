#Beyond Multiple Linear Regression - Review Exercises - Ch_1

library(knitr)
library(gridExtra)
library(GGally)
library(kableExtra)
library(jtools)
library(rsample)
library(broom)
library(tidyverse)
library(modelr)

# Textbook example

derby_plus = read_csv("Ch_1/derbyplus.csv") #note  file path and project environment

derby_plus = derby_plus %>%  mutate(fast = if_else(condition == "fast", true = 1, false = 0),
                                                 good = if_else(condition == "good", true = 1, false = 0), yearnew = year - 1896,
                                                 fastfactor = if_else(fast == 0, "not fast", "fast"))


count_of_conditions = derby_plus %>% group_by(condition) %>% summarise('count' = n()) %>% mutate('perc' = count/sum(count))

gg <- ggpairs(data = derby_plus,
              columns = c("condition", "year", "starters", "speed"))


derby_plus %>% ggplot(mapping = aes(x = year, y = speed, colour = fastfactor)) + geom_point(mapping = aes(shape = fastfactor)) +
  geom_smooth(mapping = aes(linetype = fastfactor), method = lm, se = TRUE)

model1 <- lm(speed ~ year, data = derby_plus)
model2 <- lm(speed ~ yearnew, data = derby_plus)

par(mfrow=c(2,2))
plot(model2)
par(mfrow = c(1,1))


ggplot(derby_plus, aes(x = year, y = speed)) +
  geom_point() +
  stat_smooth(method = "lm", formula = y ~ x,
              se = FALSE, linetype = 1) +
  stat_smooth(method = "lm", formula = y ~ x + I(x^2),
              se = FALSE, linetype = 2)

derby_plus = derby_plus %>%  mutate(yearnew2 = yearnew^2)
model2q = lm(data = derby_plus, speed ~ yearnew + yearnew2)

par(mfrow=c(2,2))
plot(model2q)
par(mfrow = c(1,1))


model3 = lm(data = derby_plus, formula = speed ~ fast)

ggplot(data = derby_plus, mapping = aes(x = fast, y = speed)) + geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE, linetype = 1)


model4 <- lm(speed ~ yearnew + fast, data = derby_plus)
derby_plus = derby_plus %>% add_predictions(model = model4, var = "pred")

# Exercises

# Guided Exercises - # 1

banksalary = read_csv("Ch_1/banksalary.csv")

banksalary = banksalary %>% mutate(sex_ind = if_else(sex == "FEMALE", true = 0, false = 1)) # created indicator variable based on sex



summary(banksalary)




mean_salaries = banksalary %>% group_by(sex) %>% summarize(mean_bsal = mean(bsal), mean_sal_77 = mean(sal77))
View(mean_salaries)

percentage_diff_base = (mean_salaries[2,2] - mean_salaries[1,2])/mean_salaries[1,2]
as.numeric(percentage_diff_base)



# Univariate EDA

banksalary %>% ggplot(mapping = aes(x = sal77)) + geom_histogram(binwidth = 1000, fill = "white", color = "black") + labs(x = "salary", y = "frequency")
banksalary %>% ggplot(mapping = aes(x = sal77)) + geom_histogram(binwidth = 1000, fill = "white", color = "black") + facet_wrap(~ sex) +
  labs(x = "salary", y = "frequency") # Distribution of response variable as slary in 1977


banksalary %>% ggplot(mapping = aes(x = bsal)) + geom_histogram(binwidth = 500, fill = "white", color = "black") + labs(x = "salary", y = "frequency")
banksalary %>% ggplot(mapping = aes(x = bsal)) + geom_histogram(binwidth = 500, fill = "white", color = "black") + facet_wrap(~ sex) +
  labs(x = "salary", y = "frequency") # Distribution of response variable as beginning salary



banksalary %>% ggplot(mapping = aes(x = senior)) + geom_histogram(binwidth = 2, fill = "white", color = "black") + labs(x = "senior", y = "frequency") # univariate EDA
banksalary %>% ggplot(mapping = aes(x = sex)) + geom_bar(fill = "white", color = "black") + labs(x = "sex", y = "count")
banksalary %>% ggplot(mapping = aes(x = age)) + geom_histogram(binwidth = 30, fill = "white", color = "black") + labs(x = "age", y = "frequency")
banksalary %>% ggplot(mapping = aes(x = as_factor(educ))) + geom_bar(fill = "white", color = "black") + labs(x = "educ", y = "frequency")
banksalary %>% ggplot(mapping = aes(x = as_factor(educ))) + geom_bar(fill = "white", color = "black") + labs(x = "educ", y = "frequency") + facet_wrap(~ sex)
banksalary %>% ggplot(mapping = aes(x = exper)) + geom_histogram(binwidth = 20, fill = "white", color = "black") + labs(x = "exper", y = "frequency")


#Bivariate EDA

banksalary %>% ggplot(mapping = aes(x = age, y = bsal)) + geom_point() + geom_smooth(method = "lm", se = FALSE) + labs(x = "age", y = "bsal")
banksalary %>% ggplot(mapping = aes(x = age, y = bsal )) + geom_point(mapping = aes(colour = sex)) + geom_smooth(method = "lm", se = FALSE) +
  labs(x = "age", y = "bsal")
banksalary %>% ggplot(mapping = aes(x = age, y = bsal, colour = sex)) + geom_point() + geom_smooth(method = "lm", se = FALSE) +
  labs(x = "age", y = "bsal")


banksalary %>% ggplot(mapping = aes(x = exper, y = bsal)) + geom_point() + geom_smooth(method = "lm", se = FALSE) + labs(x = "exper", y = "bsal")
banksalary %>% ggplot(mapping = aes(x = exper, y = bsal)) + geom_point(mapping = aes(colour = sex)) + geom_smooth(method = "lm", se = FALSE) +
  labs(x = "exper", y = "bsal")



banksalary %>%  ggplot(mapping = aes(x = as_factor(educ), y = bsal)) + geom_boxplot() + labs(x = "educ", y = "bsal")
banksalary %>%  ggplot(mapping = aes(x = as_factor(educ), y = bsal)) + geom_boxplot() + facet_wrap(~ sex) + labs(x = "educ", y = "bsal")

banksalary %>% ggpairs(columns = c(1, 3, 5:7))

banksalary %>% ggplot(mapping = aes(x = senior, y = bsal)) + geom_point()

# Model 1

model1 = lm(data = banksalary, formula = bsal ~ exper)
summary(model1)
plot(model1)

# Model 2

model2 = lm(data = banksalary, formula = bsal ~ exper + senior + educ + age)
summary(model2)
plot(model2)

drop_in_dev = anova(model1, model2, test = "F")
drop_in_dev

# Interaction scatterplot

banksalary = banksalary %>% mutate(age_exper_int = age * exper)
banksalary %>% ggplot(mapping = aes(x = age_exper_int, y = bsal)) +
  geom_point(mapping = aes(colour = sex)) + geom_smooth(mapping = aes(colour = sex),method = "lm", se = FALSE) +
  labs(x = "age_exper_int", y = "bsal") + theme(axis.text.x = element_text(angle = 315)) # could also have made sex variable global


#Model 3

model3 = lm(data = banksalary, formula = bsal ~ exper + senior + educ + sex)
summary(model3)
plot(model3)
confint(model3)


drop_in_dev_2 = anova(model2, model3)
drop_in_dev

AIC(model2, model3)

# Model 3 logged

model3_log = lm(data = banksalary, formula = log(bsal) ~ exper + senior + educ + sex)
summary(model3_log)
plot(model3_log)


#Own final model

banksalary = banksalary %>% mutate(age_sex = age * sex_ind, sex_exper = sex_ind * exper, sex_educ = sex_ind * educ,
                                   sex_senior = sex_ind * senior)

model4_full = lm(data = banksalary, formula = bsal ~ exper + senior + educ + sex + age_sex + sex_exper + sex_educ)
summary(model4_full)
plot(model4_full)
