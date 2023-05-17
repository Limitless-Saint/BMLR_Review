# Beyond Multiple Linear Regression - Review Exercises - Ch_4

library(gridExtra)
library(knitr)
library(kableExtra)
library(mosaic)
library(xtable)
library(pscl)
library(multcomp)
library(pander)
library(MASS)
library(tidyverse)

fHH1 <- read_delim("C:/Users/Saint/Documents/School_2017_onward/2021-2022/STA303/BMLR_Review_Exercises/fHH1.csv")

fHH1 = fHH1 %>% dplyr::select(2:6)
 
 # Sec 4.4.3

modela = glm(data = fHH1, formula = total ~ age, family = poisson)
summary(modela)

confint(modela)
exp(confint(modela))


model0 = glm(data = fHH1, formula = total ~ 1, family = poisson)
drop_in_dev = anova(model0, modela, test = "Chisq")

drop_in_dev


# Sec 4.4.6

fHH1 = fHH1 %>% mutate(age2 = age * age)
modela2 = glm(data = fHH1, formula = total ~ age + age2, family = poisson)

summary(modela2)
drop_in_dev2 = anova(modela, modela2, test = "Chisq")
drop_in_dev2

# Sec 4.4.7

modela2L = glm(data = fHH1, formula = total ~ age + age2 + location, family = poisson)
summary(modela2L)

exp(coef(modela2L))


lfitteda = predict(modela)
lresida = residuals(modela)
lresid.df = data.frame(lfitteda, lresida)

ggplot(data = lresid.df, mapping = aes(x = lfitteda, y = lresida)) + geom_point(alpha = 0.25) + 
  geom_smooth(method = "loess", size = 1.5, linetype = 2) + geom_line(y = 0, size = 1.5, col = "red") +
  labs(x = "Fitted values", y = "Deviacne Residuals")



# Sec 4.6

c_data <- read_csv("c_data.csv")

ggplot(data = c_data, mapping = aes(x = nv)) + geom_histogram(bins = 15, color = "black", fill = "red") +
  xlab("Number of violent crimes")

table2chp4 = with(data = c_data, round(prop.table(table(type, region), 2), 3))
table2chp4

c_data = c_data %>% filter(nvrate < 5)

c_data <- c_data %>%
  mutate(region, region = fct_recode(region, 
                                     "S" = "SW", "S"="SE"))

table4ch4 = c_data %>% group_by(region, type) %>% 
  summarise(MeanCount = mean(nv, na.rm = TRUE), VarCount = var(nv, na.rm = TRUE), MeanRate = mean(nvrate, na.rm = TRUE), 
            VarRate = var(nvrate, na.rm = TRUE), n = n())

table4ch4


ggplot(data = c_data, mapping = aes(x = region, y = nvrate, fill = type)) + geom_boxplot() + ylab("Violent crimes per 1000")


# Sec 4.8

modeltr = glm(data = c_data, formula = nv ~ type + region, family = poisson, offset = log(enroll1000))
summary(modeltr)

mult_comp = summary(glht(model = modeltr, linfct = mcp(region = "Tukey")))
mult_comp

modeli = glm(data = c_data, formula = nv ~ type + region + region:type, family = poisson, 
             offset = log(enroll1000))
summary(modeli)

drop_in_dev_class = anova(modeltr, modeli, test = "Chisq")
drop_in_dev_class


# Sec 4.9

modeliq = glm(data = c_data, formula = nv ~ type + region + region:type, family = quasipoisson, 
              offset = log(enroll1000))
summary(modeliq)

modeltrq = glm(data = c_data, formula = nv ~ type + region, family = quasipoisson,
               offset = log(enroll1000))

drop_in_dev_disp = anova(modeltrq, modeliq, test = "F")
drop_in_dev_disp

# Sec 4.9.3

c_data2 = c_data %>% mutate(enroll1000 = ifelse(enroll1000 < 1, 1, enroll1000)) #had to make all number non-negative to use negative binomial regression

modelinb = glm.nb(data = c_data2, formula = nv ~ type + region + region:type, offset(log(enroll1000)))
summary(modelinb)


# Sec 4.10 - ZIP Model


# EDA Plotting of Data

# Observed 
zip.data = weekendDrinks <- read_csv("weekendDrinks.csv")

obs_table = tally(group_by(zip.data, drinks)) %>% mutate(prop = round(n/sum(n), 3))
obs_table

g.obs = obs_table %>%  ggplot(mapping = aes(x = drinks, y = prop)) + geom_bar(stat = "identity") + 
  labs(x = "Number of Drinks", y = "Proportion") + coord_cartesian(ylim = c(0, 0.5))

g.obs

# Modeled

sum1 = zip.data %>% summarize(lambda = mean(drinks), maxDrinks = max(drinks))
possible.values = with(data = sum1, 0:maxDrinks)
model.prob = with(data = sum1, dpois(x = possible.values, lambda = lambda))
pois.model = data.frame(possible.values, model.prob)

g.model = ggplot(data = pois.model, mapping = aes(x = possible.values, y = model.prob)) + geom_bar(stat = "identity") + 
  labs(x = "Number of Drinks", y = "Probability") + ggtitle("Poisson Model") + coord_cartesian(ylim = c(0, 0.5))

g.model


# Modeling

sex.table <- tally(group_by(zip.data, sex))  %>%
  mutate(prop=round(n/sum(n),3))

dorm.table <- tally(group_by(zip.data, dorm))  %>%
  mutate(prop=round(n/sum(n),3))

zip.data <- zip.data %>% 
  mutate(off.campus=ifelse(dorm=="off campus",1,0))

off.table <- tally(group_by(zip.data, off.campus))  %>%
  mutate(prop=round(n/sum(n),3))

zip.data <- zip.data %>% 
  mutate(firstYear=dorm%in%c("kildahl","mohn","kittlesby"))

fy.table <- tally(group_by(zip.data, firstYear))  %>%
  mutate(prop=round(n/sum(n),3))


pois.m1 = glm(data = zip.data, formula = drinks ~ off.campus + sex, family = poisson)
summary(pois.m1)

exp(coef(pois.m1))


zip.m2 <- zeroinfl(drinks ~ off.campus + sex | firstYear,
                   data = zip.data)
summary(zip.m2)

exp(coef(zip.m2))
