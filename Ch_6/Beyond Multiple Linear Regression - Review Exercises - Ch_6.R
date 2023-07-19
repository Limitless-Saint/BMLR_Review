# Beyond Multiple Linear Regression - Review Exercises - Ch_6

library(gridExtra)
library(mnormt)
library(lme4)
library(knitr)
library(pander)
library(tidyverse)
library(foreign)
library(RColorBrewer)


set.seed(0)
dat <- tibble(x=runif(200, -5, 10),
              p=exp(-2+1*x)/(1+exp(-2+1*x)),
              y=rbinom(200, 1, p),
              y2=.3408+.0901*x,
              logit=log(p/(1-p)))
dat2 <- tibble(x = c(dat$x, dat$x),
               y = c(dat$y2, dat$p),
               `Regression model` = c(rep("linear", 200),
                                      rep("logistic", 200)))



rrHale.df = read_csv(file = "C:/Users/Saint/Documents/School_2017_onward/2021-2022/STA303/BMLR_Review/Ch_6/RR_Data_Hale.csv")

rrHale.df =  rrHale.df %>%
  select(community = County, pctBlack = `% Pop. Black`,
                distance = `Distance from RR`,
                YesVotes = `Yes 1st Vote`,
                NumVotes = `Num Voting 1st`) %>%
  filter(!is.na(pctBlack)) %>%
  mutate(propYes = YesVotes / NumVotes,
         InFavor = (propYes > .50))


rr_scatter = rrHale.df %>% ggplot(mapping = aes(x = distance, y = pctBlack, color = InFavor)) +
  geom_point(mapping = aes(shape = InFavor), size = 2.5) + ylab("Percent Black residents in the community") +
  xlab("Distance to the proposed railroad")

rr_scatter

total_proportion = sum(rrHale.df$YesVotes)/sum(rrHale.df$NumVotes)
total_proportion


## Empirical logit Plots
# Compute the empirical logits (added 0.5 to avoid log(0))
phat <- with(rrHale.df, (YesVotes+.5)/(NumVotes+1))

#rrHale.df = rrHale.df %>% select(-logits)
rrHale.df$elogit <- log(phat/(1-phat))
rrHale.df$Greensboro <- ifelse(rrHale.df$community=="Greensboro", "Greensboro", NA)

logits_plot_dis = rrHale.df %>% ggplot(mapping = aes(x = distance, y = elogit)) + geom_point(shape = 1) +
  geom_smooth(method = 'lm', se = FALSE) + xlab("distance") + ylab("empiriacal logits")

logits_plot_black = rrHale.df %>% ggplot(mapping = aes(x = pctBlack, y = elogit)) + geom_point(shape = 1) +
  geom_smooth(method = lm, se = FALSE) + xlab("percent Black") + ylab("empiriacal logits") +
  geom_text(mapping = aes(label = Greensboro), nudge_x = 7.5, nudge_y = -0.5)

grid.arrange(logits_plot_dis, logits_plot_black, ncol = 1)

cor(x = rrHale.df$distance, y = rrHale.df$pctBlack)

# Model with just distance
model.HaleD = glm(data = rrHale.df, formula = cbind(YesVotes, NumVotes - YesVotes) ~ distance, family = binomial)
summary(model.HaleD)

model.HaleBD = glm(data = rrHale.df, formula = cbind(YesVotes, NumVotes - YesVotes) ~ distance + pctBlack, family = binomial)
summary(model.HaleBD)

# Tests for significance of model coefficients

drop_in_dev = anova(model.HaleD, model.HaleBD, test = 'Chisq')
drop_in_dev

exp(confint(model.HaleBD))

# Testing for Goodness of Fit

1 - pchisq(q = model.HaleBD$deviance,df = model.HaleBD$df.residual)

model.HaleBxD = glm(data = rrHale.df, formula = cbind(YesVotes, NumVotes - YesVotes) ~ distance + pctBlack +
                      distance:pctBlack, family = binomial)

summary(model.HaleBxD)
drop_in_dev_BxD = anova(model.HaleBD, model.HaleBxD, test = 'Chisq')
drop_in_dev_BxD


#Residual Plot

rrHale.df = rrHale.df %>%  mutate(resid.BxD = residuals(model.HaleBxD), fit.BxD = fitted.values(model.HaleBxD))

ggplot(rrHale.df, aes(x = fit.BxD, y = resid.BxD)) +
  geom_point() +
  geom_text(aes(label=Greensboro), nudge_x = -.075) +
  xlab("Fitted values from interaction model") +
  ylab("Deviance residuals from interaction model")


# Overdispersion

model.HaleBxDq = glm(data = rrHale.df, formula = cbind(YesVotes, NumVotes - YesVotes) ~ distance + pctBlack +
                      distance:pctBlack, family = quasibinomial)

summary(model.HaleBxDq)

model.HaleBxDq_inter_rm = glm(data = rrHale.df, formula = cbind(YesVotes, NumVotes - YesVotes) ~ distance + pctBlack,
                              family = quasibinomial)

summary(model.HaleBxDq_inter_rm)

#Study 3

risk2009.data = foreign::read.spss(file = "C:/Users/Saint/Documents/School_2017_onward/2021-2022/STA303/BMLR_Review/Ch_6/yrbs09.sav", to.data.frame=TRUE)
names(risk2009.data)

set.seed(33)
risk2009.samp <- risk2009.data %>%
  sample_n(500)

rm(risk2009.data)

# Data Prep

sex = with(data = risk2009.samp, expr = Q2)
sex = with(data = risk2009.samp, if_else(sex == "Female", 0, 1))
sex.l=with(risk2009.samp,factor(x = sex, labels = c("Female", "Male")))
table(sex.l)

lose.wt4 = with(risk2009.samp,Q66)
table(lose.wt4)
lose.wt2= with(risk2009.samp,ifelse(Q66=="Lose weight",1,0))
table(lose.wt2)
lose.wt.l=with(risk2009.samp,factor(lose.wt2, labels = c("No weight loss", "Lose weight")))
table(lose.wt.l)

sport4 = with(risk2009.samp,Q84)
table(sport4)
sport =  with(risk2009.samp,ifelse(Q84=="0 teams",0,1))
sport.l=with(risk2009.samp,factor(sport, labels = c("No sports", "Sports")))
table(sport.l)

media=with(risk2009.samp,as.numeric(Q81)) # hours of TV per day
table(media)
media=media-2
table(media)
media=ifelse(media==0,0.5,media)
table(media)
media=ifelse(media==-1,0,media)
table(media)

risk2009 <- risk2009.samp %>%
  mutate(sex = sex.l, lose.wt = lose.wt.l, sport = sport.l,
         media = media, lose.wt.01 = lose.wt2) %>%
  mutate(sport_fac = sport4,
         sport_num = parse_number(as.character(sport4))) %>%
        select(lose.wt, lose.wt.01, sex, sport, sport_fac,
                sport_num, media, bmipct) %>%
  filter(complete.cases(.))

rm(risk2009.samp)

write_csv(x = risk2009, file = "C:/Users/Saint/Documents/School_2017_onward/2021-2022/STA303/BMLR_Review/Ch_6/risk2009.csv")


prop.table(table(x = risk2009$lose.wt.01))

#sereis of prop.tables were done here

ggplot(data = risk2009, aes(x = sex, fill = lose.wt)) +
  geom_bar(position = "fill") +
  ylab("Proportion") + scale_fill_grey()


risk2009 %>%
  group_by(sex, lose.wt) %>%
  summarise(mean = mean(bmipct), sd = sd(bmipct), n = n())


risk2009 = risk2009 %>%  group_by(sex) %>% mutate( BMIcuts = cut_number(bmipct,10))

emplogit1 = risk2009 %>% group_by(sex, BMIcuts) %>%  summarise(prop.lose = mean(lose.wt.01), n = n(), midpoint = median(bmipct)) %>%
  mutate(prop.lose = if_else(condition = prop.lose == 0, true = 0.01, false = prop.lose), emplogit = log(prop.lose/(1-prop.lose)))


ggplot(data = emplogit1, mapping = aes(x = midpoint, y = emplogit, color = sex)) + geom_point(mapping = aes(shape = sex)) +
  geom_smooth(mapping = aes(linetype = sex), method = 'lm') + xlab("BMI Percentile") + ylab("Empirical Logits")


# Initial Models


risk2009$sex = relevel(risk2009$sex, ref = "Male")

model1 = glm(data = risk2009, formula = lose.wt.01 ~ sex, family = binomial)
summary(model1)

model2 = glm(data = risk2009, formula = lose.wt.01 ~ sex + bmipct, family = binomial)
summary(model2)


covid = read_csv(file = "C:/Users/Saint/Documents/School_2017_onward/2021-2022/STA303/BMLR_Review/Ch_6/Age_and_sex_COVID-19_data.csv")



