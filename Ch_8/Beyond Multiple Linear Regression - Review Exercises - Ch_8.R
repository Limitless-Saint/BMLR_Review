# Beyond Multiple Linear Regression - Review Exercises - Ch_8

library(MASS)
library(gridExtra)
library(mnormt)
library(lme4)
library(knitr)
library(kableExtra)
library(tidyverse)

set.seed(10)
#NOTE: Since loaded MASS package be aware that select() in dplyr is masked

musicdata = read_csv(file = "C:/Users/Saint/Documents/School_2017_onward/2021-2022/STA303/BMLR_Review/Ch_8/musicdata.csv")


keydata = musicdata %>% dplyr::select(id, diary, perform_type, memory, audience, na, gender, instrument, mpqab, mpqpem, mpqnem)

# Create Level2 data set by picking off one observation
# per subject, which would be easier if every subject
# had a diary entry labeled '1' - should be 37 rows
# and 6 columns (one per L2 variable)
music.lev2 <-  keydata %>%
  group_by(id) %>%
  filter(row_number() == 1) %>%
  dplyr::select(id, gender:mpqnem)


# Add average across all performances for each subject
# for EDA plots
meanbysubj <- musicdata %>% group_by(id) %>%
  summarise(meanbysubj = mean(na, na.rm = TRUE))

music.lev2 <- music.lev2 %>%
  left_join(meanbysubj, by = "id")


# create ggplot theme for plots
# theme with grid, grey background
theme.1 <- theme(axis.title.x = element_text(size = 14),
                 axis.title.y = element_text(size = 14),
                 plot.title=element_text(hjust=.9,face="italic",size=12))


na.all = musicdata %>%  ggplot(mapping = aes(x = na)) + geom_histogram(binwidth = 2, fill = "white",color = "black") +
  theme.1 + xlim(10,35) +
  xlab("Negative Affect") + ylab("Frequency") + labs(title="(a)")


na.mean <- ggplot(data=music.lev2,aes(x=meanbysubj)) +
  geom_histogram(binwidth = 2, fill = "white",
                 color = "black") +
  theme.1 + xlim(10,35) +
  xlab("Mean Negative Affect") + ylab("Frequency") + labs(title="(b)")


mli.hist1 <- grid.arrange(na.all,na.mean,ncol=1)

mli.hist1


prop.table(table(musicdata$perform_type))
prop.table(table(musicdata$memory))
prop.table(table(musicdata$audience))



# Using the composite model specification, the unconditional means model can
# be fit to the music performance anxiety data using statistical software:

#Model A (Unconditional means model)
model.a <- lmer(formula = na ~ 1 + (1 | id), REML = TRUE , data = musicdata)

summary(model.a)


#Model B (Add large as Level 1 covariate) #need to rechracterize perform_type to large vs (small & solo)
model.b <- lmer(formula = na ~ large + (large | id), REML = TRUE,  data = musicdata)
summary(model.b)

# Guided Exercise

keydata_exer = musicdata %>% dplyr::select(id, diary, perform_type, memory, audience, pa, gender, instrument, mpqab, mpqpem, mpqnem)


theme.1 <- theme(axis.title.x = element_text(size = 14),
                 axis.title.y = element_text(size = 14),
                 plot.title=element_text(hjust=.9,face="italic",size=12))
# part 1 - Exploratory Data Analysis

# Univariate Summaries

# Level One Covariates and response
pa.all = keydata_exer %>% ggplot(mapping = aes(x = pa)) + geom_histogram(binwidth = 2, fill = "white",color = "black") +
  theme.1 + xlim(10,55) + xlab("Positive Affect") + ylab("Frequency") + labs(title = "(a)")


meanby_subj_pa = keydata_exer %>%  group_by(id) %>% summarise(mean_val = mean(pa, na.rm = TRUE))

pa.mean = meanby_subj_pa %>% ggplot(mapping = aes(x = mean_val)) + geom_histogram(binwidth = 2, fill = "white", color = "black") +
  theme.1 + xlim(10,55) + xlab("Positive Affect") + ylab("Frequency") + labs(title = "(b)")

pa_eda = grid.arrange(pa.all, pa.mean, ncol = 1)
pa_eda

prop.table(table(keydata_exer$perform_type))
prop.table(table(keydata_exer$memory))
prop.table(table(keydata_exer$audience))

#Level Two Covariates

keydata_exer_level_2 = keydata_exer %>%  group_by(id) %>%
  filter(row_number() == 1) %>%
  dplyr::select(id, pa:mpqnem)   #create data set w/ one observation per subject since covariates do not change per observation

keydata_exer_level_2$meanby_subj_pa = meanby_subj_pa$mean_val

mpqab.all = keydata_exer_level_2 %>% ggplot(mapping = aes(x = mpqab)) + geom_histogram(binwidth = 5, fill = "white", color = "black") +
  theme.1 + xlim(5, 35) + xlab("mpquab") + ylab("Frequency") + labs(title = "MPQAB")

mpqpem.all = keydata_exer_level_2 %>% ggplot(mapping = aes(x = mpqpem)) + geom_histogram(binwidth = 5, fill = "white", color = "black") +
  theme.1 + xlim(20, 70) + xlab("mpqpem") + ylab("Frequency") + labs(title = "MPQPEM")

mpqnem.all = keydata_exer_level_2 %>% ggplot(mapping = aes(x = mpqnem)) + geom_histogram(binwidth = 5, fill = "white", color = "black") +
  theme.1 + xlim(10,50) + xlab("mpqnem") + ylab("Frequency") + labs(title = "MPQNEM")

mpq.all = grid.arrange(mpqab.all, mpqpem.all, mpqnem.all, nrow = 1)

prop.table(table(keydata_exer_level_2$gender))
prop.table(table(keydata_exer_level_2$instrument))


# Bivariate Summaries

# Look at relationships among Level 1 covariates and primary response (again ignoring correlation).
# Boxplots for categorical covariates and scatterplots and lattice plot for continuous covariates.

pa_perform = keydata_exer %>%  ggplot(mapping = aes(x = as_factor(perform_type) , y = pa )) + geom_boxplot() + theme.1 +
  coord_flip() + ylab("Positive Affect") + xlab("") + labs(title = "(a)")

pa_memory = keydata_exer %>%  ggplot(mapping = aes(x = as_factor(memory) , y = pa )) + geom_boxplot() + theme.1 +
  coord_flip() + ylab("Positive Affect") + xlab("") + labs(title = "(b)")

pa_audience = keydata_exer %>%  ggplot(mapping = aes(x = as_factor(audience) , y = pa )) + geom_boxplot() + theme.1 +
  coord_flip() + ylab("Positive Affect") + xlab("") + labs(title = "(c)")

pa_categorical_level_1 = grid.arrange(pa_perform, pa_memory, pa_audience, ncol = 2)
pa_categorical_level_1

# could also examine these trends by each subject through lattice plots - one form of this is ~facet_wrap()

#Level Two

# Categorical used original dataset to get values
pa_gender = keydata_exer %>% ggplot(mapping = aes(x = as_factor(gender), y = pa)) + geom_boxplot() + theme.1 +
  coord_flip() + ylab("Positive Affect") + xlab("") + labs(title = "(a)")

pa_gender_by_mean = keydata_exer_level_2 %>% ggplot(mapping = aes(x = as_factor(gender), y = meanby_subj_pa)) + geom_boxplot() + theme.1 +
  coord_flip() + ylab("Positive Affect") + xlab("") + labs(title = "(b)")

pa_instrument = keydata_exer %>% ggplot(mapping = aes(x = as_factor(instrument), y = pa)) + geom_boxplot() + theme.1 +
  coord_flip() + ylab("Positive Affect") + xlab("") + labs(title = "(c)")

pa_instrument_by_mean = keydata_exer_level_2 %>% ggplot(mapping = aes(x = as_factor(instrument), y = meanby_subj_pa)) + geom_boxplot() + theme.1 +
  coord_flip() + ylab("Positive Affect") + xlab("") + labs(title = "(d)")


pa_categorical_level_2 = grid.arrange(pa_gender, pa_gender_by_mean, pa_instrument, pa_instrument_by_mean)
pa_categorical_level_2

# Continuous - used original dataset to get values

pa_mpqab = keydata_exer %>% ggplot(mapping = aes(x = mpqab, y = pa)) + geom_point() + geom_smooth(method = "lm", color = "black") +
  theme.1 + ylab("Positive Affect") + xlab("AB") + labs(title = "(a)")

pa_mpqpem = keydata_exer %>% ggplot(mapping = aes(x = mpqpem, y = pa)) + geom_point() + geom_smooth(method = "lm", color = "black") +
  theme.1 + ylab("Positive Affect") + xlab("PEM") + labs(title = "(b)")

pa_mpqnem = keydata_exer %>% ggplot(mapping = aes(x = mpqnem, y = pa)) + geom_point() + geom_smooth(method = "lm", color = "black") +
  theme.1 + ylab("Positive Affect") + xlab("NEM") + labs(title = "(c)")


pa_mpqab_one_obs = keydata_exer_level_2 %>% ggplot(mapping = aes(x = mpqab, y = pa)) + geom_point() + geom_smooth(method = "lm", color = "black") +
  theme.1 + ylab("Positive Affect") + xlab("AB") + labs(title = "(d)")

pa_mpqpem_one_obs = keydata_exer_level_2 %>% ggplot(mapping = aes(x = mpqpem, y = pa)) + geom_point() + geom_smooth(method = "lm", color = "black") +
  theme.1 + ylab("Positive Affect") + xlab("PEM") + labs(title = "(e)")

pa_mpqnem_one_obs = keydata_exer_level_2 %>% ggplot(mapping = aes(x = mpqnem, y = pa)) + geom_point() + geom_smooth(method = "lm", color = "black") +
  theme.1 + ylab("Positive Affect") + xlab("NEM") + labs(title = "(f)")

pa_continous_level_2_ind = grid.arrange(pa_mpqab, pa_mpqpem, pa_mpqnem)
pa_continous_level_2_single = grid.arrange(pa_mpqab_one_obs, pa_mpqpem_one_obs, pa_mpqnem_one_obs)

pa_continous_level_2_ind
pa_continous_level_2_single

#part 2

model.A = lmer(formula = pa ~ 1 + (1|id), REML = TRUE, data = keydata_exer)  #Unconditional Means / Random Intercept Model
summary(model.A)

intraclass_correlation_A = 23.72/(41.70 + 23.72)
intraclass_correlation_A

# part 3

# Had to create indicators (copied from text)

keydata_exer = keydata_exer %>% mutate(students = ifelse(test = audience=="Student(s)", yes = 1, no = 0),
                                       instructor = ifelse(test = audience == "Instructor", yes = 1, no = 0),
                                       juried = ifelse(test = audience=="Juried Recital", yes = 1, no = 0),
                                       public = ifelse(audience=="Public Performance",1,0),
                                       solo = ifelse(perform_type=="Solo",1,0),
                                       memory1 = ifelse(memory=="Memory",1,0),
                                       female = ifelse(gender=="Female",1,0),
                                       vocal = ifelse(instrument=="voice",1,0) )

model.B = lmer(data = keydata_exer, formula = pa ~ instructor + students + (instructor + students|id) , REML = TRUE)
summary(model.B)


pseudo_r_sq_mod_A_B = (41.70 - 36.39)/36.39
pseudo_r_sq_mod_A_B

test_mod = lmer(data = keydata_exer, formula = pa ~  students + (instructor|id) , REML = TRUE)
summary(test_mod)

test_mod_2 = lmer(data = keydata_exer, formula = pa ~  students + instructor + (instructor|id) , REML = TRUE)
summary(test_mod_2)


# part 4)

#in notebook

# part 5)

keydata_exer = keydata_exer %>% mutate(centred_mpqab = mpqab - mean(mpqab)) # created centred mpqab

model.C = lmer(data = keydata_exer, formula = pa ~ centred_mpqab + instructor + students + centred_mpqab:instructor +
                 centred_mpqab:students + (instructor + students|id), REML = TRUE)
summary(model.C)


# part 6)

model.D = lmer(data = keydata_exer, formula = pa ~ centred_mpqab + female + instructor + students + centred_mpqab:instructor +
                female:instructor +  centred_mpqab:students + female:students + (instructor + students|id), REML = TRUE)

summary(model.D)

#part 8)

drop_in_dev_C_D = anova(model.D, model.C, test = 'Chiaq')
drop_in_dev_C_D


pseudo_r_sq_mod_C_D_u = (20.32 - 18.53)/20.32
pseudo_r_sq_mod_C_D_u

pseudo_r_sq_mod_C_D_v = (8.062 - 8.19)/8.062
pseudo_r_sq_mod_C_D_v

pseudo_r_sq_mod_C_D_w = (10.72 - 10.72)/10.72
pseudo_r_sq_mod_C_D_w

