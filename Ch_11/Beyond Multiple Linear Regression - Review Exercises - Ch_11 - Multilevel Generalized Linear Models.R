# Beyond Multiple Linear Regression - Review Exercises - Ch.11 - Multilevel Generalized Linear Models

library(gridExtra)
library(lme4)
library(pander)
library(lattice)
library(ggmosaic)
library(knitr)
library(kableExtra)
library(tidyverse)

refdata = read_csv(file = "C:/Users/Saint/Documents/School_2017_onward/2021-2022/STA303/BMLR_Review/Ch_11/basketball0910.csv")

theme.1 <- theme(axis.title.x = element_text(size = 14),
                 axis.title.y = element_text(size = 14),
                 plot.title=element_text(hjust=.9,face="italic",size=12))


with(refdata, summary(time))
with(refdata, summary(score.diff))
with(refdata, summary(foul.diff))
with(refdata, sd(time))
with(refdata, sd(score.diff))
with(refdata, sd(foul.diff))

refdata %>% count(foul.home) %>%
  mutate(prop = n/sum(n))
refdata %>% count(lead.home) %>%
  mutate(prop = n/sum(n))
refdata %>% count(previous.foul.home) %>%
  mutate(prop = n/sum(n))
refdata %>% count(foul.type) %>%
  mutate(prop = n/sum(n))


f1 <- refdata %>% count(hometeam) %>%
  mutate(prop = n/sum(n)) %>%
  rename(n_fouls = n)
f1
f2 <- refdata %>% count(visitor) %>%
  mutate(prop = n/sum(n)) %>%
  rename(n_fouls = n)
f2
table(refdata$hometeam)
table(refdata$visitor)

onepergame <- refdata %>%
  group_by(game) %>%
  filter(row_number() == 1) %>%
  ungroup
p1 <- onepergame %>% count(hometeam) %>%
  mutate(prop = n/sum(n)) %>%
  rename(n_games = n)
print(p1, n = Inf)
p2 <- onepergame %>% count(visitor) %>%
  dplyr::mutate(prop = n/sum(n)) %>%
  rename(n_games = n)
print(p2, n = Inf)
table(onepergame$hometeam)
table(onepergame$visitor)


allhome <- p1 %>%
  inner_join(f1, by = "hometeam") %>%
  dplyr::mutate(fouls_per_game = n_fouls / n_games) %>%
  arrange(desc(fouls_per_game))
print(allhome, n = Inf)


allvis <- p2 %>%
  inner_join(f2, by = "visitor") %>%
  dplyr::mutate(fouls_per_game = n_fouls / n_games) %>%
  arrange(desc(fouls_per_game))
print(allvis, n = Inf)


allboth <- allhome %>%
  rename(team = hometeam, home_fouls = fouls_per_game) %>%
  dplyr::select(team, home_fouls) %>%
  inner_join(allvis, by = c("team" = "visitor")) %>%
  dplyr::select(team, home_fouls, fouls_per_game) %>%
  rename(visitor_fouls = fouls_per_game) %>%
  dplyr::mutate(diff = home_fouls - visitor_fouls) %>%
  arrange(desc(diff))
print(allboth, n = Inf)


with(refdata, by(time, foul.home, summary))
with(refdata, by(score.diff, foul.home, summary))
with(refdata, by(foul.diff, foul.home, summary))

refdata <- refdata %>%
  mutate(foul.factor = as.factor(ifelse(foul.home == 1,
                                        "Home", "Visitor")) )

refdata <- refdata %>%
  mutate(leadyes = ifelse(lead.home == 0, "No", "Yes"),
         prevyes = ifelse(previous.foul.home == 0, "No", "Yes")) %>%
  rename(whofoul = foul.factor)


barplot1 <- ggplot(data = refdata) +
  geom_mosaic(aes(weight = 1, x = product(whofoul, foul.type),
                  fill = whofoul)) +
  xlab("Foul Type") +
  ylab("Proportion within Foul Type") +
  labs(title = "(a)") + scale_fill_grey() +
  theme(legend.title = element_blank()) + theme.1

barplot2 <- ggplot(data = refdata) +
  geom_mosaic(aes(weight = 1, x = product(whofoul, leadyes),
                  fill = whofoul)) +
  xlab("Home Team in Lead") +
  ylab("Proportion within Leading Team") +
  labs(title = "(b)") + scale_fill_grey() +
  theme(legend.title = element_blank()) + theme.1

barplot3 <- ggplot(data = refdata) +
  geom_mosaic(aes(weight = 1, x = product(whofoul, prevyes),
                  fill = whofoul)) +
  xlab("Previous Foul on Home Team") +
  ylab("Proportion within Previous Foul") +
  labs(title = "(c)") + scale_fill_grey() +
  theme(legend.title = element_blank()) + theme.1

grid.arrange(barplot1, barplot2, barplot3, ncol = 2, nrow = 2)


model.f <- glmer(foul.home ~ foul.diff + score.diff +
                   lead.home + time + offensive + personal +
                   foul.diff:offensive + foul.diff:personal +
                   foul.diff:time + lead.home:time + (1|game) + (1|hometeam) + (1|visitor),
                 family = binomial, data = refdata)


re.int <- ranef(model.f)$`game`[["(Intercept)"]]
hist(re.int, xlab = "Random Effect",
     main = "Random Effects for Game")
Home.re <- ranef(model.f)$`hometeam`[["(Intercept)"]]
hist(Home.re, xlab = "Random Effect",
     main = "Random Effects for Home Team")
Visiting.re <- ranef(model.f)$`visitor`[["(Intercept)"]]
hist(Visiting.re, xlab = "Random Effect",
     main = "Random Effects for the Visiting Team",
     xlim = c(-0.5,0.5))
cbind(Home.re, Visiting.re)


ranef1 <- dotplot(ranef(model.f, condVar = TRUE),
                  strip = FALSE)
print(ranef1[[3]], more = TRUE) ##HOME
print(ranef1[[2]], more = TRUE) ##VIS
print(ranef1[[1]], more = TRUE)
