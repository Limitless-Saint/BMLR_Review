# Beyond Multiple Linear Regression - Review Exercises - Ch.10 - Multilevel Data With More Than Two Levels

library(knitr)
library(gridExtra)
library(GGally)
library(mice)
library(nlme)
library(lme4)
library(mnormt)
library(boot)
library(HLMdiag)
library(kableExtra)
library(pander)
library(tidyverse)



seedswd = read_csv(file = "C:/Users/Saint/Documents/School_2017_onward/2021-2022/STA303/BMLR_Review/Ch_10/seeds2.csv")
