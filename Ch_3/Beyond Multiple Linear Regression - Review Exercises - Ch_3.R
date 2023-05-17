# Beyond Multiple Linear Regression - Review Exercises - Ch_3

library(gridExtra)
library(knitr)
library(kableExtra)
library(tidyverse)

set.seed(300)


plotBinomial_cdf = function(n, p){
  y1 = 0:n
  prob = pbinom(q = y1, size = n, p = p) #P(Y = y)
  cdf_df = data.frame(y1, prob)
  ggplot(data = cdf_df, mapping = aes(x = y1, xend = y1, y = 0, yend = prob)) + geom_segment() +
    labs(x = "number of successes", y = "cumulative_prob", title = paste("n = ", n, "p = ", p))
}

binom_cdf1 = plotBinomial_cdf(n = 10, p = 0.25) + xlim(0, 10) 
binom_cdf2 = plotBinomial_cdf(n = 10, p = 0.5) 
binom_cdf3 = plotBinomial_cdf(n = 30, p = 0.25) + xlim(0, 30)
binom_cdf4 = plotBinomial_cdf(n = 60, p = 0.25) + xlim(0, 60)

grid.arrange(binom_cdf1, binom_cdf2, binom_cdf3, binom_cdf4)


plot_pois = function(lambda){
  y1 = 0:10000
  
  pmf_df = data.frame(x =  rpois(n = y1, lambda = lambda)) #generate random deviates, needed to be able to grpahically display poisson
  ggplot(data = pmf_df, mapping = aes(x = x)) + geom_histogram(mapping = aes(y = ..count../sum(..count..)), binwidth = .25) +
    labs(x = "sumber of events", y = "probability", title = paste("Poisson lambda = ", lambda))
}


plotpois_1 = plot_pois(0.5)
plotpois_2 = plot_pois(1)
plotpois_3 = plot_pois(5)

grid.arrange(plotpois_1, plotpois_2, plotpois_3)


pmf_df = data.frame(x =  rpois(n = 10000, lambda = 0.5)) #generate random deviates, needed to be able to grpahically display poisson
View(pmf_df %>% group_by(x) %>% summarise(n = n()))



x <- seq(0, 7, by = 0.01)
`r = 1, lambda = 1` <- dgamma(x, 1, rate = 1)
`r = 2, lambda = 1` <- dgamma(x, 2, rate = 1) 
`r = 5, lambda = 5` <- dgamma(x, 5, rate = 5)
`r = 5, lambda = 7` <- dgamma(x, 5, rate = 7)
gammaDf <- tibble(x, `r = 1, lambda = 1`, `r = 2, lambda = 1`, `r = 5, lambda = 5`, `r = 5, lambda = 7`) %>%
  gather(2:5, key = "Distribution", value = "value") %>%
  mutate(Distribution = factor(Distribution, 
                               levels = c("r = 2, lambda = 1", 
                                          "r = 1, lambda = 1", 
                                          "r = 5, lambda = 5", 
                                          "r = 5, lambda = 7")))


ggplot(data = gammaDf, aes(x = x, y = value, 
                           color = Distribution)) +
  geom_line(aes(linetype = Distribution)) +
  xlab("values") + ylab("density") + 
  labs(title = "Gamma Distributions") +
  theme(legend.title = element_blank())



testnorm_df = data.frame(x = rnorm(n = 20000, mean = 0, sd = 3))
testnorm_plot = testnorm_df %>%  ggplot(mapping = aes(x = x)) + geom_density(kernel = "gaussian", color = "blue")
testnorm_plot


x <- seq(-10, 20, by = 0.01)
norm1 <- dnorm(x, -5, 2)
norm2 <- dnorm(x, 0 , 1)
norm3 <- dnorm(x, 5, 5)
norm4 <- dnorm(x, 0, 3)

normDf <- tibble(x, norm1, norm2, norm3, norm4) %>%
  rename(`N(-5, 2)` = norm1,
         `N(0, 1)` = norm2,
         `N(10, 5)` = norm3,
         `N(0, 3)` = norm4) 

normDf = normDf %>%  gather(2:5, key = "Distribution", value = "value") 

normDf = normDf %>% mutate(Distribution = factor(Distribution, 
                               levels = c("N(10, 5)","N(0, 3)", "N(0, 1)", "N(-5, 2)")))

ggplot(data = normDf, 
       aes(x = x, y = value, color = Distribution)) +
  geom_line(aes(linetype = Distribution)) +
  xlab("values") + ylab("density") + 
  labs(title = "Normal Distributions")

# Guided Exercises

# 1 - Beta-Binomial

# Plain Binom

binom_guid_obs = rbinom(n = 1000, size = 10, prob = 0.8) 
binom_guid_tbl = data.frame( "success" = binom_guid_obs)

binom_guid_hist = binom_guid_tbl %>% ggplot(mapping = aes(x = success, colour = "red")) + geom_histogram(bins = 10) + 
  labs(x = "success", y = "counts")

binom_guid_hist

#beta-binom

probs_frm_beta = rbeta(n = 1000, shape1 = 4, shape2 = 1)
binom_beta_params_obs = rbinom(n = 1000, size = 10, prob = probs_frm_beta)
binom_beta_params_tbl = data.frame("success" = binom_beta_params_obs)

binom_beta_params_hist = binom_beta_params_tbl %>% ggplot(mapping = aes(x = success, colour = "red")) + geom_histogram(bins = 10) + 
  labs(x = "success", y = " counts")
binom_beta_params_hist


# Comparison

plain_binom_summ_stats = binom_guid_tbl %>% summarise("mean" = mean(success), "sd" = sd(success))
binom_beta_params_summ_stats = binom_beta_params_tbl %>%  summarise("mean" = mean(success), "sd" = sd(success))

binded_rows_plain_beta_binom = bind_rows(plain_binom_summ_stats, binom_beta_params_summ_stats)
binded_rows_plain_beta_binom = binded_rows_plain_beta_binom %>% mutate("name" = c("plain_binom", "binom_beta")) %>% select(name, 1:2)
