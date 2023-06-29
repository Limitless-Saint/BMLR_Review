# Beyond Multiple Linear Regression - Review Exercises - Ch_2


library(gridExtra)
library(knitr)
library(mosaic)
library(xtable)
library(kableExtra)
library(tidyverse)


time <- c(2,2,3,3,4,5,5,6,6,7,7,7,8,9,10,11,11,12,12,14)
correct <- c(0,0,1,0,1,0,0,1,1,0,0,1,1,1,0,1,1,1,0,1)

testdf = data.frame(time, correct)

testplot = testdf %>% ggplot(mapping = aes(x = time, y = correct)) + geom_point() + geom_smooth(method = "lm") +
  labs(x = "Hours", y = "Correct")

testplot

pb=seq(0,1,length=1001)   # possible values for prob a boy is born
lik=pb^2 * (1-pb)       # likelihood of getting observed data
GBBdf <- data.frame(pb,lik)
GBBplot<- ggplot(data=GBBdf,aes(x=pb, y=lik)) +
  geom_line(color="blue", size=2) +
  xlab("possible values of pb") + ylab("Likelihood") +
  geom_segment(aes(x = .30, y = 0, xend = .30, yend = .063), linetype = "dotted") +
  geom_segment(aes(x = 0, y = .063, xend = .30, yend = .063), linetype = "dotted") +
  geom_segment(aes(x = .60, y = 0, xend = .60, yend = .144), linetype = "dotted") +
  geom_segment(aes(x = 0, y = .144, xend = .60, yend = .144), linetype = "dotted") +
  annotate("text", x = .30, y = -0.01, label= "0.30") +
  annotate("text", x = .60, y = -0.01, label= "0.60") +
  annotate("text", x = -0.05, y = .063, label= ".063") +
  annotate("text", x = -0.05, y = .144, label= ".144")


GBBplot





# Sec 2.6

# Exercises

#2

#maximizing of p_b

oLik.f = function(pb){
  return(6000*log(pb) + 4000*log(1 - pb))
}

optimize(oLik.f, interval = c(0,1), maximum = TRUE)


# Drawing of graph

pb = seq(from = 0, to = 1, length =  1001)   # possible values for prob a boy is born

lik_q3 = (pb^6000) * ((1-pb)^4000)
log_lik_q3 = 6000 * log(pb) +  4000 * log(1-pb)
q3_df = tibble("pb" = pb, "lik" = lik_q3, "log_lik" = log_lik_q3)

q3_lik_graph = q3_df %>% ggplot(mapping = aes(x = pb, y = lik)) +  geom_line() + labs(x = "pb", y = "liklihood")
q3_log_lik_graph = q3_df %>% ggplot(mapping = aes(x = pb, y = log_lik_q3)) +  geom_line() + labs(x = "pb", y = "log - liklihood")


q3_log_lik_graph





logLik_func = function(n.boys, n.girls){

  for(i in seq_along(pb)){

  lik_q3[i] = pb[i]^n.boys * (1-pb[i])^n.girls
  log_lik_q3[i] = log(lik_q3[i])

  q3_df = tibble("pb" = pb, "lik" = lik_q3, "log_lik" = log_lik_q3)

  return(q3_df)
}

q3_df_thousands = logLik_func(6000, 4000)

q3_lik_graph = q3_df_thousands %>% ggplot(mapping = aes(x = pb, y = lik)) +  geom_line() + labs(x = "pb", y = "liklihood")
q3_log_lik_graph = q3_df_thousands %>% ggplot(mapping = aes(x = pb, y = log_lik_q3)) +  geom_line() + labs(x = "pb", y = "log - liklihood")

grid.arrange(q3_lik_graph, q3_log_lik_graph)



p = seq(0,1,.01)   # possible values for p
#
# Defining a function that calculates logLikelihoods
logLik <- function(n.boys,n.girls){
  Lik=p^n.boys*(1-p)^n.girls
  logLik=log(Lik)
  lik.frame <- data.frame(p,Lik,logLik)
  return(lik.frame)
}
#define data frames
thirtytwenty.frame <- logLik(30,20)
sixhundred.frame <- logLik(600,400)
sixthousand.frame <- logLik(6000,4000)


thirtytwenty.lik <- ggplot(thirtytwenty.frame,aes(x=p,y=Lik)) + geom_line() +
  labs(x="p",y="Likelihood",title="(a) \n30 boys, 20 girls Likelihood")+
  theme(axis.ticks = element_blank(), axis.text.y = element_blank())
thirtytwenty.log <- ggplot(thirtytwenty.frame,aes(x=p,y=logLik)) + geom_line() +
  labs(x="p",y="logLikelihood",title="(b) \n30 boys, 20 girls log(Likelihood)") +
  theme(axis.ticks = element_blank(), axis.text.y = element_blank())
sixhundred.lik <- ggplot(sixhundred.frame,aes(x=p,y=Lik)) + geom_line() +
  labs(x="p",y="Likelihood",title="(c) \n600 boys, 400 girls Likelihood") +
  theme(axis.ticks = element_blank(), axis.text.y = element_blank())
sixhundred.log <- ggplot(sixhundred.frame,aes(x=p,y=logLik)) + geom_line() +
  labs(x="p",y="logLikelihood",title="(d) \n600 boys, 400 girls log(Likelihood)") +
  theme(axis.ticks = element_blank(), axis.text.y = element_blank())
