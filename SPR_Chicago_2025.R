# Code for penny?.tex titled: "Penny Elimination and Inflation"
# This code uses the 2024 translation-level public data

# Packages used
library(ggplot2); theme_set(theme_bw())# for graphics
#library(mfx)# binomial logit marginal effects
#library(stargazer) # for displaying multinomial coefficients. Does not work with mfx
#library(texreg) # for displaying multinomial coefficients. Works with mfx (unlike stargazer). Also displayes multiple regression.
#library(huxtable)#displays multiple regressions as table => advantage, since the table can be edited in R => Problem: built-in to_latex output does not run in LaTeX in my experience. 
#library(nnet)# multinomial regressions
#library(haven)# may be needed to handle haven labelled when data is converted from other software e.g. Stata
library("xtable") #exporting to LaTeX
library(dplyr)# for sample_n
#library(gtools)# for stars.pval function

#setwd("C:/Oz_local_workspace_1/penny_R")
setwd("~/Presentations/SPR_Chicago_2025_7")
dir()
#
ind_1.df = readRDS("dcpc-2024-indlevel-public.rds")
dim(ind_1.df)
#
table(ind_1.df$cc_rewards)
sum(is.na(ind_1.df$cc_rewards))# missing
# deleting NAs for rewards
ind_2.df = subset(ind_1.df, !is.na(ind_1.df$cc_rewards))
dim(ind_2.df)

table(ind_2.df$hhincome)# income bins
sum(is.na(ind_2.df$hhincom))
# delete NA for hh income
ind_3.df = subset(ind_2.df, !is.na(ind_2.df$hhincome))
dim(ind_3.df)

# start bar graph of reward on versus income
(reward.df = data.frame(ind_3.df %>% group_by(hhincome) %>% summarise(mean(cc_rewards))))
#
ggplot(reward.df, aes(x=hhincome, y=mean.cc_rewards.)) +geom_bar(stat = "identity",  color="black", fill="cyan") +scale_y_continuous(breaks = seq(0, 1.0, 0.1), labels = scales::percent_format(accuracy = 1)) +labs(y="Card holders with rewards (%)", x= "Household income (16 income groups)")  +theme(axis.text.x = element_text(size = 14, color = "black"),  axis.text.y = element_text(size = 16, color = "black"), text = element_text(size = 20)) +geom_text(aes(label = scales::percent(mean.cc_rewards., accuracy=1), vjust =-0.5), size = 5) +scale_x_continuous(breaks = 1:16)


#FICO score
table(ind_3.df$ph006)
# delete category 7
ind_4.df = subset(ind_3.df, ph006 < 7)
table(ind_4.df$ph006)
# fraction of reward cards as function of FICO score
(reward2.df = data.frame(ind_4.df %>% group_by(ph006) %>% summarise(mean(cc_rewards))))

# FICO by income
#
(income.df = data.frame(ind_4.df %>% group_by(hhincome) %>% summarise(mean(ph006))))
