# checking fraction of transaction amounts that were rounded to 0 or 5
# This code uses the 2023 translation-level public data
setwd("~/Research/SCPC_DCPC/2023_SDCPC")
dir()
#
trans1.df = readRDS("dcpc_2023_tranlevel_public_rds.rds")
dim(trans1.df)
#
table(trans1.df$in_person)
#
trans2.df = subset(trans1.df, in_person==1)# in-person only
dim(trans2.df)
#
table(trans2.df$pi)
trans3.df = subset(trans2.df, pi==1)# cash only
dim(trans3.df)

#amount
str(trans3.df)
head(trans3.df$amnt, 100)

#create a vector of the last 2 digits (decimals)
amnt.vec = trans3.df$amnt
length((amnt.vec))
# 
decimal.vec = 100*(amnt.vec %% 1)
head(decimal.vec, 100)

#count how many payments end with 0 pennies
(count0 = sum(decimal.vec == 0))# 

#count how many payments end with 5 pennies
(count5 = sum(decimal.vec %% 5 == 0 & decimal.vec != 0))# 

#cross-verify, how many end with 0 or 5
(count05 =  sum(decimal.vec %% 5 == 0))
count0 + count5

# express as percentages
(percent0 = round(100*count0/length(decimal.vec)))
#
(percent5 = round(100*count5/length(decimal.vec)))
#
(percent05 = round(100*count05/length(decimal.vec)))
 