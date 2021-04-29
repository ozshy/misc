# count hours in a specific aircraft
library(dplyr)
setwd("~/Flying/Logbooks")
dir()
lb1.df = read.csv("Copy of Oz-online-Logbook-2021_4_24.csv")
names(lb1.df)
str(lb1.df)
lb2.df = subset(lb1.df, select=c(Airplane, Total.Airplane))
head(lb2.df)
str(lb2.df)
#
lb3.df = lb2.df[grep("112", lb2.df$Airplane), ]
dim(lb3.df)
lb3.df
(total_tomahawk = sum(lb3.df$Total.Airplane))
#
# do Warrier 
lb4.df = lb2.df[grep("161", lb2.df$Airplane), ]
dim(lb4.df)
lb4.df
(total_warrier = sum(lb4.df$Total.Airplane))
