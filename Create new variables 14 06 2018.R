#Read the dataset
library(readxl)
library(tidyverse)
library(factoextra)
library(corrplot)
library(foreign)
library(psych)
library(pander)
library(xtable)
library(xtable)
library(MASS)
library(knitr)
library(pscl)
library(stargazer)
library(tidyverse)
license.df<-na.omit(read_xlsx("Data/upload_date.xlsx",1))
#Create the factors
factor.df<-na.omit(read.csv("Data\\AggreatedData3.csv"))
factor.df$copyleft<-factor.df[,33]+factor.df[,39]+factor.df[,40]+factor.df[,38]+factor.df[,41]
factor.df$anticompetition<-factor.df[,32]+factor.df[,34]+factor.df[,36]
factor.df$anti_social_rivarly<-factor.df[,31]+factor.df[,35]
factor.df$collective_rivalry<-factor.df[,37]-factor.df[,30]

count.df <- factor.df %>% 
            group_by(thing_author_id) %>% 
            summarise(dummy_period=max(key.event))

factor.df <- left_join(factor.df,count.df,by="thing_author_id")
write.csv(factor.df,"Data\\AggreatedData4.csv")
