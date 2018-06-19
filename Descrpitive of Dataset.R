license.df<-na.omit(read_xlsx("Data/upload_date.xlsx",1))
#Total number of contributors:
length(unique(license.df$thing_author_id))

#the total number of artefacts


#the total number of contributors between the first date and the last
#date of the period of observations
factor.df<-na.omit(read.csv("Data\\AggreatedData3.csv"))
count.df <- factor.df %>% 
  group_by(thing_author_id) %>% 
  summarise(dummy_period=max(key.event))

sel<-nrow(unique(license.df[which(license.df$upload_date>="2012-09-24" & license.df$upload_date<="2013-11-05"),"thing_author_id"]))


#then the total number of contributors who only contributed in period 1, 2 and 3
library(tidyverse)
license.df<-license.df %>% 
  mutate(key.event=ifelse(upload_date<"2012-09-24","1",
                          ifelse(upload_date<"2013-11-05","2","3")))

sel1<-nrow(unique(license.df[which(license.df$key.event==1),"thing_author_id"]))
sel2<-nrow(unique(license.df[which(license.df$key.event==2),"thing_author_id"]))
sel3<-nrow(unique(license.df[which(license.df$key.event==3),"thing_author_id"]))


