#Read the dataset
library(readxl)
library(tidyverse)
library(factoextra)
library(corrplot)
library(foreign)
license.df<-read_xlsx("Data/upload_date.xlsx",1)
write.dta(license.df, file="upload.dta")

#Create the factors
license.df$license<-as.factor(license.df$license)
license.df$Type <-as.factor(license.df$Type )
#Call the mlogit library
license.df<-na.omit(license.df)

#List all liceses
license.df<- license.df %>% 
  filter(as.character(license)!="Nokia")
license.df$license<-as.character(license.df$license)


#Count by contributor
freq <- license.df %>% 
  group_by(thing_author_id, license) %>% 
  tally() %>% 
  spread(key = license, value = n) %>% 
  replace(is.na(.), 0)
write.dta(freq, file="PCA.dta")

#Read the dataset
license.df<-read.csv("Data/AggreatedData.csv")
write.dta(license.df, file="AggreatedData.dta")
