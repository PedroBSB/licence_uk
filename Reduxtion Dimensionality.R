#Read the dataset
library(readxl)
library(tidyverse)
library(factoextra)
library(corrplot)
license.df<-read_xlsx("Data/upload_date.xlsx",1)
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

freq2<-freq[,-1]
rownames(freq2)<-freq[,1]
res.pca <- prcomp(freq2, scale = TRUE, center=TRUE)


fviz_eig(res.pca, addlabels = TRUE)

#Cor plot
var <- get_pca_var(res.pca)
corrplot(var$cos2, is.corr = FALSE)

#Quality
fviz_cos2(res.pca, choice = "var", axes=1)
fviz_cos2(res.pca, choice = "var", axes=2)
fviz_cos2(res.pca, choice = "var", axes=3)
fviz_cos2(res.pca, choice = "var", axes=4)

# Contributions of variables to PC2
fviz_contrib(res.pca, choice = "var", axes = 1)
fviz_contrib(res.pca, choice = "var", axes = 2)
fviz_contrib(res.pca, choice = "var", axes = 3)
fviz_contrib(res.pca, choice = "var", axes = 4)

#Variables
fviz_pca_var(res.pca, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))


#Cluster
res.km <- kmeans(var$coord, centers = 3, nstart = 25)
grp <- as.factor(res.km$cluster)
# Color variables by groups
fviz_pca_var(res.pca, col.var = grp, 
             palette = c("#0073C2FF", "#EFC000FF", "#868686FF"),
             legend.title = "Cluster")

