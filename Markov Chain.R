#Read the dataset
library(readxl)
library(tidyverse)
library(markovchain)
library(cluster)
library(NbClust)
library(igraph)
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

temp<- license.df %>% 
       select(thing_author_id,license, upload_date) %>% 
       arrange(thing_author_id, upload_date) %>% 
       group_by(thing_author_id) %>%
       mutate(license2 = dplyr::lead(license, n = 1, default = NA)) 
temp<-na.omit(temp)
transMat<-as.matrix(table(temp$license,temp$license2))
transCount<-transMat
transCount<-as.matrix(transCount)
attributes(transCount)$class <- "matrix"
write.csv(transCount,"transCount.csv")
#Normalize row
for(i in 1:nrow(transMat)) transMat[i,]<-transMat[i,]/sum(transMat[i,])
transMat<-as.matrix(transMat)
attributes(transMat)$class <- "matrix"

#Markov Chain
names<-c('All Rights Reserved','Attribution - Non-Commercial - No Derivatives','Attribution - Non-Commercial - Share Alike','BSD License','Creative Commons - Attribution','Creative Commons - Attribution - No Derivatives','Creative Commons - Attribution - Non-Commercial','Creative Commons - Attribution - Share Alike','Creative Commons - Public Domain Dedication','GNU - GPL','GNU - LGPL','Public Domain')
dtmcA <- new("markovchain",transitionMatrix=transMat,
             states=names,
             name="All Times")
#First Plot
plot(dtmcA)

#Get the steady states
t(steadyStates(dtmcA))
summary(dtmcA)

#Distance
d_dist <- 1/as.matrix(transMat)
d_dist[is.infinite(d_dist)]<-1e+10
d_dist<-as.dist(d_dist)
# Hierarchical clustering results
hc <- hclust(d_dist, method = "complete")
# Visualization of hclust
plot(hc)

# Add rectangle around 3 groups
rect.hclust(hc, k = 3, border = 2:4) 


#Create the graph nodes
graph_o <- graph_from_adjacency_matrix(as.matrix(transMat), mode = "directed", weighted =T)

#Find communities spinglass.community walktrap.community
fc <- spinglass.community(graph_o)

#Plota as comunidades:
plot(fc, graph_o)



###################  By Key Event (Key Event 1:)


temp1<- license.df %>% 
  filter(upload_date<"2012-09-24") %>% 
  select(thing_author_id,license, upload_date) %>% 
  arrange(thing_author_id, upload_date) %>% 
  group_by(thing_author_id) %>%
  mutate(license2 = dplyr::lead(license, n = 1, default = NA)) 
temp1<-na.omit(temp1)
transMat1<-as.matrix(table(temp1$license,temp1$license2))
transCount1<-transMat1
transCount1<-as.matrix(transCount1)
attributes(transCount1)$class <- "matrix"
write.csv(transCount1,"transCount1.csv")
#Normalize row
for(i in 1:nrow(transMat1)) transMat1[i,]<-transMat1[i,]/sum(transMat1[i,])
transMat1<-as.matrix(transMat1)
attributes(transMat1)$class <- "matrix"

#Markov Chain
names<-c('All Rights Reserved','Attribution - Non-Commercial - No Derivatives','Attribution - Non-Commercial - Share Alike','BSD License','Creative Commons - Attribution','Creative Commons - Attribution - No Derivatives','Creative Commons - Attribution - Non-Commercial','Creative Commons - Attribution - Share Alike','Creative Commons - Public Domain Dedication','GNU - GPL','GNU - LGPL','Public Domain')
dtmcB <- new("markovchain",transitionMatrix=transMat1,
             states=names,
             name="Key Event 1")
#First Plot
plot(dtmcB)

#Get the steady states
t(steadyStates(dtmcB))
summary(dtmcB)

#Distance
d_dist1 <- 1/as.matrix(transMat1)
d_dist1[is.infinite(d_dist1)]<-1e+10
d_dist1<-as.dist(d_dist1)
# Hierarchical clustering results
hc1 <- hclust(d_dist1, method = "complete")
# Visualization of hclust
plot(hc1)

# Add rectangle around 3 groups
rect.hclust(hc1, k = 4, border = 2:4) 

#Create the graph nodes
graph_o1 <- graph_from_adjacency_matrix(as.matrix(transMat1), mode = "directed", weighted =T)

#Find communities spinglass.community walktrap.community
fc1 <- spinglass.community(graph_o1)

#Plota as comunidades:
plot(fc1, graph_o1)

###################  By Key Event (Key Event 2:)

temp2<- license.df %>% 
  filter(upload_date>="2012-09-24" & upload_date<"2013-11-05") %>% 
  select(thing_author_id,license, upload_date) %>% 
  arrange(thing_author_id, upload_date) %>% 
  group_by(thing_author_id) %>%
  mutate(license2 = dplyr::lead(license, n = 1, default = NA)) 
temp2<-na.omit(temp2)
transMat2<-as.matrix(table(temp2$license,temp2$license2))
transCount2<-transMat2
transCount2<-as.matrix(transCount2)
write.csv(transCount2,"transCount2.csv")
attributes(transCount2)$class <- "matrix"

#Normalize row
for(i in 1:nrow(transMat2)) transMat2[i,]<-transMat2[i,]/sum(transMat2[i,])
transMat2<-as.matrix(transMat2)
attributes(transMat2)$class <- "matrix"

#Markov Chain (without 'All Rights Reserved')
names2<-c('Attribution - Non-Commercial - No Derivatives','Attribution - Non-Commercial - Share Alike','BSD License','Creative Commons - Attribution','Creative Commons - Attribution - No Derivatives','Creative Commons - Attribution - Non-Commercial','Creative Commons - Attribution - Share Alike','Creative Commons - Public Domain Dedication','GNU - GPL','GNU - LGPL','Public Domain')
dtmcC <- new("markovchain",transitionMatrix=transMat2,
             states=names2,
             name="Key Event 2")
#First Plot
plot(dtmcC)

#Get the steady states
t(steadyStates(dtmcC))
summary(dtmcC)

#Distance
d_dist2 <- 1/as.matrix(transMat2)
d_dist2[is.infinite(d_dist2)]<-1e+10
d_dist2<-as.dist(d_dist2)
# Hierarchical clustering results
hc2 <- hclust(d_dist2, method = "complete")
# Visualization of hclust
plot(hc2)

# Add rectangle around 4 groups
rect.hclust(hc2, k = 4, border = 2:4) 

#Create the graph nodes
graph_o2 <- graph_from_adjacency_matrix(as.matrix(transMat2), mode = "directed", weighted =T)

#Find communities spinglass.community walktrap.community
fc2 <- spinglass.community(graph_o2)

#Plota as comunidades:
plot(fc2, graph_o2)

###################  By Key Event (Key Event 3:)

temp3<- license.df %>% 
  filter(upload_date>="2013-11-05") %>% 
  select(thing_author_id,license, upload_date) %>% 
  arrange(thing_author_id, upload_date) %>% 
  group_by(thing_author_id) %>%
  mutate(license3 = dplyr::lead(license, n = 1, default = NA)) 
temp3<-na.omit(temp3)
transMat3<-as.matrix(table(temp3$license,temp3$license3))
transCount3<-transMat3
transCount3<-as.matrix(transCount3)
write.csv(transCount3,"transCount3.csv")
attributes(transCount3)$class <- "matrix"

#Normalize row
for(i in 1:nrow(transMat3)) transMat3[i,]<-transMat3[i,]/sum(transMat3[i,])
transMat3<-as.matrix(transMat3)
attributes(transMat3)$class <- "matrix"

#Markov Chain 
dtmcD <- new("markovchain",transitionMatrix=transMat3,
             states=names,
             name="Key Event 3")
#First Plot
plot(dtmcD)

#Get the steady states
t(steadyStates(dtmcD))
summary(dtmcD)

#Distance
d_dist3 <- 1/as.matrix(transMat3)
d_dist3[is.infinite(d_dist3)]<-1e+10
d_dist3<-as.dist(d_dist3)
# Hierarchical clustering results
hc3 <- hclust(d_dist3, method = "complete")
# Visualization of hclust
plot(hc3)

# Add rectangle around 4 groups
rect.hclust(hc1, k = 4, border = 3:4) 

#Create the graph nodes
graph_o3 <- graph_from_adjacency_matrix(as.matrix(transMat3), mode = "directed", weighted =T)

#Find communities spinglass.community walktrap.community
fc3 <- spinglass.community(graph_o3)

#Plota as comunidades:
plot(fc3, graph_o3)
