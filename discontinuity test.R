#Pedro will carry out the discontinuity test re the key events (as suggested by the reviewer#2). 
#I've given him the attached as re the sequence of minor/major external events, 
#(which signified the progression of Makerbot's proprietary strategy from benign to aggressive)
#With the discontinuity test, this will give us some indication of whether the influence and benefits of PC 
#on design innovation will still hold

library(MatchIt)
library(tidyverse)
license.df<-read.csv("Data/AggreatedData.csv")
license.df<-na.omit(license.df)
license.df$Dummy1<-ifelse(license.df$key.event==1,1,0)
license.df$Dummy2<-ifelse(license.df$key.event==2,1,0)
license.df$Dummy3<-ifelse(license.df$key.event==3,1,0)

#Diferences in differences (Key Event 1)
license.df %>%
  group_by(Dummy1) %>%
  summarise(count = n(),
            mean = mean(out),
            std_error = sd(out) / sqrt(count))
license.df %>%
  mutate(test = (out - mean(out)) / sd(out)) %>% 
  group_by(Dummy1) %>%
  summarise(mean = mean(test))
with(license.df, t.test(out ~ Dummy1))


#Diferences in differences (Key Event 2)
license.df %>%
  group_by(Dummy2) %>%
  summarise(count = n(),
            mean = mean(out),
            std_error = sd(out) / sqrt(count))
license.df %>%
  mutate(test = (out - mean(out)) / sd(out)) %>% 
  group_by(Dummy2) %>%
  summarise(mean = mean(test))
with(license.df, t.test(out ~ Dummy2))

#Diferences in differences (Key Event 2)
license.df %>%
  group_by(Dummy3) %>%
  summarise(count = n(),
            mean = mean(out),
            std_error = sd(out) / sqrt(count))
license.df %>%
  mutate(test = (out - mean(out)) / sd(out)) %>% 
  group_by(Dummy3) %>%
  summarise(mean = mean(test))
with(license.df, t.test(out ~ Dummy3))


#Propensity Score (Key Event 1)
m_ps1 <- glm(Dummy1 ~ files_count+ thing_like_count+collection_count,
            family = binomial(), data = license.df)
summary(m_ps1)
prs_df <- data.frame(pr_score = predict(m_ps1, type = "response"),
                     Dummy1 = m_ps1$model$Dummy1)
labs <- paste("Key event:", c("1", "Other"))
prs_df %>%
  mutate(Dummy1 = ifelse(Dummy1 == 1, labs[1], labs[2])) %>%
  ggplot(aes(x = pr_score)) +
  geom_histogram(color = "white") +
  facet_wrap(~Dummy1) +
  xlab("Probability of Key Event 1") +
  theme_bw()


#Propensity Score (Key Event 2)
m_ps2 <- glm(Dummy2 ~ files_count+ thing_like_count+collection_count,
            family = binomial(), data = license.df)
summary(m_ps2)
prs_df <- data.frame(pr_score = predict(m_ps2, type = "response"),
                     Dummy2 = m_ps2$model$Dummy2)
labs <- paste("Key event:", c("1", "Other"))
prs_df %>%
  mutate(Dummy2 = ifelse(Dummy2 == 1, labs[1], labs[2])) %>%
  ggplot(aes(x = pr_score)) +
  geom_histogram(color = "white") +
  facet_wrap(~Dummy2) +
  xlab("Probability of Key Event 2") +
  theme_bw()


#Propensity Score (Key Event 3)
m_ps3 <- glm(Dummy3 ~ files_count+ thing_like_count+collection_count,
            family = binomial(), data = license.df)
summary(m_ps3)
prs_df <- data.frame(pr_score = predict(m_ps3, type = "response"),
                     Dummy3 = m_ps3$model$Dummy3)
labs <- paste("Key event:", c("1", "Other"))
prs_df %>%
  mutate(Dummy2 = ifelse(Dummy3 == 1, labs[1], labs[2])) %>%
  ggplot(aes(x = pr_score)) +
  geom_histogram(color = "white") +
  facet_wrap(~Dummy3) +
  xlab("Probability of Key Event 2") +
  theme_bw()

#Matching Algorithm (Key Event 1)
mod_match <- matchit(Dummy1 ~ files_count+ thing_like_count+collection_count,
                     method = "nearest", data = license.df)
dta_m <- match.data(mod_match)
with(dta_m, t.test(out ~ Dummy1))

#Matching Algorithm (Key Event 2)
mod_match <- matchit(Dummy2 ~ files_count+ thing_like_count+collection_count,
                     method = "nearest", data = license.df)
dta_m <- match.data(mod_match)
with(dta_m, t.test(out ~ Dummy2))

#Matching Algorithm (Key Event 3)
mod_match <- matchit(Dummy3 ~ files_count+ thing_like_count+collection_count,
                     method = "nearest", data = license.df)
dta_m <- match.data(mod_match)
with(dta_m, t.test(out ~ Dummy3))
