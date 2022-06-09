#### Install packages ####
install.packages("lmerTest")
install.packages("tidyverse")
install.packages("irr")
install.packages("stats")
install.packages("ggplot2")

#### Load packages: ####
library(lmerTest)
library(tidyverse)
library(irr)
library(stats)
library(ggplot2)

citation("stats")

#### Read data files ###
BasicInfo<- read.csv("SLP Participants.csv")

#### Code categorical variables as factors ####
BasicInfo$Gender <- as.factor(BasicInfo$Gender)
BasicInfo$Education <- as.factor(BasicInfo$Education)
BasicInfo$Major <- as.factor(BasicInfo$Major)

#### Hand-coding contrast coding for the factors: Gender
BasicInfo$Gender <- ifelse(BasicInfo$Gender == "Female", -0.5, 0.5)

#### Descriptive statistics ###
Gender1 <- table(BasicInfo$Gender)
Gender1

summarise(BasicInfo, Max = max (Age), Min = min (Age), 
          M = mean(Age), SD = sd(Age))

Age <- table(BasicInfo$Age)
Age

BasicInfo%>% group_by(Education) %>%
summarise(count = n())

BasicInfo%>% group_by(Major) %>%
  summarise(count = n())

summarise(BasicInfo, Max = max (IELTS), Min = min (IELTS), 
          M = mean(IELTS), SD = sd(IELTS))

BasicInfo%>% group_by(IELTS) %>%
  summarise(count = n())

sum(BasicInfo$ac)
sum(BasicInfo$ae)
sum(BasicInfo$ag)
sum(BasicInfo$ai)
sum(BasicInfo$ak)
sum(BasicInfo$bc)
sum(BasicInfo$be)
sum(BasicInfo$bg)
sum(BasicInfo$bi)
sum(BasicInfo$bk)

## Datasheet preparation:
# Mean score for each of the five aspects
BasicInfo <- BasicInfo %>% 
  mutate(MG = (G1 + G2) / 2)

BasicInfo <- BasicInfo %>% 
  mutate(MD = (D1 + D2) / 2)

BasicInfo <- BasicInfo %>% 
  mutate(MP = (P1 + P2) / 2)

BasicInfo <- BasicInfo %>% 
  mutate(MI = (I1 + I2) / 2)

BasicInfo <- BasicInfo %>% 
  mutate(MA = (A1 + A2) / 2)

# Total score given by each rater:
BasicInfo <- BasicInfo %>% 
  mutate(Rater1 = G1 + D1 + P1 + I1 + A1)

BasicInfo <- BasicInfo %>% 
  mutate(Rater2 = G2 + D2 + P2 + I2 + A2)

# Total BEC test score:
BasicInfo <- BasicInfo %>% 
  mutate(Testing = (Rater1 + Rater2) / 2)

BasicInfo <- BasicInfo %>% 
  mutate(Testing3 = (D1 + I1 + A1)/2 + (D2 + I2 + A2)/2)

summarise(BasicInfo, MG1 = mean(G1), SDG1 = sd(G1), 
          MG2 = mean(G2), SDG2 = sd(G2))

summarise(BasicInfo, MD1 = mean(D1), SDD1 = sd(D1), 
          MD2 = mean(D2), SDD2 = sd(D2))

summarise(BasicInfo, MP1 = mean(P1), SDP1 = sd(P1), 
          MP2 = mean(P2), SDP2 = sd(P2))

summarise(BasicInfo, MI1 = mean(I1), SDI1 = sd(I1), 
          MI2 = mean(I2), SDI2 = sd(I2))

summarise(BasicInfo, MA1 = mean(A1), SDA1 = sd(A1), 
          MA2 = mean(A2), SDA2 = sd(A2))

summarise(BasicInfo, M1 = mean(Rater1), SD1 = sd(Rater1), 
          M2 = mean(Rater2), SD2 = sd(Rater2))

# Total Active Listening amount
BasicInfo <- BasicInfo %>% 
  mutate(Total.AL = ac + ae + ag + ai + ak + bc + be + bg + bi + bk)

### RQ1 - Does active listening ability impact the BEC speaking test score?
# Model1: Total speaking test score Total active listening amount ~ 
Model1 <- lm(formula = Testing ~ Total.AL, data = BasicInfo)
  summary(Model1)
# Results: Total BEC speaking score = 14.672 + 0.217*Total AL amount +- 0.041
  # Adjusted R-squared = 0.408, F = 27.88, t = 5.28, p < 0.001
  
# Check the normal distribution of Model 1
#### histogram of residuals
  hist(residuals(Model1))
  
#### QQ-plot of residuals
  qqnorm(residuals(Model1))
  qqline(residuals(Model1))
  
# plot1 for Model1:
plot1 <- ggplot(BasicInfo, aes(x = Total.AL, y = Testing)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "red") 
plot1 + labs(x = "Total Amount of Active Listening Tokens", 
             y = "Total BEC Speaking Test Score")




# Model2: Total active listening amount ~ DM + IC + AL
Model2 <- lm(formula = Testing3 ~ Total.AL, data = BasicInfo)
  summary(Model2)
  # Results: Total BEC speaking score = 8.280 + 0.151*Total AL amount +- 0.028
  # Adjusted R-squared = 0.416, F = 28.79, p < 0.001
  
# Check the normal distribution of Model 2
#### histogram of residuals
  hist(residuals(Model2))
  
#### QQ-plot of residuals
  qqnorm(residuals(Model2))
  qqline(residuals(Model2))
  
# plot2 for Model2:
plot2 <- ggplot(BasicInfo, aes(x = Total.AL, y = Testing3)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "red") 
plot2 + labs(x = "Total Amount of Active Listening Tokens", 
             y = "Total Score of Three Aspects in Relation to Pragmatics")
  
# Model3: Total active listening amount ~ each aspect of BEC rubric
Model3 <- lm(formula = Total.AL ~ MD + MI + MA, data = BasicInfo)
summary(Model3)
# Results: no aspect is significant

colnames(BasicInfo)[11] <- "Transitional Backchannels"
colnames(BasicInfo)[12] <- "Transitional Reactive Expressions"
colnames(BasicInfo)[13] <- "Transitional Collaborative Finishes"
colnames(BasicInfo)[14] <- "Transitional Repetitions"
colnames(BasicInfo)[15] <- "Transitional Resumptive Openers"
colnames(BasicInfo)[16] <- "Non-transitional Backchannels"
colnames(BasicInfo)[17] <- "Non-transitional Reactive Expressions"
colnames(BasicInfo)[18] <- "Non-transitional Collaborative Finishes"
colnames(BasicInfo)[19] <- "Non-transitional Repetitions"
colnames(BasicInfo)[20] <- "Non-transitional Resumptive Openers"

# Model4: Total BEC testing score ~ each type of AL (to create graphs)
Model4 <- lm(formula = Testing ~ ac + ae + ag + ai + ak + bc + be + bg + bi + bk, 
               data = BasicInfo)
summary(Model4)
# Results: only [be] is significant, R-squared = 0.385, F = 3.442, = p = 0.004
  
# Check the normal distribution of Model 4
#### histogram of residuals
  hist(residuals(Model4))
  
#### QQ-plot of residuals
  qqnorm(residuals(Model4))
  qqline(residuals(Model4))
  
# plot4 for Model4:
library(car)
  
Graph10<- read.csv("SLP Participants1.csv")

Graph10 <- Graph10 %>% 
  mutate(Rater1 = G1 + D1 + P1 + I1 + A1)

Graph10 <- Graph10 %>% 
  mutate(Rater2 = G2 + D2 + P2 + I2 + A2)

Graph10 <- Graph10 %>% 
  mutate(Testing = (Rater1 + Rater2) / 2)

Model4.1 <- lm(formula = Testing ~ Transitional_Backchannels + 
                 Transitional_Reactive_Expressions +
                 Transitional_Collaborative_Finishes + 
                 Transitional_Repetitions +
                 Transitional_Resumptive_Openers +
                 Nontransitional_Backchannels +
                 Nontransitional_Reactive_Expressions +
                 Nontransitional_Collaborative_Finishes +
                 Nontransitional_Repetitions +
                 Nontransitional_Resumptive_Openers, 
               data = Graph10)
summary(Model4.1)

avPlots(Model4.1)

# Model5: Total BEC testing score ~ each type of AL
Model5 <- lm(formula = MA ~ ac + ae + ag + ai + ak + bc + be + bg + bi + bk, 
             data = BasicInfo)
summary(Model5)
# Results: only [be] is significant, R-squared = 0.385, F = 3.442, = p = 0.004


# Model6: Active listening ~ Each rater's rating on three aspects
Model6 <- lm(formula = Total.AL ~ D1 + I1 + A1, data = BasicInfo)
summary(Model6)

Model7 <- lm(formula = Total.AL ~ D2 + I2 + A2, data = BasicInfo)
summary(Model7)

# Change the title in the graphs:
Graph6<- read.csv("SLP Participants2.csv")

Graph6 <- Graph6 %>% 
  mutate(Total.AL = ac + ae + ag + ai + ak + bc + be + bg + bi + bk)

# Model6.1: Active listening ~ Each rater's rating on three aspects
Model6.1 <- lm(formula = Total.AL ~ Rater1_Discourse_Management +
               Rater1_Interactive_Communication + Rater1_Active_Listening,
               data = Graph6)
summary(Model6.1)

avPlots(Model6.1)

Model7.1 <- lm(formula = Total.AL ~ Rater2_Discourse_Management +
               Rater2_Interactive_Communication + Rater2_Active_Listening, 
               data = Graph6)
summary(Model7.1)

avPlots(Model7.1)