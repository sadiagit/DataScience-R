options(digits = 3)    # report 3 significant digits
library(tidyverse)
library(titanic)
library(ggplot2)

titanic <- titanic_train %>%
  select(Survived, Pclass, Sex, Age, SibSp, Parch, Fare) %>%
  mutate(Survived = factor(Survived),
         Pclass = factor(Pclass),
         Sex = factor(Sex))

#general overview


# count distribution of each group
titanic %>% filter(!is.na(Age)) %>%
   ggplot(aes(Age, y= ..count.., fill=Sex))+
   geom_density(alpha =0.2)+
  facet_grid(Sex ~ .)

# comparing age 18-35 for both group. Male has high proportion
titanic %>% filter(!is.na(Age) & Age >= 18 & Age <= 35  ) %>%
  mutate(group= ifelse (Sex %in%'male', "Male", "Female")) %>%
  ggplot(aes(Age,fill=group, position='stack'))+
  geom_density(alpha=0.2)


# comparing age < 17 for both group. female has high proportion
titanic %>% filter(!is.na(Age) & Age < 17  ) %>%
  mutate(group= ifelse (Sex %in%'male', "Male", "Female")) %>%
  ggplot(aes(Age,fill=group))+
  geom_density(alpha =0.2)

# comparing age = 40 (39-41) for both group. male has high proportion
titanic %>% filter(!is.na(Age) & Age %in% seq(39,41)) %>%
  mutate(group= ifelse (Sex %in%'male', "Male", "Female")) %>%
  ggplot(aes(Age, y= ..count.., fill=group))+
  geom_density(alpha=0.2)



titanic %>% filter(!is.na(Age) & Age > 60 ) %>%
  mutate(group= ifelse (Sex %in%'male', "Male", "Female")) %>%
  ggplot(aes(Age,fill=group))+
  geom_density(alpha=0.2)+
  facet_grid(group ~ .)

 params <- titanic %>%
   filter(!is.na(Age)) %>%
   summarize(mean = mean(Age), sd = sd(Age))

 titanic %>% filter(!is.na(Age)) %>% ggplot(aes(sample=Age))+geom_qq(dparams = params)+geom_abline()
 titanic %>% ggplot(aes(x=Sex, fill=Survived))+geom_bar(position = position_dodge())

 titanic %>% filter(!is.na(Age)) %>% ggplot(aes(x=Age, y=..count.., fill=Survived))+geom_density(alpha=0.2)

 titanic %>% filter(Fare > 0) %>% group_by(Survived)%>%ggplot(aes(x=Survived, y=Fare))+geom_boxplot()+
   scale_y_continuous(trans="log2")+geom_jitter(width = 0.1, alpha = 0.2)

 titanic %>% ggplot(aes(x=Pclass, fill=Survived))+geom_bar()
 titanic %>% ggplot(aes(x=Pclass, fill=Survived))+geom_bar(position = position_fill())
 titanic %>% ggplot(aes(x=Survived, fill=Pclass))+geom_bar(position = position_fill())
 titanic %>% filter(!is.na(Age)) %>% ggplot(aes(x=Age, y=..count.., fill=Survived))+
            geom_density(alpha=0.2)+
            facet_grid(Sex ~ Pclass)


