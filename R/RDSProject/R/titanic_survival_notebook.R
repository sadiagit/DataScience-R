---
title: "R Notebook"
output: html_notebook
---

This is an [R Markdown](http://rmarkdown.rstudio.com) Notebook. When you execute code within the notebook, the results appear beneath the code. 

Try executing this chunk by clicking the *Run* button within the chunk or by placing your cursor inside it and pressing *Ctrl+Shift+Enter*. 

```{r}
plot(cars)
```

Add a new chunk by clicking the *Insert Chunk* button on the toolbar or by pressing *Ctrl+Alt+I*.

When you save the notebook, an HTML file containing the code and output will be saved alongside it (click the *Preview* button or press *Ctrl+Shift+K* to preview the HTML file).

The preview shows you a rendered HTML copy of the contents of the editor. Consequently, unlike *Knit*, *Preview* does not run any R code chunks. Instead, the output of the chunk when it was last run in the editor is displayed.
```{r}
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
   geom_histogram()+
     facet_grid(group ~ .)
```

