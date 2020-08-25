# Case-control studies help determine whether certain exposures are associated with outcomes such as developing cancer. The built-in dataset esoph contains data from a case-control study in France comparing people with esophageal cancer (cases, counted in ncases) to people without esophageal cancer (controls, counted in ncontrols) that are carefully matched on a variety of demographic and medical characteristics. The study compares alcohol intake in grams per day (alcgp) and tobacco intake in grams per day (tobgp) across cases and controls grouped by age range (agegp).
# The data set is available in base R and can be called with the variable name esoph:

library(tidyverse)
library(dplyr)

# Q1. How many groups are in the study? How mane cases and controls?
esoph %>% group_by(agegp, alcgp,tobgp)

all_cases <- sum(esoph$ncases)
all_controls <- sum(esoph$ncontrols)
all_controls

#finding the highest alcohol consumption group
esoph %>%
  arrange(desc(alcgp))

# Q2 What is the probability that a subject in the highest alcohol consumption group is a cancer case?

esoph %>%
      select(agegp, alcgp,ncases, ncontrols) %>%
      filter(alcgp == '120+') %>%
      summarize( cases = sum(ncases), total = sum(ncases)+sum(ncontrols), pr = cases/total )

# Q3. What is the probability that a subject in the lowest alcohol consumption group is a cancer case?

#finding the lowest alcohol consumption group
esoph %>%
  arrange(alcgp)

esoph %>%
  select(agegp, alcgp,ncases, ncontrols) %>%
  filter(alcgp == '0-39g/day') %>%
  summarize( cases = sum(ncases), total = sum(ncases)+sum(ncontrols), pr = cases/total )


# Q4. Given that a person is a case, what is the probability that they smoke 10g or more a day?

#Order by tobacco consumption
esoph %>%
  arrange(tobgp)
esoph %>%
  select(agegp, alcgp, tobgp,ncases, ncontrols) %>%
  filter(tobgp != '0-9g/day') %>%
  summarize( tob_cases = sum(ncases), pr = tob_cases/all_cases )

# Q5. Given that a person is a control, what is the probability that they smoke 10g or more a day?

#Order by tobacco consumption
esoph %>%
  arrange(tobgp)
esoph %>%
  select(agegp, alcgp, tobgp,ncases, ncontrols) %>%
  filter(tobgp != '0-9g/day') %>%
  summarize( tob_controls = sum(ncontrols), pr = tob_controls/all_controls )


# Q6. For cases, what is the probability of being in the highest alcohol group?

esoph %>%
  select(agegp, alcgp, tobgp,ncases, ncontrols) %>%
  filter(alcgp == '120+') %>%
  summarize( h_algp = sum(ncases), pr = h_algp/all_cases )

# Q7. For cases, what is the probability of being in the highest tobacco group?
esoph %>%
  arrange(desc(tobgp))
esoph %>%
  select(agegp, alcgp, tobgp,ncases, ncontrols) %>%
  filter(tobgp == '30+') %>%
  summarize( tob_cases = sum(ncases), pr = tob_cases/all_cases )

# Q8. For cases, what is the probability of being in the highest alcohol group and the highest tobacco group?
esoph %>%
  select(agegp, alcgp, tobgp,ncases, ncontrols) %>%
  filter(tobgp == '30+' & alcgp == '120+' ) %>%
  summarize( h_al_and_tob_cases = sum(ncases), pr = h_al_and_tob_cases/all_cases )

# Q9. For cases, what is the probability of being in the highest alcohol group or the highest tobacco group?
esoph %>%
  select(agegp, alcgp, tobgp,ncases, ncontrols) %>%
  filter(tobgp == '30+' | alcgp == '120+' ) %>%
  summarize( h_al_or_tob_cases = sum(ncases), pr = h_al_or_tob_cases/all_cases )

#Q10. For controls, what is the probability of being in the highest alcohol group?
esoph %>%
  select(agegp, alcgp, tobgp,ncases, ncontrols) %>%
  filter(alcgp == '120+') %>%
  summarize( h_algp = sum(ncontrols), pr = h_algp/all_controls )

# Q11. How many times more likely are cases than controls to be in the highest alcohol group?
esoph %>%
  select(agegp, alcgp, tobgp,ncases, ncontrols) %>%
  filter(alcgp == '120+') %>%
  summarize( h_algp = sum(ncontrols), pr = h_algp/all_controls )

# Q12. How many times more likely are cases than controls to be in the highest alcohol group?

#For cases, what is the probability of being in the highest alcohol group?/For controls, what is the probability of being in the highest alcohol group?
res <- 0.225/0.06871795
res

# Q13. For controls, what is the probability of being in the highest tobacco group?

esoph %>%
  select(agegp, alcgp, tobgp,ncases, ncontrols) %>%
  filter(tobgp == '30+') %>%
  summarize( h_tob_controls = sum(ncontrols), pr = h_tob_controls/all_controls )

#Q14. For controls, what is the probability of being in the highest alcohol group and the highest tobacco group?
esoph %>%
  select(agegp, alcgp, tobgp,ncases, ncontrols) %>%
  filter(tobgp == '30+' & alcgp == '120+' ) %>%
  summarize( h_al_and_tob_controls = sum(ncontrols), pr = h_al_and_tob_controls/all_controls )

# Q15. For controls, what is the probability of being in the highest alcohol group or the highest tobacco group?
esoph %>%
  select(agegp, alcgp, tobgp,ncases, ncontrols) %>%
  filter(tobgp == '30+' | alcgp == '120+' ) %>%
  summarize( h_al_or_tob_controls = sum(ncontrols), pr = h_al_or_tob_controls/all_controls )

#Q16. How many times more likely are cases than controls to be in the highest alcohol group or the highest tobacco group?
#For cases, what is the probability of being in the highest alcohol group or the highest tobacco group?/For controls, what is the probability of being in the highest alcohol group or the highest tobacco group?
res <- 0.33/0.1394872


