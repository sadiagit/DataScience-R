library(tidyverse)

esoph %>% group_by(agegp, alcgp,tobgp)
all_cases <- sum(esoph$ncases)
all_controls <- sum(esoph$ncontrols)
all_controls

esoph %>%
  arrange(desc(alcgp))
hgp<-esoph %>% select(agegp, alcgp,ncases, ncontrols) %>% filter(alcgp == '120+')
hgp_cases<- sum(hgp$ncases)
controls<- sum(hgp$ncontrols)
t<-hgp_cases+controls
t
p<- hgp_cases/t
p