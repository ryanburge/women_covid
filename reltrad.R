library(fst)
library(socsci)
library(car)
source("D://theme.R")

m20 <- read_csv("D://final_fear.csv")


m20 <- m20 %>% rename_all(tolower)
m20 <- m20 %>% filter(q1==1)

m20 <- m20 %>% mutate(attend5=car::recode(q6, "1:2=5; 3=4; 4=3; 5=2; 6=1"))
m20 <- m20 %>% mutate(attend6 = 7 - q6)
m20 <- m20 %>% mutate(hiatt = recode(q6, "1:3=1; else=0"))  




#RELTRAD Stuff####
m20 <- m20 %>% 
  mutate(religpew = case_when(q7 == 1 | q7 == 3 ~ 1, 
                              q7 == 4 ~ 2, 
                              q7 == 5 ~ 3, 
                              q7 == 6 ~ 4, 
                              q7 == 7 ~ 5, 
                              q7 == 8 ~ 6, 
                              q7 == 9 ~ 7, 
                              q7 == 10 ~ 8, 
                              q7 == 11 ~ 9, 
                              q7 == 12 ~ 10, 
                              q7 == 13 ~ 11, 
                              q7 == 2 | q7 == 14 ~ 12)) %>% 
  mutate(religpew_protestant = case_when(q8 == 1 ~ 3, 
                                         q8 == 2 ~ 1, 
                                         q8 == 3 ~ 2, 
                                         q8 == 4 ~ 4, 
                                         q8 == 5 ~ 5, 
                                         q8 == 6 ~ 6, 
                                         q8 == 7 ~ 7, 
                                         q8 == 8 ~ 8, 
                                         q8 == 9 ~ 9, 
                                         q8 == 10 ~ 10, 
                                         q8 == 11 ~ 11, 
                                         q8 == 12 ~ 12, 
                                         q8 == 13 ~ 13, 
                                         q8 == 14 | q8 == 15 ~ 90)) %>% 
  mutate(religpew_baptist = q9) %>% 
  mutate(religpew_methodist = q10) %>% 
  mutate(religpew_nondenom = q11) %>% 
  mutate(religpew_lutheran = q12) %>% 
  mutate(religpew_presby = q13) %>% 
  mutate(religpew_pentecost = q14) %>% 
  mutate(religpew_episcop = q15) %>% 
  mutate(religpew_christian = q16) %>% 
  mutate(religpew_congreg = q17) %>% 
  mutate(religpew_holiness = q18) %>% 
  mutate(religpew_reformed = q19) %>% 
  mutate(religpew_advent = q20) %>% 
  mutate(white = case_when(q104_1 == 1 ~ 1, TRUE ~ 0)) %>% 
  mutate(black = case_when(q104_3 == 1 ~ 1, TRUE ~ 0))




m20 <- m20 %>% 
  mutate(religpew_baptist = case_when(religpew_baptist == 1 ~ 1, 
                                      religpew_baptist == 4 ~ 2, 
                                      religpew_baptist == 5 ~ 3, 
                                      religpew_baptist == 6 ~ 4,
                                      religpew_baptist == 7 ~ 5, 
                                      religpew_baptist == 8 ~ 6, 
                                      religpew_baptist == 9 ~ 7, 
                                      religpew_baptist == 10 ~ 8, 
                                      religpew_baptist == 11 ~ 9, 
                                      religpew_baptist == 12 ~ 10,
                                      religpew_baptist == 13 ~ 90)) %>% 
  mutate(religpew_methodist = case_when(religpew_methodist == 1 ~ 1, 
                                        religpew_methodist == 4 ~ 2, 
                                        religpew_methodist == 5 ~ 3, 
                                        religpew_methodist == 6 ~ 4, 
                                        religpew_methodist == 7 ~ 5,
                                        religpew_methodist == 8 ~ 90)) %>% 
  mutate(religpew_nondenom = case_when(religpew_nondenom == 1 ~ 1, 
                                       religpew_nondenom == 4 ~ 2, 
                                       religpew_nondenom == 5 ~ 3, 
                                       religpew_nondenom == 6 ~ 4, 
                                       religpew_nondenom == 7 ~ 5,
                                       religpew_nondenom == 8 ~ 90)) %>% 
  mutate(religpew_lutheran = case_when(religpew_lutheran == 1 ~ 1, 
                                       religpew_lutheran == 4 ~ 2, 
                                       religpew_lutheran == 5 ~ 3, 
                                       religpew_lutheran == 6 ~ 4)) %>% 
  mutate(religpew_presby = case_when(religpew_presby == 1 ~ 1, 
                                     religpew_presby == 4 ~ 2, 
                                     religpew_presby == 5 ~ 3, 
                                     religpew_presby == 6 ~ 4, 
                                     religpew_presby == 7 ~ 5, 
                                     religpew_presby == 8 ~ 6, 
                                     religpew_presby == 9 ~ 90)) %>% 
  mutate(religpew_pentecost = case_when(religpew_pentecost == 1 ~ 1, 
                                        religpew_pentecost == 4 ~ 2, 
                                        religpew_pentecost == 5 ~ 3, 
                                        religpew_pentecost == 6 ~ 4, 
                                        religpew_pentecost == 7 ~ 5, 
                                        religpew_pentecost == 8 ~ 6, 
                                        religpew_pentecost == 9 ~ 7, 
                                        religpew_pentecost == 10 ~ 8, 
                                        religpew_pentecost == 11 ~ 9, 
                                        religpew_pentecost == 12 ~ 90)) %>% 
  mutate(religpew_episcop = case_when(religpew_episcop == 1 ~ 1, 
                                      religpew_episcop == 8 ~ 2, 
                                      religpew_episcop == 9 ~ 3, 
                                      religpew_episcop == 10 ~ 4, 
                                      religpew_episcop == 11 ~ 90)) %>% 
  mutate(religpew_christian = case_when(religpew_christian == 1 ~ 1, 
                                        religpew_christian == 4 ~ 2, 
                                        religpew_christian == 5 ~ 3, 
                                        religpew_christian == 6 ~ 90)) %>% 
  mutate(religpew_congreg = case_when(religpew_congreg == 1 ~ 1, 
                                      religpew_congreg == 4 ~ 2, 
                                      religpew_congreg == 5 ~ 3, 
                                      religpew_congreg == 6 ~ 90)) %>% 
  mutate(religpew_holiness = case_when(religpew_holiness == 1 ~ 1, 
                                       religpew_holiness == 4 ~ 2, 
                                       religpew_holiness == 5 ~ 3, 
                                       religpew_holiness == 6 ~ 4, 
                                       religpew_holiness == 7 ~ 5, 
                                       religpew_holiness == 8 ~ 6, 
                                       religpew_holiness == 9 ~ 90)) %>% 
  mutate(religpew_reformed = case_when(religpew_reformed == 1 ~ 1, 
                                       religpew_reformed == 4 ~ 2, 
                                       religpew_reformed == 5 ~ 90)) %>% 
  mutate(religpew_advent = case_when(religpew_advent == 1 ~ 1, 
                                     religpew_advent == 7 ~ 2, 
                                     religpew_advent == 8 ~ 3, 
                                     religpew_advent == 9 ~ 90)) 




## Baptist

m20 <- m20 %>%
  mutate(sbc = recode(m20$religpew_baptist, "1=1; else=0")) %>% 
  mutate(sbc = white + sbc) %>% 
  mutate(sbc = recode(sbc, "2=1; else=0"))

m20 <- m20 %>%
  mutate(abc = recode(m20$religpew_baptist, "2=1; else=0")) %>% 
  mutate(abc = white + abc) %>% 
  mutate(abc = recode(abc, "2=1; else=0"))

m20 <- m20 %>%
  mutate(ibc = recode(m20$religpew_baptist, "5=1; else=0")) 

m20 <- m20 %>%
  mutate(bgc = recode(m20$religpew_baptist, "6=1; else=0")) 

m20 <- m20 %>%
  mutate(mbc = recode(m20$religpew_baptist, "7=1; else=0")) %>% 
  mutate(mbc = white + mbc) %>% 
  mutate(mbc = recode(mbc, "2=1; else=0"))

m20 <- m20 %>%
  mutate(cb = recode(m20$religpew_baptist, "8=1; else=0")) 

m20 <- m20 %>%
  mutate(fwb = recode(m20$religpew_baptist, "9=1; else=0")) 

m20 <- m20 %>%
  mutate(gabb = recode(m20$religpew_baptist, "10=1; else=0")) 

m20 <- m20 %>%
  mutate(obc = recode(m20$religpew_baptist, "90=1; else=0")) %>% 
  mutate(obc = white + obc) %>% 
  mutate(obc = recode(obc, "2=1; else=0"))

m20 <- m20 %>% 
  mutate(evanbap = sbc + abc + ibc + bgc + mbc + cb + fwb + gabb + obc)

## Methodist
m20 <- m20 %>%
  mutate(fmc = recode(m20$religpew_methodist, "2=1; else=0")) 

m20 <- m20 %>%
  mutate(omc = recode(m20$religpew_methodist, "90=1; else=0")) %>% 
  mutate(omc = white + omc) %>% 
  mutate(omc = recode(omc, "2=1; else=0"))

m20 <- m20 %>% 
  mutate(evanmeth = fmc + omc)

##Non-Denom

m20 <- m20 %>% 
  mutate(nd = recode(religpew_nondenom, "1:90=1; else=0")) %>% 
  mutate(evannd = nd + hiatt) %>% 
  mutate(evannd =  recode(evannd, "2=1; else=0"))

## Lutheran 

m20 <- m20 %>% 
  mutate(mz = recode(religpew_lutheran, "2=1; else=0")) %>% 
  mutate(wi = recode(religpew_lutheran, "3=1; else=0")) %>% 
  mutate(evanluth = mz + wi)

## Presbyterian

m20 <- m20 %>% 
  mutate(pca = recode(religpew_presby, "2=1; else=0")) %>% 
  mutate(epc = recode(religpew_presby, "6=1; else=0")) %>% 
  mutate(evanpres = pca + epc)

## Pentecostal 

m20 <- m20 %>% 
  mutate(evanpent = recode(religpew_pentecost, "1:90 =1; else=0"))

## Episcopal 
## None

## Congregregational

m20 <- m20 %>% 
  mutate(evancong = recode(religpew_congreg, "2=1; else=0"))

## Holiness
m20 <- m20 %>% 
  mutate(evanholy = recode(religpew_holiness, "1:90 =1; else=0"))

## Advent
## None 

## Totaling Up

m20 <- m20 %>% 
  mutate(evangelical = evanbap + evanmeth + evannd + evanluth + evanpres + evanpent + evancong + evanholy) %>% 
  mutate(evangelical = recode(evangelical, "1:4=1; else=0"))

## Making Mainline

m20 <- m20 %>% 
  mutate(abc = recode(m20$religpew_baptist, "2=1; 4=1; else=0"))

m20 <- m20 %>% 
  mutate(epis = recode(m20$religpew_episcop, "1:90=1; else=0"))

m20 <- m20 %>% 
  mutate(luth = recode(m20$religpew_lutheran, "1=1; 4=1; else=0"))

m20 <- m20 %>% 
  mutate(meth = recode(m20$religpew_methodist, "1=1; 90=1; else=0"))

m20 <- m20 %>% 
  mutate(pres = recode(m20$religpew_presby, "1=1; 90=1; else=0"))

m20 <- m20 %>% 
  mutate(cong = recode(m20$religpew_congreg, "1=1; 3=1; 90=1; else=0"))

m20 <- m20 %>% 
  mutate(doc = recode(m20$religpew_protestant, "8=1; else=0"))

m20 <- m20 %>% 
  mutate(reform = recode(m20$religpew_protestant, "11=1; else=0"))

m20 <- m20 %>% 
  mutate(mainline = abc + epis + luth + meth + pres + cong + doc + reform) %>% 
  mutate(mainline = recode(mainline, "1:5=1; else=0"))

## Black Protestant 

m20 <- m20 %>% 
  mutate(black = recode(q104_3, "1=1; else=0"))

m20 <- m20 %>% 
  mutate(meth = recode(m20$religpew_methodist, "3:4=1; else=0"))

m20 <- m20 %>%
  mutate(sbc = recode(m20$religpew_baptist, "1=1; else=0")) %>% 
  mutate(sbc = black + sbc) %>% 
  mutate(sbc = recode(sbc, "2=1; else=0"))

m20 <- m20 %>% 
  mutate(nbap = recode(m20$religpew_baptist, "3=1; else=0"))

m20 <- m20 %>%
  mutate(abc = recode(m20$religpew_baptist, "2=1; else=0")) %>% 
  mutate(abc = black + abc) %>% 
  mutate(abc = recode(abc, "2=1; else=0"))

m20 <- m20 %>%
  mutate(miss = recode(m20$religpew_baptist, "7=1; else=0")) %>% 
  mutate(miss = black + miss) %>% 
  mutate(miss = recode(miss, "2=1; else=0"))

m20 <- m20 %>%
  mutate(obap = recode(m20$religpew_baptist, "90=1; else=0")) %>% 
  mutate(obap = black + obap) %>% 
  mutate(obap = recode(obap, "2=1; else=0"))

m20 <- m20 %>%
  mutate(ometh = recode(m20$religpew_methodist, "90=1; else=0")) %>% 
  mutate(ometh = black + ometh) %>% 
  mutate(ometh = recode(ometh, "2=1; else=0"))

m20 <- m20 %>% 
  mutate(apos = recode(m20$religpew_pentecost, "6=1; 7=1; else=0"))

m20 <- m20 %>%
  mutate(open = recode(m20$religpew_pentecost, "90=1; else=0")) %>% 
  mutate(open = black + open) %>% 
  mutate(open = recode(open, "2=1; else=0"))

m20 <- m20 %>%
  mutate(holy = recode(m20$religpew_holiness, "90=1; else=0")) %>% 
  mutate(holy = black + holy) %>% 
  mutate(holy = recode(holy, "2=1; else=0"))


m20 <- m20 %>% 
  mutate(bprot = meth + sbc + nbap + abc + miss + obap + ometh + apos + open + holy) %>% 
  mutate(bprot = recode(bprot, "1:2=1; else=0"))

## Everything Else

m20 <- m20 %>% 
  mutate(catholic = recode(religpew, "2=1; else=0"))

m20 <- m20 %>% 
  mutate(jewish = recode(religpew, "5=1; else=0"))

m20 <- m20 %>% 
  mutate(other = recode(religpew, "3=1; 6:8=1; 12=1; else=0"))

m20 <- m20 %>% 
  mutate(none = recode(religpew, "9:11=1; else=0"))


## One Variable ###

m20 <- m20 %>% 
  mutate(reltrad = frcode(evangelical == 1 ~ "Evangelical",
                          mainline == 1 ~ "Mainline",
                          bprot == 1 ~ "Black Prot.",
                          catholic == 1 ~ "Catholic",
                          jewish == 1 ~ "Jewish",
                          other == 1 ~ "Other Faith",
                          none == 1 ~ "No Religion", 
                          TRUE ~ "Unclassified"))

