
## Experimental Stuff ####

m20 <- m20 %>% 
  mutate(gender = frcode(q2 == 1 ~ "Men",
                         q2 == 2 ~ "Women")) %>% 
  mutate(defy_g  = coalesce(m20$q39_4, m20$q41_4)) %>% 
  mutate(defy_t  = coalesce(m20$q40_4, m20$q42_4))  %>% 
  mutate(dg = case_when(defy_g == 1 | defy_g == 2 ~ 1, 
                        defy_g == 3 | defy_g == 4 | defy_g == 5 ~ 0)) %>% 
  mutate(dt = case_when(defy_t == 1 | defy_t == 2 ~ 1, 
                        defy_t == 3 | defy_t == 4 | defy_t == 5 ~ 0)) 

m20 %>% 
  filter(none != 1) %>% 
  group_by(gender) %>% 
  mean_ci(dg) %>% 
  mutate(type = "Defy Govt.")

m20 %>% 
  filter(none != 1) %>% 
  group_by(gender) %>% 
  mean_ci(dt) %>% 
  mutate(type = "Defy Trump")