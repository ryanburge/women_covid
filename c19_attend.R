## Frequencies ###

m20 <- m20 %>% 
  mutate(cancel = case_when(q38_1 == 1 ~ 1, q38_1 == 2 ~ 0)) %>% 
  mutate(encourage = case_when(q38_2 == 1 ~ 1, q38_2 == 2 ~ 0)) %>% 
  mutate(attend = case_when(q38_3 == 1 ~ 1, q38_3 == 2 ~ 0)) %>% 
  mutate(online = case_when(q38_4 == 1 ~ 1, q38_4 == 2 ~ 0))

ccc1 <- m20 %>% 
  filter(none != 1) %>% 
  group_by(gender) %>% 
  mean_ci(cancel, ci = .84) %>% 
  mutate(type = "Services Cancelled")

ccc2 <- m20 %>% 
  filter(none != 1) %>% 
  group_by(gender) %>% 
  mean_ci(encourage, ci = .84) %>% 
  mutate(type = "Worship Encouraged")

ccc3 <- m20 %>% 
  filter(none != 1) %>% 
  group_by(gender) %>% 
  mean_ci(attend, ci = .84) %>% 
  mutate(type = "Continue Attending")

ccc4 <- m20 %>% 
  filter(none != 1) %>% 
  group_by(gender) %>% 
  mean_ci(online, ci = .84) %>% 
  mutate(type = "Worshipping Online")

gg <- bind_df("ccc")

gg %>% 
  ggplot(., aes(x = type, y = mean, fill = gender)) +
  geom_col(color = "black", position = "dodge") +
  y_pct() + 
  fill4_3() +
  error_bar() +
  lab_bar(top = FALSE, pos = .05, sz = 4, type = mean) +
  theme_gg("Jost", legend = TRUE) +
  labs(x = "", y = "", title = "", caption = "") +
  ggsave("D://women_covid/images/attend_c19.png", type = "cairo-png", width = 7)