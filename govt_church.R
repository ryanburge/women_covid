m20 <- m20 %>% 
  mutate(gender = frcode(q2 == 1 ~ "Men",
                         q2 == 2 ~ "Women")) %>% 
  mutate(free  = coalesce(m20$q39_1, m20$q40_1, m20$q41_1, m20$q42_1)) %>%
  mutate(trust = coalesce(m20$q39_2, m20$q40_2, m20$q41_2, m20$q42_2)) %>%
  mutate(stop  = coalesce(m20$q39_3, m20$q40_3, m20$q41_3, m20$q42_3)) %>%
  mutate(defy  = coalesce(m20$q39_4, m20$q40_4, m20$q41_4, m20$q42_4))

fun <- function(df, var, name){
  
  df %>% 
    group_by(gender) %>% 
    mutate(vv = frcode({{var}} == 5 ~ "Strongly\nDisagree",
                       {{var}} == 4 ~ "Disagree",
                       {{var}} == 3 ~ "Neither",
                       {{var}} == 2 ~ "Agree",
                       {{var}} == 1 ~ "Strongly\nAgree")) %>% 
    ct(vv, show_na = FALSE) %>% 
    mutate(type = name)
  
  
}

ddd1 <- m20 %>% fun(free, "Freedom to Worship Too Important\nto Close Church Services")
ddd2 <- m20 %>% fun(trust, "Trust My Clergy To Have\nMy Best Interests at Heart")
ddd3 <- m20 %>% fun(stop, "Govt. Should Tell Churches\nTo Stop Meeting to Prevent\nSpread of Coronavirus")
ddd4 <- m20 %>% fun(defy, "If Govt. Tells Us to Stop Worshipping,\nMy Congregation Should Defy the Order")

gg <- bind_df("ddd")

gg %>% 
  ggplot(., aes(x = vv, y = pct, fill = gender)) +
  geom_col(color = "black", position = "dodge") +
  facet_wrap(~ type) +
  y_pct() + 
  fill4_3() +
  lab_bar(top = TRUE, pos = .025, sz = 2.5, type = pct) +
  theme_gg("Jost", legend = TRUE) +
  labs(x = "", y = "", title = "", caption = "") +
  ggsave("D://women_covid/images/church_c19_bars.png", type = "cairo-png", width = 7)



