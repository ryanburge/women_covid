
fun <- function(df, var, name) {
  
  m20 %>% 
    mutate(vv = frcode({{var}} == 5 ~ "Strongly\nDisagree",
                       {{var}} == 4 ~ "Disagree",
                       {{var}} == 3 ~ "Neither",
                       {{var}} == 2 ~ "Agree",
                       {{var}} == 1 ~ "Strongly\nAgree")) %>% 
    group_by(gender) %>% 
    ct(vv, show_na = FALSE) %>% 
    mutate(type = name)
  
  
}

eee1 <- m20 %>% fun(q37_1, "Coronavirus is a Major Threat")
eee2 <- m20 %>% fun(q37_2, "Hysteria is Politically Motivated")
eee3 <- m20 %>% fun(q37_3, "I am Practicing Social Distancing")
eee4 <- m20 %>% fun(q37_4, "I Trust Medical Professionals")


gg <- bind_df("eee")

gg %>% 
  ggplot(., aes(x = vv, y = pct, fill = gender)) +
  geom_col(color = "black", position = "dodge") +
  facet_wrap(~ type) +
  y_pct() + 
  fill4_3() +
  lab_bar(top = TRUE, pos = .035, sz = 2.5, type = pct) +
  theme_gg("Jost", legend = TRUE) +
  labs(x = "", y = "", title = "", caption = "") +
  ggsave("D://women_covid/images/gender_covid_c19_distance.png", type = "cairo-png", width = 8)

t.test(q37_1~female, data=m20)
t.test(q37_2~female, data=m20)
t.test(q37_3~female, data=m20)
t.test(q37_4~female, data=m20)
