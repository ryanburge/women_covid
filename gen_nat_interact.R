
### Nationalism ####

regg <- m20 %>% 
  mutate(defy  = coalesce(m20$q39_4, m20$q40_4, m20$q41_4, m20$q42_4)) %>% 
  mutate(defy2 = case_when(defy == 1 | defy == 2 ~ 1, 
                           defy == 3 | defy == 4 | defy == 5 ~ 0)) %>% 
  mutate(gender = frcode(q2 == 1 ~ "Men",
                         q2 == 2 ~ "Women")) %>% 
  mutate(trust_all = 6 - q37_6) %>% 
  mutate(fem_nat = 6 - q35_6) %>% 
  mutate(attend = 7 - q6) %>% 
  mutate(mf = q100_1 - q100_2) %>% 
  mutate(mas = q100_1) %>% 
  mutate(fem = q100_2) %>% 
  mutate(closed = frcode(q38_1 == 1 ~ "Closed", q38_1 == 2 ~ "Open")) %>% 
  mutate(white = case_when(q104_1 == 1 ~ 1, TRUE ~ 0)) %>% 
  mutate(pid2 = frcode(q91 == 1 | q91 == 2 | q91 == 3 ~ "Democrat", 
                       q91 == 5 | q91 == 6 | q91 == 7 ~ "Republican")) %>% 
  mutate(rep = case_when(q91 == 5 | q91 == 6 | q91 ==7 ~ 1, 
                         q91 <= 4 ~ 0)) %>% 
  mutate(literal = case_when(q53 == 1 ~ 1, 
                             q53 == 2 | q53 == 3 | q53 == 4 ~ 0)) %>% 
  mutate(lit2 = frcode(q53 == 1 ~ "Literalist",
                       q53 == 2 ~ "Inspired, not Literal",
                       q53 == 3 ~ "Written by Men")) %>% 
  mutate(age=2020-q3_1) %>% 
  select(gender, trust_all, mf, mas, fem, fem_nat, pid2, rep, literal, lit2, fem, educ = q105, income = q108, defy2, attend, white, age, reltrad, closed)

reg1 <- glm(defy2 ~ fem_nat*gender + white + attend + literal + age + income + educ + reltrad, data = regg, family = "binomial")

gg <- interact_plot(reg1, pred= fem_nat, modx = gender, int.width = .76, interval = TRUE) 

left <- gg +
  fill4_3() +
  color4_3() +
  y_pct() + 
  theme_gg("Jost", legend = TRUE) +
  theme(panel.spacing = unit(1, "lines")) +
  scale_x_continuous(breaks = c(1,2,3,4,5), labels = c("Strongly\nDisagree", "Disagree", "Neither", "Agree", "Strongly\nAgree")) +
  theme(plot.title = element_text(size = 12)) +
  labs(x = "The US is becoming too feminized and weak", y = "Church Should Defy Govt. Order", title = "") +
  ggsave("D://women_covid/images/defy_interact_fem_nat.png", type = "cairo-png", width =7)


reg1 <- glm(defy2 ~ fem_nat*closed + white + attend + literal + age + income + educ + reltrad, data = regg, family = "binomial")

gg <- interact_plot(reg1, pred= fem_nat, modx = closed , int.width = .76, interval = TRUE) 

right <- gg +
  fill4_3() +
  color4_3() +
  y_pct() + 
  theme_gg("Jost", legend = TRUE) +
  theme(panel.spacing = unit(1, "lines")) +
  scale_x_continuous(breaks = c(1,2,3,4,5), labels = c("Strongly\nDisagree", "Disagree", "Neither", "Agree", "Strongly\nAgree")) +
  theme(plot.title = element_text(size = 12)) +
  labs(x = "The US is becoming too feminized and weak", y = "Church Should Defy Govt. Order", title = "") +
  ggsave("D://women_covid/images/closed_interact_fem_nat.png", type = "cairo-png", width =7)

library(patchwork)

both <- left + right

ggsave("D://women_covid/images/both_fem_interacts.png", width = 10, both, type = "cairo-png")

