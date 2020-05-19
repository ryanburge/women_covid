
reg1 <- glm(defy2 ~ age*gender + literal + attend + white + rep +  income + educ, data = regg, family = "binomial")

gg <- interact_plot(reg1, pred= age, modx = gender, int.width = .76, interval = TRUE) 

gg +
  fill4_3() +
  color4_3() +
  y_pct() + 
  theme_gg("Jost", legend = TRUE) +
  theme(panel.spacing = unit(1, "lines")) +
  scale_x_continuous(limits = c(18, 80)) +
  labs(x = "Age", y = "Church Should Defy Govt. Order", title = "Interaction of Age and Gender on Defying Govt. Orders") +
  ggsave("D://women_covid/images/defy_interact_age.png", type = "cairo-png", width =7)


reg1 <- glm(defy2 ~ attend*gender*pid2 + literal + white + income + educ, data = regg, family = "binomial")

gg <- interact_plot(reg1, pred= attend, modx = gender, mod2 = pid2, int.width = .76, interval = TRUE) 

gg +
  fill4_3() +
  color4_3() +
  y_pct() + 
  theme_gg("Jost", legend = TRUE) +
  theme(panel.spacing = unit(1, "lines")) +
  scale_x_continuous(limits = c(18, 80)) +
  labs(x = "Age", y = "Church Should Defy Govt. Order", title = "Interaction of Age and Gender on Defying Govt. Orders") +
  ggsave("D://women_covid/images/defy_interact_age.png", type = "cairo-png", width =7)