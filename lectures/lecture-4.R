library(tidyverse); library(knitr); library(broom)

master <- read.csv("https://raw.githubusercontent.com/jgeller112/psy503-psych_stats/master/static/slides/10-linear_modeling/data/regress.csv")

head(master)

colnames(master) <- c("subid", "PIL", "CESD", "AUDIT", "DAST")

ggplot(master, aes(x=PIL, y=CESD))+
  geom_point() + 
  theme_minimal(base_size=16) +
  labs(x = "Meaning in Life", y = "Depression")

ggsave("PIL_CESD.png", height = 6, width = 8)

ggplot(master, aes(x=AUDIT, y=CESD))+
  geom_point() + 
  theme_minimal(base_size=16) +
  labs(x = "Alcohol Use", y = "Depression")

ggsave("PIL_AUDIT.png", height = 6, width = 8)

ggplot(master, aes(x=DAST, y=CESD))+
  geom_point() + 
  theme_minimal(base_size=16) +
  labs(x = "Drug Use", y = "Depression")

ggsave("PIL_DAST.png", height = 6, width = 8)


model_fit <- lm(CESD ~ PIL + AUDIT + DAST,
                data = master) 

summary(model_fit)

model_fit %>%
  tidy(conf.int=TRUE) %>%
  knitr::kable() 
