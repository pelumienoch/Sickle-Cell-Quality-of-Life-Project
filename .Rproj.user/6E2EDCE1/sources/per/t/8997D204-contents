library(dplyr)
library(ggplot2)
library(ggcharts)

dreaded_lang <- tibble::tribble(
  ~language, ~pct,
  "VBA", 75.2,
  "Objective-C", 68.7,
  "Assembly", 64.4,
  "C", 57.5,
  "PHP", 54.2,
  "Erlang", 52.6,
  "Ruby", 49.7,
  "R", 48.3,
  "C++", 48.0,
  "Java", 46.6
)

chart <- dreaded_lang %>%
  bar_chart(language, pct) %>%
  print()


chart +
  geom_text(aes(x = language, y = pct, label = pct))

chart +
  geom_text(aes(label = pct, hjust = "left"))

chart +
  geom_text(aes(label = pct, hjust = -0.2)) +
  ylim(NA, 100)


dreaded_lang %>%
  mutate(label = sprintf("%1.1f%%", pct)) %>%
  bar_chart(language, pct, highlight = "R", bar_color = "black") +
  geom_text(aes(label = label, hjust = -0.1), size = 5) +
  scale_y_continuous(
    limits = c(0, 100),
    expand = expansion()
  ) +
  labs(
    x = NULL,
    y = "Developers Who are Developing with the Language but<br>Have not Expressed Interest in Continuing to Do so",
    title = "Top 10 Most Dreaded Programming Languages",
    subtitle = "*R Placed 8th*",
    caption = "Source: Stackoverflow Developer Survey 2019"
  ) +
  mdthemes::md_theme_classic(base_size = 14) +
  theme(
    axis.text.x = element_blank(),
    axis.line.x = element_blank(),
    axis.ticks.x = element_blank()
  )