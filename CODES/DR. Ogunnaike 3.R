pacman::p_load(
  rio,          # File import
  here,         # File locator
  skimr,        # get overview of data
  tidyverse,    # data management + ggplot2 graphics, 
  gtsummary,    # summary statistics and tests
  forcats,
  rstatix,      # statistics
  corrr,        # correlation analayis for numeric variables
  janitor,      # adding totals and percents to tables
  flextable     # converting tables to HTML
)

full <- import(here("FULL.xlsx"))
names(full)

#### I want to create wide .docx table of the descripitve of MCS and PCS grouped by depression
FULL <- full %>%
  group_by(Depression) %>%
  rstatix::get_summary_stats(MCS2, PCS2, type = "common")
FULL  <- flextable(FULL) %>%
  save_as_docx(FULL, path = " depressionQOL.docx")


#### I want to create a kruskal wallis table of MCS and Depression
MCSDEP <- full %>% 
  select(MCS2, Depression) %>%                       # keep variables of interest
  tbl_summary(                                         # produce summary table
    statistic = MCS2 ~ "{median} ({p25}, {p75})", # specify what statistic to show (default, so could remove)
    by = Depression) %>%                                  # specify the grouping variable
  add_p(MCS2 ~ "kruskal.test")                    # specify what test to perform

MCSDEP <- as.data.frame(MCSDEP)

MCSDEP  <- flextable(MCSDEP) %>%
  save_as_docx(MCSDEP, path = " depressionMCS.docx")


### mann-whitney/ wilconxon sign rank test
MCSDEP2 <- full %>% 
  wilcox_test(MCS2 ~ Depression, PCS2 ~ Depression)


PCSDEP <- full %>% 
  select(PCS2, Depression) %>%                       # keep variables of interest
  tbl_summary(                                         # produce summary table
    statistic = PCS2 ~ "{median} ({p25}, {p75})", # specify what statistic to show (default, so could remove)
    by = Depression) %>%                                  # specify the grouping variable
  add_p(PCS2 ~ "kruskal.test")                    # specify what test to perform

PCSDEP <- as.data.frame(PCSDEP)

PCSDEP  <- flextable(PCSDEP) %>%
  save_as_docx(PCSDEP, path = " depressionPCS.docx")





#### I am creating a stacked plots of type of SCD and depression, showing the percentage

full %>% 
  count(typeof, severity_of_depression) %>% 
  group_by(severity_of_depression) %>%  # Group by outcome to calculate percentages within each outcome
  mutate(pct = n / sum(n) * 100) %>%  # Calculate percentage
  ggplot() +
  geom_col(mapping = aes(x = Depression, fill = typeof, y = n)) +
  geom_text(aes(x = Depression, label = paste0(round(pct, 1), "%"),  # Add percentage labels
                y = n, group = typeof), 
            position = position_stack(vjust = 0.5))  # Position labels in the middle of bars


#### I am creating a stacked plots of type of SCD and depression, showing the percentage and Factor level order adjusted within ggplot
full$severity_of_depression <- as.factor(full$severity_of_depression)

full %>% 
  count(`Type of SCD`, severity_of_depression) %>% 
  group_by(severity_of_depression) %>%  # Group by outcome to calculate percentages within each outcome
  mutate(pct = n / sum(n) * 100) %>%  # Calculate percentage
  ggplot() +
  geom_col(mapping = aes(x = fct_relevel(severity_of_depression, c("None", "Mild", "Moderate", "Moderately severe", "Severe" )), fill = `Type of SCD`, y = n)) +
  # fctlevel will re-arrange the order as specified
  geom_text(aes(x = severity_of_depression, label = paste0(round(pct, 1), "%"),  # Add percentage labels
                y = n, group = `Type of SCD`), 
            position = position_stack(vjust = 0.5)) +  # Position labels in the middle of bars
  labs(
    title = "Depression and Type of SCD",
    subtitle = " The severity of depression is according to the PHQ-9 classification",
    x = "Severity of Depression",
    y = "Percentages",
    #color = "Age"
  ) +
  #theme_classic()+                                 # pre-defined theme adjustments
  theme(legend.position = "bottom")                   # move legend to bottom
  




#### I am creating a bar plot plots of depression, showing the percentage

full %>%
  count(severity_of_depression) %>% 
  mutate(pct = n / sum(n) * 100) %>%  # Calculate percentage
  ggplot() +
  geom_col(mapping = aes(x = fct_relevel(severity_of_depression, c("None", "Mild", "Moderate", "Moderately severe", "Severe" )), y = n)) +
  geom_text(aes(x = severity_of_depression, label = paste0(round(pct, 2), "%"),  # Add percentage labels
                y = n), 
            position = position_stack(vjust = 0.5)) + # Position labels in the middle of bars
  labs(
    title = "Severity of Depression",
    subtitle = "severity of depression according to the PHQ-9 classification",
    x = "Severity of Depression",
    y = "frequency"
    #color = "Age"
    )


names(full)


#### Summary statstics of the health related quality of life, SF-36
HRQOL <- full %>% 
  get_summary_stats(
    PF2, RP2, REP2, EF2, EWB2, SF2, PAIN2, GH2, PCS2, MCS2,  # columns to calculate for
    type = "common")                    # summary stats to return
HRQOL  <- flextable(HRQOL) %>%
  save_as_docx(HRQOL, path = " Descriptive of HRQOL.docx")


#### Normality test for the MCS and PCS
Normtest <- full %>% 
  head(500) %>%            # first 500 rows of case linelist, for example only
  shapiro_test(PCS2, MCS2)
Normtest  <- flextable(Normtest) %>%
  save_as_docx(Normtest, path = " Normality of HRQOL.docx")



### I wanna create an age-sex pyramid

# begin ggplot
ggplot(mapping = aes(x = age, fill = Gender)) +
  
  # female histogram
  geom_histogram(data = full %>% filter(Gender == "Female"),
                 breaks = seq(0,85,5),
                 colour = "white") +
  
  # male histogram (values converted to negative)
  geom_histogram(data = full %>% filter(Gender == "Male"),
                 breaks = seq(0,85,5),
                 mapping = aes(y = ..count..*(-1)),
                 colour = "white") +
  theme(legend.position = "bottom")   +                # move legend to bottom
  
  # flip the X and Y axes
  coord_flip() +
  
  # adjust counts-axis scale
  scale_y_continuous(limits = c(-20, 40),
                     breaks = seq(-20,40,10),
                     labels = abs(seq(-20, 40, 10))) +
  labs(
    title = "Age-Sex Pyramid",
    x = "Age",
    y = "Number of participants"
  )

# B) Box plot by group
ggplot(data = full, mapping = aes(y = ifyes, x = `Type of SCD`, fill = `Type of SCD`)) + 
  geom_boxplot()+                     
  theme(legend.position = "bottom") +   # bottom legend
  labs(title = "Steady PCV based on the type of sickle cell disease",
       x = "Sickle cell disease",
       y = "PCV%")    