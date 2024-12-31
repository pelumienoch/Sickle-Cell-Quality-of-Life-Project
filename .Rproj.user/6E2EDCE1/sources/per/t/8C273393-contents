#### first we load the packages to use

pacman::p_load(
  rio,        # importing data  
  here,       # relative file pathways  
  janitor,    # data cleaning and tables
  lubridate,  # working with dates
  matchmaker, # dictionary-based cleaning
  epikit,     # age_categories() function
  tidyverse,   # data management and visualization
  gtsummary
)

here()
HBV <- import(here("MOGARs DATA.xlsx"), which = "Sheet1")

### now, lets view the dataset
head(HBV)  # this shows only the first 6 rows by default
head(HBV, 10)   # this shows the firt 10 rows

### lets skim the data, that is, to provide a summary of the dataset
pacman::p_load(skimr)

skimr::skim(HBV)

### lets check the names of the column
names(HBV)

### The column nmaes are not consistent, hence we will autocorrect

# pipe the raw dataset through the function clean_names(), assign result as "clean_HBV"  
clean_HBV<- HBV %>% 
  janitor::clean_names()

# see the new column names
names(clean_HBV)



### we can as well do a manual cleaning, see the code below
# CLEANING 'PIPE' CHAIN (starts with raw data and pipes it through cleaning steps)
##################################################################################
clean_HBV <- clean_HBV %>%
  
  # standardize column name syntax
  janitor::clean_names() %>% 
  
  # manually re-name columns
  # NEW name             # OLD name
  rename(SN              = s_n,
         faculty =      x5,
         programme_of_study =    x6,
         HBV_status =  x10,
         Use_shared_needles_or_syringes =
           how_often_do_you_engage_in_the_following_activities_1_never_2_rarely_3_sometimes_4_often_5_always,
         tattoos_or_piercings_from_unlicensed_facilities = x32,
         Share_personal_items_eg_razors_toothbrushes = x33,
         Unprotected_sexual_intercourse =  x34
         )
names(clean_HBV)
skim(clean_HBV)

clean_HBV <- clean_HBV %>% 
  mutate(across(.cols = -c(SN, gender, faculty, programme_of_study, state_of_residence,
                           marital_status,HBV_status,
                           parents_guardians_level_of_education, how_is_hepatitis_b_transmitted,
                           what_are_common_symptoms_of_hepatitis_b, how_is_hepatitis_b_diagnosed,
                           if_yes_from_where_did_you_first_hear_about_hbv, what_are_common_symptoms_of_hepatitis_b
                           , what_treatments_are_available_for_hepatitis_b, if_no_why_have_you_not_been_tested, if_no_why_have_you_not_been_vaccinated,
                           what_do_you_believe_is_the_best_way_to_prevent_hepatitis_b_infection), .fns = as.factor))
skim(clean_HBV) ### to see if it changed the class of the variables apart from the ones i excluded using '-c'

clean_HBV <- clean_HBV %>% 
  mutate(across(.cols = c(marital_status, parents_guardians_level_of_education), .fns = as.factor))
skim(clean_HBV)

#### To recode multiple columns

# Recode values
clean_HBV2 <- clean_HBV %>%
  mutate(
    age_yrs = recode(age_yrs, "1" = "15-18years", "2" = "19-22years", "3" = "23-25years"),
    marital_status = recode(marital_status, "1" = "Single", "2" = "Married",  "3" = "Cohabiting"),
    year_of_study_level = recode(year_of_study_level, "1" = "200 level"
                                         , "2" = "300 level", "3" = "400 level", "4" = "400 level"),
    parents_guardians_level_of_education = recode(parents_guardians_level_of_education, "1" = "Primary", "2" = "Secondary", "3" = "Tertiary"),
    what_causes_hepatitis_b = recode(what_causes_hepatitis_b, "1" = "Bacteria", "2" = "Virus",
                                     "3" = "Fungus", "4" = "I dont know"),
    have_you_ever_been_tested_for_hepatitis_b = recode(have_you_ever_been_tested_for_hepatitis_b, "1" = "Yes", "2" = "No"),
    if_yes_when_was_your_last_test = recode(if_yes_when_was_your_last_test, "1" = "< 6 months ago", "2" = "6months - 1 year ago",
                                            "3" = "> 1 year ago"),
    if_yes_how_many_doses = recode(if_yes_how_many_doses, "1" = "One", "2" = "Two", "3" = "Three"),
    how_would_you_rate_your_knowledge_about_hepatitis_b = recode(how_would_you_rate_your_knowledge_about_hepatitis_b, "1" = "very poor",
                                                                 "2" = "poor", "3" = "fair", "4" = "good", "5" = "excellent")
     )

#### To recode multiple columns with the same values at once

# Recode 1 to "A" and 2 to "B" in multiple columns
clean_HBV2 <- clean_HBV2 %>%
  mutate(across(c(Use_shared_needles_or_syringes, tattoos_or_piercings_from_unlicensed_facilities,
                  Share_personal_items_eg_razors_toothbrushes, Unprotected_sexual_intercourse), ~ recode(.x, "1" = "Never", "2" = "Rarely", "3" = "Sometimes", "4" = "Often", "5" = "Always")))

clean_HBV2 <- clean_HBV2 %>%
  mutate(across(c(can_i_have_hepatitis_b_and_not_be_aware_of_it, is_there_a_cure_for_hepatitis_b,
                  can_hepatitis_b_be_prevented_by_a_vaccine, if_you_are_diagnosed_with_hepatitis_b_would_you_go_for_further_investigation_and_treatment,
                  have_you_been_vaccinated_against_hepatitis_b, do_you_think_hepatitis_b_awareness_programs_are_necessary_in_universities,
                  are_you_willing_to_share_information_about_hepatitis_b_with_your_friends_and_family, can_i_have_hepatitis_b_for_years_without_symptoms), ~ recode(.x, "1" = "Yes", "2" = "No", "3" = "I dont know")))


###########################
clean_HBV3 <- clean_HBV2 %>%
  
  # remove column
  select(-c(programme_of_study)) %>% 
  
  # de-duplicate
  distinct() 
#### separating the multiple responses out from the singles responses, because i will like to perform a combination analysis
# Create a new dataset with the multiple response questions alone
multiple_responses <- clean_HBV3 %>% 
  select(c(SN,if_yes_from_where_did_you_first_hear_about_hbv,how_is_hepatitis_b_transmitted,
           how_is_hepatitis_b_diagnosed, what_treatments_are_available_for_hepatitis_b, what_are_common_symptoms_of_hepatitis_b,
           if_no_why_have_you_not_been_tested, if_no_why_have_you_not_been_vaccinated, what_do_you_believe_is_the_best_way_to_prevent_hepatitis_b_infection))
#### creating the single responses
single_responses <- clean_HBV3 %>%
  select(-c(if_yes_from_where_did_you_first_hear_about_hbv,how_is_hepatitis_b_transmitted,
           how_is_hepatitis_b_diagnosed, what_treatments_are_available_for_hepatitis_b, what_are_common_symptoms_of_hepatitis_b,
           if_no_why_have_you_not_been_tested, if_no_why_have_you_not_been_vaccinated, what_do_you_believe_is_the_best_way_to_prevent_hepatitis_b_infection))

  

pacman::p_load(
  rio,          # File import
  here,         # File locator
  skimr,        # get overview of data
  tidyverse,    # data management + ggplot2 graphics 
  gtsummary,    # summary statistics and tests
  rstatix,      # summary statistics and statistical tests
  janitor,      # adding totals and percents to tables
  scales,       # easily convert proportions to percents  
)

### descriptive statix
## lets check the variables first of all
skim(single_responses)
### SECTION A: SOCIODEMOGRAPHIC FACTOR ####
socio<-single_responses %>% 
  select(age_yrs, gender,year_of_study_level,marital_status,parents_guardians_level_of_education) %>%  # keep only the columns of interest
  tbl_summary()    ### creating a table summary

socio <- as.data.frame(socio) #### we must convert the sdf object into data frame for us to use flextable
 
my_table  <- flextable(socio) %>%
  save_as_docx(socio, path = "socio demo.docx")
  
### OBJ 1: PREVALENCE
### WITH SOMEADORNMENTS
single_responses %>%               # case linelist
  tabyl(HBV_status) %>%       # tabulate counts and proportions by age category
  adorn_pct_formatting()   # convert proportions to percents


single_responses %>%
  tabyl(HBV_status) %>% 
  adorn_totals(where = "col") %>% 
  adorn_percentages(denominator = "col") %>% 
  adorn_pct_formatting() %>% 
  adorn_ns(position = "front") %>% 
  adorn_title(col_name = "HBV Status",
    placement = "combined") %>% # this is necessary to print as image
  flextable::flextable() %>%    # convert to pretty image
  flextable::autofit()     %>%     # format to one line per row
  save_as_docx(path = "prevalence.docx")


install.packages("ggcharts")
### Lets make a bar chart where necessary



library(dplyr)
library(ggplot2)
library(ggcharts)

facultyv <- tibble::tribble(
  ~faculty, ~count,
  "ADMINISTRATION", 100,
  "EDUCATION", 77,
  "NATURAL AND APPLIED SCIENCES", 194,
  "SOCIAL SCIENCES", 51,
)

chart <- facultyv %>%
  bar_chart(faculty, count) %>%
  print()

chart +
  geom_text(aes(label = count, hjust = "left"))

# A) faculty
ggplot(single_responses %>% drop_na()) + 
  geom_bar(aes(y = faculty)) +
  geom_text(aes(label = ..count..), vjust = -0.3)+
  theme_minimal()+
  labs(title = "Number of participants by faculty",
       y = "Faculty")



# prevalence of HBV
ggplot(HBV_status) + 
  geom_col(aes(x=HBV_status, y = proportion)) +
  labs(subtitle = "Number of recovered and dead Ebola cases")

ggplot(single_responses, aes(x = HBV_status)) +
  geom_bar(fill = "skyblue") +
  labs(title = "Prevalence of HBV Status", x = "HBV Status", y = "Count")

ggplot(single_responses, aes(x = HBV_status)) +
  geom_bar(aes(y = ..count../sum(..count..)), fill = "skyblue") +
  labs(title = "Prevalence of HBV Status (Percentage)", x = "HBV Status", y = "Percentage") +
  scale_y_continuous(labels = scales::percent)

ggplot(single_responses, aes(x = HBV_status)) +
  geom_bar(aes(y = ..count../sum(..count..)), fill = "skyblue") +
  geom_text(aes(label = scales::percent(..count../sum(..count..))), vjust = -0.5) +
  labs(title = "Prevalence of HBV Status (Percentage)", x = "HBV Status", y = "Percentage") +
  scale_y_continuous(labels = scales::percent)


# Create a pie chart
ggplot(single_responses, aes(x = "", y = HBV_status, fill = HBV_status)) +
  geom_bar(stat = "identity", color = "white") +
  coord_polar(theta = "y") +
  theme_void() +
  labs(title = "Prevalence of HBV in this study")


### Prevalence by faculty y axis
ggplot(single_responses %>% drop_na(HBV_status)) + 
  geom_bar(aes(y = faculty, fill = HBV_status), width = 0.7) +
  theme_minimal()+
  theme(legend.position = "bottom") +
  labs(title = "Number of positive and negative cases, by faculty",
       y = "faculty")

### Prevalence by faculty x axis
ggplot(single_responses %>% drop_na(HBV_status)) + 
  geom_bar(aes(x = faculty, fill = HBV_status), width = 0.7) +
  theme_minimal()+
  theme(legend.position = "bottom") +
  labs(title = "Number of positive and negative cases, by faculty",
       x = "faculty")


### SECTION B
### ASSESSING THE KNOWLEDGE
Know<-single_responses %>% 
  select(have_you_ever_heard_about_hepatitis_b_virus_hbv_infection_before_this_survey,
         what_causes_hepatitis_b,can_i_have_hepatitis_b_and_not_be_aware_of_it,
         can_i_have_hepatitis_b_for_years_without_symptoms, is_there_a_cure_for_hepatitis_b,
         can_hepatitis_b_be_prevented_by_a_vaccine) %>%  # keep only the columns of interest
  tbl_summary()    ### creating a table summary

Know <- as.data.frame(Know) #### we must convert the sdf object into data frame for us to use flextable

my_know  <- flextable(Know) %>%
  save_as_docx(Know, path = "know.docx")
  
library(flextable)


### SECTION C
### ASSESSING THE PRACTICE
prac<-single_responses %>% 
  select(have_you_ever_been_tested_for_hepatitis_b, if_yes_when_was_your_last_test,
         if_you_are_diagnosed_with_hepatitis_b_would_you_go_for_further_investigation_and_treatment, have_you_been_vaccinated_against_hepatitis_b,
         if_yes_how_many_doses, Use_shared_needles_or_syringes, tattoos_or_piercings_from_unlicensed_facilities,
         Share_personal_items_eg_razors_toothbrushes, Unprotected_sexual_intercourse,how_would_you_rate_your_knowledge_about_hepatitis_b, do_you_think_hepatitis_b_awareness_programs_are_necessary_in_universities,
         are_you_willing_to_share_information_about_hepatitis_b_with_your_friends_and_family) %>%  # keep only the columns of interest
  tbl_summary()    ### creating a table summary

prac <- as.data.frame(prac) #### we must convert the sdf object into data frame for us to use flextable

my_prac  <- flextable(prac) %>%
  save_as_docx(prac, path = "prac.docx")

####  I need to export the data to use it for spss, my battery is low
export(multiple_responses, "multiple_responses.csv")

####COMBINATION ANALYSIS for information source
combo <- import(here("combination2.sav"))
here()



  
  
  pacman::p_load(
    tidyverse,     # data management and visualization
    UpSetR,        # special package for combination plots
    ggupset)       # special package for combination plots
  
  
  ### we can as well do a manual cleaning, see the code below
  # CLEANING 'PIPE' CHAIN (starts with raw data and pipes it through cleaning steps)
  ##################################################################################
  combo <- combo %>%
    
    # standardize column name syntax
    janitor::clean_names() %>% 
    
    # manually re-name columns
    # NEW name             # OLD name
    rename(
           "abdominal pain" = abdominalpain,
           "nausea and vomiting" = nauseaandvomiting,
        )
 
  
  
  # create column with the symptoms named, separated by semicolons
combi1<- combi %>%
mutate(SchoolUniversity = ifelse(SchoolUniversity == "1", "SchoolUniversity", NA),
  HealthProfessionals = ifelse(HealthProfessionals == "1", "HealthProfessionals", NA),
         FamilyFriends = ifelse(FamilyFriends == "1", "FamilyFriends", NA),
          MediaTVRadioInternet= ifelse(MediaTVRadioInternet == "1", "MediaTVRadioInternet", NA),
         Other = ifelse(Other == "1", "Other", NA))
         

combi1<- combi1 %>% 
  unite(col = "Information_source",
        c(SchoolUniversity, HealthProfessionals, FamilyFriends, MediaTVRadioInternet, Other), 
        sep = "; ",
        remove = TRUE,
        na.rm = TRUE) %>% 
  mutate(
    # make a copy of all_symptoms column, but of class "list" (which is required to use ggupset() in next step)
    Information_source_list = as.list(strsplit(Information_source, "; "))
  )


ggplot(
  data = combi1,
  mapping = aes(x = Information_source_list)) +
  geom_bar() +
  scale_x_upset(
    reverse = FALSE,
    n_intersections = 10,
    sets = c("SchoolUniversity", "HealthProfessionals", "FamilyFriends", "MediaTVRadioInternet", "Other"))+
  labs(
    title = "Sources of Information about HBV",
    subtitle = "10 most frequent combinations of sources of information",
    caption = "Caption here.",
    x = "Sources combination",
    y = "Frequency in dataset")


combi2<- combi %>%
  mutate(SchoolUniversity = ifelse(SchoolUniversity == "1", 1, 0),
         HealthProfessionals = ifelse(HealthProfessionals == "1", 1, 0),
         FamilyFriends = ifelse(FamilyFriends == "1", 1, 0),
         MediaTVRadioInternet= ifelse(MediaTVRadioInternet == "1", 1, 0),
         Other = ifelse(Other == "1", 1, 0))

# Efficiently convert "yes" to 1 and 0, but our dataset is already in 1s and 0s so, no need to run this code
combi2 <- combi %>% 
  
  # convert the "yes" and "no" values into 1s and 0s
  mutate(across(c(SchoolUniversity, HealthProfessionals, FamilyFriends, MediaTVRadioInternet, Other), .fns = ~+(.x == "yes")))

# Make the plot

combi2 %>% 
  UpSetR::upset(
    sets = c("SchoolUniversity", "HealthProfessionals", "FamilyFriends", "MediaTVRadioInternet", "Other"),
    order.by = "freq",
    sets.bar.color = c("blue", "red", "yellow", "darkgreen", "orange"), # optional colors
    empty.intersections = "on",
    # nsets = 3,
    nintersects = 10,   # number of bars to plot, in this case, plot the top 10
    number.angles = 0,
    point.size = 3.5,
    line.size = 2, 
    mainbar.y.label = "Sources combination",
    sets.x.label = "Particpants sources of information")



#### combination analysis of why they have not been tested


# create column with the symptoms named, separated by semicolons
combi1<- combi %>%
  mutate(insufficientmoney_A = ifelse(insufficientmoney_A == "1", "insufficientmoney_A", NA),
         lackofinformation = ifelse(lackofinformation == "1", "lackofinformation", NA),
         lackoftime = ifelse(lackoftime == "1", "lackoftime", NA),
         fearofpositivetest= ifelse(fearofpositivetest == "1", "fearofpositivetes", NA),
         Idontknow_A = ifelse(Idontknow_A == "1", "Idontknow_A", NA),
         notconsiderednecessary = ifelse(notconsiderednecessary == "1", "notconsiderednecessary", NA))


combi1<- combi1 %>% 
  unite(col = "Reasons_untested",
        c(insufficientmoney_A, lackofinformation, lackoftime, fearofpositivetest, Idontknow_A, notconsiderednecessary), 
        sep = "; ",
        remove = TRUE,
        na.rm = TRUE) %>% 
  mutate(
    # make a copy of all_symptoms column, but of class "list" (which is required to use ggupset() in next step)
    Reason_untested_list = as.list(strsplit(Reasons_untested, "; "))
  )


ggplot(
  data = combi1,
  mapping = aes(x = Reason_untested_list)) +
  geom_bar() +
  scale_x_upset(
    reverse = FALSE,
    n_intersections = 10,
    sets = c("insufficientmoney_A", "lackofinformation", "lackoftime", "fearofpositivetest", "Idontknow_A", "notconsiderednecessary"))+
  labs(
    title = "Reasons for not testing for HBV",
    subtitle = "10 most frequent combinations of Reasons for not testing for HBV",
    caption = "Caption here.",
    x = "Reasons combination",
    y = "Frequency in dataset")


combi2<- combi %>%
  mutate(insufficientmoney_A = ifelse(insufficientmoney_A == "1", 1, 0),
         lackofinformation = ifelse(lackofinformation == "1", 1, 0),
         lackoftime = ifelse(lackoftime == "1", 1, 0),
         fearofpositivetest = ifelse(fearofpositivetest == "1", 1, 0),
         Idontknow_A = ifelse(Idontknow_A == "1", 1, 0),
         notconsiderednecessary = ifelse(notconsiderednecessary == '1', 1, 0))

# Efficiently convert "yes" to 1 and 0, but our dataset is already in 1s and 0s so, no need to run this code
combi2 <- combi %>% 
  
  # convert the "yes" and "no" values into 1s and 0s
  mutate(across(c(SchoolUniversity, HealthProfessionals, FamilyFriends, MediaTVRadioInternet, Other), .fns = ~+(.x == "yes")))

# Make the plot

combi2 %>% 
  UpSetR::upset(
    sets = c("insufficientmoney_A", "lackofinformation", "lackoftime", "fearofpositivetest", "Idontknow_A", "notconsiderednecessary"),
    order.by = "freq",
    sets.bar.color = c("blue", "red", "yellow", "darkgreen", "orange", "purple"), # optional colors
    empty.intersections = "on",
    # nsets = 3,
    nintersects = 12,   # number of bars to plot, in this case, plot the top 12
    number.angles = 0,
    point.size = 3.5,
    line.size = 2, 
    mainbar.y.label = "Reason combination",
    sets.x.label = "Particpants reasons for not testing for HBV")




#### combination analysis of why they have not been Vaccinated


# create column with the symptoms named, separated by semicolons
combi1<- combi %>%
  mutate(unawareofthevaccine = ifelse(unawareofthevaccine == "1", "iunawareofthevaccine", NA),
         fearofneedles = ifelse(fearofneedles == "1", "fearofneedles", NA),
         insufficientmoney = ifelse(insufficientmoney == "1", "insufficientmoney", NA),
         fearofsideeffects = ifelse(fearofsideeffects == "1", "fearofsideeffects", NA),
         Idontknow = ifelse(Idontknow == "1", "Idontknow", NA),
         Notconsiderednecessary_A = ifelse(Notconsiderednecessary_A == "1", "Notconsiderednecessary_A", NA))


combi1<- combi1 %>% 
  unite(col = "Reasons_unvaccinated",
        c(unawareofthevaccine, fearofneedles, insufficientmoney, fearofsideeffects, Idontknow, Notconsiderednecessary_A), 
        sep = "; ",
        remove = TRUE,
        na.rm = TRUE) %>% 
  mutate(
    # make a copy of all_symptoms column, but of class "list" (which is required to use ggupset() in next step)
    Reason_unvaccinated_list = as.list(strsplit(Reasons_unvaccinated, "; "))
  )


ggplot(
  data = combi1,
  mapping = aes(x = Reason_unvaccinated_list)) +
  geom_bar() +
  scale_x_upset(
    reverse = FALSE,
    n_intersections = 10,
    sets = c("unawareofthevaccine", "fearofneedles", "insufficientmoney", "fearofsideeffects", "Idontknow", "Notconsiderednecessary_A"))+
  labs(
    title = "Reasons for not being vaccinated for HBV",
    subtitle = "10 most frequent combinations of Reasons for not being vaccinated for HBV",
    caption = "Caption here.",
    x = "Reasons combination",
    y = "Frequency in dataset")


combi2<- combi %>%
  mutate(unawareofthevaccine = ifelse(unawareofthevaccine == "1", 1,0),
         fearofneedles = ifelse(fearofneedles == "1", 1,0),
         insufficientmoney = ifelse(insufficientmoney == "1",1,0),
         fearofsideeffects = ifelse(fearofsideeffects == "1", 1,0),
         Idontknow = ifelse(Idontknow == "1", 1,0),
         Notconsiderednecessary_A = ifelse(Notconsiderednecessary_A == "1",  1,0))

# Efficiently convert "yes" to 1 and 0, but our dataset is already in 1s and 0s so, no need to run this code
combi2 <- combi %>% 
  
  # convert the "yes" and "no" values into 1s and 0s
  mutate(across(c(SchoolUniversity, HealthProfessionals, FamilyFriends, MediaTVRadioInternet, Other), .fns = ~+(.x == "yes")))

# Make the plot

combi2 %>% 
  UpSetR::upset(
    sets = c("unawareofthevaccine", "fearofneedles", "insufficientmoney", "fearofsideeffects", "Idontknow", "Notconsiderednecessary_A"),
    order.by = "freq",
    sets.bar.color = c("blue", "red", "yellow", "darkgreen", "orange", "purple"), # optional colors
    empty.intersections = "on",
    # nsets = 3,
    nintersects = 12,   # number of bars to plot, in this case, plot the top 12
    number.angles = 0,
    point.size = 3.5,
    line.size = 2, 
    mainbar.y.label = "Reason combination",
    sets.x.label = "Particpants reasons for not being vaccinated")






# create column with the symptoms named, separated by semicolons
combo1<- combo %>%
  mutate(`abdominal pain` = ifelse(`abdominal pain` == "1", "abdominal pain", NA),
        fever = ifelse(fever == "1", "fever", NA),
         fatigue = ifelse(fatigue == "1", "fatigue", NA),
         `nausea and vomiting` = ifelse(`nausea and vomiting` == "1", "nausea and vomiting", NA),
         jaundice = ifelse(jaundice == "1", "jaundice", NA)
        )


combo1<- combo1 %>% 
  unite(col = "Symptoms combination",
        c(`abdominal pain`, fever, fatigue, jaundice, `nausea and vomiting`), 
        sep = "; ",
        remove = TRUE,
        na.rm = TRUE) %>% 
  mutate(
    # make a copy of all_symptoms column, but of class "list" (which is required to use ggupset() in next step)
    Symptoms_combination_list = as.list(strsplit(`Symptoms combination`, "; "))
  )


ggplot(
  data = combo1,
  mapping = aes(x = Symptoms_combination_list)) +
  geom_bar() +
  scale_x_upset(
    reverse = FALSE,
    n_intersections = 10,
    sets = c("abdominal pain", "fever", "fatigue", "jaundice", "nausea and vomiting"))+
  labs(
    title = "Common symptoms of HBV as  reported by the participants",
    subtitle = "10 most frequent combinations of symptoms",
    caption = "Caption here.",
    x = "Symptoms combination",
    y = "Frequency")


combo2<- combo %>%
  mutate(`abdominal pain` = ifelse(`abdominal pain` == "1", 1, 0),
         fever = ifelse(fever == "1", 1, 0),
         fatigue = ifelse(fatigue == "1", 1, 0),
         `nausea and vomiting` = ifelse(`nausea and vomiting` == "1", 1, 0),
         jaundice = ifelse(jaundice == "1", 1, 0)
)
# Efficiently convert "yes" to 1 and 0, but our dataset is already in 1s and 0s so, no need to run this code
combo2 <- combo %>% 
  
  # convert the "yes" and "no" values into 1s and 0s
  mutate(across(c(SchoolUniversity, HealthProfessionals, FamilyFriends, MediaTVRadioInternet, Other), .fns = ~+(.x == "yes")))

# Make the plot

combo2 %>% 
  UpSetR::upset(
    sets = c("abdominal pain", "fever", "fatigue", "jaundice", "nausea and vomiting"),
    order.by = "freq",
    nintersects = 10,   # number of bars to plot, in this case, plot the top 10
    sets.bar.color = c("red", "yellow", "blue", "grey", "darkgreen"), # optional colors
    empty.intersections = "off",
    nsets = 3,
    number.angles = 0,
    point.size = 3.5,
    line.size = 1.5, 
    mainbar.y.label = "Symptoms combination",
    sets.x.label = "knowledge of symptoms by participants")


