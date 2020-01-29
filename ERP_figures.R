## Digital Justice Lab, Dartmouth College
## Eugenics Rubicon Project
## Jaqueline Wernimont, Director
## Name: Kirby Phares
## Date: Summer 2019
## Received help from Professor Yusaku Horiuchi at Dartmouth College on df_Other code


# Initial Settings --------------------------------------------------------

# This loads tidyverse, ggthemes, gdata, lubridate, ggrepl, and RColorBrewer packages to be 
# used further on 
# If not already installed, install these packages by going to "Tools" on RStudio menu bar 
# and selecting "Install Packages"
library(tidyverse)
library(ggthemes)
library(gdata)
library(lubridate)
library(ggrepel)
library(RColorBrewer)

# Imports ER_data_tidy.csv which was created in Data Cleaning.R
# Converts into into a dataframe in preparation for data manipulation
file <- "data/ER_data_tidy.csv"
df <- read.csv(file, stringsAsFactors = FALSE)


# Dataframe Creation for Figures ------------------------------------------

# df_Superintendent shows the total the break down of type of diagnosis 
# and the number of each diagnoses approved by each superintendent by year

# A new column "date" is created by changing "Enter.Date.Approved" into a readable date format 
# (before "Enter.Date.Approved" was a string)
# Year.approved created which only takes the year value of "date"
# A grouped table created in the background consisting so that operations can be performed 
# "n" column calculated by the number of different types of diagnoses per year for each superintendent
# Ungroup undos the group_by function to avoid potential unintended errors due to the grouping
# Only the rows are selected of which the year is greater than 1912 and less than 1970
# A new column created that is the total of all patients for each institution

df_Superintendent <- df %>% 
  mutate(date = ymd(Enter.Date.Approved)) %>% 
  mutate(year.approved = year(date)) %>% 
  group_by(Name.of.institution, superintendent, Diagnosis, year.approved) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  filter(year.approved >= 1912 & year.approved <= 1970) %>% 
  group_by(Name.of.institution) %>% 
  mutate(npatients = sum(n)) %>% 
  ungroup()


# df_Institution show the total number of patients for each superintendent 

# A grouped table  created in the background consisting so that operations can be performed
# A new column "date" created by changing "Enter.Date.Approved" into a readable date format 
# (before "Enter.Date.Approved" was a string)
# Year.approved created which only takes the year value of "date"
# Column "npatients" created which sums the total patients for each superintendent 
# Ungroup undos the group_by function to avoid potential unintended errors due to the grouping

df_Institution <- df %>% 
  group_by(Name.of.institution, superintendent) %>% 
  mutate(date = ymd(Enter.Date.Approved)) %>% 
  mutate(year.approved = year(date)) %>% 
  summarise(npatients = n()) %>% 
  ungroup()

# df_Complete_Institution is the combination of df_Superintendent and df_Institution

# df_Superintendent and df_Institution are joined and by "superintendent" and "Name.of.institution" 
# is specified because R will assume what to join by which might not result in what is expected
# Both dataframes contain "npatients" columns but refer to different things 
# so they are renamed to distinguish what the data represents

df_Complete_Institution <- right_join(df_Superintendent, df_Institution, 
                                      by = c("superintendent", "Name.of.institution")) %>% 
  rename(npatients_institution = npatients.x, 
          npatients_superintendent = npatients.y)


# Patients10k Figure ------------------------------------------------------

# Patients10k only selects superintendents who had more than 10000 patients
# With the number of different superintendents in the data, the figure is unreadable when including all

Patients10k <- df_Complete_Institution %>% 
  filter(npatients_superintendent >= 10000)

# Lables are used of each institution is used in the figure and creating a new dataframe 
# of only unique institution names are ket to make the lables crisp 
# Without this when the names of insitutions are printed on the figure, they are printed for each 
# appearance in the dataframe which creates blurry labels

Patients10k_labels <- Patients10k %>% 
  select(superintendent, npatients_superintendent, Name.of.institution) %>% 
  distinct()


# Number of Diagnoses Approved by Superintendents is created using Patients10k dataframe
# Na.omit and na.rm can be overkill, but ensures that all NAs in the data is removed
# Bar graph of snumber of diagnoses by superintendent, ordered by increasing values of number of patients
# Subsets the bars by the number and type of each diagnosis
# Stat = "identity" overrides bar graph's inclination to only have an x variable 
# so that "n" will be the y variable instead of "count" (default)
# Title, subtitle, x, and y labels created
# Institution labels placed above the superintendent bars
# Y axis steps by 25000 for easy reference
# Theme set
# Legend moved to bottom of figure
# Legend broken up so each "column" only has 3 rows

ggplot(na.omit(Patients10k), na.rm = TRUE) +
  geom_bar(aes(x = reorder(superintendent, npatients_superintendent),
               y = n,
               fill = Diagnosis),
           stat = "identity") +
  labs(title = "Number of Diagnoses Approved by Superintendents",
       subtitle = "Superintendents with > 10000 patients",
       x = "Superintendent",
       y = "Number of diagnoses") +
  geom_text(data = Patients10k_labels,
            aes(x = superintendent,
                y = npatients_superintendent + 1500,
                label = Name.of.institution),
            na.rm = TRUE) +
  scale_y_continuous(breaks = seq(0, 250000, by = 25000)) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(nrow = 3))

# figure saved in the file "figures" withing working directory folder, named, and sized
ggsave("figures/Number of Diagnoses Approved by Superintendents.png", width = 12, height = 7.5)



# "Other" Figures ---------------------------------------------------------

# df_Other takes df_Complete_Institution so that manipulation can be done 
# without changing df_Complete_Institution
# Rows arranged in descending order by value of n (# of diagnoses by type by year)
# Only rows selected whose institution has greater than 100000 patients
# Changes all diagnoses that are not listed as "Other", "Feebleminded", "Hebephrenic", "Dementia"
# to "All Other Diagnoses"
# Factors "Diagnosis" column so that data is correctly attributed to each diagnosis type because
# R by default sorts by alphabetical order
# "n_total" column created which is the sum of different diagnoses by institution and year
# Ungroup undos the group_by function to avoid potential unintended errors due to the grouping

df_Other <- df_Complete_Institution %>%
  arrange(desc(n)) %>%
  filter(npatients_institution >= 100000) %>%
  mutate(Diagnosis = ifelse(Diagnosis %in% c("Other", "Feebleminded", "Hebephrenic", "Dementia"), 
                            Diagnosis, "All Other Diagnoses")) %>% 
  mutate(Diagnosis = factor(Diagnosis, levels = c("Other", "Feebleminded", "Dementia", 
                                        "Hebephrenic", "All Other Diagnoses"))) %>% 
  group_by(Name.of.institution, year.approved, Diagnosis) %>% 
  summarise(n_total = sum(n)) %>%  ## Added by Prof. Horiuchi
  ungroup()


# Occurrences of Diagnoses figure created using df_Other dataframe
# Line graph of total number of diagnoses mapped by year
# Lines subsetted by type of diagnosis
# Size of line set
# Title, subtitle, x, and y labels created
# Facet separates data into different graphs for each institution (only 3 had > 100000 patients)
# Colors manually assigned to each diagnosis
# Theme set
# Legend characterics set
# Year (x axis) range set to 1920 - 1955

ggplot(df_Other)  +
  geom_line(aes(x = year.approved, 
                y = n_total, color = Diagnosis),
            size = .8) + 
  labs(x = "Year",
       y = "Number of Diagnoses",
       title = "Occurrences of Diagnoses as Reason for Forced Sterilization",
       subtitle = "Institutions with Greater than 100,000 Patients from 1920 to 1960"
  ) +
  facet_wrap(~Name.of.institution, ncol = 1) +
  scale_color_manual(values = c("indianred1", "royalblue1", "seagreen3", "orange", "grey"),
                       name = "Type", labels = c("Other", "Feebleminded", "Dementia", 
                                                 "Hebephrenic", "All Other Diagnoses")) +
  theme_minimal() +
  theme(legend.background = element_rect(linetype="solid", size = .4)) +
  guides(color = guide_legend(title = "Diagnosis")) +
  scale_x_continuous(limits = c(1920, 1955))

# Figure is saved in the file "figures" withing working directory folder, named, and sized
ggsave("figures/Occurrences of Diagnoses.png", width = 10, height = 7.5)

