## Digital Justice Lab, Dartmouth College
## Eugenics Rubicon Project
## Jaqueline Wernimont, Director
## name: Kirby Phares
## date: Summer 2019

## This file is executes all data wrangling needed to produce the figures within ERP_figures.R
## The code writes a new dataset from the transformed intial data called "ER_data_tidy.csv" 

# Initial Settings --------------------------------------------------------

# This loads tidyverse, ggthemes, and gdata packages to be used further on 
# If not already installed, install these packages by going to "Tools" on RStudio menu bar 
# and selecting "Install Packages"

library(tidyverse)
library(ggthemes)
library(gdata)

# Imports California Sterlization data 
# Converts into into a dataframe in preparation for data manipulation
file <- "data/CaliforniaSterilization1.csv"
df <- read.csv(file, stringsAsFactors = FALSE)


# Unchecked/Checked Conversion and Renaming Diagnoses ---------------------------------------------------

# key is assigned to the string to shorten the code by not having to repeat the long string
key <- "Diagnosis..check.all.that.apply....choice."

# Change responses to 1/0 from checked/unchecked
# Using key, shortens and renames diagnoses columns 
# Gathers data from long form to short form for institution and who consented
# Filters out rows with blanks superintendent column
# Filters out rows where consent was not given
# Factors and labels who consented to correct values
# Selects rows where the diagnosis is yes 
# Relevant rows select for further data manipulation
df1 <- df %>% 
  mutate_all(list( ~str_replace(., "Checked", "Yes"))) %>% 
  mutate_all(list( ~str_replace(., "Unchecked", "No"))) %>% 
  rename(Alcoholic = paste0(key, "Alcoholic."),
         Catatonic = paste0(key,"Catatonic."),
         Blind  = paste0(key, "Blind."),
         Dementia = paste0(key, "Dementia.Praecox."),
         Drug_Addiction = paste0(key, "Drug.Addict."),
         Epileptic = paste0(key, "Epileptic."),
         Feebleminded = paste0(key, "Feebleminded...Mental.Deficiency."),
         Paralysis = paste0(key, "General.Paralysis."),
         Hebephrenic = paste0(key, "Hebephrenic."),
         Insane = paste0(key, "Insane."),
         Lunatic = paste0(key, "Lunatic."),
         Manic_Depressive = paste0(key, "Manic.Depressive."),
         Mute = paste0(key,"Mute.or.other.speech.pathology."),
         Narcotic = paste0(key, "Narcotic."),
         Nervous = paste0(key, "Nervous...anxious."),
         Paranoia = paste0(key, "Paranoia.or.paranoid.condition."),
         Physically_Negative = paste0(key, "Physically.negative."),
         Physically_Normal = paste0(key, "Physically.normal."),
         Psychosis = paste0(key, "Psychosis."),
         Schizophrenic = paste0(key, "Schizophrenic."),
         Sexual_Psychopath = paste0(key, "Sexual.psychopath."),
         Syphilitic = paste0(key, "Syphilitic."),
         Other_STD = paste0(key, "Other.sexually.transmitted.disease."),
         Suicidal = paste0(key, "Suicidal."),
         Voluntary = paste0(key, "Voluntary."),
         Other = paste0(key, "Other."),
         None_Stated = paste0(key, "None.Stated.")) %>% 
  gather(key = Institution, value = Superintendent, 4:15, na.rm = TRUE) %>% 
  gather(key = WhoConsented, value = Consent_YN, 33:42, na.rm = TRUE) %>%
  filter(Superintendent != "") %>% 
  filter(Consent_YN != "Unchecked")  %>% 
  mutate(WhoConsented = factor(WhoConsented,
                               levels = c("Who.consented....choice.Mother.", 
                                          "Who.consented....choice.Father.",
                                          "Who.consented....choice.Brother.",
                                          "Who.consented....choice.Sister.",
                                          "Who.consented....choice.Spouse.",
                                          "Who.consented....choice.Extended.family.member.",
                                          "Who.consented....choice.Medical.Staff.",
                                          "Who.consented....choice.Parole.Officer.",
                                          "Who.consented....choice.Other.",
                                          "Who.consented....choice.Not.stated."),
                               labels = c("Mother",
                                          "Father",
                                          "Brother",
                                          "Sister",
                                          "Spouse",
                                          "Extended_family_member",
                                          "Medical_staff",
                                          "Parole_officer",
                                          "Other",
                                          "Not_stated"))) %>% 
  
  gather(key = "Diagnosis", value = "Y/N", Alcoholic:Other.Diagnosis..) %>% 
  filter(`Y/N` != "No") %>% 
  select(Record.ID,
         Name.of.institution,
         Superintendent,
         Acting.Assistant.Superintendent.,
         Consent.given..,
         Diagnosis,
         WhoConsented,
         Date.sterilization.request.made.by.superintendent,
         Enter.Date.Approved) 

# Further manipulation of df1
# Reasoning:
# In the original "Superintendent" column, names of one superintendent appeared in multiple variations
# so this code is to find commonalities and change it to a single name so that all data would be attributed
# to that person instead of being spread among different aliases. 
# The code changes the names quickly in R, instead of changing names one by one in Excel for ex.

# Mutate creates a new column "superintendent" from "Superintendent" 
# Grepl is a function that search for matches to an argument pattern and returns True or False 
# if the pattern matches
# Within mutate, the ifelse statement takes advantage of the T/F that grepl returns to 
# replace the name if it matches the pattern given to a common name. Else, the name remains as it appears.

# Grepl searches the "Superintendent" column for patterns and mutate changes the new, 
# created column "superintendent

# The last line of the chunk works similarly, but mutates the "Diagnosis" column 
# The word "Other" also had multiple variations, so everything that matched the pattern 
# was changed to "Other

df1 <- df1 %>% 
  mutate(superintendent = ifelse(grepl("Scan", Superintendent), "J.M. Scanland", Superintendent),
         superintendent = ifelse(grepl("Mull", Superintendent), "E.W. Mullen", superintendent),
         superintendent = ifelse(grepl("gate", Superintendent), "C.F. Applegate", superintendent),
         superintendent = ifelse(grepl("Harr", Superintendent), "H.V. Harr", superintendent),
         superintendent = ifelse(grepl("Hya", Superintendent), "H.W. Hyatt", superintendent),
         superintendent = ifelse(grepl(".ond", Superintendent), "S.B. Pond", superintendent),
         superintendent = ifelse(grepl("L.+erman", Superintendent), "Daniel Liberman", superintendent),
         superintendent = ifelse(grepl(".ald.Smith", Superintendent), "Donald Smith", superintendent),
         superintendent = ifelse(grepl("A.John.", Superintendent), "C.A. Johnson", superintendent),
         superintendent = ifelse(grepl("Siss", Superintendent), "C.E. Sisson", superintendent),
         superintendent = ifelse(grepl("Rapa", Superintendent), "Walter Rapaport", superintendent),
         superintendent = ifelse(grepl("Hag", Superintendent), "Thomas Hagerty", superintendent),
         superintendent = ifelse(grepl("Rand", Superintendent), "S.M. Rand", superintendent),
         superintendent = ifelse(grepl("kson", Superintendent), "S.H. Frederickson", superintendent),
         superintendent = ifelse(grepl("vey", Superintendent), "Herman W. Convey", superintendent),
         superintendent = ifelse(grepl("C.+Johnson", Superintendent), "C.A. Johnson", superintendent),
         superintendent = ifelse(grepl("aina", Superintendent), "S.F. Casalaina", superintendent),
         superintendent = ifelse(grepl("Leonard", Superintendent), "Thomas Leonard", superintendent),
         superintendent = ifelse(grepl("Apple", Superintendent), "C.F. Applegate", superintendent),
         superintendent = ifelse(grepl("yers", Superintendent), "Rober E. Wyers", superintendent),
         superintendent = ifelse(grepl("Rood", Superintendent), "R. Rood", superintendent),
         superintendent = ifelse(grepl("oller", Superintendent), "R.B. Toller", superintendent),
         superintendent = ifelse(grepl("Rowe", Superintendent), "M.J. Rowe", superintendent),
         superintendent = ifelse(grepl("Apple", Superintendent), "C.F. Applegate", superintendent),
         superintendent = ifelse(grepl("ker", Superintendent), "Herman Tucker", superintendent),
         superintendent = ifelse(grepl("Cut.", Superintendent), "J.A. Cutting", superintendent),
         superintendent = ifelse(grepl("Crow", Superintendent), "John J. Crowley", superintendent),
         superintendent = ifelse(grepl("Louis.+ash", Superintendent), "Louis R. Nash", superintendent),
         superintendent = ifelse(grepl("Williamson", Superintendent), "N.E. Williamson", superintendent),
         superintendent = ifelse(grepl("Charlesworth", Superintendent), "I.E. Charlesworth", superintendent),
         superintendent = ifelse(grepl("Shannon", Superintendent), "G.W. Shannon", superintendent),
         superintendent = ifelse(grepl("Garrett", Superintendent), "F.H. Garrett", superintendent),
         superintendent = ifelse(grepl("Twogood", Superintendent), "E.G. Twogood", superintendent),
         superintendent = ifelse(grepl("ald.+Smith", Superintendent), "Donald R. Smith", superintendent),
         superintendent = ifelse(grepl("Caulkins", Superintendent), "C.C. Caulkins", superintendent),
         superintendent = ifelse(grepl("Mat.+ews", Superintendent), "A.C. Matthews", superintendent),
         superintendent = ifelse(grepl("Kiser", Superintendent), "A.E. Kiser", superintendent),
         superintendent = ifelse(grepl("Williamson", Superintendent), "N.E. Williamson", superintendent),
         superintendent = ifelse(grepl("LeBaron", Superintendent), "R.O. LeBaron", superintendent),
         superintendent = ifelse(grepl("Cushman", Superintendent), "R.A Cushamn", superintendent),
         superintendent = ifelse(grepl("Ritchey", Superintendent), "R.M. Ritchey", superintendent),
         superintendent = ifelse(grepl("R.+illy", Superintendent), "J.A. Reilly", superintendent),
         superintendent = ifelse(grepl("Freeman.+Adam", Superintendent), "Freeman Adams", superintendent),
         superintendent = ifelse(grepl("F.+William", Superintendent), "Frank Williams", superintendent),
         superintendent = ifelse(grepl("Johnstone", Superintendent), "J.C. Johnstone", superintendent),
         superintendent = gsub("\\(.+\\)", "", superintendent)) %>% 
  mutate(Diagnosis = ifelse(grepl("Other", Diagnosis), "Other", Diagnosis))

# Table function lists every item in the column and was used to find the various strings 
# which should match one person or diagnosis
# Also was used as a way to check that the above code worked when different versions no longer appeared
table(df1$Superintendent)
table(df1$Diagnosis)

# Uses the dataframe that was created from the above manipulation to write a new .csv file
# Writing a new file of the "tidy" dataframe allows for usage of this data frame 
# without having to copy and paste the above code when the tidy data is needed
write.csv(df1, "data/ER_data_tidy.csv")

