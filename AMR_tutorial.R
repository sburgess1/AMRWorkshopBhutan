################
##AMR tutorial##
#https://msberends.github.io/AMR/articles/AMR.html#
################

library(AMR)
library(tidyverse)

our_data <- example_isolates_unclean
str(our_data)

#Transform microorganism names to current taxonomy
head(our_data$bacteria)
our_data$bacteria <- as.mo(our_data$bacteria, info = TRUE)
mo_uncertainties()

##Clean the SIR columns - convert from character to SIR type
#Method 1 select specific columns
#mutate_at applies function at specific column
#vars() defines a subset of columns in this case columns AMX through to GEN
#as.sir() converts raw MIC values
our_data <- our_data %>%
  mutate_at(vars(AMX:GEN), as.sir)
#Method 2 let the AMR package select eligible columns
our_data <- our_data %>%
  mutate_if(is_sir_eligible, as.sir)




df_wide <- data.frame(
  patient_id = c("J3", "J3","J3","J3"),
  date = c("2018-11-21", "2018-04-03", "2018-09-19", "2018-12-10"),
  isolate = c(1,2,3,4),
  microorganism = c("Escherichia coli", "Escherichia coli", "Escherichia coli", "Escherichia coli"),
  hospitalization_day = c(1,7,20,32),
  CLIN = c("S", "R", "R", "R"),
  ERY = c("S", "R", "R", "R"),
  GEN = c("S", "R", "S", "R"),
  CTX = c("R", "R", "R", "R"),
  OXA = c("S", "R", "R", "R"),
  VAN = c("S", "S", "S", "S")
  )

df_wide$date <- as.Date(df_wide$date)

#default points threshold is 2

df_wide1 <- df_wide %>%
  mutate(first_isolate(method = "phenotype-based"))

df_wide2 <- df_wide %>%
  mutate(first_isolate(type = "points", points_threshold = 1))

df_wide2 <- df_wide %>%
  mutate(first_isolate(type = "keyantimicrobials", points_threshold = 1))


#To only select one isolate per patient/sample the first_isolate() function is used.
#There are four different methods that can be used to remove duplicates using first_isolate() as described by  Hindler et al. in 2007 (doi:10.1086/511864 ). 
#A new column called "first_isolate" is generated which will contain the results of the first_isolate() function (TRUE or FALSE)
our_data <- our_data %>%
  mutate(first_isolate(first = first_isolate(info = TRUE)))

#Filter by first isolates = TRUE
our_data_1st <- our_data %>%
  filter(first = TRUE)

summary(our_data_1st)

#Generate frequency table
our_data_1st %>%
  count(bacteria)

#Generate frequency table with bacterial species full names
our_data_1st %>%
  count(mo_name(bacteria), sort = TRUE)

#Selecting specific columns
our_data_1st %>%
  select(date, aminoglycosides())

#Filtering for "R"
our_data_1st %>%
  filter(any(aminoglycosides() == "R"))

##Use example_isolates data to generate antibiograms

head(example_isolates)
antibiogram(example_isolates,
            antibiotics = c(aminoglycosides(), carbapenems()))
