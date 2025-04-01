#################
#Workshop Script#
#################

#This is where you write the code.
#Run the code in the console by putting cursor on the line to run and pressing run or using control enter

##The install.packages functions have been hashed out as you have already installed these
#install.packages("AMR")
#install.packages("tidyverse")
#install.packages("readxl")
#install.packages("cleaner")

#Load in the libraries
library(AMR)
library(tidyverse)
library(readxl)
library(cleaner)

#Read in the campylobacter csv file
campy <- read_csv("data/Campylobacter_117v2.csv")

#Read in the ecoli excel sheet
ecoli <- read_excel("data/bh_AMR_Surveillance_datav2.xlsx", sheet = "E.coli")

#Chain together three operations to tidy your data set                    
ecoli_select <- ecoli %>% 
        select(!starts_with("SIR")) %>% #select all columns but not (!) those ones that start with SIR
        setNames(c("meat_shop", "stratification", "date_collection", "sample_type", #Use the setNames function to rename the columns
                   "sample_collection", "sample_weight_Kg", "sample_source",
                   "farm_category", "slaughter", "transportation", "transport_medium",
                   "transport_with_other_meat", "GEN", "CHL",
                   "MEM", "CRO", "CIP", "FEP", "NAL", "AMP", "TCY", "SXT"))  %>%
        mutate(species = "Escherichia coli") # add in a column called species
str(ecoli_select)

#Change to date format (year-month-day) (using lubridate library)
ecoli_select$date_collection <- ymd(ecoli_select$date_collection)

#Change some of the values in the meat_shop column
ecoli_select <- ecoli_select %>%
        mutate(meat_shop = str_replace_all(meat_shop, "\\.", ""),  # Remove all dots
               meat_shop = str_replace(meat_shop, "Bebak MS", "BK MS")) #Replace Bebak MS with BK MS

#Change all the values in the farm category column to title case
ecoli_select <- mutate(ecoli_select, farm_category = str_to_title(farm_category))

#Read in the enterococci data sheet
enterococci <- read_excel("data/bh_AMR_Surveillance_datav2.xlsx", sheet = "Enterococci")

#Remove columns starting with SIR, change column names and add a new column with species                    
enterococci_select <- enterococci%>% 
        select(!starts_with("SIR")) %>%
        setNames(c("meat_shop", "stratification", "date_collection", "sample_type",
                   "sample_collection", "sample_weight_Kg", 
                   "sample_source", "farm_category", "slaughter", "transportation", 
                   "transport_medium", "transport_with_other_meat", 
                   "CHL", "AMP", "TCY", "ERY", "TGC", "QDA", "VAN", "LNZ")) %>%
        mutate(meat_shop = str_replace_all(meat_shop, "\\.", ""),  # Remove all dots
               meat_shop = str_replace(meat_shop, "Bebak MS", "BK MS"),
               farm_category = str_to_title(farm_category)) %>%
        mutate(species = "Enterococcus sp.")

#Read in Salmonella data sheet
salmonella <- read_excel("data/bh_AMR_Surveillance_datav2.xlsx", sheet = "Salmonella")

#Remove columns starting with SIR, change column names and add a new column with species                    
salmonella_select <- salmonella%>% 
        select(!starts_with("SIR"), -c(1, 15, 34:35)) %>% #keep all columns but not (!) those starting with SIR & remove columns 1,15, 34:35
        setNames(c("meat_shop", "stratification",  
                   "date_collection", "sample_type", "sample_collection", "sample_weight_Kg", 
                   "sample_source", "farm_category", "slaughter", "transportation", 
                   "transport_medium", "transport_with_other_meat", "GEN",
                   "CHL", "MEM", "CRO", "CIP", "NAL", "AMP", "TCY", "SXT", "ETP")) %>%
        mutate(meat_shop = str_replace_all(meat_shop, "\\.", ""),  # Remove all dots
               meat_shop = str_replace(meat_shop, "Bebak MS", "BK MS"),
               farm_category = str_to_title(farm_category)) %>%
        mutate(species = "Salmonella sp.")

#Change the date collection column to data type "date" in year-month-day (ymd) format
enterococci_select$date_collection <- ymd(enterococci_select$date_collection)
salmonella_select$date_collection <- ymd(salmonella_select$date_collection)

#Bind the ecoli, salmonella and enterococci data frames together
df <- bind_rows(ecoli_select, salmonella_select, enterococci_select) %>%
        select(species, 1:29) #select species first then remainder of columns

#modify columns GEN through to LNZ to disk data type
df2 <- df %>%
        mutate(across(GEN:LNZ, as.disk))


#Generate SIR 
df3 <- df2 %>% mutate(across(GEN:LNZ, ~ .x, .names = "{.col}_disk")) %>% #copy disk diffusion values to new columns
        mutate(across(GEN:LNZ, ~ as.sir(.x, guideline = "CLSI"))) #modify columns GEN through to LNZ to SIR using the CLSI guideline

#modify columns GEN through to SXT to disk data type in the ecoli_select dataframe
ecoli2 <- ecoli_select %>%
        mutate(across(GEN:SXT, as.disk))

#To copy disk diffusion values to new columns and convert to SIR
ecoli3 <- ecoli2 %>% mutate(across(GEN:SXT, ~ .x, .names = "{.col}_disk")) %>% #across the GEN:SXT columns copy values to new columns (~ defines an anonymous function, .x is placeholder for the column values )
        mutate(across(GEN:SXT, ~ as.sir(.x, guideline = "CLSI")))
