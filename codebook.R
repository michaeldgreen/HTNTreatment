library(dataReporter)
library(tidyverse)

# loading raw NAMCS data which I converted to csv
data_raw <- read.csv("Data/NAMCS2019.csv")

# loading pdf file of 20 pages from NAMCS data documentation that listed the key for the different medical codes
medcodes_raw <- read.csv("Data/NAMCS Drug Entry Codes.csv")

data_clean <- data_raw %>%
  select(Age = AGE, 
         `Race and Ethnicity` = RACERETH, 
         `Smoking History` = EVERTOBAC, 
         `Major Reason for Visit` = MAJOR, 
         `Primary Reason for Visit` = RFV1, 
         `Secondary Reason for Visit` = RFV2, 
         `Primary Care` = PRIMCARE, 
         Hypertension = HTN, 
         `Chronic Kidney Disease` = CKD, 
         `Congestive Heart Failure` = CHF, 
         `Diabetes Type 1` = DIABTYP1, 
         `Diabetes Type 2` = DIABTYP2, 
         Obesity = OBESITY, 
         `Systolic Blood Pressure` = BPSYS, 
         Medication = MED, 
         `Number Medications` = NUMMED, 
         `Time with Doctor` = TIMEMD, 
         `Recommended for ED or ER` = ERADMHOS)

#Telling RStudio to treat these continuous variables as numeric
data_clean <- data_clean %>%
  mutate(Age = as.numeric(Age),
         `Number Medications` = as.numeric(`Number Medications`),
         `Time with Doctor` = as.numeric(`Time with Doctor`),
         `Systolic Blood Pressure` = as.numeric(`Systolic Blood Pressure`))

#Reducing the amount of category options for reason for visi (originally above 80), then dropping unknown and blank values for Smoking History, Primary Care, and Medication variables
data_clean <- data_clean %>%
  filter(`Smoking History` != "Unknown") %>%
  filter(!(`Primary Care` %in% c("Blank", "Unknown"))) %>%
  filter(`Medication` != "Entire item blank") %>%
  mutate(`Primary Reason for Visit` = if_else(`Primary Reason for Visit` %in% c("Hypertension", "Blood pressure test"), 
                                              `Primary Reason for Visit`, "other"),
         `Secondary Reason for Visit` = if_else(`Secondary Reason for Visit` %in% c("Hypertension", "Blood pressure test"), 
                                                `Secondary Reason for Visit`, "other"))

#Making the variables readable and removing excess space on observations
medcodes_clean <- medcodes_raw %>%
  select(Medication.ID, Medication.Name) %>%
  rename(`Rx ID Number` = Medication.ID, `Rx Name` = Medication.Name) %>%
  mutate(`Rx ID Number` = trimws(`Rx ID Number`),
         `Rx Name` = trimws(`Rx Name`))

makeCodebook(data_clean, vol = "", reportTitle = NULL, file = NULL)
makeCodebook(medcodes_clean, vol = "", reportTitle = NULL, file = NULL)

