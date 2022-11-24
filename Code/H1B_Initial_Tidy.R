library(tidyverse)
library(lubridate)
library(janitor)



H1B_Data_Raw <- read_csv("PERM_Disclosure_Data_FY2021.csv")
#view(H1B_Data_Raw)
#glimpse(H1B_Data_Raw)


H1B_Data_Raw <- H1B_Data_Raw %>%
  select(CASE_STATUS, RECEIVED_DATE, DECISION_DATE, REFILE, EMPLOYER_NAME,
         EMPLOYER_CITY, EMPLOYER_STATE_PROVINCE, FW_OWNERSHIP_INTEREST,
         PW_SOC_TITLE, PW_SKILL_LEVEL, PW_WAGE, WAGE_OFFER_FROM, WAGE_OFFER_TO, WAGE_OFFER_UNIT_OF_PAY,
         WORKSITE_CITY, WORKSITE_STATE, JOB_TITLE, MINIMUM_EDUCATION, REQUIRED_TRAINING, 
         REQUIRED_EXPERIENCE, ACCEPT_FOREIGN_EDUCATION, ACCEPT_ALT_OCCUPATION, 
         FOREIGN_LANGUAGE_REQUIRED, OFFERED_TO_APPL_FOREIGN_WORKER, FOREIGN_WORKER_LIVE_ON_PREM,
         FOREIGN_WORKER_LIVE_IN_DOM_SER, PROFESSIONAL_OCCUPATION, APP_FOR_COLLEGE_U_TEACHER,
         COUNTRY_OF_CITIZENSHIP, FOREIGN_WORKER_BIRTH_COUNTRY, CLASS_OF_ADMISSION,
         FOREIGN_WORKER_EDUCATION, FOREIGN_WORKER_INFO_MAJOR, FOREIGN_WORKER_YRS_ED_COMP,
         FOREIGN_WORKER_INST_OF_ED, FOREIGN_WORKER_ED_INST_CITY, FOREIGN_WORKER_ED_INST_STATE_P,
         FOREIGN_WORKER_ED_INST_COUNTRY, FOREIGN_WORKER_REQ_EXPERIENCE, FOREIGN_WORKER_ALT_ED_EXP,
         FOREIGN_WORKER_ALT_OCC_EXP, FOREIGN_WORKER_EXP_WITH_EMPL, FOREIGN_WORKER_EMPL_PAY_FOR_ED,
         FOREIGN_WORKER_CURR_EMPLOYED, EMPLOYER_COMPLETED_APPLICATION) 


#H1B_Data %>% tabyl(CASE_STATUS)
H1B_Data <- H1B_Data_Raw %>% #Creating Response Variable - NAs assigned to withdrawn
  mutate(CASE_STATUS = case_when(CASE_STATUS == "Certified" ~ 1,
                                 CASE_STATUS == "Certified-Expired" ~ 1,
                                 CASE_STATUS == "Denied" ~ 0))

H1B_Data <- H1B_Data %>% #Tidying Dates
  mutate(RECEIVED_DATE = mdy(RECEIVED_DATE), 
         DECISION_DATE = mdy(DECISION_DATE))


H1B_Data <- H1B_Data %>% #Tidying Wages
  mutate(PW_WAGE = parse_number(PW_WAGE), 
         WAGE_OFFER_TO = parse_number(WAGE_OFFER_TO),
         WAGE_OFFER_FROM = parse_number(WAGE_OFFER_FROM),
         WAGE_OFFER_TO = ifelse(is.na(WAGE_OFFER_TO), WAGE_OFFER_FROM, WAGE_OFFER_TO),
         wAGE_OFFER_AVG = (WAGE_OFFER_FROM + WAGE_OFFER_TO)/2) 

H1B_Data <- H1B_Data %>% #removing offer to/from
  select(1:11, wAGE_OFFER_AVG, 14:45) 


#H1B_Data %>% tabyl(PW_SKILL_LEVEL)
H1B_Data <- H1B_Data %>% #Tidying Skill Level
  mutate(PW_SKILL_LEVEL = case_when(PW_SKILL_LEVEL == "Level I" ~ 1,
                                    PW_SKILL_LEVEL == "Level II" ~ 2,
                                    PW_SKILL_LEVEL == "Level III" ~ 3,
                                    PW_SKILL_LEVEL == "Level IV" ~ 4,))

#Investigating Wage Offer Type and Case Status
#H1B_Data %>% tabyl(WAGE_OFFER_UNIT_OF_PAY, CASE_STATUS)
  #We may want to keep this variable as is and use factor()


#H1B_Data %>% tabyl(MINIMUM_EDUCATION)
H1B_Data<- H1B_Data %>% #Tidying Minimum Education
  mutate(MINIMUM_EDUCATION = case_when(!is.na(MINIMUM_EDUCATION) ~ MINIMUM_EDUCATION,
                                       TRUE ~ "None"))

#Investigating Current Admission Status
#H1B_Data %>% tabyl(CLASS_OF_ADMISSION)
  #Not sure what to do with this one - we may want to ignore it

#H1B_Data %>% tabyl(FOREIGN_WORKER_EDUCATION)
H1B_Data<- H1B_Data %>% #Tidying Foreign Worker Education
  mutate(FOREIGN_WORKER_EDUCATION = case_when(!is.na(FOREIGN_WORKER_EDUCATION) ~ FOREIGN_WORKER_EDUCATION,
                                       TRUE ~ "None"))

#H1B_Data %>% tabyl(FOREIGN_WORKER_ED_INST_US) 
H1B_Data <- H1B_Data %>% #creating educated within US variable
  mutate(FOREIGN_WORKER_ED_INST_US = case_when(FOREIGN_WORKER_ED_INST_COUNTRY == "UNITED STATES OF AMERICA" ~ 1,
                                               is.na(FOREIGN_WORKER_ED_INST_COUNTRY) ~ as.numeric(NA),
                                               TRUE ~ 0))

H1B_Data <- H1B_Data %>% #dropping other institution variables
  select(1:33, FOREIGN_WORKER_ED_INST_US, 37:44) 


#H1B_Data %>% tabyl(FOREIGN_WORKER_YRS_ED_COMP)
H1B_Data <- H1B_Data %>% #Tidying years education was completed variable
  mutate(FOREIGN_WORKER_YRS_ED_COMP = case_when(FOREIGN_WORKER_YRS_ED_COMP < 1950 ~ as.numeric(NA),
                                                FOREIGN_WORKER_YRS_ED_COMP <= 2022 ~ FOREIGN_WORKER_YRS_ED_COMP,
                                                FOREIGN_WORKER_YRS_ED_COMP == 2104 ~ 2004,
                                                FOREIGN_WORKER_YRS_ED_COMP == 2105 ~ 2005),
         FOREIGN_WORKER_YRS_SINCE_ED = (2021-FOREIGN_WORKER_YRS_ED_COMP)) #Creating Years since education completed variable
H1B_Data <- H1B_Data %>% select(1:32, FOREIGN_WORKER_YRS_SINCE_ED, 33:42)



#H1B_Data %>% tabyl(FOREIGN_WORKER_REQ_EXPERIENCE)
#H1B_Data %>% tabyl(FOREIGN_WORKER_ALT_ED_EXP)
#H1B_Data %>% tabyl(FOREIGN_WORKER_ALT_OCC_EXP)
  #All three of these have Not Applicable, Y, and N answers - maybe drop them 


#Changing simple Y/N answwers to 1's and 0's
H1B_Data <- H1B_Data %>% 
  mutate(REFILE = case_when(REFILE == "Y" ~ 1, 
                            REFILE == "N" ~ 0),
         FW_OWNERSHIP_INTEREST = case_when(FW_OWNERSHIP_INTEREST == "Y" ~ 1,
                                           FW_OWNERSHIP_INTEREST == "N" ~ 0),
         REQUIRED_TRAINING = case_when(REQUIRED_TRAINING == "Y" ~ 1,
                                       REQUIRED_TRAINING == "N" ~ 0),
         REQUIRED_EXPERIENCE = case_when(REQUIRED_EXPERIENCE == "Y" ~ 1,
                                         REQUIRED_EXPERIENCE == "N" ~ 0),
         ACCEPT_FOREIGN_EDUCATION = case_when(ACCEPT_FOREIGN_EDUCATION == "Y" ~ 1,
                                              ACCEPT_FOREIGN_EDUCATION == "N" ~ 0),
         ACCEPT_ALT_OCCUPATION = case_when(ACCEPT_ALT_OCCUPATION == "Y" ~ 1,
                                              ACCEPT_ALT_OCCUPATION == "N" ~ 0),
         FOREIGN_LANGUAGE_REQUIRED = case_when(FOREIGN_LANGUAGE_REQUIRED == "Y" ~ 1,
                                              FOREIGN_LANGUAGE_REQUIRED == "N" ~ 0),
         OFFERED_TO_APPL_FOREIGN_WORKER = case_when(OFFERED_TO_APPL_FOREIGN_WORKER == "Y" ~ 1,
                                              OFFERED_TO_APPL_FOREIGN_WORKER == "N" ~ 0),
         FOREIGN_WORKER_LIVE_ON_PREM = case_when(FOREIGN_WORKER_LIVE_ON_PREM == "Y" ~ 1,
                                              FOREIGN_WORKER_LIVE_ON_PREM == "N" ~ 0),
         FOREIGN_WORKER_LIVE_IN_DOM_SER = case_when(FOREIGN_WORKER_LIVE_IN_DOM_SER == "Y" ~ 1,
                                              FOREIGN_WORKER_LIVE_IN_DOM_SER == "N" ~ 0),
         PROFESSIONAL_OCCUPATION = case_when(PROFESSIONAL_OCCUPATION == "Y" ~ 1,
                                              PROFESSIONAL_OCCUPATION == "N" ~ 0),
         APP_FOR_COLLEGE_U_TEACHER = case_when(APP_FOR_COLLEGE_U_TEACHER == "Y" ~ 1,
                                              APP_FOR_COLLEGE_U_TEACHER == "N" ~ 0),
         ACCEPT_FOREIGN_EDUCATION = case_when(ACCEPT_FOREIGN_EDUCATION == "Y" ~ 1,
                                              ACCEPT_FOREIGN_EDUCATION == "N" ~ 0),
         FOREIGN_WORKER_EXP_WITH_EMPL = case_when(FOREIGN_WORKER_EXP_WITH_EMPL == "Y" ~ 1,
                                              FOREIGN_WORKER_EXP_WITH_EMPL == "N" ~ 0),
         FOREIGN_WORKER_EMPL_PAY_FOR_ED = case_when(FOREIGN_WORKER_EMPL_PAY_FOR_ED == "Y" ~ 1,
                                              FOREIGN_WORKER_EMPL_PAY_FOR_ED == "N" ~ 0),
         FOREIGN_WORKER_CURR_EMPLOYED = case_when(FOREIGN_WORKER_CURR_EMPLOYED == "Y" ~ 1,
                                              FOREIGN_WORKER_CURR_EMPLOYED == "N" ~ 0),
         EMPLOYER_COMPLETED_APPLICATION = case_when(EMPLOYER_COMPLETED_APPLICATION == "Y" ~ 1,
                                              EMPLOYER_COMPLETED_APPLICATION == "N" ~ 0))

write_csv(H1B_Data, "H1B_Initial_Tidy.csv")

