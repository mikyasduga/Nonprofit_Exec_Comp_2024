library(dplyr)
library(collapse)
library(data.table)
library(stringr)

final_comp <- fst::read_fst("data/final_comp.fst")

## only cleaning most roles reported atleast 50 times. 
final_comp <- final_comp %>% 
  fmutate(title = if_else(str_starts(title, "AND "), 
                          str_remove(title, "AND "), 
                          title), 
          title = if_else((str_detect(title, " AND ") == FALSE), 
                          str_replace_all(title, "AND", " AND "), 
                          title), 
          title = str_squish(title), 
          title = if_else(title == "PRESIDENT DIRECTOR", "PRESIDENT AND DIRECTOR", 
                          title), 
          title = if_else(title == "DOCTOR", "PHYSICIAN", title), 
          title = if_else(title == "RECORDING SE", "RECORDING SECRETARY", title), 
          title = if_else((title == "SECTREAS" |
                            title == "SECRETARYTREAS"), "SECRETARY AND TREASURER", title), 
          title = if_else(title == "PRESIDNET AND DIRECTOR", "PRESIDENT AND DIRECTOR", 
                          title), 
          title = if_else(title == "ASSISTANT HEAD SCHOOL", "ASSISTANT HEAD OF SCHOOL", 
                          title), 
          title = if_else((title == "EXECUTIVE DIRECTORSECRETARY" |
                            title == "SECRETARYEXECUTIVE DIRECTOR"), 
                          "EXECUTIVE DIRECTOR AND SECRETARY", title), 
          
          title = case_when(
            title == "HUMAN RESOURCE DIRECTOR" ~ "HUMAN RESOURCES DIRECTOR", 
            title == "CHAIR BOARD" ~ "BOARD CHAIR", 
            title == "EXECUTIVE DIRECTOR AND EXECUTIVE DIRECTOR" ~ "EXECUTIVE DIRECTOR", 
            title == "CHRO" |
              title == "CHUMAN RESOURCESO" |
              title == "CHIEF HUMAN RESOURCE OFFICER" ~ "CHIEF HUMAN RESOURCES OFFICER", 
            title == "PRES" ~ "PRESIDENT", 
            title == "TREASURERDIRECTOR" ~ "TREASURER AND DIRECTOR", 
            title == "PRESEXECUTIVE" |
              title == "PRESEXECUTIVE DIRECTOR" ~ "PRESIDENT AND CEO", 
            title == "EXECUTIVE DIRECTOR DIRECTOR" ~ "EXECUTIVE DIRECTOR AND DIRECTOR", 
            title == "SECRETARY DIRECTOR" ~ "SECRETARY AND DIRECTOR", 
            title == "CHIEF EXECUT" ~ "EXECUTIVE DIRECTOR", 
            title == "FOUNDEREXECUTIVE DIRECTOR" ~ "FOUDER AND EXECUTIVE DIRECTOR", 
            title == "ASSOCIATE DI" ~ "ASSOCIATE DIRECTOR",
            title == "BOARD MEMBERPHYSICIAN" ~ "PHYSICIAN AND BOARD MEMBER",
            TRUE ~ title
          ), 
          title = str_replace_all(title, "EVICE", "EXECUTIVE VICE")
          
          ) %>% 
  fsubset(title != "HIGHEST COMPENSATED EMPLOY")
## AS CLEAN AS IT GETS. 


common_roles <- final_comp %>% 
  fselect(title) %>% 
  fgroup_by(title) %>% 
  fcount(title) %>% 
  fungroup() %>% 
  arrange(desc(N)) %>% 
  fsubset(N >= 50) 



















