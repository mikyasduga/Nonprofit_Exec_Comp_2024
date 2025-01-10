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



app_df <- final_comp %>% 
  fsubset(title %in% common_roles$title)


exc <- c("OF", "AND")

## to title with exceptions
to_title_we <- function(x){
  
  x <- str_to_title(x)
  
  x <- str_replace_all(x, c(" Of " = " of ", 
                            " And " = " and ", 
                            " Ceo" =  " CEO", 
                            " Md" = " MD", 
                            " Mba" = "MBA")
  )
  
  
  
              
}







ntee <- read.table("data/ntee_cd.txt", sep = "-") %>% 
  tidyr::separate_longer_delim(cols = V2, delim = ",") %>% 
  rename(label = 1, 
         ntee_cd = 2) %>% 
  mutate(ntee_cd = str_squish(ntee_cd))


## individuals with the highest compensation 
final_comp %>% 
  arrange(desc(rep_comp)) %>% 
  fselect(names, title, rep_comp, org_name, org_type, comb_501c, ntee_cd, 
           ein, state, tax_yr) %>% 
  fsubset(org_type == "PF") %>% 
  slice(1:100) %>%
  mutate((across(where(is.character), to_title_we))) %>% 
  fmutate(state = toupper(state),
          org_type = toupper(org_type),
          rep_comp = scales::dollar(rep_comp), 
          org_type = if_else(org_type == "PC", "Public charity", 
                             "Private foundation"), 
          ntee_cd = substr(ntee_cd, 1,1)
          ) %>%
  left_join(ntee, by = "ntee_cd") %>% 
  fselect(-ntee_cd) %>% 
  frename(Name = names, 
          Title = title, 
          "Reported Compensation" = rep_comp, 
          "organization" = org_name, 
          "Org Type" = org_type, 
          '501c classification' = comb_501c, 
          'Mission area' = label, 
          EIN = ein, 
          State = state, 
          'Tax year' = tax_yr
          ) %>% 
  DT::datatable()
          

create_asset_class <- function(x){


x <- mutate(x = case_when(
  x < 500000 ~ "<500K", 
  between(x, 500000, 1999999) ~ " Between 500k and 2M", 
  between(x, 2000000,4999999) ~ "Between 2M and 5M", 
  between(x, 5000000, 19999999) ~ "Between 5M and 20M", 
  x >= 20000000 ~ ">=20M", 
  TRUE ~ NA
))

x
}


final_comp <- final_comp %>% 
  fmutate(
  asset2 = cut(assets_eoy, breaks = c(0, 500000, 
                                      1999999, 4999999, 
                                      19999999, Inf)
               ,
               labels = c("<$500,000", "$500,000 to $1,999,999",
                          "$2,000,000 to $4,999,999",
                          "$5,000,000 to $19,999,999",
                          ">= $20,000,000"), 
               ordered_result = T
               )) 


## plots
library(ggplot2)

final_comp %>% 
  fsubset(title == common_roles$title[1] &
            !is.na(asset2)) %>%
  fselect(asset2, rep_comp) %>% 
  ggplot(aes(y = rep_comp))+
  geom_density()
  
  
  











