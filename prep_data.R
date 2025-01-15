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


## to title with exceptions
to_title_we <- function(x){
  
  x <- str_to_title(x)
  
  x <- str_replace_all(x, c(" Of " = " of ", 
                            " And " = " and ", 
                            " Ceo" =  " CEO", 
                            " Md" = " MD", 
                            " Mba" = " MBA", 
                            " Phd" = " PhD")
  )
  
  
  
              
}

common_roles <- final_comp %>% 
  fselect(title) %>% 
  fgroup_by(title) %>% 
  fcount(title) %>% 
  fungroup() %>% 
  arrange(desc(N)) %>% 
  fsubset(N >= 50) 



# app_df <- final_comp %>% 
#   fsubset(title %in% common_roles$title)


ntee <- read.table("data/ntee_cd.txt", sep = "-") %>% 
  tidyr::separate_longer_delim(cols = V2, delim = ",") %>% 
  rename(label = 1, 
         ntee_cd = 2) %>% 
  mutate(ntee_cd = str_squish(ntee_cd))


## individuals with the highest compensation 
# final_comp %>% 
#   arrange(desc(rep_comp)) %>% 
#   fselect(names, title, rep_comp, org_name, org_type, comb_501c, ntee_cd, 
#            ein, state, tax_yr) %>% 
#   # fsubset(org_type == "PF") %>% ## filtering section
#   slice(1:100) %>%
#   mutate((across(where(is.character), to_title_we))) %>% 
#   fmutate(state = toupper(state),
#           org_type = toupper(org_type),
#           rep_comp = scales::dollar(rep_comp), 
#           org_type = if_else(org_type == "PC", "Public charity", 
#                              "Private foundation"), 
#           ntee_cd = substr(ntee_cd, 1,1)
#           ) %>%
#   left_join(ntee, by = "ntee_cd") %>% 
#   fselect(-ntee_cd) %>% 
#   frename(Name = names, 
#           Title = title, 
#           "Reported Compensation" = rep_comp, 
#           "organization" = org_name, 
#           "Org Type" = org_type, 
#           '501c classification' = comb_501c, 
#           'Mission area' = label, 
#           EIN = ein, 
#           State = state, 
#           'Tax year' = tax_yr
#           ) %>% 
#   DT::datatable()
          

final_comp <- final_comp %>% 
  fsubset(title %in% common_roles$title) %>% 
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
               )) %>% 
  mutate((across(where(is.character), to_title_we))) %>% 
  fmutate(state = toupper(state),
          org_type = toupper(org_type),
          org_type = if_else(org_type == "PC", "Public charity", 
                             "Private foundation"), 
          ntee_cd = substr(ntee_cd, 1,1)
  ) %>%
  left_join(ntee, by = "ntee_cd") %>% 
  fselect(-ntee_cd)


common_roles <- common_roles%>%
  fmutate(title = to_title_we(title))

## plots
library(ggplot2)
library(ggdist)
library(showtext)
library(forcats)

font_add_google("Sen", "Sen")

showtext_auto()


set.seed(20250113) 


q99 <- final_comp %>% 
  fsubset(title == common_roles$title[1]) %>%
  fsummarise(qx = quantile(rep_comp, probs = 0.99)) %>% 
  as.double()

# test <- 
  final_comp %>% 
  fsubset(title == common_roles$title[1] &
            !is.na(asset2) &
            rep_comp <= q99) %>%
  fselect(asset2, rep_comp, org_name, label) %>% 
  fmutate(med_comp = median(rep_comp)) %>% 
  # slice_sample(n = 10000, replace = FALSE) %>% 
  ggplot(aes(y = rep_comp, x = fct_rev(asset2)))+
  stat_halfeye()+
  stat_interval()+
  stat_summary(geom = "point", fun = median, size =3) +
  geom_hline(aes(yintercept = med_comp), lty = "dashed")+
  # facet_wrap(~asset2, ncol = 1,
  #            scales = "free_y")+
  custom_style3()+
  scale_y_continuous(labels = scales::dollar,
                     name = NULL, 
                     expand = c(0,0))+
    scale_x_discrete(name = NULL, labels = function(i) custom_lbl_wrap(i,w=19))+
  labs(title = "Compensation for position by organization asset size", 
       caption = "Source: Internal Revenue Service 2024")+
  coord_flip()
  
  
  
final_comp %>% 
  fsubset(title == common_roles$title[1]) %>% 
  fsummarise(mean = mean(rep_comp, na.rm = T), 
             median = median(rep_comp, na.rm = T), 
             qx = quantile(rep_comp, probs = 0.99),
             min = min(rep_comp), 
             max = max(rep_comp))
 









