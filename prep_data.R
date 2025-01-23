
## prepare app data frames, functions and experiment with different mapping packages. 

library(dplyr)
library(readr)
library(collapse)
library(data.table)
library(stringr)
source("my_functions.R")
source("prep_functions.R")

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


## individuals with the highest compensation -- doesn't align with the purpose of application
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



## add a value box to show the overall median value. 

plot_dist(1)
  



# library(highcharter)
# 
# 
# dta <- download_map_data("https://code.highcharts.com/mapdata/countries/us/us-all.js")
# get_data_from_map(dta)


# 
# library(urbnmapr)
# 
# ac <- get_urbn_map(map = "states")
# ac_labels <- get_urbn_labels(map = "states")
# 
# 
#   ab <-
#     df_rel %>% 
#     # filter(title == common_roles$title[1]) %>% 
#     # group_by(state) %>% 
#     # add_count() %>% 
#     # filter(n>=5) %>% 
#     # summarise(avg_comp = round(mean(rep_comp, na.rm = T)), 
#     #           avg_hrs = round(mean(avg_hrs, na.rm = T))) %>% 
#     # mutate(hourly_comp = avg_comp/avg_hrs) %>% 
#     # mutate(FT_comp = round(hourly_comp*40)) %>% 
#     rename(state_abbv = state
#            # ,
#            # Compensation = FT_comp
#            ) %>%
#     full_join(ac %>% 
#                 select(lat, long, state_abbv, group), by = "state_abbv") %>% 
#     ggplot()+
#     geom_polygon_interactive(aes(long, lat, group = group, fill = med_comp, 
#                                  tooltip = med_comp, data_id = state_abbv), 
#                  color = "#ffffff", linewidth = 0.25) +
#     
#     scale_fill_gradient2(
#       labels = scales::dollar,
#       low = "#DEF5E5FF",
#       mid = "#3487A6FF",
#       high = "#0B0405FF",
#       midpoint = median(df_rel$med_comp), 
#       na.value = "gray80"
#     ) +
#     custom_style3()+
#     theme(
#       legend.position = "top",
#       legend.justification = 0.5,
#       legend.key.width = unit(3, "cm"),
#       panel.grid.major = element_blank(), 
#       panel.grid.minor = element_blank(), 
#       axis.text = element_blank(), 
#       plot.title = element_text(face = "bold")
#       
#     )+
#     
#     labs(title = paste0("Mean Compensation for ED's by State"), 
#          caption = "Note: This graph does not include the top 0.01% of earners.
#          Source: Internal Revenue Service 2024", 
#          x = NULL, y = NULL, fill = NULL)
  
## urbn doesn't work with interactive features. good for static mapping however. 
# girafe(ggobj = ab)%>%
#   girafe_options(opts_hover(css = "fill:cyan;"), 
#                  opts_zoom(max = 3))

  
  
# plotly::ggplotly(ab) %>% 
#   plotly::layout(
#     font = list(
#       family = "AgencyFB"
#     ), 
#     legend = list(orientaion = 'h')
#   ) ## too basic not customizable 
  
  
# states <- sf::read_sf(
#   "https://rstudio.github.io/leaflet/json/us-states.geojson")
# 
# 
# 
# states <- states %>% 
#   left_join((cbind.data.frame(
#     state.abb, state.name
#   ) %>% rename(name = 2)), 
#   by = "name") %>% 
#   mutate(state.abb = case_when(
#     name == "District of Columbia" ~ "DC", 
#     name == "Puerto Rico" ~ "PR", 
#     TRUE ~ state.abb
#   )) %>% 
#   rename(state = state.abb) %>% 
#   left_join(df_rel, by = "state")
  
  

# library(leaflet)
# 
# m <- leaflet(states) %>%
#   setView(-96, 40, 4.7) %>%
#   addTiles("MapBox", options = providerTileOptions(
#     id = "mapbox.light"))
# 
# my_pal <- c("#A0DFB9FF",
#            "#3487A6FF","#3671A0FF", "#3A599AFF",
#            "#414286FF", "#3C3163FF",
#            "#312141FF", "#201322FF")


my_pal2 <- c("#DEF5E5FF", "#A0DFB9FF",
             "#3487A6FF","#3671A0FF", "#3A599AFF",
             "#414286FF", "#3C3163FF",
             "#312141FF", "#201322FF", "#0B0405FF")


# bins <- c(quantile(df_rel$med_comp, probs = 0.0),
#           quantile(df_rel$med_comp, probs = 0.25), 
#           quantile(df_rel$med_comp, probs = 0.50),
#           quantile(df_rel$med_comp, probs = 0.75),
#           quantile(df_rel$med_comp, probs = 0.95),
#           Inf)
# pal <- colorBin(my_pal, domain = states$med_comp, bins = bins)



# mylabels <- paste(
#   "State: ", states$name, "<br/>",
#   "Median compensation: ", scales::dollar(states$med_comp), "<br/>",
#   "Half fall between: ", scales::dollar(states$q25), "&", scales::dollar(states$q75)
# ) %>%
#   lapply(htmltools::HTML)
# 
# htmltitle <- "<h5> Compensation for Executive Directors by State</h5>"
# htmlcap <- "<h5> Source: Internal Revenue Service 2024</h5>"
# 
# m %>% addPolygons(
#   fillColor = ~pal(med_comp),
#   weight = 2,
#   opacity = 1,
#   color = "white",
#   dashArray = "3",
#   fillOpacity = 0.7, 
#   label = mylabels,
#   labelOptions = labelOptions( 
#     style = list("font-weight" = "normal", padding = "3px 8px"), 
#     textsize = "13px", 
#     direction = "auto"
#   ))%>%
#   addLegend( pal= pal, 
#              values=~med_comp, 
#              opacity=0.7, 
#              title = NULL, 
#              position = "bottomleft" 
#   ) %>%
#   addControl(html=htmltitle, position = "topright") %>% 
#   addControl(html=htmlcap, position = "bottomright")

# us_value_shifted <- us_value %>%
#   shift_geometry(position = "outside") %>%
#   mutate(tooltip = paste(NAME, estimate, sep = ": "))









## functions are mostly ready. may need some minor modifications later one. 
## save data file that has been cleaned. 

fst::write_fst(final_comp, "data/final_comp2.fst")







