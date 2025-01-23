## prepare functions for app. 
library(showtext)
library(collapse)
library(ggplot2)
library(ggtext)
library(patchwork)
library(ggdist)
library(forcats)
library(ggiraph)

font_add_google("Sen", "Sen")

showtext_auto()

gdtools::register_gfont("Sen")

## my ggplot2 theme. 
custom_style3 <- function() {
  font <- "Sen"
  
  ggplot2::theme(
    
    plot.title = ggplot2::element_text(family=font,
                                       size=16,
                                       face="bold",
                                       color="#222222"),
    plot.title.position = "plot",
    plot.subtitle = ggplot2::element_text(family=font,
                                          size=15,
                                          margin=ggplot2::margin(9,0,9,0)),
    legend.position = "none",
    legend.background = ggplot2::element_blank(),
    
    legend.text = ggplot2::element_text(family=font,
                                        size=14,
                                        color="#222222", 
                                        hjust = 0),
    
    
    axis.text = ggplot2::element_text(family=font,
                                      size=14,
                                      color="#222222"),
    axis.ticks = ggplot2::element_blank(),
    axis.line = ggplot2::element_blank(),
    
   
    panel.background = ggplot2::element_blank(),
    
    strip.background = ggplot2::element_rect(fill="white"),
    strip.text = ggplot2::element_text(size  = 22,  hjust = 0, 
                                       family = font), 
    
    plot.caption = element_text(family = font, 
                                size = 13)
  )
}



add_pct_sign <- function(x){
  
  scales::percent(x, 
                  scale = 1)
  
  
}


custom_lbl_wrap <- function(x, w = 20){
  
  str_wrap(x, width = w)
}



## color palettes from Wes Anderson package. 

wes_chevalier <- c("#446455FF", "#FDD262FF", "#D3DDDCFF", "#C7B19CFF")

wes_moonrise2 <- c("#798E87FF", "#C27D38FF", "#CCC591FF", "#29211FFF")


my_pal2 <- c("#DEF5E5FF", "#A0DFB9FF",
             "#3487A6FF","#3671A0FF", "#3A599AFF",
             "#414286FF", "#3C3163FF",
             "#312141FF", "#201322FF", "#0B0405FF")


test_pal <- c("#FDD262", "#E2BB57", "#C7A44D", "#AC8D43", "#917638",
              "#765F2E", "#5B4824", "#403119", "#251A0F", "#0B0405")


test2_pal <- c("#FDD262", "#E7C062", "#D2AE62", "#BC9C62", "#A78A62",
               "#917862", "#7C6662", "#665462", "#514262", "#3C3163")




## calculate quantiles. 
qx_calc <- function(y, z=999){
  
  ## Return the 99.9th percentile if unspecified. 
  ## Return 99.5th or 99th percentile if asked
  
  vals <- final_comp %>% 
    fsubset(title == common_roles$title[y]) %>%
    fsummarise(q999 = quantile(rep_comp, probs = 0.999), 
               q995 = quantile(rep_comp, probs = 0.995), 
               q99 = quantile(rep_comp, probs = 0.99))   
  
  q999 <- vals$q999
  
  q995 <- vals$q995
  
  q990 <- vals$q99
  
  
  if(z==999) {
    return(as.double(q999))
  } else if(z==995){
    return(as.double(q995))
  } else if(z==990){
    return(as.double(q990))
  }

  
 }


## create dataframe for legend plot. 
set.seed(20250115)
df_legend <- final_comp %>% 
  fselect(rep_comp) %>% 
  slice_sample(n=1000, replace = F) %>% 
  fsubset(rep_comp<=400000)



## legend for distribution plot. no need to construct any other legend. 

p_legend <-
  df_legend %>% 
  ggplot(aes(y=rep_comp, x = 1)) +
  stat_halfeye(fill_type = "segments", alpha = 0.5) +
  stat_interval() +
  stat_summary(geom = "point", fun = median, size =3) +
  annotate(
    "text",
    x = c(0.8, 0.8, 0.8, 1.4, 1.75),
    y = c(60000, 270000, 180000, 92000, 150000),
    label = c("50 % of prices\nfall within this range", "95 % of prices", 
              "80 % of prices", "Median", "Distribution of prices"),
    family = "Sen", size = 3, vjust = 1
  ) +
  geom_curve(
    data = data.frame(
      x = c(0.8, 0.80, 0.80, 1.3, 1.7),
      xend = c(0.95, 0.95, 0.95, 1.075, 1.7), 
      y = c(60000, 270000, 180000, 92000, 87000),
      yend = c(60000, 270000, 180000, 92000, 10000)),
    aes(x = x, xend = xend, y = y, yend = yend),
    stat = "unique", curvature = -0.2, size = 0.2, color = "black",
    arrow = arrow(angle = 20, length = unit(2, "mm")), 
    linewidth = 1.05
  ) +
  scale_color_manual(values = wes_chevalier) +
  coord_flip(xlim = c(0.5, 1.85),
             ylim = c(0, 380000), expand = TRUE) +
  guides(color = "none") +
  labs(title = "Legend") +
  custom_style3()+
  theme_void(base_family = "Sen")+ 
  theme(plot.title = element_text(family = "Sen", size = 12,
                                  hjust = 0.05),
        plot.background = element_rect(color = "grey30",
                                       size = 0.2))





## plot density plot for a given position stratified by asset size. uses stat_halfeye from ggdist. 
## top 100 positions only. Insufficient data for the rest. 
plot_dist <- function(y){
  
  df <- final_comp %>% 
    fsubset(title == common_roles$title[y] &
              !is.na(asset2) &
              rep_comp < qx_calc(y, z=990)) %>%
    fselect(asset2, rep_comp, org_name, label) 
  
  med_comp <- df %>% 
    fsummarise(med_comp = median(rep_comp)) %>% 
    as.double()
  
  
  dist_plot <- df %>% 
    ggplot(aes(y = rep_comp, x = fct_rev(asset2)))+
    stat_halfeye(alpha = 0.5)+
    stat_interval()+
    stat_summary(geom = "point", fun = median, size =3) +
    geom_hline(yintercept = med_comp, lty = "dashed")+
    annotate("text", x = 6, y = med_comp, label = "Overall median wage",
             family = "Sen", size = 4, hjust = -0.05) +
    custom_style3()+
    scale_y_continuous(labels = scales::dollar,
                       name = "Annual wage")+
    scale_x_discrete(name = NULL,
                     labels = function(i) custom_lbl_wrap(i,w=19))+
    scale_color_manual(values = wes_chevalier)+
    labs(title = paste0("Compensation Distribution for ",
                       common_roles$title[y], "s ", 
                       "by Organization Asset Size"), 
         caption = "Note: This graph doesn't show the top 1% of earners.\nSource: Internal Revenue Service 2024")+
    coord_flip(clip = 'off')+
    theme(panel.grid.major.x = element_line(color = "gray"), 
          axis.title = element_text(family = "Sen", size = 13))
  
  
  
  dist_final <- dist_plot + inset_element(p_legend, l = 0.6, r = 1.0,
                                            t = 0.99, b = 0.7, clip = FALSE)
  
  
  dist_final
  
}


us_geo <- tigris::states(class = "sf", cb = TRUE, 
                         keep_zipped_shapefile = TRUE) %>% 
  tigris::shift_geometry()



## make the relevant data frame specific to the position selected. 
make_df_rel <- function(y){
  
  ## set to missing if there's insufficient data (<5 observations)
  
  df_rel <- final_comp %>%
    fselect(title, rep_comp, state, avg_hrs) %>%
    fsubset(title == common_roles$title[y]) %>%
    fsubset(rep_comp < qx_calc(y, z = 999)) %>%
    fgroup_by(state) %>%
    fsummarise(
      ct = length(rep_comp),
      med_comp = median(rep_comp, na.rm = TRUE),
      q25 =  quantile(rep_comp, probs = 0.25),
      q75 =  quantile(rep_comp, probs = 0.75)
    ) %>%
    as_tibble %>% 
    fmutate(
      med_comp = if_else(ct<5, NA, med_comp), 
      q25 = if_else(ct<5, NA, q25), 
      q75 = if_else(ct<5, NA, q75)
    )
  
  
  df_rel <- us_geo %>% 
    rename(state = STUSPS) %>% 
    left_join(df_rel, by = "state") %>% 
    filter(state %in% c(state.abb, "DC", "PR")) %>% 
    mutate(across(c("med_comp", "q25", "q75"), round))
  
  df_rel
  
  
  
}



plot_by_state <- function(y){
  
  gg <- ggplot(make_df_rel(y),
               aes(fill = med_comp)) + 
    geom_sf_interactive(aes(tooltip = paste0(NAME,"<br>",
                                             "Median compensation: ", scales::dollar(med_comp), "<br>", 
                                             "Half earn between: ", scales::dollar(q25), " & ",
                                             scales::dollar(q75)),
                            data_id = NAME), 
                        size = 0.1) + 
    scale_fill_gradientn(colors = test_pal, labels = scales::label_dollar())+
    labs(title = paste0("Median Compensation for ",
                        common_roles$title[y], "s ", "by State"),
         caption = "Note: This graph doesn't include the top 0.01% of earners.\nSource: Internal Revenue Service 2024",
         fill = NULL) + 
    custom_style3() +
    theme(axis.text = element_blank(), 
          legend.position = "top",
          legend.justification = c("left", "top"),
          legend.key.width = unit(4, "line")
    ) 
  
  
  
  
  girafe(ggobj = gg, 
         width_svg = 10,
         height_svg = 8) %>%
    girafe_options(opts_hover(css = "fill:#D3DDDCFF;"), 
                   opts_zoom(min = 0.5, max = 1),
                   opts_tooltip(
                     css = "background-color:white;color:black;padding:10px;font-family:Gill Sans MT;font-size:18px;",
                     opacity = 0.9
                     # use_fill = TRUE
                   )
                   ,
                   opts_sizing(rescale = TRUE
                               )
                   )
  
  
  
  
  
  
}











