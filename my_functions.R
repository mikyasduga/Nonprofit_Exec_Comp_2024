
## Function to check whether a return is 990 PC or other. 

id_pc <- function(x){
  library(xml2)
  dat <- read_xml(x)
  
  xml_ns_strip(dat)
  
  xml_text((xml_find_first(dat, "//ReturnHeader/ReturnTypeCd"))) == "990"
  ## identify 990s only. (990 == TRUE, excludes PF, T, EZ, etc.)
  
}


## Identify return type. 
id_pc_2 <- function(x){
  
  library(xml2)
  dat <- read_xml(x)
  
  xml_ns_strip(dat)
  
  ret_type <- xml_text((xml_find_first(dat, "//ReturnHeader/ReturnTypeCd")))
  
  as.data.frame(ret_type)
  
}

##Extract variables of interest from full XML file. 
voi_IRS_xml <- function(x){
  
  library(xml2)
  library(dplyr)
  
  tab1 <- readRDS("data/temp_tbl.rds")  ## list of all available variables
  my_vars <- readRDS("data/my_vars.rds")  ## employee count, volunteer count, total revenue, policies, etc
  
  dat <- read_xml(x)
  
  xml_ns_strip(dat) 
  
  id_vars <- tab1$variable[c(2,4,5, 7:9)] ## USAddress, group return ind,
  ## 501c3 indicator, corp indicator, formation year, State code
  
  id <- rbind(xml_text( xml_children( xml_find_first
                                      (dat, "//ReturnHeader/Filer" ) ) )[c(1,2)])
  ##EIN, name
  
  
  id_vars2 <- tab1$variable[c(4,5, 7:9)]
  
  yr <- xml_text( xml_find_first(dat, "//ReturnHeader/TaxYr")) 
  
  
  my_vars2 <- my_vars
  
  ext_xml_element <- function(x){
    cbind(xml_text(xml_find_first(dat, paste0("//ReturnData/IRS990/", my_vars2[x]))))
  }
  
  
  vars <- cbind(
    #Number address
    xml_text(xml_find_first(dat, "//ReturnHeader/Filer/USAddress/AddressLine1Txt")),
    #city
    xml_text(xml_find_first(dat, "//ReturnHeader/Filer/USAddress/CityNm")), 
    #state
    xml_text(xml_find_first(dat, "//ReturnHeader/Filer/USAddress/StateAbbreviationCd")),
    #zipcode
    xml_text(xml_find_first(dat, paste0("//ReturnHeader/Filer/USAddress/ZIPCd"))),
    
    
    xml_text(xml_find_first(dat, paste0("//ReturnData/IRS990/", id_vars2[1]))),
    
    xml_text(xml_find_first(dat, paste0("//ReturnData/IRS990/", id_vars2[2]))),
    
    xml_text(xml_find_first(dat, paste0("//ReturnData/IRS990/", id_vars2[3]))),
    
    xml_text(xml_find_first(dat, paste0("//ReturnData/IRS990/", id_vars2[4]))),
    
    xml_text(xml_find_first(dat, paste0("//ReturnData/IRS990/", id_vars2[5]))),
    
    as.data.frame(purrr::map(1:27, ext_xml_element))
    
    
    
  )
  
  
  id <- as.data.frame(id)
  names(id) <- c("EIN", "Name")
  
  
  yr <- as.data.frame(yr)
  names(yr) <- "Tax_year"
  
  vars <- as.data.frame(vars)
  names(vars) <- c("Street_add", 
                   "city", "state", "zip_code",
                   id_vars2, my_vars)
  
  
  bind_cols(id, yr, vars)
  
}



empty_to_null <- function(x){ ## some can be empty and this affects cbind. so set this values to NA. 
  
  if (is.character(x) & length(x) == 0){
    x <- NA
  }
  
  return(x)
}






## Extract individual compensation data from xml files. 
## one to many data frame as an org reports compensation for many employees. 
ext_ind_comp <- function(x){
  
  library(xml2)
  library(dplyr)
  
  tab1 <- readRDS("data/temp_tbl.rds")  ## list of all available variables
  my_vars <- readRDS("data/my_vars.rds")  ## employee count, volunteer count, total revenue, policies, etc
  
  dat <- read_xml(x)
  
  xml_ns_strip(dat)
  
  ## identify return type
  return_type <- xml_text((xml_find_first(dat, "//ReturnHeader/ReturnTypeCd")))
  
  
  id_vars <- tab1$variable[c(2,4,5, 7:9)] ## USAddress, group return ind,
  ## 501c3 indicator, corp indicator, formation year, State code
  
  id <- rbind(xml_text( xml_children( xml_find_first
                                      (dat, "//ReturnHeader/Filer" ) ) )[c(1,2)])
  ##EIN, name
  
  
  id_vars2 <- tab1$variable[c(4,5, 7:9)]
  
  
  yr <- xml_text( xml_find_first(dat, "//ReturnHeader/TaxYr")) 
  
  yr_begin <- xml_text( xml_find_first(dat, "//ReturnHeader/TaxPeriodBeginDt"))
  ## tax year beginning date
  
  yr_end <- xml_text( xml_find_first(dat, "//ReturnHeader/TaxPeriodEndDt"))
  ## tax year end date. 
  
  
  tax_yr <- cbind(return_type, yr, yr_begin, yr_end)
  
  
  ## New addition
  # any type of 501c
  ind_501c <- xml_text(xml_find_all(dat,
                                    '//ReturnData/IRS990/Organization501cInd'))
  ind_501c <- empty_to_null(ind_501c)
  
  # type of 501c. list available on IRS website
  type_501c <- xml_attrs(xml_child(xml_child(xml_child(dat, "ReturnData"),
                                             "IRS990"), "Organization501cInd"))
  
  
  type_501c <- empty_to_null(type_501c)
  
  # association indicator 
  assoc_ind <- xml_text(xml_find_all(dat,
                                     '//ReturnData/IRS990/TypeofOrganizationAssocInd'))
  
  assoc_ind <- empty_to_null(assoc_ind)
  
  
  
  
  ## expenses current year
  CY_expenses <- xml_double(xml_find_all(dat,
                                       '//ReturnData/IRS990/CYTotalExpensesAmt'))
  CY_expenses <- empty_to_null(CY_expenses)
  
  ## EOY total assets
  assets_eoy <- xml_double(xml_find_all(dat,
                                      '//ReturnData/IRS990/TotalAssetsEOYAmt'))
  
  assets_eoy <- empty_to_null(assets_eoy)
  
  
  total_employee_cnt <- xml_text(xml_find_all(dat, 
                                                "//ReturnData/IRS990/TotalEmployeeCnt"))
  
  total_employee_cnt <- empty_to_null(total_employee_cnt)
  
  
  org_info <- cbind(ind_501c, type_501c, assoc_ind, 
                    CY_expenses, assets_eoy, total_employee_cnt)
  
  
  ## end new addition
  
  
  vars <- cbind(  ## id_vars. address, 
    #Number address
    xml_text(xml_find_first(dat, "//ReturnHeader/Filer/USAddress/AddressLine1Txt")),
    #city
    xml_text(xml_find_first(dat, "//ReturnHeader/Filer/USAddress/CityNm")), 
    #state
    xml_text(xml_find_first(dat, "//ReturnHeader/Filer/USAddress/StateAbbreviationCd")),
    #zipcode
    xml_text(xml_find_first(dat, paste0("//ReturnHeader/Filer/USAddress/ZIPCd"))),
    
    
    xml_text(xml_find_first(dat, paste0("//ReturnData/IRS990/", id_vars2[1]))),
    
    xml_text(xml_find_first(dat, paste0("//ReturnData/IRS990/", id_vars2[2]))),
    
    xml_text(xml_find_first(dat, paste0("//ReturnData/IRS990/", id_vars2[3]))),
    
    xml_text(xml_find_first(dat, paste0("//ReturnData/IRS990/", id_vars2[4]))),
    
    xml_text(xml_find_first(dat, paste0("//ReturnData/IRS990/", id_vars2[5])))
    
    )
  
  
  names <- xml_text(xml_find_all(dat, "//ReturnData/IRS990/Form990PartVIISectionAGrp/PersonNm"))
  
  titles <- xml_text(xml_find_all(dat, "//ReturnData/IRS990/Form990PartVIISectionAGrp/TitleTxt"))
  
  avg_hrs <- xml_text(xml_find_all(dat, "//ReturnData/IRS990/Form990PartVIISectionAGrp/AverageHoursPerWeekRt"))
  
  avg_hrs_rltd <- xml_text(xml_find_all(dat, "//ReturnData/IRS990/Form990PartVIISectionAGrp/AverageHoursPerWeekRltdOrgRt"))
  
  Trustee_dir_ind <- xml_text(xml_find_all(dat, "//ReturnData/IRS990/Form990PartVIISectionAGrp/IndividualTrusteeOrDirectorInd"))
  
  rep_comp <- xml_text(xml_find_all(dat, "//ReturnData/IRS990/Form990PartVIISectionAGrp/ReportableCompFromOrgAmt"))
  
  rep_comp_rltd <- xml_text(xml_find_all(dat, "//ReturnData/IRS990/Form990PartVIISectionAGrp/ReportableCompFromRltdOrgAmt"))
  
  other_comp <- xml_text(xml_find_all(dat, "//ReturnData/IRS990/Form990PartVIISectionAGrp/OtherCompensationAmt"))
  
  ##fix inconsistent length of vectors to make a dataframe. cbind just starts repeating values to make vectors of equal length. 
  names_l <- length(names)
  titles_l <- length(titles)
  avg_hrs_l <- length(avg_hrs)
  avg_hrs_rltd_l <- length(avg_hrs_rltd)
  Trustee_dir_ind_l <- length(Trustee_dir_ind)
  rep_comp_l <- length(rep_comp)
  rep_comp_rltd_l <- length(rep_comp_rltd)
  other_comp_l <- length(other_comp)
  
  max_length <- max(names_l, titles_l, avg_hrs_l, avg_hrs_rltd_l,
                    Trustee_dir_ind_l, rep_comp_l, rep_comp_rltd_l, other_comp_l) 
  
  length(names) <- max_length
  length(titles) <- max_length
  length(avg_hrs) <- max_length
  length(avg_hrs_rltd) <- max_length
  length(Trustee_dir_ind) <- max_length
  length(rep_comp) <- max_length
  length(rep_comp_rltd) <- max_length
  length(other_comp) <- max_length
  
  
 # all_cols <- empty_to_null(all_cols)
  
 
  df <- cbind.data.frame(id, tax_yr, org_info,
                         vars, names, titles, avg_hrs, 
                         avg_hrs_rltd, Trustee_dir_ind, 
                   rep_comp, rep_comp_rltd, other_comp)
  
  
  names(df)[1:21] <- c("EIN", "Org_name", "Return_type",
                       "Tax_yr", "Tax_yr_begin",
                       "Tax_yr_end",
                       "ind_501c", "type_501c", "assoc_ind", 
                       "CY_expenses", "assets_eoy", "total_employee_cnt",
                      "Street_add", 
                      "city", "state", "zip_code", id_vars2)
  
  df 
  
  
}

## get file size in MB. 
get_size <- function(x){
  file.info(x)$size/1000000
}




##Find public charities in a list. 
find_pc_location <- function(xlist){
  
  pc_list <- xlist %>% 
    unlist() %>% 
    as.data.frame() %>% 
    rename(value = 1) 
  
  
  pc_location <- which(pc_list$value == TRUE |
                         pc_list$value == "TRUE" |
                         pc_list$value == "990")
  
  pc_location
}

tab1 <- read_rds("data/temp_tbl.rds")
my_vars <- read_rds("data/my_vars.rds")

id_vars2 <- tab1$variable[c(4,5, 7:9)]







pf_ext_ind_comp <- function(x){
  
  library(xml2)
  library(dplyr)
  
  tab1 <- readRDS("data/temp_tbl.rds")  ## list of all available variables
  my_vars <- readRDS("data/my_vars.rds")  ## employee count, volunteer count, total revenue, policies, etc
  
  dat <- read_xml(x)
  
  xml_ns_strip(dat)
  
  ## identify return type
  return_type <- xml_text((xml_find_first(dat, "//ReturnHeader/ReturnTypeCd")))
  
  
  id_vars <- tab1$variable[c(2,4,5, 7:9)] ## USAddress, group return ind,
  ## 501c3 indicator, corp indicator, formation year, State code
  
  id <- rbind(xml_text( xml_children( xml_find_first
                                      (dat, "//ReturnHeader/Filer" ) ) )[c(1,2)])
  ##EIN, name
  
  
  id_vars2 <- tab1$variable[c(4,5, 7:9)]
  
  
  yr <- xml_text( xml_find_first(dat, "//ReturnHeader/TaxYr")) 
  
  yr_begin <- xml_text( xml_find_first(dat, "//ReturnHeader/TaxPeriodBeginDt"))
  ## tax year beginning date
  
  yr_end <- xml_text( xml_find_first(dat, "//ReturnHeader/TaxPeriodEndDt"))
  ## tax year end date. 
  
  
  tax_yr <- cbind(return_type, yr, yr_begin, yr_end)
  
  
  ### new additions
  
  ind_501c3 <- xml_text( xml_find_first(dat, "//ReturnData/IRS990PF/Organization501c3ExemptPFInd"))
  
  assets_eoy <- xml_text( xml_find_first(dat, "//ReturnData/IRS990PF/FMVAssetsEOYAmt"))
  
  
  ## end new additions
  
  
  
  
  
  vars <- cbind(  ## id_vars. address, 
    #Number address
    xml_text(xml_find_first(dat, "//ReturnHeader/Filer/USAddress/AddressLine1Txt")),
    #city
    xml_text(xml_find_first(dat, "//ReturnHeader/Filer/USAddress/CityNm")), 
    #state
    xml_text(xml_find_first(dat, "//ReturnHeader/Filer/USAddress/StateAbbreviationCd")),
    #zipcode
    xml_text(xml_find_first(dat, paste0("//ReturnHeader/Filer/USAddress/ZIPCd"))),
    
    
    xml_text(xml_find_first(dat, paste0("//ReturnData/IRS990/", id_vars2[1]))),  ## the next 5 items are not available for PFs. 
    
    xml_text(xml_find_first(dat, paste0("//ReturnData/IRS990/", id_vars2[2]))),
    
    xml_text(xml_find_first(dat, paste0("//ReturnData/IRS990/", id_vars2[3]))),
    
    xml_text(xml_find_first(dat, paste0("//ReturnData/IRS990/", id_vars2[4]))),
    
    xml_text(xml_find_first(dat, paste0("//ReturnData/IRS990/", id_vars2[5])))
    
  )
  
  
  ##data location within xml for individual compensation. PRIVATE FOUNDATION FILINGS
  
  ind_info_loc <- "//ReturnData/IRS990PF/OfficerDirTrstKeyEmplInfoGrp/OfficerDirTrstKeyEmplGrp/"
  
  names <- xml_text(xml_find_all(dat, paste0(ind_info_loc, "PersonNm")))
  
  titles <- xml_text(xml_find_all(dat, paste0(ind_info_loc, "TitleTxt")))
  
  avg_hrs <- xml_text(xml_find_all(dat, paste0(ind_info_loc, "AverageHrsPerWkDevotedToPosRt")))
  
  rep_comp <- xml_text(xml_find_all(dat, paste0(ind_info_loc, "/CompensationAmt")))
  
  ben_program_amt <- xml_text(xml_find_all(dat, paste0(ind_info_loc, "/EmployeeBenefitProgramAmt")))
  
  other_allwnc_amt <- xml_text(xml_find_all(dat, paste0(ind_info_loc, "/ExpenseAccountOtherAllwncAmt")))

  
  ##fix inconsistent length of vectors to make a dataframe. cbind just starts repeating values to make vectors of equal length. 
  names_l <- length(names)
  titles_l <- length(titles)
  avg_hrs_l <- length(avg_hrs)
  rep_comp_l <- length(rep_comp)
  ben_program_amt_l <- length(ben_program_amt)
  other_allwnc_amt_l <- length(other_allwnc_amt)
  
  max_length <- max(names_l, titles_l, avg_hrs_l, 
                    rep_comp_l, ben_program_amt_l, other_allwnc_amt_l) 
  
  length(names) <- max_length
  length(titles) <- max_length
  length(avg_hrs) <- max_length
  length(rep_comp) <- max_length
  length(ben_program_amt) <- max_length
  length(other_allwnc_amt) <- max_length
  
  
  
  df <- cbind.data.frame(id, tax_yr, vars,
                         names, titles, avg_hrs, rep_comp,
                         ben_program_amt, other_allwnc_amt, 
                         ind_501c3, assets_eoy)
  
  
  names(df)[1:15] <- c("EIN", "Org_name", "Return_type",
                       "Tax_yr", "Tax_yr_begin",
                       "Tax_yr_end",
                       "Street_add", 
                       "city", "state", "zip_code", 
                       id_vars2)
  
  df
  
  
}



custom_style3 <- function() {
  font <- "Sen"
  
  ggplot2::theme(
    
    #Text format:
    #This sets the font, size, type and colour of text for the chart's title
    plot.title = ggplot2::element_text(family=font,
                                       size=16,
                                       face="bold",
                                       color="#222222"),
    plot.title.position = "plot",
    #This sets the font, size, type and colour of text for the chart's subtitle, as well as setting a margin between the title and the subtitle
    plot.subtitle = ggplot2::element_text(family=font,
                                          size=15,
                                          margin=ggplot2::margin(9,0,9,0)),
    # plot.caption = ggplot2::element_blank(),
    #This leaves the caption text element empty, because it is set elsewhere in the finalise plot function
    
    #Legend format
    #This sets the position and alignment of the legend, removes a title and backround for it and sets the requirements for any text within the legend. The legend may often need some more manual tweaking when it comes to its exact position based on the plot coordinates.
    legend.position = "none",
    #legend.text.align = 0,
    legend.background = ggplot2::element_blank(),
    # legend.title = ggplot2::element_blank(),
    # legend.key = ggplot2::element_blank(),
    legend.text = ggplot2::element_text(family=font,
                                        size=14,
                                        color="#222222", 
                                        hjust = 0),
    
    #Axis format
    #This sets the text font, size and colour for the axis test, as well as setting the margins and removes lines and ticks. In some cases, axis lines and axis ticks are things we would want to have in the chart - the cookbook shows examples of how to do so.
    # axis.title = ggplot2::element_blank(),
    axis.text = ggplot2::element_text(family=font,
                                      size=14,
                                      color="#222222"),
    # axis.text.x = ggplot2::element_blank(),
    axis.ticks = ggplot2::element_blank(),
    axis.line = ggplot2::element_blank(),
    
    #Grid lines
    #This removes all minor gridlines and adds major y gridlines. In many cases you will want to change this to remove y gridlines and add x gridlines. The cookbook shows you examples for doing so
    # panel.grid.minor = ggplot2::element_blank(),
    # # panel.grid.major.y = ggplot2::element_blank(),
    # panel.grid.major.x = ggplot2::element_blank(),
    
    #Blank background
    #This sets the panel background as blank, removing the standard grey ggplot background colour from the plot
    panel.background = ggplot2::element_blank(),
    
    #Strip background (#This sets the panel background for facet-wrapped plots to white, removing the standard grey ggplot background colour and sets the title size of the facet-wrap title to font size 22)
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



## color palettes from wes anderson package. 

wes_chevalier <- c("#446455FF", "#FDD262FF", "#D3DDDCFF", "#C7B19CFF")

wes_moonrise2 <- c("#798E87FF", "#C27D38FF", "#CCC591FF", "#29211FFF")












