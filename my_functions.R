
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
  
  
 
  df <- cbind.data.frame(id, tax_yr, vars, names, titles, avg_hrs, avg_hrs_rltd, Trustee_dir_ind, 
                   rep_comp, rep_comp_rltd, other_comp)
  
  
  names(df)[1:15] <- c("EIN", "Org_name", "Return_type",
                       "Tax_yr", "Tax_yr_begin",
                       "Tax_yr_end",
                      "Street_add", 
                      "city", "state", "zip_code", 
                      id_vars2)
  
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
  
  
  
  df <- cbind.data.frame(id, tax_yr, vars, names, titles, avg_hrs, rep_comp,
                         ben_program_amt, other_allwnc_amt)
  
  
  names(df)[1:15] <- c("EIN", "Org_name", "Return_type",
                       "Tax_yr", "Tax_yr_begin",
                       "Tax_yr_end",
                       "Street_add", 
                       "city", "state", "zip_code", 
                       id_vars2)
  
  df
  
  
}




















