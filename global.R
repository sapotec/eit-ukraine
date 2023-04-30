#install packages
{
  if (!("dplyr" %in% installed.packages())) {install.packages("dplyr")}
  if (!("tidyr" %in% installed.packages())) {install.packages("tidyr")}
  if (!("stringr" %in% installed.packages())) {install.packages("stringr")}
  if (!("data.table" %in% installed.packages())) {install.packages("data.table")}
  if (!("readr" %in% installed.packages())) {install.packages("readr")}
  if (!("assertive" %in% installed.packages())) {install.packages("assertive")}
  if (!("readxl" %in% installed.packages())) {install.packages("readxl")}
  if (!("rjson" %in% installed.packages())) {install.packages("rjson")}
  if (!("stringi" %in% installed.packages())) {install.packages("stringi")}
  if (!("purrr" %in% installed.packages())) {install.packages("purrr")}
  if (!("ggmap" %in% installed.packages())) {install.packages("ggmap")}
  if (!("jsonlite" %in% installed.packages())) {install.packages("jsonlite")}
  if (!("leaflet" %in% installed.packages())) {install.packages("leaflet")}
  if (!("rvest" %in% installed.packages())) {install.packages("rvest")}
  if (!("xml2" %in% installed.packages())) {install.packages("xml2")}
  if (!("sf" %in% installed.packages())) {install.packages("sf")}
  if (!("tm" %in% installed.packages())) {install.packages("tm")}
  if (!("stringdist" %in% installed.packages())) {install.packages("stringdist")}
  if (!("mgsub" %in% installed.packages())) {install.packages("mgsub")}
  if (!("tictoc" %in% installed.packages())) {install.packages("tictoc")}
}

#load libraries
{
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(data.table)
  library(readr)
  library(assertive)
  library(readxl)
  library(jsonlite)
  library(stringi)
  library(purrr)
  library(ggmap)
  library(jsonlite)
  library(leaflet)
  library(xml2)
  library(rvest)
  library(sf)
  library(tm)
  library(stringdist)
  library(mgsub)
  library(tictoc)
}

#set environment parameters
{
  #set workspace directory
  setwd("G:/Other computers/My Laptop/R/ZNO/ZNO")
  
  #setting proper locale
  #Sys.setlocale('LC_ALL','ukrainian')
  
  #setting for skipping exponential notation
  options(scipen = 999)
  
}

#resources
{
  #path to source files
  path_2022 <- "resources/ZNO_2010-2020/NMT2022/Odata2022File.csv"
  path_2021 <- "resources/ZNO_2010-2020/ZNO2021/Odata2021File.csv"
  path_2020 <- "resources/ZNO_2010-2020/ZNO2020/Odata2020File.csv"
  path_2019 <- "resources/ZNO_2010-2020/ZNO2019/Odata2019File.csv"
  path_2018 <- "resources/ZNO_2010-2020/ZNO2018/OpenData2018.csv"
  path_2017 <- "resources/ZNO_2010-2020/ZNO2017/OpenData2017.csv"
  path_2016 <- "resources/ZNO_2010-2020/ZNO2016/OpenData2016.csv"
  
  #path to ZNO raw dataset
  path_zno_raw <- "outputs/ZNO_2016+_raw_data.csv"
  
  #path to ZNO tests dataset
  path_zno_tests <- "outputs/ZNO_2016+_tests_data.csv"
  
  #path to ZNO schools dataset
  path_zno_schools <- "outputs/ZNO_2016+_school_data.csv"
  
  #path to new toponimics
  path_toponimics <- "resources/KOATUU-ATU/New_toponimics.csv"
  
  
  
  #path to KOATUU json 
  #path_KOATUU_json <- "https://data.gov.ua/dataset/d945de87-539c-45b4-932a-7dda57daf8d9/resource/296adb7a-476a-40c8-9de6-211327cb3aa1/download/koatuu.json"
  path_KOATUU_json <- "resources/KOATUU-ATU/koatuu.json"
  
  #path to KOATUU refinement file
  KOATUU_ref_file <- "resources/KOATUU-ATU/KOATUU_refinement.csv"
  
  #path to old KOATUU json
  path_old_KOATUU <- "resources/KOATUU-ATU/oldest_KOATUU.csv"
  
  #path to exceptions of toponimics
  path_toponimics_exception <- "resources/KOATUU-ATU/toponimic_exceptions.csv"
  
  gen_KOATUU_list <- list(reg="regname",area="areaname",
                           ter="tername",col_pref="general")
  
  orig_KOATUU_list <- list(reg="regname",area="areaname",hrom="hromname",
                           ter="tername",col_pref="orig")
  
  school_KOATUU_list <- list(reg="eoregname",area="eoareaname",hrom="eohromname",
                             ter="eotername",col_pref="school")
  
  
  
  #path to ATU file
  ATU_compare_file <- "resources/KOATUU-ATU/compare_table_KOATUU-ATU.xlsx"
  
  #path to ATU codifier excel
  ATU_codifier_xl <- "resources/KOATUU-ATU/ATU_codifier.xlsx"
  
  #path to ATU codifier csv
  csv_ATU_codifier <- "resources/KOATUU-ATU/ATU_codifier.csv"
  
  category_list <- list(
    district = "B",
    cities = c("M","T"),
    villages = "C",
    selusche= "X",
    misto = "M",
    smt = "T",
    hrom ="H",
    raion ="P",
    obl = "O",
    adm = "K")
  
  hromada_dict_path <- "resources/KOATUU-ATU/Hromadas_dictionary.csv"
  
  #Kyiv and Sevastopol ATU codes
  ATU_special_regions <- list(
    Kyiv ="UA80",
    Sevastopol="UA85")
  
  
  #define vector with subject names
  subjects <- c("uml","ukr","hist","math","mst","phys","chem","bio",
                "geo","eng","fra","deu","spa","rus")
  
  #links to institutions data
  inst_links_list <- list(
    schools_api="https://registry.edbo.gov.ua/api/institutions/?ut=3&lc=0&exp=json",
    vnz_api="https://registry.edbo.gov.ua/api/universities/?ut=1&lc=0&exp=json",
    ptu_api="https://registry.edbo.gov.ua/api/universities/?ut=2&lc=0&exp=json",
    pre_vnz_api="https://registry.edbo.gov.ua/api/universities/?ut=9&lc=0&exp=json")
  
  #links to Nomnatim search API
  nomin_url_start <- "https://nominatim.openstreetmap.org/search?q="
  nomin_url_end <- "&format=jsonv2&addressdetails=1&accept-language=ua&countrycodes=ua&dedupe=0"
  
  #path to new ATU geojsons
  path_hromadas <- "resources/OSM data/terhromad.geojson"
  path_rayons <- "resources/OSM data/rayon.geojson"
  path_oblast <- "resources/OSM data/oblast.geojson"
  
  #path to school resources
  Crimea_schools <- "resources/institutions/crimea_dataset.csv"
  Donetsk_schools <- "resources/institutions/don_occupied.csv"
  Lugansk_schools <- "resources/institutions/lug_occupied.csv"
  inst_new <- "resources/institutions/institutions_new.csv"
  inst_closed <- "resources/institutions/inst_closed.csv"
  inst_names <- "resources/institutions/inst_names.csv"
  
  #path to school combined datasets
  institutions <- "resources/institutions/institutions.csv"
  institutions_occupied <- "resources/institutions/institutions_occup.csv"
  institutions_full <- "resources/institutions/institutions_full.csv"
  
}

#helpers
{
  #remove extra spaces, new lines and slashes
  remove_extra_symbols <- function(data) {
    
    tic("removing extra symbols")
    
    #remove new lines,quotes,apostrophe
    pat1 <- '\\n|\"|\u00AB|\u00BB|\u201E|\u201D'
    pat2 <- '\u0060|’'
    pat3 <- '\\/'
    data <- data %>% mutate(across(everything(),
                                   ~stri_replace_all_regex(.,c(pat1,pat2,pat3),
                                                          c("","'","-"),
                                                          vectorize_all = FALSE)))
    # data <- data %>% 
    #   mutate_all(~stringr::str_replace_all(.,"\\n|\"|\u00AB|\u00BB|\u201E|\u201D|\u0060|\u2019|\\/","")) %>% 
    #   mutate_all(~trimws(.))

    #remove leading and trailing spaces
    data <- data %>% mutate(across(everything(),~ trimws(.)))
    
    toc(log=TRUE)
    
    return(data)
    
  }
  
  #resolve toponyms duplication within counties (areas)
  toponimic_exceptions <- function(data,cols_targ) {
    
    tic("resolve toponyms duplications")
    
    t_exc <- read_csv(path_toponimics_exception,
                      col_types = cols(.default = "c"))
    
    d <- data.frame()
    
    for(i in 1:nrow(t_exc)) {
      
       f_d <- data[get(cols_targ$reg)==t_exc$regname[i] & 
                   get(cols_targ$area)==t_exc$areaname[i] & 
                   get(cols_targ$ter)==t_exc$tername[i] & 
                   eoname %like% t_exc$marker[i]] %>% 
                mutate(KOATUU_first=t_exc$KOATUU_first[i],
                        KOATUU_second=t_exc$KOATUU_second[i],
                        KOATUU_third=t_exc$KOATUU_third[i],
                        KOATUU_fourth=t_exc$KOATUU_fourth[i],
                        !!cols_targ$ter:=t_exc$new_tername[i],
                        !!cols_targ$area:=t_exc$new_areaname[i])
      
       d <- bind_rows(d,f_d)
      
    }
    
    d <- d %>% distinct(outid,.keep_all = TRUE)
    data <- data %>% distinct(outid,.keep_all = TRUE)
    
    data <- data[!d, on="outid"] %>% bind_rows(.,d)
    
    toc(log=TRUE)
    
    return(data)
    
  }
  
  #rename obsolete toponyms
  rename_toponimics <- function(data,path, cols_list) {
    
    tic("rename obsolete toponyms")
    
    #fixing number of columns for dataset
    colnum <- ncol(data)
    
    #read file with old-new toponyms mapping
    toponimics <- fread(path,encoding = "UTF-8",na.strings = c("",NA))
    
    #old-new map for counties(areas)
    topon_areas <- toponimics[is.na(ter_old) & is.na(ter_new) & !is.na(region),]
    
    #old-new map for big towns
    topon_towns <- toponimics[!is.na(ter_old) & !is.na(ter_new) & !is.na(area_old) & !is.na(area_new),]
    
    #old-new map for villages and small towns
    topon_villages <- toponimics[!is.na(ter_old) & !is.na(ter_new) & is.na(area_old),]
    
    #old-new map for settlements subsequent to regional towns
    topon_subsequent <- toponimics[is.na(ter_old) & !is.na(ter_new),]
    
    #rename old counties (areas)
    areas <- data[topon_areas,on=c(paste0(cols_list$reg,"==","region"),
                                   paste0(cols_list$area,"==","area_old")),nomatch=0] %>% 
      mutate(across(.cols = matches(paste0('^',cols_list$area)), ~ area_new)) %>% 
      select(1:all_of(colnum))
    
    data <- data[!areas,on="outid"] %>% bind_rows(.,areas)
    
    #rename principal towns and its districts
    town_dist <- data[topon_towns,on=c(paste0(cols_list$reg,"==","region"),
                                       paste0(cols_list$area,"==","area_old"),
                                       paste0(cols_list$ter,"==","ter_old")),nomatch=0] %>%
      mutate(across(.cols = matches(paste0('^',cols_list$area)), ~ area_new)) %>%
      mutate(across(.cols = matches(paste0('^',cols_list$ter)), ~ ter_new)) %>%
      select(1:all_of(colnum))
    
    data <- data[!town_dist, on="outid"] %>% bind_rows(., town_dist)
    
    #rename villages and small towns
    villages_dist <- data[topon_villages,
                          on=c(paste0(cols_list$reg,"==","region"),
                               paste0(cols_list$area,"==","area_new"),
                               paste0(cols_list$ter,"==","ter_old")),nomatch=0] %>%
      mutate(across(.cols = matches(paste0('^',cols_list$ter)), ~ ter_new)) %>%
      select(1:all_of(colnum))

    data <- data[!villages_dist, on="outid"] %>% bind_rows(.,villages_dist)
    
    #rename subsequent settlments
    subs_dist <- data[topon_subsequent,
                      on=c(paste0(cols_list$reg,"==","region"),
                           paste0(cols_list$area,"==","area_old"),
                           paste0(cols_list$ter,"==","ter_new")),nomatch=0] %>%
      mutate(across(.cols = matches(paste0('^',cols_list$area)), ~ area_new)) %>%
      select(1:all_of(colnum))

    data <- data[!subs_dist, on="outid"] %>% bind_rows(., subs_dist)
    
    toc(log=TRUE)
    
    return(data)
    
  }
  
  #prepare a named list of datasets with KOATUU data
  get_KOATUU_dataset <- function(KOATUU_path, KOATUU_ref) {
    
    tic("preparing KOATUU dataset")
    
    #read KOATUU json from internet and rename columns
    KOATUU <- as.data.frame(fromJSON(KOATUU_path)) %>% 
      rename(KOATUU_first=`Перший рівень`,
             KOATUU_second=`Другий рівень`,
             KOATUU_third=`Третій рівень`,
             KOATUU_fourth=`Четвертий рівень`,
             category=`Категорія`,
             name=`Назва об'єкта українською мовою`)
    
    #make the toponymic names comparable
    KOATUU <- KOATUU %>% 
      mutate(across(name, ~ str_replace_all(.,"\\/.*$|^(М\\.)",""))) %>%
      mutate(across(name, ~ str_to_title(.))) %>%
      mutate(across(name, ~ gsub(.,pattern = "Область",replacement = "область"))) %>%
      mutate(across(name, ~ gsub(.,pattern = "Район",replacement = "район"))) %>%
      mutate(category = if_else(KOATUU_fourth=="6320483601","Щ",category)) %>% 
      as.data.table()
    
    #read local KOATUU refinement file and rename columns
    KOATUU_ref <- fread(KOATUU_ref, encoding = "UTF-8", colClasses = 'character') %>% 
      rename(KOATUU_first=`Перший рівень`,
             KOATUU_second=`Другий рівень`,
             KOATUU_third=`Третій рівень`,
             KOATUU_fourth=`Четвертий рівень`,
             category=`Категорія`,
             name=`Назва об'єкта українською мовою`) %>% 
      as.data.table()
    
    #delete duplicates in counties
    KOATUU <- KOATUU[!KOATUU_ref, on=.(KOATUU_third,KOATUU_fourth)]
    
    toc(log=TRUE)
    
    return(KOATUU)
    
  }
  
  #prepare a named list of datasets with ATU
  get_ATU_data <- function(comparator_path, ATU_file) {
    
    tic("prepare ATU list")
    
    #load KOATUU-ATU map file
    #exclude regions, areas, hromadas
    ATU_compare <- read_excel(comparator_path) %>% 
      rename(ATU_code='Кодифікатор',
             KOATUU_code="Код об'єкта КОАТУУ",
             category="Категорія об’єкта",
             name="Назва об’єкта") %>% 
      filter(category!="Н")
    
    #filter only cities districts
    ATU_compare_dist <- ATU_compare %>% 
      filter(!category %in% c("О","К","Р"), category=="В") %>% as.data.table()
    
    #exclude cities districts
    ATU_compare_towns <- ATU_compare %>% 
      filter(!category %in% c("О","К","Р","В")) %>% as.data.table()
    
    ATU_full <- read_excel(ATU_file, guess_max = 10000) %>% 
      rename(ATU_first='Перший рівень',
             ATU_second='Другий рівень',
             ATU_third='Третій рівень',
             ATU_fourth='Четвертий рівень',
             ATU_extra='Додатковий рівень',
             category="Категорія об’єкта",
             name="Назва об’єкта") %>% as.data.table()
    
    ATU_full_towns <- ATU_full %>% filter(!category %in% c("O","P","K")) %>% as.data.table()
    
    
    ATU_list <- list(ATU_compare_distr=ATU_compare_dist,
                     ATU_compare_towns=ATU_compare_towns,
                     ATU_full=ATU_full,
                     ATU_full_towns=ATU_full_towns,
                     ATU_compare=ATU_compare)
    
    toc(log=TRUE)
    
    return(ATU_list)
    
  }
  
  #read and clean full list of institutions
  read_full_institutions <- function(inst_full_path) {
    
    tic("reading and cleaning full list of institutions")
    
    p <- c("\\(до 2022 р.\\)|\\(до 2021 р.\\)|\\(до 2020 р.\\)","'",
           "ліцей \\(ліцей-інтернат\\)","коллегіум")
    
    rp <- c("","’","ліцей-інтернат","колегіум")
    
    data <- fread(inst_full_path, encoding = "UTF-8") %>% 
      mutate(across(c(lat,long),as.numeric)) %>% 
      mutate(across(type, ~str_to_lower(.x) %>%  mgsub(p,rp) %>% str_trim())) %>%
      mutate(across(c(type,category,occupied),as.factor))
    
    toc(log=TRUE)
    
    return(data)
    
  }
  
}

#functions
{
  #get list of KOATUU levels
  get_KOATUU_levels <- function(KOATUU_path, KOATUU_ref) {
    
    KOATUU <- get_KOATUU_dataset(KOATUU_path, KOATUU_ref)
    
    tic("composing list of KOATUU levels")
    
    #prepare subset for regions 
    first_only <- KOATUU %>% filter(KOATUU_second=="") %>% 
      select(KOATUU_first,category,name)
    
    #prepare subset for areas,regions capitals, districts for Kyiv and Sevastopol,
    #regional towns
    first_and_second <- KOATUU %>% 
      filter(KOATUU_second!="",KOATUU_third=="") %>% 
      select(KOATUU_first,KOATUU_second,category,name)
    
    #prepare subset for towns and regional towns' districts
    first_to_third <- KOATUU %>% 
      filter(KOATUU_third!="",KOATUU_fourth=="", category!="")
    
    #prepare subset for small and subsequent towns
    first_to_fourth_no_villages <- KOATUU %>% 
      filter(KOATUU_fourth!="",category!="С", category!="C" )
    
    #prepare subset for villages
    first_to_fourth_villages <- KOATUU %>% 
      filter(KOATUU_fourth!="",category!="Щ", category!="Т" )
    
    #make the list of subsets
    KOATUU_list <- list(first=first_only, 
                        first_and_second=first_and_second,
                        first_to_third=first_to_third, 
                        fourth_no_villages=first_to_fourth_no_villages,
                        fourth_villages=first_to_fourth_villages)
    
    toc(log=TRUE)
    
    return(KOATUU_list)
    
  }
  
  #add columns with KOATUU data
  add_KOATUU <- function(data,path,refinement,targ_cols) {
    
    #initial <- data
    
    initial_rows <- nrow(data)
    initial_columns <- ncol(data)
    
    #get a named list with KOATUU data
    KOATUU_list <- get_KOATUU_levels(path,refinement)
    
    tic("adding KOATUU columns")
    
    #add KOATUU first column
    data <- data %>% left_join(as.data.table(KOATUU_list$first),by=setNames("name",targ_cols$reg)) %>% 
      relocate(KOATUU_first,.after = targ_cols$ter) %>% 
      select(1:(all_of(initial_columns)+1))
    
    #add KOATUU codes to second column for regional towns, Kyiv and Sevastopol districts
    reg_towns_pr_dist <- data[KOATUU_list$first_and_second, 
                              on=c("KOATUU_first",paste0(targ_cols$ter,"==","name")),nomatch=0] %>% 
      relocate(KOATUU_second,.after = KOATUU_first) %>% 
      select(1:(all_of(initial_columns)+2))
    
    #add KOATUU codes to second column for counties (areas)
    areas <- data[!reg_towns_pr_dist, on=.(outid)][KOATUU_list$first_and_second,
                                                   on=c("KOATUU_first",paste0(targ_cols$area,"==","name")),nomatch=0] %>% 
      relocate(KOATUU_second,.after = KOATUU_first) %>% 
      select(1:(all_of(initial_columns)+2))
    
    data <- bind_rows(reg_towns_pr_dist,areas)
    
    #make subset for 3rd and 4th KOATUU columns
    data_with_other_KOATUU <- data[!KOATUU_list$first_and_second,
                on=c("KOATUU_first",paste0(targ_cols$ter,"==","name"))] %>% 
      mutate(KOATUU_third=NA,KOATUU_fourth=NA) %>%
      select(1:KOATUU_fourth,everything())
    
    #KOATUU first and second only subset
    data_with_firs_and_sec_only <- data[!data_with_other_KOATUU,on="outid"] %>%
      select(1:KOATUU_second,everything())
    
    #add KOATUU 3rd and 4th columns for small and subsequent towns
    t_and_f_wo_villages <- data_with_other_KOATUU[KOATUU_list$fourth_no_villages,
                 on=c("KOATUU_first","KOATUU_second",
                      paste0(targ_cols$ter,"==","name")),nomatch=0] %>% 
      mutate(KOATUU_third=i.KOATUU_third, KOATUU_fourth=i.KOATUU_fourth)
    
    #add KOATUU 3rd and 4th columns for villages
    t_and_f_villages <- data_with_other_KOATUU[!t_and_f_wo_villages, on="outid"][KOATUU_list$fourth_villages,
                 on=c("KOATUU_first","KOATUU_second",
                     paste0(targ_cols$ter,"==","name")),nomatch=0] %>% 
      mutate(KOATUU_third=i.KOATUU_third, KOATUU_fourth=i.KOATUU_fourth)
    
    t_and_f <- bind_rows(t_and_f_wo_villages,t_and_f_villages) %>% 
      relocate(KOATUU_third,KOATUU_fourth, .after = KOATUU_second)
    
    #make a subset without villages and subsequent towns
    data_only_third<- data_with_other_KOATUU[!t_and_f, on="outid"]
    
    #add KOATUU 3rd column for towns and districts of regionals towns
    third <- data_only_third[KOATUU_list$first_to_third,
                 on=c("KOATUU_first","KOATUU_second",
                      paste0(targ_cols$ter,"==","name")),nomatch=0] %>% 
      mutate(KOATUU_third=i.KOATUU_third) %>% 
      relocate(KOATUU_third,.after = KOATUU_second)
    
    #combine subsets into full dataset
    data <- bind_rows(data_with_firs_and_sec_only,third,t_and_f) %>% 
      relocate(KOATUU_third,KOATUU_fourth, .after = KOATUU_second) %>% 
      select(1:(all_of(initial_columns)+4))
    
    toc(log=TRUE)
    
    #correct by exceptions
    if(targ_cols$col_pref!="general") {

      data <- toponimic_exceptions(data,targ_cols)

    }
    
    tic("finalize KOATUU columns in general dataset")

    #return(data %>% group_by(outid) %>% filter(n()>1))

    data <- data %>%
      rename_with(.cols =  starts_with("KOATUU"),
                  .fn= ~ paste(targ_cols$col_pref,.,sep="_"))

    #make sure that there are no duplicates in Students IDs
    assert_all_are_equal_to(sum(duplicated(data$outid)),0)

    #make sure that number of rows hasn't changed
    assert_all_are_equal_to(initial_rows,nrow(data))

    #return(list(initial,data))

    toc(log=TRUE)
    
    return(data)
    
  }
  
  #add colunms with ATU data
  add_ATU <- function(data,comp_path,full_path,cols_pref) {
    
    #initial <- data
    
    initial_rows <- nrow(data)
    
    ATU_list <- get_ATU_data(comp_path,full_path)
    
    tic("adding ATU columns to general dataset")
    
    col4 <- paste0(cols_pref,"_","KOATUU_fourth")
    col3 <- paste0(cols_pref,"_","KOATUU_third")
    col2 <- paste0(cols_pref,"_","KOATUU_second")
    col1 <- paste0(cols_pref,"_","KOATUU_first")
    
    dist_one <- data[is.na(get(col4)),][ATU_list$ATU_compare_distr,
                         on=paste0(col3,"==","KOATUU_code"),nomatch=0]

    dist_two <- data[is.na(get(col3)),][ATU_list$ATU_compare_distr,
                            on=paste0(col2,"==","KOATUU_code"),nomatch=0]
    
    dist <- bind_rows(dist_one,dist_two) %>% 
      rename_with(.cols=contains("ATU_code"),~ str_c(cols_pref,"_","ATU_extra"))
    
    towns_one <- data[!is.na(get(col4)),][ATU_list$ATU_compare_towns,
                                on=paste0(col4,"==","KOATUU_code"),nomatch=0]
    
    towns_two <- data[is.na(get(col4)),][ATU_list$ATU_compare_towns,
                                on=paste0(col3,"==","KOATUU_code"),nomatch=0]
    
    towns_three <- data[is.na(get(col3)),][ATU_list$ATU_compare_towns,
                                on=paste0(col2,"==","KOATUU_code"),nomatch=0]
    
    towns <- bind_rows(towns_one,towns_two,towns_three) %>% 
      rename_with(.cols=contains("ATU_code"),~ str_c(cols_pref,"_","ATU_fourth"))
    
    data <- bind_rows(dist,towns) %>% 
      relocate(str_c(cols_pref,"_","ATU_fourth"),str_c(cols_pref,"_","ATU_extra"), 
               .after = str_c(cols_pref,"_","KOATUU_fourth")) %>% 
      select(-c(name,category))
    
    prepared_ATU_distr <- ATU_list$ATU_full_towns %>% 
      filter(category=="B")
    
    prepared_ATU <- ATU_list$ATU_full_towns %>% 
      filter(category!="B")
    
    fcole=paste0(cols_pref,"_","ATU_extra")
    fcol4=paste0(cols_pref,"_","ATU_fourth")
    
    
    final_one <- data[complete.cases(data[,get(fcole)]),][prepared_ATU_distr,
                             on=paste0(fcole,"==","ATU_extra"),nomatch=0] %>% 
      select(-(str_c(cols_pref,"_","ATU_fourth"))) %>% 
      rename_with(.cols=starts_with("ATU_fourth"), ~ str_c(cols_pref,"_",.))
    
    final_two <- data[!complete.cases(data[,get(fcole)]),][prepared_ATU,
                                on=paste0(fcol4,"==","ATU_fourth"),nomatch=0] %>% 
      rename_with(.cols=starts_with("ATU_fourth"), ~ str_c(cols_pref,"_",.))
    
    data <- bind_rows(final_one,final_two) %>% 
      select(-c(category,name, ATU_extra)) %>% 
      rename_with(.cols=starts_with("ATU_"),~ str_c(cols_pref,"_",.)) %>% 
      relocate(str_c(cols_pref,"_","ATU_first"):str_c(cols_pref,"_","ATU_third"),
               str_c(cols_pref,"_","ATU_fourth"),
               .after = str_c(cols_pref,"_","KOATUU_fourth"))
    
    #return(list(initial,data))
    
    if(cols_pref!="school") {
      
      #make sure that number of rows hasn't changed
      assert_all_are_equal_to(initial_rows,nrow(data))
      
    }
    
    toc(log=TRUE)
    
    return(data)
    
  }
  
}
