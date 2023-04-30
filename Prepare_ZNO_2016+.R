source("global.R")

#helpers
{
  
  #reading data from file
  read_data <- function(path){
    
    tic("reading dataset from file")
    
    #read from source file
    zno <- fread(path,
                 na.strings = "",
                 dec = ".",
                 encoding = "UTF-8")
    
    #convert all colunm names to lower case
    names(zno) <- tolower(names(zno))
    
    #convert outid column to lower
    zno <- zno %>% mutate(across(outid, tolower))
    
    toc(log=TRUE)
    
    return(zno)
  }
  
  #rename columns ukr to uml till 2021
  rename_ukr <- function(data) {
    
    tic.clearlog()
    tic("rename ukr columns to uml")
    
    data <- data %>% 
      rename_with(.cols =  matches("^ukr"), .fn = ~ str_replace(.x,"ukr","uml"))
    
    toc(log=TRUE)
    
    return(data)
      
  }
  
  #remove settlements names
  remove_extra_reg_data <- function(data) {
    
    tic.clearlog()
    tic("removing useless parts in values")
    
    patterns <- c("^(с\\.)|^(м.)|^(с-ще)|^(смт)|район міста$|\\((.*)\\)|\\..*|,[^,]*$")
    
    columns <- c("regname","areaname","tername",
                 "eoregname","eoareaname","eotername")
    
    data <- data %>% 
      mutate(across(ends_with(columns), ~gsub(.,pattern = patterns,replacement =  ""))) %>% 
      mutate(across(ends_with(columns), ~trimws(.))) %>% 
      mutate(across(ends_with("tername"), ~str_to_title(.)))
  
    toc(log=TRUE)
    return(data)

  }
  
  #read ATU codifier file from csv
  get_ATU_codifier_list <- function(codifier_path) {
    
    tic.clearlog()
    tic("read full ATU codifier")
    
    ATU_all <- fread(codifier_path, na.strings = "", encoding = "UTF-8") %>% 
      setnames(1:5, paste0("ATU_", c("first", "second", "third", "fourth", "extra"))) %>%
      setnames(6:7, c("category", "name")) %>% 
      remove_extra_symbols(.) %>% 
      distinct(ATU_third,category,name, .keep_all = TRUE)
    
    oblast <- ATU_all[category == category_list$obl, c(1, 7)]
    oblast$name <- ifelse(str_detect(oblast$name, "Крим"), oblast$name, str_c(oblast$name, " область"))
    
    adm <- ATU_all[category == category_list$adm, c(1, 7)]
    rajon <- ATU_all[category == category_list$raion, c(1, 2, 7)]
    rajon$name <- str_c(rajon$name, " район")
    
    hromada <- ATU_all[category == category_list$hrom, c(1:3, 7)]
    settlement <- ATU_all[category %in% c(category_list$cities, 
                                          category_list$villages, category_list$selusche), c(1:4, 7)]
    smt <- ATU_all[category == category_list$smt, c(1:4, 7)]
    misto <- ATU_all[category == category_list$misto, c(1:4, 7)]
    village <- ATU_all[category == category_list$villages, c(1:4, 7)]
    selusche <- ATU_all[category == category_list$selusche, c(1:4, 7)]
    district <- ATU_all[category == category_list$district, c(1:5, 7)]
    
    ATU <- list(oblast = oblast, adm = adm, rajon = rajon, hromada = hromada, settlement = settlement,
                smt = smt, misto = misto, village = village, selusche=selusche, district = district)
    
    return(ATU)
    
    toc(log=TRUE)
    return(ATU)
  }
  
  #read hromadas dictionary from file
  read_hromada_dict <- function(hromada_path) {
    
    tic.clearlog()
    tic("read hromada dictionary file")
    
    hromada_dict <- fread(hromada_path,na.strings = "",encoding = "UTF-8")
    
    hromada_dict$rajon <- paste(hromada_dict$rajon, "район")
    
    hromada_dict$oblast <- paste(hromada_dict$oblast, "область")
    
    toc(log=TRUE)
    return(hromada_dict)
    
  }
  
  #put proper status if ball < 100 for NMT
  align_teststatus <- function(data) {
    
    for(i in 1:length(subjects)) {
      
      prefix <- subjects[i]
      
      # Check if the prefix is present in any of the column names
      if (any(grepl(paste0("^", prefix), names(data)))) {
        
        ball100_col <- paste0(prefix, "ball100")
        
        teststatus_col <- paste0(prefix, "teststatus")
        
        data <- data %>%
          mutate(
            !!teststatus_col := case_when(
              #is.na(get(ball100_col)) ~ NA_character_,
              get(ball100_col) < 100 ~ "Не подолав поріг",
              TRUE ~ as.character(.data[[teststatus_col]])))
      }
      
    }
    
    return(data)
    
  }
  
  #align types of settlements with types that were till 2021
  align_tertypename <- function(data) {
    
    tic.clearlog()
    tic("align setllements types")
    
    data <- data %>% 
      mutate(across(tertypename,
              ~case_when(
                 str_detect(.,"селище, село") ~ "село",
                 str_detect(.,"селище міського типу") ~ "місто",
                 str_detect(.,"місто") ~ "місто")))
    
    toc(log=TRUE)
  
    return(data)         
             
  }
  
  #unification of graduates types
  align_regtypename <- function(data) {
    tic.clearlog()
    tic("unify graduates type")
    
    data <- data %>% 
      mutate(across(regtypename,~case_when(
        str_detect(.,"року|здобуде") ~ 'Випускник поточного року',
        str_detect(.,"Учень") ~ 'Учень закладу професійної освіти',
        str_detect(.,"покарань") ~ 'Випускник минулих років',
        TRUE ~ .)))
    
    toc(log=TRUE)
    
    return(data)
    
  }
  
  align_classprofilename <- function(data) {
    
    tic.clearlog()
    tic("impute missing profiles")
    
    opt <- c("Випускник поточного року",
             "Студент закладу вищої освіти",
             "Учень закладу професійної освіти")
    
    data <- data %>% 
      mutate(across(classprofilename, ~ case_when(
        str_detect(.,"багатопрофільність") ~ 'Багатопрофільний',
        (!is.na(eoname) & is.na(classprofilename)) ~ 'Немає',
        TRUE ~ .)))
    
    toc(log=TRUE)
    
    return(data)
    
  }
  
  #assign proper datatypes for all column
  assign_datatypes <- function(data) {
    
    tic("assigning datatypes")
    
    data <- data %>%  
      mutate(across(ends_with("ball100"), as.numeric),
             across(c(matches("ball12$|ball$|birth")), as.integer),
             across(c(matches("name$|test$|lang$|status$")), as.factor),
             across(c(matches("ptname$|ptregname$|ptareaname$|pttername$|eoname|KOATUU_|ATU_")), as.character))
    
    toc(log=TRUE)

    return(data)
    
  }
  
  #add column year to each dataset
  add_year <- function(data, year) {
    
    tic("adding year column to general dataset")
    
    data <- data %>% mutate(year=as.integer(year)) %>% 
      relocate(year,outid,everything())
    
    toc(log=TRUE)
    
    return(data)
    
  }
  
  #assert a final raw dataset
  assert_raw_data <- function(data,year) {
    
    tic("asserting raw dataset")
    
    # assert NA for required fields
    required_string <- "year|outid|birth|^sextypename|^regname|^areaname|^hromname|^tername|
    ^regtypename|^tertypename|ATU_first|ATU_second|ATU_third|ATU_fourth"
    
    assert_all_are_not_na(data %>% select(matches(required_string)))
    
    #check year of birth range
    assert_all_are_in_closed_range(data$birth,lower = 1900,upper = year-10)

    #check number of genders
    assert_all_are_equal_to(nlevels(data$sextypename),2)
    
    #check number of regions
    assert_all_are_less_than_or_equal_to(nlevels(data$regname),27)
    
    #check types of settlement
    assert_all_are_equal_to(nlevels(data$tertypename),2)
    
    #check types of students' background
    assert_all_are_greater_than_or_equal_to(nlevels(data$regtypename),3)
    
    #check equality of NAs for class profiles and school names
    if ("classprofilename" %in% colnames(data)) {
      assert_all_are_equal_to(sum(is.na(data$classprofilename)),sum(is.na(data$eoname)))
    }
    
    #check equality of NAs for school region and school names
    assert_all_are_equal_to(sum(is.na(data$eoregname)),sum(is.na(data$eoname)))
    
    #check equality of NAs for school area and school names
    assert_all_are_equal_to(sum(is.na(data$eoareaname)),sum(is.na(data$eoname)))
    
    #check equality of NAs for school town and school names
    assert_all_are_equal_to(sum(is.na(data$eoregname)),sum(is.na(data$eoname)))
    
    #check ball100 range
    ball100 <- data %>% select(matches("ball100$"))
    for (i in 1:length(names(ball100))) {
      
      assert_all_are_in_range(ball100[[i]],lower = 0,upper = 200,na_ignore = TRUE)
      
    }
    
    #check ball12 range
    ball12 <- data %>% select(matches("ball12$"))
    if(length(ball12>0)) {
      
      for (i in 1:length(names(ball12))) {
      
        assert_all_are_in_range(ball12[[i]],lower = 1,upper = 12,na_ignore = TRUE)
      
      }
    }
    
    #check ball range
    ball <- data %>% select(matches("ball$"))
    if(length(ball)>0) {
    
      for (i in 1:length(names(ball))) {
      
        assert_all_are_in_range(ball[[i]],lower = 0,upper = 120,na_ignore = TRUE)
      
      }
      
    }
    
    toc(log=TRUE)
    
  }
  
  #assert a final test dataset
  assert_tests_data <- function(raw,data) {
    
    tic("asserting test dataset")
    
    source_df <- raw %>% select(matches("test$"))
    
    assert_all_are_equal_to(sum(!is.na(source_df)),nrow(data))
    
    toc(log=TRUE)
    
  }
  
  #assert a final schools dataset
  assert_schools_data <- function(data,yr) {
    
    tic("asserting schools dataset")
    
    # assert NA for required fields
    required_string <- "^eoregname|^eoareaname|^eohromname|^eotername|ATU_first|ATU_second|ATU_third|ATU_fourth"
    
    assert_all_are_not_na(data %>% select(matches(required_string)))
    
    raw <- read_data(path_zno_tests) %>% filter(year==yr)
    
    source_df <- raw %>% 
      filter(!is.na(eoname),!is.na(ball100),ball100>0) %>% 
      group_by(eoname,test) %>% 
      tally()
    
    assert_all_are_equal_to(nrow(source_df),nrow(data))
    
    toc(log=TRUE)
    
  }
  
  #remove KOATUU columns
  remove_KOATUU <- function(data) {
    
    tic("remove KOATUU columns")
    
    data <- data %>% select(-c(contains("KOATUU")))
    
    toc(log=TRUE)
    
    return(data)
    
  }
  
  #compare datasets by columns
  compare_columns <- function(base,comp) {
    
    res <- compare_df_cols_same(base,comp,bind_method = "rbind")
    
    if(!res){
      
      mism <- compare_df_cols(base,comp,return = "mismatch", 
                                    bind_method = "rbind")
      
      mism_base <- mism %>% filter(is.na(base)) %>% select(column_name)
      
      print(mism_base)
      
    }
    
  }
  
  #remove duplicated students IDs
  remove_full_duplicates <- function(data) {
    
    tic("removing full duplicates")
    
    if(sum(duplicated(data$outid))>0) {
      
      data <- data %>% distinct(outid, .keep_all = TRUE)
      
    }
    
    toc(log=TRUE)
    
    return(data)
    
  }
  
  #remove inconsistent data from tests toponimic columns
  tests_location_consistent <- function(data) {
    tic("removing incorrect toponyms in tests locations")
    for(i in 1:length(subjects)) {
      
      s_name <- str_c(subjects[i],"ptname")
      reg_name <- str_c(subjects[i],"ptregname")
      area_name <- str_c(subjects[i],"ptareaname")
      ter_name <- str_c(subjects[i],"pttername")
      
      if(s_name %in% names(data)) {
        
        data <- data %>% 
          mutate(
            {{reg_name}}:= if_else(is.na(get(s_name)) & !is.na(get(reg_name)),NA_character_,get(reg_name)),
            {{area_name}}:= if_else(is.na(get(s_name)) & !is.na(get(area_name)),NA_character_,get(area_name)),
            {{ter_name}}:= if_else(is.na(get(s_name)) & !is.na(get(ter_name)),NA_character_,get(ter_name))
          )
      }
      
    }
    
    toc(log = TRUE)
    return(data)         
  }
  
}

#functions
{
  #add a territory type
  add_tertypename <- function(data,KOATUU_path,KOATUU_ref) {
    
    KOATUU <- get_KOATUU_dataset(KOATUU_path,KOATUU_ref)
    
    tic("adding tertypename column to the dataset")
    
    villages_categories <- c("C|С|Щ")
    
    towns_categories <- c("Р|М|Т")
    
    fourth <- data %>% 
      filter(!is.na(orig_KOATUU_fourth)) %>% 
      inner_join(KOATUU, by=c("orig_KOATUU_fourth"="KOATUU_fourth")) %>% 
      mutate(tertypename =
               case_when(str_detect(category,villages_categories) ~ "село",
                         str_detect(category,towns_categories) ~ "місто")) %>%
      relocate(tertypename, .after = regtypename) %>% 
      select(-c(KOATUU_first:name))
    
    third <- data %>% 
      filter(is.na(orig_KOATUU_fourth)) %>% 
      mutate(tertypename = "місто") %>% 
      relocate(tertypename, .after = regtypename)
    
    data <- bind_rows(third,fourth)
    
    toc(log=TRUE)
    
    return(data)
    
  }
  
  #add column with hromadas names
  add_hromname <- function(data,ter_list) {
    
    tic("adding column with hromadas names")
    
    ATU <- get_ATU_codifier_list(csv_ATU_codifier)
    
    hrom <- ATU$hromada %>% select(-c(ATU_first, ATU_second))
    
    data <-bind_rows(data[hrom,on=c(setNames("ATU_third",
                                paste0(ter_list$col_pref,"_","ATU_third"))), nomatch=0],
                     data[ATU$adm,on=c(setNames("ATU_first",
                                paste0(ter_list$col_pref,"_","ATU_third"))), nomatch=0]) %>%
      mutate(!!ter_list$hrom := name) %>% 
      select(-name) %>% 
      relocate(ter_list$hrom, .after=ter_list$area)
    
    toc(log=TRUE)
    
    return(data)
    
  }
  
  #delete artefacts from strings
  remove_useless_data <- function(data,...) {
    
    data <- data %>% 
      remove_extra_symbols(.) %>% 
      remove_extra_reg_data(.)
    
    return(data)
    
  }
  
  #add ATU columns to datasets 2022+
  add_ATU_new <- function(data,ter_list) {
    
    ATU <- get_ATU_codifier_list(csv_ATU_codifier)
    
    hr_dict <- read_hromada_dict(hromada_dict_path) 
    
    tic.clearlog()
    tic("add ATU to dataset 2022+")
    
    #ATU first
    obl <- bind_rows(data[ATU$oblast, on = c(setNames("name", ter_list$reg)), nomatch = 0],
                     data[ATU$adm, on = c(setNames("name", ter_list$reg)), nomatch = 0]) %>%
      relocate(ATU_first, .after = ter_list$ter)
    
    assert_all_are_equal_to(nrow(obl),nrow(data))
    assert_all_are_equal_to(sum(is.na(obl$ATU_first)),0)
    assert_all_are_equal_to(nrow(obl),n_distinct(obl$outid))
    
    #ATU second
    rajon <- bind_rows(obl[ATU$rajon,on=c(ATU_first="ATU_first",setNames("name", ter_list$area)),nomatch=0],
                       obl[get(ter_list$area) %in% c(ATU$adm$name),][,ATU_second:=ATU_first],
                       obl[!is.na(ter_list$hrom)][hr_dict,
                            on=c(setNames("oblast", ter_list$reg),setNames("adm_center", ter_list$area)),
                            nomatch=0][ATU$rajon, on=c(ATU_first="ATU_first",rajon="name"),nomatch=0]) %>% 
      relocate(ATU_first,ATU_second, .after = ter_list$ter) %>% 
      select(-c(name:population))
    
    assert_all_are_equal_to(nrow(rajon),nrow(data))
    assert_all_are_equal_to(sum(is.na(rajon$ATU_second)),0)
    assert_all_are_equal_to(nrow(rajon),n_distinct(rajon$outid))
    
    #ATU third & extra
    hrom_dist <- rajon[ATU$district, 
                       on=c(ATU_first="ATU_first",ATU_second="ATU_second",
                            setNames("name", ter_list$ter)),nomatch=0]
    
    hrom <- rajon[!hrom_dist,on=.(outid)][!is.na(ter_list$hrom),][ATU$hromada,
                  on=c(ATU_first="ATU_first",ATU_second="ATU_second",
                       setNames("name", ter_list$hrom)),nomatch=0]
    
    h_third <- bind_rows(hrom_dist,hrom)
    
    hromada <- bind_rows(h_third,rajon[!h_third,on=.(outid)][ATU$settlement,
                          on=c(ATU_first="ATU_first",ATU_second="ATU_second",
                               setNames("name", ter_list$ter)),nomatch=0]) %>% 
      relocate(ATU_third:ATU_extra, .after = ATU_second) #%>% 
      #select(c(outid:ukrpttername))
    
    assert_all_are_equal_to(nrow(hromada),nrow(data))
    assert_all_are_equal_to(sum(is.na(hromada$ATU_third)),0)
    assert_all_are_equal_to(nrow(hromada),n_distinct(hromada$outid))
    
    #ATU_fourth
    fourth <- bind_rows(hromada[is.na(ATU_fourth)&tertypename=="місто",][ATU$misto,
                                on=c(ATU_third="ATU_third",setNames("name", ter_list$ter)),nomatch=0],
                        hromada[is.na(ATU_fourth)&tertypename=="селище міського типу",][ATU$smt,
                                on=c(ATU_third="ATU_third",setNames("name", ter_list$ter)),nomatch=0],
                        hromada[is.na(ATU_fourth)&tertypename %in% c("селище, село","село"),][ATU$village,
                                on=c(ATU_third="ATU_third",setNames("name", ter_list$ter)),nomatch=0])%>% 
      mutate(ATU_fourth=i.ATU_fourth) %>% 
      select(-c(i.ATU_first,i.ATU_second, i.ATU_fourth)) %>% 
      relocate(ATU_fourth, .after=ATU_third)
    
    f_s <- hromada[!fourth,on=.(outid)][is.na(ATU_fourth)&tertypename %in% c("селище, село","село"),][ATU$selusche,
                       on=c(ATU_third="ATU_third",setNames("name", ter_list$ter)),nomatch=0] %>% 
      mutate(ATU_fourth=i.ATU_fourth) %>% 
      select(-c(i.ATU_first,i.ATU_second, i.ATU_fourth))
    
    f_smt <- hromada[!fourth,on=.(outid)][is.na(ATU_fourth)&tertypename=="місто",][ATU$smt,
                       on=c(ATU_third="ATU_third",setNames("name", ter_list$ter)),nomatch=0] %>% 
      mutate(ATU_fourth=i.ATU_fourth) %>% 
      select(-c(i.ATU_first,i.ATU_second, i.ATU_fourth))
    
    f_na <- hromada[!is.na(ATU_fourth),]
    
    final <- bind_rows(f_na,f_s,f_smt,fourth) %>% 
      rename_with(.,.cols=starts_with("ATU"),.fn = ~ str_c(ter_list$col_pref,"_",.x))
    
    #return(list(data,final))
    
    assert_all_are_equal_to(nrow(final),n_distinct(final$outid))
    assert_all_are_equal_to(nrow(final),nrow(data))
    assert_all_are_equal_to(sum(is.na(final$ATU_fourth)),0)
    
    toc(log=TRUE)
    return(final)
  }
  
  #prepare raw dataset for 2016-2021
  prepare_raw_dataset <- function(data,year) {
    
    tic(str_c("prepare raw dataset ",year))
    
    if (year==2016) {
      data <- data %>% 
        rename_ukr(.) %>%
        remove_useless_data(.) %>% 
        rename_toponimics(.,path_toponimics, orig_KOATUU_list) %>% 
        rename_toponimics(.,path_toponimics, school_KOATUU_list) %>% 
        add_KOATUU(.,path_KOATUU_json,KOATUU_ref_file,orig_KOATUU_list) %>%
        add_ATU(.,ATU_compare_file,ATU_codifier_xl,orig_KOATUU_list$col_pref) %>%
        add_hromname(.,orig_KOATUU_list) %>% 
        add_tertypename(.,path_KOATUU_json,KOATUU_ref_file) %>%
        align_regtypename(.) %>%
        add_year(.,year) %>%
        tests_location_consistent(.) %>%
        remove_KOATUU(.) %>% 
        assign_datatypes(.)
      
    } 
    
    else if(year>2016 && year<2021)  {
      
      data <- data %>% 
        rename_ukr(.) %>% 
        remove_useless_data(.) %>% 
        rename_toponimics(.,path_toponimics, orig_KOATUU_list) %>% 
        rename_toponimics(.,path_toponimics, school_KOATUU_list) %>% 
        add_KOATUU(.,path_KOATUU_json,KOATUU_ref_file,orig_KOATUU_list) %>% 
        add_ATU(.,ATU_compare_file,ATU_codifier_xl,orig_KOATUU_list$col_pref) %>%
        add_hromname(.,orig_KOATUU_list) %>%
        align_regtypename(.) %>% 
        align_classprofilename(.) %>% 
        add_year(.,year) %>%
        tests_location_consistent(.) %>% 
        remove_KOATUU(.) %>% 
        assign_datatypes(.)
      
   }
    
    else if(year==2021) {
    
      data <- data %>% 
        remove_useless_data(.) %>% 
        rename_toponimics(.,path_toponimics, orig_KOATUU_list) %>% 
        rename_toponimics(.,path_toponimics, school_KOATUU_list) %>% 
        add_KOATUU(.,path_KOATUU_json,KOATUU_ref_file,orig_KOATUU_list) %>% 
        add_ATU(.,ATU_compare_file,ATU_codifier_xl,orig_KOATUU_list$col_pref) %>%
        add_hromname(.,orig_KOATUU_list) %>%
        align_tertypename(.) %>%
        align_regtypename(.) %>% 
        align_classprofilename(.) %>% 
        add_year(.,year) %>%
        tests_location_consistent(.) %>% 
        remove_KOATUU(.) %>% 
        assign_datatypes(.)
    
    }
    
    else {
      
      data <- data %>% 
        remove_useless_data(.) %>% 
        align_teststatus(.) %>% 
        rename_toponimics(.,path_toponimics, orig_KOATUU_list) %>% 
        rename_toponimics(.,path_toponimics, school_KOATUU_list) %>% 
        add_ATU_new(.,orig_KOATUU_list) %>% 
        add_hromname(.,orig_KOATUU_list) %>%
        align_tertypename(.) %>%
        align_regtypename(.) %>%
        add_year(.,year) %>%
        tests_location_consistent(.) %>% 
        assign_datatypes(.)
    }
    
    assert_raw_data(data,year)
    
    return(data)
  }
  
  #preparing dataset with tests results
  prepare_tests_dataset <- function(data,year){
    
    tic(str_c("preparing datasets with tests result for ",year))
    
    raw <- data
    
    #preparing colnames to pivoting
    for(i in 1:length(subjects)) {
      
      data <- data %>%
        rename_with(.,.cols = starts_with(subjects[i]),
                    .fn = ~ str_c(stri_sub_replace(.,1,nchar(subjects[i]),
                                                   replacement = ""),subjects[i],sep="_"))
    }

    #pivoting dataset.As a result - test observations dataset
    zno_tests <- data %>%
      relocate(classprofilename,classlangname, .after = tertypename) %>% 
      pivot_longer(-c(year:eoparent),
                   names_to = c(".value","test_code"),
                   names_sep = "_",
                   values_drop_na = TRUE) %>% 
      select(-test_code)
    
    source_df <- raw %>% select(matches("test$"))
    
    assert_tests_data(raw,zno_tests)
    
    toc(log=TRUE)
    
    return(zno_tests)
    
  }
  
  #preparing dataset aggregated by schools
  prepare_schools_dataset <- function(data, year) {
    
    tic(str_c("preparing dataset with results by schools ",year))
    
    if(year<2022) {
      
      #get dataset with schools data
      schools_ready_data <- data %>% filter(!is.na(eoname)) %>% 
        add_KOATUU(.,path_KOATUU_json, KOATUU_ref_file,school_KOATUU_list) %>% 
        add_ATU(.,ATU_compare_file,ATU_codifier_xl,school_KOATUU_list$col_pref) %>%
        add_hromname(.,school_KOATUU_list) %>% 
        prepare_tests_dataset(.,year) %>%
        select(year, eoname,eoparent,eotypename:school_ATU_extra,test:ball) %>% 
        remove_KOATUU(.)
      
    } else {
      
      schools_ready_data <- data %>% filter(!is.na(eoname)) %>% 
        add_ATU_new(.,school_KOATUU_list) %>% 
        add_hromname(.,school_KOATUU_list) %>%
        prepare_tests_dataset(.,year) %>%
        select(year, eoname,eoparent,eotypename:school_ATU_extra,test:ball)
      
    }
    
    #clean NAs and reorder columns
    schools_data <- schools_ready_data %>%
      select(-c(teststatus,ball)) %>%
      mutate(across(.cols = ball100, ~ as.double(.))) %>%
      filter(!is.na(ball100), ball100>0)

    #preparing data ready for 2010-2014
    schools_data <- schools_data %>% group_by(year,eoname,test) %>%
          mutate(
                'range_100-123.5'= sum(ball100>=100 & ball100<=123.5),
                'range_124-135.5'= sum(ball100>=124 & ball100<=135.5),
                'range_136-150'= sum(ball100>=136 & ball100<=150),
                'range_150.5-161.5'= sum(ball100>=150.5 & ball100<=161.5),
                'range_162-172.5'= sum(ball100>=162 & ball100<=172.5),
                'range_173-183'= sum(ball100>=173 & ball100<=183),
                'range_183.5-190'= sum(ball100>=183.5 & ball100<=190),
                'range_190.5-195'= sum(ball100>=190.5 & ball100<=195),
                'range_195.5-199.5'= sum(ball100>=195.5 & ball100<=199.5),
                'range_200'= sum(ball100==200)) %>%
      ungroup() %>%
      select(-ball100) %>%
      distinct(year,eoname,test,.keep_all = TRUE)

    assert_schools_data(schools_data,year)

    toc(log=TRUE)

    return(schools_data)
    
  }
  
  #write dataset to single file
  write_to_file <- function(data,path,year) {
    
    tic(str_c("write dataset of ",year," year to file"))
    
    if(file.exists(path)){
      
      rw <- fread(path,na.strings = c("",NA)) %>% assign_datatypes(.)
      
      if(!any(rw$year==year)) {
        
        data <- data %>% bind_rows(.,rw)
        
        fwrite(data,path,sep=";",quote = FALSE)
        
      } else {print(str_c("data for ",year," alredy exist"))}
      
    } else {
      
      fwrite(data,path,sep=";",quote = FALSE,append = TRUE, col.names = TRUE)
    }
    
    toc(log=TRUE)
    
  }
  
  #TODO [refactoring] bind school_id to test dataset
  bind_school_id <- function(schools, tests,y){
    
    tests <- tests %>% filter(year==y)
    
    print(nrow(tests))
    
    #find schools which are unique by towns from schools dictionary
    school_unique_geo <- schools %>% group_by(region,area,town) %>% 
      mutate(n=n()) %>% filter(n==1) %>% ungroup()
    
    #find schools which are unique within towns from test dataset
    test_unique_geo <- tests %>%  group_by(eoregname,eoareaname,eotername) %>% 
      mutate(n=n()) %>% filter(n==1) %>% ungroup()
    
    #filter schools from dictionary coincident with unique 
    #schools within towns from test dataset
    semi_village_sc <- school_unique_geo %>% 
      semi_join(test_unique_geo,
                by=c("region"="eoregname",
                     "area"="eoareaname","town"="eotername"))
    
    #join test dataset and dictionary unique schools within towns
    village_test_data <- semi_village_sc %>% 
      left_join(tests,
                by=c("region"="eoregname",
                     "area"="eoareaname","town"="eotername")) %>% 
      mutate(school_id=id) %>% 
      select(school_id,year,eoname,test:range_200)
    
    print(c("village test data", nrow(village_test_data)))
    
    id_dup <- village_test_data %>% group_by(eoname) %>%
      filter(n()>1) %>% summarise(n=n())
    
    print(c("villages duplications ",nrow(id_dup)))
    
    #cut joined unique schools within towns from test dataset
    other_test_data <- tests %>% 
      anti_join(village_test_data,by=c("eoname"))
    
    #cut joined schools within towns from schools dictionary
    other_sc_data <- schools %>% 
      anti_join(village_test_data,by=c("id"="school_id"))
    
    #filter coincident schools by name from dictionary
    semi_unique_name_sc <- other_sc_data %>% 
      semi_join(other_test_data,
                by=c("full_name"="eoname"))
    
    #join test dataset and dictionary unique schools by names
    unique_name_test_data <- semi_unique_name_sc %>% 
      left_join(other_test_data,
                by=c("full_name"="eoname")) %>% 
      mutate(school_id=id) %>% 
      rename(eoname=full_name) %>% 
      select(school_id,year,eoname,test:range_200)
    
    print(c("unique name test data ",nrow(unique_name_test_data)))
    
    name_dup <- unique_name_test_data %>% group_by(eoname) %>%
      filter(n()>1) %>% summarise(n=n())
    
    print(c("unique name duplications ",nrow(id_dup)))
    
    #cut joned schools by names from test dataset
    rest_test_data <- other_test_data %>% 
      anti_join(unique_name_test_data,by=c("eoname"))
    
    #cut joined schools by names from schools dictionary
    rest_sc_data <- other_sc_data %>% 
      anti_join(unique_name_test_data, by=c("id"="school_id"))
    
    #combine unique names and unique within town
    final_dataset <- bind_rows(village_test_data,unique_name_test_data)
    
    print(c("final dataset ",nrow(final_dataset)))
    
    name_dup <- final_dataset %>% group_by(eoname) %>%
      filter(n()>1) %>% summarise(n=n())
    
    print(c("final dataset name's duplications ",nrow(name_dup)))
    
    dist <- 1
    
    while(dist<12)
      {
      
      print(c("dist= ",dist))
      
      i_join <- rest_sc_data %>% 
        stringdist_inner_join(rest_test_data,
                              by=c("full_name"="eoname"),
                              method="lcs",max_dist=dist, 
                              distance_col="diff") %>% 
        mutate(school_id=id) %>% 
        select(school_id,year:eoname,test:range_200)
      
      print(c("i_join ",nrow(i_join)))
      
      name_dup <- i_join %>% group_by(eoname) %>%
        filter(n()>1) %>% summarise(n=n())
      
      print(c("i_join duplications before ",nrow(name_dup)))
      
      i_join <- i_join %>% anti_join(name_dup,by=c("eoname"))
      
      name_dup <- i_join %>% group_by(eoname) %>%
        filter(n()>1) %>% summarise(n=n())
      
      print(c("i_join duplications after ",nrow(id_dup)))
      
      #cut joned schools by names from test dataset
      rest_test_data <- rest_test_data %>% 
        anti_join(i_join,by=c("eoname"))
      
      print(c("rest test data ",nrow(rest_test_data)))
      
      #cut joined schools by names from schools dictionary
      rest_sc_data <- rest_sc_data %>% 
        anti_join(i_join, by=c("id"="school_id"))
      
      print(c("rest school data ",nrow(rest_sc_data)))
      
      final_dataset <- final_dataset %>% 
        bind_rows(i_join)
      
      print(c("final dataset ",nrow(final_dataset)))
      
      name_dup <- final_dataset %>% group_by(eoname) %>%
        filter(n()>1) %>% summarise(n=n())
      
      print(c("final dataset duplications ",nrow(name_dup)))
      
      dist <- dist+1
      
    }
    
    id_dup <- final_dataset %>% group_by(school_id) %>%
      filter(n()>1) %>% summarise(n=n())
    
    final_dataset <- final_dataset %>% 
      anti_join(id_dup, by=c("school_id")) %>% 
      select(school_id,eoname)
    
    return(final_dataset)
    
  }
  
}

#prepare raw dataset 2016+
{
  #just add code below for the newest year
  
    
  #preparing datasets for 2016-2022
  read_data(path_2016) %>%
    prepare_raw_dataset(.,2016) %>% 
    write_to_file(.,path_zno_raw,2016)
  
  read_data(path_2017) %>% 
    select(-stid) %>%
    prepare_raw_dataset(.,2017) %>% 
    write_to_file(.,path_zno_raw,2017)
  
  read_data(path_2018) %>% 
    prepare_raw_dataset(.,2018) %>% 
    write_to_file(.,path_zno_raw,2018)
  
  read_data(path_2019) %>% 
    select(-ukradaptscale) %>% 
    prepare_raw_dataset(.,2019) %>% 
    write_to_file(.,path_zno_raw,2019)
  
  read_data(path_2020) %>% 
    select(-ukradaptscale) %>% 
    prepare_raw_dataset(.,2020) %>% 
    write_to_file(.,path_zno_raw,2020)
  
  read_data(path_2021) %>% 
    select(-c(umladaptscale,ukradaptscale,ukrsubtest)) %>% 
    rename_with(.cols =  matches("(?:thst)"), 
                .fn = ~ str_replace(.x,"mathst","mst")) %>% 
    prepare_raw_dataset(.,2021) %>% 
    write_to_file(.,path_zno_raw,2021)
  
  read_data(path_2022) %>% 
    select(-c(test,testdate)) %>% 
    rename(ukrteststatus=teststatus,ukrtest=block1,ukrball100=block1ball100,ukrball=block1ball,
           histtest=block2,histball100=block2ball100,histball=block2ball,
           mathtest=block3,mathball100=block3ball100,mathball=block3ball,
           ukrptregname=ptregname,ukrptareaname=ptareaname,ukrpttername=pttername) %>% 
    mutate(histteststatus=ukrteststatus,mathteststatus=ukrteststatus) %>% 
    relocate(ukrteststatus, .after = ukrtest) %>% 
    relocate(histteststatus, .after=histtest) %>% 
    relocate(mathteststatus, .after = mathtest) %>% 
    prepare_raw_dataset(.,2022) %>% 
    write_to_file(.,path_zno_raw,2022)
  
 }

#prepare dataset by tests and students 2016+
{
  
  #read raw dataset
  zno_raw <- fread(path_zno_raw,na.strings = c("",NA)) %>% assign_datatypes(.)
  
  #observation by test result per student for each year
  zno_raw %>% filter(year==2022) %>% 
    prepare_tests_dataset(.,2022) %>% 
    write_to_file(.,path_zno_tests,2022)
  
  zno_raw %>% filter(year==2021) %>% 
    prepare_tests_dataset(.,2021) %>% 
    write_to_file(.,path_zno_tests,2021)
   
  zno_raw %>% filter(year==2020) %>%  
    prepare_tests_dataset(.,2020) %>% 
    write_to_file(.,path_zno_tests,2020)

  zno_raw %>% filter(year==2019) %>%  
    prepare_tests_dataset(.,2019) %>% 
    write_to_file(.,path_zno_tests,2019)
  
  zno_raw %>% filter(year==2018) %>%  
    prepare_tests_dataset(.,2018) %>% 
    write_to_file(.,path_zno_tests,2018)
  
  zno_raw %>% filter(year==2017) %>%  
    prepare_tests_dataset(.,2017) %>% 
    write_to_file(.,path_zno_tests,2017)
  
  zno_raw %>% filter(year==2016) %>% 
    prepare_tests_dataset(.,2016) %>% 
    write_to_file(.,path_zno_tests,2016)
  
}

#prepare dataset by schools 2016+
{
  
  #read raw dataset
  zno_raw <- fread(path_zno_raw,na.strings = c("",NA))
  
  zno_raw %>% filter(year==2022) %>% 
    prepare_schools_dataset(.,2022) %>% 
    write_to_file(.,path_zno_schools,2022)
  
  zno_raw %>% filter(year==2021) %>% 
    prepare_schools_dataset(.,2021) %>% 
    write_to_file(.,path_zno_schools,2021)
  
  zno_raw %>% filter(year==2020) %>% 
    prepare_schools_dataset(.,2020) %>% 
    write_to_file(.,path_zno_schools,2020)
  
  zno_raw %>% filter(year==2019) %>% 
    prepare_schools_dataset(.,2019) %>% 
    write_to_file(.,path_zno_schools,2019)
  
  zno_raw %>% filter(year==2018) %>% 
    prepare_schools_dataset(.,2018) %>% 
    write_to_file(.,path_zno_schools,2018)
  
  zno_raw %>% filter(year==2017) %>% 
    prepare_schools_dataset(.,2017) %>% 
    write_to_file(.,path_zno_schools,2017)
  
  zno_raw %>% filter(year==2016) %>% 
    prepare_schools_dataset(.,2016) %>% 
    write_to_file(.,path_zno_schools,2016)
  
}

#bind schools ids and geocodes to dataset
{
  
  #read dataset by schools
  school_dataset <- fread(path_zno_schools, colClasses=c("character"), encoding = "UTF-8") %>% 
    mutate(across(eoname,~str_replace_all(.x,c("„|”"="","`"="'")) %>% 
                          gsub(pattern = "I",replacement = "І",ignore.case = TRUE))) %>% 
    mutate(s_name= str_trim(str_to_lower(eoname))) %>% 
    mutate(across(s_name,~str_replace_all(.x," ",""))) %>% 
    mutate(ATU_crop= str_sub(school_ATU_fourth,1,12)) 
  
   
  insts <- read_full_institutions(institutions_full) %>% 
    mutate(across(full_name,~str_replace_all(.x,c("„|”"="","`"="'")) %>% 
                            gsub(pattern = "I",replacement = "І",ignore.case = TRUE))) %>% 
    mutate(across(.cols = c(full_name:old_name4), 
                  .fns =  ~str_replace_all(str_trim(str_to_lower(.x))," ",""),
                  .names = '{str_c("s",gsub("^.*?_","_",.col))}')) %>% 
    mutate(ATU_crop = str_sub(ATU_code,1,12))
  
  
  school_ds_year <- school_dataset[year==as.numeric(max(year))-0L,] %>% 
    distinct(ATU_crop,s_name, .keep_all = TRUE)
  
  school_ds_year_init <- school_ds_year
  
  keys <- list(key1=c("ATU_crop","s_name"),
               key2=c("ATU_crop","s_name"="s_name0"),
               key3=c("ATU_crop","s_name"="s_name1"),
               key4=c("ATU_crop","s_name"="s_name2"),
               key5=c("ATU_crop","s_name"="s_name3"),
               key6=c("ATU_crop","s_name"="s_name4"))
  
  
  data <- data.frame()
  
  for(i in 1:length(keys)) {
    
    data_tmp <- school_ds_year[insts,on=keys[[i]],nomatch=0]
    
    school_ds_year <- school_ds_year[!data_tmp,on=.(s_name)]
    
    insts <- insts[!data_tmp,on=.(EDEBO_id)]
    
    data <- bind_rows(data,data_tmp)
    
  }

  school_ds_year_x <- school_ds_year[,n:=n_distinct(s_name),by=school_ATU_fourth][n==1,-"s_name"]
  
  insts_x <- insts[,n:=.N,by=.(ATU_code)][n==1,-"s_name"] 
    
  data_u <- school_ds_year_x[insts_x,on=.(ATU_crop),nomatch=0]
  
  school_ds_year <- school_ds_year[!data_u,on=.(eoname)]
  
  insts <- insts[!data_u,on=.(EDEBO_id)]
  
  data <- bind_rows(data,data_u)
  
  k <- data[duplicated(data$EDEBO_id),] #add assertion
  
  ds <- school_ds_year
  
  ins <- insts
  
  while (nrow(data) < nrow(school_ds_year_init)) {
  
    d <- ds[ins,on=.(ATU_crop),nomatch=0, allow.cartesian=TRUE][,distance:=stringdist(tolower(eoname),tolower(full_name),method = "jw")][,.SD[which.min(distance)],by=.(school_ATU_fourth)]
    
    ds <- ds[!d, on=.(eoname)]
    
    ins <- ins[!d,on=.(EDEBO_id)]
    
    data <- bind_rows(data,d)
    
    print(nrow(school_ds_year_init)-nrow(data))
    
  }
  
  data <- data %>% relocate(full_name, .after=eoname) %>% select(year,EDEBO_id,eoname:range_200)
  
  nn <- fsetdiff(as.data.table(school_ds_year_init),as.data.table(data))
  
  k <- data[duplicated(data$EDEBO_id),]
  l <- data[duplicated(data$eoname),]
  m <- school_ds_year[duplicated(school_ds_year$eoname),]
  nn <- data %>% anti_join(school_ds_year_init,by="eoname")
  
  
  
          
  
  write_csv(tests,"ZNO_16-20_tests_school_ID_data.csv",
            quote_escape = "none")
}
