#install packages
{
  # Define the packages
  packages <- c("dplyr", "tidyr", "stringr", "data.table", "readr", "assertive",
                "readxl", "rjson", "stringi", "purrr", "ggmap", "jsonlite",
                "leaflet", "rvest", "xml2", "sf", "tm", "stringdist", "mgsub",
                "tictoc", "openxlsx")

  # Identify the packages that need to be installed
  p_to_inst <- packages[!packages %in% installed.packages()[, "Package"]]

  # Install the missing packages
  if(length(p_to_inst)) install.packages(p_to_inst)

}

#load libraries
{
  # Load the libraries
  lapply(packages, require, character.only = TRUE)
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
  path_2023 <- "resources/ZNO_2010-2020/NMT2023/Odata2023File.csv"
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

  gen_KOATUU_list <- list(reg = "regname", area = "areaname",
                           ter = "tername", col_pref = "general")

  orig_KOATUU_list <- list(reg = "regname", area = "areaname",
                          hrom = "hromname",
                          ter = "tername", col_pref = "orig")

  school_KOATUU_list <- list(reg = "eoregname", area = "eoareaname",
                            hrom = "eohromname",
                             ter = "eotername", col_pref = "school")



  #path to ATU file
  ATU_compare_file <- "resources/KOATUU-ATU/compare_table_KOATUU-ATU.xlsx"

  #path to ATU codifier excel
  ATU_codifier_xl <- "resources/KOATUU-ATU/ATU_codifier.xlsx"

  #path to ATU codifier csv
  csv_ATU_codifier <- "resources/KOATUU-ATU/ATU_codifier.csv"

  category_list <- list(
    district = "B",
    cities = c("M", "T"),
    villages = "C",
    selusche = "X",
    misto = "M",
    smt = "T",
    hrom = "H",
    raion = "P",
    obl = "O",
    adm = "K")

  hromada_dict_path <- "resources/KOATUU-ATU/Hromadas_dictionary.csv"

  #Kyiv and Sevastopol ATU codes
  ATU_special_regions <- list(
    Kyiv = "UA80",
    Sevastopol = "UA85")


  #define vector with subject names
  subjects <- c("uml", "ukr", "hist", "math", "mst", "phys", "chem", "bio",
                "geo", "eng", "fra", "deu", "spa", "rus")

  #links to institutions data API
  inst_links_list <- list(
    schools_api = "https://registry.edbo.gov.ua/api/institutions/?ut=3&lc=0&exp=json",
    vnz_api = "https://registry.edbo.gov.ua/api/universities/?ut=1&lc=0&exp=json",
    ptu_api = "https://registry.edbo.gov.ua/api/universities/?ut=2&lc=0&exp=json",
    pre_vnz_api = "https://registry.edbo.gov.ua/api/universities/?ut=9&lc=0&exp=json")

  #paths to institutions prepared files
  path_schools <- "resources/institutions/Заклади загальної середньої освіти.xlsx"
  path_prevnz <- "resources/institutions/Заклади фахової передвищої освіти.xlsx"
  path_colleges <- "resources/institutions/Заклади професійної (професійно-технічної) освіти.xlsx"
  path_vnz <- "resources/institutions/Заклади вищої освіти.xlsx"

  #links to Nomnatim search API
  nomin_url_start <- "https://nominatim.openstreetmap.org/search?q="
  nomin_url_end <- "&format=jsonv2&addressdetails=1&accept-language=ua&countrycodes=ua&dedupe=0"

  #path to new ATU geojsons
  path_hromadas <- "resources/OSM data/terhromad_1.geojson"
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
  institutions_dictionary <- "resources/institutions/institutions_dictionary.csv"

}

#helpers
{
  #remove extra spaces, new lines and slashes
  remove_extra_symbols <- function(data) {

    tic("removing extra symbols")

    # Define patterns and replacements
    patterns <- c('\\n', '\"', '\u00AB', '\u00BB', '\u201E', '\u201D',
                  '\u0060', '’', '\\/')
    replacements <- c("", "", "", "", "", "", "'", "'", "-")

    # Apply stringi's stri_replace_all_regex function across all columns
    data <- data %>% mutate(across(everything(),
                                   ~stri_replace_all_regex(., patterns,
                                                           replacements,
                                                           vectorize_all = FALSE)))

    #remove leading and trailing spaces
    data <- data %>% mutate(across(everything(), ~ trimws(.)))

    toc(log = TRUE)

    return(data)

  }

  #resolve toponyms duplication within counties (areas)
  toponimic_exceptions <- function(data, cols_targ) {

    tic("resolve toponyms duplications")

    t_exc <- read_csv(path_toponimics_exception,
                      col_types = cols(.default = "c"))

    process_row <- function(reg, area, ter, marker, KOATUU_first, KOATUU_second,
                            KOATUU_third, KOATUU_fourth, new_tername, new_areaname) {
      f_d <- data[get(cols_targ$reg) == reg &
                    get(cols_targ$area) == area &
                    get(cols_targ$ter) == ter &
                    eoname %like% marker] %>%
        mutate(KOATUU_first = KOATUU_first,
               KOATUU_second = KOATUU_second,
               KOATUU_third = KOATUU_third,
               KOATUU_fourth = KOATUU_fourth,
               !!cols_targ$ter := new_tername,
               !!cols_targ$area := new_areaname)
      return(f_d)
    }

    d <- t_exc %>%
      rowwise() %>%
      do(process_row(.$regname,
                     .$areaname,
                     .$tername,
                     .$marker,
                     .$KOATUU_first,
                     .$KOATUU_second,
                     .$KOATUU_third,
                     .$KOATUU_fourth,
                     .$new_tername,
                     .$new_areaname)) %>%
      ungroup()

    d <- d %>% distinct(outid, .keep_all = TRUE)

    data <- data %>% distinct(outid, .keep_all = TRUE)

    data <- data[!as.data.table(d), on = "outid"] %>% bind_rows(., d)

    toc(log = TRUE)

    return(data)

  }

  #rename obsolete toponyms
  rename_toponimics <- function(data, path, cols_list) {

    tic("rename obsolete toponyms")

    # #fixing number of columns for dataset
    colnum <- ncol(data)

    #read file with old-new toponyms mapping
    toponimics <- fread(path, encoding = "UTF-8", na.strings = c("", NA))

    #old-new map for counties(areas)
    topon_areas <- toponimics[is.na(ter_old) & is.na(ter_new) & !is.na(region), ]

    #old-new map for big towns
    topon_towns <- toponimics[!is.na(ter_old) & !is.na(ter_new) & !is.na(area_old) & !is.na(area_new), ]

    #old-new map for villages and small towns
    topon_villages <- toponimics[!is.na(ter_old) & !is.na(ter_new) & is.na(area_old), ]

    #old-new map for settlements subsequent to regional towns
    topon_subsequent <- toponimics[is.na(ter_old) & !is.na(ter_new), ]

    #rename old counties (areas)
    areas <- data[topon_areas, on = c(paste0(cols_list$reg, "==", "region"),
                                   paste0(cols_list$area, "==", "area_old")), nomatch = 0] %>%
      mutate(across(.cols = matches(paste0('^', cols_list$area)), ~ area_new)) %>%
      select(1:all_of(colnum))

    data <- data[!areas, on = "outid"] %>% bind_rows(., areas)

    #rename principal towns and its districts
    town_dist <- data[topon_towns, on = c(paste0(cols_list$reg, "==", "region"),
                                       paste0(cols_list$area, "==", "area_old"),
                                       paste0(cols_list$ter, "==", "ter_old")),
                                       nomatch = 0] %>%
      mutate(across(.cols = matches(paste0('^', cols_list$area)), ~ area_new)) %>%
      mutate(across(.cols = matches(paste0('^', cols_list$ter)), ~ ter_new)) %>%
      select(1:all_of(colnum))

    data <- data[!town_dist, on = "outid"] %>% bind_rows(., town_dist)

    #rename villages and small towns
    villages_dist <- data[topon_villages,
                          on = c(paste0(cols_list$reg, "==", "region"),
                               paste0(cols_list$area, "==", "area_new"),
                               paste0(cols_list$ter, "==", "ter_old")), nomatch = 0] %>%
      mutate(across(.cols = matches(paste0('^', cols_list$ter)), ~ ter_new)) %>%
      select(1:all_of(colnum))

    data <- data[!villages_dist, on = "outid"] %>% bind_rows(., villages_dist)

    #rename subsequent settlments
    subs_dist <- data[topon_subsequent,
                      on = c(paste0(cols_list$reg, "==", "region"),
                           paste0(cols_list$area, "==", "area_old"),
                           paste0(cols_list$ter, "==", "ter_new")), nomatch = 0] %>%
      mutate(across(.cols = matches(paste0('^', cols_list$area)), ~ area_new)) %>%
      select(1:all_of(colnum))

    data <- data[!subs_dist, on = "outid"] %>% bind_rows(., subs_dist)

    toc(log = TRUE)

    return(data)

  }

  #prepare a named list of datasets with KOATUU data
  get_KOATUU_dataset <- function(KOATUU_path, KOATUU_ref) {

    tic("preparing KOATUU dataset")

    #read KOATUU json from internet and rename columns
    KOATUU <- as.data.frame(jsonlite::fromJSON(KOATUU_path)) %>%
      rename(KOATUU_first = `Перший рівень`,
             KOATUU_second = `Другий рівень`,
             KOATUU_third = `Третій рівень`,
             KOATUU_fourth = `Четвертий рівень`,
             category = `Категорія`,
             name = `Назва об'єкта українською мовою`) %>%
      mutate(across(name, ~ str_replace_all(., "\\/.*$|^(М\\.)", ""))) %>%
      mutate(across(name, ~ str_to_title(.))) %>%
      mutate(across(name, ~ gsub(., pattern = "Область", replacement = "область"))) %>%
      mutate(across(name, ~ gsub(., pattern = "Район", replacement = "район"))) %>%
      mutate(category = if_else(KOATUU_fourth == "6320483601", "Щ", category)) %>%
      as.data.table()

    #read local KOATUU refinement file and rename columns
    KOATUU_ref <- fread(KOATUU_ref, encoding = "UTF-8", colClasses = 'character') %>%
      rename(KOATUU_first = `Перший рівень`,
             KOATUU_second = `Другий рівень`,
             KOATUU_third = `Третій рівень`,
             KOATUU_fourth = `Четвертий рівень`,
             category = `Категорія`,
             name = `Назва об'єкта українською мовою`) %>%
      as.data.table()

    #delete duplicates in counties
    KOATUU <- KOATUU[!KOATUU_ref, on = .(KOATUU_third, KOATUU_fourth)]

    toc(log = TRUE)

    return(KOATUU)

  }

  #prepare a named list of datasets with ATU
  get_ATU_data <- function(comparator_path, ATU_file) {

    tic("prepare ATU list")

    #load KOATUU-ATU map file
    #exclude regions, areas, hromadas
    ATU_compare <- read_excel(comparator_path) %>%
      rename(ATU_code = 'Кодифікатор',
             KOATUU_code = "Код об'єкта КОАТУУ",
             category = "Категорія об’єкта",
             name = "Назва об’єкта") %>%
      filter(category != "Н") %>%
      as.data.table()

    ATU_full <- read_excel(ATU_file, guess_max = 10000) %>%
      rename(ATU_first = 'Перший рівень',
             ATU_second = 'Другий рівень',
             ATU_third = 'Третій рівень',
             ATU_fourth = 'Четвертий рівень',
             ATU_extra = 'Додатковий рівень',
             category = "Категорія об’єкта",
             name = "Назва об’єкта") %>%
      as.data.table()

    ATU_list <- list(
      ATU_compare_distr = ATU_compare[!category %in% c("О", "К", "Р") & category == "В"],
      ATU_compare_towns = ATU_compare[!category %in% c("О", "К", "Р", "В")],
      ATU_full = ATU_full,
      ATU_full_towns = ATU_full[!category %in% c("O", "P", "K")],
      ATU_compare = ATU_compare
    )

    toc(log = TRUE)

    return(ATU_list)

  }

  #read ATU codifier file from csv
  get_ATU_codifier_list <- function(codifier_path) {

    tic.clearlog()
    tic("read full ATU codifier")

    ATU_all <- fread(codifier_path, na.strings = "", encoding = "UTF-8") %>%
      setnames(1:5, paste0("ATU_", c("first", "second", "third", "fourth", "extra"))) %>%
      setnames(6:7, c("category", "name")) %>%
      remove_extra_symbols(.) %>%
      distinct(ATU_third, category, name, .keep_all = TRUE)

    ATU <- lapply(list(oblast = category_list$obl, adm = category_list$adm, rajon = category_list$raion,
                       hromada = category_list$hrom, settlement = c(category_list$cities,
                                                                    category_list$villages, category_list$selusche),
                       smt = category_list$smt, misto = category_list$misto,
                       village = category_list$villages, selusche = category_list$selusche,
                       district = category_list$district),
                  function(x) ATU_all[category == x])

    names(ATU) <- c("oblast", "adm", "rajon", "hromada", "settlement", "smt", "misto", "village", "selusche", "district")

    toc(log = TRUE)
    return(ATU)
  }

  #read and clean full list of institutions
  read_full_institutions <- function(inst_full_path) {

    tic("reading and cleaning full list of institutions")

    p <- c("\\(до 2022 р.\\)|\\(до 2021 р.\\)|\\(до 2020 р.\\)", "'",
           "ліцей \\(ліцей-інтернат\\)", "коллегіум")

    rp <- c("", "’", "ліцей-інтернат", "колегіум")

    data <- fread(inst_full_path, encoding = "UTF-8") %>%
      mutate(across(c(lat, long), as.numeric)) %>%
      mutate(across(type, ~str_to_lower(.x) %>%  mgsub(p, rp) %>% str_trim())) %>%
      mutate(across(c(type, category, occupied), as.factor))

    toc(log = TRUE)

    return(data)

  }

}

#functions
{
  #get list of KOATUU levels
  get_KOATUU_levels <- function(KOATUU_path, KOATUU_ref) {

    KOATUU <- get_KOATUU_dataset(KOATUU_path, KOATUU_ref)

    tic("composing list of KOATUU levels")

    # Split the data into subsets based on conditions
    KOATUU_list <- KOATUU %>%
      group_split(.keep = FALSE,
        first_only = KOATUU_second == "",
        first_and_second = KOATUU_second != "" & KOATUU_third == "",
        first_to_third = KOATUU_third != "" & KOATUU_fourth == "" & category != "",
        fourth_no_villages = KOATUU_fourth != "" & category != "С" & category != "C",
        fourth_villages = KOATUU_fourth != "" & category != "Щ" & category != "Т"
      )

    KOATUU_list <- KOATUU_list[- 1] %>%
      setNames(c("fourth_villages", "fourth_no_villages", "first_to_third",
                 "first_and_second", "first"))

    toc(log = TRUE)

    return(KOATUU_list)

  }

  #add columns with KOATUU data
  add_KOATUU <- function(data, path, refinement, targ_cols) {

    #initial <- data

    initial_rows <- nrow(data)
    initial_columns <- ncol(data)

    #get a named list with KOATUU data
    KOATUU_list <- get_KOATUU_levels(path, refinement)

    tic("adding KOATUU columns")

    #add KOATUU first column
    data <- data[as.data.table(KOATUU_list$first),
                 on = paste0(targ_cols$reg, "==", "name")] %>%
      relocate(KOATUU_first, .after = targ_cols$ter) %>%
      select(1:(all_of(initial_columns) + 1))

    #add KOATUU codes to second column for regional towns, 
    #Kyiv and Sevastopol districts
    reg_towns_pr_dist <- data[as.data.table(KOATUU_list$first_and_second),
                              on = c("KOATUU_first",
                                    paste0(targ_cols$ter, "==", "name")),
                nomatch = 0]

    #add KOATUU codes to second column for counties (areas)
    areas <- data[!reg_towns_pr_dist,
                  on = .(outid)][as.data.table(KOATUU_list$first_and_second),
                                 on = c("KOATUU_first", 
                               paste0(targ_cols$area, "==", "name")),
                               nomatch = 0]

    data <- bind_rows(reg_towns_pr_dist, areas) %>%
      relocate(KOATUU_second, .after = KOATUU_first) %>%
      select(1:(all_of(initial_columns) + 2))

    #make subset for 3rd and 4th KOATUU columns
    data_with_other_KOATUU <- data[!as.data.table(KOATUU_list$first_and_second),
                                   on = c("KOATUU_first", paste0(targ_cols$ter, "==", 
                                   "name"))] %>%
      mutate(KOATUU_third = NA, KOATUU_fourth = NA) %>%
      select(1:KOATUU_fourth, everything()) %>%
      as.data.table()

    #KOATUU first and second only subset
    data_with_firs_and_sec_only <- data[!data_with_other_KOATUU, on = "outid"] %>%
      select(1:KOATUU_second, everything())

    #add KOATUU 3rd and 4th columns for small and subsequent towns
    t_and_f_wo_villages <- data_with_other_KOATUU[as.data.table(KOATUU_list$fourth_no_villages),
                            on = c("KOATUU_first", "KOATUU_second",
                            paste0(targ_cols$ter, "==", "name")), nomatch = 0] %>%
      mutate(KOATUU_third = i.KOATUU_third, KOATUU_fourth = i.KOATUU_fourth)

    #add KOATUU 3rd and 4th columns for villages
    t_and_f_villages <- data_with_other_KOATUU[!as.data.table(t_and_f_wo_villages),
                            on = "outid"][as.data.table(KOATUU_list$fourth_villages),
                            on = c("KOATUU_first", "KOATUU_second",
                            paste0(targ_cols$ter, "==", "name")), nomatch = 0] %>%
      mutate(KOATUU_third = i.KOATUU_third, KOATUU_fourth = i.KOATUU_fourth)

    t_and_f <- bind_rows(t_and_f_wo_villages, t_and_f_villages) %>%
      relocate(KOATUU_third, KOATUU_fourth, .after = KOATUU_second)

    #make a subset without villages and subsequent towns
    data_only_third <- data_with_other_KOATUU[!t_and_f, on = "outid"]

    #add KOATUU 3rd column for towns and districts of regionals towns
    third <- data_only_third[as.data.table(KOATUU_list$first_to_third),
                             on = c("KOATUU_first", "KOATUU_second",
                                  paste0(targ_cols$ter, "==", "name")),
                                  nomatch = 0] %>%
      mutate(KOATUU_third = i.KOATUU_third) %>%
      relocate(KOATUU_third, .after = KOATUU_second)

    #combine subsets into full dataset
    data <- bind_rows(data_with_firs_and_sec_only, third, t_and_f) %>%
      relocate(KOATUU_third, KOATUU_fourth, .after = KOATUU_second) %>%
      select(1:(all_of(initial_columns) + 4))

    toc(log = TRUE)

    #correct by exceptions
    if(targ_cols$col_pref != "general") {

      data <- toponimic_exceptions(data, targ_cols)

    }

    tic("finalize KOATUU columns in general dataset")

    #return(data %>% group_by(outid) %>% filter(n()>1))

    data <- data %>%
      rename_with(.cols =  starts_with("KOATUU"),
                  .fn = ~ paste(targ_cols$col_pref, ., sep = "_"))

    #make sure that there are no duplicates in Students IDs
    assert_all_are_equal_to(sum(duplicated(data$outid)), 0)

    #make sure that number of rows hasn't changed
    assert_all_are_equal_to(initial_rows, nrow(data))

    #return(list(initial,data))

    toc(log = TRUE)

    return(data)

  }

  #add colunms with ATU data
  add_ATU <- function(data, comp_path, full_path, cols_pref) {

    initial_rows <- nrow(data)

    ATU_list <- get_ATU_data(comp_path, full_path)

    cols <- paste0(cols_pref, "_", c("KOATUU_fourth", "KOATUU_third", "KOATUU_second", "KOATUU_first"))

    dist_towns <- lapply(cols, function(col) {
      na_rows <- is.na(get(col, data))
      bound_data <- bind_rows(data[na_rows, ][ATU_list$ATU_compare_distr, on = paste0(col, "==", "KOATUU_code"), nomatch = 0],
                              data[na_rows, ][ATU_list$ATU_compare_towns, on = paste0(col, "==", "KOATUU_code"), nomatch = 0]) %>%
        rename_with(.cols = contains("ATU_code"), ~ str_c(cols_pref, "_", "ATU", toupper(substr(col, -1, -1))))
      return(bound_data)
    }) %>%
      bind_rows()

    data <- bind_rows(dist_towns) %>%
      relocate(sprintf("%s_ATU_D", cols_pref),
       sprintf("%s_ATU_F", cols_pref), 
       .after = sprintf("%s_KOATUU_fourth", cols_pref)) %>%
      select(-c(name, category))

    prepared_ATU_distr <- ATU_list$ATU_full_towns %>%
      filter(category == "B")

    prepared_ATU <- ATU_list$ATU_full_towns %>%
      filter(category != "B")

    fcole <- paste0(cols_pref, "_", "ATU_F")
    fcol4 <- paste0(cols_pref, "_", "ATU_D")

    final_data <- lapply(c(fcole, fcol4), function(fcol) {
      na_rows <- complete.cases(data[, get(fcol)])
      bound_data <- bind_rows(data[na_rows, ][prepared_ATU_distr, on = paste0(fcol, "==", "ATU_F"), nomatch = 0] %>%
                                select(-str_c(cols_pref, "_", "ATU_D")) %>%
                                rename_with(.cols = starts_with("ATU_F"), ~ str_c(cols_pref, "_", .)),
                              data[!na_rows, ][prepared_ATU, on = paste0(fcol, "==", "ATU_D"), nomatch = 0] %>%
                                rename_with(.cols = starts_with("ATU_D"), ~ str_c(cols_pref, "_", .))) %>%
        select(-c(category, name), -str_c(cols_pref, "_ATU_F"))
    }) %>%
      bind_rows()

    final_data <- final_data %>%
      rename_with(.cols = starts_with("ATU_"), ~ str_c(cols_pref, "_", .)) %>%
      relocate(str_c(cols_pref, "_", "ATU_first"):str_c(cols_pref, "_", "ATU_third"),
               str_c(cols_pref, "_", "ATU_fourth"),
               .after = str_c(cols_pref, "_", "KOATUU_fourth"))

    if (cols_pref != "school") {
      stopifnot(identical(initial_rows, nrow(final_data)))
    }

    return(final_data)

  }

}
