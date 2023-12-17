source("global.R")

#helpers
{
  #prepare a named list of datasets with ATU
  read_ATU_comparator <- function(comparator_path) {
    
    ATU_compare <- read_excel(comparator_path) %>% 
      rename(ATU_code='Кодифікатор',
             KOATUU_code="Код об'єкта КОАТУУ",
             category="Категорія об’єкта",
             name="Назва об’єкта")
    
    return(ATU_compare)
    
  }
  
  #read schools and universities data from EDEBO website
  read_EDEBO_data <- function(links_list, changed_koatuu_path) {
    
    schools <- as.data.frame(fromJSON(links_list$schools_api)) %>% 
      select(institution_id,institution_name,short_name,governance_name,edrpou,
             institution_type_name,university_financing_type_name,koatuu_id,address) %>% 
      rename(id=institution_id,full_name=institution_name,type=institution_type_name,
             financing_type=university_financing_type_name)
    
    vnz <- as.data.frame(fromJSON(links_list$vnz_api)) 
    
    ptu <- as.data.frame(fromJSON(links_list$ptu_api))
    
    pre_vnz <- as.data.frame(fromJSON(links_list$pre_vnz_api))
    
    univer <- bind_rows(vnz,ptu,pre_vnz) %>% 
      select(university_id,university_name,university_short_name,
             university_governance_type_name, university_edrpou,
             education_type_name,university_financing_type_name,koatuu_id,
             university_address) %>% 
      rename(id=university_id,full_name=university_name,
             short_name=university_short_name,
             governance_name=university_governance_type_name,edrpou=university_edrpou,
             type=education_type_name,financing_type=university_financing_type_name,
             address=university_address)
    
    inst <- bind_rows(schools,univer) %>% remove_extra_symbols(.) %>% 
      distinct(full_name,.keep_all = TRUE)
      
    
    #pattern for clearing address column
    patterns <- c("наб\\.|кімната|майдан|вул\\.|вулиця|ВУЛИЦЯ|квартира|провулок|просп\\.|проспект |пров\\.|буд\\.|пл.|ПЛОЩА|будинок|ал\\.|пр-кт|^б-р|\\*|\\r \\n")
    
    #clear address column
    inst <- inst %>% 
      mutate(across(address, ~ str_replace_all(.,patterns,""))) %>% 
      mutate(across(address, ~ str_replace_all(.,"-"," "))) %>% 
      mutate(across(address, ~ str_trim(.))) %>% 
      mutate(across(address, ~ str_replace_all(.,"^,",""))) %>% 
      mutate(across(address, ~ str_trim(.))) %>% 
      mutate(across(address, ~ str_replace_all(.,"^\\.","")))
    
    #read changed koatuu
    oldest_KOATUU <- fread(changed_koatuu_path,colClasses=c("character"))
    
    #update old koatuu to new ones
    for(i in 1:nrow(oldest_KOATUU)) {
      
      inst <- inst %>% 
        mutate(koatuu_id =if_else(koatuu_id==oldest_KOATUU$old_koatuu[[i]],
                                  oldest_KOATUU$new_koatuu[[i]],koatuu_id))
      
    }
    
    assert_all_are_not_na(inst$koatuu_id)
    
    assert_all_are_equal_to(sum(duplicated(inst$EDEBO_id)),0)
    
    assert_all_are_equal_to(sum(duplicated(inst_full$full_name)),0)
  
    
    return(inst)
    
  }
  
  #get geocodes from Nominatim API using different queries
  get_geocodes_nominatim <- function(data,address_query,output_filter) {
    
    result <- data.frame()
    
    for(i in 1:nrow(data)) {
      
      print(str_c("no:",i," address: ",address_query[i]))
      
      query <-  URLencode(str_c(nomin_url_start,address_query[i],nomin_url_end))
      
      output <- as.data.frame(fromJSON(query))
      
      if(is_empty(output)) { output <- output %>% add_row() }
      
      else {
        
        output <- output %>% 
          filter(type %in% output_filter) %>% slice(1)
        
        if(nrow(output)==0) { output <- output %>% add_row() }
        
      }
      
      temp <- data[i,] %>% mutate(long=output$lon, lat=output$lat, 
                                             add_map=output$display_name)
      
      result <- bind_rows(temp,result)
      
      Sys.sleep(1.2)
      
    }
    
    return(result)
    
  }
  
  #check the institutions dataset whether it's clean
  assert_final_inst_dataset <- function(data) {
    
    # assert NA for required fields
    required_string <- "EDEBO_id|full_name|^type|ATU_first|ATU_second|ATU_third|ATU_fourth|long|lat|occupied"
    assert_all_are_not_na(data %>% select(matches(required_string)))
    
    #check duplicates in EDEBO ids
    assert_all_are_equal_to(sum(duplicated(data$EDEBO_id)),0)
    
    #institutions types numbers
    assert_all_are_equal_to(nlevels(data$status),7)
    
    #occupied or non-occupied territory
    assert_all_are_equal_to(nlevels(data$closed),2)
    
    #check the wrong ATU categories
    assert_all_are_equal_to(nlevels(data$property),4)
    
    assert_all_are_not_na(c(insts$EDEBO_id,insts$inst_name,insts$type, 
                            insts$status,insts$property,insts$ATU_first,
                            insts$ATU_second,insts$ATU_third,
                            insts$ATU_fourth, insts$long,insts$lat,insts$occupied))
    
  }
  
}

#functions
{
  
  #add settlements names to institution data
  add_settlements_to_inst <- function(data, ATU_c_file, ATU_code){
    
    initial_rows <- nrow(data)
    
    ATU <- read_ATU_comparator(ATU_c_file) %>% filter(category!="Н")
    
    data <- data %>% 
      inner_join(ATU, by=c("koatuu_id"="KOATUU_code")) %>% 
      rename(town=name) %>% 
      relocate(c(ATU_code,town),.after = koatuu_id)
    
    assert_all_are_equal_to(initial_rows,nrow(data))
    
    assert_all_are_equal_to(sum(duplicated(data$id)),0)
    
    return(data)
    
  }
  
  #functions for showing schools location on leaflet map
  show_reg_on_map <- function(data) {
    
    leaflet() %>% addTiles() %>% 
      addPolygons(data = oblast,fill = FALSE,weight = 3,color="#444444") %>% 
      addPolygons(data = raions,fill = FALSE,weight = 2,color="#444444") %>% 
      addPolygons(data = hromada,fill = FALSE,weight = 1,color="#000000") %>%
      addCircles(data$long, data$lat,
                          popup = str_c(data$id," ",data$full_name))
  }
  
  #add administrative names to dataset
  add_adm_names <- function(data,ATU_comp_file,ATU_code,ATU_categories) {
    
    initial_rows <- nrow(data)
    
    inst_res <- data_frame()
    
    ATU_compare <- read_ATU_comparator(ATU_comp_file) %>% 
      filter(!category %in% c("С","Х"))
    
    for(i in 1:nrow(data)) {
      
      hrom_name <- ATU_compare %>% 
        filter(grepl(str_sub(data$ATU_code[[i]],1,-11),ATU_code),
               category %in% ATU_categories$hrom) %>% pull(name)
      
      raion_name <- ATU_compare %>% 
        filter(grepl(str_sub(data$ATU_code[[i]],1,-14),ATU_code),
               category %in% ATU_categories$raion) %>% pull(name)
      
      obl_name <- ATU_compare %>% 
        filter(grepl(str_sub(data$ATU_code[[i]],1,-16),ATU_code),
               category %in% ATU_categories$obl) %>% pull(name)
      
      inst_tmp <- data[i,] %>% 
        mutate(hromada =ifelse(length(hrom_name)==0,NA,hrom_name), 
               raion = ifelse(length(raion_name)==0,NA,raion_name),
               oblast = obl_name)
      
      inst_res <- bind_rows(inst_tmp,inst_res)
      
    }  
    
    assert_all_are_equal_to(initial_rows,nrow(inst_res))
    
    assert_all_are_equal_to(sum(duplicated(inst_res$id)),0)
    
    return(inst_res)
    
  }
  
  #update columns to fit institutions dataset
  normalize_occupied_schools <- function(data,ATU_compare_f,special_regions) {
    
    #read ATU-KOATUU comparator
    ATU_compare <- read_ATU_comparator(ATU_compare_f)
    
    #make koatuu_id and ATU_code columns as general
    data <- data %>% 
      mutate(koatuu_id = case_when(
      !is.na(general_KOATUU_fourth) ~ general_KOATUU_fourth,
      is.na(general_KOATUU_third) ~ general_KOATUU_second)) %>% 
      mutate(ATU_code = case_when(
        !is.na(general_ATU_extra) ~ general_ATU_extra,
        is.na(general_ATU_extra) ~ general_ATU_fourth)) %>% 
      mutate(across(general_ATU_second:general_ATU_fourth, ~ 
                      case_when(str_detect(as.character(.x),
                       special_regions$Sevastopol) ~ 'NA',
                                TRUE ~ .x))) %>%
      rename(EDEBO_id=outid, financing_type=ownership,long=longtitude,
             lat=latitude)
    
    #add columns oblast,raion,hromada based on ATU levels
    data <- data %>% 
      inner_join(ATU_compare, by=c("general_ATU_first"="ATU_code")) %>% 
      select(-c(KOATUU_code,category)) %>% 
      rename(oblast=name) %>% 
      left_join(ATU_compare, by=c("general_ATU_second"="ATU_code")) %>% 
      select(-c(KOATUU_code,category)) %>% 
      rename(raion=name) %>% 
      left_join(ATU_compare, by=c("general_ATU_third"="ATU_code")) %>% 
      select(-c(KOATUU_code,category)) %>% 
      rename(hromada=name) %>% 
      inner_join(ATU_compare,by="ATU_code") %>% 
      select(-KOATUU_code) %>% 
      rename(town_district=name) %>% 
      mutate(governance_name='NA',edrpou='NA',add_map='NA') %>% 
      select(EDEBO_id:short_name,governance_name,type,financing_type,edrpou,koatuu_id,ATU_code,category,
             oblast:town_district,address,add_map,long,lat) %>% 
      mutate(occupied=1)
    
    return(data)
    
  }
  
  #check if geodata within administrative boundaries
  check_geodata_within_polygons <- function(data,hromad_map,rayon_map,oblast_map) {
    
    #read hromadas geojson
    hromadas <- sf::st_read(hromad_map, stringsAsFactors = FALSE)
    
    sf::st_make_valid(hromadas)
    
    #read rayon geojson
    rayons <- sf::st_read(rayon_map, stringsAsFactors = FALSE)
    
    sf::st_make_valid(rayons)
    
    #read oblast geojson
    oblasts <- sf::st_read(oblast_map, stringsAsFactors = FALSE)
    
    sf::st_make_valid(oblasts)
    
    #filter hromadas and read geodata
    insts <- data %>% st_as_sf(.,coords = c("long","lat"))
    
    #make CRS for institutions geocodes against hromadas polygons
    st_crs(insts) <- st_crs(hromadas)
    
    
    #compare institutions geocodes against hromadas polygons
    hromadas_output <- st_join(insts,hromadas) 
    
    
    #find if any institutions doesn't fall into proper hromada's boundaries
    hromadas_fail <- hromadas_output %>% filter(ATU_third!=COD_3)
    
    #return(hromadas_fail)
    
    #assert all institutions within proper hromadas
    assert_all_are_equal_to(nrow(hromadas_fail),0)
    
    
    #make CRS for institutions geocodes against rayons polygons
    st_crs(insts) <- st_crs(rayons)
    
    #compare institutions geocodes against hromadas polygons
    rayons_output <- st_join(insts,rayons) 
    
    #find if any institutions doesn't fall into proper rayon's boundaries
    rayons_fail <- rayons_output %>% filter(ATU_second!=COD_2)
    
    #return(rayons_fail)
    
    #assert all institutions within proper rayons
    assert_all_are_equal_to(nrow(rayons_fail),0)
    
    
    #make CRS for institutions geocodes Kyiv,Sevastopol against rayons polygons
    st_crs(insts) <- st_crs(oblasts)
    
    #compare institutions geocodes against hromadas polygons
    oblast_output <- st_join(insts,oblasts) 
    
    #find if any institutions doesn't fall into proper rayon's boundaries
    oblast_fail <- oblast_output %>% filter(ATU_first!=COD_1)
    
    #return(oblast_fail)
    
    #assert all institutions within proper rayons
    assert_all_are_equal_to(nrow(oblast_fail),0)
    
  }
  
}

#prepare institutions dataset for schools in russian occupied territories
{
    #read Crimea dataset
    Crimea <- fread(Crimea_schools,colClasses=c("character")) %>% 
      rename(regname=region,areaname=area,tername=town,outid=id) %>% 
      add_KOATUU(.,path_KOATUU_json,KOATUU_ref_file,gen_KOATUU_list) %>% 
      add_ATU(.,ATU_compare_file,ATU_codifier,gen_KOATUU_list$col_pref) %>% 
      normalize_occupied_schools(.,ATU_compare_file,ATU_special_regions)
    
    Donetsk <- fread(Donetsk_schools, colClasses=c("character")) %>% 
      rename(regname=region,areaname=area,tername=town,outid=id) %>% 
      rename_toponimics(.,path_toponimics,gen_KOATUU_list) %>% 
      add_KOATUU(.,path_KOATUU_json,KOATUU_ref_file,gen_KOATUU_list) %>% 
      add_ATU(.,ATU_compare_file,ATU_codifier,gen_KOATUU_list$col_pref) %>% 
      normalize_occupied_schools(.,ATU_compare_file,ATU_special_regions)
    
    Lugansk <- fread(Lugansk_schools,colClasses=c("character")) %>% 
      rename(regname=region,areaname=area,tername=town,outid=id) %>% 
      rename_toponimics(.,path_toponimics,gen_KOATUU_list) %>% 
      add_KOATUU(.,path_KOATUU_json,KOATUU_ref_file,gen_KOATUU_list) %>% 
      add_ATU(.,ATU_compare_file,ATU_codifier,gen_KOATUU_list$col_pref) %>% 
      normalize_occupied_schools(.,ATU_compare_file,ATU_special_regions)
    
    inst_occupied <- bind_rows(Crimea,Donetsk,Lugansk)
    
    assert_all_are_equal_to(sum(duplicated(inst_occupied$EDEBO_id)),0)
    
    write_csv(inst_occupied,institutions_occupied,quote_escape = "none")
    
  }
  
#combine all institutions data
{
  #TODO closed column
  #inst_closed <- read_csv(inst_closed, col_types = cols(.default = "c"))
  
  #read new institutions
  #inst_new <- read_csv(inst_new, col_types = cols(.default = "c"))
  
  #read cleaned institutions dataset
  insts_ <- fread(institutions_full, colClasses=c("character")) %>% 
    select(-(old_name0:old_name4))
    
  #bind occupied and active schools
  # inst_full <- bind_rows(inst_closed,insts) %>% 
  #     mutate(across(full_name, 
  #                   ~str_replace_all(.x,c("I"="І","„|”"="","`"="'"))))
  # 
  inst_name <- fread(inst_names, colClasses=c("character"))
  
  ins <- insts_ %>% left_join(inst_name,by="EDEBO_id") %>% 
    rename(old_name0=name) %>% 
    relocate(old_name0:old_name4,.after = full_name)
    
  #assert uniqueness of values
  assert_all_are_equal_to(sum(duplicated(ins$EDEBO_id)),0)
  assert_all_are_equal_to(sum(duplicated(ins$full_name) & duplicated(ins$ATU_code)),0)
    
    
  #write to institutions full file
  write_csv(ins,institutions_full,quote_escape = "none")
  
}

#actualize institutions and their geodata
{
  
  #prepare dataset from files 
  {
    #read initial datasets from files
    schools <- read.xlsx(path_schools,sheet=1,startRow = 1) %>% 
      mutate(across(everything(),~ gsub("[\"]|«|»|&quot;|“","",.x))) %>% 
      rename(inst_name=1,EDEBO_id=2,short=4,status=5,type=6,property=7,
             KOATUU=8,reg=9,ter=10,address=11,parent=13) %>% 
      mutate(town = str_extract(ter, "\\b([А-ЯІЇЄҐ][а-яіїєґ'’]*[ -]?)+\\b")) %>% 
      select(EDEBO_id,inst_name,short,type,status,property,parent,reg,town,KOATUU,address) %>% 
      mutate(across(c(type,status,property),~ as.factor(.x)))
    
    prevnz <- read.xlsx(path_prevnz,sheet=1,startRow = 1) %>% 
      mutate(across(everything(),~ gsub("[\"]|«|»|&quot;|“","",.x))) %>%
      mutate(across(everything(),~ gsub("&apos;","'",.x))) %>%
      rename(inst_name=1,EDEBO_id=2,short=4,status=21,type=8,property=9,
             ATU=12,reg=14,town=13,address=15,parent=10) %>% 
      mutate(across(reg,~ gsub(" обл."," область",.x))) %>%
      mutate(across(c(reg,town), ~gsub("м. |смт. |с. ","",.x))) %>% 
      select(EDEBO_id,inst_name,short,type,status,property,parent,reg,town,ATU,address) %>% 
      mutate(status = case_when(is.na(status) ~ "працює",
                                !is.na(status) ~ "призупинено")) %>% 
      mutate(across(c(type,status,property),~ as.factor(.x)))
    
    vnz <- read.xlsx(path_vnz,sheet=1,startRow = 1) %>% 
      mutate(across(everything(),~ gsub("[\"]|«|»|&quot;|“","",.x))) %>%
      mutate(across(everything(),~ gsub("&apos;","'",.x))) %>%
      rename(inst_name=1,EDEBO_id=2,short=4,status=21,type=8,property=9,
             ATU=12,reg=14,town=13,address=15,parent=10) %>% 
      mutate(across(reg,~ gsub(" обл."," область",.x))) %>%
      mutate(across(c(reg,town), ~gsub("м. |смт. |с. ","",.x))) %>% 
      select(EDEBO_id,inst_name,short,type,status,property,parent,reg,town,ATU,address) %>% 
      mutate(status = case_when(is.na(status) ~ "працює",
                                !is.na(status) ~ "призупинено")) %>% 
      mutate(across(c(type,status,property),~ as.factor(.x)))
    
    colleges <- read.xlsx(path_colleges,sheet=1,startRow = 1) %>% 
      mutate(across(everything(),~ gsub("[\"]|«|»|&quot;|“","",.x))) %>%
      mutate(across(everything(),~ gsub("&apos;","'",.x))) %>%
      rename(inst_name=1,EDEBO_id=2,short=4,status=21,type=8,property=9,
             ATU=12,reg=14,town=13,address=15,parent=10) %>% 
      mutate(across(reg,~ gsub(" обл."," область",.x))) %>%
      mutate(across(c(reg,town), ~gsub("м. |смт. |с. ","",.x))) %>% 
      select(EDEBO_id,inst_name,short,type,status,property,parent,reg,town,ATU,address) %>% 
      mutate(status = case_when(is.na(status) ~ "працює",
                                !is.na(status) ~ "призупинено")) %>% 
      mutate(across(c(type,status,property),~ as.factor(.x)))
    
    
    #read ATU codifier from file
    ATU <- get_ATU_codifier_list(csv_ATU_codifier)
    
    #add ATU to colleges dataset
    colleges_dist <- colleges %>% inner_join(ATU$district, by=c("ATU"="ATU_extra")) %>% 
      rename(town_district=name) %>% 
      mutate(ATU_extra=ATU) %>% 
      select(-ATU)
    
    colleges_towns <- colleges %>% anti_join(colleges_dist,by="EDEBO_id") %>% 
      inner_join(ATU$settlement,by=c("ATU"="ATU_fourth")) %>% 
      rename(town_district=name) %>% 
      mutate(ATU_fourth=ATU) %>% 
      select(-ATU)
    
    colleges_full <- bind_rows(colleges_dist,colleges_towns)
    
    #adjust source file until it's 0
    assert_all_are_equal_to(nrow(colleges %>% 
                                   anti_join(colleges_full,by="EDEBO_id") %>%
                                   group_by(ATU) %>% tally()),0)
    
    
    #add ATU to vnz dataset
    vnz_dist <- vnz %>% inner_join(ATU$district, by=c("ATU"="ATU_extra")) %>% 
      rename(town_district=name) %>% 
      mutate(ATU_extra=ATU) %>% 
      select(-ATU)
    
    vnz_towns <- vnz %>% anti_join(vnz_dist,by="EDEBO_id") %>% 
      inner_join(ATU$settlement,by=c("ATU"="ATU_fourth")) %>% 
      rename(town_district=name) %>% 
      mutate(ATU_fourth=ATU) %>% 
      select(-ATU)
    
    vnz_full <- bind_rows(vnz_dist,vnz_towns)
    
    #adjust source file until it's 0
    assert_all_are_equal_to(nrow(vnz %>% 
                                   anti_join(vnz_full,by="EDEBO_id") %>% 
                                   group_by(ATU) %>% tally()),0)
    
    
    #add ATU to pre_vnz dataset
    prevnz_dist <- prevnz %>% inner_join(ATU$district, by=c("ATU"="ATU_extra")) %>% 
      rename(town_district=name) %>% 
      mutate(ATU_extra=ATU) %>% 
      select(-ATU)
    
    prevnz_towns <- prevnz %>% anti_join(prevnz_dist,by="EDEBO_id") %>% 
      inner_join(ATU$settlement,by=c("ATU"="ATU_fourth")) %>% 
      rename(town_district=name) %>% 
      mutate(ATU_fourth=ATU) %>% 
      select(-ATU)
    
    prevnz_full <- bind_rows(prevnz_dist,prevnz_towns)
    
    #adjust source file until it's 0
    assert_all_are_equal_to(nrow(prevnz %>% 
                                   anti_join(prevnz_full,by="EDEBO_id") %>% 
                                   group_by(ATU) %>% tally()),0)
    
    
    #prepare ATU to schools
    insts_wo_schools <- bind_rows(colleges_full,vnz_full,prevnz_full)
    
    ATU_compare <- read_excel(ATU_compare_file) %>% 
      rename(ATU_code='Кодифікатор',
             KOATUU_code="Код об'єкта КОАТУУ",
             category="Категорія об’єкта",
             name="Назва об’єкта") %>% 
      filter(category!="Н") %>% 
      mutate(across(name,~ gsub("&apos;|’","'",.x)))
    
    sch <- schools %>% inner_join(ATU_compare, by=c("KOATUU"="KOATUU_code"))
    
    l <- schools %>% anti_join(sch,by="EDEBO_id")
    
    sch2 <- l %>% mutate(join_key = substr(KOATUU, 1, 5)) %>%
      inner_join(ATU_compare %>% mutate(join_key = substr(KOATUU_code, 1, 5)), 
                 by = c("join_key","town"="name"))
    
    #adjust source file until it's 0
    assert_all_are_equal_to(nrow(sch2[duplicated(sch2$EDEBO_id),]),0)
    
    sch3 <- l %>% anti_join(sch2,by="EDEBO_id") %>% 
      mutate(join_key = substr(KOATUU, 1, 2)) %>%
      inner_join(ATU_compare %>% mutate(join_key = substr(KOATUU_code, 1, 2)), 
                 by = c("join_key","town"="name"))
    
    #adjust source file until it's 0
    assert_all_are_equal_to(nrow(sch3[duplicated(sch3$EDEBO_id),]),0)
    
    schools_atu <- bind_rows(sch,sch2,sch3) %>% 
      mutate(ATU_extra=if_else(category=="В",ATU_code,'NA'),
             ATU_fourth=if_else(category!="В",ATU_code,'NA')) %>% 
      select(-c(KOATUU,KOATUU_code,join_key,category,ATU_code)) %>% 
      rename(town_district=name)
    
    #adjust source file until it's 0
    assert_all_are_equal_to(nrow(schools %>% anti_join(schools_atu,by="EDEBO_id")),0)
    
    schools_dist <- schools_atu %>% inner_join(ATU$district, by="ATU_extra") %>% 
      rename(ATU_fourth=ATU_fourth.y) %>% 
      select(-c(ATU_fourth.x,name))
    
    schools_town <- schools_atu %>% anti_join(schools_dist,by="EDEBO_id") %>% 
      inner_join(ATU$settlement,by="ATU_fourth")
    
    schools_full <- bind_rows(schools_dist,schools_town) %>% 
      mutate(town_district=if_else(is.na(name),town_district,name)) %>% 
      select(-name)
    
    #adjust source file until it's 0
    assert_all_are_equal_to(nrow(schools_atu %>% anti_join(schools_full,by="EDEBO_id")),0)
    
    
    #merge all institutions into one dataset
    insts <- bind_rows(prevnz_full,schools_full,vnz_full,colleges_full) %>% 
      left_join(ATU$hromada %>% select(ATU_third,name),by="ATU_third") %>% 
      left_join(ATU$adm,by=c("ATU_third"="ATU_first")) %>% 
      mutate(hromada=if_else(is.na(name.x),name.y,name.x)) %>% 
      select(-c(name.x,name.y)) %>% 
      left_join(ATU$rajon %>% select(ATU_second,name),by="ATU_second") %>% 
      left_join(ATU$adm,by=c("ATU_second"="ATU_first")) %>% 
      mutate(rajon=if_else(is.na(name.x),name.y,name.x)) %>% 
      select(-c(name.x,name.y)) %>% 
      left_join(ATU$oblast,by="ATU_first") %>% 
      left_join(ATU$adm,by="ATU_first") %>% 
      mutate(oblast=if_else(is.na(name.x),name.y,name.x)) %>% 
      select(-c(name.x,name.y)) %>% 
      select(EDEBO_id:parent,oblast,rajon,hromada,town_district,address,
             ATU_first:ATU_fourth,ATU_extra)
    
    #read old inst dataset with geodata
    insts_old <- read_full_institutions(institutions_full) %>% 
      mutate(across(full_name,~str_replace_all(.x,c("„|”"="","`"="'")) %>% 
                      gsub(pattern = "I",replacement = "І",ignore.case = TRUE))) %>% 
      mutate(across(.cols = c(full_name:old_name4), 
                    .fns =  ~str_replace_all(str_trim(str_to_lower(.x))," ",""),
                    .names = '{str_c("s",gsub("^.*?_","_",.col))}')) %>% 
      mutate(ATU_crop = str_sub(ATU_code,1,12))
  }
  #find difference between old and new inst datasets
  delta <- insts %>% inner_join(insts_old,by="EDEBO_id") %>% 
    select(-old_name4) %>% 
    rename(type=type.x,oblast=oblast.x,hromada=hromada.x,town_district=town_district.x,
           address=address.x,old_name0=full_name,old_name1=old_name0,
           old_name2=old_name1,old_name3=old_name2,old_name4=old_name3) %>% 
    select(EDEBO_id:short,old_name0:old_name4,type:ATU_extra,long:occupied)
  
  #dealt with data to be validated
  {
    #select rows with geodata which are not present in the new dataset
    inst_to_check_names <- insts_old %>% anti_join(delta,by="EDEBO_id")
    
    inst_to_check_names_occup <- inst_to_check_names %>% filter(occupied==1) %>% 
      mutate(u_status="тимчасово окупована",
             u_date=case_when(
               str_detect(oblast,"Севастополь|Автономна Республіка Крим") ~ as.Date("20.02.2014", 
                                                                                    format = "%d.%m.%Y"),
               str_detect(oblast,"Донецька") ~ as.Date("7.04.2014",format = "%d.%m.%Y"),
               str_detect(oblast,"Луганська") ~ as.Date("29.04.2014",format = "%d.%m.%Y")
             ))
    
    
    inst_to_check_names_free <- inst_to_check_names %>% filter(occupied==0)
    
    inst_to_check_names_univ <- inst_to_check_names_free[grepl("^\\d{2,4}$",
                                                               inst_to_check_names_free$EDEBO_id)]
    
    inst_to_check_names_univ$u_status <- NA
    inst_to_check_names_univ$u_date <- NA
    
    for(i in 1:nrow(inst_to_check_names_univ)) {
      
      id <- inst_to_check_names_univ$EDEBO_id[i]
      
      url <- paste0("https://registry.edbo.gov.ua/university/",id,"/")
      
      univ_status <- read_html(url) %>% 
        html_nodes(".close-type>div") %>% 
        html_text()
      
      if (length(univ_status) > 0) {
        inst_to_check_names_univ$u_status[i] <- univ_status
      }
      
      blocked_date <- read_html(url) %>% 
        html_nodes(".close-date>div") %>% 
        html_text()
      
      if (length(blocked_date) > 0) {
        
        blocked_date <- as.Date(blocked_date, format = "%d.%m.%Y")
        
        inst_to_check_names_univ$u_date[i] <- blocked_date
      }
      
    }
    
    inst_to_check_names_univ$u_date <- as.Date(inst_to_check_names_univ$u_date,
                                               origin = "1970-01-01")
    
    inst_to_check_names_univ2 <- inst_to_check_names_univ %>% 
      filter(is.na(u_status))
    
    for(i in 1:nrow(inst_to_check_names_univ2)) {
      
      id <- inst_to_check_names_univ2$EDEBO_id[i]
      
      url <- paste0("https://vstup.osvita.ua/r1/",id,"/")
      
      page <- tryCatch(
        {
          read_html(url)
        },
        error = function(e) {
          warning("Error: The webpage could not be found (HTTP 404). Skipping...")
          NULL  # Return NULL to indicate the failure
        }
      )
      
      if (!is.null(page)) {
        
        univ_status2 <- page %>% 
          html_nodes("table.pro-vnz-table") %>% 
          html_table() %>% as.data.frame()
        
        inst_to_check_names_univ2$u_status[i] <- univ_status2[[3,2]]
      }
      
    }
    
    inst_to_check_names_univ_done <- bind_rows(inst_to_check_names_univ %>% 
                                                 filter(!is.na(u_status)),
                                               inst_to_check_names_univ2)
    
    inst_to_check_names_schools <- inst_to_check_names_free[grepl("^\\d{5,6}$",
                                                                  inst_to_check_names_free$EDEBO_id)]
    inst_to_check_names_schools$u_status <- NA
    inst_to_check_names_schools$u_date <- NA
    
    for(i in 1:nrow(inst_to_check_names_schools)) {
      
      id <- inst_to_check_names_schools$EDEBO_id[i]
      
      url <- paste0("https://registry.edbo.gov.ua/institution/",id,"/")
      
      school_status <- read_html(url) %>% 
        html_nodes("#institution-info>div:nth-child(4)>div") %>% 
        html_text()
      
      if (length(school_status) > 0) {
        inst_to_check_names_schools$u_status[i] <- school_status
      }
      
      blocked_date <- read_html(url) %>% 
        html_nodes("#institution-info>div:nth-child(19)>div") %>% 
        html_text()
      
      if (length(blocked_date) > 0) {
        
        blocked_date <- as.Date(blocked_date, format = "%d.%m.%Y")
        
        inst_to_check_names_schools$u_date[i] <- blocked_date
      }
      
    }
    
    inst_to_check_names_schools$u_date <- as.Date(inst_to_check_names_schools$u_date,
                                                  origin = "1970-01-01")
    
    inst_to_check_names_done <- bind_rows(inst_to_check_names_occup,
                                          inst_to_check_names_schools,
                                          inst_to_check_names_univ_done)
    
    assert_all_are_equal_to(nrow(inst_to_check_names_done),nrow(inst_to_check_names))
    
    reas <- "неукладенням|заборгованістю|заблоковано за заявою закладу|заборгованість|доступу"
    
    inst_to_check_names_done <- inst_to_check_names_done %>% 
      mutate(across(u_status,str_to_lower)) %>% 
      mutate(u_status=case_when(
        str_detect(u_status,reas) ~ "заблоковано",
        str_detect(u_status,"науковий|наукові|заклад|–") ~ "працює",
        str_detect(u_status,"реорганізація") ~ "реорганізовано",
        str_detect(u_status,"перебуває|призупинення") ~ "припинено",
        TRUE ~ u_status
      ))
    
    inst_to_check_names_done$u_status <- factor(inst_to_check_names_done$u_status)
    
    inst_validated_names <- inst_to_check_names_done %>% 
      select(-c(s_name:n,category,add_map,koatuu_id))
  }
  
  
  inst_v_n_d <- inst_validated_names %>% inner_join(ATU$district,
                                                    by=c("ATU_code"="ATU_extra")) %>% 
    mutate(ATU_extra=ATU_code,town_district=name) %>% 
    select(-c(ATU_code,name)) %>% 
    relocate(ATU_first:ATU_extra, .after = address)
  
  inst_v_n_t <- inst_validated_names %>% anti_join(inst_v_n_d,by="EDEBO_id") %>% 
    inner_join(ATU$settlement,by=c("ATU_code"="ATU_fourth")) %>% 
    mutate(ATU_fourth=ATU_code,town_district=name) %>% 
    select(-c(ATU_code,name)) %>% 
    relocate(ATU_first:ATU_fourth, .after = address)
  
  inst_v_n <- bind_rows(inst_v_n_d,inst_v_n_t)
    

  #dealt with items without geodata
  {
    #select rows without geodata which are not present in the old dataset
    inst_wo_geodata <- insts %>% anti_join(delta,by="EDEBO_id")
    
    inst_wo_geodata <- inst_wo_geodata %>% 
      mutate(across(address, ~str_replace(.,"бульвар|бульв. ","бульвар "))) %>% 
      mutate(across(address, ~str_replace(.,"проспект|просп. ","проспект "))) %>% 
      mutate(across(address, ~str_replace(.,"провулок|пров. ","провулок "))) %>% 
      mutate(across(address, ~str_replace(.,"набережна|наб. ","набережна "))) %>% 
      mutate(across(address, ~str_replace(.,"площа|пл. ","площа "))) %>% 
      mutate(addr_query=paste(rajon,hromada,town_district,address,sep = ","))
    
    
    output_filter <- c("school","university","tertiary",
                       "college","yes","residential","industrial",
                       "secondary","ed","primary","house","unclassified",
                       "music school","apartments","kindergarten",
                       "administrative","city","town","village","yes",
                       "residential")
    
    split_data <- split(inst_wo_geodata, rep(1:5, length.out = length(inst_wo_geodata)))
    
    
    inst_add_geodata_1 <- get_geocodes_nominatim(split_data[[5]],
                                                 split_data[[5]]$addr_query,output_filter)
    
    inst_add_geodata_1_1 <- inst_add_geodata_1 %>% filter(!is.na(long))
    
    inst_add_geodata_1_2 <- inst_add_geodata_1 %>% filter(is.na(long))
    
    qry <- inst_add_geodata_1_2 %>% 
      mutate(across(addr_query,~str_remove(.,",[^,]*$")))
    
    inst_add_geodata_1_2 <- get_geocodes_nominatim(inst_add_geodata_1_2,
                                                   qry$addr_query,output_filter) 
    
    inst_add_geodata_1_2 <- inst_add_geodata_1_2 %>% filter(!is.na(long))
    
    inst_add_geodata_1_3 <- inst_add_geodata_1 %>% 
      anti_join(inst_add_geodata_1_1, by="EDEBO_id") %>% 
      anti_join(inst_add_geodata_1_2,by="EDEBO_id")
    
    qry <- inst_add_geodata_1_3 %>% 
      mutate(across(addr_query,~ paste(rajon,hromada,town_district,sep = ",")))
    
    inst_add_geodata_1_3 <- get_geocodes_nominatim(inst_add_geodata_1_3,
                                                   qry$addr_query,output_filter)
    
    inst_add_geodata_to_write <- bind_rows(inst_add_geodata_1_1,
                                           inst_add_geodata_1_2,inst_add_geodata_1_3)
    
    
    fwrite(inst_add_geodata_to_write,
           "G:/Other computers/My Laptop/R/ZNO/ZNO/resources/institutions/institutions_new_geodata_5.csv",
           sep=";",quote = FALSE)
    
    inst_geo_1 <- fread("G:/Other computers/My Laptop/R/ZNO/ZNO/resources/institutions/institutions_new_geodata_1.csv",
                        na.strings = c("",NA))
    inst_geo_2 <- fread("G:/Other computers/My Laptop/R/ZNO/ZNO/resources/institutions/institutions_new_geodata_2.csv",
                        na.strings = c("",NA))
    inst_geo_3 <- fread("G:/Other computers/My Laptop/R/ZNO/ZNO/resources/institutions/institutions_new_geodata_3.csv",
                        na.strings = c("",NA))
    inst_geo_4 <- fread("G:/Other computers/My Laptop/R/ZNO/ZNO/resources/institutions/institutions_new_geodata_4.csv",
                        na.strings = c("",NA))
    inst_geo_5 <- fread("G:/Other computers/My Laptop/R/ZNO/ZNO/resources/institutions/institutions_new_geodata_5.csv",
                        na.strings = c("",NA))
  
  }
  inst_geo <- bind_rows(inst_geo_1,inst_geo_2,inst_geo_3,inst_geo_4,inst_geo_5)
  
  insts_final <- bind_rows(inst_v_n,inst_geo,delta)
  
  fwrite(insts_final,institutions_dictionary,sep=";",quote = FALSE)
  
}

#check correctness of the final dataset
{
  #read all institutions
  insts <- fread(institutions_dictionary,na.strings = c("",NA)) %>% 
    mutate(across(c(type,status,property,closed),as.factor))
  
  #check if data is clean
  assert_final_inst_dataset(insts)
  
  #check correctness institutions geodata
  check_geodata_within_polygons(insts,path_hromadas,path_rayons,path_oblast)

}



#obsolete 
{
  
  #prepare institutions dataset from edbo.gov.ua for non-occupied schools
  #!!!!!!ALERT:manual preparation
  {
    
    inst <- read_EDEBO_data(inst_links_list, path_old_KOATUU) %>% 
      add_settlements_to_inst(.,ATU_compare_file,ATU_codifier) %>% 
      add_adm_names(.,ATU_compare_file,ATU_codifier,category_list) %>% 
      rename(EDEBO_id=id,town_district=town) %>% 
      mutate(occupied=0) %>% 
      select(EDEBO_id:governance_name,type:financing_type,edrpou,koatuu_id,
             ATU_code,category,oblast,raion,hromada,town_district,address,occupied)
    
    
    #get data for cities
    inst_cities <- inst %>% filter(category %in% c(category_list$district,
                                                   category_list$cities,
                                                   category_list$obl))
    
    #compose query part for search by address/town,hromada
    addr_query <- str_c(inst_cities$address,
                        str_replace_na(inst_cities$hromada,","),
                        str_replace_na(inst_cities$raion,","),
                        inst_cities$oblast,sep = ",")
    
    #filter for nominatim search results for cities
    output_filter_1 <- c("school","university","tertiary",
                         "college","yes","residential","industrial",
                         "secondary","ed","primary","house","unclassified",
                         "music school","apartments","kindergarten")
    
    #filter for nominatim search results for villages and small towns
    output_filter_2 <- c("administrative","city","town","village","yes",
                         "residential")
    
    #run Nominatim search for cities schools first time
    res_cities <- get_geocodes_nominatim(inst_cities,addr_query,output_filter_1)
    
    #filter rows without geocodes
    res_cities_wo_geocodes <- res_cities %>% filter(!is.na(long))
    
    #make address query more narrower and repeat search untill get geocodes for all schools
    res_cities_x <- get_geocodes_nominatim(res_cities_wo_geocodes
                                           ,addr_query,output_filter_1)
    #bind to dataset with filled long and lat
    res_cities <- bind_rows(res_cities,res_cities_x)
    
    
    #repeat the same procedures for villages
    inst_vill <- inst_res %>% filter(!category %in% c(category_list$district,
                                                      category_list$cities,
                                                      category_list$obl))
    
    #run Nominatim search for villages schools first time
    res_vill <- get_geocodes_nominatim(inst_vill,addr_query,output_filter_2)
    
    #filter rows without geocodes
    res_vill_wo_geocodes <- res_vill %>% filter(!is.na(long))
    
    #make address query more narrower and repeat search untill get geocodes for all schools
    res_vill_x <- get_geocodes_nominatim(res_vill_wo_geocodes
                                         ,addr_query,output_filter_2)
    #bind to dataset with filled long and lat
    res_vill <- bind_rows(res_vill,res_vill_x)
    
    #bind cities and villages data together
    inst_geocodes <- bind_rows(res_cities,res_vill)
    
    #make sure that no one school is missing 
    assert_all_are_equal_to(nrow(inst),nrow(inst_geocodes))
    
    #make sure that there are no duplicates in schools ID
    assert_all_are_equal_to(sum(duplicated(inst_geocodes$EDEBO_id)),0)
    
    #write into the separate file that shoulb be corrected manually
    #write_csv("outputs/...",institutions,quote_escape = "none")
    
    #read the occupied schools dataset
    inst_occupied <- fread(institutions_occupied, colClasses=c("character"))
    
    #read the non-occupied schools dataset
    inst <- fread(institutions, colClasses=c("character"))
    
    #bind occupied and active schools
    inst_full <- bind_rows(inst_occupied,inst) %>% 
      mutate(across(full_name, 
                    ~str_replace_all(.x,c("I"="І","„|”"="","`"="'"))))
    
  }
  
  #obsolete resources
  {
    
    #institution type
    inst_types <- c("schools","ptnz","vnz")
    
    #first part of url
    region_url_domain <-"https://vn.isuo.org/"
    
    #KOATUU region codes
    koatuu_codes <- c("0500000000","0700000000","1200000000","1400000000",
                      "1800000000","2100000000","2300000000","2600000000",
                      "8000000000","3200000000","3500000000","4400000000",
                      "4600000000","4800000000","5100000000","5300000000",
                      "5600000000","5900000000","6100000000","6300000000",
                      "6500000000","6800000000","7100000000","7300000000","7400000000")
    
    koatuu <- read_excel("institutions/KOATUU_18052020.xls")
    
  }
  
  #obsolete helpers
  {
    #function of scraping schools ids
    scrape_ids_from_isuo <- function(){
      
      #initialize empty dataframe
      schools_table <- data.frame()
      
      #scrape IDs of schools
      for(i in 1:length(inst_types)) {
        
        for(j in 1:length(koatuu_codes)) {
          
          #compose url of pages with schools lists by regions
          url_tmp <- str_c(region_url_domain,"koatuu/", inst_types[i],
                           "-list/id/",koatuu_codes[j])
          
          #define number of pages with schools lists 
          pages <- read_html(url_tmp) %>% 
            html_nodes('#pagination-digg ul>li') %>% 
            html_text(trim=T)
          
          #get rid of waste in pages count
          if(length(pages)!=0) {
            pages_count <- pages[2:(length(pages)-1)]
            
            print(pages_count)
            
            for(k in 1:length(pages_count)) {
              
              print(str_c(url_tmp,"page",pages_count[k],sep = "/"))
              
              #read table on each page
              t_tmp <- read_html(str_c(url_tmp,"page",pages_count[k],sep = "/")) %>%
                html_table()
              
              #fetch schools IDs into one dataset
              t_tmp <- bind_rows(t_tmp) %>%
                select('№ у системі') %>% rename(id_='№ у системі') %>% 
                mutate(type=inst_types[i], koatuu=koatuu_codes[j]) %>%
                mutate_if(is.numeric, as.character)
              
              schools_table <- bind_rows(schools_table,t_tmp)
              
            }
          }
          
          #read IDs for colleges and universities
          else {
            
            print(url_tmp)  
            
            t_tmp <- read_html(url_tmp) %>%
              html_table()
            
            t_tmp <- bind_rows(t_tmp) %>%
              select('№ у системі') %>% rename(id_='№ у системі') %>% 
              mutate(type=inst_types[i],koatuu=koatuu_codes[j]) %>% 
              mutate_if(is.numeric, as.character)
            
            schools_table <- bind_rows(schools_table,t_tmp)
            
          }
          
        }
        
      }
      
      #remove IDs of inactive schools
      schools_table <- schools_table %>% 
        filter(id_ != "ЗЗСО, які не працюють")
      
      return(schools_table)
      
    }
    
    #function of scraping long and lat from js script on detail pages
    scrape_geocodes <- function(url) {
      
      #read long and lat in string from js script
      sc_geo <- read_html(url) %>% 
        html_nodes('script') %>% 
        .[7] %>%
        html_text() %>%
        stri_split_lines() %>% 
        flatten_chr() %>%
        keep(stri_detect_regex,"^var") %>% 
        str_sub(.,12) %>% str_sub(.,1,-2) %>% 
        fromJSON()
      
      #get geocodes from string
      geocodes <- str_split(sc_geo$geoPoint,",") %>% 
        unlist() %>% as.numeric()
      
    }
    
    #function of creating dataset for schools
    sc_school_det <- function(table,geo) {
      
      #combine data from table and geodata into one dataset
      sc_table <- table %>% 
        select(id='№ у системі:',
               full_name='Повна назва:',
               short_name='Скорочена:',
               koatuu_code='Код КОАТУУ:',
               type='Тип ЗЗСО:',
               ownership='Форма власності:',
               location_type='Тип місцевості:',
               zip='Індекс:',
               address='Поштова адреса:',
               website_url='Сайт(и):',
               portal_url='ЗЗСО на порталі «Нові знання»:') %>%
        mutate(latitude=geo[1], longtitude=geo[2]) %>% 
        as_tibble()
      
    }
    
    #function of creating dataset for colleges and univesities
    sc_ptnz_vnz_det <- function(table,geo) {
      
      #combine data from table and geodata into one dataset
      sc_table <- table %>% 
        select(id='№ у системі:',
               full_name='Повна назва:',
               short_name='Скорочена:',
               koatuu_code='Код КОАТУУ:',
               type='Тип:',
               ownership='Форма власності:',
               zip='Індекс:',
               address='Поштова адреса:',
               website_url='Сайт(и):') %>%
        mutate(latitude=geo[1], longtitude=geo[2]) %>% 
        as_tibble()
      
    }
    
  }
  
  #obsolete functions
  {
    #function of creating schools dataset
    scrape_schools_details <- function(schools_id) {
      
      schools_dataset <- data.frame()
      
      for(i in 1:length(inst_types)) {
        
        in_type_ids <- schools_id %>% filter(type==inst_types[i])
        
        for(j in 1:length(in_type_ids$id_)) {
          
          url <- str_c(region_url_domain,inst_types[i],
                       "/view/id/",in_type_ids$id_[j])
          
          print(str_c(j,"  ",url))
          
          geo <- scrape_geocodes(url)
          
          sc_table <- read_html(url) %>% 
            html_node('.zebra-stripe') %>% 
            html_table(fill=TRUE, trim=TRUE) %>% select(X1, X2)%>% 
            pivot_longer(-X1) %>% 
            pivot_wider(names_from=X1, values_from=value,
                        values_fn = list(value=list))
          
          if(inst_types[i]=="schools") {
            
            schools_tmp <- sc_school_det(sc_table,geo)
            
          }
          
          else {
            
            schools_tmp <- sc_ptnz_vnz_det(sc_table,geo)
            
          }
          
          schools_dataset <- bind_rows(schools_dataset,schools_tmp)
          
        }
        
      }
      
      return(schools_dataset)
      
    }
    
    prepare_reg_dataset <- function(data){
      
      reg <- data$address %>% str_extract(.,"\\d{10}|\\d{9}") %>%
        str_sub(.,1,2) %>% str_c(.,"00000000")
      
      area <- data$address %>% str_extract(.,"\\d{10}|\\d{9}") %>%
        str_sub(.,1,5) %>% str_c(.,"00000")
      
      town <- data$address %>% str_extract(.,"\\d{10}|\\d{9}")
      
      data <- data %>% mutate(reg=reg,area=area,town=town) %>% 
        left_join(koatuu,by=c("reg"="TE")) %>% 
        mutate(reg=NU) %>% select(id:town) %>% 
        left_join(koatuu,by=c("area"="TE")) %>% 
        mutate(area=NU) %>% select(id:town) %>% 
        left_join(koatuu,by=c("town"="TE")) %>% 
        mutate(town_type=str_c(NP,". "),town=NU) %>% 
        select(id:area,town_type,town)
      
      data <- data %>% 
        mutate_at(vars(c(reg,area)),
                  list(~ if_else(is.na(str_extract(.,".*/")),.,
                                 str_extract(.,".*/")))) %>% 
        mutate_at(vars(town),
                  list(~str_replace(.,"/",", ") )) %>% 
        mutate_at(vars(c(reg:town)),
                  list(~str_replace(.,"/",""))) %>% 
        mutate_at(vars(reg:town),str_to_lower) %>% 
        mutate_at(vars(c(reg:area,town)),str_to_sentence)
      
      data$address <- data$address %>%  
        str_replace(.,"\\d{10}|\\d{9}",
                    str_c(data$reg,", ",
                          data$area,", ",
                          str_replace_na(data$town_type,"")," ",
                          data$town))
      
      data <- data %>% select(id:longtitude)
      
      return(data)
    }
    
    #functions of getting geocodes from address
    get_geocodes <- function(dataset) {
      
      register_google("put key here")
      
      for(i in 1:nrow(dataset)) {
        
        if(is.na(dataset$latitude[i])){
          
          print(str_c(i," ",dataset$id[i]," ",dataset$address[i]))
          
          result <- geocode(dataset$address[i], output = "latlona",source = "google")
          
          dataset$latitude[i] <- result$lat
          dataset$longtitude[i] <- result$lon
        }
        
      }
      
      return(dataset)
      
    }
    
    #function of adding schools names from osvita.ua
    add_eonames_osvita_ua <- function(data){
      
      pat_1 <- 'div#description p'
      pat_2 <- 'h1.heading'
      
      for(i in 1:length(data$id)){
        
        tmp <- read_html(data$url[i])
        
        if(length(html_nodes(tmp,pat_1))!=0) {
          
          data$eoname[i] <- tmp %>%
            html_nodes(pat_1) %>% 
            html_text() %>% 
            str_remove_all(.,'["//]')
        }
        
        else {
          data$eoname[i] <- tmp %>%
            html_nodes(pat_2) %>% 
            html_text() %>% 
            str_remove_all(.,'["//]')
        }
        
      }
      
      return(data)
      
    }
    
    #function of processing and cleaning institution data from
    #https://registry.edbo.gov.ua/
    prepare_institutions_dataset <- function(data){
      
      #remove waste symbols from school names
      data <- data %>% 
        mutate_at(vars(full_name,short_name),remove_extra_symbols)
      
      #extract area code from koatuu values
      area <- data$koatuu %>% str_extract(.,"\\d{10}|\\d{9}") %>%
        str_sub(.,1,5) %>% str_c(.,"00000")
      
      #left join dataset with koatuu dictionary
      #substitute area and town codes by their names
      data <- data %>% mutate(area=area) %>% 
        left_join(koatuu,by=c("area"="TE")) %>% 
        mutate(area=NU) 
      
      #cut area name from values
      #replace extra symbols for towm
      data <- data %>% 
        mutate_at(vars(area),
                  list(~ if_else(is.na(str_extract(.,".*/")),.,
                                 str_extract(.,".*/")))) %>% 
        mutate_at(vars(c(area)),
                  list(~str_replace(.,"/",""))) %>% 
        mutate_at(vars(area),str_to_lower) %>% 
        mutate_at(vars(c(area)),str_to_sentence)
      
      #combine address with region,area, town
      data <- data %>%  
        mutate(town=str_replace_na(town)) %>% 
        mutate(address=str_c(region,", ",
                             area,", ",
                             town,", ", add)) %>% 
        select(id:region,area,town,address,latitude:website)
      
      return(data)
      
    }
    
    
  }
  
  
  #scraping from alternative sources 
  {
    #scraping schools IDs from isuo.org
    sc_ids <- scrape_ids_from_isuo()
    
    #scraping and processing by regions
    {
      #Vinnutsya====================================================
      sc_id_vin <- sc_ids %>% filter(koatuu==koatuu_codes[1])
      
      dataset_vin <- scrape_schools_details(sc_id_vin) %>%
        lapply(., remove_extra_symbols) %>% bind_rows() %>% 
        mutate_at(c(4:7),factor) %>% mutate_at(c(1,12:13), as.numeric)
      
      write_csv(dataset_vin,"School_dataset_vin.csv",quote_escape = "none")
      
      d_vin <- read_csv("schools_by_reg/School_dataset_vin.csv")
      
      d_vin <- d_vin %>% prepare_reg_dataset() %>% 
        get_geocodes()
      
      show_reg_on_map(d_vin)
      
      write_csv(d_vin,"School_dataset_vin.csv",quote_escape = "none")
      
      #Volyn====================================================
      
      sc_id_vol <- sc_ids %>% filter(koatuu==koatuu_codes[2]) %>% 
        arrange(as.numeric(id_)) %>% slice(-1)
      
      dataset_vol <- scrape_schools_details(sc_id_vol) %>%
        lapply(., remove_extra_symbols) %>% bind_rows() %>% 
        mutate_at(c(4:7),factor) %>% mutate_at(c(1,12:13), as.numeric)
      
      write_csv(dataset_vol,"School_dataset_vol.csv",quote_escape = "none")
      
      d_vol <- read_csv("School_dataset_vol.csv")
      
      d_vol <- d_vol %>% prepare_reg_dataset() %>% 
        get_geocodes()
      
      show_reg_on_map(d_vol)
      
      write_csv(d_vol,"School_dataset_vol.csv",quote_escape = "none")
      
      
      #Dnipro========================================================
      sc_id_dp <- sc_ids %>% filter(koatuu==koatuu_codes[3]) %>% 
        arrange(as.numeric(id_)) %>% slice(-(1:3))
      
      dataset_dp <- scrape_schools_details(sc_id_dp) %>%
        lapply(., remove_extra_symbols) %>% bind_rows() %>% 
        mutate_at(c(4:7),factor) %>% mutate_at(c(1,12:13), as.numeric)
      
      write_csv(dataset_dp,"School_dataset_dp.csv",quote_escape = "none")
      
      d_dp <- read_csv("School_dataset_dp.csv")
      
      d_dp <- d_dp %>% prepare_reg_dataset() %>% 
        get_geocodes()
      
      show_reg_on_map(d_dp)
      
      write_csv(d_dp,"School_dataset_dp.csv",quote_escape = "none")
      
      
      #Donetsk========================================================
      sc_id_don <- sc_ids %>% filter(koatuu==koatuu_codes[4]) %>% 
        arrange(as.numeric(id_)) %>% slice(-c(1,5))
      
      dataset_don <- scrape_schools_details(sc_id_don) %>%
        lapply(., remove_extra_symbols) %>% bind_rows() %>%
        mutate_at(c(4:7),factor) %>% mutate_at(c(1,12:13), as.numeric)
      
      write_csv(dataset_don,"School_dataset_don.csv",quote_escape = "none")
      
      d_don <- read_csv("School_dataset_don.csv")
      
      d_don <- d_don %>% prepare_reg_dataset() %>% 
        get_geocodes()
      
      show_reg_on_map(d_don)
      
      write_csv(d_don,"School_dataset_don.csv",quote_escape = "none")
      
      #Zhytomyr===========================================================
      
      sc_id_zhy <- sc_ids %>% filter(koatuu==koatuu_codes[5]) %>% 
        arrange(as.numeric(id_))
      
      dataset_zhy <- scrape_schools_details(sc_id_zhy) %>%
        lapply(., remove_extra_symbols) %>% bind_rows() %>%
        mutate_at(c(4:7),factor) %>% mutate_at(c(1,12:13), as.numeric)
      
      write_csv(dataset_zhy,"School_dataset_zhy.csv",quote_escape = "none")
      
      d_zhy <- read_csv("School_dataset_zhy.csv")
      
      d_zhy <- d_zhy %>% prepare_reg_dataset() %>% 
        get_geocodes()
      
      show_reg_on_map(d_zhy)
      
      write_csv(d_zhy,"School_dataset_zhy.csv",quote_escape = "none")
      
      #Zakarpattya=============================================================
      
      sc_id_zk <- sc_ids %>% filter(koatuu==koatuu_codes[6]) %>% 
        arrange(as.numeric(id_))
      
      dataset_zk <- scrape_schools_details(sc_id_zk) %>%
        lapply(., remove_extra_symbols) %>% bind_rows() %>%
        mutate_at(c(4:7),factor) %>% mutate_at(c(1,12:13), as.numeric)
      
      write_csv(dataset_zk,"School_dataset_zk.csv",quote_escape = "none")
      
      d_zk <- read_csv("School_dataset_zk.csv")
      
      d_zk <- d_zk %>% prepare_reg_dataset() %>% 
        get_geocodes()
      
      show_reg_on_map(d_zk)
      
      write_csv(d_zk,"School_dataset_zk.csv",quote_escape = "none")
      
      
      #Zaporizka============================================================
      
      sc_id_zp <- sc_ids %>% filter(koatuu==koatuu_codes[7]) %>% 
        arrange(as.numeric(id_))
      
      dataset_zp <- scrape_schools_details(sc_id_zp) %>%
        lapply(., remove_extra_symbols) %>% bind_rows() %>%
        mutate_at(c(4:7),factor) %>% mutate_at(c(1,12:13), as.numeric)
      
      write_csv(dataset_zp,"School_dataset_zp.csv",quote_escape = "none")
      
      d_zp <- read_csv("School_dataset_zp.csv")
      
      d_zp <- d_zp %>% prepare_reg_dataset() %>% 
        get_geocodes()
      
      show_reg_on_map(d_zp)
      
      write_csv(d_zp,"School_dataset_zp.csv",quote_escape = "none")
      
      #Ivano-Frank================================================================
      
      sc_id_if <- sc_ids %>% filter(koatuu==koatuu_codes[8]) %>% 
        arrange(as.numeric(id_))
      
      dataset_if <- scrape_schools_details(sc_id_if) %>%
        lapply(., remove_extra_symbols) %>% bind_rows() %>%
        mutate_at(c(4:7),factor) %>% mutate_at(c(1,12:13), as.numeric)
      
      write_csv(dataset_if,"School_dataset_if.csv",quote_escape = "none")
      
      d_if <- read_csv("School_dataset_if.csv")
      
      d_if <- d_if %>% prepare_reg_dataset() %>% 
        get_geocodes()
      
      show_reg_on_map(d_if)
      
      write_csv(d_if,"School_dataset_if.csv",quote_escape = "none")
      
      #Kyiv city==================================================================
      
      sc_id_kiv <- sc_ids %>% filter(koatuu==koatuu_codes[9]) %>% 
        arrange(as.numeric(id_))
      
      dataset_kiv <- scrape_schools_details(sc_id_kiv) %>%
        lapply(., remove_extra_symbols) %>% bind_rows() %>%
        mutate_at(c(4:7),factor) %>% mutate_at(c(1,12:13), as.numeric)
      
      write_csv(dataset_kiv,"School_dataset_kiv.csv",quote_escape = "none")
      
      d_kiv <- read_csv("School_dataset_kiv.csv")
      
      d_kiv <- d_kiv %>% prepare_reg_dataset() %>% 
        get_geocodes()
      
      show_reg_on_map(d_kiv)
      
      write_csv(d_kiv,"School_dataset_kiv.csv",quote_escape = "none")
      
      #Kyivska======================================================================
      
      sc_id_kr <- sc_ids %>% filter(koatuu==koatuu_codes[10]) %>% 
        arrange(as.numeric(id_)) %>% slice(-592)
      
      dataset_kr <- scrape_schools_details(sc_id_kr) %>%
        lapply(., remove_extra_symbols) %>% bind_rows() %>%
        mutate_at(c(4:7),factor) %>% mutate_at(c(1,12:13), as.numeric)
      
      write_csv(dataset_kr,"School_dataset_kr.csv",quote_escape = "none")
      
      d_kr <- read_csv("School_dataset_kr.csv")
      
      d_kr <- d_kr %>% prepare_reg_dataset() %>% 
        get_geocodes()
      
      show_reg_on_map(d_kr)
      
      write_csv(d_kr,"School_dataset_kr.csv",quote_escape = "none")
      
      #Kirovogradska===============================================================
      
      sc_id_kir <- sc_ids %>% filter(koatuu==koatuu_codes[11]) %>% 
        arrange(as.numeric(id_))
      
      dataset_kir <- scrape_schools_details(sc_id_kir) %>%
        lapply(., remove_extra_symbols) %>% bind_rows() %>%
        mutate_at(c(4:7),factor) %>% mutate_at(c(1,12:13), as.numeric)
      
      write_csv(dataset_kir,"School_dataset_kir.csv",quote_escape = "none")
      
      d_kir <- read_csv("School_dataset_kir.csv")
      
      d_kir <- d_kir %>% prepare_reg_dataset() %>% 
        get_geocodes()
      
      show_reg_on_map(d_kir)
      
      write_csv(d_kir,"School_dataset_kir.csv",quote_escape = "none")
      
      #Luganska==================================================================
      
      sc_id_lug <- sc_ids %>% filter(koatuu==koatuu_codes[12]) %>% 
        arrange(as.numeric(id_))
      
      dataset_lug <- scrape_schools_details(sc_id_lug) %>%
        lapply(., remove_extra_symbols) %>% bind_rows() %>%
        mutate_at(c(4:7),factor) %>% mutate_at(c(1,12:13), as.numeric)
      
      write_csv(dataset_lug,"School_dataset_lug.csv",quote_escape = "none")
      
      d_lug <- read_csv("School_dataset_lug.csv")
      
      d_lug <- d_lug %>% prepare_reg_dataset() %>% 
        get_geocodes()
      
      show_reg_on_map(d_lug)
      
      write_csv(d_lug,"School_dataset_lug.csv",quote_escape = "none")
      
      #Lvivska=====================================================================
      
      sc_id_lv <- sc_ids %>% filter(koatuu==koatuu_codes[13]) %>% 
        arrange(as.numeric(id_))
      
      dataset_lv <- scrape_schools_details(sc_id_lv) %>%
        lapply(., remove_extra_symbols) %>% bind_rows() %>%
        mutate_at(c(4:7),factor) %>% mutate_at(c(1,12:13), as.numeric)
      
      write_csv(dataset_lv,"School_dataset_lv.csv",quote_escape = "none")
      
      d_lv <- read_csv("School_dataset_lv.csv")
      
      d_lv <- d_lv %>% prepare_reg_dataset() %>% 
        get_geocodes()
      
      show_reg_on_map(d_lv)
      
      write_csv(d_lv,"School_dataset_lv.csv",quote_escape = "none")
      
      #Mykolaivska=================================================================
      
      sc_id_myk <- sc_ids %>% filter(koatuu==koatuu_codes[14]) %>% 
        arrange(as.numeric(id_))
      
      dataset_myk <- scrape_schools_details(sc_id_myk) %>%
        lapply(., remove_extra_symbols) %>% bind_rows() %>%
        mutate_at(c(4:7),factor) %>% mutate_at(c(1,12:13), as.numeric)
      
      write_csv(dataset_myk,"School_dataset_myk.csv",quote_escape = "none")
      
      d_myk <- read_csv("School_dataset_myk.csv")
      
      d_myk <- d_myk %>% prepare_reg_dataset() %>% 
        get_geocodes()
      
      show_reg_on_map(d_myk)
      
      write_csv(d_myk,"School_dataset_myk.csv",quote_escape = "none")
      
      #Odeska===================================================================
      
      sc_id_od <- sc_ids %>% filter(koatuu==koatuu_codes[15]) %>% 
        arrange(as.numeric(id_))
      
      dataset_od <- scrape_schools_details(sc_id_od) %>%
        lapply(., remove_extra_symbols) %>% bind_rows() %>%
        mutate_at(c(4:7),factor) %>% mutate_at(c(1,12:13), as.numeric)
      
      write_csv(dataset_od,"School_dataset_od.csv",quote_escape = "none")
      
      d_od <- read_csv("School_dataset_od.csv")
      
      d_od <- d_od %>% prepare_reg_dataset() %>% 
        get_geocodes()
      
      show_reg_on_map(d_od)
      
      write_csv(d_od,"School_dataset_od.csv",quote_escape = "none")
      
      #Poltavska=================================================================
      
      sc_id_po <- sc_ids %>% filter(koatuu==koatuu_codes[16]) %>% 
        arrange(as.numeric(id_))
      
      dataset_po <- scrape_schools_details(sc_id_po) %>%
        lapply(., remove_extra_symbols) %>% bind_rows() %>%
        mutate_at(c(4:7),factor) %>% mutate_at(c(1,12:13), as.numeric)
      
      write_csv(dataset_po,"School_dataset_po.csv",quote_escape = "none")
      
      d_po <- read_csv("School_dataset_po.csv")
      
      d_po <- d_po %>% prepare_reg_dataset() %>% 
        get_geocodes()
      
      show_reg_on_map(d_po)
      
      write_csv(d_po,"School_dataset_po.csv",quote_escape = "none")
      
      #Rivnenska=========================================================================
      
      sc_id_riv <- sc_ids %>% filter(koatuu==koatuu_codes[17]) %>% 
        arrange(as.numeric(id_))
      
      dataset_riv <- scrape_schools_details(sc_id_riv) %>%
        lapply(., remove_extra_symbols) %>% bind_rows() %>%
        mutate_at(c(4:7),factor) %>% mutate_at(c(1,12:13), as.numeric)
      
      write_csv(dataset_riv,"School_dataset_riv.csv",quote_escape = "none")
      
      d_riv <- read_csv("School_dataset_riv.csv")
      
      d_riv <- d_riv %>% prepare_reg_dataset() %>% 
        get_geocodes()
      
      show_reg_on_map(d_riv)
      
      write_csv(d_riv,"School_dataset_riv.csv",quote_escape = "none")
      
      #Sumska==================================================================
      
      sc_id_sum <- sc_ids %>% filter(koatuu==koatuu_codes[18]) %>% 
        arrange(as.numeric(id_)) %>% slice(-519)
      
      dataset_sum <- scrape_schools_details(sc_id_sum) %>%
        lapply(., remove_extra_symbols) %>% bind_rows() %>%
        mutate_at(c(4:7),factor) %>% mutate_at(c(1,12:13), as.numeric)
      
      write_csv(dataset_sum,"School_dataset_sum.csv",quote_escape = "none")
      
      d_sum <- read_csv("School_dataset_sum.csv")
      
      d_sum <- d_sum %>% prepare_reg_dataset() %>% 
        get_geocodes()
      
      show_reg_on_map(d_sum)
      
      write_csv(d_sum,"School_dataset_sum.csv",quote_escape = "none")
      
      #Ternopilska==================================================================
      
      sc_id_ter <- sc_ids %>% filter(koatuu==koatuu_codes[19]) %>% 
        arrange(as.numeric(id_))
      
      dataset_ter <- scrape_schools_details(sc_id_ter) %>%
        lapply(., remove_extra_symbols) %>% bind_rows() %>%
        mutate_at(c(4:7),factor) %>% mutate_at(c(1,12:13), as.numeric)
      
      write_csv(dataset_ter,"School_dataset_ter.csv",quote_escape = "none")
      
      d_ter <- read_csv("School_dataset_ter.csv")
      
      d_ter <- d_ter %>% prepare_reg_dataset() %>% 
        get_geocodes()
      
      show_reg_on_map(d_ter)
      
      write_csv(d_ter,"School_dataset_ter.csv",quote_escape = "none")
      
      #Kharkivska===================================================================
      
      sc_id_kha <- sc_ids %>% filter(koatuu==koatuu_codes[20]) %>% 
        arrange(as.numeric(id_)) %>% slice(-4)
      
      dataset_kha <- scrape_schools_details(sc_id_kha) %>%
        lapply(., remove_extra_symbols) %>% bind_rows() %>%
        mutate_at(c(4:7),factor) %>% mutate_at(c(1,12:13), as.numeric)
      
      write_csv(dataset_kha,"School_dataset_kha.csv",quote_escape = "none")
      
      d_kha <- read_csv("School_dataset_kha.csv")
      
      d_kha <- d_kha %>% prepare_reg_dataset() %>% 
        get_geocodes()
      
      show_reg_on_map(d_kha)
      
      write_csv(d_kha,"School_dataset_kha.csv",quote_escape = "none")
      
      #Khersonska===============================================================
      
      sc_id_khe <- sc_ids %>% filter(koatuu==koatuu_codes[21]) %>% 
        arrange(as.numeric(id_))
      
      dataset_khe <- scrape_schools_details(sc_id_khe) %>%
        lapply(., remove_extra_symbols) %>% bind_rows() %>%
        mutate_at(c(4:7),factor) %>% mutate_at(c(1,12:13), as.numeric)
      
      write_csv(dataset_khe,"School_dataset_khe.csv",quote_escape = "none")
      
      d_khe <- read_csv("School_dataset_khe.csv")
      
      d_khe <- d_khe %>% prepare_reg_dataset() %>% 
        get_geocodes()
      
      show_reg_on_map(d_khe)
      
      write_csv(d_khe,"School_dataset_khe.csv",quote_escape = "none")
      
      #Khmelnitska==============================================================
      
      sc_id_khm <- sc_ids %>% filter(koatuu==koatuu_codes[22]) %>% 
        arrange(as.numeric(id_))
      
      dataset_khm <- scrape_schools_details(sc_id_khm) %>%
        lapply(., remove_extra_symbols) %>% bind_rows() %>%
        mutate_at(c(4:7),factor) %>% mutate_at(c(1,12:13), as.numeric)
      
      write_csv(dataset_khm,"School_dataset_khm.csv",quote_escape = "none")
      
      d_khm <- read_csv("School_dataset_khm.csv")
      
      d_khm <- d_khm %>% prepare_reg_dataset() %>% 
        get_geocodes()
      
      show_reg_on_map(d_khm)
      
      write_csv(d_khm,"School_dataset_khm.csv",quote_escape = "none")
      
      #Cherkaska====================================================================
      
      sc_id_chk <- sc_ids %>% filter(koatuu==koatuu_codes[23]) %>% 
        arrange(as.numeric(id_))
      
      dataset_chk <- scrape_schools_details(sc_id_chk) %>%
        lapply(., remove_extra_symbols) %>% bind_rows() %>%
        mutate_at(c(4:7),factor) %>% mutate_at(c(1,12:13), as.numeric)
      
      write_csv(dataset_chk,"School_dataset_chk.csv",quote_escape = "none")
      
      d_chk <- read_csv("School_dataset_chk.csv")
      
      d_chk <- d_chk %>% prepare_reg_dataset() %>% 
        get_geocodes()
      
      show_reg_on_map(d_chk)
      
      write_csv(d_chk,"School_dataset_chk.csv",quote_escape = "none")
      
      #Chernivetska=============================================================
      
      sc_id_chv <- sc_ids %>% filter(koatuu==koatuu_codes[24]) %>% 
        arrange(as.numeric(id_))
      
      dataset_chv <- scrape_schools_details(sc_id_chv) %>%
        lapply(., remove_extra_symbols) %>% bind_rows() %>%
        mutate_at(c(4:7),factor) %>% mutate_at(c(1,12:13), as.numeric)
      
      write_csv(dataset_chv,"School_dataset_chv.csv",quote_escape = "none")
      
      d_chv <- read_csv("School_dataset_chv.csv")
      
      d_chv <- d_chv %>% prepare_reg_dataset() %>% 
        get_geocodes()
      
      show_reg_on_map(d_chv)
      
      write_csv(d_chv,"School_dataset_chv.csv",quote_escape = "none")
      
      #Chernigivska================================================================
      
      sc_id_chg <- sc_ids %>% filter(koatuu==koatuu_codes[25]) %>% 
        arrange(as.numeric(id_))
      
      dataset_chg <- scrape_schools_details(sc_id_chg) %>%
        lapply(., remove_extra_symbols) %>% bind_rows() %>%
        mutate_at(c(4:7),factor) %>% mutate_at(c(1,12:13), as.numeric)
      
      write_csv(dataset_chg,"School_dataset_chg.csv",quote_escape = "none")
      
      d_chg <- read_csv("School_dataset_chg.csv")
      
      d_chg <- d_chg %>% prepare_reg_dataset() %>% 
        get_geocodes()
      
      show_reg_on_map(d_chg)
      
      write_csv(d_chg,"School_dataset_chg.csv",quote_escape = "none")
      
      #scraping data from osvita.ua==========================
      
      #read prepared file with schools data in JSON format 
      json_f <- fromJSON('json.txt')
      
      #convert JSON data into data frame and extract needed colums
      schools_raw <- rbindlist(json_f) %>% select(id,value,lng,lat,tabtxt,url) %>% 
        mutate(eoname='NA')
      
      #scrape schools fullnames from website
      schools <- add_eonames_osvita_ua(schools_raw)
      
      #write dataset into csv file
      write_csv(schools,"Schools_data_osvita_ua.csv",quote_escape = "none")
      
      #Crimea=================================================================
      crimea_schools <- read_csv("Schools_data.csv")
      
      crimea_schools <- crimea_schools %>% 
        filter(tabtxt %like% "Автономна Республіка Крим|Севастополь") %>% 
        rename(address=tabtxt,
               short_name=value,
               full_name=eoname,
               longtitude=lng,
               latitude=lat,
               portal_url=url) %>% 
        mutate(koatuu_code='0100000000',
               type='NA',
               ownership='NA',
               location_type='NA',
               zip=95000,
               website_url='NA') %>% 
        select(id,full_name,short_name,koatuu_code,
               type, ownership,location_type, zip,
               address,website_url,portal_url,latitude,longtitude)
      
      for(i in 1:length(crimea_schools$portal_url)){
        
        page <- read_html(crimea_schools$portal_url[i]) %>% 
          html_nodes("table.w620") %>% html_text()
        
        if(page != ""){
          
          table <- read_html(crimea_schools$portal_url[i]) %>% 
            html_nodes("table.w620") %>% html_table() %>% bind_rows()
          
          crimea_schools$type[i] <- table$X2[1]
          crimea_schools$ownership[i] <- table$X2[2]
          
        }
        
        if(grepl("с\\.|с-ще|село|селище",crimea_schools$address[i])) {
          
          crimea_schools$location_type[i] <- "сільська"
        }
        else {
          
          crimea_schools$location_type[i] <- "міська"
          
        }
      }
      
      show_reg_on_map(crimea_schools)
      
      write_csv(crimea_schools,"School_dataset_ark.csv",quote_escape = "none")
      
    }
    
    #merge data
    {
      d_set_list <- list.files("schools_by_reg")
      
      schools_data <- data.frame()
      
      for(i in 1:length(d_set_list)) {
        d_tmp <- read_csv(str_c("schools_by_reg/",d_set_list[i]))
        schools_data <- schools_data %>% bind_rows(d_tmp)
      }
      
      schools_data <- schools_data %>% arrange(id)
      
      show_reg_on_map(schools_data) 
      
      schools_data$full_name <- schools_data$full_name %>% 
        remove_extra_symbols()
      
      write_csv(schools_data,"Shools_dataset.csv", quote_escape = "none")
    }
    
    #prepare raw dataset for Crimea
    {
      crimea_schools <- read_csv("institutions/schools_by_reg/School_dataset_ark.csv",
                                 col_types = "dccccccccccdd") %>%
        mutate(addr=address) %>% 
        separate(addr,c("state","region","area","town"),
                 sep = ",") %>% 
        rename(website=website_url,koatuu=koatuu_code) %>% 
        select(id:short_name,type, ownership, koatuu,location_type,
               region:town,address,latitude,
               longtitude,website) %>% 
        mutate_at(vars(region,area,town),str_trim)
      
      write_csv(crimea_schools,"institutions/crimea_dataset.csv", 
                quote_escape = "none")
      
      #some manual clean up needed
    }
  }
  
  
}
