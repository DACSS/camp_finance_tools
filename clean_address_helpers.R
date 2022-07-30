library(httr)
library(jsonlite)
base_url <- 'https://geocoding.geo.census.gov/geocoder/locations/onelineaddress?'

find_bad_address<-function(data, missing_adds=NULL){
  missing_address<-c("NONE GIVEN", "NO ADDRESS GIVEN", 
                     "NONE PROVIDED", "UNKNOWN", "[X]{2,}")
  missing_address<-c(missing_address, missing_adds)
  
  data%>%
    mutate(bad_add = case_when(
      !is.na(pobox) ~ "bad",
      is.na(street) ~ "bad",
      street %in% missing_address ~ "bad",
      !is.na(zip) ~ "good",
      !is.na(city) & !is.na(state) ~ "good",
      TRUE ~ "bad"))%>%
    filter(bad_add=="bad")%>%
    select(-bad_add)
}

find_apt<-function(x){
  apt_num <-str_extract(x,
                        "[,\\b ](SUITE|APT|UNIT|STE\\b|#)[#. ]*[0-9A-Z -]*")
  apt_num <-case_when(
    is.na(apt_num) & str_detect(x, ",") ~ 
      str_extract(x, "[,]{1}[ ]*[A-Za-z0-9-.]+$"),
    is.na(apt_num) ~ str_extract(x, "[A-Z-.]*[0-9]+[A-Z-.]*$"),
    TRUE ~ apt_num)%>%
    str_remove_all(., "[[:punct:]]")%>%
    str_remove(., "SUITE|APT|UNIT|STE\\b")%>%
    str_squish()
  apt_num
}

find_pobox<-function(x){
  pob <- str_extract(x, 
                     "(POST OFFICE)?[PO .>0]*[ ]*BOX[ ]*[A-Z-]*[0-9]+[A-Z-]*")
  pob <-case_when(
    is.na(pob) ~ 
      str_extract(x, "(POB|BX)[. ]*[A-Z-]*[0-9]+[A-Z-]*"),
    is.na(pob) ~ 
      str_extract(x, "^PO[ ]*[0-9]+[A-Z-]*"),
    TRUE ~ pob)
  pob<-pob%>%
    str_remove_all(., "[[:punct:]]")%>%
    str_remove_all(., "[[>]]")%>%
    str_replace(., "[A-Z0 ]{2,}", "PO BOX ")
  pob
}


prep_address<-function(data, missing_adds=missing_address){
  data %>%
    select(id_add_orig, street_orig, city_orig, state_orig, 
           zip_orig)%>%
    mutate(across(street_orig:zip_orig,
                  ~ replace_na(.x, "")),
           address_single = 
             paste(street_orig, city_orig, state_orig, zip_orig, sep=", "),
           across(street_orig:zip_orig,
                  ~ na_if(.x, "")),
      street = str_to_upper(street_orig),
      street = str_replace(street, "^([0-9]+)([A_Z]{1})", "\\1 \\2"),
      pobox = find_pobox(street),
      street = case_when(
        !is.na(pobox) & str_starts(street, "P|B") ~ pobox,
        TRUE ~ street),
      apt = find_apt(street),
      apt = case_when(
        !is.na(pobox) ~ NA_character_,
        TRUE ~ apt),
      street = str_remove_all(street, "[[:punct:]]"),
      city = str_to_upper(city_orig),
      city = str_remove_all(city, "[[:punct:]]"),
      state = str_to_upper(state_orig),
      state = str_remove_all(state, "[[:punct:]]"),
      zip = parse_number(zip_orig))
}

write_pregeo <- function(data, project = NULL){
  data<-data%>%
    select(id_add_orig,street,city,state,zip)%>%
    distinct()
  
  x <- ceiling(nrow({{data}})/10000)
  data.sub <-split({{data}}, rep(1:x, times = 10000,
                                 length=nrow({{data}})))
  for(i in 1:x){
    filename = paste("geo/Address", "_", project, 
                     "_pt", i, ".csv", sep="" )
    write_csv(data.sub[[i]], filename, col_names=FALSE)
  }
}
if(!dir.exists("geo")) dir.create("geo")

read_geo<-function(file){
  read_csv(file,
           col_types = "ccccccdciiii")%>%
    separate(coordinates, 
             c("lon", "lat"), 
             sep = ",", remove = TRUE)
}

geocode_single_address<-function(old.address, id){
  add_encode<-str_replace_all(old.address, " ", "+")
  add_encode<-str_replace_all(add_encode, ",", "%2C")
  params <- list(address = I(add_encode),
                 format = 'json',
                 benchmark = "2020")
  soup <-GET(base_url, query = params)
  dat<-fromJSON(content(soup,as='text', encoding = "UTF-8"),
                simplifyVector=TRUE)$result
  c(id_add_orig = id,
    old.address=old.address,
    new.address = dat$addressMatches$matchedAddress,
    tigerLine = dat$addressMatches$tigerLine$tigerLineId,
    side = dat$addressMatches$tigerLine$side,
    lon = dat$addressMatches$coordinates$x,
    lat = dat$addressMatches$coordinates$y,
    city = dat$addressMatches$addressComponents$city,
    state = dat$addressMatches$addressComponents$state,
    zip = dat$addressMatches$addressComponents$zip)
}

#found on stack overflow
possibly2 <- function(.f, otherwise=NULL) {
  function(...) {
    tryCatch({
      .f(...)  
    }, error = function(e) otherwise(...))
  }
}

safe_address<-function(old.address, id){
  c(id_add_orig = id,
    old.address=old.address)
}
safe_geocode_single_address<-possibly2(
  geocode_single_address,
  safe_address)

merge_orig_address<-function(data){
  data%>%
    select(-old.address)%>%
    left_join(., ungroup(address_orig))%>%
    distinct()
}

#Geocode addresses using bash
#Create a header in order to make sure that all 12 columns read in
#otherwise a missing address in the first row will mess things up.

geocode_bulk<-function(project=NULL){
  if(!exists("geo_addresses_v1")){
    system("echo 'id_add_orig,old.address,match,match.type,new.address,coordinates,tigerLine,side,state.num,county.num,block.num,district.num' > geo/geocols.txt")
    file_pattern <-str_c("cd geo; ls Address", project, "*", sep="")
    filenames<-system(file_pattern, intern = TRUE)
    for(f in filenames){
      print(paste("Processing", f, sep=" "))
      geo.call <-str_c("cd geo; curl --form addressFile=@", f, 
                       " --form benchmark=4 --form vintage=420 --form layers=0",
                       " https://geocoding.geo.census.gov/geocoder/geographies/addressbatch --output",
                       " Geo", f, sep="")
      system(geo.call, intern=FALSE)
      add.header <-str_c("cd geo; cat geocols.txt Geo", f, 
                         " > temp && mv temp Geo", f, sep="")
      system(add.header, intern=FALSE)
    }
    #Read in geocoded addresses
    geo_filenames <- list.files("geo", 
                            pattern=str_c("GeoAddress", project, "*",sep=""),
                            full.names=TRUE)
    return(geo_filenames)
  }
}
