#fewer rows
address_clean_v1<-geo_addresses_v1%>%
  filter(match=="Match")%>%
  distinct()


#Handle unmatched addresses
unmatched_addresses<-geo_addresses_v1%>%
  filter(!match=="Match")%>%
  distinct()%>%
  select(id_add_orig, old.address)

#Single Address Geocoding
if(!exists("geo_addresses_v2")){
  print("Processing single addresses. May take a while.")
  geo_addresses_v2 <- 
  map2(unmatched_addresses$old.address,
       unmatched_addresses$id_add_orig,
       safe_geocode_single_address)
  geo_addresses_v2<-map_dfr(geo_addresses_v2, unlist)
}

#Split Geocoded Data by Match Type
#there are remaining unmatched addresses, 
#duplicate matches, and also some correct matches. 
#The sum of these should be the same as 
#the number of rows in the previous `unmatched_addresses` dataset.

address_clean_v2<-geo_addresses_v2%>%
  mutate(street = str_split_fixed(new.address, ",", n=2)[,1])%>%
  select(id_add_orig, old.address, new.address, 
         street, city, state, zip, 
         lon, lat, tigerLine, side, new.address1)%>%
  filter(!is.na(new.address))%>%
  select(-new.address1)%>%
  mutate(tigerLine = as.numeric(tigerLine),
         zip = as.numeric(zip),
         match.type = "Single")

unmatched_duplicates<-geo_addresses_v2%>%
  filter(!is.na(lon1))%>%
  select(id_add_orig, old.address, ends_with(c("1","2")))

unmatched_addresses_v2<-geo_addresses_v2%>%
  filter(is.na(new.address) & is.na(new.address1))%>%
  select(id_add_orig, old.address)

#remove duplicates - doesn't need to be rerun

if(!exists("geo_address_v3")){
  data<-unmatched_duplicates%>%
    mutate(keep=NA_integer_)
  
  for(i in 1:nrow(data)){
    print(paste("Original Address: ", 
                data$old.address[i], sep=""))
    print(paste("1:", data$new.address1[i], sep=" "))
    print(paste("2:", data$new.address2[i], sep=" "))
    print("3: None of the above")
    k<-menu(c("1", "2", "3"), 
            title="Which matched address to keep?")
    data$keep[i]<-k
  }
  
  geo_address_v3<-data%>%
    pivot_longer(cols = ends_with(c("1", "2")),
                 names_to = c(".value", "addNum"),
                 names_pattern = "(.+)(.)")
  
  address_clean_v3<-geo_address_v3%>%
    filter(addNum==keep)%>%
    select(-addNum, -keep)%>%
    mutate(tigerLine = as.numeric(tigerLine),
           zip = as.numeric(zip),
           match.type = "Single Dup")
  
  unmatched_addresses_v3<-geo_address_v3%>%
    filter(keep==3)%>%
    select(id_add_orig, old.address)%>%
    distinct()
}


if(!exists("address_tofix")){
  address_tofix<-full_join(unmatched_addresses_v2,
                         unmatched_addresses_v3)%>%
  merge_orig_address(.)%>%
    mutate(match.type="Unmatched")

  #choose easy location to find file
  write_csv(address_tofix, "Data/lowell_address_tofix.csv")
}

address_clean<-merge_orig_address(address_clean_v1)%>%
  full_join(.,address_clean_v2)%>%
  full_join(., merge_orig_address(address_clean_v3))%>%
  full_join(., address_tofix)%>%
  full_join(., address_na)


address_clean<-address_clean%>%
  mutate( new.address = case_when(
      is.na(new.address) ~ old.address,
      TRUE ~ new.address))%>%
  group_by(new.address)%>%
  mutate(id_add_clean = paste("CA", cur_group_id(), sep="."))%>%
  ungroup()

