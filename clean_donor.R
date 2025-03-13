library(igraph)

create_donors_orig <- function(data, var, first_name, last_name, middle_name){
donors_orig <- {{data}} %>%
  ungroup()%>%
  filter({{var}}=="not org")%>%
  select(id_name_orig, id_name_clean, 
         orig_name, clean_name, 
         {{first_name}}, {{last_name}}, 
         id_add_orig, id_add_clean, 
         address_single, new.address, 
         occupation_orig, employer_orig,
         match.type, elect_year,
         apt, {{middle_name}})%>%
  group_by(id_name_orig, id_add_orig, occupation_orig,
           employer_orig)%>%
  mutate(years = paste(unique(elect_year), collapse="; "))%>%
  select(-elect_year)%>%
  distinct()%>%
  ungroup()
}  

#donor key
create_donor_key<-function(data, first_name, last_name){
  data%>%
    filter(match.type!="Missing")%>%
  select(id_name_clean,id_add_clean, {{last_name}}, {{first_name}})%>%
    distinct()%>%
  group_by(id_add_clean)%>%
  mutate(address_key2 = match({{last_name}}, unique({{last_name}})),
         names_same_add = n_distinct({{last_name}}))%>%
  group_by(id_add_clean, address_key2)%>%
  mutate(address_key3 = 
           match(str_sub({{first_name}},1,1), unique(str_sub({{first_name}},1,1))),
         address_key_strict = 
           paste(id_add_clean, address_key2, address_key3, sep="."),
         address_key_loose = case_when(
           names_same_add < 3 ~ paste(id_add_clean),
           TRUE ~ paste(id_add_clean, address_key2, sep=".")))%>%
  ungroup()
}

find_donors<-function(data){
  #provide function with a two column dataframe
  #include id_name_clean and address_key*
  data<-ungroup(data)%>%
    distinct()%>%
    select(1:2)
  colnames(data)<-c("donors", "addresses")
  
  donor.net<-graph_from_edgelist(as.matrix(data), directed=F)
  V(donor.net)$type <- V(donor.net)$name %in% data$donors
  
  donor.comp <-components(donor.net)
  donor.groups<-tibble(id_name_clean =V(donor.net)$name[V(donor.net)$type],
                       id_donor = donor.comp$membership[V(donor.net)$type] )%>%
    mutate(id_donor= paste("D", id_donor, sep="."))
  ungroup(donor.groups)
}

add_donor_key<-function(data=donor_key){
  data <- data%>%
    select(id_name_clean, address_key_loose)%>%
    find_donors(.)%>%
    rename(id_donor_loose = id_donor)%>%
    right_join(., data)
  
  data <- data%>%
    select(id_name_clean, address_key_strict)%>%
    find_donors(.)%>%
    rename(id_donor_strict = id_donor)%>%
    right_join(., data)

    return(data)
}

merge_donor_key<-function(key=donor_key, orig=donors_orig){
  donor_new<-key%>%
    right_join(.,orig)
  donor_na <-donor_new%>%
    filter(is.na(id_donor_strict))%>%
    select(id_name_clean)%>%
    distinct()%>%
    left_join(key)%>%
    select(id_name_clean, id_donor_strict, id_donor_loose)
  donor_new<-donor_new%>%
    left_join(donor_na, by="id_name_clean")%>%
    mutate(id_donor_strict = coalesce(id_donor_strict.x, id_donor_strict.y),
           id_donor_loose = coalesce(id_donor_loose.x, id_donor_loose.y))%>%
    select(-ends_with(c(".x",".y")))%>%
    distinct()
  id_start_strict<-max(parse_number(str_remove(donor_new$id_donor_strict, "D.")), na.rm=TRUE)+1
  id_start_loose<-max(parse_number(str_remove(donor_new$id_donor_loose, "D.")), na.rm=TRUE)+1
  id_total <- sum(is.na(donor_new$id_donor_strict))
  donor_new<-donor_new%>%
    mutate(id_donor_strict = ifelse(is.na(id_donor_strict),
           str_c("D.", c(id_start_strict:(id_start_strict+id_total))),
           id_donor_strict),
      id_donor_loose = ifelse(is.na(id_donor_loose),
           str_c("D.", c(id_start_loose:(id_start_loose+id_total))),
           id_donor_loose)
      )
  return(donor_new)
  }
