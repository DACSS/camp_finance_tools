check_duplicates<-function(.data, id, orig_vars){
  .data%>%
    group_by({{id}})%>%
    summarise(n = n_distinct({{orig_vars}}))%>%
    group_by(n)%>%
    summarise(n())
}

find_duplicates<-function(data, id, threshold=1,...){
  data%>%
    group_by({{id}})%>%
    select(...)%>%
    distinct()%>%
    mutate(n = n())%>%
    filter(n>threshold)%>%
    group_split()
}

find_duplicates_cleanid<-function(data, id, threshold=1, 
                                  keepvars=NULL, sumvars=NULL){
  data%>%
    group_by({{id}})%>%
    select(id_add_clean, id_name_clean, {{keepvars}}, {{sumvars}})%>%
    mutate(across({{sumvars}}, ~paste(unique(.x), collapse="; ")))%>%
    distinct()%>%
    mutate(n= n_distinct(id_add_clean, id_name_clean))%>%
    filter(n>threshold)%>%
    group_split()
}
