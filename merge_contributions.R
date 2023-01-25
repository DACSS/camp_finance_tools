contributions_clean_v1 <- contributions_orig %>%
  select(id_trans, id_add_orig, id_name_orig, 
         orig_name, street_orig, city_orig, state_orig, zip_orig,
         elect_candidate, elect_year, 
         donate_date, amount, employer_orig,
         occupation_orig)

contributions_clean_v1 <- name_clean %>%
  right_join(., contributions_clean_v1, by = "id_name_orig")

contributions_clean_v1 <- address_clean%>%
  right_join(., contributions_clean_v1) 