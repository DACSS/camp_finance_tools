contributions_cash <- orig_data%>%
  filter(orig_name %in% c("Aggregated Unitemized Receipts",
                          "FROM [[:print:]] CASH$"))

#Create Unique IDs for Original Data
#We create a new dataframe called 'contributions_orig' 
#which includes new IDs for each transaction, 
#unique contributor name, and unique address. 
#We will use the name and address IDs when we 
#merge the clean versions back together. 

contributions_orig <- orig_data %>%
  anti_join(., contributions_cash)%>%
  mutate(id_trans = row_number()) %>%
  group_by(orig_name) %>%
  mutate(id_name_orig = paste("N", cur_group_id(), sep=".")) %>%
  ungroup() %>%
  group_by(street_orig, city_orig, state_orig, zip_orig) %>%
  mutate(id_add_orig = paste("A", cur_group_id(), sep=".")) %>%
  ungroup()