#' Parse a Set of Names (Vectorized)
#'
#' This function returns a data frame of parse attributes based on an input of a vector of names. These names may include organizations, and may be in first last or last, first format.
#' @param x Provide the charater vector to parse.
#' @param reverse Indicate whether or not the name is provided as last, first (middle) or first (middle) last
#' @param orgs Indicate whether or not the name list potentially includes organizations or PACs.
#' @param ... Alternate arguments for suffixes, prefixes, org_names, etc.
#' @keywords parse
#' @note Recommend running the "setup" function prior to this step, as it will create the .Rdata file to make name parsing go more quickly, particularly with repeated runs.
#' @export
#' @examples
#' x <- 'livingston III,  Mr. MICHAEL JOHN9'
#' parse.names(x)
#' ##my updated version that doesn't go element by element 

parse_names<-function (x, id, add_prefixes = NULL, add_suffixes = NULL, 
                       reverse = FALSE, orgs = TRUE, add_orgterms = NULL) 
{
  prefixes <- c('MR','DR','MISS','MS','MRS', "CAPT", "REP", "HON", "HONORABLE", "THE HONORABLE") 
  prefixes <- c(prefixes, add_prefixes)
  suffixes <- c('JR','II','III','IV','SR',"MD", "DDS","M D", "PHD", "PH D", "CFP", "ESQ") 
  suffixes <- c(suffixes, add_suffixes)
  
  library(data.table)
  # if class (x) is dataframe, take first string column
  # if class (x) is factor vector, convert
  # if class (x) is non-string then throw error
  
  # make a copy
  input.name <- x
  
  # prepare name for parsing
  x <- prep_names(x)
  
  #new approach to prefix and suffix removal
  salutation.name <-str_extract(x, str_c("^",prefixes, " ",collapse="|"))
  x<-str_remove(x, str_c("^",prefixes," ",collapse="|"))
  suffix.name <- str_extract(x, str_c(" ",suffixes,"\\b",collapse="|"))
  x<-str_remove(x, str_c(" ",suffixes,"\\b",collapse="|"))
  
  # split words in to vector
  # count length needed
  n_names <-str_count(x, " ")+1
  listx <- str_split(x,' ')
  names<-tibble(orig_name=input.name, 
                id_name_orig = id,
                n_names = n_names, 
                salutation = salutation.name,
                suffix = suffix.name)
    
  # reorder name based on reverse = TRUE or FALSE
  # could incorporate a guessing algorithm from original package
  names<-names%>%
    mutate(
      first_name = case_when(
        reverse == TRUE ~ map_chr(listx, 2, .default = NA),
        reverse == FALSE ~ map_chr(listx, 1, .default = NA)),
      last_name = case_when(
        reverse == TRUE ~ map_chr(listx, 1, .default = NA),
        reverse == FALSE ~ map_chr(listx, ~tail(.x, 1L), .default = NA)),
      middle_name = case_when(
        reverse == TRUE ~ map_chr(listx, ~str_c(tail(.x,-2), collapse=" "), 
                                  .default = NA),
        reverse == FALSE ~ map_chr(listx, ~str_c(tail(head(.x,-1),-1), collapse=" "), 
                                   .default = NA)),
      clean_name = paste(first_name, middle_name, last_name, sep=" "),
      name.and = str_detect(orig_name, paste(c(" and ","\\&","\\&amp"), collapse="|")))
  
  #calculate number of initials
  #helpful for org identification
  l <- lapply(listx,nchar)  # length of each element
  names<-names%>%
    mutate(n_inits = map_dbl(l, ~ sum(.x==1)))
  
  #identify organizations
  #currently not including unrecognized names from census and ss data
  if(orgs==TRUE){
    names<-find_orgs(names, add_orgterms)
  }
  
  return(names)
}