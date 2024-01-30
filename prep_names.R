#' Prep Character Vector for Name Parsing
#'
#' This function
#' @param x Provide the character vector to prep.
#' @param reverse Indicate whether or not the name is provided as last, first (middle) or first (middle) last
#' @param orgs Indicate whether or not the name list potentially includes organizations or PACs.
#' @export
#' @note If more then one element is passed in, only the first element will be prepared and returned.
#' @note probably needs more notes here.
#' @examples
#' x <- 'livingston III,   MICHAEL JOHN9'
#' prep_name(x)

# since the census data does not have spaces, lets remove dashes of complex last names
prep_names <- function(x, compound= NULL) {
  compound_last_names<-c(" LA " = " LA"," DEN " = " DEN", " DER " = " DER",  
                         " LOS " = " LOS", " VON " = " VON", 
                         " VAN " = " VAN", " DEL " = " DEL", " DE " = " DE", 
                         " MC " = " MC" ," SA " = " SA",
                         " MAC " = " MAC", " ST " = " ST")
  compound_last_names <- c(compound, compound_last_names)
  
  return.value <- x
  
  # replace periods with spaces and cast to upper case
  return.value <- toupper(gsub('.', ' ', return.value, fixed = TRUE))
  
  # remove punctuation
  #hyphenated last names split into two last names (or last middle)
  # but not O'Brien type names - collapse those into single name
  return.value <- gsub("'", "", return.value)
  return.value <- gsub("[[:punct:]]", " ", return.value)
  return.value <- gsub('[[:digit:]]', '', return.value)
  
  # remove dup'd spaces
  return.value <- str_squish(return.value)
  
  # dash compound last names
  return.value<-return.value%>%
    str_replace_all(compound_last_names)
  
  #adding this to deal with o-brien which will ultimately be removed for census data
  return.value <-gsub("'", "-", return.value)
  
  return(return.value)
}
