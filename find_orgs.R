#' Find Organizations in Campaign Donation Data
#'
#' This function
#' @param x Provide the "donor name" character vector to prep.
#' @param orgterms Provide a "dictionary" or vector of terms that can be used to identify organizations. Regular expressions can be included, but be careful about overmatching individual names. (e.g., "Council" is a last name and a word often used in organization names.) A break is automatically added before and after each string in the vector, that matches spaces or start/end of string, so modify via regex if needed to account for variants of a word.
#' @export
#' @note If more then one element is passed in, only the first element will be handled and returned.
#' @note At this point, it doesn't matter of individual donor names are last, first or first last
#' 
#' @examples
#' x <- 'Seattle Firefighters Association'
#' find.orgs(x)


find_orgs<-function(x=names, org_terms=NULL){
  orgterms <-c("SEATTLE[S]*", "GLACIER", "WA", "NW", "OF",  "LLC",
                 "INC", "INCORPORATED", "ASSOC[IATONSE]*", "GROUP[S]*", "ACCT",
                 "COMMITTEE[S]*", "POLITICAL", "FUND", "OFFICE", "LLP", "CORPORAT[EIONS]*",
                 "LTD", "GUILD", "PSIE", "UNION", "PAC", "FOUNDATION", "FACILIT[YIES]*",
                 "LEAGUE", "CHAMBER", "SEIU", "EMPLOYEES", "ALLIANCE", "INTERNATIONAL",
                 "CTTE", "COMM", "CO", "LP", "PLLC", "COMPAN[YIES]*", "INSURANCE", "CONSERV[ANCYTION]*",
                 "LOCAL", "COUNCIL[S]*", "ENGINEERING", "HOTEL[S]*", "SERVICE[S]*", "ENTERPRISE[S]*",
                 "ACCOUNT", "STRATEG[YIES]*", "STUDIO[S]*", "PROPERT[YIES]*", "PARTNERS[HIP]*",
                 "INVESTMENTS", "ESTATE[S]*", "COALITION", "ADVERTISING", "SOLUTION[S]*",
                 "SAVINGS", "CORP", "CHEESE", "CONSTRUCTION", "PRESENTATION", "COMMUNICAT[[:alpha:]]*",
                 "AUTOMATION", "CONTRACTORS", "MANAGEMENT", "HOLDING[S]*", "CAPITAL", "RESEARCH",
                 "VOT[INGERS]*", "RECYCLERS", "[BI]*CYCLE[S]*", "VENTURES", "BUSINESS", "WASTE", "SYSTEM[S]*",
                 "TECHNOLOG[YIES]*","NAMEPLATE[S]*", "DEVELOPMENT[S]*", "SEAFOOD[S]*",
                 "VIDEO[S]*", "DESIGN[[:alpha:]]*", "DYNAMIC[S]*", "HOUSING", "BUILD[ING]*",
                 "CONSULT[INGANTS]*", "GRILL", "DRILLING", "WINDOWS", "FISH AND", "COMMODOTIES",
                 "LIMOSUINE[S]*", "BOUTIQUE", "PLANNING", "UNITE[D]*", "HEALTH[CARE]*", "ASSN",
                 "FIREFIGHTER[S]*", "INDIAN",  "INSTITUTE", "AGC", "[POLY]*CLINIC",
                 "RESTAURANT[S]*", "AGENC[YIES]*", "INDUSTR[YIESAL]*", "FOOD", "PEOPLE", "PUGET",
                 "CLUB[S]*", "ARCHITECT[S]*", "LTD", "LIMITED", "PUBLISHER[S]*", "SIGNS", "SHELLFISH",
                 "BREWERY", " INN[ ^]", "MUSIC", "LAW FIRM", "LEGAL", "BLUE CROSS", "CITY", "HOMES",
                 "ORGANIC", "TABLE", "THE", "CITIZEN[S]*", "FOR", "AAA", "VOICE", "ACTION", "AMALGAMATED",
                 "AMAZON", "REPRESENT", "FUSE", "AIRLINE[S]*", "VISION", "[SEA]*FOOD[S]*", "CHAPTER",
                 "COMCAST", "RESOURCE[S]*", "RESIDENTIAL", "DEMOCRA[CYATS]*", "ORGANIC[S]*")
  orgterms<-c(org_terms, orgterms)
  
  x<-x%>%
    mutate(org = case_when(
      #add breaks before and after each word
      str_detect(clean_name,str_c("\\b",orgterms,"\\b", sep="", collapse="|")) ~ "org",
      n_names>4 & !name.and ~ "org",
      n_names == 1 ~ "org",
      n_inits == n_names ~ "org",
      #firstname middleinit lastname pattern is always a name
      nchar(first_name)>1 & nchar(middle_name)==1 & nchar(last_name)>1 & n_names==3 ~ "not org",
      #firstinit middlename lastname pattern is always a name
      nchar(first_name)==1 & nchar(middle_name)>1 & nchar(last_name)>1 & n_names==3 ~ "not org",
      #look for couples with first1 & first2 last pattern
      #this should probably be fixed for different datasets
      
      #add semi-automated "and" name coding
      name.and ~ "not org",
      #firstinit middleinit lastname pattern is always a name unless there is an ampersand
      nchar(first_name)==1 & nchar(middle_name)==1 & nchar(last_name)>1 & n_names==3 ~ "not org",
      #catch all remaining names with at least 2 unrecognized
      #this is only an issue for individuals with first and last names missing from reference set and no initials
      #or maybe names with firstinit lastname
      #orgs will have multiple nas
      #num.nas>2 ~ "org",
      TRUE ~"not org"
    ))
  return(x)
}
