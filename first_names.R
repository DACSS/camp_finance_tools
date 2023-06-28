filenames <- list.files("./names", pattern="*.txt")

read_babynames<-function(path="./names", fn){
  read_csv(paste(path, fn, sep="/"), 
           col_names = c("name", "gender", "count"))%>%
    mutate(year = parse_number(fn))
}

babynames<- map_dfr(filenames, ~read_babynames(fn=.x))
