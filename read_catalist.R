library(readxl)

write_precat <- function(data, project = "AB2017", n.rows=100000){
  x <- ceiling(nrow({{data}})/n.rows)
  
  data.sub <-split({{data}}, rep(1:x, times = n.rows,
                                 length=nrow({{data}})))
  for(i in 1:x){
    filename = paste("catalist_pre/Donor", project, 
                     "_pt", i, ".csv", sep="" )
    zipname = paste("catalist_pre/Donor", project, 
                    "_pt", i, ".zip", sep="" )
    write_csv(data.sub[[i]], filename, col_names=TRUE)
    zip(zipname,filename)
  }
}


read_catalist<-function(file, newvar = "newvar",project="lowell"){
  filepath<-paste("catalist_post/", project, file, sep="")
  #read colnames
  catalist.cols<-read_csv(filepath,
                            skip=1)%>%
    select(-1, -2)%>%
    colnames(.)
  catalist.cols<-c("drop", "id", catalist.cols)
  catalist.data<-read_csv(filepath,
                            skip=4,
                            col_names=catalist.cols)%>%
    select(-drop)
  catalist.data<-pivot_longer(catalist.data,
                              cols = -id,
                              names_to = newvar,
                              values_to = "value")%>%
    filter(value==1)%>%
    select(-value)
}
