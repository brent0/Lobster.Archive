#' @title  THE MAIN GUI FUNCTION!
#' @description  Opens web page of options for data entry
#' @import opencpu
#' @export
enter_data_app <- function(){
  opencpu::ocpu_start_app("Lobster.Archive", no_cache = TRUE)
}

#' @title  r.choosedir
#' @description  Function that allows the opening of a folder browser
#' @import jsonlite opencpu tcltk
#' @return file list
#' @export
r.choosedir <- function(sub = F){

  dx = tcltk::tk_choose.dir(default = "", caption = "Select directory")
fl = list.files(dx, full.names = T, recursive = sub )
#remove any temporary files
tempind = which(grepl("~\\$", fl))
if(length(tempind) > 0 ){
  fl = fl[-tempind]
}

#Only keep these file types
filetype = c(".pdf", ".png", ".jpeg", ".jpg", ".svg", ".doc", ".txt", ".csv", ".xlsx")
ind = c()
for(i in 1:length(filetype)){
   ind = c(ind, fl[which(grepl(filetype[i], fl))])
}

exi = LA.getQuery(query = "Select URI from LobsterArchive")
indexi = which(ind %in% exi$URI)
if(length(indexi) > 0 ){
  ind = ind[-indexi]
}
ind  = gsub(",", "%2C", ind)
ret = NULL
ret$omit = length(indexi)
ret$files = c(dx, ind)
  return(jsonlite::toJSON(ret))

}


#' @title  r.getPreview
#' @description  Function allows the preview of files
#' @import jsonlite opencpu
#' @return message to webpage
#' @export
r.getPreview <- function(flist){
  fl = unlist(strsplit(flist, "#filesep"))
  for(i in 1:length(fl)){
    browseURL(fl[i])
  }
  return(TRUE)

}
#' @title  r.write
#' @description Function that writes archive data to database
#' @import ROracle DBI jsonlite
#' @return message to webpage
#' @export
r.write = function(pro, year, ur){
  library("ROracle")
  out = ""
  drv <- DBI::dbDriver("Oracle")
  con <- ROracle::dbConnect(drv, username = oracle.snowcrab.user, password = oracle.snowcrab.password, dbname = oracle.snowcrab.server)
  ur = gsub("'", "''", ur)
  wri = paste("INSERT INTO LobsterArchive (PROJECT, YEAR, URI) VALUES( '",pro,"' , '",year,"' , '", ur,"')", sep = "")

  rs = ROracle::dbSendQuery(con, wri)
  if(dbGetInfo(rs, what = "rowsAffected") == 1){
    out = paste(out,"\nEntry ",ur, " with year:",year, "  project: " ,pro, " successfully added", sep = "")
  }
  else{
    out =  paste(out, "\nError: " ,writeerror , "\n" , rs, "\n", sep = "")
    return(out)
    die()
  }
  ROracle::dbCommit(con)
  out = paste(out,"\n\n", sep = "")

  ROracle::dbDisconnect(con)
  #RODBC::odbcClose(conn)
  return(out)

}
#' @title  autoavailableName
#' @description Function that help autopopulate Names in the html form
#' @import ROracle DBI jsonlite
#' @export
autoavailableName = function(){
  library("ROracle")

  drv <- DBI::dbDriver("Oracle")
  con <- ROracle::dbConnect(drv, username = oracle.snowcrab.user, password = oracle.snowcrab.password, dbname = oracle.snowcrab.server)


  result = ""
  result = ROracle::dbSendQuery(con, "select * from LobsterArchive_NAME")
  result = ROracle::fetch(result)
  result = unique(result)
  ROracle::dbDisconnect(con)
  return(jsonlite::toJSON(result))


}

#' @title  auto_availableLFA
#' @description Function that help autopopulate LFA in the html form
#' @import ROracle DBI jsonlite
#' @export
autoavailableLFA = function(){
  library("ROracle")

    drv <- DBI::dbDriver("Oracle")
    con <- ROracle::dbConnect(drv, username = oracle.snowcrab.user, password = oracle.snowcrab.password, dbname = oracle.snowcrab.server)


  result = ""
  result = ROracle::dbSendQuery(con, "select LFA from LobsterArchive_LFA")
  result = ROracle::fetch(result)
  result = unique(result)
  ROracle::dbDisconnect(con)
  result$id = 1:nrow(result)
  result$text = result$LFA
  result$LFA = NULL
  id = nrow(result)
  result = rbind(result, c(id+1, "LFA001"))
  result = rbind(result, c(id+2, "LFA002"))
  result = rbind(result, c(id+3, "LFA003"))
  result = rbind(result, c(id+4, "LFA004"))

 # result = list("results" = result)
  result$id = as.numeric(result$id)
  x = jsonlite::toJSON(result)

  return(x)

}
#' @title  auto_availableSpecies
#' @description Function that help autopopulate Species in the html form
#' @import ROracle DBI jsonlite
#' @export
autoavailableSpecies = function(){

  fn =system.file("extdata", "speciescodes.csv", package = "Lobster.Archive")
  result = read.csv(file=fn, head=T, sep=",")
  result = result[order(result$name),]
  result$id = 1:nrow(result)
  result$text = result$name

  x = jsonlite::toJSON(result)

  return(x)

}
#' @title  auto_availableDistrict
#' @description Function that help autopopulate District in the html form
#' @import ROracle DBI jsonlite
#' @export
autoavailableDistrict = function(){

  fn =system.file("extdata", "districtcodes.csv", package = "Lobster.Archive")
  result = read.csv(file=fn, head=T, sep=",")
  result = result[order(result$name),]
  result$id = 1:nrow(result)
  result$text = result$name

  x = jsonlite::toJSON(result)

  return(x)

}
#' @title  auto_availableStadistrict
#' @description Function that help autopopulate District in the html form
#' @import ROracle DBI jsonlite
#' @export
autoavailableStatdistrict = function(){

  fn =system.file("extdata", "districtcodes.csv", package = "Lobster.Archive")
  result = read.csv(file=fn, head=T, sep=",")
  result = result[order(result$statname),]
  result$id = 1:nrow(result)
  result$text = result$statname

  x = jsonlite::toJSON(result)

  return(x)

}
#' @title  auto_availablePort
#' @description Function that help autopopulate Port in the html form
#' @import ROracle DBI jsonlite
#' @export
autoavailablePort = function(){
  library("ROracle")

  drv <- DBI::dbDriver("Oracle")
  con <- ROracle::dbConnect(drv, username = oracle.snowcrab.user, password = oracle.snowcrab.password, dbname = oracle.snowcrab.server)


  result = ""
  result = ROracle::dbSendQuery(con, "select WHARF_NAME from MARFISSCI.WHARVES")
  result = ROracle::fetch(result)
  result = unique(result)
  ROracle::dbDisconnect(con)
  result$text = result$WHARF_NAME
  result$WHARF_NAME = NULL
  result$id = 1:nrow(result)


  x = jsonlite::toJSON(result)

  return(x)

}
#' @title  autoavailable
#' @description Function that returns unique values of a column
#' @import ROracle DBI jsonlite
#' @param column The column to return
#' @export
autoavailable = function(column = ""){
  library("ROracle")

  drv <- DBI::dbDriver("Oracle")
  con <- ROracle::dbConnect(drv, username = oracle.snowcrab.user, password = oracle.snowcrab.password, dbname = oracle.snowcrab.server)
  result = ""
  result = ROracle::dbSendQuery(con, paste("select ",column," from LobsterArchive", sep=""))
  result = ROracle::fetch(result)
  result = unique(result)
  ROracle::dbDisconnect(con)
  return(jsonlite::toJSON(result))

}

#' @title  myUrlEncode
#' @description  Encode url
#' @param string The url to decode
#' @export
myUrlEncode <- function(string) {
 replacements = c("%21", "%2A", "%27", "%28", "%29", "%3B", "%40", "%2B", "%24", "%2C", "%3F", "%23", "%5B", "%5D")
  entities = c("!", "\\*", "'", "\\(", ")", ";", "@", "\\+", "\\$", ",", "\\?", "#", "\\[", "]")


  for(i in 1:length(entities)){

    string = gsub(entities[i], replacements[i], string)
  }
  return(string)
}

#' @title  myUrlDecode
#' @description  Decode url
#' @param string The url to decode
#' @export
myUrlDecode <- function(string) {
  entities = c("%21", "%2A", "%27", "%28", "%29", "%3B", "%3A", "%40", "%2B", "%24", "%2C", "%2F", "%3F", "%23", "%5B", "%5D")
  replacements = c("!", "\\*", "'", "\\(", ")", ";", ":", "@", "\\+", "\\$", ",", "/", "\\?", "#", "\\[", "]")


  for(i in 1:length(entities)){

    string = gsub(entities[i], replacements[i], string)
  }
  return(string)
}


#' @title  LA.getQuery
#' @description Function that queriest LobsterArchive
#' @import ROracle DBI jsonlite
#' @return data result
#' @export
LA.getQuery = function(query = ""){
  library("ROracle")

  drv <- DBI::dbDriver("Oracle")
  con <- ROracle::dbConnect(drv, username = oracle.snowcrab.user, password = oracle.snowcrab.password, dbname = oracle.snowcrab.server)

  result = ""
  result = ROracle::dbSendQuery(con, query)
  result = ROracle::fetch(result)

  ROracle::dbDisconnect(con)
  return(result)

}
