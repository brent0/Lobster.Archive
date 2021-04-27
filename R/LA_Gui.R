#' @import rChoiceDialogs rJava

#' @title Archive_App
#' @description THE MAIN GUI FUNCTION! Opens web page of options for data entry
#' @import opencpu
#' @export
Archive_App <- function(){
  opencpu::ocpu_start_app("Lobster.Archive", no_cache = TRUE)
}

#' @title  r.backup
#' @description  Function allows backing up the oracle tables to R data files
#' @import ROracle DBI jsonlite opencpu
#' @return message to webpage
#' @export
r.backup <- function(def = "L:/"){
  if(def == ""){
    def = "L:"
  }
  drv <- DBI::dbDriver("Oracle")
  con <- ROracle::dbConnect(drv, username = oracle.username, password = oracle.password, dbname = oracle.server)

  result = ROracle::dbSendQuery(con, "Select * from LOBSTER.LOBSTERARCHIVE")
  LOBSTERARCHIVE = ROracle::fetch(result)

  result = ROracle::dbSendQuery(con, "Select * from LOBSTER.LOBSTERARCHIVE_COMMUNITY")
  LOBSTERARCHIVE_COMMUNITY = ROracle::fetch(result)

  result = ROracle::dbSendQuery(con, "Select * from LOBSTER.LOBSTERARCHIVE_DISTRICT_LOOKUP")
  LOBSTERARCHIVE_DISTRICT_LOOKUP = ROracle::fetch(result)

  result = ROracle::dbSendQuery(con, "Select * from LOBSTER.LOBSTERARCHIVE_LFA")
  LOBSTERARCHIVE_LFA = ROracle::fetch(result)

  result = ROracle::dbSendQuery(con, "Select * from LOBSTER.LOBSTERARCHIVE_NAME")
  LOBSTERARCHIVE_NAME = ROracle::fetch(result)

  result = ROracle::dbSendQuery(con, "Select * from LOBSTER.LOBSTERARCHIVE_PORT")
  LOBSTERARCHIVE_PORT = ROracle::fetch(result)

  result = ROracle::dbSendQuery(con, "Select * from LOBSTER.LOBSTERARCHIVE_PORT_LOOKUP")
  LOBSTERARCHIVE_PORT_LOOKUP = ROracle::fetch(result)

  result = ROracle::dbSendQuery(con, "Select * from LOBSTER.LOBSTERARCHIVE_PROVINCE")
  LOBSTERARCHIVE_PROVINCE = ROracle::fetch(result)

  result = ROracle::dbSendQuery(con, "Select * from LOBSTER.LOBSTERARCHIVE_SPECIES")
  LOBSTERARCHIVE_SPECIES = ROracle::fetch(result)

  result = ROracle::dbSendQuery(con, "Select * from LOBSTER.LOBSTERARCHIVE_SPECIES_LOOKUP")
  LOBSTERARCHIVE_SPECIES_LOOKUP = ROracle::fetch(result)

  result = ROracle::dbSendQuery(con, "Select * from LOBSTER.LOBSTERARCHIVE_STATDIST")
  LOBSTERARCHIVE_STATDIST = ROracle::fetch(result)

  result = ROracle::dbSendQuery(con, "Select * from LOBSTER.LOBSTERARCHIVE_YEAR")
  LOBSTERARCHIVE_YEAR = ROracle::fetch(result)

  sysdate = Sys.Date()
  ROracle::dbDisconnect(con)
  if(!dir.exists(file.path(def, "Lobster Archive Backup", sysdate)))
    dir.create(file.path(def, "Lobster Archive Backup", sysdate), recursive = T)
  save( LOBSTERARCHIVE, LOBSTERARCHIVE_COMMUNITY, LOBSTERARCHIVE_DISTRICT_LOOKUP,
        LOBSTERARCHIVE_LFA, LOBSTERARCHIVE_NAME, LOBSTERARCHIVE_PORT,
        LOBSTERARCHIVE_PORT_LOOKUP, LOBSTERARCHIVE_PROVINCE, LOBSTERARCHIVE_SPECIES,
        LOBSTERARCHIVE_SPECIES_LOOKUP, LOBSTERARCHIVE_STATDIST, LOBSTERARCHIVE_YEAR,
        file = file.path(def, "Lobster Archive Backup", sysdate, "tables.RData"))

  return(paste("Tables backed up to: ", file.path(def, "Lobster Archive Backup", sysdate, "tables.RData"), sep=""))

}

#' @title  r.remove
#' @description  Function allows removal of uri from Oracle tables
#' @import ROracle DBI jsonlite opencpu
#' @return message to webpage
#' @export
r.remove <- function(uri = ""){
  if(uri == ""){
    return("No URI supplied")
  }
  drv <- DBI::dbDriver("Oracle")
  con <- ROracle::dbConnect(drv, username = oracle.username, password = oracle.password, dbname = oracle.server)

  result = ROracle::dbSendQuery(con, paste("Delete from LOBSTER.LOBSTERARCHIVE where uri = '", uri, "'", sep=""))
  if(ROracle::dbGetInfo(result, what = "rowsAffected") > 1){
    ROracle::dbRollback(con)
    ROracle::dbDisconnect(con)
    return("dubious deletion -- rolling back transaction")
  }
  result2 = ROracle::dbSendQuery(con, paste("Delete from LOBSTER.LOBSTERARCHIVE_COMMUNITY where uri = '", uri, "'", sep=""))
  if(ROracle::dbGetInfo(result2, what = "rowsAffected") > 1){
    ROracle::dbRollback(con)
    ROracle::dbDisconnect(con)
    return("dubious deletion -- rolling back transaction")
  }
  result3 = ROracle::dbSendQuery(con, paste("Delete from LOBSTER.LOBSTERARCHIVE_LFA where uri = '", uri, "'", sep=""))
  if(ROracle::dbGetInfo(result3, what = "rowsAffected") > 1){
    ROracle::dbRollback(con)
    ROracle::dbDisconnect(con)
    return("dubious deletion -- rolling back transaction")
  }
  result4 = ROracle::dbSendQuery(con, paste("Delete from LOBSTER.LOBSTERARCHIVE_NAME where uri = '", uri, "'", sep=""))
  if(ROracle::dbGetInfo(result4, what = "rowsAffected") > 1){
    ROracle::dbRollback(con)
    ROracle::dbDisconnect(con)
    return("dubious deletion -- rolling back transaction")
  }
  result5 = ROracle::dbSendQuery(con, paste("Delete from LOBSTER.LOBSTERARCHIVE_PORT where uri = '", uri, "'", sep=""))
  if(ROracle::dbGetInfo(result5, what = "rowsAffected") > 1){
    ROracle::dbRollback(con)
    ROracle::dbDisconnect(con)
    return("dubious deletion -- rolling back transaction")
  }
  result6 = ROracle::dbSendQuery(con, paste("Delete from LOBSTER.LOBSTERARCHIVE_PROVINCE where uri = '", uri, "'", sep=""))
  if(ROracle::dbGetInfo(result6, what = "rowsAffected") > 1){
    ROracle::dbRollback(con)
    ROracle::dbDisconnect(con)
    return("dubious deletion -- rolling back transaction")
  }
  result7 = ROracle::dbSendQuery(con, paste("Delete from LOBSTER.LOBSTERARCHIVE_SPECIES where uri = '", uri, "'", sep=""))
  if(ROracle::dbGetInfo(result7, what = "rowsAffected") > 1){
    ROracle::dbRollback(con)
    ROracle::dbDisconnect(con)
    return("dubious deletion -- rolling back transaction")
  }

  result8 = ROracle::dbSendQuery(con, paste("Delete from LOBSTER.LOBSTERARCHIVE_STATDIST where uri = '", uri, "'", sep=""))
  if(ROracle::dbGetInfo(result8, what = "rowsAffected") > 1){
    ROracle::dbRollback(con)
    ROracle::dbDisconnect(con)
    return("dubious deletion -- rolling back transaction")
  }

  result9 = ROracle::dbSendQuery(con, paste("Delete from LOBSTER.LOBSTERARCHIVE_YEAR where uri = '", uri, "'", sep=""))
  if(ROracle::dbGetInfo(result9, what = "rowsAffected") > 1){
    ROracle::dbRollback(con)
    ROracle::dbDisconnect(con)
    return("dubious deletion -- rolling back transaction")
  }
  ROracle::dbCommit(con)

  ROracle::dbClearResult(result)
  ROracle::dbClearResult(result2)
  ROracle::dbClearResult(result3)
  ROracle::dbClearResult(result4)
  ROracle::dbClearResult(result5)
  ROracle::dbClearResult(result6)
  ROracle::dbClearResult(result7)
  ROracle::dbClearResult(result8)
  ROracle::dbClearResult(result9)


  ROracle::dbDisconnect(con)

  return(paste("Tables purged of URI: ", uri, sep=""))

}



#' @title  r.choosedir
#' @description  Function that allows the opening of a folder browser
#' @import ROracle DBI jsonlite opencpu
#' @return file list
#' @export
r.choosedir <- function(sub = F, def = "L:/"){
  cont = FALSE

  out <- tryCatch(
    {
      library(rChoiceDialogs)

      if(def == ""){
        def = "L:"
        Sys.setenv('LAARC' = "L:")
        dx = rChoiceDialogs::jchoose.dir(default = Sys.getenv('LAARC'),  caption = "Choose Directory", modal = F)

        dx = gsub("\\\\", "/", dx)
      }else{
        def = sub("/", "", def)
        Sys.setenv('LAARC' = def)
        dx = rChoiceDialogs::jchoose.dir(default = Sys.getenv('LAARC'),  caption = "Choose Directory", modal = F)
        dx = gsub("\\\\", "/", dx)
      }
      cont = TRUE
    },
    error=function(cond) {
      Sys.sleep(1)
      r.choosedir(sub = sub, def = def)
    },
    finally={
    }
  )
  if(cont){
    fl = list.files(dx, full.names = T, recursive = sub )

    #remove any temporary files
    tempind = which(grepl("~\\$", fl))
    if(length(tempind) > 0 ){
      fl = fl[-tempind]
    }

    #Only keep these file types
    filetype = c(".pdf", ".png", ".jfif", ".jpeg", ".jpg", ".svg", ".doc", ".txt", ".csv", ".xlsx")
    ind = c()
    for(i in 1:length(filetype)){
      ind = c(ind, fl[which(grepl(filetype[i], fl))])
    }

    drv <- DBI::dbDriver("Oracle")
    con <- ROracle::dbConnect(drv, username = oracle.username, password = oracle.password, dbname = oracle.server)

    exi = ""
    exi = ROracle::dbSendQuery(con, "Select URI from LOBSTER.LOBSTERARCHIVE")
    exi = ROracle::fetch(exi)
    ind = gsub(def, "L:", ind)
    indexi = which(ind %in% exi$URI)
    if(length(indexi) > 0 ){
      ind = ind[-indexi]
    }
    ind  = gsub(",", "%2C", ind)
    ind = gsub("'", "&#39", ind)
    ret = NULL
    ret$omit = length(indexi)
    ret$files = c(dx, ind)
    return(jsonlite::toJSON(ret))
  }
}


#' @title  r.move
#' @description  Function allows the moving files to a folder
#' @import opencpu
#' @return message to webpage
#' @export
r.move <- function(flist){

  library(rChoiceDialogs)

  dx = jchoose.dir(default = Sys.getenv("HOME"),  caption = "Select Drive")
  dx = gsub("\\\\", "/", dx)

  fl = unlist(strsplit(flist, "#filesep"))
  for(i in 1:length(fl)){
    file.copy(fl[i], paste(dx, basename(fl[i]), sep = .Platform$file.sep), overwrite = T, copy.date = TRUE)
  }
  return(paste("Files copied to ", dx, sep=""))

}

#' @title  checkdrive
#' @description  Checks to see if L drive exists and sets variable if so
#' @import opencpu
#' @return message to webpage
#' @export
checkdrive <- function(){
  if(dir.exists(file.path("L:\\"))){
    return("T")
  }
  else{
    return("F")
  }
}

#' @title  changedrive
#' @description  Function allows no L: drive operation
#' @import opencpu
#' @return message to webpage
#' @export
changedrive <- function(){

  library(rChoiceDialogs)

  dx = jchoose.dir(default = Sys.getenv("HOME"),  caption = "Select Drive")

  dx = gsub("\\\\", "/", dx)
  dx = paste(unlist(strsplit(dx, ":"))[1], ":/", sep = "")

  return(dx)

}

#' @title  r.getPreview
#' @description  Function allows the preview of files
#' @import jsonlite opencpu
#' @return message to webpage
#' @export
r.getPreview <- function(flist){
  fl = unlist(strsplit(flist, "#filesep"))
  for(i in 1:length(fl)){
    x = fl[i]
    ind = gsub("&#39", "'", x)
    shell.exec(x)
  }
  return(TRUE)

}
#' @title  r.write
#' @description Function that writes archive data to database
#' @import ROracle DBI jsonlite opencpu
#' @return message to webpage
#' @export
r.write = function(proj, years, uri, firstnames, lastnames, lfas, districts, sdistricts, communities, portcodes, codeports, provinces, docname, abstractname, pagesname, speciesnames, speciescodes, Ad, Ar, As, Ba, By, Ca, Cs, Ct, Cl, Co, Cr, Cu, De, Dv, Dr, Ef, En, Fr, Ge, Gu, Hi, Im, In, Id, It, Jo, La, Le, Li, Lo, Ma, Mu, Mt, Mi, Mo, Ms, Nf, Ne, No, Of, Fi, Pa, Po, Pr, Pc, Ra, Re, Rp, Se, Sl, So, Su, Sc, Tag, Te, Ts, Tr, Ta, Up, Us, Vi, Vn, Vo, Wo){

  out = ""
  out = paste(out," File: ", uri, sep = "")

  drv <- DBI::dbDriver("Oracle")
  con <- ROracle::dbConnect(drv, username = oracle.username, password = oracle.password, dbname = oracle.server)

  uri = gsub("'", "''", uri)
  if(firstnames != "" || lastnames != ""){
    fnames = gsub(" ", "", unlist(strsplit(firstnames, ",")))
    lnames = gsub(" ", "", unlist(strsplit(lastnames, ",")))
    wri = paste("INSERT INTO LOBSTER.LOBSTERARCHIVE_NAME (FIRST, LAST, URI) VALUES( '",fnames,"' , '",lnames,"' , '",uri,"')", sep = "")
    for(i in 1:length(wri)){
      rs = ROracle::dbSendQuery(con, wri[i])
      if(ROracle::dbGetInfo(rs, what = "rowsAffected") == 1){
        out = paste(out,"\n   LOBSTERARCHIVE_NAME done", sep = "")
      }
      else{
        out =  paste("\nError: " ,ROracle::writeerror , "\n" , rs, "\n", sep = "")
        ROracle::dbDisconnect(con)
        return(out)

      }
    }

  }


  if(is.character(years)){
    yrs = gsub(" ", "", unlist(strsplit(years, ",")))


    wri = paste("INSERT INTO LOBSTER.LOBSTERARCHIVE_YEAR (URI, YEAR) VALUES( '",uri,"' , '",yrs,"')", sep = "")
    for(i in 1:length(wri)){
      rs = ROracle::dbSendQuery(con, wri[i])
      if(ROracle::dbGetInfo(rs, what = "rowsAffected") == 1){
        out = paste(out,"\n   LOBSTERARCHIVE_YEAR done", sep = "")
      }
      else{
        out =  paste("\nError: " ,ROracle::writeerror , "\n" , rs, "\n", sep = "")
        ROracle::dbDisconnect(con)
        return(out)
      }

    }
  }
  if(is.character(lfas)){
    lfa = gsub(" ", "", unlist(strsplit(lfas, ",")))
    district = gsub(" ", "", unlist(strsplit(districts, ",")))
    lfa = gsub("'", "''", lfa)
    district = gsub("'", "''", district)

    wri = paste("INSERT INTO LOBSTER.LOBSTERARCHIVE_LFA (URI, LFA, DISTRICT) VALUES( '",uri,"' , '",lfa,"' , '",district,"')", sep = "")
    for(i in 1:length(wri)){
      rs = ROracle::dbSendQuery(con, wri[i])
      if(ROracle::dbGetInfo(rs, what = "rowsAffected") == 1){
        out = paste(out,"\n   LOBSTERARCHIVE_LFA done", sep = "")
      }
      else{
        out =  paste("\nError: " ,ROracle::writeerror , "\n" , rs, "\n", sep = "")
        ROracle::dbDisconnect(con)
        return(out)

      }

    }
  }
  if(is.character(sdistricts)){
    sdistrict = gsub(" ", "", unlist(strsplit(sdistricts, ",")))
    sdistrict = gsub("'", "''", sdistrict)
    wri = paste("INSERT INTO LOBSTER.LOBSTERARCHIVE_STATDIST (URI, STATDIST) VALUES( '",uri,"' , '",sdistrict,"')", sep = "")
    for(i in 1:length(wri)){
      rs = ROracle::dbSendQuery(con, wri[i])
      if(ROracle::dbGetInfo(rs, what = "rowsAffected") == 1){
        out = paste(out,"\n   LOBSTERARCHIVE_STATDIST done", sep = "")
      }
      else{
        out =  paste("\nError: " ,ROracle::writeerror , "\n" , rs, "\n", sep = "")
        ROracle::dbDisconnect(con)
        return(out)

      }


    }
  }

  if(is.character(communities)){
    commun = gsub(" ", "", unlist(strsplit(communities, ",")))
    commun = gsub("'", "''", commun)
    wri = paste("INSERT INTO LOBSTER.LOBSTERARCHIVE_COMMUNITY(URI, COMMUNITY_CODE) VALUES( '",uri,"' , '",commun,"')", sep = "")
    for(i in 1:length(wri)){
      rs = ROracle::dbSendQuery(con, wri[i])
      if(ROracle::dbGetInfo(rs, what = "rowsAffected") == 1){
        out = paste(out,"\n   LOBSTERARCHIVE_COMMUNITY done", sep = "")
      }
      else{
        out =  paste("\nError: " ,ROracle::writeerror , "\n" , rs, "\n", sep = "")
        ROracle::dbDisconnect(con)
        return(out)

      }


    }
  }
  if(is.character(portcodes)){
    portc = gsub(" ", "", unlist(strsplit(portcodes, ",")))
    codep = gsub(" ", "", unlist(strsplit(codeports, ",")))
    portc = gsub("'", "''", portc)
    codep = gsub("'", "''", codep)

    wri = paste("INSERT INTO LOBSTER.LOBSTERARCHIVE_PORT(URI, PORT_NAME, PORT_CODE) VALUES( '",uri,"' , '",portc,"' , '",codep,"')", sep = "")
    for(i in 1:length(wri)){
      rs = ROracle::dbSendQuery(con, wri[i])
      if(ROracle::dbGetInfo(rs, what = "rowsAffected") == 1){
        out = paste(out,"\n   LOBSTERARCHIVE_PORT done", sep = "")
      }
      else{
        out =  paste("\nError: " ,ROracle::writeerror , "\n" , rs, "\n", sep = "")
        ROracle::dbDisconnect(con)
        return(out)

      }

      #  ROracle::dbCommit(con)

    }
  }
  if(is.character(provinces)){
    prov = gsub(" ", "", unlist(strsplit(provinces, ",")))
    prov = gsub("'", "''", prov)

    wri = paste("INSERT INTO LOBSTER.LOBSTERARCHIVE_PROVINCE(URI, PROV) VALUES( '",uri,"' , '",prov,"')", sep = "")
    for(i in 1:length(wri)){
      rs = ROracle::dbSendQuery(con, wri[i])
      if(ROracle::dbGetInfo(rs, what = "rowsAffected") == 1){
        out = paste(out,"\n   LOBSTERARCHIVE_PROVINCE done", sep = "")
      }
      else{
        out =  paste("\nError: " ,ROracle::writeerror , "\n" , rs, "\n", sep = "")
        ROracle::dbDisconnect(con)
        return(out)

      }


    }

  }
  if(is.character(speciesnames)){
    specn = gsub(" ", "", unlist(strsplit(speciesnames, ",")))
    specc = gsub(" ", "", unlist(strsplit(speciescodes, ",")))
    specn = gsub("'", "''", specn)
    specc = gsub("'", "''", specc)

    wri = paste("INSERT INTO LOBSTER.LOBSTERARCHIVE_SPECIES(URI, SPECIES_NAME, SPECIES_CODE) VALUES( '",uri,"' , '",specn,"' , '",specc,"')", sep = "")
    for(i in 1:length(wri)){
      rs = ROracle::dbSendQuery(con, wri[i])
      if(ROracle::dbGetInfo(rs, what = "rowsAffected") == 1){
        out = paste(out,"\n   LOBSTERARCHIVE_SPECIES done", sep = "")
      }
      else{
        out =  paste("\nError: " ,ROracle::writeerror , "\n" , rs, "\n", sep = "")
        ROracle::dbDisconnect(con)
        return(out)

      }
    }
  }
  if(!is.character(proj)){
    proj = ""
  }
  if(!is.character(docname)){
    docname = ""
  }
  if(!is.character(pagesname)){
    pagesname = ""
  }
  if(!is.character(abstractname)){
    abstractname = ""
  }
  proj = gsub("'", "''", proj)
  docname = gsub("'", "''", docname)
  pagesname = gsub("'", "''", pagesname)
  abstractname = gsub("'", "''", abstractname)


  wri = paste("INSERT INTO LOBSTER.LOBSTERARCHIVE(PROJECT,  URI,  DOCUMENT_NAME,  PAGES,  ABSTRACT,  ADVISORY_COMMITTEE,  AERIAL,  ASSESSMENT,  BAIT,  BYCATCH,  CATCH,  CATCH_SUMMARY,  CATCHABILITY,  COLLECTORS,  CORRESPONDANCE, CRIS, CUSK,  DEPTH,  DIVE,  DREDGE,  EFFORT,  ENVIRONMENTAL_CONDITIONS,  FRAMEWORK,  GEAR_SPECIFICATIONS, GULF_ST_LAWRENCE,  HISTORICAL,  IMAGES,  IN_ORACLE,  INDIGENOUS,  INTERVIEW,  JOURNAL,  LARVAE,  LENGTH_FREQ, LIFE_HISTORY, LOBSTER_CL_DB, MANDATORY_LOGBOOK, MANUSCRIPT,  MATURITY,  MINUTES,  MORPHOMETRICS,  MSC, NEWFOUNDLAND, NEWSPAPER, NOTENTERED,  OFFSHORE,  FISHING_POSITIONS, PARTIALLY_ENTERED, POSTER,  PRICE,  PROCEEDINGS,  RAW_DATA,  REVIEW, REPORT, SET_DETAILS_SUMMARY,  SLIP_WEIGHTS,  SOAK_TIME,  SUBSTRATE,  SUCTION,  TAGGING, TEMPERATURE,  THESIS,  TRAP_BASED_SURVEY,  TRAWL,  UPDAT, UNITED_STATES,  VIDEO,  V_NOTCH,  VOLUNTARY_LOGBOOK,  WORKSHOP_SEMINAR)
                              VALUES( '",proj,"' , '",uri,"' , '",docname,"' , '",pagesname,"' , '",abstractname,"' , '",Ad,"' , '",Ar,"' , '",As,"' , '",Ba,"' , '",By,"' , '",Ca,"' , '",Cs,"' , '",Ct,"' , '",Cl,"' , '",Co,"' , '",Cr,"' , '",Cu,"' , '",De,"' , '",Dv,"' , '",Dr,"' , '",Ef,"' , '",En,"' , '",Fr,"' , '",Ge,"' , '",Gu,"' , '",Hi,"' , '",Im,"' , '",In,"' , '",Id,"' , '",It,"' , '",Jo,"' , '",La,"' , '",Le,"' , '",Li,"' , '",Lo,"' , '",Ma,"' , '",Mu,"' , '",Mt,"' , '",Mi,"' , '",Mo,"' , '",Ms,"' , '",Nf,"' , '",Ne,"' , '",No,"' , '",Of,"' , '",Fi,"' , '",Pa,"' , '",Po,"' , '",Pr,"' , '",Pc,"' , '",Ra,"' , '",Re,"' , '",Rp,"' , '",Se,"' , '",Sl,"' , '",So,"' , '",Su,"' , '",Sc,"' , '",Tag,"' , '",Te,"' , '",Ts,"' , '",Tr,"' , '",Ta,"' , '",Up,"' , '",Us,"' , '",Vi,"' , '",Vn,"' , '",Vo,"' , '",Wo,"')", sep = "")

  rs = ROracle::dbSendQuery(con, wri)
  if(ROracle::dbGetInfo(rs, what = "rowsAffected") == 1){
    out = paste(out,"\n   LOBSTERARCHIVE done \n\n", sep = "")
  }
  else{
    out =  paste("\nError: " ,ROracle::writeerror , "\n" , rs, "\n", sep = "")
    ROracle::dbDisconnect(con)
    return(out)

  }
  ROracle::dbCommit(con)

  ROracle::dbDisconnect(con)

  return(out)

}


#' @title  r.read
#' @description Function that read archived data and returns files
#' @import ROracle DBI jsonlite opencpu
#' @return list of uri's to webpage
#' @export
r.read = function(proj, years, firstnames, lastnames, lfas, districts, sdistricts, communities, portcodes, codeports, provinces, docname, abstractname, pagesname, speciesnames, speciescodes, Ad, Ar, As, Ba, By, Ca, Cs, Ct, Cl, Co, Cr, Cu, De, Dv, Dr, Ef, En, Fr, Ge, Gu, Hi, Im, In, Id, It, Jo, La, Le, Li, Lo, Ma, Mu, Mt, Mi, Mo, Ms, Nf, Ne, No, Of, Fi, Pa, Po, Pr, Pc, Ra, Re, Rp, Se, Sl, So, Su, Sc, Tag, Te, Ts, Tr, Ta, Up, Us, Vi, Vn, Vo, Wo, strict){

  drv = DBI::dbDriver("Oracle")
  con = ROracle::dbConnect(drv, username = oracle.username, password = oracle.password, dbname = oracle.server)

  nameframe = NULL
  if(firstnames != "" || lastnames != ""){
    fnames = gsub(" ", "", unlist(strsplit(firstnames, ",")))
    lnames = gsub(" ", "", unlist(strsplit(lastnames, ",")))

    if(strict == "OR"){
      query = "Select * from LOBSTER.LOBSTERARCHIVE_NAME where "
      for(i in 1:length(fnames)){

        joiner = "' AND "
        if(fnames[i] == "" || lnames[i] == "")
          joiner = "' OR "

        if(i == length(fnames)){
          query = paste(query, "FIRST = '", fnames[i], joiner, "LAST = '", lnames[i], "' ", sep = "")
        }else{
          query = paste(query, "FIRST = '", fnames[i], joiner, "LAST = '", lnames[i], "' ",strict," ", sep = "")
        }
      }
      nameframe = ROracle::dbSendQuery(con, query)
      nameframe = ROracle::fetch(nameframe)
    }else{

      for(i in 1:length(fnames)){
        tnf = NULL
        query = "Select * from LOBSTER.LOBSTERARCHIVE_NAME where "
        query = paste(query, "FIRST = '", fnames[i], "' AND ", "LAST = '", lnames[i], "' ", sep = "")

        tnf = ROracle::dbSendQuery(con, query)
        tnf = ROracle::fetch(tnf)
        if(is.null(nameframe)){
          nameframe = tnf
        }
        else{
          tomove = which(tnf$URI %in% nameframe$URI)
          tokeep = which(nameframe$URI %in% tnf$URI)
          nameframe = nameframe[tokeep,]
          nameframe = rbind(nameframe, tnf[tomove,])
        }
      }
    }
    if(nrow(nameframe)==0)nameframe = NULL
  }


  yearframe = NULL
  if(is.character(years) && years != ""){
    yrs = gsub(" ", "", unlist(strsplit(years, ",")))
    if(strict == "OR"){
      query = "Select * from LOBSTER.LOBSTERARCHIVE_YEAR where "
      for(i in 1:length(yrs)){
        if(i == length(yrs)){
          query = paste(query, "YEAR = '", yrs[i], "' ", sep = "")
        }else{
          query = paste(query, "YEAR = '", yrs[i],"' ",strict," ", sep = "")
        }
      }
      yearframe = ROracle::dbSendQuery(con, query)
      yearframe <- ROracle::fetch(yearframe)
    }else{
      for(i in 1:length(years)){
        tnf = NULL
        query = "Select * from LOBSTER.LOBSTERARCHIVE_YEAR where "
        query = paste(query, "YEAR = '", yrs[i], "' ", sep = "")
        tnf = ROracle::dbSendQuery(con, query)
        tnf = ROracle::fetch(tnf)
        if(is.null(yearframe)){
          yearframe = tnf
        }
        else{
          tomove = which(tnf$URI %in% yearframe$URI)
          tokeep = which(yearframe$URI %in% tnf$URI)
          yearframe = yearframe[tokeep,]
          yearframe = rbind(yearframe, tnf[tomove,])
        }
      }

    }
    if(nrow(yearframe)==0)yearframe = NULL
  }


  lfaframe = NULL
  if(is.character(lfas) && lfas != ""){
    lfa = gsub(" ", "", unlist(strsplit(lfas, ",")))
    district = gsub(" ", "", unlist(strsplit(districts, ",")))
    if(strict == "OR"){

      query = "Select * from LOBSTER.LOBSTERARCHIVE_LFA where "
      for(i in 1:length(lfa)){
        if(i == length(lfa)){
          query = paste(query, "LFA = '", lfa[i], "' AND ", "DISTRICT = '", district[i], "' ", sep = "")
        }else{
          query = paste(query, "LFA = '", lfa[i], "' AND ", "DISTRICT = '", district[i], "' ",strict," ", sep = "")
        }
      }
      lfaframe = ROracle::dbSendQuery(con, query)
      lfaframe = ROracle::fetch(lfaframe)
    }else{
      for(i in 1:length(lfa)){
        tnf = NULL
        query = "Select * from LOBSTER.LOBSTERARCHIVE_LFA where  "
        query = paste(query, "LFA = '", lfa[i], "' AND ", "DISTRICT = '", district[i], "' ", sep = "")
        tnf = ROracle::dbSendQuery(con, query)
        tnf = ROracle::fetch(tnf)
        if(is.null(lfaframe)){
          lfaframe = tnf
        }
        else{
          tomove = which(tnf$URI %in% lfaframe$URI)
          tokeep = which(lfaframe$URI %in% tnf$URI)
          lfaframe = lfaframe[tokeep,]
          lfaframe = rbind(lfaframe, tnf[tomove,])
        }
      }
    }
    if(nrow(lfaframe)==0)lfaframe = NULL
  }


  statframe = NULL
  if(is.character(sdistricts) && sdistricts != ""){
    sdistrict = gsub(" ", "", unlist(strsplit(sdistricts, ",")))
    sdistrict = gsub("'", "''", sdistrict)
    if(strict == "OR"){
      query = "Select * from LOBSTER.LOBSTERARCHIVE_STATDIST where "
      for(i in 1:length(sdistrict)){
        if(i == length(sdistrict)){
          query = paste(query, "STATDIST = '", sdistrict[i], "' ", sep = "")
        }else{
          query = paste(query, "STATDIST = '", sdistrict[i],"' ",strict," ", sep = "")
        }
      }
      statframe = ROracle::dbSendQuery(con, query)
      statframe <- ROracle::fetch(statframe)
    }else{
      for(i in 1:length(sdistrict)){
        tnf = NULL
        query = "Select * from LOBSTER.LOBSTERARCHIVE_STATDIST where  "
        query = paste(query, "STATDIST = '", sdistrict[i], "' ", sep = "")
        tnf = ROracle::dbSendQuery(con, query)
        tnf = ROracle::fetch(tnf)
        if(is.null(statframe)){
          statframe = tnf
        }
        else{
          tomove = which(tnf$URI %in% statframe$URI)
          tokeep = which(statframe$URI %in% tnf$URI)
          statframe = statframe[tokeep,]
          statframe = rbind(statframe, tnf[tomove,])
        }
      }
    }
    if(nrow(statframe)==0)statframe = NULL
  }
  communframe = NULL
  if(is.character(communities) && communities != ""){
    commun = gsub(" ", "", unlist(strsplit(communities, ",")))
    commun = gsub("'", "''", commun)
    if(strict == "OR"){
      query = "Select * from LOBSTER.LOBSTERARCHIVE_COMMUNITY where "
      for(i in 1:length(commun)){
        if(i == length(commun)){
          query = paste(query, "COMMUNITY_CODE = '", commun[i], "' ", sep = "")
        }else{
          query = paste(query, "COMMUNITY_CODE = '", commun[i],"' ",strict," ", sep = "")
        }
      }
      communframe = ROracle::dbSendQuery(con, query)
      communframe = ROracle::fetch(communframe)
    }else{
      for(i in 1:length(commun)){
        tnf = NULL
        query = "Select * from LOBSTER.LOBSTERARCHIVE_COMMUNITY where  "
        query = paste(query, "COMMUNITY_CODE = '", commun[i], "' ", sep = "")
        tnf = ROracle::dbSendQuery(con, query)
        tnf = ROracle::fetch(tnf)
        if(is.null(communframe)){
          communframe = tnf
        }
        else{
          tomove = which(tnf$URI %in% communframe$URI)
          tokeep = which(communframe$URI %in% tnf$URI)
          communframe = communframe[tokeep,]
          communframe = rbind(communframe, tnf[tomove,])
        }
      }
    }
    if(nrow(communframe)==0)communframe = NULL
  }

  portframe = NULL
  if(is.character(portcodes) && portcodes != ""){
    portc = gsub(" ", "", unlist(strsplit(portcodes, ",")))
    codep = gsub(" ", "", unlist(strsplit(codeports, ",")))
    portc = gsub("'", "''", portc)
    codep = gsub("'", "''", codep)
    if(strict == "OR"){


      query = "Select * from LOBSTER.LOBSTERARCHIVE_PORT where "
      for(i in 1:length(portc)){
        if(i == length(portc)){
          query = paste(query, "PORT_NAME = '", portc[i], "' AND ", "PORT_CODE = '", codep[i], "' ", sep = "")
        }else{
          query = paste(query, "PORT_NAME = '", portc[i], "' AND ", "PORT_CODE = '", codep[i], "' ",strict," ", sep = "")
        }
      }
      portframe = ROracle::dbSendQuery(con, query)
      portframe <- ROracle::fetch(portframe)
    }else{
      for(i in 1:length(portc)){
        tnf = NULL
        query = "Select * from LOBSTER.LOBSTERARCHIVE_PORT where  "
        query = paste(query, "PORT_NAME = '", portc[i], "' AND ", "PORT_CODE = '", codep[i], "' ", sep = "")
        tnf = ROracle::dbSendQuery(con, query)
        tnf = ROracle::fetch(tnf)
        if(is.null(portframe)){
          portframe = tnf
        }
        else{
          tomove = which(tnf$URI %in% portframe$URI)
          tokeep = which(portframe$URI %in% tnf$URI)
          portframe = portframe[tokeep,]
          portframe = rbind(portframe, tnf[tomove,])
        }
      }
    }
    if(nrow(portframe)==0)portframe = NULL
  }

  provframe = NULL
  if(is.character(provinces) && provinces != ""){
    prov = gsub(" ", "", unlist(strsplit(provinces, ",")))
    prov = gsub("'", "''", prov)
    if(strict == "OR"){

      query = "Select * from LOBSTER.LOBSTERARCHIVE_PROVINCE where "
      for(i in 1:length(prov)){
        if(i == length(prov)){
          query = paste(query, "PROV = '", prov[i], "' ", sep = "")
        }else{
          query = paste(query, "PROV = '", prov[i],"' ",strict," ", sep = "")
        }
      }
      provframe = ROracle::dbSendQuery(con, query)
      provframe = ROracle::fetch(provframe)
    }else{
      for(i in 1:length(prov)){
        tnf = NULL
        query = "Select * from LOBSTER.LOBSTERARCHIVE_PROVINCE where  "
        query = paste(query, "PROV = '", prov[i], "' ", sep = "")
        tnf = ROracle::dbSendQuery(con, query)
        tnf = ROracle::fetch(tnf)
        if(is.null(provframe)){
          provframe = tnf
        }
        else{
          tomove = which(tnf$URI %in% provframe$URI)
          tokeep = which(provframe$URI %in% tnf$URI)
          provframe = provframe[tokeep,]
          provframe = rbind(provframe, tnf[tomove,])
        }
      }
    }
    if(nrow(provframe)==0)provframe = NULL
  }
  speciesframe = NULL
  if(is.character(speciesnames) && speciesnames != ""){
    specn = gsub(" ", "", unlist(strsplit(speciesnames, ",")))
    specc = gsub(" ", "", unlist(strsplit(speciescodes, ",")))
    specn = gsub("'", "''", specn)
    specc = gsub("'", "''", specc)
    if(strict == "OR"){
      query = "Select * from LOBSTER.LOBSTERARCHIVE_SPECIES where "
      for(i in 1:length(specn)){
        if(i == length(specn)){
          query = paste(query, "SPECIES_NAME = '", specn[i], "' AND ", "SPECIES_CODE = '", specc[i], "' ", sep = "")
        }else{
          query = paste(query, "SPECIES_NAME = '", specn[i], "' AND ", "SPECIES_CODE = '", specc[i], "' ",strict," ", sep = "")
        }
      }
      speciesframe = ROracle::dbSendQuery(con, query)
      speciesframe <- ROracle::fetch(speciesframe)
    }else{
      for(i in 1:length(specn)){
        tnf = NULL
        query = "Select * from LOBSTER.LOBSTERARCHIVE_SPECIES where  "
        query = paste(query, "SPECIES_NAME = '", specn[i], "' AND ", "SPECIES_CODE = '", specc[i], "' ", sep = "")
        tnf = ROracle::dbSendQuery(con, query)
        tnf = ROracle::fetch(tnf)
        if(is.null(speciesframe)){
          speciesframe = tnf
        }
        else{
          tomove = which(tnf$URI %in% speciesframe$URI)
          tokeep = which(speciesframe$URI %in% tnf$URI)
          speciesframe = speciesframe[tokeep,]
          speciesframe = rbind(speciesframe, tnf[tomove,])
        }
      }
    }
    if(nrow(speciesframe)==0)speciesframe = NULL
  }

  optsframe = NULL
  query = "Select * from LOBSTER.LOBSTERARCHIVE where "
  if(is.character(proj)){
    if(proj != ""){
      proj = gsub("'", "''", proj)
      query = paste(query, strict, " PROJECT = '", proj, "' ", sep = "")
    }
  }


  if(is.character(docname)){
    if(docname != ""){
      docname = gsub("'", "''", docname)
      query = paste(query, strict, " DOCUMENT_NAME = '", docname, "' ", sep = "")
    }
  }
  if(is.character(pagesname)){
    if(pagesname != ""){
      pagesname = gsub("'", "''", pagesname)
      query = paste(query, strict, " PAGES = '", pagesname, "' ", sep = "")
    }
  }
  if(is.character(abstractname)){
    if(abstractname != ""){
      abstractname = gsub("'", "''", abstractname)
      query = paste(query, strict, " ABSTRACT = '", abstractname, "' ", sep = "")
    }
  }

  if(Ad == "Y"){
    query = paste(query, strict, " ADVISORY_COMMITTEE = '", Ad, "' ", sep = "")
  }
  if(Ar == "Y"){
    query = paste(query, strict, " AERIAL = '", Ar, "' ", sep = "")
  }
  if(As == "Y"){
    query = paste(query, strict, " ASSESSMENT = '", As, "' ", sep = "")
  }
  if(Ba == "Y"){
    query = paste(query, strict, " BAIT = '", Ba, "' ", sep = "")
  }
  if(By == "Y"){
    query = paste(query, strict, " BYCATCH = '", By, "' ", sep = "")
  }
  if(Ca == "Y"){
    query = paste(query, strict, " CATCH = '", Ca, "' ", sep = "")
  }
  if(Cs == "Y"){
    query = paste(query, strict, " CATCH_SUMMARY = '", Cs, "' ", sep = "")
  }
  if(Ct == "Y"){
    query = paste(query, strict, " CATCHABILITY = '", Ct, "' ", sep = "")
  }
  if(Cl == "Y"){
    query = paste(query, strict, " COLLECTORS = '", Cl, "' ", sep = "")
  }
  if(Co == "Y"){
    query = paste(query, strict, " CORRESPONDANCE = '", Co, "' ", sep = "")
  }
  if(Cr == "Y"){
    query = paste(query, strict, " CRIS = '", Cr, "' ", sep = "")
  }
  if(Cu == "Y"){
    query = paste(query, strict, " CUSK = '", Cu, "' ", sep = "")
  }
  if(De == "Y"){
    query = paste(query, strict, " DEPTH = '", De, "' ", sep = "")
  }
  if(Dv == "Y"){
    query = paste(query, strict, " DIVE = '", Dv, "' ", sep = "")
  }
  if(Dr == "Y"){
    query = paste(query, strict, " DREDGE = '", Dr, "' ", sep = "")
  }
  if(Ef == "Y"){
    query = paste(query, strict, " EFFORT = '", Ef, "' ", sep = "")
  }
  if(En == "Y"){
    query = paste(query, strict, " ENVIRONMENTAL_CONDITIONS = '", En, "' ", sep = "")
  }
  if(Fr == "Y"){
    query = paste(query, strict, " FRAMEWORK = '", Fr, "' ", sep = "")
  }
  if(Ge == "Y"){
    query = paste(query, strict, " GEAR_SPECIFICATIONS = '", Ge, "' ", sep = "")
  }
  if(Gu == "Y"){
    query = paste(query, strict, " GULF_ST_LAWRENCE = '", Gu, "' ", sep = "")
  }
  if(Hi == "Y"){
    query = paste(query, strict, " HISTORICAL = '", Hi, "' ", sep = "")
  }
  if(Im == "Y"){
    query = paste(query, strict, " IMAGES = '", Im, "' ", sep = "")
  }
  if(In == "Y"){
    query = paste(query, strict, " IN_ORACLE = '", In, "' ", sep = "")
  }
  if(Id == "Y"){
    query = paste(query, strict, " INDIGENOUS = '", Id, "' ", sep = "")
  }
  if(It == "Y"){
    query = paste(query, strict, " INTERVIEW = '", It, "' ", sep = "")
  }
  if(Jo == "Y"){
    query = paste(query, strict, " JOURNAL = '", Jo, "' ", sep = "")
  }
  if(La == "Y"){
    query = paste(query, strict, " LARVAE = '", La, "' ", sep = "")
  }
  if(Le == "Y"){
    query = paste(query, strict, " LENGTH_FREQ = '", Le, "' ", sep = "")
  }
  if(Li == "Y"){
    query = paste(query, strict, " LIFE_HISTORY = '", Li, "' ", sep = "")
  }
  if(Lo == "Y"){
    query = paste(query, strict, " LOBSTER_CL_DB = '", Lo, "' ", sep = "")
  }
  if(Ma == "Y"){
    query = paste(query, strict, " MANDATORY_LOGBOOK = '", Ma, "' ", sep = "")
  }
  if(Mu == "Y"){
    query = paste(query, strict, " MANUSCRIPT = '", Mu, "' ", sep = "")
  }
  if(Mt == "Y"){
    query = paste(query, strict, " MATURITY = '", Mt, "' ", sep = "")
  }
  if(Mi == "Y"){
    query = paste(query, strict, " MINUTES = '", Mi, "' ", sep = "")
  }
  if(Mo == "Y"){
    query = paste(query, strict, " MORPHOMETRICS = '", Mo, "' ", sep = "")
  }
  if(Ms == "Y"){
    query = paste(query, strict, " MSC = '", Ms, "' ", sep = "")
  }
  if(Nf == "Y"){
    query = paste(query, strict, " NEWFOUNDLAND = '", Nf, "' ", sep = "")
  }
  if(Ne == "Y"){
    query = paste(query, strict, " NEWSPAPER = '", Ne, "' ", sep = "")
  }
  if(No == "Y"){
    query = paste(query, strict, " NOTENTERED = '", No, "' ", sep = "")
  }
  if(Of == "Y"){
    query = paste(query, strict, " OFFSHORE = '", Of, "' ", sep = "")
  }
  if(Fi == "Y"){
    query = paste(query, strict, " FISHING_POSITIONS = '", Fi, "' ", sep = "")
  }
  if(Pa == "Y"){
    query = paste(query, strict, " PARTIALLY_ENTERED = '", Pa, "' ", sep = "")
  }
  if(Po == "Y"){
    query = paste(query, strict, " POSTER = '", Po, "' ", sep = "")
  }
  if(Pr == "Y"){
    query = paste(query, strict, " PRICE = '", Pr, "' ", sep = "")
  }
  if(Pc == "Y"){
    query = paste(query, strict, " PROCEEDINGS = '", Pc, "' ", sep = "")
  }
  if(Ra == "Y"){
    query = paste(query, strict, " RAW_DATA = '", Ra, "' ", sep = "")
  }
  if(Re == "Y"){
    query = paste(query, strict, " REVIEW = '", Re, "' ", sep = "")
  }
  if(Rp == "Y"){
    query = paste(query, strict, " REPORT = '", Rp, "' ", sep = "")
  }
  if(Se == "Y"){
    query = paste(query, strict, " SET_DETAILS_SUMMARY = '", Se, "' ", sep = "")
  }
  if(Sl == "Y"){
    query = paste(query, strict, " SLIP_WEIGHTS = '", Sl, "' ", sep = "")
  }
  if(So == "Y"){
    query = paste(query, strict, " SOAK_TIME = '", So, "' ", sep = "")
  }
  if(Su == "Y"){
    query = paste(query, strict, " SUBSTRATE = '", Su, "' ", sep = "")
  }
  if(Sc == "Y"){
    query = paste(query, strict, " SUCTION = '", Sc, "' ", sep = "")
  }
  if(Tag == "Y"){
    query = paste(query, strict, " TAGGING = '", Tag, "' ", sep = "")
  }
  if(Te == "Y"){
    query = paste(query, strict, " TEMPERATURE = '", Te, "' ", sep = "")
  }
  if(Ts == "Y"){
    query = paste(query, strict, " THESIS = '", Ts, "' ", sep = "")
  }
  if(Tr == "Y"){
    query = paste(query, strict, " TRAP_BASED_SURVEY = '", Tr, "' ", sep = "")
  }
  if(Ta == "Y"){
    query = paste(query, strict, " TRAWL = '", Ta, "' ", sep = "")
  }
  if(Up == "Y"){
    query = paste(query, strict, " UPDAT = '", Up, "' ", sep = "")
  }
  if(Us == "Y"){
    query = paste(query, strict, " UNITED_STATES = '", Us, "' ", sep = "")
  }
  if(Vi == "Y"){
    query = paste(query, strict, " VIDEO = '", Vi, "' ", sep = "")
  }
  if(Vn == "Y"){
    query = paste(query, strict, " V_NOTCH = '", Vn, "' ", sep = "")
  }
  if(Vo == "Y"){
    query = paste(query, strict, " VOLUNTARY_LOGBOOK = '", Vo, "' ", sep = "")
  }
  if(Wo == "Y"){
    query = paste(query, strict, " WORKSHOP_SEMINAR = '", Wo, "' ", sep = "")
  }

  if(query != "Select * from LOBSTER.LOBSTERARCHIVE where "){
    query = sub(strict, "", query)
    optsframe = ROracle::dbSendQuery(con, query)
    optsframe = ROracle::fetch(optsframe)
  }else{
    optsframe = NULL
  }
  ROracle::dbDisconnect(con)

  returi = NULL
  if(strict == "OR"){

    if(!is.null(nameframe)){
      returi = c(returi, unique(nameframe$URI))
      returi = unique(returi)
    }
    if(!is.null(yearframe)){
      returi = c(returi, unique(yearframe$URI))
      returi = unique(returi)
    }
    if(!is.null(lfaframe)){
      returi = c(returi, unique(lfaframe$URI))
      returi = unique(returi)
    }
    if(!is.null(speciesframe)){
      returi = c(returi, unique(speciesframe$URI))
      returi = unique(returi)
    }
    if(!is.null(provframe)){
      returi = c(returi, unique(provframe$URI))
      returi = unique(returi)
    }
    if(!is.null(statframe)){
      returi = c(returi, unique(statframe$URI))
      returi = unique(returi)
    }
    if(!is.null(portframe)){
      returi = c(returi, unique(portframe$URI))
      returi = unique(returi)
    }
    if(!is.null(communframe)){
      returi = c(returi, unique(communframe$URI))
      returi = unique(returi)
    }
    if(!is.null(optsframe)){
      returi = c(returi, unique(optsframe$URI))
      returi = unique(returi)
    }

  }else{
    if(!is.null(nameframe)){
      if(is.null(returi)){
        returi = c(returi, unique(nameframe$URI))
      }else{
        ind = which(returi %in% unique(nameframe$URI))
        if(length(ind)>0){
          returi =  returi[ind]
        }
        else{
          return("No matches for the selection")
        }
      }
    }
    if(!is.null(yearframe)){
      if(is.null(returi)){
        returi = c(returi, unique(yearframe$URI))
      }else{
        ind = which(returi %in% unique(yearframe$URI))
        if(length(ind)>0){
          returi =  returi[ind]
        }
        else{
          return("No matches for the selection")
        }
      }
    }
    if(!is.null(lfaframe)){
      if(is.null(returi)){
        returi = c(returi, unique(lfaframe$URI))
      }else{
        ind = which(returi %in% unique(lfaframe$URI))
        if(length(ind)>0){
          returi =  returi[ind]
        }
        else{
          return("No matches for the selection")
        }
      }
    }
    if(!is.null(speciesframe)){
      if(is.null(returi)){
        returi = c(returi, unique(speciesframe$URI))
      }else{
        ind = which(returi %in% unique(speciesframe$URI))
        if(length(ind)>0){
          returi =  returi[ind]
        }
        else{
          return("No matches for the selection")
        }
      }
    }
    if(!is.null(provframe)){
      if(is.null(returi)){
        returi = c(returi, unique(provframe$URI))
      }else{
        ind = which(returi %in% unique(provframe$URI))
        if(length(ind)>0){
          returi =  returi[ind]
        }
        else{
          return("No matches for the selection")
        }
      }
    }
    if(!is.null(statframe)){
      if(is.null(returi)){
        returi = c(returi, unique(statframe$URI))
      }else{
        ind = which(returi %in% unique(statframe$URI))
        if(length(ind)>0){
          returi =  returi[ind]
        }
        else{
          return("No matches for the selection")
        }
      }
    }
    if(!is.null(portframe)){
      if(is.null(returi)){
        returi = c(returi, unique(portframe$URI))
      }else{
        ind = which(returi %in% unique(portframe$URI))
        if(length(ind)>0){
          returi =  returi[ind]
        }
        else{
          return("No matches for the selection")
        }
      }
    }
    if(!is.null(communframe)){
      if(is.null(returi)){
        returi = c(returi, unique(communframe$URI))
      }else{
        ind = which(returi %in% unique(communframe$URI))
        if(length(ind)>0){
          returi =  returi[ind]
        }
        else{
          return("No matches for the selection")
        }
      }
    }
    if(!is.null(optsframe)){
      if(is.null(returi)){
        returi = c(returi, unique(optsframe$URI))
      }else{
        ind = which(returi %in% unique(optsframe$URI))
        if(length(ind)>0){
          returi = returi[ind]
        }
        else{
          return("No matches for the selection")
        }
      }
    }
  }

  if(length(returi) == 0)return("No matches for the selection")

  ind  = gsub(",", "%2C", returi)
  ind = gsub("'", "&#39", ind)
  ret = NULL
  ret$files = ind
  return(jsonlite::toJSON(ret))


}




#' @title  autoavailableName
#' @description Function that help autopopulate Names in the html form
#' @import ROracle DBI jsonlite opencpu
#' @export
autoavailableName = function(){

  drv <- DBI::dbDriver("Oracle")
  con <- ROracle::dbConnect(drv, username = oracle.username, password = oracle.password, dbname = oracle.server)

  result = ""
  result = ROracle::dbSendQuery(con, "select * from LOBSTER.LOBSTERARCHIVE_NAME")
  result = ROracle::fetch(result)
  result$uni = paste(result$FIRST,result$LAST,sep="")
  rem = which(result$uni == "NANA")
  if(length(rem) > 0)result = result[-rem,]
  res = NULL
  res$FIRST = result$FIRST[which(!duplicated(result$uni))]
  res$LAST = result$LAST[which(!duplicated(result$uni))]
  res = data.frame(res)
  ROracle::dbDisconnect(con)
  return(jsonlite::toJSON(res))


}

#' @title  autoavailableSpecies
#' @description Function that help autopopulate Species in the html form
#' @import ROracle DBI jsonlite opencpu
#' @param return order ro, common or scientific
#' @export
autoavailableSpecies = function(ro = "common"){

  drv = DBI::dbDriver("Oracle")
  con = ROracle::dbConnect(drv, username = oracle.username, password = oracle.password, dbname = oracle.server)
  result = ROracle::dbSendQuery(con, "select * from LOBSTER.LOBSTERARCHIVE_SPECIES_LOOKUP")
  result = ROracle::fetch(result)

  ROracle::dbDisconnect(con)

  if(ro == "common")
    result$name = paste(result$COMMON, result$SPECIES_CODE, sep=": ")
  if(ro == "code")
    result$name = paste(result$SPECIES_CODE, result$COMMON, sep=": ")
  if(ro == "scientific")
    result$name = paste(result$SPECIES_CODE, result$SCIENTIFIC, sep=": ")
  result$id = 1:nrow(result)
  result$text = result$name

  x = jsonlite::toJSON(result)

  return(x)

}


#' @title  autoavailablemain
#' @description Function that sets master table of area options
#' @import ROracle DBI jsonlite opencpu
#' @export
autoavailablemain = function(){

  drv = DBI::dbDriver("Oracle")
  con = ROracle::dbConnect(drv, username = oracle.username, password =oracle.password, dbname = oracle.server)
  result2 = ROracle::dbSendQuery(con, "select * from LOBSTER.LOBSTERARCHIVE_PORT_LOOKUP")
  result2 = ROracle::fetch(result2)

  result = ROracle::dbSendQuery(con, "select * from LOBSTER.LOBSTERARCHIVE_DISTRICT_LOOKUP")
  result = ROracle::fetch(result)

  ROracle::dbDisconnect(con)

  main_area = merge(result2, result, by = "LFA")


  main_area$pc = main_area$PROV_CODE
  main_area$pc[which(main_area$pc == '1')] = "(NS)"
  main_area$pc[which(main_area$pc == '2')] = "(NB)"

  main_area$fullport = paste(main_area$PORT_NAME, " " ,main_area$pc, ": LFA", main_area$LFA, sep="")
  main_area$fulllfa = paste("LFA", main_area$LFA," ", main_area$PORT_NAME, " " ,main_area$pc,  sep="")

  main_area$portcode = paste(main_area$PORT_NAME, ": " ,main_area$PORT_CODE, sep="")
  main_area$codeport = paste(main_area$PORT_CODE, ": " ,main_area$PORT_NAME, sep="")

  main_area = main_area[order(main_area$PORT_NAME),]
  main_area$id = 1:nrow(main_area)

  x = jsonlite::toJSON(main_area)

  return(x)

}

#' @title  autoavailable
#' @description Function that returns unique values of a column
#' @import ROracle DBI jsonlite opencpu
#' @param column The column to return
#' @export
autoavailable = function(column = ""){


  drv <- DBI::dbDriver("Oracle")
  con <- ROracle::dbConnect(drv, username = oracle.username, password = oracle.password, dbname = oracle.server)
  result = ""
  result = ROracle::dbSendQuery(con, paste("select ",column," from LOBSTER.LobsterArchive", sep=""))
  result = ROracle::fetch(result)
  result = unique(result)
  ROracle::dbDisconnect(con)
  return(jsonlite::toJSON(result))

}


