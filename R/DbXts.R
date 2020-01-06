#' dbWriteXts
#'
#' @param conn
#' @param name
#' @param value
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
dbWriteXts = function(conn, name, value, ...){
  checkmate::assertClass(value,"xts")

  df = data.frame(dbxts_index = as.character(index(value)),value)

  dbWriteTable(conn,name, df, ...)

}


#' dbReadXts
#'
#' @param conn
#' @param name
#' @param ...
#'
#' @return
#' @export
#' @import DBI
#' @import xts
#' @importFrom  dplyr select
#'
#' @examples
dbReadXts = function(conn, name, ...){

  df = dbReadTable(conn, name, ...)

  if("dbxts_index" %in% names(df)){
    ret = xts(dplyr::select(df,-dbxts_index), as.Date(df$dbxts_index))
  }else{
    stop(paste0(name," is NOT an xts object."))
  }

  ret

}


