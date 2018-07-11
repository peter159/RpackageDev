##' convert json data to data.frame
##'
##' convert json data to data.frame
##' @param jsonfilename path of json file
##' @return
##' `json2df()` return a data.frame obj
##' @examples
##' json2df("./extensions.json")

##' @export
##' @rdname json2df
json2df <- function(jsonfilename)
{
  json <- fromJSON(jsonfilename,stringsAsFactors = FALSE)
  json <- t(sapply(json,function(x) unlist(x)))
  json <- as.data.frame(json,stringsAsFactors = FALSE)
  return(json)
}
