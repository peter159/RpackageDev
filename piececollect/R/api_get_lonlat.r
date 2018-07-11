#' Get longitude and latitude
#'
#' This is the basic interface of getting longitude and latitude from web api
#'
#' The output of `api_get_lonlat()` will be a vector of two regarding longitude and latitude
#'
#' @param address address which can be identified by web api
#' @param api one of 'baidu','google','siweituxin'. The names can be abbreviated to any unique substring. Default is "baidu"
#' @param verbose one of TRUE, FALSE
#' @return
#' `api_get_lonlat()` return a two number vector regarding longitude and latitude
#' @examples
#' api_get_lonlat(上海普陀区丽和苑)
#'
#' @export
#' @rdname api_get_lonlat
api_get_lonlat <- function(address,api = c("baidu","google","siweituxin","gaode"),verbose = FALSE){
  api <- match.arg(api)
  if (api == 'baidu'){
    return(.api_get_lonlat_baidu(address,verbose))
  }
}

.api_get_lonlat_baidu <- function(address,verbose){
  ## reference from http://allthingsr.blogspot.de/2012/01/geocode-your-data-using-r-json-and.html

  address <- gsub(" ","%20",address)
  connectStr <- paste0('http://api.map.baidu.com/geocoder/v2/?output=json&ak=ZUnxMO4h5pXWBCDDgIluIMd4R1YMwIZD&address=',address)
  if (verbose) cat("fetching ",connectStr,"\n")
  con <- url(connectStr,method = 'libcurl')
  tryCatch({
    data.json <- fromJSON(paste(readLines(con,warn = FALSE), collapse=""))
    ## Flatten the received JSON
    data.json <- unlist(data.json)
    lng <- data.json["result.location.lng"]
    lat <- data.json["result.location.lat"]
    gcodes <- as.numeric(c(lng, lat))
  },
  error = function(e){
    gcodes <<- c(NA,NA)
  })
  close(con)
  names(gcodes) <- c("lon", "lat")
  return(gcodes)
}
