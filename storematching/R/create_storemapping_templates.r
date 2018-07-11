##' Create storemapping templates
##'
##' create standard data templates for storemapping function
##' @return
##' return 2 CSV files stored in working directory, "clientstore.csv" and "storemaster.csv"
##' @examples
##' #suggest to use it under your working directory
##' setwd("set your working directory here")
##' create_storemapping_templates()
##' @export
##' @rdname create_storemapping_templates
create_storemapping_templates <- function()
{
  clientstore <- data.frame(type = 0,
                            storecode = 0,
                            cityname = 0,
                            storename = 0,
                            brand = 0,
                            addr = 0,
                            lon = 0,
                            lat = 0)

  storemaster <- data.frame(TYPE = 0,
                            SHOPCODE = 0,
                            CITYNAME = 0,
                            PROVINCE = 0,
                            STORENAME = 0,
                            BRAND = 0,
                            ADDR = 0,
                            lon = 0,
                            lat = 0)
  if (!file.exists("clientstore.csv"))
  {
    write.csv(clientstore[0,],"clientstore.csv",row.names = FALSE)
  }
  if (!file.exists("storemaster.csv"))
  {
    write.csv(storemaster[0,],"storemaster.csv",row.names = FALSE)
  }
  cat("All templates created, please fill them in for mapping!!!")
}
