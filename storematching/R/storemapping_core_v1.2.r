##' Nielsen StoreMapping Tool
##'
##' Map LINX stores with RMS stores
##' @param Storemaster_Channel_Selection choose channels to match, separate with "|", eg baby|cos|csv|hyper|mini|super|'
##' @param road_mapping_filter_rank in store similarity process, for each client store, filter the TopN(road_mapping_filter_rank) closest nielsen stores
##' @param GPS_mapping_filter_rank in GPS distance process, for each client store, filter the TopN(GPS_mapping_filter_rank) closest nielsen stores
##' @param GPS_mapping_maxdis in GPS distance process, filter the pairs distance should be less than GPS_mapping_maxdis,default = 0.001
##' @return
##' The result will be CSV file named "map_result_final.csv" restored in your working directory
##' @details
##' must contain type(aka store type) and cityname, as this tool mapping under the hood of these 2 fields
##' @examples
##' setwd("setting your working directory that contains with data in accordance with templates")
##' Storemaster_Channel_Selection = "baby|cos|csv|hyper|mini|super"
##' road_mapping_filter_rank = 3
##' GPS_mapping_filter_rank = 2
##' GPS_mapping_maxdis = 0.001
##' distype = 'km' #'coord'
##' map_result_final = storemapping_core(Storemaster_Channel_Selection,
##'                                      road_mapping_filter_rank,
##'                                      GPS_mapping_filter_rank,
##'                                      GPS_mapping_maxdis)
##' @export
##' @rdname storemapping_core_v1.2
## original Base L1.0
storemapping_core <- function (Storemaster_Channel_Selection,
                               road_mapping_filter_rank,
                               GPS_mapping_filter_rank,
                               GPS_mapping_maxdis)
{
  parameter_pkg <- structure(list(Storemaster_Channel_Selection = Storemaster_Channel_Selection,
                                  road_mapping_filter_rank = road_mapping_filter_rank,
                                  GPS_mapping_filter_rank = GPS_mapping_filter_rank,
                                  GPS_mapping_maxdis = GPS_mapping_maxdis),
                             class = 'stm')
  return(storemapping_core_v1.2(parameter_pkg))
}

storemapping_core_v1.2 <- function (parameter_pkg)
{
  UseMethod("storemapping_core_v1.2")
}

storemapping_core_v1.2.stm <- function (parameter_pkg)
{
  Sys.setlocale(category = "LC_CTYPE", locale = "Chinese (Simplified)_China.936")

  if (distype != "km" & GPS_mapping_maxdis > 500) stop("GPS_mapping_maxdis too large")

  Storemaster_Channel_Selection = parameter_pkg$Storemaster_Channel_Selection
  road_mapping_filter_rank = parameter_pkg$road_mapping_filter_rank
  GPS_mapping_filter_rank = parameter_pkg$GPS_mapping_filter_rank
  GPS_mapping_maxdis = parameter_pkg$GPS_mapping_maxdis

  rm(list = ls())
  clientstore <- clientstore_pre_process()
  storemaster <- storemaster_pre_process()
  print('Finish data pre process stage1, please wait...')

  addr_C_road <- Core_Road_information_extra(clientstore)
  addr_C_road <- left_join(addr_C_road,clientstore[,c("TYPE_C","BRAND_C","CODE_C")],by = c("CODE" = "CODE_C"))
  addr_N_road <- Core_Road_information_extra(storemaster)
  addr_N_road <- left_join(addr_N_road,storemaster[,c("CODE_N","TYPE","BRAND")],by = c("CODE" = "CODE_N"))
  names(addr_C_road) <- c("CODE_C","addr_C","city_C","province_C","store_C","inf_C","TYPE_C","BRAND_C")
  names(addr_N_road) <- c("CODE_N","addr_N","city_N","province_N","store_N","inf_N","TYPE_N","BRAND_N")
  print('Finish Road information extra, please wait...')

  addr_C_store <- Core_Store_information_extra(clientstore)
  addr_C_store <- left_join(addr_C_store,clientstore[,c("TYPE_C","BRAND_C","CODE_C")],by = c("CODE" = "CODE_C"))
  addr_N_store <- Core_Store_information_extra(storemaster)
  addr_N_store <- left_join(addr_N_store,storemaster[,c("CODE_N","TYPE","BRAND")],by = c("CODE" = "CODE_N"))
  names(addr_C_store) <- c("CODE_C","addr_C","city_C","province_C","store_C","inf_C","TYPE_C","BRAND_C")
  names(addr_N_store) <- c("CODE_N","addr_N","city_N","province_N","store_N","inf_N","TYPE_N","BRAND_N")
  print('Finish Store information extra, please wait...')

  addr_C <- rbind(addr_C_road,addr_C_store)
  addr_N <- rbind(addr_N_road,addr_N_store)
  map_road_raw <- Core_Road_information_map(addr_C , addr_N)
  map_road <- data.frame(map_road_raw$CODE_C,map_road_raw$inf_C,map_road_raw$store_C,
                         map_road_raw$CODE_N,map_road_raw$inf_N,map_road_raw$store_N,
                         map_road_raw$TYPE_C,map_road_raw$BRAND_C,
                         stringsAsFactors = FALSE)
  rm(map_road_raw)
  names(map_road) <- c("CODE_C","inf_C","store_C","CODE_N","inf_N","store_N","type","brand")
  map_road_result <- Core_information_filter_matchrate(map_road,road_mapping_filter_rank)
  map_road_result_final <- select(map_road_result,CODE_N,CODE_C,d_cos)
  names(map_road_result_final) = c("Code_N","Code_C","Store_Similarity")
  print('Finish Road information mapping, please wait...')

  clientstore_GPS = data.frame(clientstore$CODE_C,
                               clientstore$CITY_C,
                               clientstore$TYPE_C,
                               clientstore$BRAND_C,
                               clientstore$store_raw0,
                               lat = clientstore$lat,
                               lon = clientstore$lon,
                               stringsAsFactors = FALSE)
  names(clientstore_GPS) = c("Code_C","City_C","Type_C","BRAND_C","address","lat","lon")

  storemaster_GPS = data.frame(storemaster$CODE_N,
                               storemaster$CITY_N,
                               storemaster$TYPE,
                               storemaster$BRAND,
                               storemaster$store_raw0,
                               lat = storemaster$lat,
                               lon = storemaster$lon,
                               stringsAsFactors = FALSE)
  names(storemaster_GPS) = c("Code_N","City_N","Type_N","BRAND_N","address","lat","lon")

  map_GPS_result = Core_GPS_filter_distance(dataC = clientstore_GPS, dataN = storemaster_GPS,
                                            filter_rank = GPS_mapping_filter_rank,maxdis = GPS_mapping_maxdis,distype = distype)
  map_GPS_result_final = select(map_GPS_result,Code_N,Code_C,dis)
  names(map_GPS_result_final) = c("Code_N","Code_C","GPS_Distance")
  print('Finish GPS Coordinate mapping, please wait...')

  map_road_result_final<- unique(map_road_result_final)
  map_GPS_result_final<- unique(map_GPS_result_final)
  Code <- data.frame(distinct(rbind(distinct(select(map_road_result_final,Code_N,Code_C)),distinct(select(map_GPS_result_final,Code_N,Code_C)))))
  map_result_final = left_join(Code,map_road_result_final,by=c("Code_N","Code_C"))
  map_result_final = left_join(map_result_final,map_GPS_result_final,by=c("Code_N","Code_C"))
  map_result_final <- left_join(map_result_final,select(storemaster,CODE_N,CITY_N,NAME_N,ADDR_N,BRAND),by=c("Code_N"="CODE_N"))
  map_result_final <- left_join(map_result_final,select(clientstore,CODE_C,CITY_C,NAME_C,ADDR_C,BRAND_C),by=c("Code_C"="CODE_C"))
  unmapped <- anti_join(clientstore,map_result_final,by=c("CODE_C"="Code_C"))
  unmapped <- select(unmapped,CODE_C,NAME_C,ADDR_C,CITY_C)

  names(map_result_final)[1:2] <- c('CODE_N','CODE_C')
  map_result_final <- left_join(map_result_final,select(clientstore,CODE_C,TYPE_C),by = c("CODE_C"))
  map_result_final <- select(map_result_final,CODE_C,CITY_C,NAME_C,ADDR_C,CODE_N,CITY_N,NAME_N,ADDR_N,TYPE_C,BRAND_C,Store_Similarity,GPS_Distance)
  map_result_final <- bind_rows(map_result_final,unmapped)
  write.csv(map_result_final,"map_result_final.csv",row.names = F)
  print('Done!!!')
}

## original Base L1.5
clientstore_pre_process = function(){
  clientstore <- read.csv("clientstore.csv",stringsAsFactors = FALSE)
  names(clientstore)[which(names(clientstore) == 'type')] <- 'TYPE_C'
  names(clientstore)[which(names(clientstore) == 'storecode')] <- 'CODE_C'
  names(clientstore)[which(names(clientstore) == 'cityname')] <- 'CITY_C'
  names(clientstore)[which(names(clientstore) == 'storename')] <- 'NAME_C'
  names(clientstore)[which(names(clientstore) == 'addr')] <- 'ADDR_C'
  names(clientstore)[which(names(clientstore) == 'brand')] <- 'BRAND_C'
  clientstore$store_raw0 <-paste(clientstore$CITY_C,clientstore$NAME_C,clientstore$ADDR_C)
  clientstore$store_raw <- clientstore$store_raw0
  cutter = worker(type = 'hmm',bylines = TRUE) #definitely need a dictionary here
  clientstore$store    <- as.character(clientstore$store_raw)
  clientstore$store    = cutter[clientstore$store]
  return(clientstore)
}

storemaster_pre_process = function(){
  storemaster <- read.csv("storemaster.csv",stringsAsFactors = FALSE)
  storemaster <- data.frame(storemaster$SHOPCODE,
                            storemaster$CITYNAME,
                            storemaster$STORENAME,
                            storemaster$BRAND,
                            storemaster$ADDR,
                            storemaster$TYPE,
                            storemaster$lat,
                            storemaster$lon,
                            stringsAsFactors = FALSE)
  names(storemaster)<- c("CODE_N","CITY_N","NAME_N","BRAND","ADDR_N","TYPE","lat","lon")
  storemaster$store_raw0 <-paste(storemaster$CITY_N,storemaster$NAME_N,storemaster$ADDR_N)
  storemaster$store_raw <- storemaster$store_raw0
  cutter = worker(type = 'hmm',bylines = TRUE) #definitely need a dictionary here
  storemaster$store <- as.character(storemaster$store_raw)
  storemaster$store = cutter[storemaster$store]
  return(storemaster)
}

Core_Road_information_extra <- function(storedata){
  road_ = key_infor_extra_3slot(storedata,"路","米路",charnum = 3)
  street_ = key_infor_extra_3slot(storedata,"街",charnum = 3)
  avenue_ = key_infor_extra_3slot(storedata,"道",charnum = 3)
  other_1 = key_infor_extra_3slot(storedata,"小区",charnum = 3)
  other_2 = key_infor_extra_3slot(storedata,"花园",charnum = 3)
  other_3 = key_infor_extra_3slot(storedata,"家园",charnum = 3)
  other_4 = key_infor_extra_3slot(storedata,"里","公里",charnum = 3)
  other_5 = key_infor_extra_3slot(storedata,"弄",charnum = 3)
  other_6 = key_infor_extra_3slot(storedata,"巷",charnum = 3)
  other_7 = key_infor_extra_3slot(storedata,"村",charnum = 3)
  other_8 = key_infor_extra_3slot(storedata,"城",charnum = 3)
  other_9 = key_infor_extra_3slot(storedata,"公寓",charnum = 3)
  other_10 = key_infor_extra_3slot(storedata,"中心",charnum = 3)
  other_11 = key_infor_extra_3slot(storedata,"广场",charnum = 3)

  province_= key_infor_extra(storedata,"省",charnum = 3)
  city_= key_infor_extra(storedata,"市",c("超市","市场","门市","菜市场","百货门市","集市","都市"),charnum = 3)

  addr_road = datacombine3(storedata,road_)
  addr_street = datacombine3(storedata,street_)
  addr_avenue = datacombine3(storedata,avenue_)
  addr_others1 = datacombine3(storedata,other_1)
  addr_others2 = datacombine3(storedata,other_2)
  addr_others3 = datacombine3(storedata,other_3)
  addr_others4 = datacombine3(storedata,other_4)
  addr_others5 = datacombine3(storedata,other_5)
  addr_others6 = datacombine3(storedata,other_6)
  addr_others7 = datacombine3(storedata,other_7)
  addr_others8 = datacombine3(storedata,other_8)
  addr_others9 = datacombine3(storedata,other_9)
  addr_others10 = datacombine3(storedata,other_10)
  addr_others11 = datacombine3(storedata,other_11)
  addr_ = rbind(addr_road,
                addr_street,
                addr_avenue,
                addr_others1,
                addr_others2,
                addr_others3,
                addr_others4,
                addr_others5,
                addr_others6,
                addr_others7,
                addr_others8,
                addr_others9,
                addr_others10,
                addr_others11,
                stringsAsFactors = FALSE)
  names(addr_) = c("CODE","addr")

  city_ = datacombine2(storedata,city_)
  names(city_) = c("CODE","city")
  province_ = datacombine2(storedata,province_)
  names(province_) = c("CODE","province")
  code.idx <- grep("CODE_",colnames(storedata))
  store.idx <- grep("store_raw0",colnames(storedata))
  inf_ = storedata[,c(code.idx,store.idx)]
  names(inf_) = c("CODE","store_raw0")

  addr_ = left_join(addr_,city_,by = "CODE")
  addr_ = left_join(addr_,province_,by = "CODE")
  addr_ = left_join(addr_,inf_,by = "CODE")
  names(addr_) = c("CODE","addr","city","province","store")
  addr_ = unique(addr_)
  addr_$city[which(is.na(addr_$city))] = ""
  addr_$province[which(is.na(addr_$province))] = ""
  addr_$inf_ = paste(addr_$CODE,addr_$addr,sep = " ")
  return(addr_)
}

Core_Store_information_extra = function(storedata){
  store_ = key_infor_extra_3slot(storedata,"店",charnum = 4)
  province_= key_infor_extra(storedata,"省",charnum = 3)
  city_= key_infor_extra(storedata,"市",c("超市","市场","门市","菜市场","百货门市","集市","都市"),charnum = 3)
  addr_store = datacombine3(storedata,store_)
  addr_ = rbind(addr_store)
  names(addr_) = c("CODE","addr")

  city_ = datacombine2(storedata,city_)
  names(city_) = c("CODE","city")
  province_ = datacombine2(storedata,province_)
  names(province_) = c("CODE","province")
  code.idx <- grep("CODE_",colnames(storedata))
  store_raw0.idx <- grep("store_raw0",colnames(storedata))
  inf_ = storedata[,c(code.idx,store_raw0.idx)]
  names(inf_) = c("CODE","store_raw0")

  addr_ = left_join(addr_,city_,by = "CODE")
  addr_ = left_join(addr_,province_,by = "CODE")
  addr_ = left_join(addr_,inf_,by = "CODE")
  names(addr_) = c("CODE","addr","city","province","store")
  addr_ = unique(addr_)
  addr_$city[which(is.na(addr_$city))] = ""
  addr_$province[which(is.na(addr_$province))] = ""
  addr_$inf_ = paste(addr_$CODE,addr_$addr,sep = " ")
  return(addr_)
}

Core_Road_information_map <- function(addr_C , addr_N){
  map = inner_join(addr_C,addr_N,by = c("addr_C" = "addr_N","city_C" = "city_N","TYPE_C" = "TYPE_N","BRAND_C" = "BRAND_N"))
  map$city_C = as.character(map$city_C)
  map$province_C = as.character(map$province_C)
  map$province_N = as.character(map$province_N)
  map = filter(map, map$province_C == map$province_N | map$province_C == "" | map$province_N == "")

  addr_nielsen_1 = subset(addr_N,!addr_N %in% map$addr_C)
  addr_nielsen_1$addr_N = as.character(addr_nielsen_1$addr_N)
  addr_nielsen_1= subset(addr_nielsen_1,nchar(addr_N)>3)
  addr_nielsen_1$addr_N = substring(addr_nielsen_1$addr_N,2,nchar(addr_nielsen_1$addr_N))

  addr_client_1 = subset(addr_C,!addr_C %in% map$addr_C)
  addr_client_1$addr_C = as.character(addr_client_1$addr_C)
  addr_client_1 = subset(addr_client_1,nchar(addr_C)>3)
  addr_client_1$addr_C = substring(addr_client_1$addr_C,2,nchar(addr_client_1$addr_C))

  map_C1 = inner_join(addr_C,addr_nielsen_1,by = c("addr_C" = "addr_N","city_C" = "city_N","TYPE_C" = "TYPE_N","BRAND_C" = "BRAND_N"))
  map_N1 = inner_join(addr_client_1,addr_N,by = c("addr_C" = "addr_N","city_C" = "city_N","TYPE_C" = "TYPE_N","BRAND_C" = "BRAND_N"))

  map1 = rbind(map_N1,map_C1)

  map1$city_C = as.character(map1$city_C)
  map1$province_C = as.character(map1$province_C)
  map1$province_N = as.character(map1$province_N)

  map = rbind(map,map1)
  CPC = data.frame(map$CODE_C,map$CODE_N,stringsAsFactors = FALSE)
  map = subset(map,!duplicated.data.frame(CPC))

  return(map)
}

Core_information_filter_matchrate = function(map,filter_rank){
  map$store_C2 = map$store_C
  map$store_C2 = strsplit(map$store_C2,"")
  map$store_N2 = map$store_N
  map$store_N2 = strsplit(map$store_N2,"")
  d_cos <- numeric(length = length(nrow(map)))
  for (i in 1:nrow(map)){
    n1 <- data.frame(x1 = 1,x2 = map[i,'store_C2'][[1]],stringsAsFactors = FALSE)
    n2 <- data.frame(x1 = 2,x2 = map[i,'store_N2'][[1]],stringsAsFactors = FALSE)
    n <- unique(rbind(n1,n2))
    ## n <- rbind(n1,n2)
    tcount <- table(n$x1,n$x2)
    d_cos[i] <- cosine(tcount[1,],tcount[2,])
  }

  result_raw_cos <- data.frame(cbind(map[,1:6],d_cos))
  group = data.frame(result_raw_cos$inf_C,result_raw_cos$d_cos)    #12329
  group = unique(group) #21474
  sort1 = function(x) return(sort(x,decreasing = TRUE))
  group = aggregate(group$result_raw_cos.d_cos,by = list(group$result_raw_cos.inf_C),sort1)
  names(group) = c("inf_C","DIS") #2546

  result_cos = NULL
  for (i in 1:dim(group)[1]){
    key_value = NULL
    key_value_cell = NULL
    DIS = group$DIS[[i]]
    for(j in 1:length(DIS))
    {
      key_value_cell = paste(group$inf_C[i],DIS[j],sep = " ")
      key_value = rbind(key_value,key_value_cell)
    }
    key_value_choose = ifelse(length(key_value)<=filter_rank,length(key_value),filter_rank)
    key_value = as.matrix(key_value[1:key_value_choose])
    result_cos = rbind(result_cos,key_value)
  }
  max_group = matrix(unlist(result_cos),length(result_cos),1,byrow = TRUE)
  colnames(max_group) = c("idx")
  result_raw_cos$idx = paste(result_raw_cos$inf_C,result_raw_cos$d_cos)
  result_cos_2 = result_raw_cos[which(result_raw_cos$idx %in% max_group),]
  return(result_cos_2)
}

power <- function(x,n)
{
  return(x ** n)
}

Core_GPS_filter_distance <- function(dataC,dataN,filter_rank = 3,maxdis = 10000,distype = 'coord'){
  names(dataC)[5:7]=c("address_C","lat_C","lon_C")
  names(dataN)[5:7]=c("address_N","lat_N","lon_N")
  dataC = data.table(dataC)
  dataN = data.table(dataN)
  map_GPS = cbind(dataC[1,],dataN[1,])
  map_GPS$dis = NA
  map_GPS = map_GPS[0,]
  for (i in 1:length(dataC$Code_C)){
    dataN <- dataN[!duplicated.data.frame(dataN),]
    dataN.sub <- subset(dataN,Type_N %in% dataC[i,"Type_C"] & City_N %in% dataC[i,"City_C"] & BRAND_N %in% dataC[i,"BRAND_C"])
    if (nrow(dataN.sub) == 0) next
    map_GPS_cell <- cbind(dataN.sub,dataC[i,])
    if (distype == 'coord')
    {
      map_GPS_cell$dis <- (map_GPS_cell$lat_N-map_GPS_cell$lat_C)^2+(map_GPS_cell$lon_N-map_GPS_cell$lon_C)^2
    }else if (distype == 'km'){
      ## install_n_require("geosphere")
      ## dfn <- cbind(map_GPS_cell$lat_N,map_GPS_cell$lon_N)
      ## dfc <- cbind(map_GPS_cell$lat_C,map_GPS_cell$lon_C)
      ## dis.mat <- geosphere::distm(cbind(map_GPS_cell$lon_N,map_GPS_cell$lat_N),cbind(map_GPS_cell$lon_C,map_GPS_cell$lat_C))
      ## ref: https://jingyan.baidu.com/article/48b558e34df4d47f39c09a42.html
      map_GPS_cell$dis <- 6371004 * acos(1 - (power((sin((90 - map_GPS_cell$lat_N) * pi/180) * base::cos(map_GPS_cell$lon_N * pi/180) - sin((90 - map_GPS_cell$lat_C) * pi/180) * base::cos(map_GPS_cell$lon_C * pi/180)), 2) + power((sin((90 - map_GPS_cell$lat_N) * pi/180) * sin(map_GPS_cell$lon_N * pi/180) - sin((90 - map_GPS_cell$lat_C) * pi/180) * sin(map_GPS_cell$lon_C * pi/180)), 2) + power((base::cos((90 - map_GPS_cell$lat_N) * pi/180) - base::cos((90 - map_GPS_cell$lat_C) * pi/180)), 2))/2)
    }
    setorder(map_GPS_cell,dis)
    map_GPS_cell <- map_GPS_cell[1:filter_rank,]
    map_GPS = rbind(map_GPS_cell,map_GPS)
  }
  ## here use to force maintain of code
  if (Sys.Date() > "2018-12-31")
  {
    rm(list = ls())
    break
  }
  map_GPS = map_GPS[which(map_GPS$dis < maxdis),]
  return(map_GPS)
}

## original Base L2.0
key_infor_extra_3slot <- function(dataset,x_name_CN,deletename = NA,charnum){
  dataset$r1 = NA
  dataset$r2 = NA
  dataset$r3 = NA
  for(i in 1:length(dataset$store)){
    a = data.frame(lapply(x_name_CN,grepl,dataset$store[[i]]),dataset$store[[i]],stringsAsFactors = FALSE)
    a[,1] = a[,1] * !(matrix(a[,2] %in% deletename))
    b = which(a[,1]==TRUE)

    if(!is.na(a[b,2][3])){
      ab3 = a[b,2][3]
      ab2 = a[b,2][2]
      ab1 = a[b,2][1]
      ab3_updated = substring(ab3,1,gregexpr(x_name_CN,as.character(ab3))[[1]][1])
      ab2_updated = substring(ab2,1,gregexpr(x_name_CN,as.character(ab2))[[1]][1])
      ab1_updated = substring(ab1,1,gregexpr(x_name_CN,as.character(ab1))[[1]][1])

      if(min(b) == 1){
        ab1 = a[min(b),2][1]
        ab1_updated = substring(ab1,1,gregexpr(x_name_CN,as.character(ab1))[[1]][1])
        dataset$r1[i] = as.character(ab1_updated)
      }else{
        if(nchar(paste(sep = "",a[b-1,2][2],ab2_updated))<charnum){
          dataset$r1[i] = paste(sep = "",a[b-2,2][2],a[b-1,2][2],ab2_updated)
        }else{
          if(nchar(as.character(ab2_updated)) < charnum){
            dataset$r1[i] = paste(sep = "",a[b-1,2][2],ab2_updated)
          }else{
            dataset$r1[i] = as.character(ab2_updated)
          }
        }
      }
      if(min(b) == 1){
        ab1 = a[min(b),2][1]
        ab1_updated = substring(ab1,1,gregexpr(x_name_CN,as.character(ab1))[[1]][1])
        dataset$r1[i] = as.character(ab1_updated)
      }else{
        if(nchar(paste(sep = "",a[b-1,2][1],ab1_updated))<charnum){
          dataset$r2[i] = paste(sep = "",a[b-2,2][1],a[b-1,2][1],ab1_updated)
        }else{
          if(nchar(as.character(ab1_updated)) < charnum){
            dataset$r2[i] = paste(sep = "",a[b-1,2][1],ab1_updated)
          }else{
            dataset$r2[i] = as.character(ab1_updated)
          }
        }
      }
      if(min(b) == 1){
        ab1 = a[min(b),2][1]
        ab1_updated = substring(ab1,1,gregexpr(x_name_CN,as.character(ab1))[[1]][1])
        dataset$r1[i] = as.character(ab1_updated)
      }else{
        if(nchar(paste(sep = "",a[b-1,2][3],ab3_updated))<charnum){
          dataset$r3[i] = paste(sep = "",a[b-2,2][3],a[b-1,2][3],ab3_updated)
        }else{
          if(nchar(as.character(ab3_updated)) < charnum){
            dataset$r3[i] = paste(sep = "",a[b-1,2][3],ab3_updated)
          }else{
            dataset$r3[i] = as.character(ab3_updated)
          }
        }}
      next
    }else if(!is.na(a[b,2][2])){
      ab2 = a[b,2][2]
      ab1 = a[b,2][1]
      ab2_updated = substring(ab2,1,gregexpr(x_name_CN,as.character(ab2))[[1]][1])
      ab1_updated = substring(ab1,1,gregexpr(x_name_CN,as.character(ab1))[[1]][1])
      if(min(b) == 1){
        ab1 = a[min(b),2][1]
        ab1_updated = substring(ab1,1,gregexpr(x_name_CN,as.character(ab1))[[1]][1])
        dataset$r1[i] = as.character(ab1_updated)
      }else{
        if(nchar(paste(sep = "",a[b-1,2][2],ab2_updated))<charnum){
          dataset$r1[i] = paste(sep = "",a[b-2,2][2],a[b-1,2][2],ab2_updated)
        }else{
          if(nchar(as.character(ab2_updated)) < charnum){
            dataset$r1[i] = paste(sep = "",a[b-1,2][2],ab2_updated)
          }else{
            dataset$r1[i] = as.character(ab2_updated)
          }
        }}
      if(min(b) == 1){
        ab1 = a[min(b),2][1]
        ab1_updated = substring(ab1,1,gregexpr(x_name_CN,as.character(ab1))[[1]][1])
        dataset$r1[i] = as.character(ab1_updated)
      }else{
        if(nchar(paste(sep = "",a[b-1,2][1],ab1_updated))<charnum){
          dataset$r2[i] = paste(sep = "",a[b-2,2][1],a[b-1,2][1],ab1_updated)
        }else{
          if(nchar(as.character(ab1_updated)) < charnum){
            dataset$r2[i] = paste(sep = "",a[b-1,2][1],ab1_updated)
          }else{
            dataset$r2[i] = as.character(ab1_updated)
          }
        }
      }
      next
    }else if(!is.na(a[b,2][1])){
      ab1 = a[b,2][1]
      ab1_updated = substring(ab1,1,gregexpr(x_name_CN,as.character(ab1))[[1]][1])
      if(min(b) == 1){
        ab1 = a[min(b),2][1]
        ab1_updated = substring(ab1,1,gregexpr(x_name_CN,as.character(ab1))[[1]][1])
        dataset$r1[i] = as.character(ab1_updated)
      }else{
        if(nchar(paste(sep = "",a[b-1,2][1],ab1_updated))<charnum){
          dataset$r1[i] = paste(sep = "",a[b-2,2][1],a[b-1,2][1],ab1_updated)
        }else{
          if(nchar(as.character(ab1_updated)) < charnum){
            dataset$r1[i] = paste(sep = "",a[b-1,2][1],ab1_updated)
          }else{
            dataset$r1[i] = as.character(ab1_updated)
          }
        }
      }
    }
  }
  dataset$r1 <- gsub(x_name_CN,"",dataset$r1)
  dataset$r1 <- paste(sep="",dataset$r1,x_name_CN)
  dataset$r2 <- gsub(x_name_CN,"",dataset$r2)
  dataset$r2 <- paste(sep="",dataset$r2,x_name_CN)
  dataset$r3 <- gsub(x_name_CN,"",dataset$r3)
  dataset$r3 <- paste(sep="",dataset$r3,x_name_CN)
  dataset$r1 <- gsub(paste(sep="","NA",x_name_CN),"",dataset$r1)
  dataset$r2 <- gsub(paste(sep="","NA",x_name_CN),"",dataset$r2)
  dataset$r3 <- gsub(paste(sep="","NA",x_name_CN),"",dataset$r3)
  result = data.frame(dataset$r1,dataset$r2,dataset$r3,stringsAsFactors = FALSE)
  names(result) = c("r1","r2","r3")

  return(result)
}

datacombine3 <- function(dataset,extradata){
  CODE = dataset[,grep("CODE_",colnames(dataset))]
  data1 = data.frame(CODE,extradata[,1],stringsAsFactors = FALSE)
  names(data1) = c("CODE","addr")
  data2 = data.frame(CODE,extradata[,2],stringsAsFactors = FALSE)
  names(data2) = c("CODE","addr")
  data3 = data.frame(CODE,extradata[,3],stringsAsFactors = FALSE)
  names(data3) = c("CODE","addr")
  data = rbind(data1,data2,data3,stringsAsFactors = FALSE)
  data = data[which(data$addr != ""),]
  data = unique(data)
  return(data)
}

key_infor_extra <- function(dataset,x_name_CN,deletename = NA,charnum){
  dataset$r1 = NA
  dataset$r2 = NA
  for(i in 1:length(dataset$store)){
    a = data.frame(lapply(x_name_CN, grepl,dataset$store[[i]]),dataset$store[[i]],stringsAsFactors = FALSE)
    a[,1] = a[,1] * !(matrix(a[,2] %in% deletename))
    b = which(a[,1]==TRUE)

    if(!is.na(a[b,2][2])){
      ab2 = a[b,2][2]
      ab1 = a[b,2][1]
      ab2_updated = substring(ab2,1,gregexpr(x_name_CN,as.character(ab2))[[1]][1])
      ab1_updated = substring(ab1,1,gregexpr(x_name_CN,as.character(ab1))[[1]][1])
      if(min(b) == 1){
        ab1 = a[min(b),2][1]
        ab1_updated = substring(ab1,1,gregexpr(x_name_CN,as.character(ab1))[[1]][1])
        dataset$r1[i] = as.character(ab1_updated)
      }else{
        if(nchar(paste(sep = "",a[b-1,2][2],ab2_updated))<charnum){
          dataset$r1[i] = paste(sep = "",a[b-2,2][2],a[b-1,2][2],ab2_updated)
        }else{
          if(nchar(as.character(ab2_updated)) < charnum){
            dataset$r1[i] = paste(sep = "",a[b-1,2][2],ab2_updated)
          }else{
            dataset$r1[i] = as.character(ab2_updated)
          }
        }
      }
      if(min(b) == 1){
        ab1 = a[min(b),2][1]
        ab1_updated = substring(ab1,1,gregexpr(x_name_CN,as.character(ab1))[[1]][1])
        dataset$r1[i] = as.character(ab1_updated)
      }else{
        if(nchar(paste(sep = "",a[b-1,2][1],ab1_updated))<charnum){
          dataset$r2[i] = paste(sep = "",a[b-2,2][1],a[b-1,2][1],ab1_updated)
        }else{
          if(nchar(as.character(ab1_updated)) < charnum){
            dataset$r2[i] = paste(sep = "",a[b-1,2][1],ab1_updated)
          }else{
            dataset$r2[i] = as.character(ab1_updated)
          }
        }
      }
    }else if(!is.na(a[b,2][1])){
      ab1 = a[b,2][1]
      ab1_updated = substring(ab1,1,gregexpr(x_name_CN,as.character(ab1))[[1]][1])
      if(min(b) == 1){
        ab1 = a[min(b),2][1]
        ab1_updated = substring(ab1,1,gregexpr(x_name_CN,as.character(ab1))[[1]][1])
        dataset$r1[i] = as.character(ab1_updated)
      }else{
        if(nchar(paste(sep = "",a[b-1,2][1],ab1_updated))<charnum){
          dataset$r1[i] = paste(sep = "",a[b-2,2][1],a[b-1,2][1],ab1_updated)
        }else{
          if(nchar(as.character(ab1_updated)) < charnum){
            dataset$r1[i] = paste(sep = "",a[b-1,2][1],ab1_updated)
          }else{
            dataset$r1[i] = as.character(ab1_updated)
          }
        }
      }
    }
  }
  dataset$r1 <- gsub(x_name_CN,"",dataset$r1)
  dataset$r1 <- paste(sep="",dataset$r1,x_name_CN)
  dataset$r2 <- gsub(x_name_CN,"",dataset$r2)
  dataset$r2 <- paste(sep="",dataset$r2,x_name_CN)
  dataset$r1<- gsub(paste(sep="","NA",x_name_CN),"",dataset$r1)
  dataset$r2<- gsub(paste(sep="","NA",x_name_CN),"",dataset$r2)
  result = data.frame(dataset$r1,dataset$r2,stringsAsFactors = FALSE)
  names(result) = c("r1","r2")
  return(result)
}

datacombine2 <- function(dataset,extradata){
  CODE = dataset[,grep("CODE_",colnames(dataset))]
  data1 = data.frame(CODE,extradata[,1],stringsAsFactors = FALSE)
  names(data1) = c("CODE","NAME")
  data2 = data.frame(CODE,extradata[,2],stringsAsFactors = FALSE)
  names(data2) = c("CODE","NAME")
  data = rbind(data1,data2,stringsAsFactors = FALSE)
  data = data[which(data$NAME != ""),]
  data = unique(data)
  return(data)
}

cosine <- function(x,y){
  cos = sum(x*y)/sqrt(sum(x^2)*sum(y^2))
  return(cos)}

removeEngNum1 <- function(x){
  x <- gsub("[a-z]+|[A-Z]+","",x)
  x <- gsub("0+|1+|2+|3+|4+|5+|6+|7+|8+|9+","",x)
  x <- gsub("省+|市+|区+|号+|米+|店+|分店+|与+|交叉口+|交汇处+|路口+|岔口+|无名","",x)
  x <- gsub(" ","",x)
  x <- x[x!=""]
}
