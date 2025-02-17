library(raster)
library(tidyverse)
library(sf)
library(lubridate)
library(here)
library(doParallel)

lshp <- list.files(here("inputs\\shpByBurn"), pattern = ".shp$", full.names = TRUE)
#shp <- st_read(lshp[1], stringsAsFactors = FALSE, quiet = TRUE)

df.regions <- data.frame(region = c("SouthWest", "SouthWest","Swan", "Swan","Warren","Warren"), 
                         district = c("BWD", "WTN", "SWC", "PHS", "FRK", "DON"))

dates <- read.csv(here::here("inputs", "burn severity request_2024-25.csv")) 
#colnames(dates)[1] <- "BURNID"
dates <- dates %>%
  mutate(
    BURNID = str_replace(BURNID, "_", ""),
    start = as.Date(parse_date_time(start, c("ymd", "dmy"))),
    end = as.Date(parse_date_time(end, c("ymd", "dmy")))
  )
write_csv(dplyr::select(dates, BURNID, start, end), here("inputs", "clean_dates.csv"))

burns.f <- list.dirs(here("all_rgbs"), recursive = FALSE, full.names = FALSE)
burns.f <- burns.f[str_detect(burns.f, "rgb_")]
burns <- str_split_fixed(burns.f, "_", 2)[,2]
burns <- unique(dates$BURNID)
#burns <- "DON152"

i <- 1
#Define how many cores (memory is limiting factor here)
UseCores <- 10
#Register CoreCluster
cl <- makeCluster(UseCores)
registerDoParallel(cl)
foreach(i = 1:length(burns)) %dopar% {
  library(raster)
  library(tidyverse)
  library(sf)
  library(lubridate)
  library(here)
  
  #ply <- filter(shp, BURNID == burns[i])
  shp.name <- lshp[str_detect(lshp, burns[i])]
  ply <- st_read(shp.name)
  ply <- st_buffer(ply, dist = 120)
  
  date <- filter(dates, BURNID == burns[i])
  
  #fstart <- ymd("2021-09-15")
  fstart <- parse_date_time(date$start[1], c("ymd", "dmy"))
  if (month(parse_date_time(date$end[1], c("ymd", "dmy"))) %in% c(4:7)){
    fend <- parse_date_time(date$end[1], c("ymd", "dmy")) + 30
  }else{
     fend <- parse_date_time(date$end[1], c("ymd", "dmy"))
  }
  
 
  #fend <- ymd("2022-09-15")
  
  region <- df.regions$region[which(df.regions$district == str_sub(burns[i], end = 3))]
  
  plist <- as.data.frame(list.files(here("all_rgbs", paste0("rgb_",burns[i])), "png"))
  colnames(plist)[1] <- "file" 
  
  if(nrow(plist)!=0){
  
  plist <- mutate(plist, date = ymd(str_split_fixed(file, "_", 3)[,2]))
  date.list <- plist$date
  
  tlist <- as.data.frame(list.files(here("tifs", burns[i]), pattern = "tif$"))
  colnames(tlist)[1] <- "file" 
  tlist <- tlist %>% mutate(date = ymd(str_split_fixed(file, "_", 3)[,2])) %>%
    filter(date %in% date.list)
  
  #pre image nbr
  preDates <- filter(tlist, date < fstart)
  if (nrow(preDates)==0){
    preIm <- raster(here( "tifs", burns[i], tlist$file[1])) 
    cat(burns[i], "has no pre burn image\n")
  }else{
    preDate <- preDates[nrow(preDates),]
    preIm <- raster(here("tifs", burns[i], preDate$file[1]))
    preIm[preIm == 0] <- NA
    preDate$type <- "pre"
    imUsed <- preDate
  }
  plot(preIm)
  
  preIm10 <- crop(preIm, ply)
  plot(preIm10)
  
  #post image nbr
  postDates <- filter(tlist, date > fstart & date <= fend)
  postDates.1 <- filter(tlist, date > fend)
  postDates <- bind_rows( postDates,  postDates.1[1,]) %>%
    na.omit()
  
  # if (nrow(postDates) == 0){
  #   postDates <- filter(tlist, date > fstart)
  #   postDate1 <- postDates[1,]
  # }
  
  
  if (nrow(postDates) != 0){
    
    #im <- paste0(here("tifs", burns[i], postDate$file[1]))
    ims <- stack(here("tifs", burns[i], postDates$file))
    plot(ims)
    postIm20 <- crop(ims, ply)
  
    postNBRmin <- calc(postIm20, min)
    postNBRmin[postNBRmin == 0] <- NA
    plot(postNBRmin)
 
    dNBRmax <- preIm10 - postNBRmin
  plot(dNBRmax, main = burns[i])

  names(dNBRmax) <- "Index"
  #dNBRmax[dNBRmax > 1] <- 0
  #dNBRmax[dNBRmax < -1] <- 0
  
  dir.create(here::here( "dNBR"), showWarnings = FALSE)
  writeRaster(dNBRmax, here( "dNBR", paste0(burns[i],"_dNBR.tiff")), overwrite=TRUE)  
  postDates$type <- "post"
  imUsed <- bind_rows(imUsed, postDates) 
  write.csv(imUsed, here( "tifs", burns[i], "imgUsed.csv"))
  #plot(stk)
  }else{
    cat(burns[i], "has no post burn image\n")
  }
}
}
stopCluster(cl)

