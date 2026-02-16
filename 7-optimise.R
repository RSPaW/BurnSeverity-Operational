library(raster)
library(tidyverse)
library(sf)
library(lubridate)
library(here)
library(doParallel)

dir2 <- "Z:\\DEC\\Prescribed_Bushfire_Outcomes_2018-134\\DATA\\Working\\Operational\\xModels"
rst.per <- raster(here(dir2,"perenialVeg", "rem_Woody_veg_2020.tif"))
dir.create(here("bufferStats", "allPre"), showWarnings = FALSE)
lshp <- list.files(here("inputs\\shpByBurn\\"), pattern = ".shp$", full.names = FALSE)

alb.shp <- list.files(here("inputs\\"), pattern = ".shp$", full.names = TRUE)
shp <- st_read(alb.shp[1], stringsAsFactors = FALSE, quiet = TRUE)
#crs(shp)
csvs <- list.files(here::here(), pattern = ".csv")
csvs <- csvs[csvs != "allDates.csv"]
dates <- read.csv(here::here(csvs))
#colnames(dates)[1] <- "BURNID"

dates <- dates %>%
  mutate(
    BURNID = str_trim(str_replace(BURNID, "_", "")),
    start = parse_date_time(start, c("ymd", "dmy")),
    end = parse_date_time(end, c("ymd", "dmy"))
  )

# Convert parsed dates to Date class
dates$start <- as.Date(dates$start)
dates$end <- as.Date(dates$end)

burns <- unique(shp$BURNID)
burns <- str_split_fixed(lshp, "_", 2)[,1]
burns <- "FRK112"

i <- 2
#Define how many cores (memory is limiting factor here)
UseCores <- 5
#Register CoreCluster
cl <- makeCluster(UseCores)
registerDoParallel(cl)
foreach(i = 1:length(burns)) %dopar% {
  library(raster)
  library(tidyverse)
  library(sf)
  library(lubridate)
  library(fasterize)
  library(lwgeom)
  library(here)
  
#for(i in 1:length(burns)){
  df.all <- data.frame()
  #ply <- filter(shp, BURNID == burns[i])
  ply <- st_read(here("inputs", "shpByBurn", paste0(burns[i], "_boundry.shp")))
  ply <- st_buffer(ply, dist = 150)
  
  date <- filter(dates, BURNID == burns[i])
  
  #fstart <- ymd("2021-09-15")
  fstart <- parse_date_time(date$start[1], c("ymd", "dmy"))
  fend <- parse_date_time(date$end[1], c("ymd", "dmy"))
  #fend <- ymd("2022-09-15")
  
  
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
      preIm <- raster(here("tifs", burns[i], tlist$file[1])) 
      cat(burns[i], "has no pre burn image\n")
    }#else{
    d <- 1
    for(d in 1:nrow(preDates)){
      preDate <- preDates[d,]
      preIm <- raster(here("tifs", burns[i], preDate$file[1]))
    #}
    plot(preIm)
    
    preIm10 <- mask(crop(preIm, ply), mask = ply)
    plot(preIm10)
    
    #post image nbr
    postDates <- filter(tlist, date > fstart & date <= fend)
    if (nrow(postDates) == 0){
      postDates <- filter(tlist, date > fstart)
      postDate1 <- postDates[1,]
    }
    
    
    if (nrow(postDates) != 0){
      
      #im <- paste0(here("tifs", burns[i], postDate$file[1]))
      ims <- raster(here("tifs", burns[i], postDates$file[1]))
      k <- 2
      if (nrow(postDates)>1){
      for (k in 2:nrow(postDates)){
        ims.k <- raster(here("tifs", burns[i], postDates$file[k]))
        ims.k <- resample(ims.k, ims)
        ims <- stack(ims, ims.k)
      }
      }
      #plot(ims)
      postIm20 <- mask(crop(ims, ply), mask = ply)
      
      postNBRmin <- calc(postIm20, min)
      
      plot(postNBRmin)
      
      dNBRmax <- preIm - postNBRmin
      plot(dNBRmax, main = burns[i])
      
      names(dNBRmax) <- "Index"
      
      ########################################
      # 4b-unburnt code
      
      #t.burn <- filter(tlist, Burn.Id == burns[i])
      burn.shp <- st_read(here("inputs", "shpByBurn", paste0(burns[i], "_boundry.shp")))
      #burn.shp <- ply
      burn.shpx <- burn.shp
      burn.shpx <-  dplyr::select(burn.shpx,BURNID)
      
      if (dir.exists(here("notVeg"))){
        noVeg.shps.list <- list.files(here("notVeg"), pattern = "shp$", full.names = TRUE)
        noVeg.shps <- st_read(noVeg.shps.list[1], quiet = TRUE)
        noVeg.shps$BURNID <- "NA"
        burn.shpx <- rbind(burn.shpx, st_transform(dplyr::select(noVeg.shps, BURNID), crs(burn.shpx) )) %>%
          st_cast("MULTIPOLYGON")
      }
      
      burn.shpx$n <- 2
      dnbr <- dNBRmax #raster(here("dNBR", t.burn$tif[1]))
      plot(dnbr)
      burn.rst <- fasterize(burn.shpx, dnbr, field = "n")
      burn.rst[is.na(burn.rst)] <- 1
      burn.rst[burn.rst == 2] <- NA
      plot(burn.rst)
      burn.rst.buf <- mask(burn.rst, st_buffer(burn.shp, 120))
      plot(burn.rst.buf)
      dnbr.ub <- dnbr * burn.rst.buf
      
      plot(dnbr.ub)
      per.i <- crop(rst.per, st_buffer(st_transform(burn.shp, crs(rst.per)), 30))
      per.i <- projectRaster(per.i, dnbr)
      dnbr.ub <- dnbr.ub * per.i
      
      
      
      
      burn.rst.buf <- dnbr.ub
      burn.rst.buf[is.na(burn.rst.buf)==FALSE] <- 1
      

        q <- quantile(dnbr.ub, probs = seq(0, 1, 0.10))
        df <- as.data.frame(as.numeric(q[10]))
        colnames(df)[1] <- "threshold"
        df$date <- str_split_fixed(preDate$file[1], "_", 3)[,2]
        df$BURNID <- burns[i]
        df.all <- bind_rows(df, df.all)
        cat(as.numeric(q[10]), "\n") 
      }
  }
  }
 df.all$date <- ymd(df.all$date) 
 if (nrow(df.all) !=0){
  ggplot(df.all, aes(date, threshold))+
    geom_point()+
    geom_line()+
      labs(title = paste0(burns[i], ": min = ", df.all$date[which(df.all$threshold == min(df.all$threshold))]))+
    coord_cartesian(ylim = c(0, 0.3))+
    theme_bw() 
  ggsave(here("bufferStats", "allPre", paste0("allPre_", burns[i], ".jpg")), width = 4, height = 3)
 }else{
   ggplot(df.all, aes(date))+
     geom_histogram()+
     #geom_line()+
     labs(title = paste0(burns[i], ": Maybe no post fire image"))+
     coord_cartesian(ylim = c(0, 0.3))+
     theme_bw() 
   ggsave(here("bufferStats", "allPre", paste0("allPre_", burns[i], ".jpg")), width = 3, height = 3)
 }
}
stopCluster(cl)



