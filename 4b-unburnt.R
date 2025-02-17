# updated: 3/2/2023
# buffer calculation corrected

library(raster)
library(tidyverse)
library(sf)
library(lubridate)
library(fasterize)
library(lwgeom)
library(here)
library(doParallel)

use.heath <- "y"

if (use.heath == "y"){
  v <- paste0("v", Sys.Date())
}else{
  v <- paste0("nh", Sys.Date())
}
#v <- "v2025-02-03"

lshp <- list.files(here("inputs\\shpByBurn"), pattern = ".shp$", full.names = TRUE)
shp <- st_read(lshp[1])
shp <- shp[0,]
i <- 1
for (i in 1:length(lshp)){
  shp.tmp <- st_read(lshp[i], quiet = TRUE) %>%
    st_make_valid() %>%
    st_cast("MULTIPOLYGON") %>%
    dplyr::select(BURNID) %>%
    st_transform(crs = crs(shp))
 #shp.tmp <- shp.tmp[,-2]
  shp <- rbind(shp, shp.tmp)
}
#shp <- st_read(lshp[1], stringsAsFactors = FALSE, quiet = TRUE)

df.regions <- data.frame(region = c("SouthWest", "SouthWest","Swan", "Swan","Warren","Warren"), 
                         district = c("BWD", "WTN", "SWC", "PHS", "FRK", "DON"))


dates <- read.csv(here::here("inputs", "request_2024-25.csv")) 
#colnames(dates)[1] <- "BURNID"
dates <- dates %>%
  mutate(
    BURNID = str_replace(BURNID, "_", ""),
    start = as.Date(parse_date_time(start, c("ymd", "dmy"))),
    end = as.Date(parse_date_time(end, c("ymd", "dmy")))
  )


shp.tmp <- st_read(here("models\\Template_AFED\\Template_AFED.shp"))
shp.tmp[1,1] <- NA

tlist <- as.data.frame(list.files(here(), pattern = "dNBR.tiff$", recursive = TRUE))
colnames(tlist) <- "loc"
tlist <- tlist %>% mutate(tif = str_split_fixed(loc, "dNBR/", 2)[,2]) %>%
  mutate(BURNID = str_split_fixed(tif, "_", 2)[,1])

burns.f <- list.dirs(here("all_rgbs"), recursive = FALSE, full.names = FALSE)
burns.f <- burns.f[str_detect(burns.f, "rgb_")]
burns <- str_split_fixed(burns.f, "_", 2)[,2]

burns <- unique(tlist$BURNID)
#
#burns <- "DON152"


rst.per <- raster(here("models", "perenialVeg", "rem_Woody_veg_2020.tif"))
i <- 1

dir.create(here("models", "tmp"), showWarnings = FALSE)

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
  library(fasterize)
  library(lwgeom)
  library(here)
  
  region <- df.regions$region[which(df.regions$district == str_sub(burns[i], end = 3))]
  
  t.burn <- filter(tlist, BURNID == burns[i])
  if(nrow(t.burn)!=0){
  burn.shp <- filter(shp, BURNID == burns[i])
  burn.shpx <- shp
  burn.shpx <-  dplyr::select(burn.shpx,BURNID)
  
  if (dir.exists(here("notVeg"))){
    noVeg.shps.list <- list.files(here("notVeg"), pattern = "shp$", full.names = TRUE)
    noVeg.shps <- st_read(noVeg.shps.list[1], quiet = TRUE)
    noVeg.shps$BURNID <- "NA"
    burn.shpx <- rbind(burn.shpx, st_transform(dplyr::select(noVeg.shps, BURNID), crs(burn.shpx) )) %>%
      st_cast("MULTIPOLYGON")
  }
  
  burn.shpx$n <- 2
  dnbr <- raster(here( "dNBR", t.burn$tif[1]))
  ply <- st_buffer(burn.shp, dist = 150)
  dnbr <- mask(crop(dnbr, ply), mask = ply)
  
  plot(dnbr)
  burn.rst <- fasterize(burn.shpx, dnbr, field = "n")
  burn.rst[is.na(burn.rst)] <- 1
  burn.rst[burn.rst == 2] <- NA
  plot(burn.rst)
  #burn.rst.buf <- mask(burn.rst, st_buffer(burn.shp, 120))
  #plot(burn.rst.buf)
  dnbr.ub <- dnbr * burn.rst
  
  per.i <- crop(rst.per, st_buffer(st_transform(burn.shp, crs(rst.per)), 150))
  per.i <- projectRaster(per.i, dnbr)
  dnbr.ub <- dnbr.ub * per.i
  plot(dnbr.ub)
 
  burn.rst.buf <- dnbr.ub
  burn.rst.buf[is.na(burn.rst.buf)==FALSE] <- 1
  
  buff.pix.predict <- ((sum(as.numeric(st_area(st_buffer(burn.shp, 150)))) - sum(as.numeric(st_area(burn.shp))))/10000) * 25
  
  if(nrow(freq(burn.rst.buf))==1){
    buff.pix.actual <- 1
  }else{
    buff.pix.actual <- as.numeric(freq(burn.rst.buf)[1,2])
  }
  
  # check if there is more than 5% of the burn perimeter that was used
  # if((buff.pix.actual/buff.pix.predict)>0.01){
  #   q <- quantile(dnbr.ub, probs = seq(0, 1, 0.05))
  #   threshold <- q[20]
  #   if(threshold<0){
  #     threshold <- 0.0
  #   }
  # }else{
  #   threshold <- 0.05
  # }
  
  if((buff.pix.actual/buff.pix.predict)>0.01){
    q <- quantile(dnbr.ub, probs = seq(0, 1, 0.05))
    threshold <- q[20]
    buf.dif <- q[20]
    if (threshold > 0.03){
      threshold <- 0.03
    }
    unburnt <- dnbr 
    unburnt[unburnt>threshold] <- NA
    unburnt[unburnt<=threshold] <- 1
    cat("buffer difference for", burns[i], " = ", buf.dif, "\n")
  }else{
    unburnt <- dnbr 
    threshold <- 0.03
    buf.dif <- 0.03
    unburnt[unburnt>threshold] <- NA
    unburnt[unburnt<=threshold] <- 1
    cat("unburnt threshold for", burns[i], " = 0.05\n")
  }
  
  ub.stats <- data.frame(BURNID = as.character(), threshold = as.numeric())[1, ]
  ub.stats$BURNID[1] <- burns[i]
  ub.stats$threshold[1] <- buf.dif
  saveRDS(ub.stats, here("models", "tmp", paste0("ub_", burns[i])))
  
  dir.create(here("bufferStats"), showWarnings = FALSE)
  dir.create(here("bufferStats", "figures"), showWarnings = FALSE)
  png(here::here("bufferStats", "figures", paste0(burns[i], ".png")), 
      width = 550, height = 550)
  
  plot(dnbr.ub, main = paste0(burns[i], ": ", round(threshold, digits = 2)))
  
  dev.off()
  
  # unburnt <- dnbr 
  # unburnt[unburnt>threshold] <- NA
   plot(unburnt)
  # unburnt[unburnt<=threshold] <- 1
  # cat("unburnt threshold for", burns[i], " = ", threshold, "\n")
  
  
  
  unburnt <- mask(unburnt, mask = burn.shp)
  plot(unburnt)
  
  dir.create(here::here(v), showWarnings = FALSE)
  dir.create(here::here(v, "actual_burnt"), showWarnings = FALSE)
  
  writeRaster(unburnt, here(v, "actual_burnt",paste0(burns[i], "_unburnt.tif") ), overwrite=TRUE)
  if(T){
    burnt <- dnbr
    plot(burnt)
    burnt[burnt>=threshold] <- 1
    burnt[burnt<threshold] <- NA
    burnt <- mask(burnt, mask = burn.shp)
    
    burnt.ply <- st_as_sf(rasterToPolygons(burnt, dissolve = TRUE))[,-1] 
    
    c <-st_crs(burnt.ply)
    if (is.na(c$input)){
      burnt.ply <- st_set_crs(burnt.ply, crs(burnt))
    }
    
    burnt.ply <- st_transform(burnt.ply, crs = crs(shp.tmp))
    
    burnt.ply <- cbind(burnt.ply, st_drop_geometry(shp.tmp))
    
    burnt.ply$NUMBER <- paste0(str_sub(burns[i], end = 3), "_", str_sub(burns[i], start = 4))
    burnt.ply$DISTRICT <- str_sub(burns[i], end = 3)
    burnt.ply$DATE1 <- as.Date(parse_date_time(dates$start[which(dates$BURNID == burns[i])], 
                                               c("ymd", "dmy")))
    burnt.ply$CAPT_METH <- "RS10"
    burnt.ply$AUTHOR <- "automated"
    burnt.ply$Hectares <- round(as.numeric(st_area(burnt.ply))/10000, 2)
    burnt.ply$Perimeter <- as.numeric(st_perimeter(st_transform(burnt.ply, crs = crs(shp)))/1000)
    burnt.ply$YEAR1 <- year(burnt.ply$DATE1)
    burnt.ply$POLY_TYPE <- "Actual Burnt"
    burnt.ply$Master_Key <- burn.shp$id[1]
    
    burnt.ply$SEASON1 <- case_when(yday(burnt.ply$DATE1[1]) <= 79 ~ "SU", 
                                   yday(burnt.ply$DATE1[1]) <= 171 ~ "AU",
                                   yday(burnt.ply$DATE1[1]) <= 263 ~ "WI", 
                                   TRUE ~ "SP")
    
    burnt.ply$FIRE_SEASO <- case_when(month(burnt.ply$DATE1[1]) <= 6 ~ 
                                        paste0(year(burnt.ply$DATE1[1])-1, "/", 
                                               year(burnt.ply$DATE1[1])), 
                                      TRUE ~ paste0(year(burnt.ply$DATE1[1]), "/", 
                                                    year(burnt.ply$DATE1[1])+1))
    
    st_write(burnt.ply, here(v, "actual_burnt",paste0(burns[i], "_burnt_area.shp") ), 
             append=FALSE, quiet = TRUE)
  }
  
   
  #cat(i, "of", length(burns), "\n")
  }
}
stopCluster(cl)


ub <- list.files(here("models", "tmp"), pattern = "ub", full.names = TRUE)
ub.stats <- lapply(ub, readRDS) %>% bind_rows()
ub.stats
ggplot(ub.stats, aes(BURNID, threshold))+
  geom_col()+
  geom_hline(yintercept=0.2, linetype="dashed", color = "red")+
  geom_text(aes(label = round(threshold, 2)), hjust = -0.1)+
  coord_flip()+
  ylim(0, 0.35)+
  theme_bw()
d <- list.files(here("bufferStats"), pattern = "unburnt_dif")
ggsave(here( "bufferStats", paste0("unburnt_dif_",length(d)+1 , ".jpg")))
