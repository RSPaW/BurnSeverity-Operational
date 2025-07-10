######################################################################################################
library(gdalUtilities)
library(raster)
library(tidyverse)
library(sf)
library(lubridate)
library(here)

use.heath <- "y"

dates <- read.csv(here::here("inputs", "burn severity request_2025Jan_June.csv")) 
#colnames(dates)[1] <- "BURNID"
dates <- dates %>% mutate(BURNID = str_replace(BURNID, "_", "")) %>%
  mutate(pageNumber = paste0(BURNID, "_", burnName)) %>%
  dplyr::select(BURNID, burnName, pageNumber, mapText)

dates$pageNumber <- str_replace_all(dates$pageNumber, " ", "")
dates$pageNumber <- str_replace_all(dates$pageNumber, "\\(", "-")
dates$pageNumber <- str_replace_all(dates$pageNumber, "\\)", "")
dates$pageNumber <- str_replace_all(dates$pageNumber, "\\/", "-")

#dates$pageNumber

dir.create(here("maps"), showWarnings = FALSE)

dir2 <- "Z:\\DEC\\Prescribed_Bushfire_Outcomes_2018-134\\DATA\\Working\\Operational\\xModels"
shp <- st_read(here(dir2,"Template_AFED\\Template_AFED.shp"))[0,]
burnt.shp <- shp #dplyr::select(shp, -BURNID)


all.tifs <- NA
df.stat <- data.frame()
i <- 1
# for (i in 1:length(region)){
#   foldr <- here(region[i])
  fold2 <- list.dirs(here(), recursive = FALSE)
  if (use.heath == "y"){
    fold2 <- fold2[str_detect(fold2, "/v2")]
    vs <- sort(ymd(str_split_fixed(fold2, "/v", 2)[,2]))
  }else{
    fold2 <- fold2[str_detect(fold2, "/nh2")]
    vs <- sort(ymd(str_split_fixed(fold2, "/nh", 2)[,2]))
  }
  fold2 <- fold2[str_detect(fold2, as.character(vs[length(vs)]))]
  
  tif.m <- as.data.frame(list.files(here( "multiSeason", "severity_geoTifs"), pattern = "tif$", full.names = TRUE))
  colnames(tif.m) <- "file"
  tif.m <- tif.m %>% mutate(BURNID = str_split_fixed(str_split_fixed(file, "BurnSeverity_", 2)[,2], "_", 2)[,1])
  
  tifs <- as.data.frame(list.files(paste0(fold2, "/severity_geoTifs"), pattern = "tif$", full.names = TRUE))
  colnames(tifs) <- "file"
  tifs <- tifs %>% mutate(BURNID = str_split_fixed(str_split_fixed(file, "BurnSeverity_", 2)[,2], "_", 2)[,1]) %>%
    filter((BURNID %in% tif.m$BURNID) == FALSE)
  

  tifs <- bind_rows(tifs, tif.m)
  
  
  csv <- read.csv(paste0(fold2, "/severity_stats/summaryStats.csv"))
  df.stat <- bind_rows(df.stat, csv)
  shp.name <- list.files(paste0(fold2, "/treatment_area"), pattern = "AFED.shp$", full.names = TRUE) 
  shpi <- st_read(shp.name)%>%
    st_transform(crs = st_crs(shp))
  shp <- rbind(shp, shpi)

  shps <- list.files(paste0(fold2, "/actual_burnt"), pattern = "shp$", full.names = TRUE)
  j <- 1
  for(j in 1:length(shps)){
    #shp.j <- st_read(shps[j], quiet = TRUE) 
   
    
    shp.j <- st_read(shps[j], quiet = TRUE)
    
    shp.m <- st_cast(shp.j, "POLYGON")
    shp.m$area <- as.numeric(st_area(shp.m))
    shp.m <- filter(shp.m, area > 500)
    #plot(shp.m[,1])
    shp.s <- shp.m %>% group_by(NUMBER) %>%
      summarise()%>%
      st_cast("MULTIPOLYGON")
    shp.s <- left_join(shp.s, st_drop_geometry(shp.j[1,]),  by = "NUMBER") 
    shp.s <-  st_transform(shp.s, crs = st_crs(burnt.shp))
    
     burnt.shp <- rbind(burnt.shp, shp.s)
  }
  
plot(shp[,2])
plot(burnt.shp[,2])



shpj <- shp %>% left_join(df.stat, by = "BURNID") %>%
  left_join(dates, by = "BURNID" ) 

df.txt <- shpj %>% st_drop_geometry() %>%
  dplyr::select(BURNID, burnName, pageNumber, mapText)

#shp.m <- st_read(list.files(here("multiSeason", "treatment_area"), full.names = TRUE, pattern = "shp$") )

shpf <- shpj %>% dplyr::select(BURNID, X1, X2, X3, X4, X5, X6, date) %>%
  filter((BURNID %in% shp.m$BURNID) == FALSE) %>%
  #rbind(st_transform(shp.m, crs = crs(shp))) %>%
  left_join(df.txt, by = "BURNID") %>%
  mutate(date = ymd(date))

folder.name <- str_split_fixed(here(), "Operational/", 2)[,2]
#shpf$imLast <- NA

csvs <- list.files(here("tifs"), pattern = "imgUsed.csv", recursive = TRUE, full.names = TRUE)

imgUsed <- read.csv(csvs[1]) %>%
  dplyr::select(file, date, type)
for(i in 2:length(csvs)){
  imgUsed.i <- read.csv(csvs[i]) %>%
    dplyr::select(file, date, type)
  imgUsed <- bind_rows(imgUsed, imgUsed.i)
}
imgUsed <- imgUsed %>% mutate(BURNID = str_split_fixed(file, "_", 4)[,1]) %>%
  group_by(BURNID) %>%
  summarise(date = max(ymd(date))) %>%
  rename(imLast = date)

shpf <- left_join(shpf, imgUsed, by = "BURNID")
#for (i in 1:length(region)){
  # rgb.f <- list.dirs(here("all_rgbs"), recursive = FALSE)[-1]
  # rgb.f <- rgb.f[str_detect(rgb.f, "_InvestigateFurther") == FALSE]
  # rgb.f <- rgb.f[str_detect(rgb.f, "_NoFire") == FALSE]
  # if (length(rgb.f)!=0){
  #   for (j in 1:length(rgb.f)){
  #     pngs <- list.files(rgb.f[j], pattern = "png$", recursive = TRUE)
  #     pngs <- str_replace(pngs, "remove/", "")
  #     mxdate <- max(ymd(str_split_fixed(pngs, "_", 4)[,2]))+1
  #     id <- unique(str_split_fixed(pngs, "_", 4)[,1])
  #     shpf$imLast[which(shpf$BURNID == id)] <- as.character(mxdate)
  #   }
  # }
#}
dir.create(here("burntArea"), showWarnings = FALSE)
st_write(burnt.shp, here("burntArea", paste0(folder.name, "_burntArea_", Sys.Date(), ".shp")), append=FALSE)

shpf <- st_make_valid(shpf)
shpf$Hectares <- round(as.numeric(st_area(shpf))/10000, 0)
st_write(shpf, here("maps", paste0(folder.name, "_mapOutput_", Sys.Date(), ".shp")), append=FALSE)

#all.tifs <- na.omit(tifs)

## check projection

rst.tmp <- raster(tifs$file[1])
for(i in 1:length(tifs$file)){
  rst.i <- raster(tifs$file[i]) %>%
    projectRaster(crs = crs(rst.tmp), method = "ngb")
  writeRaster(rst.i, tifs$file[i], overwrite=TRUE)
}

tif.vrt <-gdalbuildvrt(gdalfile = tifs$file, output.vrt = here("maps", "output.vrt"))

ouput.rst<-round(raster::raster(tif.vrt), 0)

if (use.heath == "y"){
 writeRaster(ouput.rst, here("maps", paste0(folder.name, "_mosaic_", Sys.Date(), ".tif")), format='GTiff', overwrite=TRUE)
  }else{
  writeRaster(ouput.rst, here("maps", paste0(folder.name, "_noHeath_", Sys.Date(), ".tif")), format='GTiff', overwrite=TRUE)
}
