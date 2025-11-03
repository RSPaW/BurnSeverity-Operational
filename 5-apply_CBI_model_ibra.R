library(raster)
library(tidyverse)
library(sf)
library(lubridate)
library(doParallel)
library(here)
library(lwgeom)

# code includes heath mapping and unburnt correction, rv 21-03-2022
use.heath <- "y"

if (use.heath == "y"){
  v <- paste0("v", Sys.Date())
}else{
  v <- paste0("nh", Sys.Date())
}
#v <- "v2025-02-03"

#mdir <- "Z:\\DEC\\Prescribed_Bushfire_Outcomes_2018-134\\DATA\\Working\\ts.Model\\fireSeverity\\models"
model.date <- "2021-09-29"

mtx <- c(-Inf, 0.3, 2,  
         0.3, 1.3, 2,  
         1.3, 2.1, 3, 
         2.1, 2.7, 4, 
         2.7, Inf, 5)
rclmat <- matrix(mtx, ncol=3, byrow=TRUE)
dir2 <- "Z:\\DEC\\Prescribed_Bushfire_Outcomes_2018-134\\DATA\\Working\\Operational\\xModels"
maxVals <- readRDS(here(dir2, "maxVals.sentinel"))
lshp <- list.files(here("inputs\\shpByBurn"), pattern = ".shp$", full.names = TRUE)
shp <- st_read(lshp[1])
shp <- shp[0,]
i <- 10
for (i in 1:length(lshp)){
  shp.tmp <- st_read(lshp[i], quiet = TRUE) %>%
    st_make_valid() %>%
    group_by(BURNID) %>%
    summarise()%>%
    st_cast("MULTIPOLYGON")%>%
    st_transform(crs = crs(shp))
  
  shp <- rbind(shp, shp.tmp)
}

mlist <- as.data.frame(list.files(here(dir2)), stringsAsFactors = FALSE)
colnames(mlist) <- "models"
mlist <- mlist %>% mutate(type = str_split_fixed(models, "\\.", 7)[,1],
                zone = str_split_fixed(models, "\\.", 7)[,2], 
                name = str_split_fixed(models, "\\.", 7)[,5],
                date = str_split_fixed(models, "\\.", 7)[,7], 
                index = str_split_fixed(models, "\\.", 7)[,6]) 

#ibra <- st_read(here("models\\IBRA_wa.shp"), quiet = TRUE)

heath.list <- sort(list.files("Z:\\DEC\\Prescribed_Bushfire_Outcomes_2018-134\\DATA\\Working\\heath\\predict\\heath_shp", 
                              pattern = "predicted.*.shp$", full.names = TRUE), 
                   decreasing = TRUE)

heath.list2 <- list.files(here(dir2, "heath"), pattern = "shp$")
heath <- st_read(heath.list[1], quiet = TRUE) %>%
  st_transform(crs(shp))
heath <- heath[,0]
i <- 1
for (i in 1:length(heath.list2)){
  heath.i <- st_read(here(dir2, "heath", heath.list2[i]), quiet = TRUE) %>%
    st_transform(crs(shp))
  heath.i <- heath.i[,0]
  heath <- rbind(heath, heath.i)
}
#plot(heath[,1])


if (dir.exists(here("models", "prevTifs")) ){
  ptifs <- list.files(here("models", "prevTifs"), pattern = ".tif", full.names = TRUE)
}else{
  ptifs <- NA
}

df.regions <- data.frame(region = c("SouthWest", "SouthWest","Swan", "Swan","Warren","Warren"), 
                         district = c("BWD", "WTN", "SWC", "PHS", "FRK", "DON"))

tlist <- as.data.frame(list.files(here(), pattern = "dNBR.tiff$", recursive = TRUE))
colnames(tlist) <- "loc"
tlist <- tlist %>% mutate(tif = str_split_fixed(loc, "dNBR/", 2)[,2]) %>%
  mutate(BURNID = str_split_fixed(tif, "_", 2)[,1])

burns.f <- list.dirs(here("all_rgbs"), recursive = FALSE, full.names = FALSE)
burns.f <- burns.f[str_detect(burns.f, "rgb_")]
burns <- str_split_fixed(burns.f, "_", 2)[,2]
#burns <- "PHS252"

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
  library(RColorBrewer)
  library(colorspace)
  library(segmented)
  library(here)
  library(fasterize)
  library(lwgeom)
  #for (i in 1:length(burns)){
  
  bdr.name <- burns[i]
  
  ply <- filter(shp, shp$BURNID == burns[i])

  ibra.df <- readRDS(here("models", "ibras", bdr.name))
  burn.ibra <- ibra.df$ibra[1]
  
  if (use.heath == "y"){
  heath.i <- heath[unique(unlist(st_intersects( ply, heath))),]
  }else{
    heath.i <- data.frame()
  }
  
  if (burn.ibra == "Dandarragan Plateau"){burn.ibra <- "Dandaragan Plateau"}
  
  b <- filter(tlist, BURNID == burns[i])
  if (nrow(b)!=0){
  region <- df.regions$region[which(df.regions$district == str_sub(burns[i], end = 3))]
  
  rst <- raster(here::here("dNBR", b$tif[1]))
  plot(rst)
  rst <- mask(crop(rst, ply), ply)
  
  mx.dNBR <- maxVals$dNBR[which(maxVals$ibra == burn.ibra)]
  
  rst[rst >= mx.dNBR] <- mx.dNBR
  plot(rst)
  
  ibra.model <- filter(mlist, name == burn.ibra, type == "qd")

  dir.create(here::here(v), showWarnings = FALSE)
  dir.create(here::here(v, "severity_geoTifs"), showWarnings = FALSE)
  dir.create(here::here(v, "ozcbi_geoTifs"), showWarnings = FALSE)
  dir.create(here::here(v, "severity_maps"), showWarnings = FALSE)
  dir.create(here::here(v, "severity_stats"), showWarnings = FALSE)
  
  m <- 1
  #for (m in 1:nrow(ibra.model)){
    mod <- readRDS(paste0(here(dir2), "\\", ibra.model$models[m]))
    mod.index <- str_split_fixed(ibra.model$models[m], "\\.", 7)[6]
    
    names(rst)[1] <- "index"
    rst.p <- predict(rst, mod)
    plot(rst.p)
    writeRaster(rst.p, here::here(v, "ozcbi_geoTifs", paste0("OzCBI_", burns[i], "_", str_replace(v, "v", ""), ".tif")), overwrite=TRUE)
    
    
    rst.p <- reclassify(rst.p, rclmat)

    ############# add heath
    if (nrow(heath.i)!=0){
      heath.i$n <- 1
      rst.h <- fasterize(heath.i, rst.p, field = "n")
      rst.nh <- rst.h 
      rst.nh[is.na(rst.nh)] <- 2
      rst.nh[rst.nh==1] <- NA
      rst.nh[rst.nh==2] <- 1
      plot(rst.nh)
      
      rst.hp <- rst.h * rst.p
      rst.hp[rst.hp>1] <- 6
      #rst.hp[rst.hp==1] <- NA
      plot(rst.hp)
      
      rst.nhp <- rst.nh * rst.p
      rst.p <- cover(rst.nhp, rst.hp)
      plot(rst.p)
    }

  
      
      unburnt.rst <- raster(here( v, "actual_burnt", paste0(bdr.name, "_unburnt.tif"))) %>%
        crop(ply)

      rst.p <- raster::cover(unburnt.rst, crop(rst.p, ply))
      rst.p <- raster::mask(rst.p, mask = ply)
      plot(rst.p)
      
      # prev.tif.name <- ptifs[str_detect(ptifs, bdr.name)]
      # if (length(prev.tif.name) != 0){
      #   prev.tif <- raster::resample(raster(prev.tif.name), rst.p, method = "ngb")
      #   
      #   plot(prev.tif)
      #   stk.tmp <- stack(prev.tif, rst.p)
      #   rst.p <- calc(stk.tmp, fun = max)
      # 
      # }
      
      ### stats
      df.freq <- as.data.frame(freq(rst.p)) %>% na.omit()
      df.freq <- mutate(df.freq, perc1 = round((count/sum(count))*100, 2)) %>%
        dplyr::select(-count)
      
      df.freq <- left_join(data.frame(value = seq(1, 6), perc = 0), df.freq, by = "value") %>%
        mutate(perc = perc + perc1) %>%
        dplyr::select(-perc1)
      df.freq$perc[is.na(df.freq$perc)] <- 0
      df.freq$BURNID <- burns[i]
      df.freq <- spread(df.freq, value, perc)
      
      df.freq$model <- ibra.model$type[m]
      df.freq$index <- mod.index
      df.freq$date <- Sys.Date()
      df.freq$Master_Key <- ply$id[1]
      if (file.exists(here(v,  "severity_stats", paste0("\\FireSevStat_",  
                                                        burn.ibra,"_",  burns[i], "_", 
                                                        ibra.model$type[m], "_s2a_",
                                                        mod.index, ".csv")))){
        file.remove(here(v,  "severity_stats", paste0("\\FireSevStat_",  
                                                      burn.ibra,"_",  burns[i], "_", 
                                                      ibra.model$type[m], "_s2a_",
                                                      mod.index, ".csv")))
      }
      write_csv(df.freq, here(v,  "severity_stats", paste0("\\FireSevStat_",  
                                burn.ibra,"_",  burns[i], "_", 
                                ibra.model$type[m], "_s2a_",
                                mod.index, ".csv")))
    
      
      writeRaster(rst.p, here::here(v, "severity_geoTifs", paste0("BurnSeverity_", burns[i], "_", str_replace(v, "v", ""), ".tif")), overwrite=TRUE)
    
      
      all.dates <- read.csv(here("tifs", burns[i], "imgUsed.csv"))
      Image.dates <- paste0("from ", all.dates$date[1], " to ", all.dates$date[nrow(all.dates)])
      ub <- readRDS(here("models", "tmp", paste0("ub_", bdr.name)))
      
      png(here::here(v, "severity_maps", paste0("BurnSeverityMap_", burns[i], "_", str_replace(v, "v", ""), ".png")), width = 550, height = 550)
       plot(rst.p, breaks = c(0, 1, 2, 3, 4, 5, 6), col = c("lightblue", "darkolivegreen2", "yellow", "orange", "red",  "grey70"), 
           main=paste0("Burn severity map for: ", burns[i],"\nCreated: ", Sys.Date(), 
                       ", Imagery: ", all.dates$date[1], " to ", all.dates$date[nrow(all.dates)]), 
           sub = paste0("Buffer difference = ", round(ub$threshold, 3), ", ", Sys.Date(),  "\nDeveloped by Densmore and van Dongen (2021)"))
      dev.off()
      
      freq.df <- as.data.frame(freq(rst.p)) %>% na.omit()
      freq.df <- mutate(freq.df, Percent = (count/sum(freq.df$count))*100)
      
      ggplot(freq.df, aes(value, Percent))+
        geom_col()+
        geom_text(aes(label=round(Percent, digits = 1)), vjust = -0.2)+
        labs(title = paste0("Percent area per severity class: ",burns[i],  "\nCreated: ", Sys.Date()) 
             , x = "Severity class", y = "Percent of burnt area (%)"
        )+
        theme_bw()
      ggsave(here::here(v, "severity_stats", paste0("BurnSeverityGraph_", burns[i], ".jpg")), width = 4, height = 5)
      ### metadata
       
      library(MESS)
      BurnID = burns[i]
      Date = as.character(Sys.Date())
      Index = "NBR"
      Satellite = "Sentinel"
      Heath = if(nrow(heath.i)!=0){"Heath mapping included"}else{"Heath mapping not included"}
      Severity.Classes <- "####### CBI ########"
      Unburnt = paste0("0 to ", rclmat[1,2])
      Low = paste0(rclmat[2,1], " to ", rclmat[2,2])
      Medium = paste0(rclmat[3,1], " to ", rclmat[3,2])
      High = paste0(rclmat[4,1], " to ", rclmat[4,2])
      Very.High = paste0(rclmat[5,1], " to 3")
      Ibra.model = ibra.model[1,1]
      notes = "Developed by Valerie Densmore and Ricky van Dongen, 2021"
      
      all.dates <- as.data.frame(str_split_fixed(list.files(here(paste0("all_rgbs/rgb_", burns[i])), "png" ), "_", n=3)[,2])
      colnames(all.dates)[1] <- "im.dates"
      all.dates$n <- paste0("image-", row.names(all.dates))
      all.dates <- spread(all.dates, n, im.dates)
      
      #Image.dates <- paste0("from ", all.dates[1,1], " to ", all.dates[, ncol(all.dates)])
      
      df <- data.frame(BurnID, Date, Index, Satellite, Image.dates, Heath, 
                       Severity.Classes, Unburnt, Low, Medium, High, Very.High,
                       Ibra.model, notes)
      df <- bind_cols(df, all.dates)
      
      write.xml(df, here(v, "severity_geoTifs", paste0("BurnSeverity_", burns[i], "_", str_replace(v, "v", ""), "_metadata.xml")))
      write.xml(df, here(v, "severity_maps", paste0("BurnSeverityMap_", burns[i], "_", str_replace(v, "v", ""), "_metadata.xml")))
      write.xml(df, here(v, "severity_stats", paste0("BurnSeverityGraph_", burns[i], "_", str_replace(v, "v", ""), "_metadata.xml")))
      
  #}
  }
}
stopCluster(cl)
###############################################################

# summary fire stats
#v <- paste0("v", Sys.Date())
df <- lapply(list.files(here(v, "severity_stats"), "FireSevStat", full.names = TRUE), read_csv) %>%
  bind_rows()
if (nrow(df!=0)){
write_csv(df, here(v, "severity_stats", "summaryStats.csv"))
dfg <- gather(df, class, perc, 2:6)
ggplot(dfg, aes(class, perc)) +
  geom_col()+
  labs(y = "Percent of area (%)", x = "Severity class")+
  facet_wrap(.~BURNID)
ggsave(here(v, "severity_stats", "summaryStats.jpg"), width = 5, height = 5)

##############################################################

dir.create(here(v, "treatment_area"), showWarnings = FALSE)
shp.list <- list.files(here(v, "actual_burnt"), pattern = "shp$")
shp.treat <- st_read(here(v, "actual_burnt", shp.list[1]), quiet = TRUE) %>%
  st_drop_geometry()
shp.treat <- shp.treat[0,]
i <- 1
for(i in 1:length(shp.list)){
  shpi <- st_read(here( v, "actual_burnt", shp.list[i]), quiet = TRUE) %>%
    st_drop_geometry()
  shp.treat <- bind_rows(shp.treat, shpi)
}

shp.treat$BURNID <- str_replace(shp.treat$NUMBER, "_", "")
shp.treat <- left_join(shp.treat, dplyr::select(shp, BURNID), by = "BURNID")
shp.treat <- st_sf(shp.treat)

shp.treat$Hectares <-  round(as.numeric(st_area(shp.treat))/10000, 2)
shp.treat$Perimeter <- as.numeric(st_perimeter(shp.treat)/1000)
plot(shp[,1])
shape.name <- str_split_fixed(lshp, "clean_", 2)[,2]
dir.create(here(v, "treatment_area"), showWarnings = FALSE)
f.name <- str_split_fixed(here(), "Operational/", 2)[,2]
st_write(shp.treat, here(v, "treatment_area", paste0(f.name, "_treatment_AFED.shp")), append=FALSE)


shp.map <- left_join(shp.treat, df, by = "BURNID")
#shp.map <- st_cast(shp.map, "POLYGON")

st_write(shp.map, here(v, "treatment_area", paste0(f.name, "_treatment_forMap.shp")) , append=FALSE)
#plot(shp.map[,1])
}


