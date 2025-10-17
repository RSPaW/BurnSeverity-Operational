# Load libraries
library(sf) # to handle shp files
library(tidyverse) # data manipulation
library(lubridate) # date manipulation
library(here) # reference data locations


#pre.days <- 250 # number of days prior to start date included
post.days <- 200 # number of days following to end date included
yr <- 2024

#change csv name
csvs <- list.files(here::here(), pattern = ".csv")
toCk <-  read.csv(here(csvs)) %>%
  dplyr::select(BURNID, start, end, mapText, nSeason, Comment) %>%
  mutate(start = dmy(start), end = dmy(end), 
         BURNID = str_replace(BURNID, "_", ""))

shp <- st_read("V:\\GIS1-Corporate\\data\\GDB\\Fire\\Burn_data\\CPT_FIRE_ANNUAL_IBP.gdb", 
               layer = "CPT_FIRE_ANNUAL_IBP", quiet = TRUE) %>%
  st_make_valid() %>%
  mutate(BURNID = str_replace(BURNID, "_", ""), 
         DISTRICT = str_sub(BURNID, end = 3)) %>%
  filter(BURNID %in% toCk$BURNID)
plot(shp[,3])

ck.missing <- filter(toCk, (BURNID %in% shp$BURNID) == FALSE)

####################################################################
mga50 <- "+proj=utm +zone=50 +south +datum=WGS84 +units=m +no_defs"
s <- st_transform(shp, mga50)
s <- st_centroid(s)

s$x <- as.character(round(st_coordinates(s)[,1], 0))
s$y <- as.character(round(st_coordinates(s)[,2],0))
s <- mutate(s, id = paste0(str_replace(DISTRICT, " ", ""), "-", yr, "-", str_sub(x, start = -4),  str_sub(y, start = -4))) %>%
  dplyr::select(BURNID, id) %>%
  st_drop_geometry()

shp <- left_join(shp, s, by = "BURNID")

nrow(shp) == nrow(toCk)
plot(shp[,1])
shp.alb <- shp %>% left_join(toCk, by = "BURNID") %>%
  st_transform(crs = "EPSG:3577")

shp.alb$end[is.na(shp.alb$end)] <- Sys.Date()

shp.alb <- mutate(shp.alb, im_strt = floor_date(start, "years"), 
                  im_end = case_when(end + days(post.days) > Sys.Date() ~ floor_date(Sys.Date(), "day"), 
                                     TRUE ~ end + days(post.days)))


###
# see what we have
i <- 1
j <- 4
#for (i in 1:length(regions)){
  rgb.f <- list.dirs(here( "all_rgbs"), recursive = FALSE)[-1]
  rgb.f <- rgb.f[str_detect(rgb.f, "_InvestigateFurther") == FALSE]
  rgb.f <- rgb.f[str_detect(rgb.f, "_NoFire") == FALSE]
#  rgb.f <- rgb.f[str_detect(rgb.f, "_Redo") == FALSE]
  if (length(rgb.f)!=0){
    for (j in 1:length(rgb.f)){
      pngs <- list.files(rgb.f[j], pattern = "png$", recursive = TRUE)
      pngs <- str_replace(pngs, "remove/", "")
      mxdate <- max(ymd(str_split_fixed(pngs, "_", 4)[,2]))+1
      id <- unique(str_split_fixed(pngs, "_", 4)[,1])
      shp.alb$im_strt[which(shp.alb$BURNID == id)] <- mxdate
    }
  }
#}
dir.create(here("inputs"), showWarnings = FALSE)
# toCk <- arrange(toCk, BURNID)
#   write.csv(toCk, here("inputs", "request_2024-25.csv"))



v <- length(list.files(here("inputs"), pattern = "shp$"))
shp.albF <- shp.alb %>% mutate(days = floor(difftime( im_end, im_strt, units = "days"))) %>%
  filter(days >= 30 & mapText != "" ) 

#shp.albF <- filter(shp.albF, BURNID %in% c("DON147", "DON157", "FRK107", "DON106"))
st_write(shp.albF, here("inputs", paste0("operational_", Sys.Date(), "_alb.shp")), append=FALSE)

#####################################################################

# create directory for inputs
dir.create(here::here("inputs", "shpByBurn"), showWarnings = FALSE)
i <- 1
for (i in 1:nrow(shp.alb)){
  ply <- dplyr::select(shp.alb[i,], BURNID, id)
  if (file.exists(here::here("inputs", "shpByBurn", paste0(ply$BURNID[1], "_boundry.shp"))) == FALSE){
      st_write(ply, here::here("inputs", "shpByBurn", paste0(ply$BURNID[1], "_boundry.shp")), 
           delete_dsn=TRUE, showWarnings = FALSE, quiet = TRUE)
  }
}

#################################################################
# get ibras
dir <- "Z:\\DEC\\Prescribed_Bushfire_Outcomes_2018-134\\DATA\\Working\\Operational\\xModels"
ibra <- st_read(here(dir,"IBRA_wa.shp"), quiet = TRUE) 
dir.create(here::here("models"), showWarnings = FALSE)
dir.create(here::here("models", "ibras"), showWarnings = FALSE)
library(doParallel)
i <- 1
UseCores <- 10
#Register CoreCluster
cl <- makeCluster(UseCores)
registerDoParallel(cl)
foreach(i = 1:nrow(shp.alb)) %dopar% {
  library(sf) # to handle shp files
  library(tidyverse) # data manipulation
  library(lubridate) # date manipulation
  library(here) # reference data locations
#for(i in 1:nrow(shp.alb)){
  if(file.exists(here::here("models", "ibras", shp.alb$BURNID[i])) == FALSE){
    ply <- shp.alb[i,]
    ply.i <- st_intersection(st_transform(ply, crs = st_crs(ibra)), ibra)
    ply.i <- dplyr::select(ply.i, IWA_SUB_NA)
    ply.i$area <- as.numeric(st_area(ply.i))
    ply.i <- arrange(ply.i, desc(area))
    burn.ibra <- as.data.frame(ply.i$IWA_SUB_NA[1])
    colnames(burn.ibra)[1] <- "ibra"
    burn.ibra$BURNID <- ply$BURNID[1]
    saveRDS(burn.ibra, here::here("models", "ibras", ply$BURNID[1]))
  }
}
stopCluster(cl)

#################################################################
#ONLY RUN when a boundarie needs to be updated 
### update shpByBurn
 # 
 #  shp.new <- st_read(here("inputs", "SeverityRequest_treatment_areas_Sep2025.shp"))
 #  shp.new <- shp.new %>% mutate(BURNID = str_replace(NUMBER, "_", "")) %>%
 #    dplyr::select(BURNID) %>%
 #    st_transform(crs = "EPSG:3577")
 # 
 #  plot(shp.new[,1])
 #  i <- 1
 # for (i in 1:nrow(shp.new)){
 #    ply <- shp.new[i,]
 #    plot(ply)
 #    st_write(ply, here::here("inputs", "shpByBurn", paste0(ply$BURNID[1], "_boundry.shp")),
 #             delete_dsn=TRUE, showWarnings = FALSE, quiet = TRUE)
 # }


