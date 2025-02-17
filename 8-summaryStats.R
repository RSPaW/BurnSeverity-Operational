library(raster)
library(tidyverse)
library(sf)
library(lubridate)
library(doParallel)
library(here)
library(landscapemetrics)


fold2 <- list.dirs(here(), recursive = FALSE)
vs <- sort(ymd(str_split_fixed(fold2, "/v", 2)[,2]))

fold2 <- fold2[str_detect(fold2, as.character(vs[length(vs)]))]
tifs <- list.files(paste0(fold2, "/severity_geoTifs"), pattern = "tif$", full.names = TRUE)

sev.lab <- data.frame(sev.class = c(1:6), sev.lab = c("UB", "Low", "Med", "High", "v.High", "Burnt.Heath"))

dir.create(here("stats"))

i <- 1
df.all <- data.frame()
for (i in 1:length(tifs)){
  rst <- raster(tifs[i])  
  burn <- str_split_fixed(str_split_fixed(tifs[i], "severity_geoTifs", 2)[,2], "_", 3)[,2]
  plot(rst)  
  pix.size <- res(rst)[1]
  
  rst.ub <- rst
 rst.ub[rst.ub != 1] <- NA
 plot(rst.ub) 
 clm <- clump(rst.ub)
 df.cmp <- as.data.frame(freq(clm)) %>%
   na.omit()
 
 mean.patch <- (mean(df.cmp$count)*pix.size^2)/10000

  df.freq <- as.data.frame(freq(rst)) %>%
    na.omit()
  
  #burnt.pix <- df %>% filter(value >1) 
  
   df <- df.freq  %>% mutate(area.ha = (count*pix.size^2)/10000) %>%
    dplyr::select(value, area.ha) %>%
    rename(sev.class = value)
  
  
  rst.dis <- rst
  rst.dis[is.na(rst.dis)] <- 1  
  rst.dis[rst.dis!=1] <- NA
  ub.p <- lsm_l_para_mn(rst.dis)
  rst.dis <- distance(rst.dis)
  rst.dis[rst.dis == 0] <- NA
  plot(rst.dis)
  
  
  se.ub <- (cellStats(rst.dis, sd)/sqrt(sum(df.freq$count[which(df.freq$value >1)])))*pix.size
  
  m.ub <- cellStats(rst.dis, mean)
  rst.dis.ub <- rst.dis
  
  rst.dis <- rst
  rst.dis[is.na(rst.dis)] <- 1  
  rst.dis[rst.dis>2] <- NA
  rst.dis[rst.dis<=2] <- 1
  lsev.p <- lsm_l_para_mn(rst.dis)
  
  rst.dis <- distance(rst.dis)
  rst.dis[rst.dis == 0] <- NA
  plot(rst.dis)
  
  se.lsev <- (cellStats(rst.dis, sd)/sqrt(sum(df.freq$count[which(df.freq$value >2)])))*pix.size
  
  m.lsev <- cellStats(rst.dis, mean)
  rst.dis.lsev <- rst.dis

  jpeg(file=here("stats", paste0("Burnstats_", burn, ".jpg")), width = 800, height = 800, quality = 100)
  par(mfrow = c(2,2))
  
  plot(rst, breaks = c(0,1,2,3,4,5, 6), col = c("lightblue", "darkolivegreen2", "yellow", "orange", "red",  "grey70"), 
       main = "Severity map" )
  #plot(rst, main = "Severity map")
  plot(rst.dis.ub, main = "Dist to UB", sub = paste0("mean = ", round(m.ub, 0), " m, std.error = ", round(se.ub, 0), " m"))
  plot(rst.dis.lsev, main = "Dist to LowSev", sub = paste0("mean = ", round(m.lsev, 0), " m, std.error = ", round(se.lsev, 0), " m"))
  barplot(df$area.ha ~ df$sev.class, xlab = "Sev class",
        main= paste0("Area burnt = ", round(sum(df$area.ha)), " ha"))
  mtext(paste("Burn Id: ", burn), side = 3, line = -1.5, outer = TRUE)
  
  dev.off()
  dfj <-  df %>% full_join(sev.lab, by = "sev.class") %>%
    dplyr::select(-sev.class) %>%
    spread(sev.lab, area.ha) %>%
    mutate(BURNID = burn, dist.ub = m.ub, dist.ub.se = se.ub, 
           dist.lsev.se = se.lsev, dist.lsev = m.lsev, 
           n.patch = nrow(df.cmp), m.patch = mean.patch, 
           ub.patch = ub.p$value[1], lsev.patch = lsev.p$value[1])
  dfj[is.na(dfj)] <- 0

  df.all <- bind_rows(dfj, df.all)
  
}

saveRDS(df.all, here("stats", "all.stats"))
df.all <- readRDS(here("stats", "all.stats"))

df <- arrange(df.all, dist.ub)
df$BURNID <- factor(df$BURNID, levels = df$BURNID)
ggplot(df, aes(BURNID, dist.ub))+
  geom_col()+
  geom_errorbar(aes(ymin=dist.ub-dist.ub.se, ymax=dist.ub+dist.ub.se), width=.2,
                position=position_dodge(.9)) +
  coord_flip()+
  labs(title = "Spring prescribed burns 2023", 
       subtitle = "Note: burns may not be complete", 
       y = "Mean distance to unburnt (m)", 
       caption = paste0("Created: ", Sys.Date()))+
  theme_bw()
ggsave(here("stats", "allStat_Mean_Dist_to_UB.jpg"), height = 7, width = 5)

df$district <- str_sub(df$BURNID, end = 3)
d <- unique(df$district )

for (i in 1:length(d)){
  dfi <- filter(df, district == d[i])
  ggplot(dfi, aes(BURNID, dist.ub))+
    geom_col()+
    geom_errorbar(aes(ymin=dist.ub-dist.ub.se, ymax=dist.ub+dist.ub.se), width=.2,
                  position=position_dodge(.9)) +
    coord_flip()+
    labs(title = paste0("Spring prescribed burns 2023: ",d[i]), 
         subtitle = "Note: burns may not be complete", 
         y = "Mean distance to unburnt (m)", 
         caption = paste0("Created: ", Sys.Date()))+
    theme_bw()
  ggsave(here("stats", paste0("allStat_Mean_Dist_to_UB_", d[i], ".jpg")), height = 5, width = 5)
}



df <- arrange(df.all, dist.lsev)
df$BURNID <- factor(df$BURNID, levels = df$BURNID)
ggplot(df, aes(BURNID, dist.lsev))+
  geom_col()+
  geom_errorbar(aes(ymin=dist.lsev-dist.lsev.se, ymax=dist.lsev+dist.lsev.se), width=.2,
                position=position_dodge(.9)) +
  coord_flip()+
  labs(title = "Spring prescribed burns 2023", 
       subtitle = "Note: burns may not be complete", 
       y = "Mean distance to low severity (m)", 
       caption = paste0("Created: ", Sys.Date()))+
  theme_bw()
ggsave(here("stats", "allStat_Mean_Dist_to_LSev.jpg"), height = 7, width = 5)

# df <- arrange(df.all, ub.patch)
# df$BURNID <- factor(df$BURNID, levels = df$BURNID)
# ggplot(df, aes(BURNID, ub.patch))+
#   geom_col()+
#   coord_flip()+
#   labs(title = "Spring prescribed burns 2023", 
#        subtitle = "Note: burns may not be complete", 
#        y = "Unburnt patchiness", 
#        caption = paste0("Created: ", Sys.Date()))+
#   theme_bw()
# ggsave(here("stats", "allStat_patchiness_UB.jpg"), height = 7, width = 5)

# df <- arrange(df.all, lsev.patch)
# df$BURNID <- factor(df$BURNID, levels = df$BURNID)
# ggplot(df, aes(BURNID, lsev.patch))+
#   geom_col()+
#   coord_flip()+
#   labs(title = "Spring prescribed burns 2023", 
#        subtitle = "Note: burns may not be complete", 
#        y = "Low severity patchiness", 
#        caption = paste0("Created: ", Sys.Date()))+
#   theme_bw()
# ggsave(here("stats", "allStat_patchiness_LSev.jpg"), height = 7, width = 5)

df <- dplyr::select(df.all, BURNID, UB, Low, Med, High, v.High, Burnt.Heath) #mutate(t.area = Burnt.Heath+High+Low+Med+UB+v.High) %>% arrange(t.area)
dft <- df
dft$t <- rowSums(dft[,2:7])
dft <- arrange(dft, t)

dfg <- gather(df, class, area, 2:7)
dfg$BURNID <- factor(dfg$BURNID, levels = dft$BURNID)
dfg$class <- factor(dfg$class, levels = c("UB", "Low", "Med", "High", "v.High", "Burnt.Heath"))

ggplot(dfg, aes(BURNID, area, fill = class))+
  geom_col()+
  scale_fill_manual(values=c("forestgreen", "lawngreen", "yellow2", "orange", "red", "grey"))+
  coord_flip()+
  labs(title = "Spring prescribed burns 2023", 
       subtitle = "Note: burns may not be complete \n or may have been completed over several seasons", 
       y = "Area (Ha)", 
       caption = paste0("Created: ", Sys.Date()))+
  theme_bw()
ggsave(here("stats", "allStat_areaByClass.jpg"), height = 7, width = 5)

dfg$district <- str_sub(dfg$BURNID, end = 3)
d <- unique(dfg$district )

for (i in 1:length(d)){
  dfi <- filter(dfg, district == d[i])
ggplot(dfi, aes(BURNID, area, fill = class))+
  geom_col()+
  scale_fill_manual(values=c("forestgreen", "lawngreen", "yellow2", "orange", "red", "grey"))+
  coord_flip()+
  labs(title = paste0("Spring prescribed burns 2023: ",d[i]), 
       subtitle = "Note: burns may not be complete \n or may have been completed over several seasons", 
       y = "Area (Ha)", 
       caption = paste0("Created: ", Sys.Date()))+
  theme_bw()
ggsave(here("stats", paste0("allStat_areaByClass_", d[i], ".jpg")), height = 5, width = 5)
}


ggplot(df.all, aes(n.patch, m.patch))+
  geom_point()

dfp <- df.all
dfp$district <- str_sub(dfp$BURNID, end = 3)
dfp <- mutate(dfp, area_ha = Burnt.Heath + High + Low + Med + UB + v.High)
d <- unique(dfp$district )
i <- 1
library(ggrepel)

for (i in 1:length(d)){
  dfi <- filter(dfp, district == d[i])
  ggplot(dfi, aes(n.patch, m.patch, colour = area_ha, label = BURNID))+
    geom_point()+
    geom_text_repel()+
    labs(title = paste0("Spring prescribed burns 2023: ",d[i]), 
         subtitle = "Note: burns may not be complete \n or may have been completed over several seasons", 
         y = "Mean patch area (Ha)", x = "Number of patches", colour = "Area (Ha)",
         caption = paste0("Created: ", Sys.Date()))+
    theme_bw()
  ggsave(here("stats", paste0("allStat_patch_", d[i], ".jpg")), height = 5, width = 5)
}


ggplot(dfp, aes(dist.ub, area_ha))+
  geom_point()+
  scale_y_continuous(trans='log2')

