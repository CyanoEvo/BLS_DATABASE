#load packages
library(dplyr)
library(ggplot2)
library(ggplot2)
library(maps)
library(mapproj)

#Data
worldmap <- map_data('world',region=c("uk", "ireland","isle of man", "isles of scilly"))
BLS <- BLS_database_export_for_MBA

#This will do a very simple filter for a single taxo and plot a map

BLS_FILTER<-BLS %>% filter(grepl('Stereocaulon pileatum', Taxon.Name))

ggplot() + 
  geom_polygon(data = worldmap, 
               aes(x = long, y = lat, 
                   group = group), 
               fill = 'gray90', 
               color = 'black') + 
  coord_map() +
  theme_void() +
  geom_point(data = BLS_FILTER,alpha=0.2,size=3,
             aes(x = as.numeric(Longitude), 
                 y = as.numeric(Latitude),
                 colour = Substrate)) +
  facet_wrap(facets = "Taxon.Name")+ 
  theme(legend.position = c(0.9, 0.7)) + 
  guides(colour = guide_legend(override.aes = list(alpha = 1, size = 3)))





BLS_OCH<-BLS %>% filter(grepl('Ochrolechia', Taxon.Name))
BLS_OCH["Substrate"][BLS_OCH["Substrate"] == ""] <- "Unknown"
BLS_OCH<-BLS_OCH %>% filter(!grepl('Unknown', Substrate))
BLS_OCH<-BLS_OCH %>% filter(!grepl('None', Substrate))
BLS_OCH<-BLS_OCH %>% filter(!grepl('\\+Lig', Substrate))
BLS_OCH<-BLS_OCH %>% filter(!grepl('\\+Bry', Substrate))
BLS_OCH<-BLS_OCH %>% filter(!grepl('\\+Cort', Substrate))
BLS_OCH<-BLS_OCH %>% filter(!grepl('\\+Sax', Substrate))
BLS_OCH<-BLS_OCH %>% filter(!grepl('\\+Terr', Substrate))
BLS_OCH<-BLS_OCH %>% filter(!grepl('Lic', Substrate))


BLS_OCH<-BLS_OCH %>% filter(!grepl('^Ochrolechia$', Taxon.Name))
BLS_OCH<-BLS_OCH %>% filter(!grepl('Ochrolechia inaequatula', Taxon.Name))
BLS_OCH<-BLS_OCH %>% filter(!grepl('Ochrolechia arborea', Taxon.Name))
BLS_OCH<-BLS_OCH %>% filter(!grepl('Ochrolechia xanthostoma', Taxon.Name))
BLS_OCH<-BLS_OCH %>% mutate(Taxon.Name=recode(Taxon.Name, 'Ochrolechia frigida f. frigida'='Ochrolechia frigida'))
BLS_OCH<-BLS_OCH %>% mutate(Taxon.Name=recode(Taxon.Name, 'Ochrolechia frigida f. lapuensis'='Ochrolechia frigida'))
BLS_OCH<-BLS_OCH %>% mutate(Taxon.Name=recode(Taxon.Name, 'Ochrolechia turneri s. lat.'='Ochrolechia turneri'))
BLS_OCH<-BLS_OCH %>% mutate(Taxon.Name=recode(Taxon.Name, 'Ochrolechia turneri s. str.'='Ochrolechia turneri'))


O_PARELLA<-BLS_OCH %>% filter(grepl('Ochrolechia parella', Taxon.Name))
O_PARELLA_SUBSTRATE_COUNT<-data.frame(table(O_PARELLA$Substrate))
O_PARELLA_SUBSTRATE_COUNT<-data.frame(table(O_PARELLA$Substrate))
O_PARELLA_SUBSTRATE_COUNT %>%
  arrange(Freq) %>%    # First sort by val. This sort the dataframe but NOT the factor levels
  mutate(Var1=factor(Var1, levels=Var1)) %>%   # This trick update the factor levels
  ggplot( aes(x=Var1, y=Freq)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_bw() +
  xlab("")+
  ylab("Records (n)") +
  theme(aspect.ratio = 1)

O_TARTAREA<-BLS_OCH %>% filter(grepl('Ochrolechia tartarea', Taxon.Name))
O_TARTAREA_SUBSTRATE_COUNT<-data.frame(table(O_TARTAREA$Substrate))
O_TARTAREA_SUBSTRATE_COUNT %>%
  arrange(Freq) %>%    # First sort by val. This sort the dataframe but NOT the factor levels
  mutate(Var1=factor(Var1, levels=Var1)) %>%   # This trick update the factor levels
  ggplot( aes(x=Var1, y=Freq)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_bw() +
  xlab("")+
  ylab("Records (n)")+
  theme(aspect.ratio = 1)

O_ANDROGYNA<-BLS_OCH %>% filter(grepl('Ochrolechia androgyna', Taxon.Name))
O_ANDROGYNA_SUBSTRATE_COUNT<-data.frame(table(O_ANDROGYNA$Substrate))
O_ANDROGYNA_SUBSTRATE_COUNT %>%
  arrange(Freq) %>%    # First sort by val. This sort the dataframe but NOT the factor levels
  mutate(Var1=factor(Var1, levels=Var1)) %>%   # This trick update the factor levels
  ggplot(aes(x=Var1, y=Freq)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_bw() +
  xlab("")+
  ylab("Records (n)")+
  theme(aspect.ratio = 1)

O_SUBVIRIDIS<-BLS_OCH %>% filter(grepl('Ochrolechia subviridis', Taxon.Name))
O_SUBVIRIDIS_SUBSTRATE_COUNT<-data.frame(table(O_SUBVIRIDIS$Substrate))
O_SUBVIRIDIS_SUBSTRATE_COUNT %>%
  arrange(Freq) %>%    # First sort by val. This sort the dataframe but NOT the factor levels
  mutate(Var1=factor(Var1, levels=Var1)) %>%   # This trick update the factor levels
  ggplot( aes(x=Var1, y=Freq)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_bw() +
  xlab("")+
  ylab("Records (n)")+
  theme(aspect.ratio = 1)

O_XANTHOSTOMA<-BLS_OCH %>% filter(grepl('Ochrolechia xanthostoma', Taxon.Name))
O_XANTHOSTOMA_SUBSTRATE_COUNT<-data.frame(table(O_XANTHOSTOMA$Substrate))
O_XANTHOSTOMA_SUBSTRATE_COUNT %>%
  arrange(Freq) %>%    # First sort by val. This sort the dataframe but NOT the factor levels
  mutate(Var1=factor(Var1, levels=Var1)) %>%   # This trick update the factor levels
  ggplot( aes(x=Var1, y=Freq)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_bw() +
  xlab("")+
  ylab("Records (n)")+
  theme(aspect.ratio = 1)

O_FRIGIDA<-BLS_OCH %>% filter(grepl('Ochrolechia frigida', Taxon.Name))
O_FRIGIDA_SUBSTRATE_COUNT<-data.frame(table(O_FRIGIDA$Substrate))
O_FRIGIDA_SUBSTRATE_COUNT %>%
  arrange(Freq) %>%    # First sort by val. This sort the dataframe but NOT the factor levels
  mutate(Var1=factor(Var1, levels=Var1)) %>%   # This trick update the factor levels
  ggplot( aes(x=Var1, y=Freq)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_bw() +
  xlab("")+
  ylab("Records (n)")+
  theme(aspect.ratio = 1)

BLS_FILTER<-BLS %>% filter(grepl('', Taxon.Name))


#Option 2
ggplot() + 
  geom_polygon(data = worldmap, 
               aes(x = long, y = lat, 
                   group = group), 
               fill = 'gray90', 
               color = 'black') + 
  coord_map() +
  theme_void() +
# stat_density2d(data = BLS_OCH,alpha=1,size=0.4,
#                              aes(x = as.numeric(Longitude), 
#                                  y = as.numeric(Latitude),
#                                  colour = Taxon.Name))+ 
  geom_point(data = BLS_FILTER,alpha=0.2,size=1,
             aes(x = as.numeric(Longitude), 
                 y = as.numeric(Latitude),
                 colour = Substrate)) +
  facet_wrap(facets = "Taxon.Name")+ 
  theme(legend.position = c(0.9, 0.2)) + 
  guides(colour = guide_legend(override.aes = list(alpha = 1, size = 3)))

BLS_OCH %>%
  arrange(Freq) %>%    # First sort by val. This sort the dataframe but NOT the factor levels
  mutate(Var1=factor(Var1, levels=Var1)) %>%   # This trick update the factor levels
  ggplot( aes(x=Var1, y=Freq)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_bw() +
  xlab("")+
  ylab("Records (n)")+
  theme(aspect.ratio = 1)


library(ggmap)


UK<-get_stamenmap(
  bbox = c(left = -10.924719, bottom = 49.702554, right = 2.659294, top = 61.097366),
  zoom = 6,
  maptype = c("terrain-background"),
  crop = TRUE,
  messaging = FALSE,
  urlonly = FALSE,
  color = c("color", "bw"),
  force = FALSE,
  where = tempdir(),
  https = FALSE)
ggmap(UK)
UKMap <- ggmap(UK, base_layer = ggplot(aes(x = longitude, y = latitude),
                                                 data = BLS_OCH))
UKMap +
  stat_density2d(aes(x = lon, y = lat, fill = ..level.., alpha = ..level..),
                 bins = 5, geom = "polygon",
                 data = violent_crimes) +
  scale_fill_gradient(low = "black", high = "red") +
  facet_wrap(~ day)


