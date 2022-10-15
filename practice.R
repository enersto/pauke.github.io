
library(geoviz)
library(ggplot2)
library(data.table)
library(rgdal)
library(sf)
library(openxlsx)
library(dplyr)
library(RColorBrewer)
library(tmap)

gz <- read_sf("/Users/pauke/Downloads/广州市/广州市_乡镇边界.shp",options = "ENCODING=GBK")
#gd <- read_sf("/Users/pauke/Downloads/广州市/矢量数据/广东省/广东省_乡镇边界.dbf",options = "ENCODING=GBK")
edge <- read_sf("/Users/pauke/Downloads/矢量数据/建成区/广州.shp",options = "ENCODING=GBK")
gz_t <- st_transform(gz_t,4326)
edge <- st_transform(edge,4326)

Desc(Name ~ type2,insc_sp)


edge_mt <- st_cast(edge,"MULTIPOLYGON")

edge_l <- st_cast(edge,"LINESTRING")

edge_over <-  st_overlaps(edge,gz_t)
st_area(gz_t,edge)


edge_l <- st_boundary(edge)
gz_l <- st_join(gz_t[,c("Name","code","layer1","layer2","area","geometry")],edge_l)
names(gz_l)
gz_l_ag <- setDT(gz_l)[,.(cnt= length(Floor)),by = c("Name")]

ggplot()+ geom_sf(data = edge_mt,aes(geometry = geometry))+ 
  geom_sf(data = gz_t,aes(geometry = geometry),color = "lightblue",fill =NA)

  
ggplot()+ geom_sf(data = edge_mt,aes(geometry = geometry))

ggplot()+ geom_sf(data = gd,aes(geometry = geometry))


ggplot()+ geom_sf(data = gz_t,aes(geometry = geometry))+
  geom_sf(data = edge,aes(geometry = geometry))


gz_t_e <- st_join(gz_t[,c("Name","code","layer1","layer2","area","geometry")],edge_mt)

t1<- read.xlsx("/Users/pauke/Downloads/矢量数据/街道层级人口.xlsx")
names(t1)
t1 <- setDT(t1)[,densities:= n7th/area/10000][,change:=
                                          (n7th-n6th)/n6th][,change_cut:=
                                                              cut(change,c(-Inf,-0.1,0,0.1,
                                                                        0.2,0.5,1,2,7,Inf),right = F)]
t1 <- t1[,n7th_s := n7th/10000]

gz_t <- read_sf("/Users/pauke/Downloads/矢量数据/行政区划/乡镇街道/广东省_乡镇边界.shp",options = "ENCODING=GBK")
gz_t <- gz_t[substr(gz_t$code,1,4) %in% c('4401','4406'),]
names(gz_t)
gz_t <- left_join(gz_t,t1,by =c("Name"= "town"))

gz_t <- setDT(gz_t)[is.na(gz_t$layer1),area_empty := -1]
gz_t <- st_as_sf(gz_t)


ggplot()+ geom_sf(data = gz_t,aes(fill= densities,geometry = geometry))+
  scale_fill_distiller(palette='Spectral')+
  geom_sf_text(data = gz_t[gz_t$densities >= 20000,],aes(label = Name),
               family='Songti SC',size = 3,fontface="bold")+
  theme(text = element_text(family='Songti SC'))

ggplot()+ geom_sf(data = gz_t,aes(fill= n7th_s,geometry = geometry))+
  scale_fill_distiller(palette='Spectral')+
  geom_sf_text(data = gz_t[gz_t$n7th_s >= 30,],aes(label = Name),
               family='Songti SC',size = 3,fontface="bold")+
  theme(text = element_text(family='Songti SC'))


ggplot()+ geom_sf(data = gz_t,aes(fill = area_empty,geometry = geometry))+
  geom_sf_text(data = gz_t[is.na(gz_t$layer1),],aes(label = Name),
               family='Songti SC',size = 3)+
  theme(text = element_text(family='Songti SC'))


ggplot()+ 
  geom_sf(data = gz,aes(fill =第七次,geometry = geometry)) + 
  scale_fill_brewer(palette="YlOrRd")

ggplot()+ 
  geom_sf(data = gz,aes(fill =第七次_s,geometry = geometry)) + 
  scale_fill_distiller(palette="YlOrRd",direction = 1)+
  theme(text = element_text(family='Songti SC'))

ggplot()+ geom_sf(data = gz_t_rs_1,aes(geometry = geometry))+
  geom_sf(data = poi_rs,aes(geometry = geometry))

ggplot()+ geom_sf(data = gz_t_rs_1,aes(fill= cnt,geometry = geometry))+
  scale_fill_distiller(palette='Spectral')+
  geom_sf_text(data = gz_t_rs_1[gz_t_rs_1$cnt >= 200,],aes(label = Name),
               family='Songti SC',size = 3)+
  theme(text = element_text(family='Songti SC'))



ggplot()+ geom_sf(data = gz_t_bk_2,aes(fill= pre_cnt,geometry = geometry))+
  scale_fill_distiller(palette='Spectral')+
  geom_sf_text(data = gz_t_bk_2[gz_t_bk_2$pre_cnt >= 10,],aes(label = Name),
               family='Songti SC',size = 3)+
  theme(text = element_text(family='Songti SC'))


ggplot()+ geom_sf(data = gz_t_ml_2,aes(fill= pre_cnt,geometry = geometry))+
  scale_fill_distiller(palette='Spectral')+
  geom_sf_text(data = gz_t_ml_2[gz_t_ml_2$pre_cnt >= 2,],aes(label = Name),
               family='Songti SC',size = 3)+
  theme(text = element_text(family='Songti SC'))

ggplot()+ geom_sf(data = gz_t_bs_1,aes(geometry = geometry))+
  geom_sf(data = poi_bs,aes(geometry = geometry))

gz <- read_sf("/Users/pauke/Downloads/矢量数据/行政区划/乡镇街道/广州市_乡镇边界.shp")
##############poi data#########
gz_t <- read_sf("/Users/pauke/Downloads/矢量数据/行政区划/乡镇街道/广州市_乡镇边界.shp",options = "ENCODING=GBK")
gz_c <- read_sf("/Users/pauke/Downloads/矢量数据/行政区划/县区/guangzhou.shp")


###https://youwuqiong.top/405807.html
poi_ms = read_sf("/Users/pauke/Downloads/矢量数据/POI数据_2020/广州地铁站.shp")
poi_bs = read_sf("/Users/pauke/Downloads/矢量数据/POI数据_2020/广州公交站.shp")
poi_pk = read_sf("/Users/pauke/Downloads/矢量数据/POI数据_2020/广州公园.shp")
poi_bb = read_sf("/Users/pauke/Downloads/矢量数据/POI数据_2020/广州写字楼.shp")
poi_hp = read_sf("/Users/pauke/Downloads/矢量数据/POI数据_2020/广州医院.shp")
poi_ml = read_sf("/Users/pauke/Downloads/矢量数据/POI数据_2020/广州商场.shp")
poi_ed = read_sf("/Users/pauke/Downloads/矢量数据/POI数据_2020/广州学校.shp")
poi_ls = read_sf("/Users/pauke/Downloads/矢量数据/POI数据_2020/广州小区.shp")
poi_sp = read_sf("/Users/pauke/Downloads/矢量数据/POI数据_2020/广州超市.shp")
poi_ht = read_sf("/Users/pauke/Downloads/矢量数据/POI数据_2020/广州酒店.shp")
poi_bk = read_sf("/Users/pauke/Downloads/矢量数据/POI数据_2020/广州银行.shp")
poi_rs = read_sf("/Users/pauke/Downloads/矢量数据/POI数据_2020/广州餐饮.shp")
poi_mk = read_sf("/Users/pauke/Downloads/矢量数据/POI数据_2020/广州菜市场.shp")

poi_ms_fs =read_sf("/Users/pauke/Downloads/矢量数据/POI数据_2020/佛山/地铁站.shp")
poi_bs_fs =read_sf("/Users/pauke/Downloads/矢量数据/POI数据_2020/佛山/公交站.shp")
poi_pk_fs =read_sf("/Users/pauke/Downloads/矢量数据/POI数据_2020/佛山/公园.shp")
poi_bb_fs =read_sf("/Users/pauke/Downloads/矢量数据/POI数据_2020/佛山/写字楼.shp")
poi_hp_fs =read_sf("/Users/pauke/Downloads/矢量数据/POI数据_2020/佛山/医院.shp")
poi_ml_fs =read_sf("/Users/pauke/Downloads/矢量数据/POI数据_2020/佛山/商场.shp")
poi_ed_fs =read_sf("/Users/pauke/Downloads/矢量数据/POI数据_2020/佛山/学校.shp")
poi_ls_fs =read_sf("/Users/pauke/Downloads/矢量数据/POI数据_2020/佛山/住宅.shp")
poi_sp_fs =read_sf("/Users/pauke/Downloads/矢量数据/POI数据_2020/佛山/购物.shp")
poi_ht_fs =read_sf("/Users/pauke/Downloads/矢量数据/POI数据_2020/佛山/酒店.shp")
poi_bk_fs =read_sf("/Users/pauke/Downloads/矢量数据/POI数据_2020/佛山/银行.shp")
poi_rs_fs =read_sf("/Users/pauke/Downloads/矢量数据/POI数据_2020/佛山/餐饮.shp")


names(poi_ms)
names(poi_bs)

names(poi_ms)
names(poi_bs)
names(poi_pk)
names(poi_bb)
names(poi_hp)
names(poi_ml)
names(poi_ed)
names(poi_ls)
names(poi_sp)
names(poi_ht)
names(poi_bk)
names(poi_rs)
names(poi_mk)

names(poi_ms_fs)
names(poi_bs_fs)
names(poi_pk_fs)
names(poi_bb_fs)
names(poi_hp_fs)
names(poi_ml_fs)
names(poi_ed_fs)
names(poi_ls_fs)
names(poi_sp_fs)
names(poi_ht_fs)
names(poi_bk_fs)
names(poi_rs_fs)



building <- read_sf("/Users/pauke/Desktop/广州建筑轮廓数据/广州.shp")
ggplot()+ geom_sf(data = building,aes(geometry = geometry))

building <- read_sf("/Users/pauke/Downloads/矢量数据/水系/水系.shp")
ggplot()+ geom_sf(data = gz_c,aes(geometry = geometry))+
  geom_sf(data = building,aes(geometry = geometry),color = "lightblue")

gz_c <- read_sf("/Users/pauke/Downloads/矢量数据/行政区划/县区/广东省_县界.shp",options = "ENCODING=GBK")
gz_c <- gz_c[substr(gz_c$code,1,4) %in% c('4401','4406'),]



gz_t <- read_sf("/Users/pauke/Downloads/矢量数据/行政区划/乡镇街道/广东省_乡镇边界.shp",options = "ENCODING=GBK")
gz_t <- gz_t[substr(gz_t$code,1,4) %in% c('4401','4406'),]
ggplot()+ geom_sf(data = gz_t,aes(geometry = geometry))



building <- read_sf("/Users/pauke/Downloads/矢量数据/铁路/铁路.shp")
ggplot()+ geom_sf(data = gz_c,aes(geometry = geometry))+
  geom_sf(data = building,aes(geometry = geometry),color = "lightblue")



names(gd)
gd <- setDT(gd)[type3 == "高等院校",]
ggplot()+ geom_sf(data = gz,aes(geometry = geometry))+
  geom_sf(data = gd,aes(geometry = geometry), color = "red",size = 0.8)

data(World)

gz_t <- st_transform(gz_t,3857)

ggplot()+ geom_sf(data = gz_t,aes(fill= n7th_s,geometry = geometry))



gd_43 <- st_transform(gd,4326)

intersection <- st_intersection(gd_43,gz)
names(intersection)
split_count <- setDT(intersection)[,.(cnt= length(name)),by = c("Name")]

names(gz)
split_count <- st_intersection(gd_43,gz) %>%
  group_by(Name) %>% 
  count() %>%
  arrange(desc(n))



ggplot()+ 
  geom_sf(data = split_count,aes(fill =n,geometry = geometry)) 

###########about epsg###########
gd <- read_sf("/Users/pauke/Downloads/广州市/矢量数据/广东省/广东省_县界.shp",options = "ENCODING=GBK")

gd_43 <- st_transform(gd,4326)
ggplot()+ geom_sf(data = gd_43,aes(geometry = geometry))

gd_38 <- st_transform(gd,3857)
ggplot()+ geom_sf(data = gd_38,aes(geometry = geometry))

gz_38 <- st_transform(gz,3857)
ggplot()+ geom_sf(data = gz_38,aes(geometry = geometry))

gz_43 <- st_transform(gz,4326)
ggplot()+ geom_sf(data = gz_43,aes(geometry = geometry))

write_sf(inse_rs,"/Users/pauke/Downloads/矢量数据/rs.shp")

library(tmap)
china <- World[World$name == "China",]

china <- st_transform(china,3857)
ggplot()+ geom_sf(data =china,aes(geometry = geometry))


data(World) # free dataset that will load
countries = World %>%
  filter(name == "Afghanistan" | 
           name == "Turkmenistan" |
           name == "Iran") #Picking three countries in the world

pnts = data.frame(name=c("Point1","Point2","Point3","Point4"),latitude = c(34,35,36,37),longitude=c(63,65,62.5,66.5)) #Picking random points

sp_points = st_as_sf(pnts,coords = c('longitude',"latitude"))#make points spatial
st_crs(sp_points)= 4326 # Give the points a coordinate reference system (CRS)
sp_points=st_transform(sp_points,crs = st_crs(countries)) # Match the point and polygon CRS

tm_shape(countries)+
  tm_borders() +
  tm_text('name')+
  tm_shape(sp_points)+
  tm_dots(col='red',size=1.2)+
  tm_text('name',ymod = 1) #Creates the map below




##########italy example############
# Load libraries ----------------------------------------------------------

library(raster)


# Get sample data ---------------------------------------------------------

# Get polygon
polygon <- getData('GADM', country='URY', level = 1)[,1] # Download polygon of country admin level 1 
polygon <- st_as_sf(polygon) # convert to sf object
colnames(polygon) <- c("id_polygons", "geometry") # change colnames
polygon$id_polygons <- paste0("poly_", LETTERS[1:19]) #  change polygon ID

# Get sample random poins from polygon bbox
set.seed(4)
bbox <- st_as_sfc(st_bbox(polygon))
points <- st_sample(x = bbox, size = 100, type = "random")
points <- st_as_sf(data.frame(id_points = as.character(1:100)), points) # add points ID

# Plot data ---------------------------------------------------------------

# Plot polygon + points
plot(polygon, graticule = st_crs(4326), key.pos = 1)
plot(points, pch = 19, col = "black", add = TRUE)

# Intersection between polygon and points ---------------------------------

intersection <- st_intersection(x = polygon, y = points)

# Plot intersection
plot(polygon, graticule = st_crs(4326), key.pos = 1)
plot(intersection[1], col = "black", pch = 19, add = TRUE)

# View result
table(intersection$id_polygons) # using table

# using dplyr
int_result <- intersection %>% 
  group_by(id_polygons) %>% 
  count()

as.data.frame(int_result)[,-3]



library(sf)
library(tidyverse)

########### from the question ############
#creating data example
id <- c("844", "844", "844", "844", "844","855", "855", "855", "855", "855")

lat <- c(-30.6456, -29.5648, -28.6667, -31.5587, -30.6934, -29.3147, -28.0538, 
         -26.5877, -26.6923, -27.40865)
long <- c(-50.4879, -49.8715, -51.8716, -50.4456, -50.9842, -51.9787, -47.2343, 
          -49.2859, -48.19599, -49.64302)

df <- data.frame(id = as.factor(id), lat, long)

#converting to sf
df.sf <- df %>% 
  sf::st_as_sf(coords = c("long", "lat"), crs = 4326)

#creating linestrings
df.line <- df.sf %>% 
  dplyr::group_by(id) %>%
  dplyr::summarize() %>%
  sf::st_cast("LINESTRING") 

#creating grid
xy <- sf::st_coordinates(df.sf)

grid <- sf::st_make_grid(sf::st_bbox(df.sf),
                         cellsize = 1, square = FALSE) %>%
  sf::st_as_sf() %>%
  dplyr::mutate(cell = 1:nrow(.))
###### End from question ##########


# Transform to a crs that uses meters for buffering
df.line <- st_transform(df.line, 3857)
grid <- st_transform(grid, 3857)

# Create a geometry to 'cut' the lines with by buffering the grid by 100m.
# You may want to change the buffer distance.
blade <- st_cast(grid, 'MULTILINESTRING') %>% 
  st_buffer(100) %>% 
  st_union() %>% 
  st_as_sf()

# erase the parts of the lines that cross the buffered grids
line_split <- rmapshaper::ms_erase(df.line, blade) %>%
  st_cast('LINESTRING')

split_count <- st_intersection(line_split, grid) %>%
  group_by(id, cell) %>% 
  count() %>%
  arrange(desc(n))

head(split_count)


#########version 3#########

library(magrittr)


tt <- read_sf(path, "USA_adm1")

# subset some states to make it plot faster
tt1 <- tt[tt$NAME_1 %in% c("South Dakota", "Wyoming",  
                           "Nebraska", "Iowa"), ]