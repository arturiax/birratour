library(ggmap)
library(ggplot2)
library(dplyr)
library(ggrepel)
library(animation)
library(showtext)
library(purrr)
library(tidyr)
library(emojifont)
library(png)
load.emojifont('OpenSansEmoji.ttf')

#register_google(key = "AIzaSyCPOfHdNxn_H3XTYjsnpLpsuNooJrlifvk")


theme_map <- function(base_size=9, base_family="") {
  require(grid)
  theme_bw(base_size=base_size, base_family=base_family) %+replace%
    theme(axis.line=element_blank(),
          axis.text=element_blank(),
          axis.ticks=element_blank(),
          axis.title=element_blank(),
          panel.background=element_blank(),
          panel.border=element_blank(),
          panel.grid=element_blank(),
          panel.spacing=unit(0, "lines"),
          plot.background=element_blank(),
          legend.justification = c(0,0),
          legend.position = c(0,0)
    )
}

# statico
register_google(key = "AIzaSyBUT6aMZlrHMReT0ldvKDmfD658d5NTF8U")




ggmap_credentials()

lugar <- get_map("Bar Minuto, Portugalete, spain", zoom=16, maptype="watercolor", source="stamen")#, source="stamen", maptype = "watercolor")

ggmap(lugar)

lista<-list("Silverstone", "Taberna Aldatza", "Café Jaque", "Bar Minuto")
direcc<- sapply(lista, paste0, ", Portugalete, Spain")
sapply(direcc, geocode)->cords2

lugares <- data_frame(place= direcc, nombre = unlist(lista)) %>% 
            mutate_geocode(place) %>% 
            arrange(lon) %>% 
            mutate(orden = as.factor(1:4))


#caminos
register_google(key = "AIzaSyDGy_qgMFo4rKVY2FNwMgxJM-n5UaSvESc")

trek_df <- map2(lugares$place[1:3],lugares$place[2:4], trek, mode = "walking", structure="trek")

routeQueryCheck()

font_add_google("Lobster", "lobster")
font_add_google("Allerta Stencil", "allerta")
font_add_google("Gloria Hallelujah", "gloria")
font_add_google("Shrikhand", "shri")
font_add_google("Bungee", "bungee")

mypng <- readPNG("/home/art/birratour/ppp.png")



showtext_auto()

ani.options(interval=.15)
saveGIF({
  

  q<-ggmap(lugar) +
    annotate("rect", xmin=-3.025, xmax=-3.0132, ymin=43.316, ymax=43.325, alpha=0.6) +
    annotate("text", x=-3.019, y=43.32338, family="bungee", color="#EE9A00", size=14, label="BIRRATOUR\nPORTU") +
    annotation_raster(mypng, xmin = -3.0215, xmax = -3.0165, ymin=43.3165, ymax=43.3215) +
    coord_fixed(1.3) +
    theme_map()
  for (j in 1:7) print(q)
  
  q<-ggmap(lugar) +
    annotate("rect", xmin=-3.025, xmax=-3.0132, ymin=43.316, ymax=43.325, alpha=0.6) +
    annotate("text", x=-3.01945, y=43.321, family="OpenSansEmoji", color="#EF9A00", size=60, label="\U1f37a")+
    coord_fixed(1.3) +
    theme_map()
  
  for (j in 1:6) print(q)  
  
for (i in 1:3){
  #i<-4
  
   q<-ggmap(lugar, base_layer=ggplot(lugares[i, ], aes(lon, lat))) + 
               geom_point(aes(color=orden), size = 6)  +
               geom_text(aes(label=orden), color="white") +
 # geom_path(data=trek_df[[1]], aes(lon, lat), size=3) +
    geom_label(aes(label=nombre), fill="tan1", size=5, color="white", nudge_x=ifelse(i==1,0.003,0.002))  +
      guides(fill=FALSE, color=FALSE) +
     annotation_raster(mypng, xmin=-3.0256, xmax=-3.0228, ymin=43.323, ymax=43.325)+
     geom_text_repel(family="OpenSansEmoji", label="\U1f37a") +
     coord_fixed(1.3) +
  theme_map()
 
   for (j in 1:10) print(q)
  
 for( j in seq(trek_df[[i]]$lat)) {
  q<-ggmap(lugar, base_layer=ggplot(lugares[i, ], aes(lon, lat))) + 
   
   
   geom_path(data=trek_df[[i]][1:j, ], aes(lon, lat), size=2, color="blue") +
   #geom_label_repel(aes(label=nombre), fill="tan1",point.padding = 1.5, size=4, color="white")  +
    geom_point(aes(color=orden), size = 10)  +
    geom_text(aes(label=orden), color="white") +
    annotation_raster(mypng, xmin=-3.0256, xmax=-3.0228, ymin=43.323, ymax=43.325)+
    
   guides(fill=FALSE, color=FALSE) +
    coord_fixed(1.3) +
   theme_map()
   print(q)
  } 

}  
  
    q<-ggmap(lugar, base_layer=ggplot(lugares[4, ], aes(lon, lat))) + 
    geom_point(aes(color=orden), size = 6)  +
    geom_text(aes(label=orden), color="white") +
    # geom_path(data=trek_df[[1]], aes(lon, lat), size=3) +
      geom_label(aes(label=nombre), fill="tan1", size=5, color="white", nudge_x=-0.002)  +
      annotation_raster(mypng, xmin=-3.0256, xmax=-3.0228, ymin=43.323, ymax=43.325)+
      geom_text_repel(family="OpenSansEmoji", label="\U1f37a") +
      guides(fill=FALSE, color=FALSE) +
      coord_fixed(1.3) +
    theme_map()
    for (j in 1:10) print(q) 
    
    q<-ggmap(lugar) +
      annotate("rect", xmin=-3.025, xmax=-3.0132, ymin=43.316, ymax=43.325, alpha=0.6) +
      annotate("text", x=-3.01945, y=43.321, family="OpenSansEmoji", color="#EF9A00", size=60, label="\U1f37b")+
      
      coord_fixed(1.3) +
      theme_map()
    
    for (j in 1:8) print(q)
    
    
})  
showtext_auto(FALSE) 




