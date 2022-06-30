library(raster)
library(landscapemetrics)
library(tmap)

landscape

forest <- landscape == 2

patch <- get_patches(landscape)
patch <- patch$layer_1$class_2

map_lc <- tm_shape(landscape) +
    tm_raster(pal = "viridis", legend.show = FALSE) +
    tm_layout(main.title = "Paisagem",
              main.title.position = "center",
              main.title.size = 3) 
tmap_save(map_lc, "map_paisagem.png", bg = NA)

map_fo <- tm_shape(forest) +
    tm_raster(pal = c("gray", "forestgreen"), legend.show = FALSE) +
    tm_layout(main.title = "Binário",
              main.title.position = "center",
              main.title.size = 3) 
tmap_save(map_fo, "map_floresta.png", bg = NA)

map_p <- tm_shape(patch) +
    tm_raster(pal = "viridis", legend.show = FALSE) +
    tm_layout(main.title = "Patch",
              main.title.position = "center",
              main.title.size = 3) 
tmap_save(map_p, "map_p.png", bg = NA)

map_c <- tm_shape(forest) +
    tm_raster(pal = c("white", "#1f135f"), legend.show = FALSE) +
    tm_layout(main.title = "Class",
              main.title.position = "center",
              main.title.size = 3) 
tmap_save(map_c, "map_c.png", bg = NA)

map_l <- tm_shape(landscape) +
    tm_raster(pal = c("#443a83", "#1f135f", "#9991c5"), legend.show = FALSE) +
    tm_layout(main.title = "Landscape",
              main.title.position = "center",
              main.title.size = 3) 
tmap_save(map_l, "map_l.png", bg = NA)

