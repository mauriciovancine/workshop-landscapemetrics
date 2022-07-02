library(magick)

setwd("/home/mude/data/github/mauriciovancine/workshop-landscapemetrics/01_slides/img/")

img <- magick::image_read("ecologia_espacial.png")
img_tr <- image_transparent(img, 'white')
image_write(img_tr, "ecologia_espacial_bg.png")

img <- magick::image_read("ecologia_espacial_estudos.png")
img_tr <- image_transparent(img, 'white')
image_write(img_tr, "ecologia_espacial_estudos_bg.png")

img <- magick::image_read("hauer_etal_2016.png")
img_tr <- image_transparent(img, 'white')
image_write(img_tr, "hauer_etal_2016_bg.png")

img <- magick::image_read("escala_organismos.png")
img_tr <- image_transparent(img, 'white')
image_write(img_tr, "escala_organismos_bg.png")

img <- magick::image_read("forest-patch-landscape-mosaic.png")
img_tr <- image_transparent(img, 'white')
image_write(img_tr, "forest-patch-landscape-mosaic_bg.png")

img <- magick::image_read("estrutura_paisagem.png")
img_tr <- image_transparent(img, 'white')
image_write(img_tr, "estrutura_paisagem_bg.png")

img <- magick::image_read("mosaico.png")
img_tr <- image_transparent(img, 'white')
image_write(img_tr, "mosaico_bg.png")

img <- magick::image_read("tipos_metricas.png")
img_tr <- image_transparent(img, 'white')
image_write(img_tr, "tipos_metricas_bg.png")

img <- magick::image_read("comp_conf.png")
img_tr <- image_transparent(img, 'white')
image_write(img_tr, "comp_conf_bg.png")

img <- magick::image_read("comp_conf_espacial.png")
img_tr <- image_transparent(img, 'white')
image_write(img_tr, "comp_conf_espacial_bg.png")

img <- magick::image_read("comp_conf_tabela.png")
img_tr <- image_transparent(img, 'white')
image_write(img_tr, "comp_conf_tabela_bg.png")

img <- magick::image_read("regra_patches.png")
img_tr <- image_transparent(img, 'white')
image_write(img_tr, "regra_patches_bg.png")

img <- magick::image_read("amostragem.png")
img_tr <- image_transparent(img, 'white')
image_write(img_tr, "amostragem_bg.png")

img <- magick::image_read("corr.png")
img_tr <- image_transparent(img, 'white')
image_write(img_tr, "corr_bg.png")

img <- magick::image_read("escala01.png")
img_tr <- image_transparent(img, 'white')
image_write(img_tr, "escala01_bg.png")

img <- magick::image_read("escala02.png")
img_tr <- image_transparent(img, 'white')
image_write(img_tr, "escala02_bg.png")

img <- magick::image_read("perguntas.png")
img_tr <- image_transparent(img, 'white')
image_write(img_tr, "perguntas_bg.png")

img <- magick::image_read("variaveis.png")
img_tr <- image_transparent(img, 'white')
image_write(img_tr, "variaveis_bg.png")

img <- magick::image_read("geoprocessamento.jpg")
img_tr <- image_transparent(img, 'white')
image_write(img_tr, "geoprocessamento_bg.png")

img <- magick::image_read("crs.png")
img_tr <- image_transparent(img, 'white')
image_write(img_tr, "crs_bg.png")

img <- magick::image_read("datum.jpeg")
img_tr <- image_transparent(img, 'white')
image_write(img_tr, "datum_bg.png")

img <- magick::image_read("geo_gps_way_rout_track.png")
img_tr <- image_transparent(img, 'white')
image_write(img_tr, "gps_way_route_track.png")

img <- magick::image_read("sr.png")
img_tr <- image_transparent(img, 'white')
image_write(img_tr, "sr_bg.png")

img <- magick::image_read("gis.png")
img_tr <- image_transparent(img, 'white')
image_write(img_tr, "gis_bg.png")

img <- magick::image_read("geo_shp_formats.png")
img_tr <- image_transparent(img, 'white')
image_write(img_tr, "geo_shp_formats_bg.png")

img <- magick::image_read("geo_raster_cont_cat.png")
img_tr <- image_transparent(img, 'white')
image_write(img_tr, "geo_raster_cont_cat_bg.png")

img <- magick::image_read("vetor.png")
img_tr <- image_transparent(img, 'white')
image_write(img_tr, "vetor_bg.png")

img <- magick::image_read("raster.png")
img_tr <- image_transparent(img, 'white')
image_write(img_tr, "raster_bg.png")

img <- magick::image_read("geo_data_convert.png")
img_tr <- image_transparent(img, 'white')
image_write(img_tr, "geo_data_convert_bg.png")

img <- magick::image_read("rstudio_script.jpg")
img_tr <- image_transparent(img, 'white')
image_write(img_tr, "rstudio_script_bg.png")

