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
