#' ----
#' theme: metricas paisagem no r: landscapemetrics
#' autor: mauricio vancine
#' data: 2022-07-05
#' ----

# preparar r --------------------------------------------------------------

# pacotes
library(tidyverse)
library(sf)
library(raster)
library(fasterize)
library(landscapetools)
library(landscapemetrics)
library(tmap)

# importar dados ----------------------------------------------------------

# vetor ----

## download ----
for(i in c(".dbf", ".prj", ".shp", ".shx")){
    download.file(url = paste0("http://geo.fbds.org.br/SP/RIO_CLARO/USO/SP_3543907_USO", i),
                  destfile = paste0("03_data//SP_3543907_USO", i), mode = "wb")
}

## importar ----
uso <- sf::st_read("03_data/SP_3543907_USO.shp")
uso

# plot
tm_shape(uso) +
    tm_fill(col = "CLASSE_USO", title = "Legenda",
            pal = c("blue", "orange", "gray", "forestgreen", "green"))

# tabela de atributos
sf::st_drop_geometry(uso)

# criar uma coluna numerica para as classes de uso da terra
uso$classe_num <- factor(uso$CLASSE_USO)
sf::st_drop_geometry(uso)

# rasterizar --------------------------------------------------------------

## criar um raster vazio
ra <- fasterize::raster(uso, res = 30)
ra

# rasterizar
uso_raster <- fasterize::fasterize(sf = uso, raster = ra, field = "classe_num")
uso_raster

# mapa fasterize
tm_shape(uso_raster) +
    tm_raster(style = "cat", title = "Legenda",
              palette = c("blue", "orange", "gray", "forestgreen", "green")) +
    tm_grid(lines = FALSE, labels.rot = c(0, 90), labels.size = .8) +
    tm_compass(position = c(.73, .08)) +
    tm_scale_bar(position = c(.63, 0), text.size = .65) +
    tm_layout(legend.position = c("left", "bottom")) 

# buffers -----------------------------------------------------------------

# amostragens de campo
amost <- readr::read_csv("03_data/pontos_amostragem.csv")
amost

# amostragens vetoriais
amost_vetor <- sf::st_as_sf(x = co, coords = c("x", "y"), crs = raster::crs(uso))
amost_vetor

# buffers
buffers <- sf::st_buffer(x = amost_vetor, dist = 1000)
buffers

# mapa
tm_shape(uso_raster) +
    tm_raster(style = "cat", title = "Legenda",
              palette = c("blue", "orange", "gray", "forestgreen", "green")) +
    tm_shape(buffers) +
    tm_borders(col = "red", lwd = 2) +
    tm_shape(amost_vetor) +
    tm_dots(size = .7, shape = 20, alpha = .7) +
    tm_grid(lines = FALSE, labels.rot = c(0, 90), labels.size = .8) +
    tm_compass(position = c(.73, .08)) +
    tm_scale_bar(position = c(.63, 0), text.size = .65) +
    tm_layout(legend.position = c("left", "bottom")) 

# ajustar paisagens -------------------------------------------------------

# crop e mask das paisagens
paisagens <- raster::mask(uso_raster, buffers)
paisagens

tm_shape(paisagens) +
    tm_raster(style = "cat", title = "Legenda",
              palette = c("blue", "orange", "gray", "forestgreen", "green")) +
    tm_shape(buffers) +
    tm_borders(col = "red", lwd = 2) +
    tm_shape(amost_vetor) +
    tm_dots(size = .7, shape = 20, alpha = .7)

# crop e mask das paisagens individualmente
paisagens <- NULL

for(i in 1:10){
    
    # informacao
    print(paste0("Ajustando a paisagem ", i))
    
    # filter
    buffers_i <- buffers[i, ]
    
    # crop e mask
    paisagens[[i]] <- uso_raster %>% 
        raster::crop(buffers_i) %>% 
        raster::mask(buffers_i)
    
}

paisagens

# mapas
cores <- data.frame(val = freq(uso_raster)[1:5, 1],
                    col =  c("blue", "orange", "gray", "forestgreen", "green"))
cores

for(i in 1:10){
    
    # nomes
    nome_paisagem <- paste0("map_pai", i)
    nome_floresta <- paste0("map_for", i)
    
    # mapas
    assign(nome_paisagem, 
           tm_shape(paisagens[[i]]) +
               tm_raster(style = "cat", legend.show = FALSE,
                         palette = cores[cores$val %in% freq(paisagens[[i]])[, 1], 2]) +
               tm_shape(buffers[i, ]) +
               tm_borders(col = "red", lwd = 2) +
               tm_shape(amost_vetor) +
               tm_dots(size = .7, shape = 20, alpha = .7) +
               tm_layout(main.title = names(paisagens)[i])
    )
    
    assign(nome_floresta, 
           tm_shape(paisagens[[i]] == 4) +
               tm_raster(legend.show = FALSE,
                         pal = c("gray", "forestgreen")) +
               tm_shape(buffers[i, ]) +
               tm_borders(col = "red", lwd = 2) +
               tm_shape(amost_vetor) +
               tm_dots(size = .7, shape = 20, alpha = .7) +
               tm_layout(main.title = names(paisagens)[i])
    )
}

# todos os mapas
tmap::tmap_arrange(map_pai1, map_pai2, map_pai3, map_pai4, map_pai5, map_pai6, 
                   map_pai7, map_pai8, map_pai9, map_pai10)

tmap::tmap_arrange(map_for1, map_for2, map_for3, map_for4, map_for5, map_for6, 
                   map_for7, map_for8, map_for9, map_for10)

# checar o raster --------------------------------------------------------

# checar o raster
landscapemetrics::check_landscape(paisagens)

#' prerequisitos do raster
#' 1. sistema de referencias de coordenadas e projetada (crs)
#' 2. unidade esta em metros (units)
#' 3. classes como valores inteiros (class)
#' 4. numero de classes (n_class)

# listar as metricas ------------------------------------------------------
# metricas
all_metrics <- landscapemetrics::list_lsm()
all_metrics

# patch metrics
patch_metrics <- landscapemetrics::list_lsm() %>%
    dplyr::filter(level == "patch") %>% 
    dplyr::arrange(type)
patch_metrics

patch_metrics %>%
    dplyr::group_by(type) %>% 
    dplyr::summarise(n = n())

# class metrics
class_metrics <- landscapemetrics::list_lsm() %>%
    dplyr::filter(level == "class") %>% 
    dplyr::arrange(type)
class_metrics

class_metrics_type <- class_metrics %>%
    dplyr::group_by(type) %>% 
    dplyr::summarise(n = n())
class_metrics_type

# landscape metrics
landscape_metrics <- landscapemetrics::list_lsm() %>%
    dplyr::filter(level == "landscape") %>% 
    dplyr::arrange(type)
landscape_metrics

landscape_metrics_type <- landscape_metrics %>%
    dplyr::group_by(type) %>% 
    dplyr::summarise(n = n())
landscape_metrics_type

# calcular as metricas ----------------------------------------------------
#' estrutura das funcoes
#' 1. prefixo: ‘lsm_’
#' 2. nivel: ‘p’, ‘c’ e ‘l’ para patch‐, class‐ e landscape‐level
#' 3. metrica: patch area - ‘lsm_p_area’
#' 4. todas as funcoes funcionam para rasterlayers, rasterstack/rasterbrick ou list
#' 5. algumas funcoes permitem add parametros: edge depth ou cell neighbourhood rule

# area no nivel de mancha (patch - p)
area_p <- landscapemetrics::lsm_p_area(landscape = paisagens[[1]])
area_p

# area no nivel de classe (class - c)
area_c <- landscapemetrics::lsm_c_area_mn(landscape = paisagens[[1]])
area_c

# area no nivel de paisagem (landscape - l)
area_l <- landscapemetrics::lsm_l_area_mn(landscape = paisagens[[1]])
area_l

# calcular todas as metricas por nivel ------------------------------------

#' calculate_lsm()
#' calcula varias metricas simultaneamente
#' facilita a entrada de parametros
#' permite escolha por ‘level’, ‘metric’, ‘name’, ‘type’, ‘what’

# patch level
lsm_patch <- landscapemetrics::calculate_lsm(landscape = rc_raster_pai, 
                                             level = "patch", 
                                             edge_depth = 1, # celulas
                                             neighbourhood = 8, # oito celulas nas vizinhancas
                                             full_name = TRUE, 
                                             verbose = TRUE, 
                                             progress = TRUE)
lsm_patch

# class level
lsm_class <- landscapemetrics::calculate_lsm(landscape = rc_raster_pai, 
                                             level = "class", 
                                             edge_depth = 1, # celulas
                                             neighbourhood = 8, # oito celulas nas vizinhancas
                                             full_name = TRUE, 
                                             verbose = TRUE, 
                                             progress = TRUE)
lsm_class

# landscape level
lsm_landscape <- landscapemetrics::calculate_lsm(landscape = rc_raster_pai, 
                                                 level = "landscape",
                                                 edge_depth = 1, # celulas
                                                 neighbourhood = 8, # oito celulas nas vizinhancas
                                                 full_name = TRUE, 
                                                 verbose = TRUE, 
                                                 progress = TRUE)
lsm_landscape

# export
readr::write_csv(lsm_patch, "./metricas_tabelas/metricas_patch.csv")
readr::write_csv(lsm_class, "./metricas_tabelas/metricas_class.csv")
readr::write_csv(lsm_landscape, "./metricas_tabelas/metricas_landscape.csv")

# mapas -------------------------------------------------------------------
# plotar paisagem e metricas
landscapemetrics::show_patches(landscape = rc_raster_pai$paisagem_01, 
                               class = 4, labels = FALSE)

landscapemetrics::show_cores(rc_raster_pai$paisagem_01, class = 4, 
                             edge_depth = 1, labels = FALSE)

landscapemetrics::show_cores(rc_raster_pai$paisagem_01, class = 4, 
                             edge_depth = 2, labels = FALSE)

landscapemetrics::show_lsm(rc_raster_pai$paisagem_01, what = "lsm_p_area", class = 4, 
                           labels = FALSE)

# espacializar os valores das metricas ------------------------------------
# reclassificar
rc_raster_pai01_fo <- raster::reclassify(x = rc_raster_pai$paisagem_01, 
                                         rcl = c(0,3,NA, 3,4,1))
landscapetools::show_landscape(rc_raster_pai01_fo)

# calcular e espacializar
rc_raster_pai01_fo_patch <- landscapemetrics::spatialize_lsm(rc_raster_pai01_fo,
                                                             what = "patch", 
                                                             progress = TRUE)
rc_raster_pai01_fo_patch

# mapa
landscapetools::show_landscape(rc_raster_pai01_fo_patch[[1]]$lsm_p_area) +
    labs(title = "Área")

# exportar
for(i in 1:length(rc_raster_pai01_fo_patch[[1]])){
    
    # informacao
    print(paste0("Paisagem 01 - ", names(rc_raster_pai01_fo_patch[[1]][i])))
    
    # exportar
    raster::writeRaster(x = rc_raster_pai01_fo_patch[[1]][[i]],
                        filename = paste0("./metricas_raster/paisagem_01_", names(rc_raster_pai01_fo_patch[[1]][i])),
                        format = "GTiff",
                        options = c("COMPRESS=DEFLATE" , "TFW=TRUE"),
                        overwrite = TRUE)
}

# exemplo -----------------------------------------------------------------
# paisagens com floresta e água
# list
rc_raster_pai_flo_agu_sep <- list()
rc_raster_pai_flo_agu_sep

rc_raster_pai_flo_agu_jun <- list()
rc_raster_pai_flo_agu_jun

# reclassificar as paisagens das paisagens
for(i in 1:10){
    
    # informacao
    print(paste0("Ajustando a paisagem ", i))
    
    # reclassify
    rc_raster_pai_flo_agu_sep[[i]] <- raster::reclassify(x = rc_raster_pai[[i]],
                                                         rcl = c(0,1,1, 1,2,NA, 2,3,NA, 3,4,4, 4,5,NA))
    
    # reclassify
    rc_raster_pai_flo_agu_jun[[i]] <- raster::reclassify(x = rc_raster_pai[[i]],
                                                         rcl = c(0,1,1, 1,2,NA, 2,3,NA, 3,4,1, 4,5,NA))
    
}

# verificar
landscapetools::show_landscape(rc_raster_pai_flo_agu_sep[[1]])
landscapetools::show_landscape(rc_raster_pai_flo_agu_jun[[1]])

# metrica para tamanho e densidade de borda das lagoas
borda_lagoas <- landscapemetrics::calculate_lsm(landscape = rc_raster_pai_flo_agu_sep,
                                                what = "lsm_c_ed",
                                                edge_depth = 1) %>% 
    dplyr::filter(class == 1)
borda_lagoas

# metrica para tamanho de floresta
area_floresta <- landscapemetrics::calculate_lsm(landscape = rc_raster_pai_flo_agu_sep,
                                                 what = "lsm_c_ca") %>% 
    dplyr::filter(class == 4)
area_floresta

# distancia de lagos e florestas
dist_floresta_lago <- landscapemetrics::calculate_lsm(landscape = rc_raster_pai_flo_agu_jun,
                                                      what = c("lsm_l_enn_mn"))
dist_floresta_lago

# numero de especies por paisagem
sp_n <- c(5, 3, 6, 8, 5, 2, 0, 9, 4, 2)
sp_n

# data
da <- tibble::tibble(sp_n = sp_n,
                     borda_lagoa = borda_lagoas$value,
                     area_floresta = area_floresta$value,
                     dist_floresta_lago = dist_floresta_lago$value)
da

# modelos
mo_borda_lagoas <- glm(sp_n ~ borda_lagoa, data = da, family = "poisson")
broom::tidy(mo_borda_lagoas)

mo_area_floresta <- glm(sp_n ~ log10(area_floresta), data = da, family = "poisson")
broom::tidy(mo_area_floresta)

mo_dist_floresta_lago <- glm(sp_n ~ dist_floresta_lago, data = da, family = "poisson")
broom::tidy(mo_dist_floresta_lago)

# aicc
aic <- bbmle::ICtab(mo_borda_lagoas, mo_area_floresta, mo_dist_floresta_lago, 
                    type = "AICc",
                    weights = TRUE,
                    delta = TRUE,
                    logLik = TRUE,
                    sort = TRUE,
                    nobs = nrow(da))
aic

class(aic) <- "data.frame"
aic

write.csv(aic, "./modelos/aic_results.csv")

# graficos
ggplot(data = da) +
    aes(x = borda_lagoa, y = sp_n) +
    stat_smooth(method = "glm", method.args = list(family = "poisson"), col = "black", level = .95) +
    geom_point(shape = 21, size = 5, col = "black", fill = "blue", alpha = .8) +
    theme_classic() +
    labs(x = "Densidade de borda de lagoas", y = "Número de espécies") +
    theme(axis.title = element_text(size = 24),
          axis.text.x = element_text(size = 20),
          axis.text.y = element_text(size = 20),
          legend.title = element_text(size = 14),
          legend.text = element_text(size = 12))
ggsave("./modelos/modelo_borda_lagoa.png", he = 15, wi = 20, un = "cm", dpi = 300)

# graficos
ggplot(data = da) +
    aes(x = log10(area_floresta), y = sp_n) +
    stat_smooth(method = "glm", method.args = list(family = "poisson"), col = "black", level = .95) +
    geom_point(shape = 21, size = 5, col = "black", fill = "forestgreen", alpha = .8) +
    theme_classic() +
    labs(x = "Área de floresta (log10)", y = "Número de espécies") +
    theme(axis.title = element_text(size = 24),
          axis.text.x = element_text(size = 20),
          axis.text.y = element_text(size = 20),
          legend.title = element_text(size = 14),
          legend.text = element_text(size = 12))
ggsave("./modelos/modelo_area_flo.png", he = 15, wi = 20, un = "cm", dpi = 300)

# graficos
ggplot(data = da) +
    aes(x = dist_floresta_lago, y = sp_n) +
    stat_smooth(method = "glm", method.args = list(family = "poisson"), col = "black", level = .95) +
    geom_point(shape = 21, size = 5, col = "black", fill = "cyan4", alpha = .8) +
    theme_classic() +
    labs(x = "Distância de lagoas e florestas", y = "Número de espécies") +
    theme(axis.title = element_text(size = 24),
          axis.text.x = element_text(size = 20),
          axis.text.y = element_text(size = 20),
          legend.title = element_text(size = 14),
          legend.text = element_text(size = 12))
ggsave("./modelos/modelo_dist_flo_agu.png", he = 15, wi = 20, un = "cm", dpi = 300)

# end ---------------------------------------------------------------------