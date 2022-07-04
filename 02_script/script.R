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
library(landscapemetrics)
library(tmap)

# source
source("https://raw.githubusercontent.com/phuais/multifit/master/multifit.R")

# options
options(timeout = 600)

# importar dados ----------------------------------------------------------

# vetor ----

## download ----
for(i in c(".dbf", ".prj", ".shp", ".shx")){
    download.file(url = paste0("http://geo.fbds.org.br/SP/RIO_CLARO/USO/SP_3543907_USO", i),
                  destfile = paste0("03_data/SP_3543907_USO", i), mode = "wb")
}

## importar ----
uso <- sf::st_read("03_data/SP_3543907_USO.shp", quiet = TRUE)

# tabela de atributos
sf::st_drop_geometry(uso)

# plot
tm_shape(uso) +
    tm_fill(col = "CLASSE_USO", title = "Legenda",
            pal = c("blue", "orange", "gray", "forestgreen", "green"))

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
amost_vetor <- sf::st_as_sf(x = amost, coords = c("x", "y"), crs = raster::crs(uso))
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
paisagens_list <- NULL

for(i in 1:10){
    
    # informacao
    print(paste0("Ajustando a paisagem ", i))
    
    # filter
    buffers_i <- buffers[i, ]
    
    # crop e mask
    paisagens_list[[i]] <- uso_raster %>% 
        raster::crop(buffers_i) %>% 
        raster::mask(buffers_i)
    
}

paisagens_list

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
           tm_shape(paisagens_list[[i]]) +
               tm_raster(style = "cat", legend.show = FALSE,
                         palette = cores[cores$val %in% freq(paisagens_list[[i]])[, 1], 2]) +
               tm_shape(buffers[i, ]) +
               tm_borders(col = "red", lwd = 2) +
               tm_shape(amost_vetor) +
               tm_dots(size = .7, shape = 20, alpha = .7) +
               tm_layout(main.title = names(paisagens_list)[i])
    )
    
    assign(nome_floresta, 
           tm_shape(raster::reclassify(x = paisagens_list[[i]], rcl = c(0,3,NA, 3,4,1, 4,6,NA))) +
               tm_raster(legend.show = FALSE,
                         pal = "forestgreen") +
               tm_shape(buffers[i, ]) +
               tm_borders(col = "red", lwd = 2) +
               tm_shape(amost_vetor) +
               tm_dots(size = .7, shape = 20, alpha = .7) +
               tm_layout(main.title = names(paisagens_list)[i])
    )
    
}

# todos os mapas
tmap::tmap_arrange(map_pai1, map_pai2, map_pai3, map_pai4, map_pai5, map_pai6, 
                   map_pai7, map_pai8, map_pai9, map_pai10)

tmap::tmap_arrange(map_for1, map_for2, map_for3, map_for4, map_for5, map_for6, 
                   map_for7, map_for8, map_for9, map_for10)

# checar o raster --------------------------------------------------------

# checar o raster
landscapemetrics::check_landscape(paisagens_list)

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

# mapas -------------------------------------------------------------------

# plotar paisagem e metricas
landscapemetrics::show_patches(landscape = paisagens_list[[1]], 
                               class = 4, directions = 8)

landscapemetrics::show_patches(landscape = paisagens_list[[1]],
                               class = 4, directions = 4)

landscapemetrics::show_cores(landscape = paisagens_list[[1]], 
                             class = 4, edge_depth = 1)

landscapemetrics::show_cores(landscape = paisagens_list[[1]], 
                             class = 4, edge_depth = 2)

landscapemetrics::show_lsm(landscape = paisagens_list[[1]], 
                           what = "lsm_p_area", class = 4)

# calcular as metricas ----------------------------------------------------

#' estrutura das funcoes
#' 1. prefixo: ‘lsm_’
#' 2. nivel: ‘p’, ‘c’ e ‘l’ para patch, class e landscape level
#' 3. metrica: patch area - ‘lsm_p_area’
#' 4. todas as funcoes funcionam para rasterlayers, rasterstack/rasterbrick ou list
#' 5. algumas funcoes permitem add parametros: edge depth ou cell neighbourhood rule

# area no nivel de mancha (patch - p)
paisagens_area_p <- landscapemetrics::lsm_p_area(landscape = paisagens_list[[1]])
paisagens_area_p

paisagens_area_p_all <- landscapemetrics::lsm_p_area(landscape = paisagens_list)
paisagens_area_p_all

# area no nivel de classe (class - c)
paisagens_area_c <- landscapemetrics::lsm_c_area_mn(landscape = paisagens_list[[1]])
paisagens_area_c

paisagens_area_c_all <- landscapemetrics::lsm_c_area_mn(landscape = paisagens_list)
paisagens_area_c_all

# area no nivel de paisagem (landscape - l)
paisagens_area_l <- landscapemetrics::lsm_l_area_mn(landscape = paisagens_list[[1]])
paisagens_area_l

paisagens_area_l_all <- landscapemetrics::lsm_l_area_mn(landscape = paisagens_list)
paisagens_area_l_all

# calcular todas as metricas por nivel ------------------------------------

#' calculate_lsm()
#' calcula varias metricas simultaneamente
#' facilita a entrada de parametros
#' permite escolha por ‘level’, ‘metric’, ‘name’, ‘type’, ‘what’

# patch level
lsm_patch <- landscapemetrics::calculate_lsm(landscape = paisagens_list[[1]], 
                                             level = "patch", 
                                             edge_depth = 1, # celulas
                                             neighbourhood = 8, # oito celulas nas vizinhancas
                                             full_name = TRUE, 
                                             verbose = TRUE, 
                                             progress = TRUE)
lsm_patch

# class level
lsm_class <- landscapemetrics::calculate_lsm(landscape = paisagens_list[[1]], 
                                             level = "class", 
                                             edge_depth = 1, # celulas
                                             neighbourhood = 8, # oito celulas nas vizinhancas
                                             full_name = TRUE, 
                                             verbose = TRUE, 
                                             progress = TRUE)
lsm_class

# landscape level
lsm_landscape <- landscapemetrics::calculate_lsm(landscape = paisagens_list[[1]], 
                                                 level = "landscape",
                                                 edge_depth = 1, # celulas
                                                 neighbourhood = 8, # oito celulas nas vizinhancas
                                                 full_name = TRUE, 
                                                 verbose = TRUE, 
                                                 progress = TRUE)
lsm_landscape

# espacializar as metricas ------------------------------------

# reclassificar
paisagen01_forest <- raster::reclassify(x = paisagens_list[[1]], rcl = c(0,3,NA, 3,4,1))
paisagen01_forest

tm_shape(paisagen01_forest) +
    tm_raster(pal = "forestgreen", legend.show = FALSE)

# calcular e espacializar
paisagen01_forest_patch <- landscapemetrics::spatialize_lsm(paisagen01_forest,
                                                            what = "patch", 
                                                            progress = TRUE)
paisagen01_forest_patch

# mapa
tm_shape(paisagen01_forest_patch$layer_1$lsm_p_area) +
    tm_raster(pal = "viridis", title = "Área (ha)")

tm_shape(paisagen01_forest_patch$layer_1$lsm_p_shape) +
    tm_raster(pal = "viridis", title = "Formato")

tm_shape(paisagen01_forest_patch$layer_1$lsm_p_enn) +
    tm_raster(pal = "viridis", title = "Distância (m)")

# multiplas escalas -------------------------------------------------------

# tamanhos
tamanhos <- c(100, 200, 500, 1000)
tamanhos

# multiplos buffers
buffers_multi <- sf::st_buffer(x = sf::st_as_sf(purrr::map_dfr(amost_vetor, rep, 4)), 
                               dist = rep(tamanhos, each = 10))
buffers_multi$buffer <- rep(tamanhos, each = 10)
buffers_multi

# map
tm_shape(uso_raster) +
    tm_raster(style = "cat", title = "Legenda", alpha = .5,
              palette = c("blue", "orange", "gray", "forestgreen", "green")) +
    tm_shape(buffers_multi) +
    tm_borders(col = "red") 

# metricas multiplas escalas
metricas_multiplas_escalas <- tamanhos %>% 
    set_names() %>% 
    map_dfr(~sample_lsm(landscape = uso_raster, 
                        y = amost_vetor, 
                        shape = "circle",
                        size = .,
                        what = c("lsm_c_np", "lsm_c_area_mn", "lsm_c_pland"),
                        all_classes = TRUE,
                        return_raster = TRUE,
                        verbose = TRUE,
                        progress = TRUE), 
            .id = "buffer")
metricas_multiplas_escalas

# map
tm_shape(paisagens_list[[1]]) +
    tm_raster(style = "cat", title = "Legenda", alpha = .7,
              palette = c("blue", "orange", "forestgreen")) +
    tm_shape(buffers_multi) +
    tm_borders(col = "red") +
    tm_shape(amost_vetor[1, ]) +
    tm_bubbles()

tm_shape(metricas_multiplas_escalas[metricas_multiplas_escalas$layer == 1 &
                                        metricas_multiplas_escalas$buffer == 100, ]$raster_sample_plots[[1]], 
         bbox = buffers_multi[31,]) +
    tm_raster(style = "cat", title = "Legenda", alpha = .7,
              palette = c("blue", "orange", "forestgreen")) +
    tm_shape(buffers_multi) +
    tm_borders(col = "red")  +
    tm_shape(amost_vetor[1, ]) +
    tm_bubbles()

tm_shape(metricas_multiplas_escalas[metricas_multiplas_escalas$layer == 1 &
                                        metricas_multiplas_escalas$buffer == 200, ]$raster_sample_plots[[1]], 
         bbox = buffers_multi[31,]) +
    tm_raster(style = "cat", title = "Legenda", alpha = .7,
              palette = c("blue", "orange", "forestgreen")) +
    tm_shape(buffers_multi) +
    tm_borders(col = "red")  +
    tm_shape(amost_vetor[1, ]) +
    tm_bubbles()

tm_shape(metricas_multiplas_escalas[metricas_multiplas_escalas$layer == 1 &
                                        metricas_multiplas_escalas$buffer == 500, ]$raster_sample_plots[[1]], 
         bbox = buffers_multi[31,]) +
    tm_raster(style = "cat", title = "Legenda", alpha = .7,
              palette = c("blue", "orange", "forestgreen")) +
    tm_shape(buffers_multi) +
    tm_borders(col = "red")  +
    tm_shape(amost_vetor[1, ]) +
    tm_bubbles()

tm_shape(metricas_multiplas_escalas[metricas_multiplas_escalas$layer == 1 &
                                        metricas_multiplas_escalas$buffer == 1000, ]$raster_sample_plots[[1]], 
         bbox = buffers_multi[31,]) +
    tm_raster(style = "cat", title = "Legenda", alpha = .7,
              palette = c("blue", "orange", "forestgreen")) +
    tm_shape(buffers_multi) +
    tm_borders(col = "red")  +
    tm_shape(amost_vetor[1, ]) +
    tm_bubbles()

# multifit -----------------------------------------------------------------

# numero de especies por paisagem
n_sp <- tibble::tibble(id = 1:10, s = c(5, 3, 6, 5, 3, 2, 1, 10, 7, 4))
n_sp

# preparar os dados numero de manchas
data_np <- metricas_multiplas_escalas %>% 
    dplyr::filter(metric == "np",
                  class == 4) %>% 
    dplyr::mutate(value = ifelse(is.na(value), 0, value)) %>% 
    tidyr::pivot_wider(id_cols = plot_id, 
                       names_from = c(metric, buffer), 
                       values_from = value) %>% 
    dplyr::right_join(n_sp, ., by = c("id" = "plot_id"))
data_np

fits_np <- multifit(data = data_np,
                    mod = "glm", 
                    multief = colnames(data_np)[3:6], 
                    formula = s ~ multief, 
                    criterion = "AIC",
                    plot_est = TRUE)
fits_np

fits_np$summary
fits_np$plot
fits_np$models

# preparar os dados area
data_area <- metricas_multiplas_escalas %>% 
    dplyr::filter(metric == "area_mn",
                  class == 4) %>% 
    dplyr::mutate(value = ifelse(is.na(value), 0, value)) %>% 
    tidyr::pivot_wider(id_cols = plot_id, 
                       names_from = c(metric, buffer), 
                       values_from = value) %>% 
    dplyr::right_join(n_sp, ., by = c("id" = "plot_id"))
data_area

fits_area <- multifit(data = data_area,
                      mod = "glm", 
                      multief = colnames(data_area)[3:6], 
                      formula = s ~ multief, 
                      criterion = "AIC",
                      plot_est = TRUE)
fits_area

fits_area$summary
fits_area$plot
fits_area$models

# preparar os dados pland
data_pland <- metricas_multiplas_escalas %>% 
    dplyr::filter(metric == "pland",
                  class == 4) %>% 
    dplyr::mutate(value = ifelse(is.na(value), 0, value)) %>% 
    tidyr::pivot_wider(id_cols = plot_id, 
                       names_from = c(metric, buffer), 
                       values_from = value) %>% 
    dplyr::right_join(n_sp, ., by = c("id" = "plot_id"))
data_pland

fits_pland <- multifit(data = data_pland,
                       mod = "glm", 
                       multief = colnames(data_pland)[3:6], 
                       formula = s ~ multief, 
                       criterion = "AIC",
                       plot_est = TRUE)
fits_pland

fits_pland$summary
fits_pland$plot
fits_pland$models


# modelos finais
fits_np$models$np_100
fits_area$models$area_mn_200
fits_pland$models$pland_500

broom::tidy(fits_np$models$np_100)
broom::tidy(fits_area$models$area_mn_200)
broom::tidy(fits_pland$models$pland_500)

# aicc
aic <- bbmle::ICtab(fits_np$models$np_100,
                    fits_area$models$area_mn_200,
                    fits_pland$models$pland_500, 
                    type = "AICc",
                    weights = TRUE,
                    delta = TRUE,
                    logLik = TRUE,
                    sort = TRUE,
                    nobs = 10)
aic

# graficos
ggplot(data = data_pland) +
    aes(x = pland_500, y = s) +
    stat_smooth(method = "glm", method.args = list(family = "poisson"), col = "black", level = .95) +
    geom_point(shape = 21, size = 5, col = "black", fill = "blue", alpha = .8) +
    theme_classic() +
    labs(x = "Porcentagem de habitat (%) - 500 m", y = "Número de espécies") +
    theme(axis.title = element_text(size = 24),
          axis.text.x = element_text(size = 20),
          axis.text.y = element_text(size = 20),
          legend.title = element_text(size = 14),
          legend.text = element_text(size = 12))

ggplot(data = data_area) +
    aes(x = area_mn_200, y = s) +
    stat_smooth(method = "glm", method.args = list(family = "poisson"), col = "black", level = .95) +
    geom_point(shape = 21, size = 5, col = "black", fill = "forestgreen", alpha = .8) +
    theme_classic() +
    labs(x = "Área média das manchas (ha) - 200 m", y = "Número de espécies") +
    theme(axis.title = element_text(size = 24),
          axis.text.x = element_text(size = 20),
          axis.text.y = element_text(size = 20),
          legend.title = element_text(size = 14),
          legend.text = element_text(size = 12))

# end ---------------------------------------------------------------------