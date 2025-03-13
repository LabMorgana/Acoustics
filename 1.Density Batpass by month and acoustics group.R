                                  #########################################################
###### Script for the analysis of the density of the Congo bats as well as the graphs presented in the publication #####
                                     #########################################################
## Load package 

pacman::p_load(tidyverse, FactoMineR, factoextra, ggplot2, openxlsx, lubridate, ggradar, readxl, tidyr, stringr, glmmTMB, DHARMa)

####### Load data ######### -----

selected_rows <- read.csv(file = "~/Downloads/Bat_All_acoustics_transf_date_bothcave.csv", header = T, sep = ",")

## add columns of time decimal modified
selected_rows$times_in_decimal_modified <- ifelse(selected_rows$times_in_decimal >= 0 & selected_rows$times_in_decimal <= 7, selected_rows$times_in_decimal + 24, selected_rows$times_in_decimal)

### by group and sites ----------

## MB

# Hippo ------
Hippo_mb <- selected_rows |>
  filter(SpMaxF2 %in% "CF-FMd") |>
  filter(Sites == "Mont Belo") |>
  dplyr::filter(dplyr::between(MainMode, 125, 160)) |>
  dplyr::filter(dplyr::between(Ramp90, -20, 0))

## By month
Hippo_mb_jan <- Hippo_mb |>
  filter(month == 1)

Hippo_mb_feb <- Hippo_mb |>
  filter(month == 2)

Hippo_mb_mar <- Hippo_mb |>
  filter(month == 3)

Hippo_mb_apr <- Hippo_mb |>
  filter(month == 4)

Hippo_mb_may <- Hippo_mb |>
  filter(month == 5)

Hippo_mb_jun <- Hippo_mb |>
  filter(month == 6)

Hippo_mb_jul <- Hippo_mb |>
  filter(month == 7)

Hippo_mb_aug <- Hippo_mb |>
  filter(month == 8)

Hippo_mb_sept <- Hippo_mb |>
  filter(month == 9)

Hippo_mb_oct <- Hippo_mb |>
  filter(month == 10)

Hippo_mb_nov <- Hippo_mb |>
  filter(month == 11)

Hippo_mb_dec <- Hippo_mb |>
  filter(month == 12)

### With density jan
density_mb_hippo_jan <- density(as.numeric(Hippo_mb_jan$times_in_decimal_modified), bw= "SJ")

density_mb_hippo_jan <- data.frame(x=density_mb_hippo_jan$x,y=density_mb_hippo_jan$y) 

### With density feb
density_mb_hippo_feb <- density(as.numeric(Hippo_mb_feb$times_in_decimal_modified), bw= "SJ")

density_mb_hippo_feb <- data.frame(x=density_mb_hippo_feb$x,y=density_mb_hippo_feb$y) 

### With density mar
density_mb_hippo_mar <- density(as.numeric(Hippo_mb_mar$times_in_decimal_modified), bw= "SJ")

density_mb_hippo_mar <- data.frame(x=density_mb_hippo_mar$x,y=density_mb_hippo_mar$y) 

### With density apr
density_mb_hippo_apr <- density(as.numeric(Hippo_mb_apr$times_in_decimal_modified), bw= "SJ")

density_mb_hippo_apr <- data.frame(x=density_mb_hippo_apr$x,y=density_mb_hippo_apr$y) 

### With density may
density_mb_hippo_may <- density(as.numeric(Hippo_mb_may$times_in_decimal_modified), bw= "SJ")

density_mb_hippo_may <- data.frame(x=density_mb_hippo_may$x,y=density_mb_hippo_may$y) 

### With density jun
density_mb_hippo_jun <- density(as.numeric(Hippo_mb_jun$times_in_decimal_modified), bw= "SJ")

density_mb_hippo_jun <- data.frame(x=density_mb_hippo_jun$x,y=density_mb_hippo_jun$y) 

### With density jul
density_mb_hippo_jul <- density(as.numeric(Hippo_mb_jul$times_in_decimal_modified), bw= "SJ")

density_mb_hippo_jul <- data.frame(x=density_mb_hippo_jul$x,y=density_mb_hippo_jul$y) 

### With density aug
density_mb_hippo_aug <- density(as.numeric(Hippo_mb_aug$times_in_decimal_modified), bw= "SJ")

density_mb_hippo_aug <- data.frame(x=density_mb_hippo_aug$x,y=density_mb_hippo_aug$y) 

### With density sept
density_mb_hippo_sep <- density(as.numeric(Hippo_mb_sept$times_in_decimal_modified), bw= "SJ")

density_mb_hippo_sep <- data.frame(x=density_mb_hippo_sep$x,y=density_mb_hippo_sep$y) 

### With density oct
density_mb_hippo_oct <- density(as.numeric(Hippo_mb_oct$times_in_decimal_modified), bw= "SJ")

density_mb_hippo_oct <- data.frame(x=density_mb_hippo_oct$x,y=density_mb_hippo_oct$y) 

### With density nov
density_mb_hippo_nov <- density(as.numeric(Hippo_mb_nov$times_in_decimal_modified), bw= "SJ")

density_mb_hippo_nov <- data.frame(x=density_mb_hippo_nov$x,y=density_mb_hippo_nov$y) 

### With density dec
density_mb_hippo_dec <- density(as.numeric(Hippo_mb_dec$times_in_decimal_modified), bw= "SJ")

density_mb_hippo_dec <- data.frame(x=density_mb_hippo_dec$x,y=density_mb_hippo_dec$y) 



#### Minio group ------
Minio_35107kHz_mb <- selected_rows  %>%
  filter(SpMaxF2 %in% "FMd-QCF") |>
  filter(Sites == "Mont Belo") |>
  dplyr::filter(dplyr::between(MainMode, 39, 107)) |> 
  dplyr::filter(dplyr::between(Ramp90, -20, 1)) |> 
  dplyr::filter(dplyr::between(Ind, 0.2, 1)) 

Minio_QCF_mb <- selected_rows  %>%
  filter(SpMaxF2 %in% "QCF-FMd") |>
  filter(Sites == "Mont Belo") |>
  dplyr::filter(dplyr::between(MainMode, 55, 65)) |> 
  dplyr::filter(dplyr::between(Ramp90, -20, -0.1)) |> 
  dplyr::filter(dplyr::between(Ind, 0.2, 1)) 

Minio_all_mb <- merge(Minio_35107kHz_mb, Minio_QCF_mb, all.x = T, all.y=T)

Minio_Allac_mb <- Minio_all_mb[!duplicated(Minio_all_mb[c('Sites', 'Date_Night2')]), ]

## By month
Minio_mb_jan <- Minio_Allac_mb |>
  filter(month == 1)

Minio_mb_feb <- Minio_Allac_mb |>
  filter(month == 2)

Minio_mb_mar <- Minio_Allac_mb |>
  filter(month == 3)

Minio_mb_apr <- Minio_Allac_mb |>
  filter(month == 4)

Minio_mb_may <- Minio_Allac_mb |>
  filter(month == 5)

Minio_mb_jun <- Minio_Allac_mb |>
  filter(month == 6)

Minio_mb_jul <- Minio_Allac_mb |>
  filter(month == 7)

Minio_mb_aug <- Minio_Allac_mb |>
  filter(month == 8)

Minio_mb_sept <- Minio_Allac_mb |>
  filter(month == 9)

Minio_mb_oct <- Minio_Allac_mb |>
  filter(month == 10)

Minio_mb_nov <- Minio_Allac_mb |>
  filter(month == 11)

Minio_mb_dec <- Minio_Allac_mb |>
  filter(month == 12)

### With density jan
density_mb_Minio_jan <- density(as.numeric(Minio_mb_jan$times_in_decimal_modified), bw= "SJ")

density_mb_Minio_jan <- data.frame(x=density_mb_Minio_jan$x,y=density_mb_Minio_jan$y) 

### With density feb
density_mb_Minio_feb <- density(as.numeric(Minio_mb_feb$times_in_decimal_modified), bw= "SJ")

density_mb_Minio_feb <- data.frame(x=density_mb_Minio_feb$x,y=density_mb_Minio_feb$y) 

### With density mar
density_mb_Minio_mar <- density(as.numeric(Minio_mb_mar$times_in_decimal_modified), bw= "SJ")

density_mb_Minio_mar <- data.frame(x=density_mb_Minio_mar$x,y=density_mb_Minio_mar$y) 

### With density apr
density_mb_Minio_apr <- density(as.numeric(Minio_mb_apr$times_in_decimal_modified), bw= "SJ")

density_mb_Minio_apr <- data.frame(x=density_mb_Minio_apr$x,y=density_mb_Minio_apr$y) 

### With density may
density_mb_Minio_may <- density(as.numeric(Minio_mb_may$times_in_decimal_modified), bw= "SJ")

density_mb_Minio_may <- data.frame(x=density_mb_Minio_may$x,y=density_mb_Minio_may$y) 

### With density jun
density_mb_Minio_jun <- density(as.numeric(Minio_mb_jun$times_in_decimal_modified), bw= "SJ")

density_mb_Minio_jun <- data.frame(x=density_mb_Minio_jun$x,y=density_mb_Minio_jun$y) 

### With density jul
density_mb_Minio_jul <- density(as.numeric(Minio_mb_jul$times_in_decimal_modified), bw= "SJ")

density_mb_Minio_jul <- data.frame(x=density_mb_Minio_jul$x,y=density_mb_Minio_jul$y) 

### With density aug
density_mb_Minio_aug <- density(as.numeric(Minio_mb_aug$times_in_decimal_modified), bw= "SJ")

density_mb_Minio_aug <- data.frame(x=density_mb_Minio_aug$x,y=density_mb_Minio_aug$y) 

### With density sept
density_mb_Minio_sep <- density(as.numeric(Minio_mb_sept$times_in_decimal_modified), bw= "SJ")

density_mb_Minio_sep <- data.frame(x=density_mb_Minio_sep$x,y=density_mb_Minio_sep$y) 

### With density oct
density_mb_Minio_oct <- density(as.numeric(Minio_mb_oct$times_in_decimal_modified), bw= "SJ")

density_mb_Minio_oct <- data.frame(x=density_mb_Minio_oct$x,y=density_mb_Minio_oct$y) 

### With density nov
density_mb_Minio_nov <- density(as.numeric(Minio_mb_nov$times_in_decimal_modified), bw= "SJ")

density_mb_Minio_nov <- data.frame(x=density_mb_Minio_nov$x,y=density_mb_Minio_nov$y) 

### With density dec
density_mb_Minio_dec <- density(as.numeric(Minio_mb_dec$times_in_decimal_modified), bw= "SJ")

density_mb_Minio_dec <- data.frame(x=density_mb_Minio_dec$x,y=density_mb_Minio_dec$y) 


## Rhinolophus landeri-alcyone ----------------

Rhino_la_mb <- selected_rows  %>%
  filter(SpMaxF2 %in% "FMu-CF-FMd") |>
  filter(Sites %in% "Mont Belo") |>
  dplyr::filter(dplyr::between(MainMode, 45, 62)) |> 
  dplyr::filter(dplyr::between(Ramp90, -20, 0))

Rhino_la_om_mb <- selected_rows  %>%
  filter(SpMaxF2 %in% "other-mammal") |>
  filter(Sites %in% "Mont Belo") |>
  dplyr::filter(dplyr::between(MainMode, 45, 62)) |> 
  dplyr::filter(dplyr::between(Ramp90, -20, 0))

Rhino_la_all_mb <- merge(Rhino_la_mb, Rhino_la_om_mb, all.x = T, all.y=T)

Rhino_la_Allac_mb <- Rhino_la_all_mb[!duplicated(Rhino_la_all_mb[c('Sites', 'Date_Night2')]), ]


## By month
Rhino_la_mb_jan <- Rhino_la_Allac_mb |>
  filter(month == 1)

Rhino_la_mb_feb <- Rhino_la_Allac_mb |>
  filter(month == 2)

Rhino_la_mb_mar <- Rhino_la_Allac_mb |>
  filter(month == 3)

Rhino_la_mb_apr <- Rhino_la_Allac_mb |>
  filter(month == 4)

Rhino_la_mb_may <- Rhino_la_Allac_mb |>
  filter(month == 5)

Rhino_la_mb_jun <- Rhino_la_Allac_mb |>
  filter(month == 6)

Rhino_la_mb_jul <- Rhino_la_Allac_mb |>
  filter(month == 7)

Rhino_la_mb_aug <- Rhino_la_Allac_mb |>
  filter(month == 8)

Rhino_la_mb_sept <- Rhino_la_Allac_mb |>
  filter(month == 9)

Rhino_la_mb_oct <- Rhino_la_Allac_mb |>
  filter(month == 10)

Rhino_la_mb_nov <- Rhino_la_Allac_mb |>
  filter(month == 11)

Rhino_la_mb_dec <- Rhino_la_Allac_mb |>
  filter(month == 12)

### With density jan
density_mb_Rhino_la_jan <- density(as.numeric(Rhino_la_mb_jan$times_in_decimal_modified), bw= "SJ")

density_mb_Rhino_la_jan <- data.frame(x=density_mb_Rhino_la_jan$x,y=density_mb_Rhino_la_jan$y) 

### With density feb
density_mb_Rhino_la_feb <- density(as.numeric(Rhino_la_mb_feb$times_in_decimal_modified), bw= "SJ")

density_mb_Rhino_la_feb <- data.frame(x=density_mb_Rhino_la_feb$x,y=density_mb_Rhino_la_feb$y) 

### With density mar
density_mb_Rhino_la_mar <- density(as.numeric(Rhino_la_mb_mar$times_in_decimal_modified), bw= "SJ")

density_mb_Rhino_la_mar <- data.frame(x=density_mb_Rhino_la_mar$x,y=density_mb_Rhino_la_mar$y) 

### With density apr
density_mb_Rhino_la_apr <- density(as.numeric(Rhino_la_mb_apr$times_in_decimal_modified), bw= "SJ")

density_mb_Rhino_la_apr <- data.frame(x=density_mb_Rhino_la_apr$x,y=density_mb_Rhino_la_apr$y) 

### With density may
density_mb_Rhino_la_may <- density(as.numeric(Rhino_la_mb_may$times_in_decimal_modified), bw= "SJ")

density_mb_Rhino_la_may <- data.frame(x=density_mb_Rhino_la_may$x,y=density_mb_Rhino_la_may$y) 

### With density jun
density_mb_Rhino_la_jun <- density(as.numeric(Rhino_la_mb_jun$times_in_decimal_modified), bw= "SJ")

density_mb_Rhino_la_jun <- data.frame(x=density_mb_Rhino_la_jun$x,y=density_mb_Rhino_la_jun$y) 

### With density jul
density_mb_Rhino_la_jul <- density(as.numeric(Rhino_la_mb_jul$times_in_decimal_modified), bw= "SJ")

density_mb_Rhino_la_jul <- data.frame(x=density_mb_Rhino_la_jul$x,y=density_mb_Rhino_la_jul$y) 

### With density aug
density_mb_Rhino_la_aug <- density(as.numeric(Rhino_la_mb_aug$times_in_decimal_modified), bw= "SJ")

density_mb_Rhino_la_aug <- data.frame(x=density_mb_Rhino_la_aug$x,y=density_mb_Rhino_la_aug$y) 

### With density sept
density_mb_Rhino_la_sep <- density(as.numeric(Rhino_la_mb_sept$times_in_decimal_modified), bw= "SJ")

density_mb_Rhino_la_sep <- data.frame(x=density_mb_Rhino_la_sep$x,y=density_mb_Rhino_la_sep$y) 

### With density oct
density_mb_Rhino_la_oct <- density(as.numeric(Rhino_la_mb_oct$times_in_decimal_modified), bw= "SJ")

density_mb_Rhino_la_oct <- data.frame(x=density_mb_Rhino_la_oct$x,y=density_mb_Rhino_la_oct$y) 

### With density nov
density_mb_Rhino_la_nov <- density(as.numeric(Rhino_la_mb_nov$times_in_decimal_modified), bw= "SJ")

density_mb_Rhino_la_nov <- data.frame(x=density_mb_Rhino_la_nov$x,y=density_mb_Rhino_la_nov$y) 

### With density dec
density_mb_Rhino_la_dec <- density(as.numeric(Rhino_la_mb_dec$times_in_decimal_modified), bw= "SJ")

density_mb_Rhino_la_dec <- data.frame(x=density_mb_Rhino_la_dec$x,y=density_mb_Rhino_la_dec$y) 


## Rhinolophus denti group-------------------

Rhino_de_mb <- selected_rows  %>%
  filter(SpMaxF2 %in% "FMu-CF-FMd") |>
  dplyr::filter(dplyr::between(MainMode, 95, 110)) |> 
  dplyr::filter(dplyr::between(Ramp90, -20, -0.1)) |> 
  dplyr::filter(dplyr::between(Ind, 0.35, 1)) 

Rhino_de_om_mb <- selected_rows  %>%
  filter(SpMaxF2 %in% "other-mammal") |>
  dplyr::filter(dplyr::between(MainMode, 95, 110)) |> 
  dplyr::filter(dplyr::between(Ramp90, -20, -0.1)) |> 
  dplyr::filter(dplyr::between(Ind, 0.35, 1))

Rhino_de_all_mb <- merge(Rhino_de_mb, Rhino_de_om_mb, all.x = T, all.y=T)

Rhino_de_Allac_mb <- Rhino_de_all_mb[!duplicated(Rhino_de_all_mb[c('Sites', 'Date_Night2')]), ]

## By month
Rhino_de_mb_jan <- Rhino_de_Allac_mb |>
  filter(month == 1)

Rhino_de_mb_feb <- Rhino_de_Allac_mb |>
  filter(month == 2)

Rhino_de_mb_mar <- Rhino_de_Allac_mb |>
  filter(month == 3)

Rhino_de_mb_apr <- Rhino_de_Allac_mb |>
  filter(month == 4)

Rhino_de_mb_may <- Rhino_de_Allac_mb |>
  filter(month == 5)

Rhino_de_mb_jun <- Rhino_de_Allac_mb |>
  filter(month == 6)

Rhino_de_mb_jul <- Rhino_de_Allac_mb |>
  filter(month == 7)

Rhino_de_mb_aug <- Rhino_de_Allac_mb |>
  filter(month == 8)

Rhino_de_mb_sept <- Rhino_de_Allac_mb |>
  filter(month == 9)

Rhino_de_mb_oct <- Rhino_de_Allac_mb |>
  filter(month == 10)

Rhino_de_mb_nov <- Rhino_de_Allac_mb |>
  filter(month == 11)

Rhino_de_mb_dec <- Rhino_de_Allac_mb |>
  filter(month == 12)

### With density jan
density_mb_Rhino_de_jan <- density(as.numeric(Rhino_de_mb_jan$times_in_decimal_modified), bw= "SJ")

density_mb_Rhino_de_jan <- data.frame(x=density_mb_Rhino_de_jan$x,y=density_mb_Rhino_de_jan$y) 
### With density feb
density_mb_Rhino_de_feb <- density(as.numeric(Rhino_de_mb_feb$times_in_decimal_modified), bw= "SJ")

density_mb_Rhino_de_feb <- data.frame(x=density_mb_Rhino_de_feb$x,y=density_mb_Rhino_de_feb$y) 

### With density mar
density_mb_Rhino_de_mar <- density(as.numeric(Rhino_de_mb_mar$times_in_decimal_modified), bw= "SJ")

density_mb_Rhino_de_mar <- data.frame(x=density_mb_Rhino_de_mar$x,y=density_mb_Rhino_de_mar$y) 

### With density apr
density_mb_Rhino_de_apr <- density(as.numeric(Rhino_de_mb_apr$times_in_decimal_modified), bw= "SJ")

density_mb_Rhino_de_apr <- data.frame(x=density_mb_Rhino_de_apr$x,y=density_mb_Rhino_de_apr$y) 

### With density may
density_mb_Rhino_de_may <- density(as.numeric(Rhino_de_mb_may$times_in_decimal_modified), bw= "SJ")

density_mb_Rhino_de_may <- data.frame(x=density_mb_Rhino_de_may$x,y=density_mb_Rhino_de_may$y) 

### With density jun
density_mb_Rhino_de_jun <- density(as.numeric(Rhino_de_mb_jun$times_in_decimal_modified), bw= "SJ")

density_mb_Rhino_de_jun <- data.frame(x=density_mb_Rhino_de_jun$x,y=density_mb_Rhino_de_jun$y) 

### With density jul
density_mb_Rhino_de_jul <- density(as.numeric(Rhino_de_mb_jul$times_in_decimal_modified), bw= "SJ")

density_mb_Rhino_de_jul <- data.frame(x=density_mb_Rhino_de_jul$x,y=density_mb_Rhino_de_jul$y) 

### With density aug
density_mb_Rhino_de_aug <- density(as.numeric(Rhino_de_mb_aug$times_in_decimal_modified), bw= "SJ")

density_mb_Rhino_de_aug <- data.frame(x=density_mb_Rhino_de_aug$x,y=density_mb_Rhino_de_aug$y) 

### With density sept
density_mb_Rhino_de_sep <- density(as.numeric(Rhino_de_mb_sept$times_in_decimal_modified), bw= "SJ")

density_mb_Rhino_de_sep <- data.frame(x=density_mb_Rhino_de_sep$x,y=density_mb_Rhino_de_sep$y) 

### With density oct
density_mb_Rhino_de_oct <- density(as.numeric(Rhino_de_mb_oct$times_in_decimal_modified), bw= "SJ")

density_mb_Rhino_de_oct <- data.frame(x=density_mb_Rhino_de_oct$x,y=density_mb_Rhino_de_oct$y) 

### With density nov
density_mb_Rhino_de_nov <- density(as.numeric(Rhino_de_mb_nov$times_in_decimal_modified), bw= "SJ")

density_mb_Rhino_de_nov <- data.frame(x=density_mb_Rhino_de_nov$x,y=density_mb_Rhino_de_nov$y) 
### With density dec
density_mb_Rhino_de_dec <- density(as.numeric(Rhino_de_mb_dec$times_in_decimal_modified), bw= "SJ")

density_mb_Rhino_de_dec <- data.frame(x=density_mb_Rhino_de_dec$x,y=density_mb_Rhino_de_dec$y) 


## For BOUNDOU CAVE -----------

# Hipposideros group
Hippo_bd <- selected_rows |>
  filter(SpMaxF2 %in% "CF-FMd") |>
  filter(Sites == "Boundou") |>
  dplyr::filter(dplyr::between(MainMode, 125, 160)) |>
  dplyr::filter(dplyr::between(Ramp90, -20, 0))

## By month
Hippo_bd_jan <- Hippo_bd |>
  filter(month == 1)

Hippo_bd_feb <- Hippo_bd |>
  filter(month == 2)

Hippo_bd_mar <- Hippo_bd |>
  filter(month == 3)

Hippo_bd_apr <- Hippo_bd |>
  filter(month == 4)

Hippo_bd_may <- Hippo_bd |>
  filter(month == 5)

Hippo_bd_jun <- Hippo_bd |>
  filter(month == 6)

Hippo_bd_jul <- Hippo_bd |>
  filter(month == 7)

Hippo_bd_aug <- Hippo_bd |>
  filter(month == 8)

Hippo_bd_sept <- Hippo_bd |>
  filter(month == 9)

Hippo_bd_oct <- Hippo_bd |>
  filter(month == 10)

Hippo_bd_nov <- Hippo_bd |>
  filter(month == 11)

Hippo_bd_dec <- Hippo_bd |>
  filter(month == 12)

### With density jan
density_bd_hippo_jan <- density(as.numeric(Hippo_bd_jan$times_in_decimal_modified), bw= "SJ")

density_bd_hippo_jan <- data.frame(x=density_bd_hippo_jan$x,y=density_bd_hippo_jan$y) 

### With density feb
density_bd_hippo_feb <- density(as.numeric(Hippo_bd_feb$times_in_decimal_modified), bw= "SJ")

density_bd_hippo_feb <- data.frame(x=density_bd_hippo_feb$x,y=density_bd_hippo_feb$y) 

### With density mar
density_bd_hippo_mar <- density(as.numeric(Hippo_bd_mar$times_in_decimal_modified), bw= "SJ")

density_bd_hippo_mar <- data.frame(x=density_bd_hippo_mar$x,y=density_bd_hippo_mar$y) 

### With density apr
density_bd_hippo_apr <- density(as.numeric(Hippo_bd_apr$times_in_decimal_modified), bw= "SJ")

density_bd_hippo_apr <- data.frame(x=density_bd_hippo_apr$x,y=density_bd_hippo_apr$y) 

### With density may
density_bd_hippo_may <- density(as.numeric(Hippo_bd_may$times_in_decimal_modified), bw= "SJ")

density_bd_hippo_may <- data.frame(x=density_bd_hippo_may$x,y=density_bd_hippo_may$y) 

### With density jun
density_bd_hippo_jun <- density(as.numeric(Hippo_bd_jun$times_in_decimal_modified), bw= "SJ")

density_bd_hippo_jun <- data.frame(x=density_bd_hippo_jun$x,y=density_bd_hippo_jun$y) 

### With density jul
density_bd_hippo_jul <- density(as.numeric(Hippo_bd_jul$times_in_decimal_modified), bw= "SJ")

density_bd_hippo_jul <- data.frame(x=density_bd_hippo_jul$x,y=density_bd_hippo_jul$y) 

### With density aug
density_bd_hippo_aug <- density(as.numeric(Hippo_bd_aug$times_in_decimal_modified), bw= "SJ")

density_bd_hippo_aug <- data.frame(x=density_bd_hippo_aug$x,y=density_bd_hippo_aug$y) 

### With density sept
density_bd_hippo_sep <- density(as.numeric(Hippo_bd_sept$times_in_decimal_modified), bw= "SJ")

density_bd_hippo_sep <- data.frame(x=density_bd_hippo_sep$x,y=density_bd_hippo_sep$y) 

### With density oct
density_bd_hippo_oct <- density(as.numeric(Hippo_bd_oct$times_in_decimal_modified), bw= "SJ")

density_bd_hippo_oct <- data.frame(x=density_bd_hippo_oct$x,y=density_bd_hippo_oct$y) 

### With density nov
density_bd_hippo_nov <- density(as.numeric(Hippo_bd_nov$times_in_decimal_modified), bw= "SJ")

density_bd_hippo_nov <- data.frame(x=density_bd_hippo_nov$x,y=density_bd_hippo_nov$y) 

### With density dec
density_bd_hippo_dec <- density(as.numeric(Hippo_bd_dec$times_in_decimal_modified), bw= "SJ")

density_bd_hippo_dec <- data.frame(x=density_bd_hippo_dec$x,y=density_bd_hippo_dec$y) 


# Miniopterus group

Minio_35107kHz_bd <- selected_rows  %>%
  filter(SpMaxF2 %in% "FMd-QCF") |>
  filter(Sites == "Boundou") |>
  dplyr::filter(dplyr::between(MainMode, 39, 107)) |> 
  dplyr::filter(dplyr::between(Ramp90, -20, 1)) |> 
  dplyr::filter(dplyr::between(Ind, 0.2, 1)) 

Minio_QCF_bd <- selected_rows  %>%
  filter(SpMaxF2 %in% "QCF-FMd") |>
  filter(Sites == "Boundou") |>
  dplyr::filter(dplyr::between(MainMode, 55, 65)) |> 
  dplyr::filter(dplyr::between(Ramp90, -20, -0.1)) |> 
  dplyr::filter(dplyr::between(Ind, 0.2, 1)) 

Minio_all_bd <- merge(Minio_35107kHz_bd, Minio_QCF_bd, all.x = T, all.y=T)

Minio_Allac_bd <- Minio_all_bd[!duplicated(Minio_all_bd[c('Sites', 'Date_Night2')]), ]

## By month
Minio_bd_jan <- Minio_Allac_bd |>
  filter(month == 1)

Minio_bd_feb <- Minio_Allac_bd |>
  filter(month == 2)

Minio_bd_mar <- Minio_Allac_bd |>
  filter(month == 3)

Minio_bd_apr <- Minio_Allac_bd |>
  filter(month == 4)

Minio_bd_may <- Minio_Allac_bd |>
  filter(month == 5)

Minio_bd_jun <- Minio_Allac_bd |>
  filter(month == 6)

Minio_bd_jul <- Minio_Allac_bd |>
  filter(month == 7)

Minio_bd_aug <- Minio_Allac_bd |>
  filter(month == 8)

Minio_bd_sept <- Minio_Allac_bd |>
  filter(month == 9)

Minio_bd_oct <- Minio_Allac_bd |>
  filter(month == 10)

Minio_bd_nov <- Minio_Allac_bd |>
  filter(month == 11)

Minio_bd_dec <- Minio_Allac_bd |>
  filter(month == 12)

### With density jan
density_bd_Minio_jan <- density(as.numeric(Minio_bd_jan$times_in_decimal_modified), bw= "SJ")

density_bd_Minio_jan <- data.frame(x=density_bd_Minio_jan$x,y=density_bd_Minio_jan$y) 

### With density feb
density_bd_Minio_feb <- density(as.numeric(Minio_bd_feb$times_in_decimal_modified), bw= "SJ")

density_bd_Minio_feb <- data.frame(x=density_bd_Minio_feb$x,y=density_bd_Minio_feb$y) 

### With density mar
density_bd_Minio_mar <- density(as.numeric(Minio_bd_mar$times_in_decimal_modified), bw= "SJ")

density_bd_Minio_mar <- data.frame(x=density_bd_Minio_mar$x,y=density_bd_Minio_mar$y) 

### With density apr
density_bd_Minio_apr <- density(as.numeric(Minio_bd_apr$times_in_decimal_modified), bw= "SJ")

density_bd_Minio_apr <- data.frame(x=density_bd_Minio_apr$x,y=density_bd_Minio_apr$y) 

### With density may
density_bd_Minio_may <- density(as.numeric(Minio_bd_may$times_in_decimal_modified), bw= "SJ")

density_bd_Minio_may <- data.frame(x=density_bd_Minio_may$x,y=density_bd_Minio_may$y) 

### With density jun
density_bd_Minio_jun <- density(as.numeric(Minio_bd_jun$times_in_decimal_modified), bw= "SJ")

density_bd_Minio_jun <- data.frame(x=density_bd_Minio_jun$x,y=density_bd_Minio_jun$y) 

### With density jul
density_bd_Minio_jul <- density(as.numeric(Minio_bd_jul$times_in_decimal_modified), bw= "SJ")

density_bd_Minio_jul <- data.frame(x=density_bd_Minio_jul$x,y=density_bd_Minio_jul$y) 

### With density aug
density_bd_Minio_aug <- density(as.numeric(Minio_bd_aug$times_in_decimal_modified), bw= "SJ")

density_bd_Minio_aug <- data.frame(x=density_bd_Minio_aug$x,y=density_bd_Minio_aug$y) 

### With density sept
density_bd_Minio_sep <- density(as.numeric(Minio_bd_sept$times_in_decimal_modified), bw= "SJ")

density_bd_Minio_sep <- data.frame(x=density_bd_Minio_sep$x,y=density_bd_Minio_sep$y) 

### With density oct
density_bd_Minio_oct <- density(as.numeric(Minio_bd_oct$times_in_decimal_modified), bw= "SJ")

density_bd_Minio_oct <- data.frame(x=density_bd_Minio_oct$x,y=density_bd_Minio_oct$y) 

### With density nov
density_bd_Minio_nov <- density(as.numeric(Minio_bd_nov$times_in_decimal_modified), bw= "SJ")

density_bd_Minio_nov <- data.frame(x=density_bd_Minio_nov$x,y=density_bd_Minio_nov$y) 

### With density dec
density_bd_Minio_dec <- density(as.numeric(Minio_bd_dec$times_in_decimal_modified), bw= "SJ")

density_bd_Minio_dec <- data.frame(x=density_bd_Minio_dec$x,y=density_bd_Minio_dec$y) 

## Rhinolophus denti group

Rhino_de_bd <- selected_rows  %>%
  filter(SpMaxF2 %in% "FMu-CF-FMd") |>
  dplyr::filter(dplyr::between(MainMode, 95, 110)) |> 
  dplyr::filter(dplyr::between(Ramp90, -20, -0.1)) |> 
  dplyr::filter(dplyr::between(Ind, 0.35, 1)) 

Rhino_de_om_bd <- selected_rows  %>%
  filter(SpMaxF2 %in% "other-mammal") |>
  dplyr::filter(dplyr::between(MainMode, 95, 110)) |> 
  dplyr::filter(dplyr::between(Ramp90, -20, -0.1)) |> 
  dplyr::filter(dplyr::between(Ind, 0.35, 1))

Rhino_de_all_bd <- merge(Rhino_de_bd, Rhino_de_om_bd, all.x = T, all.y=T)

Rhino_de_Allac_bd <- Rhino_de_all_bd[!duplicated(Rhino_de_all_bd[c('Sites', 'Date_Night2')]), ]

## By month
Rhino_de_bd_jan <- Rhino_de_Allac_bd |>
  filter(month == 1)

Rhino_de_bd_feb <- Rhino_de_Allac_bd |>
  filter(month == 2)

Rhino_de_bd_mar <- Rhino_de_Allac_bd |>
  filter(month == 3)

Rhino_de_bd_apr <- Rhino_de_Allac_bd |>
  filter(month == 4)

Rhino_de_bd_may <- Rhino_de_Allac_bd |>
  filter(month == 5)

Rhino_de_bd_jun <- Rhino_de_Allac_bd |>
  filter(month == 6)

Rhino_de_bd_jul <- Rhino_de_Allac_bd |>
  filter(month == 7)

Rhino_de_bd_aug <- Rhino_de_Allac_bd |>
  filter(month == 8)

Rhino_de_bd_sept <- Rhino_de_Allac_bd |>
  filter(month == 9)

Rhino_de_bd_oct <- Rhino_de_Allac_bd |>
  filter(month == 10)

Rhino_de_bd_nov <- Rhino_de_Allac_bd |>
  filter(month == 11)

Rhino_de_bd_dec <- Rhino_de_Allac_bd |>
  filter(month == 12)

### With density jan
density_bd_Rhino_de_jan <- density(as.numeric(Rhino_de_bd_jan$times_in_decimal_modified), bw= "SJ")

density_bd_Rhino_de_jan <- data.frame(x=density_bd_Rhino_de_jan$x,y=density_bd_Rhino_de_jan$y) 

### With density feb
density_bd_Rhino_de_feb <- density(as.numeric(Rhino_de_bd_feb$times_in_decimal_modified), bw= "SJ")

density_bd_Rhino_de_feb <- data.frame(x=density_bd_Rhino_de_feb$x,y=density_bd_Rhino_de_feb$y) 

### With density mar
density_bd_Rhino_de_mar <- density(as.numeric(Rhino_de_bd_mar$times_in_decimal_modified), bw= "SJ")

density_bd_Rhino_de_mar <- data.frame(x=density_bd_Rhino_de_mar$x,y=density_bd_Rhino_de_mar$y) 

### With density apr
density_bd_Rhino_de_apr <- density(as.numeric(Rhino_de_bd_apr$times_in_decimal_modified), bw= "SJ")

density_bd_Rhino_de_apr <- data.frame(x=density_bd_Rhino_de_apr$x,y=density_bd_Rhino_de_apr$y) 

### With density may
density_bd_Rhino_de_may <- density(as.numeric(Rhino_de_bd_may$times_in_decimal_modified), bw= "SJ")

density_bd_Rhino_de_may <- data.frame(x=density_bd_Rhino_de_may$x,y=density_bd_Rhino_de_may$y) 

### With density jun
density_bd_Rhino_de_jun <- density(as.numeric(Rhino_de_bd_jun$times_in_decimal_modified), bw= "SJ")

density_bd_Rhino_de_jun <- data.frame(x=density_bd_Rhino_de_jun$x,y=density_bd_Rhino_de_jun$y) 

### With density jul
density_bd_Rhino_de_jul <- density(as.numeric(Rhino_de_bd_jul$times_in_decimal_modified), bw= "SJ")

density_bd_Rhino_de_jul <- data.frame(x=density_bd_Rhino_de_jul$x,y=density_bd_Rhino_de_jul$y) 

### With density aug
density_bd_Rhino_de_aug <- density(as.numeric(Rhino_de_bd_aug$times_in_decimal_modified), bw= "SJ")

density_bd_Rhino_de_aug <- data.frame(x=density_bd_Rhino_de_aug$x,y=density_bd_Rhino_de_aug$y) 

### With density sept
density_bd_Rhino_de_sep <- density(as.numeric(Rhino_de_bd_sept$times_in_decimal_modified), bw= "SJ")

density_bd_Rhino_de_sep <- data.frame(x=density_bd_Rhino_de_sep$x,y=density_bd_Rhino_de_sep$y) 

### With density oct
density_bd_Rhino_de_oct <- density(as.numeric(Rhino_de_bd_oct$times_in_decimal_modified), bw= "SJ")

density_bd_Rhino_de_oct <- data.frame(x=density_bd_Rhino_de_oct$x,y=density_bd_Rhino_de_oct$y) 

### With density nov
density_bd_Rhino_de_nov <- density(as.numeric(Rhino_de_bd_nov$times_in_decimal_modified), bw= "SJ")

density_bd_Rhino_de_nov <- data.frame(x=density_bd_Rhino_de_nov$x,y=density_bd_Rhino_de_nov$y) 

### With density dec
density_bd_Rhino_de_dec <- density(as.numeric(Rhino_de_bd_dec$times_in_decimal_modified), bw= "SJ")

density_bd_Rhino_de_dec <- data.frame(x=density_bd_Rhino_de_dec$x,y=density_bd_Rhino_de_dec$y) 


## Rhino Macro Tria group--------

RTG <- selected_rows  %>%
  filter(SpMaxF2 %in% "FMu-CF-FMd") |>
  filter(Sites %in% "Boundou") |>
  dplyr::filter(dplyr::between(MainMode, 45, 85)) |> 
  dplyr::filter(dplyr::between(Ramp90, -20, -0.1))

RTG2 <- selected_rows  %>%
  filter(SpMaxF2 %in% "CF-FMd") |>
  filter(Sites %in% "Boundou") |>
  dplyr::filter(dplyr::between(MainMode, 45, 89)) |> 
  dplyr::filter(dplyr::between(Ramp90, -20, -0.1))

Rhino_gigas <- selected_rows  %>%
  filter(SpMaxF2 %in% "other-mammal") |>
  filter(Sites %in% "Boundou") |>
  dplyr::filter(dplyr::between(MainMode, 45, 85)) |> 
  dplyr::filter(dplyr::between(Ramp90, -20, -0.1))

RTG_all <- merge(RTG, RTG2, all.x = T, all.y=T)
RTG_all2 <- merge(RTG_all, Rhino_gigas, all.x = T, all.y=T)

Rhino_macro_tria_all <- RTG_all2[!duplicated(RTG_all2[c('Sites', 'Date_Night2')]), ]

## By month
rmt_bd_jan <- Rhino_macro_tria_all |>
  filter(month == 1)

rmt_bd_feb <- Rhino_macro_tria_all |>
  filter(month == 2)

rmt_bd_mar <- Rhino_macro_tria_all |>
  filter(month == 3)

rmt_bd_apr <- Rhino_macro_tria_all |>
  filter(month == 4)

rmt_bd_may <- Rhino_macro_tria_all |>
  filter(month == 5)

rmt_bd_jun <- Rhino_macro_tria_all |>
  filter(month == 6)

rmt_bd_jul <- Rhino_macro_tria_all |>
  filter(month == 7)

rmt_bd_aug <- Rhino_macro_tria_all |>
  filter(month == 8)

rmt_bd_sept <- Rhino_macro_tria_all |>
  filter(month == 9)

rmt_bd_oct <- Rhino_macro_tria_all |>
  filter(month == 10)

rmt_bd_nov <- Rhino_macro_tria_all |>
  filter(month == 11)

rmt_bd_dec <- Rhino_macro_tria_all |>
  filter(month == 12)

### With density jan
density_bd_rmt_jan <- density(as.numeric(rmt_bd_jan$times_in_decimal_modified), bw= "SJ")

density_bd_rmt_jan <- data.frame(x=density_bd_rmt_jan$x,y=density_bd_rmt_jan$y) 

### With density feb
density_bd_rmt_feb <- density(as.numeric(rmt_bd_feb$times_in_decimal_modified), bw= "SJ")

density_bd_rmt_feb <- data.frame(x=density_bd_rmt_feb$x,y=density_bd_rmt_feb$y) 

### With density mar
density_bd_rmt_mar <- density(as.numeric(rmt_bd_mar$times_in_decimal_modified), bw= "SJ")

density_bd_rmt_mar <- data.frame(x=density_bd_rmt_mar$x,y=density_bd_rmt_mar$y) 

### With density apr
density_bd_rmt_apr <- density(as.numeric(rmt_bd_apr$times_in_decimal_modified), bw= "SJ")

density_bd_rmt_apr <- data.frame(x=density_bd_rmt_apr$x,y=density_bd_rmt_apr$y) 

### With density may
density_bd_rmt_may <- density(as.numeric(rmt_bd_may$times_in_decimal_modified), bw= "SJ")

density_bd_rmt_may <- data.frame(x=density_bd_rmt_may$x,y=density_bd_rmt_may$y) 

### With density jun
density_bd_rmt_jun <- density(as.numeric(rmt_bd_jun$times_in_decimal_modified), bw= "SJ")

density_bd_rmt_jun <- data.frame(x=density_bd_rmt_jun$x,y=density_bd_rmt_jun$y) 

### With density jul
density_bd_rmt_jul <- density(as.numeric(rmt_bd_jul$times_in_decimal_modified), bw= "SJ")

density_bd_rmt_jul <- data.frame(x=density_bd_rmt_jul$x,y=density_bd_rmt_jul$y) 

### With density aug
density_bd_rmt_aug <- density(as.numeric(rmt_bd_aug$times_in_decimal_modified), bw= "SJ")

density_bd_rmt_aug <- data.frame(x=density_bd_rmt_aug$x,y=density_bd_rmt_aug$y) 

### With density sept
density_bd_rmt_sep <- density(as.numeric(rmt_bd_sept$times_in_decimal_modified), bw= "SJ")

density_bd_rmt_sep <- data.frame(x=density_bd_rmt_sep$x,y=density_bd_rmt_sep$y) 

### With density oct
density_bd_rmt_oct <- density(as.numeric(rmt_bd_oct$times_in_decimal_modified), bw= "SJ")

density_bd_rmt_oct <- data.frame(x=density_bd_rmt_oct$x,y=density_bd_rmt_oct$y) 

### With density nov
density_bd_rmt_nov <- density(as.numeric(rmt_bd_nov$times_in_decimal_modified), bw= "SJ")

density_bd_rmt_nov <- data.frame(x=density_bd_rmt_nov$x,y=density_bd_rmt_nov$y) 

### With density dec
density_bd_rmt_dec <- density(as.numeric(rmt_bd_dec$times_in_decimal_modified), bw= "SJ")

density_bd_rmt_dec <- data.frame(x=density_bd_rmt_dec$x,y=density_bd_rmt_dec$y) 


### Graph Density #### ---------

### MB - Hippo  -----

Hippo_mb_jan_plot <- ggplot() + 
  geom_line(data=density_mb_hippo_jan,aes(x=x,y=y),linewidth=1.5,color="black") +
  scale_x_continuous(name = "Hour", 
                     breaks = seq(17, 31, by = 1),  # Adjusted to include hours from 17 to 30
                     limits = c(17, 31),            # Adjusted to include hours from 17 to 30
                     labels = c("17:00", "18:00", "19:00", "20:00", "21:00", "22:00", "23:00", "00:00", "01:00", "02:00", "03:00", "04:00", "05:00", "06:00", "07:00"), 
                     expand = c(0, 0)) +
scale_y_continuous(name = "Density") + ggtitle("January") +
  theme(text = element_text(size=10), axis.text.x = element_text(angle = 90))
Hippo_mb_jan_plot

Hippo_mb_feb_plot <- ggplot() + 
  geom_line(data=density_mb_hippo_feb,aes(x=x,y=y),linewidth=1.5,color="black") +
  scale_x_continuous(name = "Hour",                                             breaks = seq(17, 31, by = 1),                        limits = c(17, 31),                               labels = c("17:00", "18:00", "19:00", "20:00", "21:00", "22:00", "23:00", "00:00", "01:00", "02:00", "03:00", "04:00", "05:00", "06:00", "07:00"),                       expand = c(0, 0)) +
  scale_y_continuous(name = "Density") + ggtitle("February") +
  theme(text = element_text(size=10), axis.text.x = element_text(angle = 90))
Hippo_mb_feb_plot

Hippo_mb_mar_plot <- ggplot() + 
  geom_line(data=density_mb_hippo_mar,aes(x=x,y=y),linewidth=1.5,color="black") +
  scale_x_continuous(name = "Hour",                       
                     breaks = seq(17, 31, by = 1),  
                     limits = c(17, 31),                               labels = c("17:00", "18:00", "19:00", "20:00", "21:00", "22:00", "23:00", "00:00", "01:00", "02:00", "03:00", "04:00", "05:00", "06:00", "07:00"),                       expand = c(0, 0)) + 
  scale_y_continuous(name = "Density") + ggtitle("March") +
  theme(text = element_text(size=10), axis.text.x = element_text(angle = 90))
Hippo_mb_mar_plot

Hippo_mb_apr_plot <- ggplot() + 
  geom_line(data=density_mb_hippo_apr,aes(x=x,y=y),linewidth=1.5,color="black") +
  scale_x_continuous(name = "Hour",                                             breaks = seq(17, 31, by = 1),                        limits = c(17, 31),                               labels = c("17:00", "18:00", "19:00", "20:00", "21:00", "22:00", "23:00", "00:00", "01:00", "02:00", "03:00", "04:00", "05:00", "06:00", "07:00"),                       expand = c(0, 0)) +
  scale_y_continuous(name = "Density") + ggtitle("April") +
  theme(text = element_text(size=10), axis.text.x = element_text(angle = 90))
Hippo_mb_apr_plot

Hippo_mb_jun_plot <- ggplot() + 
  geom_line(data=density_mb_hippo_jun,aes(x=x,y=y),linewidth=1.5,color="black") +
  scale_x_continuous(name = "Hour",                                             breaks = seq(17, 31, by = 1),                        limits = c(17, 31),                               labels = c("17:00", "18:00", "19:00", "20:00", "21:00", "22:00", "23:00", "00:00", "01:00", "02:00", "03:00", "04:00", "05:00", "06:00", "07:00"),                       expand = c(0, 0)) +
  scale_y_continuous(name = "Density") + ggtitle("June") +
  theme(text = element_text(size=10), axis.text.x = element_text(angle = 90))
Hippo_mb_jun_plot

Hippo_mb_jul_plot <- ggplot() + 
  geom_line(data=density_mb_hippo_jul,aes(x=x,y=y),linewidth=1.5,color="black") +
  scale_x_continuous(name = "Hour",                                             breaks = seq(17, 31, by = 1),                        limits = c(17, 31),                               labels = c("17:00", "18:00", "19:00", "20:00", "21:00", "22:00", "23:00", "00:00", "01:00", "02:00", "03:00", "04:00", "05:00", "06:00", "07:00"),                       expand = c(0, 0)) +
  scale_y_continuous(name = "Density") + ggtitle("July") +
  theme(text = element_text(size=10), axis.text.x = element_text(angle = 90))
Hippo_mb_jul_plot

Hippo_mb_aug_plot <- ggplot() + 
  geom_line(data=density_mb_hippo_aug,aes(x=x,y=y),linewidth=1.5,color="black") +
  scale_x_continuous(name = "Hour",                                             breaks = seq(17, 31, by = 1),                        limits = c(17, 31),                               labels = c("17:00", "18:00", "19:00", "20:00", "21:00", "22:00", "23:00", "00:00", "01:00", "02:00", "03:00", "04:00", "05:00", "06:00", "07:00"),                       expand = c(0, 0)) +
  scale_y_continuous(name = "Density") + ggtitle("August") +
  theme(text = element_text(size=10), axis.text.x = element_text(angle = 90))
Hippo_mb_aug_plot

Hippo_mb_sep_plot <- ggplot() + 
  geom_line(data=density_mb_hippo_sep,aes(x=x,y=y),linewidth=1.5,color="black") +
  scale_x_continuous(name = "Hour",                                             breaks = seq(17, 31, by = 1),                        limits = c(17, 31),                               labels = c("17:00", "18:00", "19:00", "20:00", "21:00", "22:00", "23:00", "00:00", "01:00", "02:00", "03:00", "04:00", "05:00", "06:00", "07:00"),                       expand = c(0, 0)) +
  scale_y_continuous(name = "Density") + ggtitle("September") +
  theme(text = element_text(size=10), axis.text.x = element_text(angle = 90))
Hippo_mb_sep_plot

Hippo_mb_oct_plot <- ggplot() + 
  geom_line(data=density_mb_hippo_oct,aes(x=x,y=y),linewidth=1.5,color="black") +
  scale_x_continuous(name = "Hour",                                             breaks = seq(17, 31, by = 1),                        limits = c(17, 31),                               labels = c("17:00", "18:00", "19:00", "20:00", "21:00", "22:00", "23:00", "00:00", "01:00", "02:00", "03:00", "04:00", "05:00", "06:00", "07:00"),                       expand = c(0, 0)) +
  scale_y_continuous(name = "Density") + ggtitle("October") +
  theme(text = element_text(size=10), axis.text.x = element_text(angle = 90))
Hippo_mb_oct_plot

Hippo_mb_nov_plot <- ggplot() + 
  geom_line(data=density_mb_hippo_nov,aes(x=x,y=y),linewidth=1.5,color="black") +
  scale_x_continuous(name = "Hour",                                             breaks = seq(17, 31, by = 1),                        limits = c(17, 31),                               labels = c("17:00", "18:00", "19:00", "20:00", "21:00", "22:00", "23:00", "00:00", "01:00", "02:00", "03:00", "04:00", "05:00", "06:00", "07:00"),                       expand = c(0, 0)) +
  scale_y_continuous(name = "Density") + ggtitle("November") +
  theme(text = element_text(size=10), axis.text.x = element_text(angle = 90))
Hippo_mb_nov_plot

Hippo_mb_dec_plot <- ggplot() + 
  geom_line(data=density_mb_hippo_dec,aes(x=x,y=y),linewidth=1.5,color="black") +
  scale_x_continuous(name = "Hour",                                             breaks = seq(17, 31, by = 1),                        limits = c(17, 31),                               labels = c("17:00", "18:00", "19:00", "20:00", "21:00", "22:00", "23:00", "00:00", "01:00", "02:00", "03:00", "04:00", "05:00", "06:00", "07:00"),                       expand = c(0, 0)) +
  scale_y_continuous(name = "Density") + ggtitle("December") +
  theme(text = element_text(size=10), axis.text.x = element_text(angle = 90))
Hippo_mb_dec_plot


### MB - Minio  -----

Minio_mb_jan_plot <- ggplot() + 
  geom_line(data=density_mb_Minio_jan,aes(x=x,y=y),linewidth=1.5,color="black") +
  scale_x_continuous(name = "Hour",                                             breaks = seq(17, 31, by = 1),                        limits = c(17, 31),                               labels = c("17:00", "18:00", "19:00", "20:00", "21:00", "22:00", "23:00", "00:00", "01:00", "02:00", "03:00", "04:00", "05:00", "06:00", "07:00"),                       expand = c(0, 0)) +
  scale_y_continuous(name = "Density") + ggtitle("January") +
  theme(text = element_text(size=10), axis.text.x = element_text(angle = 90))
Minio_mb_jan_plot

Minio_mb_feb_plot <- ggplot() + 
  geom_line(data=density_mb_Minio_feb,aes(x=x,y=y),linewidth=1.5,color="black") +
  scale_x_continuous(name = "Hour",                                             breaks = seq(17, 31, by = 1),                        limits = c(17, 31),                               labels = c("17:00", "18:00", "19:00", "20:00", "21:00", "22:00", "23:00", "00:00", "01:00", "02:00", "03:00", "04:00", "05:00", "06:00", "07:00"),                       expand = c(0, 0)) +
  scale_y_continuous(name = "Density") + ggtitle("February") +
  theme(text = element_text(size=10), axis.text.x = element_text(angle = 90))
Minio_mb_feb_plot

Minio_mb_mar_plot <- ggplot() + 
  geom_line(data=density_mb_Minio_mar,aes(x=x,y=y),linewidth=1.5,color="black") +
  scale_x_continuous(name = "Hour",                                             breaks = seq(17, 31, by = 1),                        limits = c(17, 31),                               labels = c("17:00", "18:00", "19:00", "20:00", "21:00", "22:00", "23:00", "00:00", "01:00", "02:00", "03:00", "04:00", "05:00", "06:00", "07:00"),                       expand = c(0, 0)) +
  scale_y_continuous(name = "Density") + ggtitle("March") +
  theme(text = element_text(size=10), axis.text.x = element_text(angle = 90))
Minio_mb_mar_plot

Minio_mb_apr_plot <- ggplot() + 
  geom_line(data=density_mb_Minio_apr,aes(x=x,y=y),linewidth=1.5,color="black") +
  scale_x_continuous(name = "Hour",                                             breaks = seq(17, 31, by = 1),                        limits = c(17, 31),                               labels = c("17:00", "18:00", "19:00", "20:00", "21:00", "22:00", "23:00", "00:00", "01:00", "02:00", "03:00", "04:00", "05:00", "06:00", "07:00"),                       expand = c(0, 0)) +
  scale_y_continuous(name = "Density") + ggtitle("April") +
  theme(text = element_text(size=10), axis.text.x = element_text(angle = 90))
Minio_mb_apr_plot

Minio_mb_jun_plot <- ggplot() + 
  geom_line(data=density_mb_Minio_jun,aes(x=x,y=y),linewidth=1.5,color="black") +
  scale_x_continuous(name = "Hour",                                             breaks = seq(17, 31, by = 1),                        limits = c(17, 31),                               labels = c("17:00", "18:00", "19:00", "20:00", "21:00", "22:00", "23:00", "00:00", "01:00", "02:00", "03:00", "04:00", "05:00", "06:00", "07:00"),                       expand = c(0, 0)) +
  scale_y_continuous(name = "Density") + ggtitle("June") +
  theme(text = element_text(size=10), axis.text.x = element_text(angle = 90))
Minio_mb_jun_plot

Minio_mb_jul_plot <- ggplot() + 
  geom_line(data=density_mb_Minio_jul,aes(x=x,y=y),linewidth=1.5,color="black") +
  scale_x_continuous(name = "Hour",                                             breaks = seq(17, 31, by = 1),                        limits = c(17, 31),                               labels = c("17:00", "18:00", "19:00", "20:00", "21:00", "22:00", "23:00", "00:00", "01:00", "02:00", "03:00", "04:00", "05:00", "06:00", "07:00"),                       expand = c(0, 0)) +
  scale_y_continuous(name = "Density") + ggtitle("July") +
  theme(text = element_text(size=10), axis.text.x = element_text(angle = 90))
Minio_mb_jul_plot

Minio_mb_aug_plot <- ggplot() + 
  geom_line(data=density_mb_Minio_aug,aes(x=x,y=y),linewidth=1.5,color="black") +
  scale_x_continuous(name = "Hour",                                             breaks = seq(17, 31, by = 1),                        limits = c(17, 31),                               labels = c("17:00", "18:00", "19:00", "20:00", "21:00", "22:00", "23:00", "00:00", "01:00", "02:00", "03:00", "04:00", "05:00", "06:00", "07:00"),                       expand = c(0, 0)) +
  scale_y_continuous(name = "Density") + ggtitle("August") +
  theme(text = element_text(size=10), axis.text.x = element_text(angle = 90))
Minio_mb_aug_plot

Minio_mb_sep_plot <- ggplot() + 
  geom_line(data=density_mb_Minio_sep,aes(x=x,y=y),linewidth=1.5,color="black") +
  scale_x_continuous(name = "Hour",                                             breaks = seq(17, 31, by = 1),                        limits = c(17, 31),                               labels = c("17:00", "18:00", "19:00", "20:00", "21:00", "22:00", "23:00", "00:00", "01:00", "02:00", "03:00", "04:00", "05:00", "06:00", "07:00"),                       expand = c(0, 0)) +
  scale_y_continuous(name = "Density") + ggtitle("September") +
  theme(text = element_text(size=10), axis.text.x = element_text(angle = 90))
Minio_mb_sep_plot

Minio_mb_oct_plot <- ggplot() + 
  geom_line(data=density_mb_Minio_oct,aes(x=x,y=y),linewidth=1.5,color="black") +
  scale_x_continuous(name = "Hour",                                             breaks = seq(17, 31, by = 1),                        limits = c(17, 31),                               labels = c("17:00", "18:00", "19:00", "20:00", "21:00", "22:00", "23:00", "00:00", "01:00", "02:00", "03:00", "04:00", "05:00", "06:00", "07:00"),                       expand = c(0, 0)) +
  scale_y_continuous(name = "Density") + ggtitle("October") +
  theme(text = element_text(size=10), axis.text.x = element_text(angle = 90))
Minio_mb_oct_plot

Minio_mb_nov_plot <- ggplot() + 
  geom_line(data=density_mb_Minio_nov,aes(x=x,y=y),linewidth=1.5,color="black") +
  scale_x_continuous(name = "Hour",                                             breaks = seq(17, 31, by = 1),                        limits = c(17, 31),                               labels = c("17:00", "18:00", "19:00", "20:00", "21:00", "22:00", "23:00", "00:00", "01:00", "02:00", "03:00", "04:00", "05:00", "06:00", "07:00"),                       expand = c(0, 0)) +
  scale_y_continuous(name = "Density") + ggtitle("November") +
  theme(text = element_text(size=10), axis.text.x = element_text(angle = 90))
Minio_mb_nov_plot

Minio_mb_dec_plot <- ggplot() + 
  geom_line(data=density_mb_Minio_dec,aes(x=x,y=y),linewidth=1.5,color="black") +
  scale_x_continuous(name = "Hour",                                             breaks = seq(17, 31, by = 1),                        limits = c(17, 31),                               labels = c("17:00", "18:00", "19:00", "20:00", "21:00", "22:00", "23:00", "00:00", "01:00", "02:00", "03:00", "04:00", "05:00", "06:00", "07:00"),                       expand = c(0, 0)) +
  scale_y_continuous(name = "Density") + ggtitle("December") +
  theme(text = element_text(size=10), axis.text.x = element_text(angle = 90))
Minio_mb_dec_plot


### MB - Rhinolophus landeri-alcyone ------

Rhino_la_mb_jan_plot <- ggplot() + 
  geom_line(data=density_mb_Rhino_la_jan,aes(x=x,y=y),linewidth=1.5,color="black") +
  scale_x_continuous(name = "Hour",                                             breaks = seq(17, 31, by = 1),                        limits = c(17, 31),                               labels = c("17:00", "18:00", "19:00", "20:00", "21:00", "22:00", "23:00", "00:00", "01:00", "02:00", "03:00", "04:00", "05:00", "06:00", "07:00"),                       expand = c(0, 0)) +
  scale_y_continuous(name = "Density") + ggtitle("January") +
  theme(text = element_text(size=10), axis.text.x = element_text(angle = 90))
Rhino_la_mb_jan_plot

Rhino_la_mb_feb_plot <- ggplot() + 
  geom_line(data=density_mb_Rhino_la_feb,aes(x=x,y=y),linewidth=1.5,color="black") +
  scale_x_continuous(name = "Hour",                                             breaks = seq(17, 31, by = 1),                        limits = c(17, 31),                               labels = c("17:00", "18:00", "19:00", "20:00", "21:00", "22:00", "23:00", "00:00", "01:00", "02:00", "03:00", "04:00", "05:00", "06:00", "07:00"),                       expand = c(0, 0)) +
  scale_y_continuous(name = "Density") + ggtitle("February") +
  theme(text = element_text(size=10), axis.text.x = element_text(angle = 90))
Rhino_la_mb_feb_plot

Rhino_la_mb_mar_plot <- ggplot() + 
  geom_line(data=density_mb_Rhino_la_mar,aes(x=x,y=y),linewidth=1.5,color="black") +
  scale_x_continuous(name = "Hour",                                             breaks = seq(17, 31, by = 1),                        limits = c(17, 31),                               labels = c("17:00", "18:00", "19:00", "20:00", "21:00", "22:00", "23:00", "00:00", "01:00", "02:00", "03:00", "04:00", "05:00", "06:00", "07:00"),                       expand = c(0, 0)) +
  scale_y_continuous(name = "Density") + ggtitle("March") +
  theme(text = element_text(size=10), axis.text.x = element_text(angle = 90))
Rhino_la_mb_mar_plot

Rhino_la_mb_apr_plot <- ggplot() + 
  geom_line(data=density_mb_Rhino_la_apr,aes(x=x,y=y),linewidth=1.5,color="black") +
  scale_x_continuous(name = "Hour",                                             breaks = seq(17, 31, by = 1),                        limits = c(17, 31),                               labels = c("17:00", "18:00", "19:00", "20:00", "21:00", "22:00", "23:00", "00:00", "01:00", "02:00", "03:00", "04:00", "05:00", "06:00", "07:00"),                       expand = c(0, 0)) +
  scale_y_continuous(name = "Density") + ggtitle("April") +
  theme(text = element_text(size=10), axis.text.x = element_text(angle = 90))
Rhino_la_mb_apr_plot

Rhino_la_mb_jun_plot <- ggplot() + 
  geom_line(data=density_mb_Rhino_la_jun,aes(x=x,y=y),linewidth=1.5,color="black") +
  scale_x_continuous(name = "Hour",                                             breaks = seq(17, 31, by = 1),                        limits = c(17, 31),                               labels = c("17:00", "18:00", "19:00", "20:00", "21:00", "22:00", "23:00", "00:00", "01:00", "02:00", "03:00", "04:00", "05:00", "06:00", "07:00"),                       expand = c(0, 0)) +
  scale_y_continuous(name = "Density") + ggtitle("June") +
  theme(text = element_text(size=10), axis.text.x = element_text(angle = 90))
Rhino_la_mb_jun_plot

Rhino_la_mb_jul_plot <- ggplot() + 
  geom_line(data=density_mb_Rhino_la_jul,aes(x=x,y=y),linewidth=1.5,color="black") +
  scale_x_continuous(name = "Hour",                                             breaks = seq(17, 31, by = 1),                        limits = c(17, 31),                               labels = c("17:00", "18:00", "19:00", "20:00", "21:00", "22:00", "23:00", "00:00", "01:00", "02:00", "03:00", "04:00", "05:00", "06:00", "07:00"),                       expand = c(0, 0)) +
  scale_y_continuous(name = "Density") + ggtitle("July") +
  theme(text = element_text(size=10), axis.text.x = element_text(angle = 90))
Rhino_la_mb_jul_plot

Rhino_la_mb_aug_plot <- ggplot() + 
  geom_line(data=density_mb_Rhino_la_aug,aes(x=x,y=y),linewidth=1.5,color="black") +
  scale_x_continuous(name = "Hour",                                             breaks = seq(17, 31, by = 1),                        limits = c(17, 31),                               labels = c("17:00", "18:00", "19:00", "20:00", "21:00", "22:00", "23:00", "00:00", "01:00", "02:00", "03:00", "04:00", "05:00", "06:00", "07:00"),                       expand = c(0, 0)) +
  scale_y_continuous(name = "Density") + ggtitle("August") +
  theme(text = element_text(size=10), axis.text.x = element_text(angle = 90))
Rhino_la_mb_aug_plot

Rhino_la_mb_sep_plot <- ggplot() + 
  geom_line(data=density_mb_Rhino_la_sep,aes(x=x,y=y),linewidth=1.5,color="black") +
  scale_x_continuous(name = "Hour",                                             breaks = seq(17, 31, by = 1),                        limits = c(17, 31),                               labels = c("17:00", "18:00", "19:00", "20:00", "21:00", "22:00", "23:00", "00:00", "01:00", "02:00", "03:00", "04:00", "05:00", "06:00", "07:00"),                       expand = c(0, 0)) +
  scale_y_continuous(name = "Density") + ggtitle("September") +
  theme(text = element_text(size=10), axis.text.x = element_text(angle = 90))
Rhino_la_mb_sep_plot

Rhino_la_mb_oct_plot <- ggplot() + 
  geom_line(data=density_mb_Rhino_la_oct,aes(x=x,y=y),linewidth=1.5,color="black") +
  scale_x_continuous(name = "Hour",                                             breaks = seq(17, 31, by = 1),                        limits = c(17, 31),                               labels = c("17:00", "18:00", "19:00", "20:00", "21:00", "22:00", "23:00", "00:00", "01:00", "02:00", "03:00", "04:00", "05:00", "06:00", "07:00"),                       expand = c(0, 0)) +
  scale_y_continuous(name = "Density") + ggtitle("October") +
  theme(text = element_text(size=10), axis.text.x = element_text(angle = 90))
Rhino_la_mb_oct_plot

Rhino_la_mb_nov_plot <- ggplot() + 
  geom_line(data=density_mb_Rhino_la_nov,aes(x=x,y=y),linewidth=1.5,color="black") +
  scale_x_continuous(name = "Hour",                                             breaks = seq(17, 31, by = 1),                        limits = c(17, 31),                               labels = c("17:00", "18:00", "19:00", "20:00", "21:00", "22:00", "23:00", "00:00", "01:00", "02:00", "03:00", "04:00", "05:00", "06:00", "07:00"),                       expand = c(0, 0)) +
  scale_y_continuous(name = "Density") + ggtitle("November") +
  theme(text = element_text(size=10), axis.text.x = element_text(angle = 90))
Rhino_la_mb_nov_plot

Rhino_la_mb_dec_plot <- ggplot() + 
  geom_line(data=density_mb_Rhino_la_dec,aes(x=x,y=y),linewidth=1.5,color="black") +
  scale_x_continuous(name = "Hour",                                             breaks = seq(17, 31, by = 1),                        limits = c(17, 31),                               labels = c("17:00", "18:00", "19:00", "20:00", "21:00", "22:00", "23:00", "00:00", "01:00", "02:00", "03:00", "04:00", "05:00", "06:00", "07:00"),                       expand = c(0, 0)) +
  scale_y_continuous(name = "Density") + ggtitle("December") +
  theme(text = element_text(size=10), axis.text.x = element_text(angle = 90))
Rhino_la_mb_dec_plot

### MB - Rhinolophus denti ------

Rhino_de_mb_jan_plot <- ggplot() + 
  geom_line(data=density_mb_Rhino_de_jan,aes(x=x,y=y),linewidth=1.5,color="black") +
  scale_x_continuous(name = "Hour",                                             breaks = seq(17, 31, by = 1),                        limits = c(17, 31),                               labels = c("17:00", "18:00", "19:00", "20:00", "21:00", "22:00", "23:00", "00:00", "01:00", "02:00", "03:00", "04:00", "05:00", "06:00", "07:00"),                       expand = c(0, 0)) +
  scale_y_continuous(name = "Density") + ggtitle("January") +
  theme(text = element_text(size=10), axis.text.x = element_text(angle = 90))
Rhino_de_mb_jan_plot

Rhino_de_mb_feb_plot <- ggplot() + 
  geom_line(data=density_mb_Rhino_de_feb,aes(x=x,y=y),linewidth=1.5,color="black") +
  scale_x_continuous(name = "Hour",                                             breaks = seq(17, 31, by = 1),                        limits = c(17, 31),                               labels = c("17:00", "18:00", "19:00", "20:00", "21:00", "22:00", "23:00", "00:00", "01:00", "02:00", "03:00", "04:00", "05:00", "06:00", "07:00"),                       expand = c(0, 0)) +
  scale_y_continuous(name = "Density") + ggtitle("February") +
  theme(text = element_text(size=10), axis.text.x = element_text(angle = 90))
Rhino_de_mb_feb_plot

Rhino_de_mb_mar_plot <- ggplot() + 
  geom_line(data=density_mb_Rhino_de_mar,aes(x=x,y=y),linewidth=1.5,color="black") +
  scale_x_continuous(name = "Hour",                                             breaks = seq(17, 31, by = 1),                        limits = c(17, 31),                               labels = c("17:00", "18:00", "19:00", "20:00", "21:00", "22:00", "23:00", "00:00", "01:00", "02:00", "03:00", "04:00", "05:00", "06:00", "07:00"),                       expand = c(0, 0)) +
  scale_y_continuous(name = "Density") + ggtitle("March") +
  theme(text = element_text(size=10), axis.text.x = element_text(angle = 90))
Rhino_de_mb_mar_plot

Rhino_de_mb_apr_plot <- ggplot() + 
  geom_line(data=density_mb_Rhino_de_apr,aes(x=x,y=y),linewidth=1.5,color="black") +
  scale_x_continuous(name = "Hour",                                             breaks = seq(17, 31, by = 1),                        limits = c(17, 31),                               labels = c("17:00", "18:00", "19:00", "20:00", "21:00", "22:00", "23:00", "00:00", "01:00", "02:00", "03:00", "04:00", "05:00", "06:00", "07:00"),                       expand = c(0, 0)) +
  scale_y_continuous(name = "Density") + ggtitle("April") +
  theme(text = element_text(size=10), axis.text.x = element_text(angle = 90))
Rhino_de_mb_apr_plot

Rhino_de_mb_jun_plot <- ggplot() + 
  geom_line(data=density_mb_Rhino_de_jun,aes(x=x,y=y),linewidth=1.5,color="black") +
  scale_x_continuous(name = "Hour",                                             breaks = seq(17, 31, by = 1),                        limits = c(17, 31),                               labels = c("17:00", "18:00", "19:00", "20:00", "21:00", "22:00", "23:00", "00:00", "01:00", "02:00", "03:00", "04:00", "05:00", "06:00", "07:00"),                       expand = c(0, 0)) +
  scale_y_continuous(name = "Density") + ggtitle("June") +
  theme(text = element_text(size=10), axis.text.x = element_text(angle = 90))
Rhino_de_mb_jun_plot

Rhino_de_mb_jul_plot <- ggplot() + 
  geom_line(data=density_mb_Rhino_de_jul,aes(x=x,y=y),linewidth=1.5,color="black") +
  scale_x_continuous(name = "Hour",                                             breaks = seq(17, 31, by = 1),                        limits = c(17, 31),                               labels = c("17:00", "18:00", "19:00", "20:00", "21:00", "22:00", "23:00", "00:00", "01:00", "02:00", "03:00", "04:00", "05:00", "06:00", "07:00"),                       expand = c(0, 0)) +
  scale_y_continuous(name = "Density") + ggtitle("July") +
  theme(text = element_text(size=10), axis.text.x = element_text(angle = 90))
Rhino_de_mb_jul_plot

Rhino_de_mb_aug_plot <- ggplot() + 
  geom_line(data=density_mb_Rhino_de_aug,aes(x=x,y=y),linewidth=1.5,color="black") +
  scale_x_continuous(name = "Hour",                                             breaks = seq(17, 31, by = 1),                        limits = c(17, 31),                               labels = c("17:00", "18:00", "19:00", "20:00", "21:00", "22:00", "23:00", "00:00", "01:00", "02:00", "03:00", "04:00", "05:00", "06:00", "07:00"),                       expand = c(0, 0)) +
  scale_y_continuous(name = "Density") + ggtitle("August") +
  theme(text = element_text(size=10), axis.text.x = element_text(angle = 90))
Rhino_de_mb_aug_plot

Rhino_de_mb_sep_plot <- ggplot() + 
  geom_line(data=density_mb_Rhino_de_sep,aes(x=x,y=y),linewidth=1.5,color="black") +
  scale_x_continuous(name = "Hour",                                             breaks = seq(17, 31, by = 1),                        limits = c(17, 31),                               labels = c("17:00", "18:00", "19:00", "20:00", "21:00", "22:00", "23:00", "00:00", "01:00", "02:00", "03:00", "04:00", "05:00", "06:00", "07:00"),                       expand = c(0, 0)) +
  scale_y_continuous(name = "Density") + ggtitle("September") +
  theme(text = element_text(size=10), axis.text.x = element_text(angle = 90))
Rhino_de_mb_sep_plot

Rhino_de_mb_oct_plot <- ggplot() + 
  geom_line(data=density_mb_Rhino_de_oct,aes(x=x,y=y),linewidth=1.5,color="black") +
  scale_x_continuous(name = "Hour",                                             breaks = seq(17, 31, by = 1),                        limits = c(17, 31),                               labels = c("17:00", "18:00", "19:00", "20:00", "21:00", "22:00", "23:00", "00:00", "01:00", "02:00", "03:00", "04:00", "05:00", "06:00", "07:00"),                       expand = c(0, 0)) +
  scale_y_continuous(name = "Density") + ggtitle("October") +
  theme(text = element_text(size=10), axis.text.x = element_text(angle = 90))
Rhino_de_mb_oct_plot

Rhino_de_mb_nov_plot <- ggplot() + 
  geom_line(data=density_mb_Rhino_de_nov,aes(x=x,y=y),linewidth=1.5,color="black") +
  scale_x_continuous(name = "Hour",                                             breaks = seq(17, 31, by = 1),                        limits = c(17, 31),                               labels = c("17:00", "18:00", "19:00", "20:00", "21:00", "22:00", "23:00", "00:00", "01:00", "02:00", "03:00", "04:00", "05:00", "06:00", "07:00"),                       expand = c(0, 0)) +
  scale_y_continuous(name = "Density") + ggtitle("November") +
  theme(text = element_text(size=10), axis.text.x = element_text(angle = 90))
Rhino_de_mb_nov_plot

Rhino_de_mb_dec_plot <- ggplot() + 
  geom_line(data=density_mb_Rhino_de_dec,aes(x=x,y=y),linewidth=1.5,color="black") +
  scale_x_continuous(name = "Hour",                                             breaks = seq(17, 31, by = 1),                        limits = c(17, 31),                               labels = c("17:00", "18:00", "19:00", "20:00", "21:00", "22:00", "23:00", "00:00", "01:00", "02:00", "03:00", "04:00", "05:00", "06:00", "07:00"),                       expand = c(0, 0)) +
  scale_y_continuous(name = "Density") + ggtitle("December") +
  theme(text = element_text(size=10), axis.text.x = element_text(angle = 90))
Rhino_de_mb_dec_plot

### Boundou -------

### BD - Hippo --------

Hippo_bd_jan_plot <- ggplot() + 
  geom_line(data=density_bd_hippo_jan,aes(x=x,y=y),linewidth=1.5,color="black") +
  scale_x_continuous(name = "Hour",                                             breaks = seq(17, 31, by = 1),                        limits = c(17, 31),                               labels = c("17:00", "18:00", "19:00", "20:00", "21:00", "22:00", "23:00", "00:00", "01:00", "02:00", "03:00", "04:00", "05:00", "06:00", "07:00"),                       expand = c(0, 0)) +
  scale_y_continuous(name = "Density") + ggtitle("January") +
  theme(text = element_text(size=10), axis.text.x = element_text(angle = 90))
Hippo_bd_jan_plot

Hippo_bd_feb_plot <- ggplot() + 
  geom_line(data=density_bd_hippo_feb,aes(x=x,y=y),linewidth=1.5,color="black") +
  scale_x_continuous(name = "Hour",                                             breaks = seq(17, 31, by = 1),                        limits = c(17, 31),                               labels = c("17:00", "18:00", "19:00", "20:00", "21:00", "22:00", "23:00", "00:00", "01:00", "02:00", "03:00", "04:00", "05:00", "06:00", "07:00"),                       expand = c(0, 0)) +
  scale_y_continuous(name = "Density") + ggtitle("February") +
  theme(text = element_text(size=10), axis.text.x = element_text(angle = 90))
Hippo_bd_feb_plot

Hippo_bd_mar_plot <- ggplot() + 
  geom_line(data=density_bd_hippo_mar,aes(x=x,y=y),linewidth=1.5,color="black") +
  scale_x_continuous(name = "Hour",                                             breaks = seq(17, 31, by = 1),                        limits = c(17, 31),                               labels = c("17:00", "18:00", "19:00", "20:00", "21:00", "22:00", "23:00", "00:00", "01:00", "02:00", "03:00", "04:00", "05:00", "06:00", "07:00"),                       expand = c(0, 0)) +
  scale_y_continuous(name = "Density") + ggtitle("March") +
  theme(text = element_text(size=10), axis.text.x = element_text(angle = 90))
Hippo_bd_mar_plot

Hippo_bd_apr_plot <- ggplot() + 
  geom_line(data=density_bd_hippo_apr,aes(x=x,y=y),linewidth=1.5,color="black") +
  scale_x_continuous(name = "Hour",                                             breaks = seq(17, 31, by = 1),                        limits = c(17, 31),                               labels = c("17:00", "18:00", "19:00", "20:00", "21:00", "22:00", "23:00", "00:00", "01:00", "02:00", "03:00", "04:00", "05:00", "06:00", "07:00"),                       expand = c(0, 0)) +
  scale_y_continuous(name = "Density") + ggtitle("April") +
  theme(text = element_text(size=10), axis.text.x = element_text(angle = 90))
Hippo_bd_apr_plot

Hippo_bd_jun_plot <- ggplot() + 
  geom_line(data=density_bd_hippo_jun,aes(x=x,y=y),linewidth=1.5,color="black") +
  scale_x_continuous(name = "Hour",                                             breaks = seq(17, 31, by = 1),                        limits = c(17, 31),                               labels = c("17:00", "18:00", "19:00", "20:00", "21:00", "22:00", "23:00", "00:00", "01:00", "02:00", "03:00", "04:00", "05:00", "06:00", "07:00"),                       expand = c(0, 0)) +
  scale_y_continuous(name = "Density") + ggtitle("June") +
  theme(text = element_text(size=10), axis.text.x = element_text(angle = 90))
Hippo_bd_jun_plot

Hippo_bd_jul_plot <- ggplot() + 
  geom_line(data=density_bd_hippo_jul,aes(x=x,y=y),linewidth=1.5,color="black") +
  scale_x_continuous(name = "Hour",                                             breaks = seq(17, 31, by = 1),                        limits = c(17, 31),                               labels = c("17:00", "18:00", "19:00", "20:00", "21:00", "22:00", "23:00", "00:00", "01:00", "02:00", "03:00", "04:00", "05:00", "06:00", "07:00"),                       expand = c(0, 0)) +
  scale_y_continuous(name = "Density") + ggtitle("July") +
  theme(text = element_text(size=10), axis.text.x = element_text(angle = 90))
Hippo_bd_jul_plot

Hippo_bd_aug_plot <- ggplot() + 
  geom_line(data=density_bd_hippo_aug,aes(x=x,y=y),linewidth=1.5,color="black") +
  scale_x_continuous(name = "Hour",                                             breaks = seq(17, 31, by = 1),                        limits = c(17, 31),                               labels = c("17:00", "18:00", "19:00", "20:00", "21:00", "22:00", "23:00", "00:00", "01:00", "02:00", "03:00", "04:00", "05:00", "06:00", "07:00"),                       expand = c(0, 0)) +
  scale_y_continuous(name = "Density") + ggtitle("August") +
  theme(text = element_text(size=10), axis.text.x = element_text(angle = 90))
Hippo_bd_aug_plot

Hippo_bd_sep_plot <- ggplot() + 
  geom_line(data=density_bd_hippo_sep,aes(x=x,y=y),linewidth=1.5,color="black") +
  scale_x_continuous(name = "Hour",                                             breaks = seq(17, 31, by = 1),                        limits = c(17, 31),                               labels = c("17:00", "18:00", "19:00", "20:00", "21:00", "22:00", "23:00", "00:00", "01:00", "02:00", "03:00", "04:00", "05:00", "06:00", "07:00"),                       expand = c(0, 0)) +
  scale_y_continuous(name = "Density") + ggtitle("September") +
  theme(text = element_text(size=10), axis.text.x = element_text(angle = 90))
Hippo_bd_sep_plot

Hippo_bd_oct_plot <- ggplot() + 
  geom_line(data=density_bd_hippo_oct,aes(x=x,y=y),linewidth=1.5,color="black") +
  scale_x_continuous(name = "Hour",                                             breaks = seq(17, 31, by = 1),                        limits = c(17, 31),                               labels = c("17:00", "18:00", "19:00", "20:00", "21:00", "22:00", "23:00", "00:00", "01:00", "02:00", "03:00", "04:00", "05:00", "06:00", "07:00"),                       expand = c(0, 0)) +
  scale_y_continuous(name = "Density") + ggtitle("October") +
  theme(text = element_text(size=10), axis.text.x = element_text(angle = 90))
Hippo_bd_oct_plot

Hippo_bd_nov_plot <- ggplot() + 
  geom_line(data=density_bd_hippo_nov,aes(x=x,y=y),linewidth=1.5,color="black") +
  scale_x_continuous(name = "Hour",                                             breaks = seq(17, 31, by = 1),                        limits = c(17, 31),                               labels = c("17:00", "18:00", "19:00", "20:00", "21:00", "22:00", "23:00", "00:00", "01:00", "02:00", "03:00", "04:00", "05:00", "06:00", "07:00"),                       expand = c(0, 0)) +
  scale_y_continuous(name = "Density") + ggtitle("November") +
  theme(text = element_text(size=10), axis.text.x = element_text(angle = 90))
Hippo_bd_nov_plot

Hippo_bd_dec_plot <- ggplot() + 
  geom_line(data=density_bd_hippo_dec,aes(x=x,y=y),linewidth=1.5,color="black") +
  scale_x_continuous(name = "Hour",                                             breaks = seq(17, 31, by = 1),                        limits = c(17, 31),                               labels = c("17:00", "18:00", "19:00", "20:00", "21:00", "22:00", "23:00", "00:00", "01:00", "02:00", "03:00", "04:00", "05:00", "06:00", "07:00"),                       expand = c(0, 0)) +
  scale_y_continuous(name = "Density") + ggtitle("December") +
  theme(text = element_text(size=10), axis.text.x = element_text(angle = 90))
Hippo_bd_dec_plot


### BD - Minio -----

Minio_bd_jan_plot <- ggplot() + 
  geom_line(data=density_bd_Minio_jan,aes(x=x,y=y),linewidth=1.5,color="black") +
  scale_x_continuous(name = "Hour",                                             breaks = seq(17, 31, by = 1),                        limits = c(17, 31),                               labels = c("17:00", "18:00", "19:00", "20:00", "21:00", "22:00", "23:00", "00:00", "01:00", "02:00", "03:00", "04:00", "05:00", "06:00", "07:00"),                       expand = c(0, 0)) +
  scale_y_continuous(name = "Density") + ggtitle("January") +
  theme(text = element_text(size=10), axis.text.x = element_text(angle = 90))
Minio_bd_jan_plot

Minio_bd_feb_plot <- ggplot() + 
  geom_line(data=density_bd_Minio_feb,aes(x=x,y=y),linewidth=1.5,color="black") +
  scale_x_continuous(name = "Hour",                                             breaks = seq(17, 31, by = 1),                        limits = c(17, 31),                               labels = c("17:00", "18:00", "19:00", "20:00", "21:00", "22:00", "23:00", "00:00", "01:00", "02:00", "03:00", "04:00", "05:00", "06:00", "07:00"),                       expand = c(0, 0)) +
  scale_y_continuous(name = "Density") + ggtitle("February") +
  theme(text = element_text(size=10), axis.text.x = element_text(angle = 90))
Minio_bd_feb_plot

Minio_bd_mar_plot <- ggplot() + 
  geom_line(data=density_bd_Minio_mar,aes(x=x,y=y),linewidth=1.5,color="black") +
  scale_x_continuous(name = "Hour",                                             breaks = seq(17, 31, by = 1),                        limits = c(17, 31),                               labels = c("17:00", "18:00", "19:00", "20:00", "21:00", "22:00", "23:00", "00:00", "01:00", "02:00", "03:00", "04:00", "05:00", "06:00", "07:00"),                       expand = c(0, 0)) +
  scale_y_continuous(name = "Density") + ggtitle("March") +
  theme(text = element_text(size=10), axis.text.x = element_text(angle = 90))
Minio_bd_mar_plot

Minio_bd_apr_plot <- ggplot() + 
  geom_line(data=density_bd_Minio_apr,aes(x=x,y=y),linewidth=1.5,color="black") +
  scale_x_continuous(name = "Hour",                                             breaks = seq(17, 31, by = 1),                        limits = c(17, 31),                               labels = c("17:00", "18:00", "19:00", "20:00", "21:00", "22:00", "23:00", "00:00", "01:00", "02:00", "03:00", "04:00", "05:00", "06:00", "07:00"),                       expand = c(0, 0)) +
  scale_y_continuous(name = "Density") + ggtitle("April") +
  theme(text = element_text(size=10), axis.text.x = element_text(angle = 90))
Minio_bd_apr_plot

Minio_bd_jun_plot <- ggplot() + 
  geom_line(data=density_bd_Minio_jun,aes(x=x,y=y),linewidth=1.5,color="black") +
  scale_x_continuous(name = "Hour",                                             breaks = seq(17, 31, by = 1),                        limits = c(17, 31),                               labels = c("17:00", "18:00", "19:00", "20:00", "21:00", "22:00", "23:00", "00:00", "01:00", "02:00", "03:00", "04:00", "05:00", "06:00", "07:00"),                       expand = c(0, 0)) +
  scale_y_continuous(name = "Density") + ggtitle("June") +
  theme(text = element_text(size=10), axis.text.x = element_text(angle = 90))
Minio_bd_jun_plot

Minio_bd_jul_plot <- ggplot() + 
  geom_line(data=density_bd_Minio_jul,aes(x=x,y=y),linewidth=1.5,color="black") +
  scale_x_continuous(name = "Hour",                                             breaks = seq(17, 31, by = 1),                        limits = c(17, 31),                               labels = c("17:00", "18:00", "19:00", "20:00", "21:00", "22:00", "23:00", "00:00", "01:00", "02:00", "03:00", "04:00", "05:00", "06:00", "07:00"),                       expand = c(0, 0)) +
  scale_y_continuous(name = "Density") + ggtitle("July") +
  theme(text = element_text(size=10), axis.text.x = element_text(angle = 90))
Minio_bd_jul_plot

Minio_bd_aug_plot <- ggplot() + 
  geom_line(data=density_bd_Minio_aug,aes(x=x,y=y),linewidth=1.5,color="black") +
  scale_x_continuous(name = "Hour",                                             breaks = seq(17, 31, by = 1),                        limits = c(17, 31),                               labels = c("17:00", "18:00", "19:00", "20:00", "21:00", "22:00", "23:00", "00:00", "01:00", "02:00", "03:00", "04:00", "05:00", "06:00", "07:00"),                       expand = c(0, 0)) +
  scale_y_continuous(name = "Density") + ggtitle("August") +
  theme(text = element_text(size=10), axis.text.x = element_text(angle = 90))
Minio_bd_aug_plot

Minio_bd_sep_plot <- ggplot() + 
  geom_line(data=density_bd_Minio_sep,aes(x=x,y=y),linewidth=1.5,color="black") +
  scale_x_continuous(name = "Hour",                                             breaks = seq(17, 31, by = 1),                        limits = c(17, 31),                               labels = c("17:00", "18:00", "19:00", "20:00", "21:00", "22:00", "23:00", "00:00", "01:00", "02:00", "03:00", "04:00", "05:00", "06:00", "07:00"),                       expand = c(0, 0)) +
  scale_y_continuous(name = "Density") + ggtitle("September") +
  theme(text = element_text(size=10), axis.text.x = element_text(angle = 90))
Minio_bd_sep_plot

Minio_bd_oct_plot <- ggplot() + 
  geom_line(data=density_bd_Minio_oct,aes(x=x,y=y),linewidth=1.5,color="black") +
  scale_x_continuous(name = "Hour",                                             breaks = seq(17, 31, by = 1),                        limits = c(17, 31),                               labels = c("17:00", "18:00", "19:00", "20:00", "21:00", "22:00", "23:00", "00:00", "01:00", "02:00", "03:00", "04:00", "05:00", "06:00", "07:00"),                       expand = c(0, 0)) +
  scale_y_continuous(name = "Density") + ggtitle("October") +
  theme(text = element_text(size=10), axis.text.x = element_text(angle = 90))
Minio_bd_oct_plot

Minio_bd_nov_plot <- ggplot() + 
  geom_line(data=density_bd_Minio_nov,aes(x=x,y=y),linewidth=1.5,color="black") +
  scale_x_continuous(name = "Hour",                                             breaks = seq(17, 31, by = 1),                        limits = c(17, 31),                               labels = c("17:00", "18:00", "19:00", "20:00", "21:00", "22:00", "23:00", "00:00", "01:00", "02:00", "03:00", "04:00", "05:00", "06:00", "07:00"),                       expand = c(0, 0)) +
  scale_y_continuous(name = "Density") + ggtitle("November") +
  theme(text = element_text(size=10), axis.text.x = element_text(angle = 90))
Minio_bd_nov_plot

Minio_bd_dec_plot <- ggplot() + 
  geom_line(data=density_bd_Minio_dec,aes(x=x,y=y),linewidth=1.5,color="black") +
  scale_x_continuous(name = "Hour",                                             breaks = seq(17, 31, by = 1),                        limits = c(17, 31),                               labels = c("17:00", "18:00", "19:00", "20:00", "21:00", "22:00", "23:00", "00:00", "01:00", "02:00", "03:00", "04:00", "05:00", "06:00", "07:00"),                       expand = c(0, 0)) +
  scale_y_continuous(name = "Density") + ggtitle("December") +
  theme(text = element_text(size=10), axis.text.x = element_text(angle = 90))
Minio_bd_dec_plot

### Rhinolophus denti --------

Rhino_de_bd_jan_plot <- ggplot() + 
  geom_line(data=density_bd_Rhino_de_jan,aes(x=x,y=y),linewidth=1.5,color="black") +
  scale_x_continuous(name = "Hour",                                             breaks = seq(17, 31, by = 1),                        limits = c(17, 31),                               labels = c("17:00", "18:00", "19:00", "20:00", "21:00", "22:00", "23:00", "00:00", "01:00", "02:00", "03:00", "04:00", "05:00", "06:00", "07:00"),                       expand = c(0, 0)) +
  scale_y_continuous(name = "Density") + ggtitle("January") +
  theme(text = element_text(size=10), axis.text.x = element_text(angle = 90))
Rhino_de_bd_jan_plot

Rhino_de_bd_feb_plot <- ggplot() + 
  geom_line(data=density_bd_Rhino_de_feb,aes(x=x,y=y),linewidth=1.5,color="black") +
  scale_x_continuous(name = "Hour",                                             breaks = seq(17, 31, by = 1),                        limits = c(17, 31),                               labels = c("17:00", "18:00", "19:00", "20:00", "21:00", "22:00", "23:00", "00:00", "01:00", "02:00", "03:00", "04:00", "05:00", "06:00", "07:00"),                       expand = c(0, 0)) +
  scale_y_continuous(name = "Density") + ggtitle("February") +
  theme(text = element_text(size=10), axis.text.x = element_text(angle = 90))
Rhino_de_bd_feb_plot

Rhino_de_bd_mar_plot <- ggplot() + 
  geom_line(data=density_bd_Rhino_de_mar,aes(x=x,y=y),linewidth=1.5,color="black") +
  scale_x_continuous(name = "Hour",                                             breaks = seq(17, 31, by = 1),                        limits = c(17, 31),                               labels = c("17:00", "18:00", "19:00", "20:00", "21:00", "22:00", "23:00", "00:00", "01:00", "02:00", "03:00", "04:00", "05:00", "06:00", "07:00"),                       expand = c(0, 0)) +
  scale_y_continuous(name = "Density") + ggtitle("March") +
  theme(text = element_text(size=10), axis.text.x = element_text(angle = 90))
Rhino_de_bd_mar_plot

Rhino_de_bd_apr_plot <- ggplot() + 
  geom_line(data=density_bd_Rhino_de_apr,aes(x=x,y=y),linewidth=1.5,color="black") +
  scale_x_continuous(name = "Hour",                                             breaks = seq(17, 31, by = 1),                        limits = c(17, 31),                               labels = c("17:00", "18:00", "19:00", "20:00", "21:00", "22:00", "23:00", "00:00", "01:00", "02:00", "03:00", "04:00", "05:00", "06:00", "07:00"),                       expand = c(0, 0)) +
  scale_y_continuous(name = "Density") + ggtitle("April") +
  theme(text = element_text(size=10), axis.text.x = element_text(angle = 90))
Rhino_de_bd_apr_plot

Rhino_de_bd_jun_plot <- ggplot() + 
  geom_line(data=density_bd_Rhino_de_jun,aes(x=x,y=y),linewidth=1.5,color="black") +
  scale_x_continuous(name = "Hour",                                             breaks = seq(17, 31, by = 1),                        limits = c(17, 31),                               labels = c("17:00", "18:00", "19:00", "20:00", "21:00", "22:00", "23:00", "00:00", "01:00", "02:00", "03:00", "04:00", "05:00", "06:00", "07:00"),                       expand = c(0, 0)) +
  scale_y_continuous(name = "Density") + ggtitle("June") +
  theme(text = element_text(size=10), axis.text.x = element_text(angle = 90))
Rhino_de_bd_jun_plot

Rhino_de_bd_jul_plot <- ggplot() + 
  geom_line(data=density_bd_Rhino_de_jul,aes(x=x,y=y),linewidth=1.5,color="black") +
  scale_x_continuous(name = "Hour",                                             breaks = seq(17, 31, by = 1),                        limits = c(17, 31),                               labels = c("17:00", "18:00", "19:00", "20:00", "21:00", "22:00", "23:00", "00:00", "01:00", "02:00", "03:00", "04:00", "05:00", "06:00", "07:00"),                       expand = c(0, 0)) +
  scale_y_continuous(name = "Density") + ggtitle("July") +
  theme(text = element_text(size=10), axis.text.x = element_text(angle = 90))
Rhino_de_bd_jul_plot

Rhino_de_bd_aug_plot <- ggplot() + 
  geom_line(data=density_bd_Rhino_de_aug,aes(x=x,y=y),linewidth=1.5,color="black") +
  scale_x_continuous(name = "Hour",                                             breaks = seq(17, 31, by = 1),                        limits = c(17, 31),                               labels = c("17:00", "18:00", "19:00", "20:00", "21:00", "22:00", "23:00", "00:00", "01:00", "02:00", "03:00", "04:00", "05:00", "06:00", "07:00"),                       expand = c(0, 0)) +
  scale_y_continuous(name = "Density") + ggtitle("August") +
  theme(text = element_text(size=10), axis.text.x = element_text(angle = 90))
Rhino_de_bd_aug_plot

Rhino_de_bd_sep_plot <- ggplot() + 
  geom_line(data=density_bd_Rhino_de_sep,aes(x=x,y=y),linewidth=1.5,color="black") +
  scale_x_continuous(name = "Hour",                                             breaks = seq(17, 31, by = 1),                        limits = c(17, 31),                               labels = c("17:00", "18:00", "19:00", "20:00", "21:00", "22:00", "23:00", "00:00", "01:00", "02:00", "03:00", "04:00", "05:00", "06:00", "07:00"),                       expand = c(0, 0)) +
  scale_y_continuous(name = "Density") + ggtitle("September") +
  theme(text = element_text(size=10), axis.text.x = element_text(angle = 90))
Rhino_de_bd_sep_plot

Rhino_de_bd_oct_plot <- ggplot() + 
  geom_line(data=density_bd_Rhino_de_oct,aes(x=x,y=y),linewidth=1.5,color="black") +
  scale_x_continuous(name = "Hour",                                             breaks = seq(17, 31, by = 1),                        limits = c(17, 31),                               labels = c("17:00", "18:00", "19:00", "20:00", "21:00", "22:00", "23:00", "00:00", "01:00", "02:00", "03:00", "04:00", "05:00", "06:00", "07:00"),                       expand = c(0, 0)) +
  scale_y_continuous(name = "Density") + ggtitle("October") +
  theme(text = element_text(size=10), axis.text.x = element_text(angle = 90))
Rhino_de_bd_oct_plot

Rhino_de_bd_nov_plot <- ggplot() + 
  geom_line(data=density_bd_Rhino_de_nov,aes(x=x,y=y),linewidth=1.5,color="black") +
  scale_x_continuous(name = "Hour",                                             breaks = seq(17, 31, by = 1),                        limits = c(17, 31),                               labels = c("17:00", "18:00", "19:00", "20:00", "21:00", "22:00", "23:00", "00:00", "01:00", "02:00", "03:00", "04:00", "05:00", "06:00", "07:00"),                       expand = c(0, 0)) +
  scale_y_continuous(name = "Density") + ggtitle("November") +
  theme(text = element_text(size=10), axis.text.x = element_text(angle = 90))
Rhino_de_bd_nov_plot

Rhino_de_bd_dec_plot <- ggplot() + 
  geom_line(data=density_bd_Rhino_de_dec,aes(x=x,y=y),linewidth=1.5,color="black") +
  scale_x_continuous(name = "Hour",                                             breaks = seq(17, 31, by = 1),                        limits = c(17, 31),                               labels = c("17:00", "18:00", "19:00", "20:00", "21:00", "22:00", "23:00", "00:00", "01:00", "02:00", "03:00", "04:00", "05:00", "06:00", "07:00"),                       expand = c(0, 0)) +
  scale_y_continuous(name = "Density") + ggtitle("December") +
  theme(text = element_text(size=10), axis.text.x = element_text(angle = 90))
Rhino_de_bd_dec_plot

#### Rhino Macro tria -------

Rmt_bd_jan_plot <- ggplot() + 
  geom_line(data=density_bd_rmt_jan,aes(x=x,y=y),linewidth=1.5,color="black") +
  scale_x_continuous(name = "Hour",                                             breaks = seq(17, 31, by = 1),                        limits = c(17, 31),                               labels = c("17:00", "18:00", "19:00", "20:00", "21:00", "22:00", "23:00", "00:00", "01:00", "02:00", "03:00", "04:00", "05:00", "06:00", "07:00"),                       expand = c(0, 0)) +
  scale_y_continuous(name = "Density") + ggtitle("January") +
  theme(text = element_text(size=10), axis.text.x = element_text(angle = 90))
Rmt_bd_jan_plot

Rmt_bd_feb_plot <- ggplot() + 
  geom_line(data=density_bd_rmt_feb,aes(x=x,y=y),linewidth=1.5,color="black") +
  scale_x_continuous(name = "Hour",                                             breaks = seq(17, 31, by = 1),                        limits = c(17, 31),                               labels = c("17:00", "18:00", "19:00", "20:00", "21:00", "22:00", "23:00", "00:00", "01:00", "02:00", "03:00", "04:00", "05:00", "06:00", "07:00"),                       expand = c(0, 0)) +
  scale_y_continuous(name = "Density") + ggtitle("February") +
  theme(text = element_text(size=10), axis.text.x = element_text(angle = 90))
Rmt_bd_feb_plot

Rmt_bd_mar_plot <- ggplot() + 
  geom_line(data=density_bd_rmt_mar,aes(x=x,y=y),linewidth=1.5,color="black") +
  scale_x_continuous(name = "Hour",                                             breaks = seq(17, 31, by = 1),                        limits = c(17, 31),                               labels = c("17:00", "18:00", "19:00", "20:00", "21:00", "22:00", "23:00", "00:00", "01:00", "02:00", "03:00", "04:00", "05:00", "06:00", "07:00"),                       expand = c(0, 0)) +
  scale_y_continuous(name = "Density") + ggtitle("March") +
  theme(text = element_text(size=10), axis.text.x = element_text(angle = 90))
Rmt_bd_mar_plot

Rmt_bd_apr_plot <- ggplot() + 
  geom_line(data=density_bd_rmt_apr,aes(x=x,y=y),linewidth=1.5,color="black") +
  scale_x_continuous(name = "Hour",                                             breaks = seq(17, 31, by = 1),                        limits = c(17, 31),                               labels = c("17:00", "18:00", "19:00", "20:00", "21:00", "22:00", "23:00", "00:00", "01:00", "02:00", "03:00", "04:00", "05:00", "06:00", "07:00"),                       expand = c(0, 0)) +
  scale_y_continuous(name = "Density") + ggtitle("April") +
  theme(text = element_text(size=10), axis.text.x = element_text(angle = 90))
Rmt_bd_apr_plot

Rmt_bd_may_plot <- ggplot() + 
  geom_line(data=density_bd_rmt_may,aes(x=x,y=y),linewidth=1.5,color="black") +
  scale_x_continuous(name = "Hour",                                             breaks = seq(17, 31, by = 1),                        limits = c(17, 31),                               labels = c("17:00", "18:00", "19:00", "20:00", "21:00", "22:00", "23:00", "00:00", "01:00", "02:00", "03:00", "04:00", "05:00", "06:00", "07:00"),                       expand = c(0, 0)) +
  scale_y_continuous(name = "Density") + ggtitle("May") +
  theme(text = element_text(size=10), axis.text.x = element_text(angle = 90))
Rmt_bd_may_plot

Rmt_bd_jun_plot <- ggplot() + 
  geom_line(data=density_bd_rmt_jun,aes(x=x,y=y),linewidth=1.5,color="black") +
  scale_x_continuous(name = "Hour",                                             breaks = seq(17, 31, by = 1),                        limits = c(17, 31),                               labels = c("17:00", "18:00", "19:00", "20:00", "21:00", "22:00", "23:00", "00:00", "01:00", "02:00", "03:00", "04:00", "05:00", "06:00", "07:00"),                       expand = c(0, 0)) +
  scale_y_continuous(name = "Density") + ggtitle("June") +
  theme(text = element_text(size=10), axis.text.x = element_text(angle = 90))
Rmt_bd_jun_plot

Rmt_bd_jul_plot <- ggplot() + 
  geom_line(data=density_bd_rmt_jul,aes(x=x,y=y),linewidth=1.5,color="black") +
  scale_x_continuous(name = "Hour",                                             breaks = seq(17, 31, by = 1),                        limits = c(17, 31),                               labels = c("17:00", "18:00", "19:00", "20:00", "21:00", "22:00", "23:00", "00:00", "01:00", "02:00", "03:00", "04:00", "05:00", "06:00", "07:00"),                       expand = c(0, 0)) +
  scale_y_continuous(name = "Density") + ggtitle("July") +
  theme(text = element_text(size=10), axis.text.x = element_text(angle = 90))
Rmt_bd_jul_plot

Rmt_bd_aug_plot <- ggplot() + 
  geom_line(data=density_bd_rmt_aug,aes(x=x,y=y),linewidth=1.5,color="black") +
  scale_x_continuous(name = "Hour",                                             breaks = seq(17, 31, by = 1),                        limits = c(17, 31),                               labels = c("17:00", "18:00", "19:00", "20:00", "21:00", "22:00", "23:00", "00:00", "01:00", "02:00", "03:00", "04:00", "05:00", "06:00", "07:00"),                       expand = c(0, 0)) +
  scale_y_continuous(name = "Density") + ggtitle("August") +
  theme(text = element_text(size=10), axis.text.x = element_text(angle = 90))
Rmt_bd_aug_plot

Rmt_bd_sep_plot <- ggplot() + 
  geom_line(data=density_bd_rmt_sep,aes(x=x,y=y),linewidth=1.5,color="black") +
  scale_x_continuous(name = "Hour",                                             breaks = seq(17, 31, by = 1),                        limits = c(17, 31),                               labels = c("17:00", "18:00", "19:00", "20:00", "21:00", "22:00", "23:00", "00:00", "01:00", "02:00", "03:00", "04:00", "05:00", "06:00", "07:00"),                       expand = c(0, 0)) +
  scale_y_continuous(name = "Density") + ggtitle("September") +
  theme(text = element_text(size=10), axis.text.x = element_text(angle = 90))
Rmt_bd_sep_plot

Rmt_bd_oct_plot <- ggplot() + 
  geom_line(data=density_bd_rmt_oct,aes(x=x,y=y),linewidth=1.5,color="black") +
  scale_x_continuous(name = "Hour",                                             breaks = seq(17, 31, by = 1),                        limits = c(17, 31),                               labels = c("17:00", "18:00", "19:00", "20:00", "21:00", "22:00", "23:00", "00:00", "01:00", "02:00", "03:00", "04:00", "05:00", "06:00", "07:00"),                       expand = c(0, 0)) +
  scale_y_continuous(name = "Density") + ggtitle("October") +
  theme(text = element_text(size=10), axis.text.x = element_text(angle = 90))
Rmt_bd_oct_plot

Rmt_bd_nov_plot <- ggplot() + 
  geom_line(data=density_bd_rmt_nov,aes(x=x,y=y),linewidth=1.5,color="black") +
  scale_x_continuous(name = "Hour",                                             
                     breaks = seq(17, 31, by = 1),                        
                     limits = c(17, 31),                               
                     labels = c("17:00", "18:00", "19:00", "20:00", "21:00", "22:00", "23:00", "00:00", "01:00", "02:00", "03:00", "04:00", "05:00", "06:00", "07:00"),                       
                     expand = c(0, 0)) +
  scale_y_continuous(name = "Density") + ggtitle("November") +
  theme(text = element_text(size=10), axis.text.x = element_text(angle = 90))
Rmt_bd_nov_plot

Rmt_bd_dec_plot <- ggplot() + 
  geom_line(data=density_bd_rmt_dec,aes(x=x,y=y),linewidth=1.5,color="black") +
  scale_x_continuous(name = "Hour",                                            
                     breaks = seq(17, 31, by = 1),                       
                     limits = c(17, 31),                               
                     labels = c("17:00", "18:00", "19:00", "20:00", "21:00", "22:00", "23:00", "00:00", "01:00", "02:00", "03:00", "04:00", "05:00", "06:00", "07:00"),                       
                     expand = c(0, 0)) +
  scale_y_continuous(name = "Density") + ggtitle("December") +
  theme_bw() +
  theme(text = element_text(size=10), axis.text.x = element_text(angle = 90), plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank()) +
  theme(axis.line = element_line(color = 'black'))
Rmt_bd_dec_plot

###### PLOT COMBINE SPECIES #########-----------

## MB ###------

mb_jan_plot <- ggplot() + 
  geom_line(data=density_mb_Rhino_la_jan,aes(x=x,y=y),linewidth=1.5,color="#FDE725FF") +
  geom_line(data=density_mb_hippo_jan,aes(x=x,y=y),linewidth=1.5,color = "#E7298A") +
  geom_line(data=density_mb_Minio_jan,aes(x=x,y=y),linewidth=1.5,color="#7570B3") +
  geom_line(data=density_mb_Rhino_de_jan,aes(x=x,y=y),linewidth=1.5,color="skyblue2") +
  scale_x_continuous(name = "Hour",                                            
                     breaks = seq(17, 31, by = 1),                       
                     limits = c(17, 31),                               
                     labels = c("17:00", "18:00", "19:00", "20:00", "21:00", "22:00", "23:00", "00:00", "01:00", "02:00", "03:00", "04:00", "05:00", "06:00", "07:00"),                       
                     expand = c(0, 0)) +
  scale_y_continuous(name = "Density") + ggtitle("January") +
  theme_bw() +
  theme(text = element_text(size=10), axis.text.x = element_text(angle = 90), plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank()) +
  theme(axis.line = element_line(color = 'black'))
mb_jan_plot

mb_feb_plot <- ggplot() + 
  geom_line(data=density_mb_Rhino_la_feb,aes(x=x,y=y),linewidth=1.5,color="#FDE725FF") +
  geom_line(data=density_mb_hippo_feb,aes(x=x,y=y),linewidth=1.5,color = "#E7298A") +
  geom_line(data=density_mb_Minio_feb,aes(x=x,y=y),linewidth=1.5,color="#7570B3") +
  geom_line(data=density_mb_Rhino_de_feb,aes(x=x,y=y),linewidth=1.5,color="skyblue2") +
  scale_x_continuous(name = "Hour",                                            
                     breaks = seq(17, 31, by = 1),                       
                     limits = c(17, 31),                               
                     labels = c("17:00", "18:00", "19:00", "20:00", "21:00", "22:00", "23:00", "00:00", "01:00", "02:00", "03:00", "04:00", "05:00", "06:00", "07:00"),                       
                     expand = c(0, 0)) +
  scale_y_continuous(name = "Density") + ggtitle("February") +
  #scale_color_manual(name = "Acoustic group", values = c("Rhinolophus Macro Tria group" = "#FDE725FF", "Hipposideros caffer" = "#E7298A", "Miniopterus group" = "#7570B3", "Rhinolophus denti" = "skyblue2")) +
  theme_bw() +
  theme(text = element_text(size=10), axis.text.x = element_text(angle = 90), plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank()) +
  theme(axis.line = element_line(color = 'black'))
mb_feb_plot

mb_mar_plot <- ggplot() + 
  geom_line(data=density_mb_Rhino_la_mar,aes(x=x,y=y),linewidth=1.5,color="#FDE725FF") +
  geom_line(data=density_mb_hippo_mar,aes(x=x,y=y),linewidth=1.5,color = "#E7298A") +
  geom_line(data=density_mb_Minio_mar,aes(x=x,y=y),linewidth=1.5,color="#7570B3") +
  geom_line(data=density_mb_Rhino_de_mar,aes(x=x,y=y),linewidth=1.5,color="skyblue2") +
  scale_x_continuous(name = "Hour",                                            
                     breaks = seq(17, 31, by = 1),                       
                     limits = c(17, 31),                               
                     labels = c("17:00", "18:00", "19:00", "20:00", "21:00", "22:00", "23:00", "00:00", "01:00", "02:00", "03:00", "04:00", "05:00", "06:00", "07:00"),                       
                     expand = c(0, 0)) +
  scale_y_continuous(name = "Density") + ggtitle("March") +
  theme_bw() +
  theme(text = element_text(size=10), axis.text.x = element_text(angle = 90), plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank()) +
  theme(axis.line = element_line(color = 'black')) + scale_color_manual(values=c("#FDE725FF", "#E7298A", "#7570B3", "skyblue2"))
mb_mar_plot

mb_apr_plot <- ggplot() + 
  geom_line(data=density_mb_Rhino_la_apr,aes(x=x,y=y),linewidth=1.5,color="#FDE725FF") +
  geom_line(data=density_mb_hippo_apr,aes(x=x,y=y),linewidth=1.5,color = "#E7298A") +
  geom_line(data=density_mb_Minio_apr,aes(x=x,y=y),linewidth=1.5,color="#7570B3") +
  geom_line(data=density_mb_Rhino_de_apr,aes(x=x,y=y),linewidth=1.5,color="skyblue2") +
  scale_x_continuous(name = "Hour",                                            
                     breaks = seq(17, 31, by = 1),                       
                     limits = c(17, 31),                               
                     labels = c("17:00", "18:00", "19:00", "20:00", "21:00", "22:00", "23:00", "00:00", "01:00", "02:00", "03:00", "04:00", "05:00", "06:00", "07:00"),                       
                     expand = c(0, 0)) +
  scale_y_continuous(name = "Density") + ggtitle("April") +
  theme_bw() +
  theme(text = element_text(size=10), axis.text.x = element_text(angle = 90), plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank()) +
  theme(axis.line = element_line(color = 'black'))
mb_apr_plot

mb_may_plot <- ggplot() + 
  geom_line(data=density_mb_Rhino_la_may,aes(x=x,y=y),linewidth=1.5,color="#FDE725FF") +
  geom_line(data=density_mb_hippo_may,aes(x=x,y=y),linewidth=1.5,color = "#E7298A") +
  geom_line(data=density_mb_Minio_may,aes(x=x,y=y),linewidth=1.5,color="#7570B3") +
  geom_line(data=density_mb_Rhino_de_may,aes(x=x,y=y),linewidth=1.5,color="skyblue2") +
  scale_x_continuous(name = "Hour",                                            
                     breaks = seq(17, 31, by = 1),                       
                     limits = c(17, 31),                               
                     labels = c("17:00", "18:00", "19:00", "20:00", "21:00", "22:00", "23:00", "00:00", "01:00", "02:00", "03:00", "04:00", "05:00", "06:00", "07:00"),                       
                     expand = c(0, 0)) +
  scale_y_continuous(name = "Density") + ggtitle("May") +
  theme_bw() +
  theme(text = element_text(size=10), axis.text.x = element_text(angle = 90), plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank()) +
  theme(axis.line = element_line(color = 'black'))
mb_may_plot

mb_jun_plot <- ggplot() + 
  geom_line(data=density_mb_Rhino_la_jun,aes(x=x,y=y),linewidth=1.5,color="#FDE725FF") +
  geom_line(data=density_mb_hippo_jun,aes(x=x,y=y),linewidth=1.5,color = "#E7298A") +
  geom_line(data=density_mb_Minio_jun,aes(x=x,y=y),linewidth=1.5,color="#7570B3") +
  geom_line(data=density_mb_Rhino_de_jun,aes(x=x,y=y),linewidth=1.5,color="skyblue2") +
  scale_x_continuous(name = "Hour",                                            
                     breaks = seq(17, 31, by = 1),                       
                     limits = c(17, 31),                               
                     labels = c("17:00", "18:00", "19:00", "20:00", "21:00", "22:00", "23:00", "00:00", "01:00", "02:00", "03:00", "04:00", "05:00", "06:00", "07:00"),                       
                     expand = c(0, 0)) +
  scale_y_continuous(name = "Density") + ggtitle("June") +
  theme_bw() +
  theme(text = element_text(size=10), axis.text.x = element_text(angle = 90), plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank()) +
  theme(axis.line = element_line(color = 'black'))
mb_jun_plot

mb_jul_plot <- ggplot() + 
  geom_line(data=density_mb_Rhino_la_jul,aes(x=x,y=y),linewidth=1.5,color="#FDE725FF") +
  geom_line(data=density_mb_hippo_jul,aes(x=x,y=y),linewidth=1.5,color = "#E7298A") +
  geom_line(data=density_mb_Minio_jul,aes(x=x,y=y),linewidth=1.5,color="#7570B3") +
  geom_line(data=density_mb_Rhino_de_jul,aes(x=x,y=y),linewidth=1.5,color="skyblue2") +
  scale_x_continuous(name = "Hour",                                            
                     breaks = seq(17, 31, by = 1),                       
                     limits = c(17, 31),                               
                     labels = c("17:00", "18:00", "19:00", "20:00", "21:00", "22:00", "23:00", "00:00", "01:00", "02:00", "03:00", "04:00", "05:00", "06:00", "07:00"),                       
                     expand = c(0, 0)) +
  scale_y_continuous(name = "Density") + ggtitle("July") +
  theme_bw() +
  theme(text = element_text(size=10), axis.text.x = element_text(angle = 90), plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank()) +
  theme(axis.line = element_line(color = 'black'))
mb_jul_plot

mb_aug_plot <- ggplot() + 
  geom_line(data=density_mb_Rhino_la_aug,aes(x=x,y=y),linewidth=1.5,color="#FDE725FF") +
  geom_line(data=density_mb_hippo_aug,aes(x=x,y=y),linewidth=1.5,color = "#E7298A") +
  geom_line(data=density_mb_Minio_aug,aes(x=x,y=y),linewidth=1.5,color="#7570B3") +
  geom_line(data=density_mb_Rhino_de_aug,aes(x=x,y=y),linewidth=1.5,color="skyblue2") +
  scale_x_continuous(name = "Hour",                                            
                     breaks = seq(17, 31, by = 1),                       
                     limits = c(17, 31),                               
                     labels = c("17:00", "18:00", "19:00", "20:00", "21:00", "22:00", "23:00", "00:00", "01:00", "02:00", "03:00", "04:00", "05:00", "06:00", "07:00"),                       
                     expand = c(0, 0)) +
  scale_y_continuous(name = "Density") + ggtitle("August") +
  theme_bw() +
  theme(text = element_text(size=10), axis.text.x = element_text(angle = 90), plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank()) +
  theme(axis.line = element_line(color = 'black'))
mb_aug_plot

mb_sep_plot <- ggplot() + 
  geom_line(data=density_mb_Rhino_la_sep,aes(x=x,y=y),linewidth=1.5,color="#FDE725FF") +
  geom_line(data=density_mb_hippo_sep,aes(x=x,y=y),linewidth=1.5,color = "#E7298A") +
  geom_line(data=density_mb_Minio_sep,aes(x=x,y=y),linewidth=1.5,color="#7570B3") +
  geom_line(data=density_mb_Rhino_de_sep,aes(x=x,y=y),linewidth=1.5,color="skyblue2") +
  scale_x_continuous(name = "Hour",                                            
                     breaks = seq(17, 31, by = 1),                       
                     limits = c(17, 31),                               
                     labels = c("17:00", "18:00", "19:00", "20:00", "21:00", "22:00", "23:00", "00:00", "01:00", "02:00", "03:00", "04:00", "05:00", "06:00", "07:00"),                       
                     expand = c(0, 0)) +
  scale_y_continuous(name = "Density") + ggtitle("September") +
  theme_bw() +
  theme(text = element_text(size=10), axis.text.x = element_text(angle = 90), plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank()) +
  theme(axis.line = element_line(color = 'black'))
mb_sep_plot

mb_oct_plot <- ggplot() + 
  geom_line(data=density_mb_Rhino_la_oct,aes(x=x,y=y),linewidth=1.5,color="#FDE725FF") +
  geom_line(data=density_mb_hippo_oct,aes(x=x,y=y),linewidth=1.5,color = "#E7298A") +
  geom_line(data=density_mb_Minio_oct,aes(x=x,y=y),linewidth=1.5,color="#7570B3") +
  geom_line(data=density_mb_Rhino_de_oct,aes(x=x,y=y),linewidth=1.5,color="skyblue2") +
  scale_x_continuous(name = "Hour",                                            
                     breaks = seq(17, 31, by = 1),                       
                     limits = c(17, 31),                               
                     labels = c("17:00", "18:00", "19:00", "20:00", "21:00", "22:00", "23:00", "00:00", "01:00", "02:00", "03:00", "04:00", "05:00", "06:00", "07:00"),                       
                     expand = c(0, 0)) +
  scale_y_continuous(name = "Density") + ggtitle("October") +
  theme_bw() +
  theme(text = element_text(size=10), axis.text.x = element_text(angle = 90), plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank()) +
  theme(axis.line = element_line(color = 'black'))
mb_oct_plot

mb_nov_plot <- ggplot() + 
  geom_line(data=density_mb_Rhino_la_nov,aes(x=x,y=y),linewidth=1.5,color="#FDE725FF") +
  geom_line(data=density_mb_hippo_nov,aes(x=x,y=y),linewidth=1.5,color = "#E7298A") +
  geom_line(data=density_mb_Minio_nov,aes(x=x,y=y),linewidth=1.5,color="#7570B3") +
  geom_line(data=density_mb_Rhino_de_nov,aes(x=x,y=y),linewidth=1.5,color="skyblue2") +
  scale_x_continuous(name = "Hour",                                            
                     breaks = seq(17, 31, by = 1),                       
                     limits = c(17, 31),                               
                     labels = c("17:00", "18:00", "19:00", "20:00", "21:00", "22:00", "23:00", "00:00", "01:00", "02:00", "03:00", "04:00", "05:00", "06:00", "07:00"),                       
                     expand = c(0, 0)) +
  scale_y_continuous(name = "Density") + ggtitle("November") +
  theme_bw() +
  theme(text = element_text(size=10), axis.text.x = element_text(angle = 90), plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank()) +
  theme(axis.line = element_line(color = 'black'))
mb_nov_plot

mb_dec_plot <- ggplot() + 
  geom_line(data=density_mb_Rhino_la_dec,aes(x=x,y=y),linewidth=1.5,color="#FDE725FF") +
  geom_line(data=density_mb_hippo_dec,aes(x=x,y=y),linewidth=1.5,color = "#E7298A") +
  geom_line(data=density_mb_Minio_dec,aes(x=x,y=y),linewidth=1.5,color="#7570B3") +
  geom_line(data=density_mb_Rhino_de_dec,aes(x=x,y=y),linewidth=1.5,color="skyblue2") +
  scale_x_continuous(name = "Hour",                                            
                     breaks = seq(17, 31, by = 1),                       
                     limits = c(17, 31),                               
                     labels = c("17:00", "18:00", "19:00", "20:00", "21:00", "22:00", "23:00", "00:00", "01:00", "02:00", "03:00", "04:00", "05:00", "06:00", "07:00"),                       
                     expand = c(0, 0)) +
  scale_y_continuous(name = "Density") + ggtitle("December") +
  theme_bw() +
  theme(text = element_text(size=10), axis.text.x = element_text(angle = 90), plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank()) +
  theme(axis.line = element_line(color = 'black'))
mb_dec_plot

# Combine the plots using facet_wrap ----
combined_plot_MB <- list(mb_jan_plot, mb_feb_plot, mb_mar_plot, mb_apr_plot, mb_may_plot, mb_jun_plot, mb_jul_plot, mb_aug_plot, mb_sep_plot, mb_oct_plot, mb_nov_plot, mb_dec_plot)

# Your color and label specifications
legend_colors <- c("#FDE725FF", "#E7298A", "#7570B3", "skyblue2")
legend_labels <- c("Rhinolophus landeri-alcyone", "Hipposideros caffer", "Miniopterus group", "Rhinolophus denti")

# Add legend to the first plot in the list
combined_plot_MB[[1]] <- combined_plot_MB[[1]] +
  scale_color_manual(name = "Legend",
                     values = setNames(legend_colors, legend_labels),
                     labels = legend_labels) +
  theme(legend.position = "top",
        legend.key = element_rect(fill = "white", color = "white"))


plot_MB <- patchwork::wrap_plots(combined_plot_MB)
plot_MB


## BD ###------

bd_jan_plot <- ggplot() + 
  geom_line(data=density_bd_rmt_jan,aes(x=x,y=y),linewidth=1.5,color="#1B9E77") +
  geom_line(data=density_bd_hippo_jan,aes(x=x,y=y),linewidth=1.5,color = "#E7298A") +
  geom_line(data=density_bd_Minio_jan,aes(x=x,y=y),linewidth=1.5,color="#7570B3") +
  geom_line(data=density_bd_Rhino_de_jan,aes(x=x,y=y),linewidth=1.5,color="skyblue2") +
  scale_x_continuous(name = "Hour",                                            
                     breaks = seq(17, 31, by = 1),                       
                     limits = c(17, 31),                               
                     labels = c("17:00", "18:00", "19:00", "20:00", "21:00", "22:00", "23:00", "00:00", "01:00", "02:00", "03:00", "04:00", "05:00", "06:00", "07:00"),                       
                     expand = c(0, 0)) +
  scale_y_continuous(name = "Density") + ggtitle("January") +
  theme_bw() +
  theme(text = element_text(size=10), axis.text.x = element_text(angle = 90), plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank()) +
  theme(axis.line = element_line(color = 'black'))
bd_jan_plot

bd_feb_plot <- ggplot() + 
  geom_line(data=density_bd_rmt_feb,aes(x=x,y=y),linewidth=1.5,color="#1B9E77") +
  geom_line(data=density_bd_hippo_feb,aes(x=x,y=y),linewidth=1.5,color = "#E7298A") +
  geom_line(data=density_bd_Minio_feb,aes(x=x,y=y),linewidth=1.5,color="#7570B3") +
  geom_line(data=density_bd_Rhino_de_feb,aes(x=x,y=y),linewidth=1.5,color="skyblue2") +
  scale_x_continuous(name = "Hour",                                            
                     breaks = seq(17, 31, by = 1),                       
                     limits = c(17, 31),                               
                     labels = c("17:00", "18:00", "19:00", "20:00", "21:00", "22:00", "23:00", "00:00", "01:00", "02:00", "03:00", "04:00", "05:00", "06:00", "07:00"),                       
                     expand = c(0, 0)) +
  scale_y_continuous(name = "Density") + ggtitle("February") +
  #scale_color_manual(name = "Acoustic group", values = c("Rhinolophus Macro Tria group" = "#1B9E77", "Hipposideros caffer" = "#E7298A", "Miniopterus group" = "#7570B3", "Rhinolophus denti" = "skyblue2")) +
  theme_bw() +
  theme(text = element_text(size=10), axis.text.x = element_text(angle = 90), plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank()) +
  theme(axis.line = element_line(color = 'black'))
bd_feb_plot

bd_mar_plot <- ggplot() + 
  geom_line(data=density_bd_rmt_mar,aes(x=x,y=y),linewidth=1.5,color="#1B9E77") +
  geom_line(data=density_bd_hippo_mar,aes(x=x,y=y),linewidth=1.5,color = "#E7298A") +
  geom_line(data=density_bd_Minio_mar,aes(x=x,y=y),linewidth=1.5,color="#7570B3") +
  geom_line(data=density_bd_Rhino_de_mar,aes(x=x,y=y),linewidth=1.5,color="skyblue2") +
  scale_x_continuous(name = "Hour",                                            
                     breaks = seq(17, 31, by = 1),                       
                     limits = c(17, 31),                               
                     labels = c("17:00", "18:00", "19:00", "20:00", "21:00", "22:00", "23:00", "00:00", "01:00", "02:00", "03:00", "04:00", "05:00", "06:00", "07:00"),                       
                     expand = c(0, 0)) +
  scale_y_continuous(name = "Density") + ggtitle("March") +
  theme_bw() +
  theme(text = element_text(size=10), axis.text.x = element_text(angle = 90), plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank()) +
  theme(axis.line = element_line(color = 'black')) + scale_color_manual(values=c("#1B9E77", "#E7298A", "#7570B3", "skyblue2"))
bd_mar_plot

bd_apr_plot <- ggplot() + 
  geom_line(data=density_bd_rmt_apr,aes(x=x,y=y),linewidth=1.5,color="#1B9E77") +
  geom_line(data=density_bd_hippo_apr,aes(x=x,y=y),linewidth=1.5,color = "#E7298A") +
  geom_line(data=density_bd_Minio_apr,aes(x=x,y=y),linewidth=1.5,color="#7570B3") +
  geom_line(data=density_bd_Rhino_de_apr,aes(x=x,y=y),linewidth=1.5,color="skyblue2") +
  scale_x_continuous(name = "Hour",                                            
                     breaks = seq(17, 31, by = 1),                       
                     limits = c(17, 31),                               
                     labels = c("17:00", "18:00", "19:00", "20:00", "21:00", "22:00", "23:00", "00:00", "01:00", "02:00", "03:00", "04:00", "05:00", "06:00", "07:00"),                       
                     expand = c(0, 0)) +
  scale_y_continuous(name = "Density") + ggtitle("April") +
  theme_bw() +
  theme(text = element_text(size=10), axis.text.x = element_text(angle = 90), plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank()) +
  theme(axis.line = element_line(color = 'black'))
bd_apr_plot

bd_may_plot <- ggplot() + 
  geom_line(data=density_bd_rmt_may,aes(x=x,y=y),linewidth=1.5,color="#1B9E77") +
  geom_line(data=density_bd_hippo_may,aes(x=x,y=y),linewidth=1.5,color = "#E7298A") +
  geom_line(data=density_bd_Minio_may,aes(x=x,y=y),linewidth=1.5,color="#7570B3") +
  geom_line(data=density_bd_Rhino_de_may,aes(x=x,y=y),linewidth=1.5,color="skyblue2") +
  scale_x_continuous(name = "Hour",                                            
                     breaks = seq(17, 31, by = 1),                       
                     limits = c(17, 31),                               
                     labels = c("17:00", "18:00", "19:00", "20:00", "21:00", "22:00", "23:00", "00:00", "01:00", "02:00", "03:00", "04:00", "05:00", "06:00", "07:00"),                       
                     expand = c(0, 0)) +
  scale_y_continuous(name = "Density") + ggtitle("May") +
  theme_bw() +
  theme(text = element_text(size=10), axis.text.x = element_text(angle = 90), plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank()) +
  theme(axis.line = element_line(color = 'black'))
bd_may_plot

bd_jun_plot <- ggplot() + 
  geom_line(data=density_bd_rmt_jun,aes(x=x,y=y),linewidth=1.5,color="#1B9E77") +
  geom_line(data=density_bd_hippo_jun,aes(x=x,y=y),linewidth=1.5,color = "#E7298A") +
  geom_line(data=density_bd_Minio_jun,aes(x=x,y=y),linewidth=1.5,color="#7570B3") +
  geom_line(data=density_bd_Rhino_de_jun,aes(x=x,y=y),linewidth=1.5,color="skyblue2") +
  scale_x_continuous(name = "Hour",                                            
                     breaks = seq(17, 31, by = 1),                       
                     limits = c(17, 31),                               
                     labels = c("17:00", "18:00", "19:00", "20:00", "21:00", "22:00", "23:00", "00:00", "01:00", "02:00", "03:00", "04:00", "05:00", "06:00", "07:00"),                       
                     expand = c(0, 0)) +
  scale_y_continuous(name = "Density") + ggtitle("June") +
  theme_bw() +
  theme(text = element_text(size=10), axis.text.x = element_text(angle = 90), plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank()) +
  theme(axis.line = element_line(color = 'black'))
bd_jun_plot

bd_jul_plot <- ggplot() + 
  geom_line(data=density_bd_rmt_jul,aes(x=x,y=y),linewidth=1.5,color="#1B9E77") +
  geom_line(data=density_bd_hippo_jul,aes(x=x,y=y),linewidth=1.5,color = "#E7298A") +
  geom_line(data=density_bd_Minio_jul,aes(x=x,y=y),linewidth=1.5,color="#7570B3") +
  geom_line(data=density_bd_Rhino_de_jul,aes(x=x,y=y),linewidth=1.5,color="skyblue2") +
  scale_x_continuous(name = "Hour",                                            
                     breaks = seq(17, 31, by = 1),                       
                     limits = c(17, 31),                               
                     labels = c("17:00", "18:00", "19:00", "20:00", "21:00", "22:00", "23:00", "00:00", "01:00", "02:00", "03:00", "04:00", "05:00", "06:00", "07:00"),                       
                     expand = c(0, 0)) +
  scale_y_continuous(name = "Density") + ggtitle("July") +
  theme_bw() +
  theme(text = element_text(size=10), axis.text.x = element_text(angle = 90), plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank()) +
  theme(axis.line = element_line(color = 'black'))
bd_jul_plot

bd_aug_plot <- ggplot() + 
  geom_line(data=density_bd_rmt_aug,aes(x=x,y=y),linewidth=1.5,color="#1B9E77") +
  geom_line(data=density_bd_hippo_aug,aes(x=x,y=y),linewidth=1.5,color = "#E7298A") +
  geom_line(data=density_bd_Minio_aug,aes(x=x,y=y),linewidth=1.5,color="#7570B3") +
  geom_line(data=density_bd_Rhino_de_aug,aes(x=x,y=y),linewidth=1.5,color="skyblue2") +
  scale_x_continuous(name = "Hour",                                            
                     breaks = seq(17, 31, by = 1),                       
                     limits = c(17, 31),                               
                     labels = c("17:00", "18:00", "19:00", "20:00", "21:00", "22:00", "23:00", "00:00", "01:00", "02:00", "03:00", "04:00", "05:00", "06:00", "07:00"),                       
                     expand = c(0, 0)) +
  scale_y_continuous(name = "Density") + ggtitle("August") +
  theme_bw() +
  theme(text = element_text(size=10), axis.text.x = element_text(angle = 90), plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank()) +
  theme(axis.line = element_line(color = 'black'))
bd_aug_plot

bd_sep_plot <- ggplot() + 
  geom_line(data=density_bd_rmt_sep,aes(x=x,y=y),linewidth=1.5,color="#1B9E77") +
  geom_line(data=density_bd_hippo_sep,aes(x=x,y=y),linewidth=1.5,color = "#E7298A") +
  geom_line(data=density_bd_Minio_sep,aes(x=x,y=y),linewidth=1.5,color="#7570B3") +
  geom_line(data=density_bd_Rhino_de_sep,aes(x=x,y=y),linewidth=1.5,color="skyblue2") +
  scale_x_continuous(name = "Hour",                                            
                     breaks = seq(17, 31, by = 1),                       
                     limits = c(17, 31),                               
                     labels = c("17:00", "18:00", "19:00", "20:00", "21:00", "22:00", "23:00", "00:00", "01:00", "02:00", "03:00", "04:00", "05:00", "06:00", "07:00"),                       
                     expand = c(0, 0)) +
  scale_y_continuous(name = "Density") + ggtitle("September") +
  theme_bw() +
  theme(text = element_text(size=10), axis.text.x = element_text(angle = 90), plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank()) +
  theme(axis.line = element_line(color = 'black'))
bd_sep_plot

bd_oct_plot <- ggplot() + 
  geom_line(data=density_bd_rmt_oct,aes(x=x,y=y),linewidth=1.5,color="#1B9E77") +
  geom_line(data=density_bd_hippo_oct,aes(x=x,y=y),linewidth=1.5,color = "#E7298A") +
  geom_line(data=density_bd_Minio_oct,aes(x=x,y=y),linewidth=1.5,color="#7570B3") +
  geom_line(data=density_bd_Rhino_de_oct,aes(x=x,y=y),linewidth=1.5,color="skyblue2") +
  scale_x_continuous(name = "Hour",                                            
                     breaks = seq(17, 31, by = 1),                       
                     limits = c(17, 31),                               
                     labels = c("17:00", "18:00", "19:00", "20:00", "21:00", "22:00", "23:00", "00:00", "01:00", "02:00", "03:00", "04:00", "05:00", "06:00", "07:00"),                       
                     expand = c(0, 0)) +
  scale_y_continuous(name = "Density") + ggtitle("October") +
  theme_bw() +
  theme(text = element_text(size=10), axis.text.x = element_text(angle = 90), plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank()) +
  theme(axis.line = element_line(color = 'black'))
bd_oct_plot

bd_nov_plot <- ggplot() + 
  geom_line(data=density_bd_rmt_nov,aes(x=x,y=y),linewidth=1.5,color="#1B9E77") +
  geom_line(data=density_bd_hippo_nov,aes(x=x,y=y),linewidth=1.5,color = "#E7298A") +
  geom_line(data=density_bd_Minio_nov,aes(x=x,y=y),linewidth=1.5,color="#7570B3") +
  geom_line(data=density_bd_Rhino_de_nov,aes(x=x,y=y),linewidth=1.5,color="skyblue2") +
  scale_x_continuous(name = "Hour",                                            
                     breaks = seq(17, 31, by = 1),                       
                     limits = c(17, 31),                               
                     labels = c("17:00", "18:00", "19:00", "20:00", "21:00", "22:00", "23:00", "00:00", "01:00", "02:00", "03:00", "04:00", "05:00", "06:00", "07:00"),                       
                     expand = c(0, 0)) +
  scale_y_continuous(name = "Density") + ggtitle("November") +
  theme_bw() +
  theme(text = element_text(size=10), axis.text.x = element_text(angle = 90), plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank()) +
  theme(axis.line = element_line(color = 'black'))
bd_nov_plot

bd_dec_plot <- ggplot() + 
  geom_line(data=density_bd_rmt_dec,aes(x=x,y=y),linewidth=1.5,color="#1B9E77") +
  geom_line(data=density_bd_hippo_dec,aes(x=x,y=y),linewidth=1.5,color = "#E7298A") +
  geom_line(data=density_bd_Minio_dec,aes(x=x,y=y),linewidth=1.5,color="#7570B3") +
  geom_line(data=density_bd_Rhino_de_dec,aes(x=x,y=y),linewidth=1.5,color="skyblue2") +
  scale_x_continuous(name = "Hour",                                            
                     breaks = seq(17, 31, by = 1),                       
                     limits = c(17, 31),                               
                     labels = c("17:00", "18:00", "19:00", "20:00", "21:00", "22:00", "23:00", "00:00", "01:00", "02:00", "03:00", "04:00", "05:00", "06:00", "07:00"),                       
                     expand = c(0, 0)) +
  scale_y_continuous(name = "Density") + ggtitle("December") +
  theme_bw() +
  theme(text = element_text(size=10), axis.text.x = element_text(angle = 90), plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank()) +
  theme(axis.line = element_line(color = 'black'))
bd_dec_plot

# Combine the plots using facet_wrap
combined_plot_BD <- list(bd_jan_plot, bd_feb_plot, bd_mar_plot, bd_apr_plot, bd_may_plot, bd_jun_plot, bd_jul_plot, bd_aug_plot, bd_sep_plot, bd_oct_plot, bd_nov_plot, bd_dec_plot)

plot_BD <- patchwork::wrap_plots(combined_plot_BD)
plot_BD


