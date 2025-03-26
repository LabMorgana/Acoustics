###############################################################
### Script Graph Appendix 2:  Number of bat passes for each acoustic group according to month and site 
###                 (Boundou and Mont Belo Cave).#############-------
###############################################################

### Summary

# I. Load package L11-15
## II. For Hipposideros L24 - 184
### III.For Miniopterus group L189 - 405
#### IV. Rhinolophus landeri-alcyone group L407 - 596
##### V. Rhinolophus denti group L600 - 817
###### VI. Rhino Tria Gigas group L822- 954
####### VII. Plot all graph together L958 - 967


## I. load package and data
devtools::install_github("ricardo-bion/ggradar", 
                          dependencies = TRUE)
pacman::p_load(tidyverse, FactoMineR, factoextra, ggplot2, openxlsx, lubridate, ggradar, readxl, tidyr, stringr)

Hippo_ac <- read.csv(file = "~/Downloads/Hippo_ac.csv", header = T, sep = ",")
Minio_Allac <-  read.csv(file = "~/Downloads/Minio_Allac_script3.csv", header = T, sep = ",")
Rhino_denti_Allac <- read.csv(file = "~/Downloads/Rhino_denti_Allac_script3.csv", header = T, sep = ",")
Rum_lan_alcy_Allac <- read.csv(file = "~/Downloads/Rum_lan_alcy_Allac_script3.csv", header = T, sep = ",")
Rhino_macro_tria_all <- read.csv(file = "~/Downloads/Rhino_macro_tria_all_script3.csv", header = T, sep = ",")
  

#####################################################
###### First group : Hipposideros ###############--------------
##############################################################

## ### II_A. Bat cave exit (2h after sunset) ----------------------

Hippo_go <- Hippo_ac  |>
  mutate(secAfterSunset3 = as.numeric(secAfterSunset2)) |>
  mutate(month2 = zoo::as.yearmon(as.character(month), "%m")) |>
    mutate(month3 = format(month2, "%b")) |>
  dplyr::filter(dplyr::between(secAfterSunset3, -120, 7200)) 


###### II_A.1) make group for Mont Belo -----------------------

Hippo_go_mb <- Hippo_go |>
  filter(Sites == "Mont Belo") |>
  group_by(month2) |>
  summarise(Nb_batpass = n(), Nb_day_month = n_distinct(Date_formatted))  

Hippo_go_mb <- Hippo_go_mb  |>
mutate(Mean_Batpass_month = Nb_batpass/Nb_day_month) |>
arrange(month2)

bilan_Hippo_go_mb <- as.data.frame(Hippo_go_mb)
bilan_Hippo_go_mb$month2 <- as.factor(bilan_Hippo_go_mb$month2)
bilan_Hippo_go_mb$month2 <- gsub('2025', '', bilan_Hippo_go_mb$month2)
bilan_Hippo_go_mb$Mean_Batpass_month <- as.numeric(bilan_Hippo_go_mb$Mean_Batpass_month)


## remove columns that are not necessary

bilan_Hippo_go_mb2 <- bilan_Hippo_go_mb |>
  select(-Nb_batpass, -Nb_day_month)

## transform
Hippo_go_mb_matrix <- 
  bilan_Hippo_go_mb2 |> 
  pivot_wider(
    names_from = 'month2',
    values_from = 'Mean_Batpass_month'
  )

Hippo_go_mb_matrix <- data.frame(Hippo_go_mb_matrix)

###### II_A.2) make group for Boundou ------------

Hippo_go_bd <- Hippo_go |>
  filter(Sites == "Boundou") |>
  group_by(month2) |>
  summarise(Nb_batpass = n(), Nb_day_month = n_distinct(Date_formatted))  

Hippo_go_bd <- Hippo_go_bd  |>
mutate(Mean_Batpass_month = Nb_batpass/Nb_day_month) |>
arrange(month2)

bilan_Hippo_go_bd <- as.data.frame(Hippo_go_bd)
bilan_Hippo_go_bd$month2 <- as.factor(bilan_Hippo_go_bd$month2)
bilan_Hippo_go_bd$month2 <- gsub('2025', '', bilan_Hippo_go_bd$month2)
bilan_Hippo_go_bd$Mean_Batpass_month <- as.numeric(bilan_Hippo_go_bd$Mean_Batpass_month)

## remove columns that are not necessary

bilan_Hippo_go_bd2 <- bilan_Hippo_go_bd |>
  select(-Nb_batpass, -Nb_day_month)

## transform
Hippo_go_bd_matrix <- 
  bilan_Hippo_go_bd2 |> 
  pivot_wider(
    names_from = 'month2',
    values_from = 'Mean_Batpass_month'
  )

Hippo_go_bd_matrix <- data.frame(Hippo_go_bd_matrix)

# II_B. Bat cave re-enter (end of the night - before sunrise) ---------------

Hippo_re <- Hippo_ac  %>%
   mutate(month2 = zoo::as.yearmon(as.character(month), "%m")) |>
    mutate(month3 = format(month2, "%b")) |>
  dplyr::filter(dplyr::between(time_24, "04:00", "08:00"))


###### II_B.1) make group for Mont Belo (MB) -----------------------

Hippo_re_mb <- Hippo_re |>
  filter(Sites == "Mont Belo") |>
  group_by(month2) |>
  summarise(Nb_batpass = n(), Nb_day_month = n_distinct(Date_formatted)) 

Hippo_re_mb <- Hippo_re_mb  |>
mutate(Mean_Batpass_month = Nb_batpass/Nb_day_month) |>
arrange(month2)

bilan_Hippo_re_mb <- as.data.frame(Hippo_re_mb)
bilan_Hippo_re_mb$month2 <- as.factor(bilan_Hippo_re_mb$month2)
bilan_Hippo_re_mb$month2 <- gsub('2025', '', bilan_Hippo_re_mb$month2)
bilan_Hippo_re_mb$Mean_Batpass_month <- as.numeric(bilan_Hippo_re_mb$Mean_Batpass_month)

## remove columns that are not necessary

bilan_Hippo_re_mb2 <- bilan_Hippo_re_mb |>
  select(-Nb_batpass, -Nb_day_month)

Hippo_re_mb_matrix <- 
  bilan_Hippo_re_mb2 |> 
  pivot_wider(
    names_from = 'month2',
    values_from = 'Mean_Batpass_month'
  )

Hippo_re_mb_matrix <- data.frame(Hippo_re_mb_matrix)


###### II.B.2) make group for Boundou ------------

Hippo_re_bd <- Hippo_re |>
  filter(Sites == "Boundou") |>
  group_by(month2) |>
  summarise(Nb_batpass = n(), Nb_day_month = n_distinct(Date_formatted))  

Hippo_re_bd <- Hippo_re_bd  |>
mutate(Mean_Batpass_month = Nb_batpass/Nb_day_month) |>
arrange(month2)

bilan_Hippo_re_bd <- as.data.frame(Hippo_re_bd)
bilan_Hippo_re_bd$month2 <- as.factor(bilan_Hippo_re_bd$month2)
bilan_Hippo_re_bd$month2 <- gsub('2025', '', bilan_Hippo_re_bd$month2)
bilan_Hippo_re_bd$Mean_Batpass_month <- as.numeric(bilan_Hippo_re_bd$Mean_Batpass_month)

## remove columns that are not necessary

bilan_Hippo_re_bd2 <- bilan_Hippo_re_bd |>
  select(-Nb_batpass, -Nb_day_month)

Hippo_re_bd_matrix <- 
  bilan_Hippo_re_bd2 |> 
  pivot_wider(
    names_from = 'month2',
    values_from = 'Mean_Batpass_month'
  )

Hippo_re_bd_matrix <- data.frame(Hippo_re_bd_matrix)


###### II_C.1) Radar plot for MB------------

# Define Colors for Groups
group_colors <- c("Cave emergence (2h after sunset)" = "tomato2", "Cave enter (2h before sunrise)" = "darkorange")

# group matrix
Hippo_emerged_mb <- gtools::smartbind(Hippo_go_mb_matrix,Hippo_re_mb_matrix, fill=0)
group <- c("Cave emergence (2h after sunset)", "Cave enter (2h before sunrise)")
Hippo_emerged_mb <- data.frame(group = group,    # Append new column to front of data
                        Hippo_emerged_mb)

# plot
plot_emerg_Hippo_mb <- ggradar(Hippo_emerged_mb,
         axis.label.size = 6, 
        axis.label.offset = 1.1,  
        group.point.size = 2,
        group.line.width = 1,
        grid.label.size = 5,
        fill = F, 
        fill.alpha = 0.20,
        values.radar = c("0", "25", "55"), 
        grid.max = 55, 
        grid.mid = 25,
        # Background and grid lines
  background.circle.colour = "white",
  gridline.mid.colour = "grey",
  legend.position = "bottom",
group.colours = group_colors) # Assigning custom colors
plot_emerg_Hippo_mb


###### II_C.2) Radar plot  BD------------
group_colors <- c("Cave emergence (2h after sunset)" = "tomato2", "Cave enter (2h before sunrise)" = "darkorange")
Hippo_emerged_bd <- gtools::smartbind(Hippo_go_bd_matrix,Hippo_re_bd_matrix, fill=0)
group <- c("Cave emergence (2h after sunset)", "Cave enter (2h before sunrise)")
Hippo_emerged_bd <- data.frame(group = group,    # Append new column to front of data
                        Hippo_emerged_bd)
####
plot_emerg_Hippo_bd <- ggradar(Hippo_emerged_bd,
         axis.label.size = 6, 
        axis.label.offset = 1.1,  
        group.point.size = 2,
        group.line.width = 1,
        grid.label.size = 5,
        fill = F, 
        fill.alpha = 0.20,
        values.radar = c("0", "245", "490"), 
        grid.max = 490, 
        grid.mid = 245,
        # Background and grid lines
  background.circle.colour = "white",
  gridline.mid.colour = "grey",
  legend.position = "bottom",
  group.colours = group_colors)
plot_emerg_Hippo_bd

#####################
### III. Minio sp group
###########################

### III.A. Bat cave exit (2h after sunset)

Minio_go <- Minio_Allac  |>
  mutate(secAfterSunset3 = as.numeric(secAfterSunset2)) |>
  mutate(month2 = zoo::as.yearmon(as.character(month), "%m")) |>
    mutate(month3 = format(month2, "%b")) |>
  dplyr::filter(dplyr::between(secAfterSunset3, -120, 7200)) 

###### III.A.1) make group for Mont Belo -----------------------

Minio_go_mb <- Minio_go |>
  filter(Sites == "Mont Belo") |>
  group_by(month2) |>
  summarise(Nb_batpass = n(), Nb_day_month = n_distinct(Date_formatted))  

Minio_go_mb <- Minio_go_mb  |>
mutate(Mean_Batpass_month = Nb_batpass/Nb_day_month) |>
arrange(month2)

bilan_Minio_go_mb <- as.data.frame(Minio_go_mb)
bilan_Minio_go_mb$month2 <- as.factor(bilan_Minio_go_mb$month2)
bilan_Minio_go_mb$month2 <- gsub('2025', '', bilan_Minio_go_mb$month2)
bilan_Minio_go_mb$Mean_Batpass_month <- as.numeric(bilan_Minio_go_mb$Mean_Batpass_month)

## remove columns that are not necessary

bilan_Minio_go_mb2 <- bilan_Minio_go_mb |>
  select(-Nb_batpass, -Nb_day_month)

## transform
Minio_go_mb_matrix <- 
  bilan_Minio_go_mb2 |> 
  pivot_wider(
    names_from = 'month2',
    values_from = 'Mean_Batpass_month'
  )

Minio_go_mb_matrix <- data.frame(Minio_go_mb_matrix)

###### III.A.2) make group for Boundou ------------

Minio_go_bd <- Minio_go |>
  filter(Sites == "Boundou") |>
  group_by(month2) |>
  summarise(Nb_batpass = n(), Nb_day_month = n_distinct(Date_formatted))  

Minio_go_bd <- Minio_go_bd  |>
mutate(Mean_Batpass_month = Nb_batpass/Nb_day_month) |>
arrange(month2)

bilan_Minio_go_bd <- as.data.frame(Minio_go_bd)
bilan_Minio_go_bd$month2 <- as.factor(bilan_Minio_go_bd$month2)
bilan_Minio_go_bd$month2 <- gsub('2025', '', bilan_Minio_go_bd$month2)
bilan_Minio_go_bd$Mean_Batpass_month <- as.numeric(bilan_Minio_go_bd$Mean_Batpass_month)

## remove columns that are not necessary

bilan_Minio_go_bd2 <- bilan_Minio_go_bd |>
  select(-Nb_batpass, -Nb_day_month)

## transform
Minio_go_bd_matrix <- 
  bilan_Minio_go_bd2 |> 
  pivot_wider(
    names_from = 'month2',
    values_from = 'Mean_Batpass_month'
  )

Minio_go_bd_matrix <- data.frame(Minio_go_bd_matrix)

# III.B. Bar cave re-enter (end of the night - before sunrise)

Minio_re <- Minio_Allac  %>%
   mutate(month2 = zoo::as.yearmon(as.character(month), "%m")) |>
    mutate(month3 = format(month2, "%b")) |>
  dplyr::filter(dplyr::between(time_24, "04:00", "08:00"))

###### III.B.1) make group for Mont Belo -----------------------

Minio_re_mb <- Minio_re |>
  filter(Sites == "Mont Belo") |>
  group_by(month2) |>
  summarise(Nb_batpass = n(), Nb_day_month = n_distinct(Date_formatted)) 

Minio_re_mb <- Minio_re_mb  |>
mutate(Mean_Batpass_month = Nb_batpass/Nb_day_month) |>
arrange(month2)

bilan_Minio_re_mb <- as.data.frame(Minio_re_mb)
bilan_Minio_re_mb$month2 <- as.factor(bilan_Minio_re_mb$month2)
bilan_Minio_re_mb$month2 <- gsub('2025', '', bilan_Minio_re_mb$month2)
bilan_Minio_re_mb$Mean_Batpass_month <- as.numeric(bilan_Minio_re_mb$Mean_Batpass_month)

## remove columns that are not necessary

bilan_Minio_re_mb2 <- bilan_Minio_re_mb |>
  select(-Nb_batpass, -Nb_day_month)

Minio_re_mb_matrix <- 
  bilan_Minio_re_mb2 |> 
  pivot_wider(
    names_from = 'month2',
    values_from = 'Mean_Batpass_month'
  )

Minio_re_mb_matrix <- data.frame(Minio_re_mb_matrix)


###### III.B.2) make group for Boundou ------------

Minio_re_bd <- Minio_re |>
  filter(Sites == "Boundou") |>
  group_by(month2) |>
  summarise(Nb_batpass = n(), Nb_day_month = n_distinct(Date_formatted))  

Minio_re_bd <- Minio_re_bd  |>
mutate(Mean_Batpass_month = Nb_batpass/Nb_day_month) |>
arrange(month2)

bilan_Minio_re_bd <- as.data.frame(Minio_re_bd)
bilan_Minio_re_bd$month2 <- as.factor(bilan_Minio_re_bd$month2)
bilan_Minio_re_bd$month2 <- gsub('2025', '', bilan_Minio_re_bd$month2)
bilan_Minio_re_bd$Mean_Batpass_month <- as.numeric(bilan_Minio_re_bd$Mean_Batpass_month)

## remove columns that are not necessary

bilan_Minio_re_bd2 <- bilan_Minio_re_bd |>
  select(-Nb_batpass, -Nb_day_month)

Minio_re_bd_matrix <- 
  bilan_Minio_re_bd2 |> 
  pivot_wider(
    names_from = 'month2',
    values_from = 'Mean_Batpass_month'
  )

Minio_re_bd_matrix <- data.frame(Minio_re_bd_matrix)

###### III.C.1) Radar plot MB------------

# group matrix
group_colors <- c("Cave emergence (2h after sunset)" = "tomato2", "Cave enter (2h before sunrise)" = "darkorange")
Minio_emerged_mb <- gtools::smartbind(Minio_go_mb_matrix,Minio_re_mb_matrix, fill=0)
group <- c("Cave emergence (2h after sunset)", "Cave enter (2h before sunrise)")
Minio_emerged_mb <- data.frame(group = group,    # Append new column to front of data
                        Minio_emerged_mb)


# plot
plot_emerg_Minio_mb <- ggradar(Minio_emerged_mb,
         axis.label.size = 6, 
        axis.label.offset = 1.1,  
        group.point.size = 2,
        group.line.width = 1,
        grid.label.size = 5,
        fill = F, 
        fill.alpha = 0.20,
        values.radar = c("0", "425", "850"), 
        grid.max = 850, 
        grid.mid = 425,
        # Background and grid lines
  background.circle.colour = "white",
  gridline.mid.colour = "grey",
  legend.position = "bottom",
group.colours = group_colors)
plot_emerg_Minio_mb

###### III.C.2) Radar plot  BD------------
group_colors <- c("Cave emergence (2h after sunset)" = "tomato2", "Cave enter (2h before sunrise)" = "darkorange")
Minio_emerged_bd <- gtools::smartbind(Minio_go_bd_matrix,Minio_re_bd_matrix, fill=0)
group <- c("Cave emergence (2h after sunset)", "Cave enter (2h before sunrise)")
Minio_emerged_bd <- data.frame(group = group,    # Append new column to front of data
                        Minio_emerged_bd)

# plot
plot_emerg_Minio_bd <- ggradar(Minio_emerged_bd,
         axis.label.size = 6, 
        axis.label.offset = 1.1, 
        group.point.size = 2,
        group.line.width = 1,
        grid.label.size = 5,
        fill = F, 
        fill.alpha = 0.20,
        values.radar = c("0", "425", "850"), 
        grid.max = 850, 
        grid.mid = 425,
        # Background and grid lines
  background.circle.colour = "white",
  gridline.mid.colour = "grey",
  legend.position = "none",
group.colours = group_colors)
plot_emerg_Minio_bd

#################################################
#### IV. Rhinolophus landeri alcyone group
#################################################

### Bat cave exit (2h after sunset)

Rum_lan_alcy_go <- Rum_lan_alcy_Allac  |>
  mutate(secAfterSunset3 = as.numeric(secAfterSunset2)) |>
  mutate(month2 = zoo::as.yearmon(as.character(month), "%m")) |>
    mutate(month3 = format(month2, "%b")) |>
  dplyr::filter(dplyr::between(secAfterSunset3, -120, 7200)) 

###### IV.A.1) make group for Mont Belo -----------------------

Rum_lan_alcy_go_mb <- Rum_lan_alcy_go |>
  filter(Sites == "Mont Belo") |>
  group_by(month2) |>
  summarise(Nb_batpass = n(), Nb_day_month = n_distinct(Date_formatted))  

Rum_lan_alcy_mb <- Rum_lan_alcy_go_mb  |>
mutate(Mean_Batpass_month = Nb_batpass/Nb_day_month) |>
arrange(month2)

bilan_Rum_lan_alcy_mb <- as.data.frame(Rum_lan_alcy_mb)
bilan_Rum_lan_alcy_mb$month2 <- as.factor(bilan_Rum_lan_alcy_mb$month2)
bilan_Rum_lan_alcy_mb$month2 <- gsub('2025', '', bilan_Rum_lan_alcy_mb$month2)
bilan_Rum_lan_alcy_mb$Mean_Batpass_month <- as.numeric(bilan_Rum_lan_alcy_mb$Mean_Batpass_month)

## remove columns that are not necessary

bilan_Rum_lan_alcy_mb2 <- bilan_Rum_lan_alcy_mb |>
  select(-Nb_batpass, -Nb_day_month)

## transform
Rum_lan_alcy_mb_matrix <- 
  bilan_Rum_lan_alcy_mb2 |> 
  pivot_wider(
    names_from = 'month2',
    values_from = 'Mean_Batpass_month'
  )

Rum_lan_alcy_mb_matrix <- data.frame(Rum_lan_alcy_mb_matrix)

###### IV.A.2) make group for Boundou ------------

Rum_lan_alcy_bd <- Rum_lan_alcy_go |>
  filter(Sites == "Mont Belo") |>
  group_by(month2) |>
  summarise(Nb_batpass = n(), Nb_day_month = n_distinct(Date_formatted))  

Rum_lan_alcy_bd <- Rum_lan_alcy_bd  |>
mutate(Mean_Batpass_month = Nb_batpass/Nb_day_month) |>
arrange(month2)

bilan_Rum_lan_alcy_bd <- as.data.frame(Rum_lan_alcy_bd)
bilan_Rum_lan_alcy_bd$month2 <- as.factor(bilan_Rum_lan_alcy_bd$month2)
bilan_Rum_lan_alcy_bd$month2 <- gsub('2025', '', bilan_Rum_lan_alcy_bd$month2)
bilan_Rum_lan_alcy_bd$Mean_Batpass_month <- as.numeric(bilan_Rum_lan_alcy_bd$Mean_Batpass_month)

## remove columns that are not necessary

bilan_Rum_lan_alcy_bd2 <- bilan_Rum_lan_alcy_bd |>
  select(-Nb_batpass, -Nb_day_month)

## transform
Rum_lan_alcy_bd_matrix <- 
  bilan_Rum_lan_alcy_bd2 |> 
  pivot_wider(
    names_from = 'month2',
    values_from = 'Mean_Batpass_month'
  )

Rum_lan_alcy_bd_matrix <- data.frame(Rum_lan_alcy_bd_matrix)
  

# IV.B. Bar cave re-enter (end of the night - before sunrise)

Rum_lan_alcy_re <- Rum_lan_alcy_Allac  %>%
   mutate(month2 = zoo::as.yearmon(as.character(month), "%m")) |>
    mutate(month3 = format(month2, "%b")) |>
  dplyr::filter(dplyr::between(time_24, "04:00", "08:00"))


###### IV.B.1) make group for Mont Belo -----------------------

Rum_lan_alcy_re_mb <- Rum_lan_alcy_re |>
  filter(Sites == "Mont Belo") |>
  group_by(month2) |>
  summarise(Nb_batpass = n(), Nb_day_month = n_distinct(Date_formatted)) 

Rum_lan_alcy_re_mb <- Rum_lan_alcy_re_mb  |>
mutate(Mean_Batpass_month = Nb_batpass/Nb_day_month) |>
arrange(month2)

bilan_Rum_lan_alcy_re_mb <- as.data.frame(Rum_lan_alcy_re_mb)
bilan_Rum_lan_alcy_re_mb$month2 <- as.factor(bilan_Rum_lan_alcy_re_mb$month2)
bilan_Rum_lan_alcy_re_mb$month2 <- gsub('2025', '', bilan_Rum_lan_alcy_re_mb$month2)
bilan_Rum_lan_alcy_re_mb$Mean_Batpass_month <- as.numeric(bilan_Rum_lan_alcy_re_mb$Mean_Batpass_month)

## remove columns that are not necessary

bilan_Rum_lan_alcy_re_mb2 <- bilan_Rum_lan_alcy_re_mb |>
  select(-Nb_batpass, -Nb_day_month)

Rum_lan_alcy_re_mb_matrix <- 
  bilan_Rum_lan_alcy_re_mb2 |> 
  pivot_wider(
    names_from = 'month2',
    values_from = 'Mean_Batpass_month'
  )

Rum_lan_alcy_re_mb_matrix <- data.frame(Rum_lan_alcy_re_mb_matrix)


###### IV.B.2) make group for Boundou ------------

Rum_lan_alcy_re_bd <- Rum_lan_alcy_re |>
  filter(Sites == "Mont Belo") |>
  group_by(month2) |>
  summarise(Nb_batpass = n(), Nb_day_month = n_distinct(Date_formatted))  

Rum_lan_alcy_re_bd <- Rum_lan_alcy_re_bd  |>
mutate(Mean_Batpass_month = Nb_batpass/Nb_day_month) |>
arrange(month2)

bilan_Rum_lan_alcy_re_bd <- as.data.frame(Rum_lan_alcy_re_bd)
bilan_Rum_lan_alcy_re_bd$month2 <- as.factor(bilan_Rum_lan_alcy_re_bd$month2)
bilan_Rum_lan_alcy_re_bd$month2 <- gsub('2025', '', bilan_Rum_lan_alcy_re_bd$month2)
bilan_Rum_lan_alcy_re_bd$Mean_Batpass_month <- as.numeric(bilan_Rum_lan_alcy_re_bd$Mean_Batpass_month)

## remove columns that are not necessary

bilan_Rum_lan_alcy_re_bd2 <- bilan_Rum_lan_alcy_re_bd |>
  select(-Nb_batpass, -Nb_day_month)

Rum_lan_alcy_re_bd_matrix <- 
  bilan_Rum_lan_alcy_re_bd2 |> 
  pivot_wider(
    names_from = 'month2',
    values_from = 'Mean_Batpass_month'
  )

Rum_lan_alcy_re_bd_matrix <- data.frame(Rum_lan_alcy_re_bd_matrix)

###### IV.C.1) Radar plot  MB------------


# group matrix
group_colors <- c("Cave emergence (2h after sunset)" = "tomato2", "Cave enter (2h before sunrise)" = "darkorange")
Rum_lan_alcy_emerged_mb <- gtools::smartbind(Rum_lan_alcy_mb_matrix,Rum_lan_alcy_re_mb_matrix, fill=0)
group <- c("Cave emergence (2h after sunset)", "Cave enter (2h before sunrise)")
Rum_lan_alcy_emerged_mb <- data.frame(group = group,    # Append new column to front of data
                        Rum_lan_alcy_emerged_mb)

# plot
plot_emerg_Rum_lan_alcy_mb <- ggradar(Rum_lan_alcy_emerged_mb,
         axis.label.size = 6, 
        axis.label.offset = 1.1,  
        group.point.size = 2,
        group.line.width = 1,
        grid.label.size = 5,
        fill = F, 
        fill.alpha = 0.20,
        values.radar = c("0", "420", "847"), 
        grid.max = 847, 
        grid.mid = 420,
        # Background and grid lines
  background.circle.colour = "white",
  gridline.mid.colour = "grey",
  legend.position = "none",
group.colours = group_colors)
plot_emerg_Rum_lan_alcy_mb


#############################
################## V. Rhinolophus denti group ######################
############################################


### V.A. Bat cave exit (2h after sunset)

Rhino_denti_go <- Rhino_denti_Allac  |>
  mutate(secAfterSunset3 = as.numeric(secAfterSunset2)) |>
  mutate(month2 = zoo::as.yearmon(as.character(month), "%m")) |>
    mutate(month3 = format(month2, "%b")) |>
  dplyr::filter(dplyr::between(secAfterSunset3, -120, 7200)) 

###### V.A.1) make group for Mont Belo -----------------------

Rhino_denti_go_mb <- Rhino_denti_go |>
  filter(Sites == "Mont Belo") |>
  group_by(month2) |>
  summarise(Nb_batpass = n(), Nb_day_month = n_distinct(Date_formatted))  

Rhino_denti_mb <- Rhino_denti_go_mb  |>
mutate(Mean_Batpass_month = Nb_batpass/Nb_day_month) |>
arrange(month2)

bilan_Rhino_denti_mb <- as.data.frame(Rhino_denti_mb)
bilan_Rhino_denti_mb$month2 <- as.factor(bilan_Rhino_denti_mb$month2)
bilan_Rhino_denti_mb$month2 <- gsub('2025', '', bilan_Rhino_denti_mb$month2)
bilan_Rhino_denti_mb$Mean_Batpass_month <- as.numeric(bilan_Rhino_denti_mb$Mean_Batpass_month)

## remove columns that are not necessary

bilan_Rhino_denti_mb2 <- bilan_Rhino_denti_mb |>
  select(-Nb_batpass, -Nb_day_month)

## transform
Rhino_denti_mb_matrix <- 
  bilan_Rhino_denti_mb2 |> 
  pivot_wider(
    names_from = 'month2',
    values_from = 'Mean_Batpass_month'
  )

Rhino_denti_mb_matrix <- data.frame(Rhino_denti_mb_matrix)

###### V.A.2) make group for Boundou ------------

Rhino_denti_bd <- Rhino_denti_go |>
  filter(Sites == "Boundou") |>
  group_by(month2) |>
  summarise(Nb_batpass = n(), Nb_day_month = n_distinct(Date_formatted))  

Rhino_denti_bd <- Rhino_denti_bd  |>
mutate(Mean_Batpass_month = Nb_batpass/Nb_day_month) |>
arrange(month2)

bilan_Rhino_denti_bd <- as.data.frame(Rhino_denti_bd)
bilan_Rhino_denti_bd$month2 <- as.factor(bilan_Rhino_denti_bd$month2)
bilan_Rhino_denti_bd$month2 <- gsub('2025', '', bilan_Rhino_denti_bd$month2)
bilan_Rhino_denti_bd$Mean_Batpass_month <- as.numeric(bilan_Rhino_denti_bd$Mean_Batpass_month)

## remove columns that are not necessary

bilan_Rhino_denti_bd2 <- bilan_Rhino_denti_bd |>
  select(-Nb_batpass, -Nb_day_month)

## transform
Rhino_denti_bd_matrix <- 
  bilan_Rhino_denti_bd2 |> 
  pivot_wider(
    names_from = 'month2',
    values_from = 'Mean_Batpass_month'
  )

Rhino_denti_bd_matrix <- data.frame(Rhino_denti_bd_matrix)


# V.B. Bar cave re-enter (end of the night - before sunrise)

Rhino_denti_re <- Rhino_denti_Allac  %>%
   mutate(month2 = zoo::as.yearmon(as.character(month), "%m")) |>
    mutate(month3 = format(month2, "%b")) |>
  dplyr::filter(dplyr::between(time_24, "04:00", "08:00"))


###### V.B.1) make group for Mont Belo -----------------------

Rhino_denti_re_mb <- Rhino_denti_re |>
  filter(Sites == "Mont Belo") |>
  group_by(month2) |>
  summarise(Nb_batpass = n(), Nb_day_month = n_distinct(Date_formatted)) 

Rhino_denti_re_mb <- Rhino_denti_re_mb  |>
mutate(Mean_Batpass_month = Nb_batpass/Nb_day_month) |>
arrange(month2)

bilan_Rhino_denti_re_mb <- as.data.frame(Rhino_denti_re_mb)
bilan_Rhino_denti_re_mb$month2 <- as.factor(bilan_Rhino_denti_re_mb$month2)
bilan_Rhino_denti_re_mb$month2 <- gsub('2025', '', bilan_Rhino_denti_re_mb$month2)
bilan_Rhino_denti_re_mb$Mean_Batpass_month <- as.numeric(bilan_Rhino_denti_re_mb$Mean_Batpass_month)

## remove columns that are not necessary

bilan_Rhino_denti_re_mb2 <- bilan_Rhino_denti_re_mb |>
  select(-Nb_batpass, -Nb_day_month)

Rhino_denti_re_mb_matrix <- 
  bilan_Rhino_denti_re_mb2 |> 
  pivot_wider(
    names_from = 'month2',
    values_from = 'Mean_Batpass_month'
  )

Rhino_denti_re_mb_matrix <- data.frame(Rhino_denti_re_mb_matrix)


###### V.B.2) make group for Boundou ------------

Rhino_denti_re_bd <- Rhino_denti_re |>
  filter(Sites == "Boundou") |>
  group_by(month2) |>
  summarise(Nb_batpass = n(), Nb_day_month = n_distinct(Date_formatted))  

Rhino_denti_re_bd <- Rhino_denti_re_bd  |>
mutate(Mean_Batpass_month = Nb_batpass/Nb_day_month) |>
arrange(month2)

bilan_Rhino_denti_re_bd <- as.data.frame(Rhino_denti_re_bd)
bilan_Rhino_denti_re_bd$month2 <- as.factor(bilan_Rhino_denti_re_bd$month2)
bilan_Rhino_denti_re_bd$month2 <- gsub('2025', '', bilan_Rhino_denti_re_bd$month2)
bilan_Rhino_denti_re_bd$Mean_Batpass_month <- as.numeric(bilan_Rhino_denti_re_bd$Mean_Batpass_month)

## remove columns that are not necessary

bilan_Rhino_denti_re_bd2 <- bilan_Rhino_denti_re_bd |>
  select(-Nb_batpass, -Nb_day_month)

Rhino_denti_re_bd_matrix <- 
  bilan_Rhino_denti_re_bd2 |> 
  pivot_wider(
    names_from = 'month2',
    values_from = 'Mean_Batpass_month'
  )

Rhino_denti_re_bd_matrix <- data.frame(Rhino_denti_re_bd_matrix)


###### V.C.1) Radar plot MB------------

# group matrix
group_colors <- c("Cave emergence (2h after sunset)" = "tomato2", "Cave enter (2h before sunrise)" = "darkorange")
Rhino_denti_emerged_mb <- gtools::smartbind(Rhino_denti_mb_matrix,Rhino_denti_re_mb_matrix, fill=0)
group <- c("Cave emergence (2h after sunset)", "Cave enter (2h before sunrise)")
Rhino_denti_emerged_mb <- data.frame(group = group,    # Append new column to front of data
                        Rhino_denti_emerged_mb)

# plot
plot_emerg_Rhino_denti_mb <- ggradar(Rhino_denti_emerged_mb,
         axis.label.size = 6, 
        axis.label.offset = 1.1, 
        group.point.size = 2,
        group.line.width = 1,
        grid.label.size = 5,
        fill = F, 
        fill.alpha = 0.20,
        values.radar = c("0", "1.25", "2.5"), 
        grid.max = 2.5, 
        grid.mid = 1.25,
        # Background and grid lines
  background.circle.colour = "white",
  gridline.mid.colour = "grey",
  legend.position = "none",
group.colours = group_colors)
plot_emerg_Rhino_denti_mb 

###### V.C.2) Radar plot BD------------
group_colors <- c("Cave emergence (2h after sunset)" = "tomato2", "Cave enter (2h before sunrise)" = "darkorange")
Rhino_denti_emerged_bd <- gtools::smartbind(Rhino_denti_bd_matrix,Rhino_denti_re_bd_matrix, fill=0)
group <- c("Cave emergence (2h after sunset)", "Cave enter (2h before sunrise)")
Rhino_denti_emerged_bd <- data.frame(group = group,    # Append new column to front of data
                       Rhino_denti_emerged_bd)


# plot
plot_emerg_Rhino_denti_bd <- ggradar(Rhino_denti_emerged_bd,
         axis.label.size = 6, 
        axis.label.offset = 1.1, 
        group.point.size = 2,
        group.line.width = 1,
        grid.label.size = 5,
        fill = F, 
        fill.alpha = 0.20,
        values.radar = c("0", "44", "89"), 
        grid.max = 89, 
        grid.mid = 44,
        # Background and grid lines
  background.circle.colour = "white",
  gridline.mid.colour = "grey",
  legend.position = "none",
group.colours = group_colors)


#######################################
###############  VI. Rhino Tria Gigas group (only at Boundou cave)
#############################################


### VI.A. Bat cave exit (2h after sunset)

Rmt_go <- Rhino_macro_tria_all  |>
  mutate(secAfterSunset3 = as.numeric(secAfterSunset2)) |>
  mutate(month2 = zoo::as.yearmon(as.character(month), "%m")) |>
  mutate(month3 = format(month2, "%b")) |>
  dplyr::filter(dplyr::between(secAfterSunset3, -120, 7200)) 



###### VI.A.1) make group for Boundou ------------

Rmt_bd <- Rmt_go |>
  filter(Sites == "Boundou") |>
  group_by(month2) |>
  summarise(Nb_batpass = n(), Nb_day_month = n_distinct(Date_formatted))  

Rmt_bd <- Rmt_bd  |>
mutate(Mean_Batpass_month = Nb_batpass/Nb_day_month) |>
arrange(month2)

bilan_Rmt_bd <- as.data.frame(Rmt_bd)
bilan_Rmt_bd$month2 <- as.factor(bilan_Rmt_bd$month2)
bilan_Rmt_bd$month2 <- gsub('2025', '', bilan_Rmt_bd$month2)
bilan_Rmt_bd$Mean_Batpass_month <- as.numeric(bilan_Rmt_bd$Mean_Batpass_month)

## remove columns that are not necessary

bilan_Rmt_bd2 <- bilan_Rmt_bd |>
  select(-Nb_batpass, -Nb_day_month)

## transform
Rmt_bd_matrix <- 
  bilan_Rmt_bd2 |> 
  pivot_wider(
    names_from = 'month2',
    values_from = 'Mean_Batpass_month'
  )

Rmt_bd_matrix <- data.frame(Rmt_bd_matrix)


# VI.B. Bar cave re-enter (end of the night - before sunrise)

Rmt_re <- Rhino_macro_tria_all  %>%
  #mutate(secAfterSunrise2 = as.numeric(secAfterSunrise)) |>
   mutate(month2 = zoo::as.yearmon(as.character(month), "%m")) |>
    mutate(month3 = format(month2, "%b")) |>
  dplyr::filter(dplyr::between(time_24, "04:00", "08:00"))


###### VI.B.1) make group for Boundou ------------

Rmt_re_bd <- Rmt_re |>
  filter(Sites == "Boundou") |>
  group_by(month2) |>
  summarise(Nb_batpass = n(), Nb_day_month = n_distinct(Date_formatted))  

Rmt_re_bd <- Rmt_re_bd  |>
mutate(Mean_Batpass_month = Nb_batpass/Nb_day_month) |>
arrange(month2)

bilan_Rmt_re_bd <- as.data.frame(Rmt_re_bd)
bilan_Rmt_re_bd$month2 <- as.factor(bilan_Rmt_re_bd$month2)
bilan_Rmt_re_bd$month2 <- gsub('2025', '', bilan_Rmt_re_bd$month2)
bilan_Rmt_re_bd$Mean_Batpass_month <- as.numeric(bilan_Rmt_re_bd$Mean_Batpass_month)

## remove columns that are not necessary

bilan_Rmt_re_bd2 <- bilan_Rmt_re_bd |>
  select(-Nb_batpass, -Nb_day_month)

Rmt_re_bd_matrix <- 
  bilan_Rmt_re_bd2 |> 
  pivot_wider(
    names_from = 'month2',
    values_from = 'Mean_Batpass_month'
  )

Rmt_re_bd_matrix <- data.frame(Rmt_re_bd_matrix)



###### VI.C. Radar plot BD------------
group_colors <- c("Cave emergence (2h after sunset)" = "tomato2", "Cave enter (2h before sunrise)" = "darkorange")
Rmt_emerged_bd <- gtools::smartbind(Rmt_bd_matrix,Rmt_re_bd_matrix, fill=0)
group <- c("Cave emergence (2h after sunset)", "Cave enter (2h before sunrise)")
Rmt_emerged_bd <- data.frame(group = group,    # Append new column to front of data
                        Rmt_emerged_bd)

# plot
plot_emerg_Rmt_bd <- ggradar(Rmt_emerged_bd,
        axis.label.size = 6, 
        axis.label.offset = 1.1, 
        group.point.size = 2,
        group.line.width = 1,
        grid.label.size = 5,
        fill = F, 
        fill.alpha = 0.20,
        values.radar = c("0", "427", "855"), 
        grid.max = 855, 
        grid.mid = 427,
        # Background and grid lines
  background.circle.colour = "white",
  gridline.mid.colour = "grey",
  legend.position = "none",
group.colours = group_colors)


##############################
############  VII. Plot all graph together
####################################################################

library(patchwork)
batplot <- plot_emerg_Hippo_bd + plot_emerg_Hippo_mb + 
  plot_emerg_Minio_bd + plot_emerg_Minio_mb +
  plot_emerg_Rhino_denti_bd +  plot_emerg_Rhino_denti_mb +
  plot_emerg_Rum_lan_alcy_mb +  plot_emerg_Rmt_bd +
  plot_layout(nrow= 4, guides = "collect") &
  theme(legend.position='bottom')

batplot

