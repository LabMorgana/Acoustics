#### MODELE ACTIVITY ####

pacman::p_load(tidyverse, FactoMineR, factoextra, ggplot2, openxlsx, lubridate, ggradar, readxl, tidyr, stringr, glmmTMB, DHARMa, ggeffects)

### Load files

AllBat_day_nb_batpass_allsites <- read.csv(file ="~/Downloads/AllBat_day_nb_batpass_allsites.csv")

AllBat_day_nb_batpass_allsites$Year <- format(as.POSIXct(AllBat_day_nb_batpass_allsites$Date_formatted, "%Y-%m-%d", tz = ""), format = "%Y")

## add a columns month in number 

AllBat_day_nb_batpass_allsites$Month_nb <- format(as.Date(AllBat_day_nb_batpass_allsites$Date_formatted, format="%Y-%m-%d"),"%m")
AllBat_day_nb_batpass_allsites$Month_nb <- as.factor(AllBat_day_nb_batpass_allsites$Month_nb)

## Species

table(AllBat_day_nb_batpass_allsites$Group_sp)

### Select species and time

### select cave emergence ------

Ac_Hippo_exit_mb <- AllBat_day_nb_batpass_allsites |>
  filter(Time == "cave exit (2h after sunset)") |>
  filter(Sites== "Mont Belo") |>
  filter(Group_sp == "Hippo sp") |>
  dplyr::arrange(Date_formatted)

Ac_Hippo_exit_bd <- AllBat_day_nb_batpass_allsites |>
  filter(Time == "cave exit (2h after sunset)") |>
  filter(Group_sp == "Hippo sp") |>
  filter(Sites == "Boundou") |>
  dplyr::arrange(Date_formatted)

Ac_Minio_exit_mb <- AllBat_day_nb_batpass_allsites |>
  filter(Time == "cave exit (2h after sunset)") |>
  filter(Group_sp == "Minio sp") |>
  filter(Sites== "Mont Belo") |>
  arrange(Date_formatted)

Ac_Minio_exit_bd <- AllBat_day_nb_batpass_allsites |>
  filter(Time == "cave exit (2h after sunset)") |>
  filter(Group_sp == "Minio sp") |>
  filter(Sites == "Boundou") |>
  arrange(Date_formatted)

Ac_Rhino_la_exit_mb <- AllBat_day_nb_batpass_allsites |>
  filter(Time == "cave exit (2h after sunset)") |>
  filter(Group_sp == "Rhino fum") |>
  filter(Sites== "Mont Belo") |>
  arrange(Date_formatted)


Ac_Rhino_de_exit_mb <- AllBat_day_nb_batpass_allsites |>
  filter(Time == "cave exit (2h after sunset)") |>
  filter(Group_sp == "Rhino ld") |>
  filter(Sites== "Mont Belo") |>
  arrange(Date_formatted)

Ac_Rhino_de_exit_bd <- AllBat_day_nb_batpass_allsites |>
  filter(Time == "cave exit (2h after sunset)") |>
  filter(Group_sp == "Rhino ld") |>
  filter(Sites == "Boundou") |>
  arrange(Date_formatted)

Ac_Rhino_macro_tria_exit <- AllBat_day_nb_batpass_allsites |>
  filter(Time == "cave exit (2h after sunset)") |>
  filter(Group_sp == "Rhino macro tria") |>
  filter(Sites == "Boundou") |>
  arrange(Date_formatted)


## transform character in factor ------
Ac_Hippo_exit_mb <- mutate_if(Ac_Hippo_exit_mb, is.character, as.factor)
Ac_Hippo_exit_bd <- mutate_if(Ac_Hippo_exit_bd, is.character, as.factor)
Ac_Minio_exit_mb <- mutate_if(Ac_Minio_exit_mb, is.character, as.factor)
Ac_Minio_exit_bd <- mutate_if(Ac_Minio_exit_bd, is.character, as.factor)
Ac_Rhino_la_exit_mb <- mutate_if(Ac_Rhino_la_exit_mb, is.character, as.factor)
Ac_Rhino_de_exit_mb <- mutate_if(Ac_Rhino_de_exit_mb, is.character, as.factor)
Ac_Rhino_de_exit_bd <- mutate_if(Ac_Rhino_de_exit_bd, is.character, as.factor)
Ac_Rhino_macro_tria_exit <- mutate_if(Ac_Rhino_macro_tria_exit, is.character, as.factor)


## look at distribution -------

library(fitdistrplus)
descdist(Ac_Hippo_exit_bd$Nb_batpass, boot=1000)
descdist(Ac_Hippo_exit_mb$Nb_batpass, boot=1000)
descdist(Ac_Minio_exit_bd$Nb_batpass, boot=1000)
descdist(Ac_Minio_exit_mb$Nb_batpass, boot=1000)
descdist(Ac_Rhino_la_exit_mb$Nb_batpass, boot=1000)
descdist(Ac_Rhino_de_exit_bd$Nb_batpass, boot=1000)
descdist(Ac_Rhino_de_exit_mb$Nb_batpass, boot=1000)
descdist(Ac_Rhino_macro_tria_exit$Nb_batpass, boot=1000)


plotdist(Ac_Hippo_exit_bd$Nb_batpass, histo = TRUE, demp = TRUE)
plotdist(Ac_Hippo_exit_mb$Nb_batpass, histo = TRUE, demp = TRUE)
plotdist(Ac_Minio_exit_bd$Nb_batpass, histo = TRUE, demp = TRUE)
plotdist(Ac_Minio_exit_mb$Nb_batpass, histo = TRUE, demp = TRUE)
plotdist(Ac_Rhino_la_exit_mb$Nb_batpass, histo = TRUE, demp = TRUE)
plotdist(Ac_Rhino_de_exit_bd$Nb_batpass, histo = TRUE, demp = TRUE)
plotdist(Ac_Rhino_de_exit_mb$Nb_batpass, histo = TRUE, demp = TRUE)
plotdist(Ac_Rhino_macro_tria_exit$Nb_batpass, histo = TRUE, demp = TRUE)


##### Modele day  ~ month and month_year with sites together ------

## Hippo exit mb
model_Ac_Hippo_exit_mb <- glmmTMB::glmmTMB(as.numeric(Nb_batpass) ~ Month_nb  + (1|month2), data = Ac_Hippo_exit_mb, family = lognormal)
## model checking and diagnostics
simulationOutput_Ac_Hippo_exit_mb <- simulateResiduals(fittedModel = model_Ac_Hippo_exit_mb)
plot(simulationOutput_Ac_Hippo_exit_mb)
testDispersion(simulationOutput_Ac_Hippo_exit_mb)
testZeroInflation(simulationOutput_Ac_Hippo_exit_mb)
## summary
summary(model_Ac_Hippo_exit_mb)

## Hippo exit bd
model_Ac_Hippo_exit_bd <- glmmTMB::glmmTMB(as.numeric(Nb_batpass) ~ Month_nb  + (1|month2), data = Ac_Hippo_exit_bd, family = lognormal)
## model checking and diagnostics
simulationOutput_Ac_Hippo_exit_bd <- simulateResiduals(fittedModel = model_Ac_Hippo_exit_bd)
plot(simulationOutput_Ac_Hippo_exit_bd)
testDispersion(simulationOutput_Ac_Hippo_exit_bd)
testZeroInflation(simulationOutput_Ac_Hippo_exit_bd)
## summary
summary(model_Ac_Hippo_exit_bd)

#### Miniopterus exit mb
model_Ac_Minio_exit_mb <- glmmTMB::glmmTMB(as.numeric(Nb_batpass) ~ Month_nb + (1|month2), data = Ac_Minio_exit_mb, family = lognormal)
## model checking and diagnostics
simulationOutput_Ac_Minio_exit_mb <- simulateResiduals(fittedModel = model_Ac_Minio_exit_mb)
plot(simulationOutput_Ac_Minio_exit_mb)
testDispersion(simulationOutput_Ac_Minio_exit_mb)
testZeroInflation(simulationOutput_Ac_Minio_exit_mb)
## summary
summary(model_Ac_Minio_exit_mb)


#### Miniopterus exit bd
model_Ac_Minio_exit_bd <- glmmTMB::glmmTMB(as.numeric(Nb_batpass) ~ Month_nb + (1|month2), data = Ac_Minio_exit_bd, family = lognormal)
## model checking and diagnostics
simulationOutput_Ac_Minio_exit_bd <- simulateResiduals(fittedModel = model_Ac_Minio_exit_bd)
plot(simulationOutput_Ac_Minio_exit_bd)
testDispersion(simulationOutput_Ac_Minio_exit_bd)
testZeroInflation(simulationOutput_Ac_Minio_exit_bd)
## summary
summary(model_Ac_Minio_exit_bd)


#### Rhinolophus alcyone landeri exit mb
model_Ac_Rhino_la_exit_mb <- glmmTMB::glmmTMB(as.numeric(Nb_batpass) ~ Month_nb + (1|month2), data = Ac_Rhino_la_exit_mb, family = glmmTMB::lognormal)
## model checking and diagnostics
simulationOutput_Ac_Rhino_la_exit_mb <- simulateResiduals(fittedModel = model_Ac_Rhino_la_exit_mb)
plot(simulationOutput_Ac_Rhino_la_exit_mb)
testDispersion(simulationOutput_Ac_Rhino_la_exit_mb)
testZeroInflation(simulationOutput_Ac_Rhino_la_exit_mb)
## summary
summary(model_Ac_Rhino_la_exit_mb)


#### Rhinolophus denti exit mb
model_Ac_Rhino_de_exit_mb <- glmmTMB::glmmTMB(as.numeric(Nb_batpass) ~ Month_nb  + (1|month2), data = Ac_Rhino_de_exit_mb, family = glmmTMB::lognormal)
## model checking and diagnostics
simulationOutput_Ac_Rhino_de_exit_mb <- simulateResiduals(fittedModel = model_Ac_Rhino_de_exit_mb)
plot(simulationOutput_Ac_Rhino_de_exit_mb)
testDispersion(simulationOutput_Ac_Rhino_de_exit_mb)
testZeroInflation(simulationOutput_Ac_Rhino_de_exit_mb)
## summary
summary(model_Ac_Rhino_de_exit_mb)

#### Rhinolophus denti exit bd
model_Ac_Rhino_de_exit_bd <- glmmTMB::glmmTMB(as.numeric(Nb_batpass) ~ Month_nb  + (1|month2), data = Ac_Rhino_de_exit_bd, family = glmmTMB::lognormal)
## model checking and diagnostics
simulationOutput_Ac_Rhino_de_exit_bd <- simulateResiduals(fittedModel = model_Ac_Rhino_de_exit_bd)
plot(simulationOutput_Ac_Rhino_de_exit_bd)
testDispersion(simulationOutput_Ac_Rhino_de_exit_bd)
testZeroInflation(simulationOutput_Ac_Rhino_de_exit_bd)
## summary
summary(model_Ac_Rhino_de_exit_bd)


### Rhino macro tria exit
model_Ac_Rhino_macro_tria_exit <- glmmTMB::glmmTMB(as.numeric(Nb_batpass) ~ Month_nb + (1|month2), data = Ac_Rhino_macro_tria_exit, family = glmmTMB::lognormal)
## model checking and diagnostics
simulationOutput_Ac_Rhino_macro_tria_exit <- simulateResiduals(fittedModel = model_Ac_Rhino_macro_tria_exit)
plot(simulationOutput_Ac_Rhino_macro_tria_exit)
testDispersion(simulationOutput_Ac_Rhino_macro_tria_exit)
testZeroInflation(simulationOutput_Ac_Rhino_macro_tria_exit)
## summary
summary(model_Ac_Rhino_macro_tria_exit)


#### MAKE COEFF PLOT FOR ALL MODELS EMERGENCE ----------
library(sjPlot)
library(sjlabelled)
library(sjmisc)
library(ggplot2)

model_exit_mb <- list(model_Ac_Hippo_exit_mb, model_Ac_Minio_exit_mb,model_Ac_Rhino_la_exit_mb, model_Ac_Rhino_de_exit_mb) 
model_bd<- list(model_Ac_Hippo_exit_bd, model_Ac_Minio_exit_bd, model_Ac_Rhino_de_exit_bd,model_Ac_Rhino_macro_tria_exit)
ggstats::ggcoef_compare(model_bd, exponentiate = F)


# graph with bonferroni correction
plot_emergence_mb <- plot_models(model_exit_mb, show.intercept= T, legend.title = "Group", axis.labels = c(
  "Dec", "Nov","Oct","Sep","Aug","Jul","Jun","May","Apr","Mar","Feb", "Intercept - Jan"
),
m.labels = c( "Hipposideros caffer","Miniopterus group", "Rhinolophus landeri-alcyone", "Rhinolophus denti"), show.values = F, p.shape = TRUE, std.est = "std2", spacing = 0.3, p.adjust="bonferroni")

plot_emergence_mb + theme_sjplot(base_size = 12, base_family = "")

plot_emergence_bd <- plot_models(model_bd, show.intercept= T, legend.title = "Group", axis.labels = c(
  "Dec", "Nov","Oct","Sep","Aug","Jul","Jun","May","Apr","Mar","Feb", "Intercept - Jan"
),
m.labels = c( "Hipposideros caffer","Miniopterus group", "Rhinolophus denti", "Rhinolophus Macro Tria group"), show.values = F, p.shape = TRUE, std.est = "std2", spacing = 0.3, p.adjust="bonferroni")

plot_emergence_bd + theme_sjplot(base_size = 12, base_family = "")

### create tab model with bonferroni correction

tab_model(model_Ac_Hippo_exit_mb, model_Ac_Minio_exit_mb,model_Ac_Rhino_la_exit_mb, model_Ac_Rhino_de_exit_mb, pred.labels= c("Intercept Jan", "Feb", "Mar","Apr","May", "Jun", "Jul","Aug","Sep","Oct","Nov","Dec"), dv.labels= c("Hipposideros caffer emergence Mont Belo","Miniopterus group emergence Mont Belo", "Rhino landeri-alcyone emergence Mont Belo", "Rhinolophus denti emergence Mont Belo"), p.adjust="bonferroni", p.style = "numeric_stars", show.aicc = T)


tab_model(model_Ac_Hippo_exit_bd, model_Ac_Minio_exit_bd,model_Ac_Rhino_de_exit_bd,model_Ac_Rhino_macro_tria_exit, pred.labels= c("Intercept Jan", "Feb", "Mar","Apr","May", "Jun", "Jul","Aug","Sep","Oct","Nov","Dec"), dv.labels= c("Hipposideros caffer emergence Boundou","Miniopterus group emergence Boundou", "Rhinolophus denti emergence Boundou", "Rhinolophus Macro Tria group emergence Boundou"), p.adjust="bonferroni", p.style = "numeric_stars", show.aicc = T)


########. Calculated predicted values for each group
predict_model_Ac_Hippo_exit_mb<- ggeffects::ggpredict(model_Ac_Hippo_exit_mb, terms = "Month_nb")
predict_model_Ac_Hippo_exit_bd<- ggeffects::ggpredict(model_Ac_Hippo_exit_bd, terms = "Month_nb")
predict_model_Ac_Minio_exit_mb<- ggeffects::ggpredict(model_Ac_Minio_exit_mb, terms = "Month_nb")
predict_model_Ac_Minio_exit_bd<- ggeffects::ggpredict(model_Ac_Minio_exit_bd, terms = "Month_nb")
predict_model_Ac_Rhino_la_exit_mb<- ggeffects::ggpredict(model_Ac_Rhino_la_exit_mb, terms = "Month_nb")
predict_model_Ac_Rhino_de_exit_mb<- ggeffects::ggpredict(model_Ac_Rhino_de_exit_mb, terms = "Month_nb")
predict_model_Ac_Rhino_de_exit_bd<- ggeffects::ggpredict(model_Ac_Rhino_de_exit_bd, terms = "Month_nb")
predict_model_Ac_Rhino_macro_tria_exit <- ggeffects::ggpredict(model_Ac_Rhino_macro_tria_exit, terms = "Month_nb")

###### Graph -------------
y_expand_factor <- 0.5
## ----
plot_model_Ac_Hippo_exit_mb <- ggplot(predict_model_Ac_Hippo_exit_mb, aes(x, y = predicted, group= 1)) +
  geom_line(linewidth=0.5, color ="#E7298A")  +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .2, fill ="#E7298A") +
  labs(x="Month", 
       y="Number of bat pass",
       title = "Mont Belo") +
  theme_bw() +
  scale_y_continuous(expand = c(0, y_expand_factor))
plot_model_Ac_Hippo_exit_mb


##
plot_model_Ac_Hippo_exit_bd <- ggplot(predict_model_Ac_Hippo_exit_bd, aes(x, y = predicted, group= 1)) +
  geom_line(linewidth=0.5, color ="#E7298A")  +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .2, fill ="#E7298A") +
  labs(x="Month", 
       y="Number of bat pass",
       title = "Boundou") +
  theme_bw() +
  scale_y_continuous(expand = c(0, y_expand_factor))
plot_model_Ac_Hippo_exit_bd

## -----
plot_model_Ac_Minio_exit_mb <- ggplot(predict_model_Ac_Minio_exit_mb, aes(x, y = predicted, group= 1)) +
  geom_line(linewidth=0.5, color="#7570B3")  +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .2, fill ="#7570B3") +
  labs(x="Month", 
       y="Number of bat pass",
       title = "Mont Belo") +
  theme_bw() +
  scale_y_continuous(expand = c(0, y_expand_factor))
plot_model_Ac_Minio_exit_mb

##
plot_model_Ac_Minio_exit_bd <- ggplot(predict_model_Ac_Minio_exit_bd, aes(x, y = predicted, group= 1)) +
  geom_line(linewidth=0.5, color="#7570B3")  +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .2, fill ="#7570B3") +
  labs(x="Month", 
       y="Number of bat pass",
       title = "Boundou") +
  theme_bw() +
  scale_y_continuous(expand = c(0, y_expand_factor))
plot_model_Ac_Minio_exit_bd

## -----
plot_model_Ac_Rhino_la_exit_mb <- ggplot(predict_model_Ac_Rhino_la_exit_mb, aes(x, y = predicted, group= 1)) +
  geom_line(linewidth=0.5, color= "#FDE725FF")  +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .2, fill ="#FDE725FF") +
  labs(x="Month", 
       y="Number of bat pass",
       title = "Mont Belo") +
  theme_bw() +
  scale_y_continuous(expand = c(0, y_expand_factor))
plot_model_Ac_Rhino_la_exit_mb

## ---- Don't work not enough data
plot_model_Ac_Rhino_de_exit_mb <- ggplot(predict_model_Ac_Rhino_de_exit_mb, aes(x, y = predicted, group= 1)) +
  geom_line(linewidth=0.5, color ="skyblue2")  +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .2, fill ="skyblue2") +
  labs(x="Month", 
       y="Number of bat pass",
       title = "Mont Belo") +
  theme_bw() +
  scale_y_continuous(expand = c(0, y_expand_factor))
plot_model_Ac_Rhino_de_exit_mb


plot_model_Ac_Rhino_de_exit_bd <- ggplot(predict_model_Ac_Rhino_de_exit_bd, aes(x, y = predicted, group= 1)) +
  geom_line(linewidth=0.5, color= "skyblue2")  +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .2, fill ="skyblue2") +
  labs(x="Month", 
       y="Number of bat pass",
       title = "Boundou") +
  theme_bw() +
  scale_y_continuous(expand = c(0, y_expand_factor))
plot_model_Ac_Rhino_de_exit_bd

## ------
plot_model_Ac_Rhino_macro_tria_exit <- ggplot(predict_model_Ac_Rhino_macro_tria_exit, aes(x = x, y = predicted, group = 1)) +
  geom_line(linewidth = 1, color="#1B9E77") +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .2, fill ="#1B9E77") +
  labs(x = "Month", 
       y = "Number of bat pass",
       title = "Boundou") +
  theme_bw() +
  scale_y_continuous(expand = c(0, y_expand_factor))

plot_model_Ac_Rhino_macro_tria_exit

#### agregation predicted plot

# Combine the plots using facet_wrap ----
combined_predict_MB <- list(plot_model_Ac_Hippo_exit_mb, plot_model_Ac_Minio_exit_mb, plot_model_Ac_Rhino_la_exit_mb)

# Your color and label specifications
legend_colors <- c("#FDE725FF", "#E7298A", "#7570B3", "skyblue2")
legend_labels <- c("Rhinolophus landeri-alcyone", "Hipposideros caffer", "Miniopterus group", "Rhinolophus denti")

# Add legend to the first plot in the list
combined_predict_MB[[1]] <- combined_predict_MB[[1]] +
  scale_fill_manual(name = "Legend",
                    values = setNames(legend_colors, legend_labels),
                    labels = legend_labels) +
  theme(legend.position = "top",
        legend.key = element_rect(fill = "white", color = "white"))

predict_plot_MB <- plot_model_Ac_Hippo_exit_mb + plot_model_Ac_Minio_exit_mb + plot_model_Ac_Rhino_la_exit_mb
predict_plot_MB


### Combined MB & BD

combined_predict <- list(plot_model_Ac_Hippo_exit_mb, plot_model_Ac_Minio_exit_mb, plot_model_Ac_Rhino_la_exit_mb, plot_model_Ac_Hippo_exit_bd, plot_model_Ac_Minio_exit_bd, plot_model_Ac_Rhino_de_exit_bd,plot_model_Ac_Rhino_macro_tria_exit)

# Your color and label specifications
legend_colors <- c("#FDE725FF", "#E7298A", "#7570B3", "skyblue2", "#1B9E77")
legend_labels <- c("Rhinolophus landeri-alcyone", "Hipposideros caffer", "Miniopterus group", "Rhinolophus denti", "Rhinolophus Macro Tria")

# Add legend to the first plot in the list
combined_predict[[1]] <- combined_predict[[1]] +
  scale_fill_manual(name = "Legend",
                    values = setNames(legend_colors, legend_labels),
                    labels = legend_labels) +
  theme(legend.position = "top",
        legend.key = element_rect(fill = "white", color = "white"))

predict_plot <- patchwork::wrap_plots(combined_predict) +patchwork::plot_layout(guides = 'collect')
predict_plot


#################### RE- ENTRY OF BATS INSIDE CAVE ##################-----------

### files

### re-enter

Ac_Hippo_re_mb <- AllBat_day_nb_batpass_allsites |>
  filter(Time == "re-enter cave (2h before sunrise)") |>
  filter(Sites== "Mont Belo") |>
  filter(Group_sp == "Hippo sp") |>
  dplyr::arrange(Date_formatted)

Ac_Hippo_re_bd <- AllBat_day_nb_batpass_allsites |>
  filter(Time == "re-enter cave (2h before sunrise)") |>
  filter(Group_sp == "Hippo sp") |>
  filter(Sites == "Boundou") |>
  dplyr::arrange(Date_formatted)

Ac_Minio_re_mb <- AllBat_day_nb_batpass_allsites |>
  filter(Time == "re-enter cave (2h before sunrise)") |>
  filter(Group_sp == "Minio sp") |>
  filter(Sites== "Mont Belo") |>
  arrange(Date_formatted)

Ac_Minio_re_bd <- AllBat_day_nb_batpass_allsites |>
  filter(Time == "re-enter cave (2h before sunrise)") |>
  filter(Group_sp == "Minio sp") |>
  filter(Sites == "Boundou") |>
  arrange(Date_formatted)

Ac_Rhino_la_re_mb <- AllBat_day_nb_batpass_allsites |>
  filter(Time == "re-enter cave (2h before sunrise)") |>
  filter(Group_sp == "Rhino fum") |>
  filter(Sites== "Mont Belo") |>
  arrange(Date_formatted)

Ac_Rhino_de_re_mb <- AllBat_day_nb_batpass_allsites |>
  filter(Time == "re-enter cave (2h before sunrise)") |>
  filter(Group_sp == "Rhino ld") |>
  filter(Sites== "Mont Belo") |>
  arrange(Date_formatted)

Ac_Rhino_de_re_bd <- AllBat_day_nb_batpass_allsites |>
  filter(Time == "re-enter cave (2h before sunrise)") |>
  filter(Group_sp == "Rhino ld") |>
  filter(Sites == "Boundou") |>
  arrange(Date_formatted)

Ac_Rhino_macro_tria_re <- AllBat_day_nb_batpass_allsites |>
  filter(Time == "re-enter cave (2h before sunrise)") |>
  filter(Group_sp == "Rhino macro tria") |>
  filter(Sites == "Boundou") |>
  arrange(Date_formatted)


##

## transform character in factor ------
Ac_Hippo_re_mb <- mutate_if(Ac_Hippo_re_mb, is.character, as.factor)
Ac_Hippo_re_bd <- mutate_if(Ac_Hippo_re_bd, is.character, as.factor)
Ac_Minio_re_mb <- mutate_if(Ac_Minio_re_mb, is.character, as.factor)
Ac_Minio_re_bd <- mutate_if(Ac_Minio_re_bd, is.character, as.factor)
Ac_Rhino_la_re_mb <- mutate_if(Ac_Rhino_la_re_mb, is.character, as.factor)
Ac_Rhino_de_re_mb <- mutate_if(Ac_Rhino_de_re_mb, is.character, as.factor)
Ac_Rhino_de_re_bd <- mutate_if(Ac_Rhino_de_re_bd, is.character, as.factor)
Ac_Rhino_macro_tria_re <- mutate_if(Ac_Rhino_macro_tria_re, is.character, as.factor)


######### RE_ENTER ##########

## Hippo re-enter mb
model_Ac_Hippo_re_mb <- glmmTMB::glmmTMB(as.numeric(Nb_batpass) ~ Month_nb  + (1|month2), data = Ac_Hippo_re_mb, family = lognormal)
## model checking and diagnostics
simulationOutput_Ac_Hippo_re_mb <- simulateResiduals(fittedModel = model_Ac_Hippo_re_mb)
plot(simulationOutput_Ac_Hippo_re_mb)
testDispersion(simulationOutput_Ac_Hippo_re_mb)
testZeroInflation(simulationOutput_Ac_Hippo_re_mb)
## summary
summary(model_Ac_Hippo_re_mb)

## Hippo re bd
model_Ac_Hippo_re_bd <- glmmTMB::glmmTMB(as.numeric(Nb_batpass) ~ Month_nb  + (1|month2), data = Ac_Hippo_re_bd, family = lognormal)
## model checking and diagnostics
simulationOutput_Ac_Hippo_re_bd <- simulateResiduals(fittedModel = model_Ac_Hippo_re_bd)
plot(simulationOutput_Ac_Hippo_re_bd)
testDispersion(simulationOutput_Ac_Hippo_re_bd)
testZeroInflation(simulationOutput_Ac_Hippo_re_bd)
## summary
summary(model_Ac_Hippo_re_bd)

#### Miniopterus re mb
model_Ac_Minio_re_mb <- glmmTMB::glmmTMB(as.numeric(Nb_batpass) ~ Month_nb + (1|month2), data = Ac_Minio_re_mb, family = lognormal)
## model checking and diagnostics
simulationOutput_Ac_Minio_re_mb <- simulateResiduals(fittedModel = model_Ac_Minio_re_mb)
plot(simulationOutput_Ac_Minio_re_mb)
testDispersion(simulationOutput_Ac_Minio_re_mb)
testZeroInflation(simulationOutput_Ac_Minio_re_mb)
## summary
summary(model_Ac_Minio_re_mb)


#### Miniopterus re bd
model_Ac_Minio_re_bd <- glmmTMB::glmmTMB(as.numeric(Nb_batpass) ~ Month_nb + (1|month2), data = Ac_Minio_re_bd, family = lognormal)
## model checking and diagnostics
simulationOutput_Ac_Minio_re_bd <- simulateResiduals(fittedModel = model_Ac_Minio_re_bd)
plot(simulationOutput_Ac_Minio_re_bd)
testDispersion(simulationOutput_Ac_Minio_re_bd)
testZeroInflation(simulationOutput_Ac_Minio_re_bd)
## summary
summary(model_Ac_Minio_re_bd)


#### Rhinolophus alcyone landeri re mb
model_Ac_Rhino_la_re_mb <- glmmTMB::glmmTMB(as.numeric(Nb_batpass) ~ Month_nb + (1|month2), data = Ac_Rhino_la_re_mb, family = glmmTMB::lognormal)
## model checking and diagnostics
simulationOutput_Ac_Rhino_la_re_mb <- simulateResiduals(fittedModel = model_Ac_Rhino_la_re_mb)
plot(simulationOutput_Ac_Rhino_la_re_mb)
testDispersion(simulationOutput_Ac_Rhino_la_re_mb)
testZeroInflation(simulationOutput_Ac_Rhino_la_re_mb)
## summary
summary(model_Ac_Rhino_la_re_mb)


#### Rhinolophus denti re mb
model_Ac_Rhino_de_re_mb <- glmmTMB::glmmTMB(as.numeric(Nb_batpass) ~ Month_nb  + (1|month2), data = Ac_Rhino_de_re_mb, family = glmmTMB::lognormal)
## model checking and diagnostics
simulationOutput_Ac_Rhino_de_re_mb <- simulateResiduals(fittedModel = model_Ac_Rhino_de_re_mb)
plot(simulationOutput_Ac_Rhino_de_re_mb)
testDispersion(simulationOutput_Ac_Rhino_de_re_mb)
testZeroInflation(simulationOutput_Ac_Rhino_de_re_mb)
## summary
summary(model_Ac_Rhino_de_re_mb)

#### Rhinolophus denti re bd
model_Ac_Rhino_de_re_bd <- glmmTMB::glmmTMB(as.numeric(Nb_batpass) ~ Month_nb  + (1|month2), data = Ac_Rhino_de_re_bd, family = glmmTMB::lognormal)
## model checking and diagnostics
simulationOutput_Ac_Rhino_de_re_bd <- simulateResiduals(fittedModel = model_Ac_Rhino_de_re_bd)
plot(simulationOutput_Ac_Rhino_de_re_bd)
testDispersion(simulationOutput_Ac_Rhino_de_re_bd)
testZeroInflation(simulationOutput_Ac_Rhino_de_re_bd)
## summary
summary(model_Ac_Rhino_de_re_bd)


### Rhino macro tria re
model_Ac_Rhino_macro_tria_re <- glmmTMB::glmmTMB(as.numeric(Nb_batpass) ~ Month_nb + (1|month2), data = Ac_Rhino_macro_tria_re, family = glmmTMB::lognormal)
## model checking and diagnostics
simulationOutput_Ac_Rhino_macro_tria_re <- simulateResiduals(fittedModel = model_Ac_Rhino_macro_tria_re)
plot(simulationOutput_Ac_Rhino_macro_tria_re)
testDispersion(simulationOutput_Ac_Rhino_macro_tria_re)
testZeroInflation(simulationOutput_Ac_Rhino_macro_tria_re)
## summary
summary(model_Ac_Rhino_macro_tria_re)

#####

#### MAKE COEFF PLOT FOR ALL MODELS EMERGENCE ----------
library(sjPlot)
library(sjlabelled)
library(sjmisc)
library(ggplot2)

model_re_mb <- list(model_Ac_Hippo_re_mb, model_Ac_Minio_re_mb,model_Ac_Rhino_la_re_mb, model_Ac_Rhino_de_re_mb) 
model_re_bd<- list(model_Ac_Hippo_re_bd, model_Ac_Minio_re_bd, model_Ac_Rhino_de_re_bd,model_Ac_Rhino_macro_tria_re)


# graph with bonferroni correction
plot_re_mb <- plot_models(model_re_mb, show.intercept= T, legend.title = "Group", axis.labels = c(
  "Dec", "Nov","Oct","Sep","Aug","Jul","Jun","May","Apr","Mar","Feb", "Intercept - Jan"
),
m.labels = c( "Hipposideros caffer","Miniopterus group", "Rhinolophus landeri-alcyone", "Rhinolophus denti"), show.values = F, p.shape = TRUE, std.est = "std2", spacing = 0.3, p.adjust="bonferroni")

plot_re_mb + theme_sjplot(base_size = 12, base_family = "")

plot_re_bd <- plot_models(model_re_bd, show.intercept= T, legend.title = "Group", axis.labels = c(
  "Dec", "Nov","Oct","Sep","Aug","Jul","Jun","May","Apr","Mar","Feb", "Intercept - Jan"
),
m.labels = c( "Hipposideros caffer","Miniopterus group", "Rhinolophus denti", "Rhinolophus Macro Tria group"), show.values = F, p.shape = TRUE, std.est = "std2", spacing = 0.3, p.adjust="bonferroni")

plot_re_bd + theme_sjplot(base_size = 12, base_family = "")

### create tab model with bonferroni correction

tab_model(model_Ac_Hippo_re_mb, model_Ac_Minio_re_mb,model_Ac_Rhino_la_re_mb, model_Ac_Rhino_de_re_mb, pred.labels= c("Intercept Jan", "Feb", "Mar","Apr","May", "Jun", "Jul","Aug","Sep","Oct","Nov","Dec"), dv.labels= c("Hipposideros caffer enter Mont Belo","Miniopterus group enter Mont Belo", "Rhino landeri-alcyone enter Mont Belo", "Rhinolophus denti enter Mont Belo"), p.adjust="bonferroni", p.style = "numeric_stars", show.aicc = T)


tab_model(model_Ac_Hippo_re_bd, model_Ac_Minio_re_bd,model_Ac_Rhino_de_re_bd,model_Ac_Rhino_macro_tria_re, pred.labels= c("Intercept Jan", "Feb", "Mar","Apr","May", "Jun", "Jul","Aug","Sep","Oct","Nov","Dec"), dv.labels= c("Hipposideros caffer enter Boundou","Miniopterus group enter Boundou", "Rhinolophus denti enter Boundou", "Rhinolophus Macro Tria group enter Boundou"), p.adjust="bonferroni", p.style = "numeric_stars", show.aicc = T)


########. Calculated predicted values for each group
predict_model_Ac_Hippo_re_mb<- ggeffects::ggpredict(model_Ac_Hippo_re_mb, terms = "Month_nb")
predict_model_Ac_Hippo_re_bd<- ggeffects::ggpredict(model_Ac_Hippo_re_bd, terms = "Month_nb")
predict_model_Ac_Minio_re_mb<- ggeffects::ggpredict(model_Ac_Minio_re_mb, terms = "Month_nb")
predict_model_Ac_Minio_re_bd<- ggeffects::ggpredict(model_Ac_Minio_re_bd, terms = "Month_nb")
predict_model_Ac_Rhino_la_re_mb<- ggeffects::ggpredict(model_Ac_Rhino_la_re_mb, terms = "Month_nb")
predict_model_Ac_Rhino_de_re_mb<- ggeffects::ggpredict(model_Ac_Rhino_de_re_mb, terms = "Month_nb")
predict_model_Ac_Rhino_de_re_bd<- ggeffects::ggpredict(model_Ac_Rhino_de_re_bd, terms = "Month_nb")
predict_model_Ac_Rhino_macro_tria_re <- ggeffects::ggpredict(model_Ac_Rhino_macro_tria_re, terms = "Month_nb")

###### Graph -------------

## ----
plot_model_Ac_Hippo_re_mb <- ggplot(predict_model_Ac_Hippo_re_mb, aes(x, y = predicted, group= 1)) +
  geom_line(linewidth=0.5, color ="#E7298A")  +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .2, fill ="#E7298A") +
  labs(x="Month", 
       y="Number of bat pass",
       title = "Mont Belo") +
  theme_bw() +
  scale_y_continuous(expand = c(0, y_expand_factor))
plot_model_Ac_Hippo_re_mb


##
plot_model_Ac_Hippo_re_bd <- ggplot(predict_model_Ac_Hippo_re_bd, aes(x, y = predicted, group= 1)) +
  geom_line(linewidth=0.5, color ="#E7298A")  +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .2, fill ="#E7298A") +
  labs(x="Month", 
       y="Number of bat pass",
       title = "Boundou") +
  theme_bw() +
  scale_y_continuous(expand = c(0, y_expand_factor))
plot_model_Ac_Hippo_re_bd

## -----
plot_model_Ac_Minio_re_mb <- ggplot(predict_model_Ac_Minio_re_mb, aes(x, y = predicted, group= 1)) +
  geom_line(linewidth=0.5, color="#7570B3")  +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .2, fill ="#7570B3") +
  labs(x="Month", 
       y="Number of bat pass",
       title = "Mont Belo") +
  theme_bw() +
  scale_y_continuous(expand = c(0, y_expand_factor))
plot_model_Ac_Minio_re_mb

##
plot_model_Ac_Minio_re_bd <- ggplot(predict_model_Ac_Minio_re_bd, aes(x, y = predicted, group= 1)) +
  geom_line(linewidth=0.5, color="#7570B3")  +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .2, fill ="#7570B3") +
  labs(x="Month", 
       y="Number of bat pass",
       title = "Boundou") +
  theme_bw() +
  scale_y_continuous(expand = c(0, y_expand_factor))
plot_model_Ac_Minio_re_bd

## -----
plot_model_Ac_Rhino_la_re_mb <- ggplot(predict_model_Ac_Rhino_la_re_mb, aes(x, y = predicted, group= 1)) +
  geom_line(linewidth=0.5, color= "#FDE725FF")  +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .2, fill ="#FDE725FF") +
  labs(x="Month", 
       y="Number of bat pass",
       title = "Mont Belo") +
  theme_bw() +
  scale_y_continuous(expand = c(0, y_expand_factor))
plot_model_Ac_Rhino_la_re_mb

## ---- Don't work not enough data
plot_model_Ac_Rhino_de_re_mb <- ggplot(predict_model_Ac_Rhino_de_re_mb, aes(x, y = predicted, group= 1)) +
  geom_line(linewidth=0.5, color ="skyblue2")  +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .2, fill ="skyblue2") +
  labs(x="Month", 
       y="Number of bat pass",
       title = "Mont Belo") +
  theme_bw() +
  scale_y_continuous(expand = c(0, y_expand_factor))
plot_model_Ac_Rhino_de_re_mb


plot_model_Ac_Rhino_de_re_bd <- ggplot(predict_model_Ac_Rhino_de_re_bd, aes(x, y = predicted, group= 1)) +
  geom_line(linewidth=0.5, color= "skyblue2")  +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .2, fill ="skyblue2") +
  labs(x="Month", 
       y="Number of bat pass",
       title = "Boundou") +
  theme_bw() +
  scale_y_continuous(expand = c(0, y_expand_factor))
plot_model_Ac_Rhino_de_re_bd

## ------
plot_model_Ac_Rhino_macro_tria_re <- ggplot(predict_model_Ac_Rhino_macro_tria_re, aes(x = x, y = predicted, group = 1)) +
  geom_line(linewidth = 1, color="#1B9E77") +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .2, fill ="#1B9E77") +
  labs(x = "Month", 
       y = "Number of bat pass",
       title = "Boundou") +
  theme_bw() +
  scale_y_continuous(expand = c(0, y_expand_factor))

plot_model_Ac_Rhino_macro_tria_re

#### agregation predicted plot

# Combine the plots using facet_wrap ----
combined_predict_re_MB <- list(plot_model_Ac_Hippo_re_mb, plot_model_Ac_Minio_re_mb, plot_model_Ac_Rhino_la_re_mb)

# Your color and label specifications
legend_colors <- c("#FDE725FF", "#E7298A", "#7570B3", "skyblue2")
legend_labels <- c("Rhinolophus landeri-alcyone", "Hipposideros caffer", "Miniopterus group", "Rhinolophus denti")

# Add legend to the first plot in the list
combined_predict_re_MB[[1]] <- combined_predict_re_MB[[1]] +
  scale_fill_manual(name = "Legend",
                    values = setNames(legend_colors, legend_labels),
                    labels = legend_labels) +
  theme(legend.position = "top",
        legend.key = element_rect(fill = "white", color = "white"))

predict_plot_re_MB <- plot_model_Ac_Hippo_re_mb + plot_model_Ac_Minio_re_mb + plot_model_Ac_Rhino_la_re_mb
predict_plot_re_MB


### Combined MB & BD

combined_predict_re <- list(plot_model_Ac_Hippo_re_mb, plot_model_Ac_Minio_re_mb, plot_model_Ac_Rhino_la_re_mb, plot_model_Ac_Hippo_re_bd, plot_model_Ac_Minio_re_bd, plot_model_Ac_Rhino_de_re_bd,plot_model_Ac_Rhino_macro_tria_re)

# Your color and label specifications
legend_colors <- c("#FDE725FF", "#E7298A", "#7570B3", "skyblue2", "#1B9E77")
legend_labels <- c("Rhinolophus landeri-alcyone", "Hipposideros caffer", "Miniopterus group", "Rhinolophus denti", "Rhinolophus Macro Tria")

# Add legend to the first plot in the list
combined_predict_re[[1]] <- combined_predict_re[[1]] +
  scale_fill_manual(name = "Legend",
                    values = setNames(legend_colors, legend_labels),
                    labels = legend_labels) +
  theme(legend.position = "top",
        legend.key = element_rect(fill = "white", color = "white"))

predict_plot_re <- patchwork::wrap_plots(combined_predict_re) +patchwork::plot_layout(guides = 'collect')
predict_plot_re

