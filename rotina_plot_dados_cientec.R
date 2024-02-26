# ---------------------------------------------------------------------------- #
# Name         : 
# Description  : 
# Written by   : Rodrigo Lustosa
# Writing date : 24 Nov 2023 10:29 (GMT -03)
# ---------------------------------------------------------------------------- #

# initialization ----------------------------------------------------------

# packages
library(tidyverse)
library(lubridate)

# directory and file names
dir_input  <- "data/tidied"
dir_output <- "plots"
file_data       <- "dados_cientec.csv"
file_data_dia   <- "dados_cientec_diarios.csv"

# read data ---------------------------------------------------------------

file_path <- file.path(dir_input,file_data)
data_estations <- read_csv(file_path)

file_path <- file.path(dir_input,file_data_dia)
data_estations_dia <- read_csv(file_path)


# computations ------------------------------------------------------------

max_acum <- data_estations_dia$max
min_acum <- data_estations_dia$min
for (i in 2:length(max_acum)) {
  max_acum[i] <- max(max_acum[(i-1):i],na.rm = T)
  min_acum[i] <- min(min_acum[(i-1):i],na.rm = T)
}
data_estations_dia <- data_estations_dia %>% 
  # select(-max_acum,-min_acum) %>% 
  mutate(max_acum,min_acum)

data_estations_dia <- full_join(data_estations_dia,
                                data_estations %>% 
                                  mutate(data = date(data)) %>% 
                                  group_by(data) %>% 
                                  summarise(media = mean(temp,na.rm = T)))

data_estations_ano <- data_estations_dia %>% mutate(ano = year(data)) %>% 
  group_by(ano) %>% 
  summarise(media = mean(media,na.rm = T),
            max = mean(max,na.rm = T),
            min = mean(min,na.rm = T))

# stats -------------------------------------------------------------------

head(data_estations_dia %>% arrange(min),20)

head(data_estations_dia %>% arrange(desc(max)),20)

# plots -------------------------------------------------------------------

plot <- data_estations_dia %>% 
  select(-media) %>%
  pivot_longer(-data) %>% 
  ggplot(aes(data,value, 
             linewidth = name,
             alpha = name,
             color = name)) +
  ggtitle("Estação CIENTEC (IAG/USP)") +
  ylab("Temperatura (°C)") +
  geom_line() +
  # scale_x_date(breaks = ymd(paste(seq(1930,2100,10),"01 01")), date_labels = "%Y") +
  scale_color_manual("",values = c("#ff7878","red","#8578ff","blue"),
                     labels = c("máx. diária","máx. histórica","mín. diária","mín. histórica")) +
  scale_linewidth_manual(values = c(0.5,1,0.5,1)) +
  scale_alpha_manual(values = c(0.9,0.8,
                                0.9,0.8)) +
  guides(size = "legend", linewidth = "none", alpha = "none") +
  theme(legend.position = "bottom",axis.text = element_text(colour = "black"))
plot
# file_path <- file.path(dir_output,"maxmin.png")
file_path <- file.path(dir_output,"maxmin2.png")
ggsave(file_path,plot,width = 16, height = 10, units = "cm")

plot <- data_estations_ano %>% 
  filter(ano != 2023) %>% 
  pivot_longer(media:min) %>%
  ggplot(aes(ano,value, color = name)) +
  ggtitle("Cientec") +
  geom_line() +
  ylab("Temperatura (°C)") +
  # ylab("Temperature (°C)") +
  # xlab("Year") +
  scale_x_continuous(breaks = seq(1900,2030,10)) +
  scale_y_continuous(breaks = seq(0,40,2)) +
  geom_text(aes(label = name),
    data = data.frame(ano = 2021, value = c(14.3,18.5,25.4),
                      name = c("min", "media","max"))
                      # name = c("min", "mean","max"))
    ) +
  theme(legend.position = "none")
plot
file_path <- file.path(dir_output,"maxminmean_year.png")
ggsave(file_path,plot,width = 16, height = 10, units = "cm")

# plots for animations ----------------------------------------------------


# for series of plots bellow
temp_min <- min(data_estations_dia$min,na.rm = T)
temp_max <- max(data_estations_dia$max,na.rm = T)
date_min <- min(data_estations_dia$data)
date_max <- max(data_estations_dia$data)
date_dif <- date_max - date_min


# series #1 
date_fim <- date_min
while (date_fim <= date_max + 50) {
  file_path <- file.path(dir_output,"ano",str_c("maxmin-",date_fim,".png"))
  if(!file.exists(file_path)){
    plot <- data_estations_dia %>% 
      select(-media) %>%
      # select(-max,-min) %>% 
      # select(-max_acum,-min_acum) %>%
      pivot_longer(-data) %>% 
      ggplot(aes(data,value, 
                 linewidth = name,
                 alpha = name,
                 color = name)) +
      ggtitle("Estação CIENTEC (IAG/USP)") +
      ylab("Temperatura (°C)") +
      scale_x_date(
        breaks = ymd(paste(rep(seq(1930,2100,1),each=4),
                           rep(seq(1,10,3)),
                           "01")),
        date_labels = "%b %Y",
        limits = c(date_fim - 365,date_fim),
        expand = c(0,0)) +
      scale_y_continuous(limits = c(temp_min,temp_max)) +
      # scale_color_manual("",values = c("#ff7878","#8578ff"),
      #                    labels = c("máx. diária","máx. histórica","mín. diária","mín. histórica")) +
      scale_color_manual("",values = c("#ff7878","red","#8578ff","blue"),
                         labels = c("máx. diária","máx. histórica","mín. diária","mín. histórica")) +
      scale_linewidth_manual(values = c(0.5,1,0.5,1)) +
      scale_alpha_manual(values = c(0.9,0.8,
                                    0.9,0.8)) +
      geom_line() +
      guides(size = "legend", linewidth = "none", alpha = "none") +
      theme(legend.position = "bottom",axis.text = element_text(colour = "black"))
    # plot
    ggsave(file_path,plot,width = 16, height = 10, units = "cm")
  }
  date_fim <- date_fim + 50
}

# series #2 - only first year
for (d in 1:365) {
  file_path <- file.path(dir_output,"primer_ano",str_c("maxmin-",d,".png"))
  if(!file.exists(file_path)){
    plot <- data_estations_dia %>% 
      select(-media) %>%
      filter(data >= date_min, data <= date_min + d) %>% 
      # select(-max,-min) %>% 
      # select(-max_acum,-min_acum) %>%
      pivot_longer(-data) %>% 
      ggplot(aes(data,value, 
                 linewidth = name,
                 alpha = name,
                 color = name)) +
      ggtitle("Estação CIENTEC (IAG/USP)") +
      ylab("Temperatura (°C)") +
      scale_x_date(
        breaks = ymd(paste(rep(seq(1930,2100,1),each=4),
                           rep(seq(1,10,3)),
                           "01")),
        date_labels = "%b %Y",
        limits = c(date_min,date_min + 365),
        expand = c(0,0)) +
      scale_y_continuous(limits = c(temp_min,temp_max)) +
      # scale_color_manual("",values = c("#ff7878","#8578ff"),
      #                    labels = c("máx. diária","máx. histórica","mín. diária","mín. histórica")) +
      scale_color_manual("",values = c("#ff7878","red","#8578ff","blue"),
                         labels = c("máx. diária","máx. histórica","mín. diária","mín. histórica")) +
      scale_linewidth_manual(values = c(0.5,1,0.5,1)) +
      scale_alpha_manual(values = c(0.9,0.8,
                                    0.9,0.8)) +
      geom_line() +
      guides(size = "legend", linewidth = "none", alpha = "none") +
      theme(legend.position = "bottom",axis.text = element_text(colour = "black"))
    # plot
    ggsave(file_path,plot,width = 16, height = 10, units = "cm")
  }
  date_fim <- date_fim + 100
}

# series #3 - first year to everything
date_fim <- date_min + 365
d <- 0
e <- 0
while (d+365 <= date_dif) {
  d <- d + 2^e
  e <- e + 0.3
  file_path <- file.path(dir_output,"serie",str_c("maxmin-",date_fim+d,".png"))
  if(!file.exists(file_path)){
    plot <- data_estations_dia %>% 
      select(-media) %>%
      # select(-max,-min) %>% 
      # select(-max_acum,-min_acum) %>%
      pivot_longer(-data) %>% 
      ggplot(aes(data,value, 
                 linewidth = name,
                 alpha = name,
                 color = name)) +
      ggtitle("Estação CIENTEC (IAG/USP)") +
      ylab("Temperatura (°C)") +
      scale_x_date(
        # breaks = ymd(paste(rep(seq(1930,2100,1),each=4),
        #                    rep(seq(1,10,3)),
        #                    "01")),
        # date_labels = "%b %Y",
        limits = c(date_min,date_fim+d),
        expand = c(0,0)) +
      scale_y_continuous(limits = c(temp_min,temp_max)) +
      scale_color_manual("",values = c("#ff7878","red","#8578ff","blue"),
                         labels = c("máx. diária","máx. histórica","mín. diária","mín. histórica")) +
      scale_linewidth_manual(values = c(0.5,1,0.5,1)) +
      scale_alpha_manual(values = c(0.9,0.8,
                                    0.9,0.8)) +
      geom_line() +
      guides(size = "legend", linewidth = "none", alpha = "none") +
      theme(legend.position = "bottom",axis.text = element_text(colour = "black"))
    # plot
    ggsave(file_path,plot,width = 16, height = 10, units = "cm")
  }
}

# ---------------------------------------------------------------------------- #