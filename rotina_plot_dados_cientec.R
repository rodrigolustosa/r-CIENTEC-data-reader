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

# stats -------------------------------------------------------------------

head(data_estations_dia %>% arrange(min),20)

head(data_estations_dia %>% arrange(desc(max)),20)

# plots -------------------------------------------------------------------

plot <- data_estations_dia %>% 
  # select(-max,-min) %>% 
  pivot_longer(-data) %>% 
  ggplot(aes(data,value, 
             linewidth = name,
             alpha = name,
             color = name)) +
  ggtitle("Estação CIENTEC (IAG/USP)") +
  ylab("Temperatura (°C)") +
  scale_x_date(breaks = ymd(paste(seq(1930,2100,10),"01 01")), date_labels = "%Y") +
  scale_color_manual("",values = c("#ff7878","red","#8578ff","blue"),
                     labels = c("máx. diária","máx. histórica","mín. diária","mín. histórica")) +
  scale_linewidth_manual(values = c(0.5,1,0.5,1)) +
  scale_alpha_manual(values = c(0.9,0.8,
                                0.9,0.8)) +
  geom_line() +
  guides(size = "legend", linewidth = "none", alpha = "none") +
  theme(legend.position = "bottom")
plot
file_path <- file.path(dir_output,"maxmin.png")
ggsave(file_path,plot,width = 16, height = 10, units = "cm")

# ---------------------------------------------------------------------------- #