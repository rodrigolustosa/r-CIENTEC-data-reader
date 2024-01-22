# ---------------------------------------------------------------------------- #
# Name         : 
# Description  : This code read and organizes data from CIENTEC (IAG-USP)
#   http://www.estacao.iag.usp.br ask in the "Solicitacao de Dados"
# Written by   : Rodrigo Lustosa
# Writing date : 16 July 2021
# ---------------------------------------------------------------------------- #

# initialization ----------------------------------------------------------

# packages
library(stringi)
library(stringr)
library(dplyr)
library(readr)
library(tidyr)
library(lubridate)
library(readxl)
library(xlsx)
# library(openxlsx)

# directories
dir_input  <- "data/raw"
dir_output <- "data/tidied"
dir_basic_info <- "data/basic_info"
dir_in_temp <- "T ar"

# files 
fil_estations_basic_info <- "a_infos_estacoes.txt"
fil_output_data       <- "dados_cientec.csv"
fil_output_data_dia   <- "dados_cientec_diarios.csv"
fil_output_basic_info <- "estacoes_cientec_localizacao.txt"

# check working directory -------------------------------------------------

dir_wd <- dirname(rstudioapi::getSourceEditorContext()$path)
if(dir_wd != getwd())
  setwd(dir_wd)

# functions ---------------------------------------------------------------

# remove accents and other complex simbols
rm.ac <- function(lines)
  return(stringi::stri_trans_general(lines, "Latin-ASCII")) #remove accents

# string to lower and replace spaces
str_basic <- function(string)
  return(str_to_lower(str_replace_all(str_trim(string)," ","_")))

# data file names ---------------------------------------------------------

# list temperature files input
files_path <- file.path(dir_input,dir_in_temp)
list_files_temp <- dir(files_path)

# stations basic information ----------------------------------------------

# read and write file
file_path <- file.path(dir_basic_info,fil_estations_basic_info)
basic_data_estations <- read_csv(file_path)
file_path <- file.path(dir_basic_info,fil_output_basic_info)
write_csv(basic_data_estations,file_path)

# estations data ----------------------------------------------------------

# number of files
n_files <- length(list_files_temp)
# all data
data_estations     <- vector("list",n_files*12)
data_estations_dia <- vector("list",n_files*12)
# read all files
for(i in 1:n_files){ 
  file_path <- file.path(dir_input,dir_in_temp,list_files_temp[i])
  # for cases where months are not ordered 
  sheet_names <- excel_sheets(path = file_path)
  mes_ordem <- match(sheet_names, c("JAN","FEV","MAR","ABR","MAI","JUN",
                                    "JUL","AGO","SET","OUT","NOV","DEZ"))
  # read data
  for(mes in 1:12){
    i_data <- mes + (i-1)*12
    i_sheet <- which(mes == mes_ordem)
    # read data
    data_estations[[i_data]] <- read_excel(file_path,sheet = i_sheet)
    # detect if there is interpolated data
    prov <- as.matrix(data_estations[[i_data]])
    i_tag_interp <- which(matrix(str_detect(prov,"nterpolado"),
                                 nrow = nrow(prov)),
                          arr.ind = T)
    n_tags <- nrow(i_tag_interp)
    # extract year
    mes_ano <- names(data_estations[[i_data]])#[2]
    for(k in 1:length(mes_ano))
      if(!is.na(str_match(mes_ano[k],"\\d{4}")[1,1]))
        ano <- str_match(mes_ano[k],"\\d{4}")[1,1]
    # check if all years are the same
    if(mes == 1){
      ano_teste <- ano
      same_year <- T
    } else
      same_year <- ano == ano_teste
    # only read if sheet is from the same year as the first month 
    if(same_year){
      # rename header with proper line
      n_col <- length(data_estations[[i_data]])
      col_last_day <- days_in_month(ymd(str_c(ano,mes,"01",sep = "-"))) + 1
      names(data_estations[[i_data]]) <- str_c("X",c("HD",1:(n_col-1)))
      # read max and min temperature
      i_max <- which(str_detect(data_estations[[i_data]]$XHD,"XIMA"))[1]
      i_min <- which(str_detect(data_estations[[i_data]]$XHD,"NIMA"))[1]
      while(is.na(data_estations[[i_data]]$X1[i_max])){i_max <- i_max+1}
      while(is.na(data_estations[[i_data]]$X1[i_min])){i_min <- i_min+1}
      data_estations_dia[[i_data]] <- t(data_estations[[i_data]][c(i_max,i_min),
                                                                 2:col_last_day])
      # tide max and min data
      data_estations_dia[[i_data]] <- data.frame(data_estations_dia[[i_data]]) %>% 
        rename(max = X1, min = X2) %>% 
        mutate(dia = str_remove(rownames(data_estations_dia[[i_data]]),"X"),
               data=ymd(paste(ano,mes,dia)), .before = 1) %>% select(-dia)
      data_estations_dia[[i_data]]$max <- round(as.numeric(data_estations_dia[[i_data]]$max),2)
      data_estations_dia[[i_data]]$min <- round(as.numeric(data_estations_dia[[i_data]]$min),2)
      # select only lines of hourly temperature
      data_estations[[i_data]] <- data_estations[[i_data]][3:(24+2),1:col_last_day]
      # tide data
      data_estations[[i_data]] <- data_estations[[i_data]] %>% 
        gather("dia","temp",all_of(2:col_last_day)) %>% 
        mutate(dia=str_remove(dia,"X")) 
      # tag interpolated data
      interp <- vector(length = nrow(data_estations[[i_data]]))
      if(length(i_tag_interp) > 0){
        i_tag_interp[,1] <- i_tag_interp[,1] + 1 # because of the header
        # read cells styles
        wb     <- loadWorkbook(file_path)
        sheet_m <- getSheets(wb)[[i_sheet]]
        rows  <- getRows(sheet_m)
        cells <- getCells(rows)
        styles <- sapply(cells, getCellStyle)
        # get interpolated data style
        i_tag          <- vector("numeric",n_tags)
        tag_font_index <- vector("numeric",n_tags)
        for (t in 1:n_tags) {
          i_tag[t] <- which(names(styles) == paste(i_tag_interp[t,], 
                                                   collapse = "."))
          tag_font_index[t] <- styles[[i_tag[t]]]$getFontIndex()
        }
        tag_font_index <- unique(tag_font_index)
        for (d in 1:nrow(data_estations[[i_data]])) {
          row <- as.numeric(data_estations[[i_data]]$XHD[d])+3
          col <- as.numeric(data_estations[[i_data]]$dia[d])+1
          i_cell <- which(names(styles) == paste(row,col, sep = "."))
          interp[d] <- styles[[i_cell]]$getFontIndex() %in% tag_font_index
        }
      }
      # add interpolate and date column
      data_estations[[i_data]] <- data_estations[[i_data]] %>% 
        mutate(interp) %>% 
        mutate(ano,mes,data=ymd_h(str_c(ano,mes,dia,XHD,":",sep = "-")),
               .before=1) %>% 
        select(-c(XHD,dia,mes,ano)) %>% filter(!is.na(temp))
    }else{
      data_estations[[i_data]] <- NULL
      data_estations_dia[[i_data]] <- NULL
    }
  }
}
# for(i in 1:length(data_estations)){
#   if(length(data_estations[[i]]) > 4)
#     print(i)
# }
# combine data in one
data_estations     <- bind_rows(data_estations)
data_estations_dia <- bind_rows(data_estations_dia)
# tide data
data_estations <- data_estations %>% filter(!is.na(data)) %>% 
  mutate(estacao=basic_data_estations$estacao,
         latitude=basic_data_estations$latitude,
         longitude=basic_data_estations$longitude) %>% 
  select(estacao:longitude,data:temp) %>% 
  arrange(data)
# data_estations$temp <- round(data_estations$temp,2)
data_estations$temp <- round(as.numeric(data_estations$temp),2)
data_estations_dia <- data_estations_dia %>% arrange(data)
#save hourly data
file_path <- file.path(dir_output,fil_output_data)
write_csv(data_estations,file_path,na="")
#save daily data
file_path <- file.path(dir_output,fil_output_data_dia)
write_csv(data_estations_dia,file_path,na="")

# clear workspace
rm(list = ls())

# ---------------------------------------------------------------------------- #