

#######################################################################################################
##                                                                                                   ##
##  Script name: CESD_check.R                                                                        ##
##  Purpose of script: Check if mental health risk groups are experiencing higher mental health      ##
##                     detriment and less bounce-back during covid epidemic.                         ##
##                     (Asserted by Paul Dolan in Sept 2021, citing Giuntella et al. 2021)           ##
##                                                                                                   ##
##  Notes: Sources:                                                                                  ##
##         Dolan assertion: https://youtu.be/Gne13Rr4mN0?t=6961                                      ##
##         Giuntella et al. 2021: https://doi.org/10.1073/pnas.2016632118                            ##
##                                                                                                   ##
##  Author: Mart Roben                                                                               ##
##  Date Created: 7. Jan 2022                                                                        ##
##                                                                                                   ##
##  Copyright: MIT License                                                                           ##
##  https://github.com/martroben/covid_CESD_check                                                    ##
##                                                                                                   ##
##  Contact: fb.com/martroben                                                                        ##
##                                                                                                   ##
#######################################################################################################


#################
# Load packages #
#################

if (!require("pacman", quietly = TRUE)) install.packages("pacman")

pacman::p_load("magrittr",
               "tibble",
               "dplyr",
               "tidyr",
               "readr",
               "stringr",
               "purrr",
               "ggplot2",
               "zip")



################
# Data sources #
################

# Lifestyle and mental health disruptions during COVID-19
# Osea Giuntella, Kelly Hyde, Silvia Saccardo, Sally Sadoff
# Proceedings of the National Academy of Sciences Mar 2021, 118 (9) e2016632118; DOI: 10.1073/pnas.2016632118
# Open Science Framework data & materials: https://osf.io/f85e3/
raw_data_url <- "https://osf.io/ps6jv/download"



#############
# Functions #
#############

get_filename_path <- function(x, filenames) {
  
  pattern <- stringr::str_c(".*/", x)
  
  filenames %>%
    stringr::str_extract(pattern) %>%
    magrittr::extract(!is.na(.))
}

get_cesd_change <- function(data) {
  
  data %>%
    purrr::map_dfr(tidyr::drop_na) %>%
    dplyr::mutate(risk_group = Baseline >= 16,
                  cesd_change = Endline - Baseline)
}



##################
# Importing data #
##################

raw_data_filename <- "CESD_raw_data.zip"
pre_epidemic_data_csv_filenames <- c("fall2019_CESD_scores.csv", "spr2019_CESD_scores.csv")
epidemic_data_csv_filenames <- "spr2020_CESD_scores.csv"

# Download .zip file with data
download.file(raw_data_url, raw_data_filename)

# Unzip necessary .csv files
zipped_data_filenames <- zip::zip_list(raw_data_filename)$filename

c(pre_epidemic_data_csv_filenames, epidemic_data_csv_filenames) %>%
  purrr::map_chr(get_filename_path, zipped_data_filenames) %>%
  zip::unzip(raw_data_filename, files = ., junkpaths = TRUE)

pre_epidemic_data_raw <- pre_epidemic_data_csv_filenames %>% purrr::map(read_csv, show_col_types = FALSE)
epidemic_data_raw <- epidemic_data_csv_filenames %>% purrr::map(read_csv, show_col_types = FALSE)



##################
# Analyzing data #
##################

pre_epidemic_data <- get_cesd_change(pre_epidemic_data_raw) %>%
  dplyr::mutate(
    group = dplyr::case_when(
      risk_group ~ "risk_noep",
      TRUE ~ "norisk_noep"),
    cohort = "noep")
    
epidemic_data <- get_cesd_change(epidemic_data_raw) %>%
  dplyr::mutate(
    group = dplyr::case_when(
      risk_group ~ "risk_ep",
      TRUE ~ "norisk_ep"),
    cohort = "ep")

bounceback_data <- epidemic_data %>%
  dplyr::mutate(
    cesd_change = july - Endline,
    group = dplyr::case_when(
     risk_group ~ "risk_bounce",
     TRUE ~ "norisk_bounce"),
    cohort = "bounceback")

medians <- c("norisk_noep" = pre_epidemic_data %>% dplyr::filter(!risk_group) %>% dplyr::pull(cesd_change) %>% median(),
             "norisk_ep" = epidemic_data %>% dplyr::filter(!risk_group) %>% dplyr::pull(cesd_change) %>% median(),
             "risk_noep" = pre_epidemic_data %>% dplyr::filter(risk_group) %>% dplyr::pull(cesd_change) %>% median(),
             "risk_ep" = epidemic_data %>% dplyr::filter(risk_group) %>% dplyr::pull(cesd_change) %>% median(),
             "norisk_bounce" = bounceback_data %>% dplyr::filter(!risk_group) %>% dplyr::pull(cesd_change) %>% median(),
             "risk_bounce" = bounceback_data %>% dplyr::filter(risk_group) %>% dplyr::pull(cesd_change) %>% median()) %>%
  tibble::enframe(name = "group", value = "cesd_change")



########
# Plot #
########

plot_title <- "CES-D (depression score) changes during & after semester"
plot_subtitle <- "Comparison of low risk groups (initial CES-D < 16) and high risk groups (initial CES-D >= 16)"
plot_caption <- "data: Giuntella et al. 2021 doi.org/10.1073/pnas.2016632118\nanalysis: github.com/martroben/covid_CESD_check"

cesd_plot <- ggplot2::ggplot() +
  geom_boxplot(data = dplyr::bind_rows(pre_epidemic_data, epidemic_data, bounceback_data),
               aes(x = group, y = cesd_change, color = cohort)) +
  scale_x_discrete(limits = c("norisk_noep",
                              "risk_noep",
                              "norisk_ep",
                              "risk_ep",
                              "norisk_bounce",
                              "risk_bounce"),
                   labels = c("norisk_noep" = "LOW RISK\npre-epidemic\nsemester",
                              "risk_noep" = "HIGH RISK\npre-epidemic\nsemester",
                              "norisk_ep" = "LOW RISK\nepidemic\nsemester",
                              "risk_ep" = "HIGH RISK\nepidemic\nsemester",
                              "norisk_bounce" = "LOW RISK\nepidemic\nafter semester",
                              "risk_bounce" = "HIGH RISK\nepidemic\nafter semester")) +
  geom_text(data = medians, aes(x = group, y = cesd_change, label = cesd_change), size = 3, vjust = -0.5) +
  labs(title = plot_title,
       subtitle = plot_subtitle,
       caption = plot_caption,
       y = "CES-D change",
       x = element_blank()) +
  theme(plot.subtitle = element_text(size = 8),
        plot.caption = element_text(size = 6),
        legend.position = "none",
        panel.background = element_rect(fill = "#F2F2F2")) +
  scale_color_manual(values = viridis_pal(end = 0.6)(3))

cesd_plot

# Apparently not
