
# read and process R_e data ----
# obtained from covid-19-Re (https://github.com/covid-19-Re/dailyRe-Data)
if (!(file.exists(path_data_r_e_ch_processed)) | do_new_load_data) {
  
  # read daily R_e data from github
  data_r_e_ch_raw <- read_csv("https://raw.githubusercontent.com/covid-19-Re/dailyRe-Data/master/CHE-estimates.csv")
  
  # store raw data
  write_csv(x = data_r_e_ch_raw,
            file = path_data_r_e_ch_raw)
  
  # `data_r_e_ch_raw`:
  # (a) filter to values for the whole of Switzerland
  # (b) filter to values in 2021
  # (c) filter to values based on confirmed cases
  # (d) filter to values obtained by sliding window approach
  # (e) add column `x` (x-coordinate used for plotting)
  # (f) select columns `x` and `median_R_mean`
  data_r_e_ch_processed <- data_r_e_ch_raw %>%
    filter(region == "CHE") %>%
    filter(date >= ymd("2021-01-01")) %>%
    filter(date <= ymd("2021-12-31")) %>%
    filter(data_type == "Confirmed cases") %>%
    filter(estimate_type == "Cori_slidingWindow") %>%
    mutate(x = seq(from = 0, to = 12, by = 12/364)) %>%
    dplyr::select(c("date", "x", "country", "estimate_type", "median_R_mean", "median_R_lowHPD", "median_R_highHPD")) %>%
    dplyr::rename(c("value" = "median_R_mean", "boundary_low" = "median_R_lowHPD", "boundary_high" = "median_R_highHPD"))
  
  # store processed data
  write_csv(x = data_r_e_ch_processed, file = path_data_r_e_ch_processed)
  
}

if (!(file.exists(path_data_r_e_dk_processed)) | do_new_load_data) {
  
  # read daily R_e data from github
  data_r_e_dk_raw <- read_csv("https://raw.githubusercontent.com/covid-19-Re/dailyRe-Data/master/DNK-estimates.csv")
  
  # store raw data
  write_csv(x = data_r_e_dk_raw,
            file = path_data_r_e_dk_raw)
  
  # `data_r_e_dk_raw`:
  # (a) filter to values for the whole of Switzerland
  # (b) filter to values in 2021
  # (c) filter to values based on confirmed cases
  # (d) filter to values obtained by sliding window approach
  # (e) add column `x` (x-coordinate used for plotting)
  # (f) select columns `x` and `median_R_mean`
  data_r_e_dk_processed <- data_r_e_dk_raw %>%
    filter(region == "DNK") %>%
    filter(date >= ymd("2021-01-01")) %>%
    filter(date <= ymd("2021-12-31")) %>%
    filter(data_type == "Confirmed cases") %>%
    filter(estimate_type == "Cori_slidingWindow") %>%
    mutate(x = seq(from = 0, to = 12, by = 12/364)) %>%
    dplyr::select(c("date", "x", "country", "estimate_type", "median_R_mean", "median_R_lowHPD", "median_R_highHPD")) %>%
    dplyr::rename(c("value" = "median_R_mean", "boundary_low" = "median_R_lowHPD", "boundary_high" = "median_R_highHPD"))
  
  # store processed data
  write_csv(x = data_r_e_dk_processed, file = path_data_r_e_dk_processed)
  
}

if (!(file.exists(path_data_r_e_de_processed)) | do_new_load_data) {
  
  # read daily R_e data from github
  data_r_e_de_raw <- read_csv("https://raw.githubusercontent.com/covid-19-Re/dailyRe-Data/master/DEU-estimates.csv")
  
  # store raw data
  write_csv(x = data_r_e_de_raw,
            file = path_data_r_e_de_raw)
  
  # `data_r_e_de_raw`:
  # (a) filter to values for the whole of Switzerland
  # (b) filter to values in 2021
  # (c) filter to values based on confirmed cases
  # (d) filter to values obtained by sliding window approach
  # (e) add column `x` (x-coordinate used for plotting)
  # (f) select columns `x` and `median_R_mean`
  data_r_e_de_processed <- data_r_e_de_raw %>%
    filter(region == "DEU") %>%
    filter(date >= ymd("2021-01-01")) %>%
    filter(date <= ymd("2021-12-31")) %>%
    filter(data_type == "Confirmed cases") %>%
    filter(estimate_type == "Cori_slidingWindow") %>%
    mutate(x = seq(from = 0, to = 12, by = 12/364)) %>%
    dplyr::select(c("date", "x", "country", "estimate_type", "median_R_mean", "median_R_lowHPD", "median_R_highHPD")) %>%
    dplyr::rename(c("value" = "median_R_mean", "boundary_low" = "median_R_lowHPD", "boundary_high" = "median_R_highHPD"))
  
  # store processed data
  write_csv(x = data_r_e_de_processed, file = path_data_r_e_de_processed)
  
}
