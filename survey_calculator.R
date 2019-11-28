library(rcompanion)
library(tidyverse)
setwd("/Users/Vinny/Documents/r_working_directory")

## import file - in future create new data.frame from CSV, using vectors to import count & mass into individual columns

temp_cockle <- read.csv("temp_cockle.csv")

stations <- temp_cockle$Stn
block <- temp_cockle$Block
grid <- temp_cockle$Grid
sampled <- temp_cockle$Sampled
y0_c <- temp_cockle$Y0Count
y1_c <- temp_cockle$Y1Count
y2_c <- temp_cockle$Y2Count
y3_c <- temp_cockle$Y3Count
yt_c <- y0_c + y1_c + y2_c + y3_c

y0_m <- temp_cockle$Y0Weight
y1_m <- temp_cockle$Y1Weight
y2_m <- temp_cockle$Y2Weight
y3_m <- temp_cockle$Y3Weight
yt_m <- y0_m + y1_m + y2_m + y3_m

substrata <- temp_cockle$Substrata
fifteen <- temp_cockle$fifteen

north_stations <- c(187, 188, 189, 207, 208, 209, 227, 228, 229)
south_stations <- c(391, 392, 393, 395, 396, 400, 401, 402, 478)
site_number <- length(sampled)
class <- rep(c("Y0", "Y1", "Y2", "Y3", "Yt"), times = c(site_number, site_number, site_number, site_number, site_number))
class = factor(class, levels = c("Y0", "Y1", "Y2", "Y3", "Yt"), ordered = TRUE)
sampled_250 <- sum(sampled == "Y" & grid =="250m", na.rm = TRUE)
sampled_100n <- sum(sampled == "Y" & block =="ZN", na.rm = TRUE)
sampled_100s <- sum(sampled == "Y" & block =="ZS", na.rm = TRUE)
sampled_250_n <- sum(sampled == "Y" & stations %in% north_stations, na.rm = TRUE)
sampled_250_s <- sum(sampled == "Y" & stations %in% south_stations, na.rm = TRUE)

stations_rep <- rep(stations, times = 5)
block_rep <- rep(block, times = 5)
grid_rep <- rep(grid, times = 5)
sampled_rep <- rep(sampled, times = 5)
count <- c(y0_c, y1_c, y2_c, y3_c, yt_c)
mass <- c(y0_m, y1_m, y2_m, y3_m, yt_m)

temp_cockle_df <- data.frame(stations_rep, block_rep, grid_rep, sampled_rep, class,  count, mass)

## mutate to add values per sq metre

temp_cockle_df <- temp_cockle_df %>% mutate(count_metre = count*10) %>% mutate(mass_metre = mass*10/1000) 

## summary table

summary_tab <- temp_cockle_df %>% filter(grid_rep %in% c ("250m", "100m_n", "100m_s")) %>%  mutate(count_sum = case_when(grid_rep == "250m" ~ count_metre*250^2, grid_rep != "250m" ~ count_metre*100^2)) %>% mutate(mass_sum = case_when(grid_rep == "250m" ~ mass_metre*250^2, grid_rep == "100m_n" ~ mass_metre*100^2, grid_rep == "100m_s" ~ mass_metre*100^2)) %>% group_by(class, grid_rep) %>% summarize(count_totals = sum(count_sum, na.rm = TRUE), mass_totals = sum(mass_sum, na.rm = TRUE))

summary_tab

## filter grid before groupwise mean

g_250 <- temp_cockle_df %>% filter(grid_rep %in% c("250m", "100m_n", "100m_s")) %>% filter(!is.na(mass_metre)) %>% filter(!is.na(count_metre))

## mass confidence intervals (Bca)

mass_conf <- groupwiseMean(mass_metre ~ class + grid_rep, data = g_250, conf = 0.95, digits = 3, R = 10000, boot = TRUE, traditional = FALSE, normal = FALSE, basic = FALSE, percentile = FALSE, bca = TRUE)

## count confidence intervals (Bca)

count_conf <- groupwiseMean(count_metre ~ class + grid_rep, data = g_250, conf = 0.95, digits = 3, R = 10000, boot = TRUE, traditional = FALSE, normal = FALSE, basic = FALSE, percentile = FALSE, bca = TRUE)

## mutate to calculate total mass confidence intervals

mass_conf %>% mutate(mass_lower = case_when(grid_rep == "250m" ~ Bca.lower * 250^2*sampled_250, grid_rep == "100m_n" ~ Bca.lower * 100^2*sampled_100n, grid_rep == "100m_s" ~ Bca.lower * 100^2*sampled_100s)) %>% mutate(mass_upper = case_when(grid_rep == "250m" ~ Bca.upper * 250^2*sampled_250, grid_rep == "100m_n" ~ Bca.upper * 100^2*sampled_100n, grid_rep == "100m_s" ~ Bca.upper * 100^2*sampled_100s)) %>% mutate(total_mean = case_when(grid_rep == "250m" ~ Mean * 250^2*sampled_250, grid_rep == "100m_n" ~ Mean * 100^2*sampled_100n, grid_rep == "100m_s" ~ Mean * 100^2*sampled_100s))

## mutate to calculate total count confidence intervals

count_conf %>% mutate(count_lower = case_when(grid_rep == "250m" ~ Bca.lower * 250^2*sampled_250, grid_rep == "100m_n" ~ Bca.lower * 100^2*sampled_100n, grid_rep == "100m_s" ~ Bca.lower * 100^2*sampled_100s)) %>% mutate(count_upper = case_when(grid_rep == "250m" ~ Bca.upper * 250^2*sampled_250, grid_rep == "100m_n" ~ Bca.upper * 100^2*sampled_100n, grid_rep == "100m_s" ~ Bca.upper * 100^2*sampled_100s)) %>% mutate(total_mean = case_when(grid_rep == "250m" ~ Mean * 250^2*sampled_250, grid_rep == "100m_n" ~ Mean * 100^2*sampled_100n, grid_rep == "100m_s" ~ Mean * 100^2*sampled_100s))

## filter for north confidence intervals

n_250 <- temp_cockle_df %>% filter(stations_rep %in% north_stations) %>% filter(sampled_rep == "Y") %>% filter(class %in% c("Y1", "Y2", "Y3", "Yt")) %>% filter(!is.na(mass_metre)) %>% filter(!is.na(count_metre))

## North 250 mass confidence intervals

mass_conf_n <- groupwiseMean(mass_metre ~ class + grid_rep, data = n_250, conf = 0.95, digits = 3, R = 10000, boot = TRUE, traditional = FALSE, normal = FALSE, basic = FALSE, percentile = FALSE, bca = TRUE)

## Mutate for total confidence limits

mass_conf_n %>% mutate(mass_lower = case_when(grid_rep == "250m" ~ Bca.lower * 250^2*sampled_250_n, grid_rep == "100m_n" ~ Bca.lower * 100^2*sampled_100n, grid_rep == "100m_s" ~ Bca.lower * 100^2*sampled_100s)) %>% mutate(mass_upper = case_when(grid_rep == "250m" ~ Bca.upper * 250^2*sampled_250_n, grid_rep == "100m_n" ~ Bca.upper * 100^2*sampled_100n, grid_rep == "100m_s" ~ Bca.upper * 100^2*sampled_100s)) %>% mutate(total_mean = case_when(grid_rep == "250m" ~ Mean * 250^2*sampled_250_n, grid_rep == "100m_n" ~ Mean * 100^2*sampled_100n, grid_rep == "100m_s" ~ Mean * 100^2*sampled_100s))

## North 250 count confidence intervals

count_conf_n <- groupwiseMean(count_metre ~ class + grid_rep, data = n_250, conf = 0.95, digits = 3, R = 10000, boot = TRUE, traditional = FALSE, normal = FALSE, basic = FALSE, percentile = FALSE, bca = TRUE)

## Mutate for total confidence limits

count_conf_n %>% mutate(count_lower = case_when(grid_rep == "250m" ~ Bca.lower * 250^2*sampled_250_n, grid_rep == "100m_n" ~ Bca.lower * 100^2*sampled_100n, grid_rep == "100m_s" ~ Bca.lower * 100^2*sampled_100s)) %>% mutate(count_upper = case_when(grid_rep == "250m" ~ Bca.upper * 250^2*sampled_250_n, grid_rep == "100m_n" ~ Bca.upper * 100^2*sampled_100n, grid_rep == "100m_s" ~ Bca.upper * 100^2*sampled_100s)) %>% mutate(total_mean = case_when(grid_rep == "250m" ~ Mean * 250^2*sampled_250_n, grid_rep == "100m_n" ~ Mean * 100^2*sampled_100n, grid_rep == "100m_s" ~ Mean * 100^2*sampled_100s))


## filter for south confidence intervals

s_250 <- temp_cockle_df %>% filter(stations_rep %in% south_stations) %>% filter(sampled_rep == "Y") %>% filter(class %in% c("Y1", "Y2", "Y3", "Yt")) %>% filter(!is.na(mass_metre)) %>% filter(!is.na(count_metre))

## South 250 mass confidence intervals

mass_conf_s <- groupwiseMean(mass_metre ~ class + grid_rep, data = s_250, conf = 0.95, digits = 3, R = 10000, boot = TRUE, traditional = FALSE, normal = FALSE, basic = FALSE, percentile = FALSE, bca = TRUE)

mass_conf_s %>% mutate(mass_lower = case_when(grid_rep == "250m" ~ Bca.lower * 250^2*sampled_250_s, grid_rep == "100m_n" ~ Bca.lower * 100^2*sampled_100n, grid_rep == "100m_s" ~ Bca.lower * 100^2*sampled_100s)) %>% mutate(mass_upper = case_when(grid_rep == "250m" ~ Bca.upper * 250^2*sampled_250_s, grid_rep == "100m_n" ~ Bca.upper * 100^2*sampled_100n, grid_rep == "100m_s" ~ Bca.upper * 100^2*sampled_100s)) %>% mutate(total_mean = case_when(grid_rep == "250m" ~ Mean * 250^2*sampled_250_s, grid_rep == "100m_n" ~ Mean * 100^2*sampled_100n, grid_rep == "100m_s" ~ Mean * 100^2*sampled_100s))

## South 250 count confidence intervals

count_conf_s <- groupwiseMean(count_metre ~ class + grid_rep, data = s_250, conf = 0.95, digits = 3, R = 10000, boot = TRUE, traditional = FALSE, normal = FALSE, basic = FALSE, percentile = FALSE, bca = TRUE)

count_conf_s %>% mutate(count_lower = case_when(grid_rep == "250m" ~ Bca.lower * 250^2*sampled_250_s, grid_rep == "100m_n" ~ Bca.lower * 100^2*sampled_100n, grid_rep == "100m_s" ~ Bca.lower * 100^2*sampled_100s)) %>% mutate(count_upper = case_when(grid_rep == "250m" ~ Bca.upper * 250^2*sampled_250_s, grid_rep == "100m_n" ~ Bca.upper * 100^2*sampled_100n, grid_rep == "100m_s" ~ Bca.upper * 100^2*sampled_100s)) %>% mutate(total_mean = case_when(grid_rep == "250m" ~ Mean * 250^2*sampled_250_s, grid_rep == "100m_n" ~ Mean * 100^2*sampled_100n, grid_rep == "100m_s" ~ Mean * 100^2*sampled_100s))

