library(tidyverse)
library(tidyquant)
library(readxl)
rr = read_excel("Documents/EDE+/Replication/Monetary_shocks/RRimport.xlsx")
#view(rr)
glimpse(rr)

#make dataset quarterly
rr_q<- rr %>%
  group_by(Year, Quarter) %>%
  summarize(
    mps_t = sum(RESID,na.rm = T),
    .groups = "drop"
  ) 
#view(rr_q)
glimpse(rr_q)

#Controls and FFR (fred data)

fred_vars <- c("GDPC1", "CPIAUCSL", "UNRATE", "FEDFUNDS")
macro_data <- tq_get(fred_vars, get = "economic.data", from = "1969-01-01", to = "2007-12-31")

#view(macro_data)

macro_q <- macro_data %>%
  mutate(
    Year = year(date),
    Quarter = quarter(date)
  ) %>% 
  group_by(symbol, Year, Quarter) %>%
  summarize(
    value = mean(price, na.rm = T),
    .groups = "drop"
  ) %>%
  pivot_wider(names_from = symbol, values_from = value) %>%
  mutate(
    log_gdp = log(GDPC1),
    log_cpi = log(CPIAUCSL)
  )

glimpse(macro_q)
#view(macro_q)

#merge datasets

base_df <- rr_q %>% inner_join(macro_q, by = c("Year", "Quarter")) %>% filter(Year >= 1969 & Year <= 1996)

view(base_df)
glimpse(base_df)


# bringing in innovation index data
library(readr)
patent_data <- read_csv("Documents/EDE+/Replication/KPSS_2024.csv")

# shrinking to quarterly estimates 
library(dplyr)
library(lubridate)

patent_data <- patent_data %>%
  mutate(issue_date = mdy(issue_date))
glimpse(patent_data)

patent_data_clean <- patent_data %>%
  filter(issue_date > ymd("1966-12-31"),
         issue_date < ymd("1998-12-31")) %>%
  mutate(quarter_date = floor_date(issue_date, unit = "quarter")) 

# organizing by quarter
patent_data_clean <- patent_data_clean %>%
  group_by(quarter_date) %>%
  summarise(
    xi_nominal_sum = sum(xi_nominal, na.rm = TRUE),
    xi_real_sum = sum(xi_real, na.rm = TRUE),
    total_cites = sum(cites, na.rm = TRUE),
    patent_count = n(),
  ) %>%
  arrange(quarter_date)

# creating variable that will allow merge with R&R data
patent_data_clean <- patent_data_clean %>%
  mutate(Year = year(quarter_date),
         Quarter = quarter(quarter_date))

full_df <- base_df %>%
  inner_join(patent_data_clean, by = c("Year", "Quarter"))

full_df <- full_df %>%
  arrange(Year, Quarter) %>%
  mutate(log_innovation = log(xi_real_sum))


