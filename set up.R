library(readr)
library(dplyr)
library(ggplot2)

data <-
  read_csv(
    "https://www.opendata.nhs.scot/dataset/df10dbd4-81b3-4bfa-83ac-b14a5ec62296/resource/8654b6d8-9765-4ced-8e9b-4611aa4596eb/download/12.1_delivery.csv"
  )

ca_names <-
  read_csv(
    "https://www.opendata.nhs.scot/dataset/9f942fdb-e59e-44f5-b534-d6e17229cc7b/resource/967937c4-8d67-4f39-974f-fd58c4acfda5/download/geography_codes_and_labels_ca2011_01042019.csv"
  ) %>%
  janitor::clean_names() %>%
  group_by(ca2011) %>%
  summarise(hb2014 = first(hb2014name)) %>%
  ungroup()

data <- data %>%
  janitor::clean_names() %>%
  select(-simd_quintile_qf, -simd_version) %>% 
  filter(ca2011 != "RA2704") %>%
  left_join(ca_names, by = "ca2011")
  

write_rds(data, "data.rds")

