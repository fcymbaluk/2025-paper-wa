# Loading libraries

install.packages("tydiverse")
library(tidyverse)
library(magrittr)
library(dplyr)
library(tidyr)
library(tibble)


#' #' I use environmental, social and economic data to explore trends and patterns
#' of deforestation in the Amazon biome (my dependent variable) and possible
#' effects related to social and economic aspects (my independent variables).
#' The data is sourced from the datazoom.amazonia R package, which facilitates
#' access to official Brazilian Amazon data, including agriculture, deforestation,
#' production, etc. The package provides functions that download and pre-process
#' selected datasets. The datasets that I use are:
#' 
#' Environemntal:
#' 
#' - MapBiomas: A Brazilian project that provides annual land use and land cover
#' reports. The data is based on satellite images and provides information on the
#' Amazon biome, including deforestation and regeneration of vegetation.
#' Areas are measured in hectares (ha).
#' 
#' - Imazon: A Brazilian NGO that provides reports on deforestation in the Amazon
#' biome. The dataset Loads data categorizing each municipality by the level of
#' deforestation pressure it faces. The categories used by Imazon have three
#' levels, ranging from 0 to 3.
#' 
#' Social:
#' 
#' - IPS: The Social Progress Index (IPS) is a composite index that measures social
#' progress in Brazilian municipalities. Includes information on the overall social
#' progress score, as well as scores for specific components such sanitation, health,
#' and education. The data is available for 2014, 2018, 2021, and 2023.
#' 
#' Economic (issues when trying to load the data. To be fixed):
#' 
#' - PIB-Munic: The Brazilian Institute of Geography and Statistics (IBGE) provides
#' data of Brazilian municipalities on the Gross Domestic Product (GDP) at current
#' prices, taxes, net of subsidies on products, gross value added, total and by economic
#' activity, and respective shares. Data is available from 2002 to 2018.
#' 
#' - PAM: The Municipal Agricultural Production (PAM) is an annual survey conducted
#' by IBGE which provides on the production of crops in Brazilian municipalities.
#' Output is only included in the dataset if the planted area occupies over 1 acre.
#' The data available has a yearly frequency and is available from 1974 to the present.
#' 
#' - PPM: The Municipal Livestock Production (PPM) is an annual survey conducted by
#' IBGE which provides data on the production of livestock in Brazilian municipalities,
#' such inventories (e.g:cattle, pigs and hogs) and production (e.g: milk, eggs, honey).
#' The data available has a yearly frequency and is available from 1974 to the present.
#' 
#' - SIGMINE: The National Mining Agency (ANM) provides data on mining legally activities
#' in Brazilian municipalities. The data includes information on location, status,
#' product being mined and area in square meters etc

#' #' Cite as:
#'   Data Zoom (2023). Data Zoom: Simplifying Access To Brazilian Microdata.
#' https://www.econ.puc-rio.br/datazoom/english/index.html
#' 
#' Or if you prefer a BibTeX entry:
#'   @Unpublished{DataZoom2023,
#'     author = {Data Zoom},
#'     title = {Data Zoom: Simplifying Access To Brazilian Microdata},
#'     url = {https://www.econ.puc-rio.br/datazoom/english/index.html},
#'     year = {2023},
#'   }

install.packages("datazoom.amazonia")
library(datazoom.amazonia)


# First, I will download the Municipalities dataset and select the variables that interest
# to me, such as the municipality code, state, and whether the municipality is
# located within the Legal Amazon. I will also filter the data to include only the
# municipalities in the Legal Amazon. I doubble check the data to ensure that the
# variables are correctly loaded. I have got correctly 772 municipalities in the
# Legal Amazon (22 in the state of Acre, 16 in Amapá, 62 in Amazonas, 181 in Maranhão,
# 141 in Mato Grosso, 144 in Pará, 52 in Rondônia, 15 in Roraima and 139 in Tocantins).

data_municipalities <- datazoom.amazonia::municipalities %>% 
  select(code_muni, name_state, legal_amazon) %>% 
  filter(legal_amazon == 1)

glimpse(data_municipalities)

data_municipalities %>% group_by(name_state) %>% 
  summarize(n_distinct(code_muni))

data_municipalities <- data_municipalities %>% 
  select(-name_state) %>% 
  rename(municipality_code = code_muni)

#' I downloaded the Mapbiomas dataset and selected the variables that interest me,
#' such as the municipality code, state, year, deforestation class, and the value of
#' deforestation. The dataset contains six classes of land transition. I will focus on 
#' the data about primary vegetation (Veg. Primaria) and suppression of primary 
#' vegetation (Supressao Veg. Primaria). This choice is based on the fact that these
#' classes represent the most significant changes in land use and cover in the Amazon
#' and are directly related to the historic pattern of deforestation of the biome. As
#' The dataset brings information about four different levels of vegetation and vegetation 
#' suppression, I will sum the values of all those levels to get the total value of original
#' and supressed vegetation. I got data of primary vegetation and suppression of primary 
#' vegetation in the Amazon biome from 1987 to 2021. There are also data for 1986 and 2022, 
#' but just for primary vegetation, excluded the amount of suppression to these 2 years.

data_deforestation <- load_mapbiomas(dataset = "mapbiomas_deforestation_regeneration",
                    raw_data = FALSE,
                    geo_level = "municipality",
                    language = "eng"
                    )

glimpse(data_deforestation)

unique(data_deforestation$deforestation_class)

data_deforestation %>%
  count(deforestation_class)

data_deforestation <- data_deforestation %>%
    filter(deforestation_class %in% c("Supressao Veg. Primaria", "Veg. Primaria")) %>%
    select(-biome, -level_0, -level_1, -level_2, -level_3, -level_4) %>%
    group_by(municipality, municipality_code, state, year, deforestation_class) %>%
    summarise(total_value = sum (value, na.rm = TRUE), .groups = "drop")
  
glimpse(data_deforestation)

head(data_deforestation, n = 100) %>% 
  print(n = 100)

tail(data_deforestation)


#' I also downloaded the Imazon dataset and selected the variables related to deforestation
#' pressure at municipal level. 

data_pressure <- load_imazon(dataset = "imazon_shp",
                             raw_data = FALSE,
                             language = "eng"
                             )
glimpse (data_pressure)

head (data_pressure, n = 20) %>% 
  print(n = 20)

data_pressure <- data_pressure %>%
  select(-state)


#' For the social data, I downloaded the IPS dataset and selected variables related
#' to the social progress index, water and sanitation, environmental quality,
#' adequate sewage, adequate water supply, and elementary education quality. With 

data_ips <- load_ips(dataset = "all", 
                     raw_data = FALSE, 
                     time_period = c(2014, 2018, 2021, 2023)
                     )

glimpse(data_ips)

data_ips <- data_ips %>%
  select(municipality_code, year, ips_amazon, water_and_sanitation, 
         environment_quality, adaquate_sewage_percent_population, 
         adaquate_water_supply_percent_population, elementary_education_quality_ideb_0_to_10_scale,
         habitation_with_adequate_walls_percent, infant_mortality_until_5_years_deaths_1_000_live_births,
         malnutrition_percent_population, index_water_services_percent_population,
         violence_against_indigenous_people_scale_from_1_to_5)


#' For the economic data, I downloaded the PIB-Munic dataset, the The Municipal 
#' Agricultural Production (PAM) dataset, and the Municipal Livestock Production (PPM)
#' dataset. I selected the variables that interest me, such as the municipality code,
#' state, year, and the value of the Gross Domestic Product (GDP) at current prices,
#' taxes, net of subsidies on products, gross value added, total and by economic activity,
#' and respective shares. I also selected the variables related to the production of
#' crops and livestock in Brazilian municipalities. 
#' 

data_pibmunic <- load_pibmunic(
  raw_data = FALSE,
  geo_level = "municipality",
  time_period = 2006:2020
  ) %>% 
  filter(state %in% c("AC", "AP", "AM", "MA", "MT", "PA", "RO", "RR", "TO"))
)

glimpse(data_pibmunic)

data_pibmunic <- data_pibmunic %>%
  filter(deforestation_class %in% c("Supressao Veg. Primaria", "Veg. Primaria")) %>%
  select(-biome, -level_0, -level_1, -level_2, -level_3, -level_4) %>%
  group_by(municipality, municipality_code, state, year, deforestation_class) %>%
  summarise(total_value = sum (value, na.rm = TRUE), .groups = "drop")

data_crops <- load_pam(
  dataset = "all_crops",
  raw_data = FALSE,
  geo_level = "municipality",
  time_period = 2006:2021,
  language = "eng"
)

#' With the data loaded, I create a single dataset with the variables of interest to
#' the research. I merge the datasets using the municipality code as the key.

data_environ_social_econ <- data_municipalities %>%
  inner_join(data_deforestation, by = "municipality_code")

data_environ_social_econ %>% group_by(year) %>%
  summarize(n_distinct(municipality_code)) %>% 
  print(n=37)

data_environ_social_econ <- data_environ_social_econ %>%
  pivot_wider(names_from = deforestation_class, values_from = total_value) %>%
  rename(deforestation = `Supressao Veg. Primaria`, 
         primary_vegetation = `Veg. Primaria`)

#' Normalizing data:
#' 
#'Following Assunção and Rocha (2019), we use a normalized measure of the annual 
#'deforestation increase to smoothen the cross-sectional deforestation variation 
#'arising from municipality size heterogeneity. 

#'The Normalized variable was constructed according to the following expression:
#'
# "Normalized_incremento"it = ("Incremento"it − "mean_incremento") / "sd_incremento" 

stats_environ <- data_environ_social_econ %>%
  group_by(municipality_code) %>%
  summarize(mean_deforestation = mean(deforestation, na.rm = TRUE),
            sum_deforestation = sum(deforestation, na.rm = TRUE), 
            median_deforestation = median(deforestation, na.rm = TRUE),
            sd_deforestation = sd(deforestation, na.rm = TRUE), 
            .groups = "drop")

glimpse(stats_environ)

data_environ_social_econ <- data_environ_social_econ %>%
  left_join(stats_environ, by = "municipality_code")

data_environ_social_econ <- data_environ_social_econ %>%
  mutate(normalized_deforestation = (deforestation - mean_deforestation) / sd_deforestation)

data_environ_social_econ <- data_environ_social_econ %>%
  mutate(percentual_deforestation = (deforestation / primary_vegetation) * 100)

data_environ_social_econ <- left_join(data_environ_social_econ, data_pressure, 
                                       by = "municipality_code")

glimpse(data_environ_social_econ)

print(cbind(data_environ_social_econ$municipality.x, data_environ_social_econ$municipality.y))

data_environ_social_econ <- select(data_environ_social_econ, -municipality.x) %>% 
                            rename(municipality = municipality.y)

data_environ_social_econ <- left_join(data_environ_social_econ, data_ips, 
                                      by = c("municipality_code", "year"))

data_environ_social_econ <- data_environ_social_econ %>%
  mutate(year = as.numeric(year))

data_environ_social_econ <- left_join(data_environ_social_econ, data_ips, 
                                      by = c("municipality_code", "year"))

glimpse(data_environ_social_econ)

# I construct a PPCDAm list dataset that represents the municipalities that are part of the
# PPCDAm program. The PPCDAm is the Action Plan for Prevention and Control of Deforestation in
# the Legal Amazon. The dataset lists if a municipality is in or out of a government blocklist. 
# The value 0 in the ppcdam column represents that the municipality is out of the list. The value 
# 1 represents the municipality in. The dataset is available from 2004 to 2024.

library(readxl)
data_ppcdam <- read_excel("PPCDAm-list.xlsx")

str(data_ppcdam)

data_ppcdam <- data_ppcdam %>% 
  pivot_longer(cols = all_of(as.character(2004:2024)), 
               names_to = "year", 
               values_to = "ppcdam")

data_ppcdam <- data_ppcdam %>%
  mutate(year = as.numeric(year)) %>%
  rename(municipality = "Município") %>%
  select(-Estado)

glimpse(data_ppcdam)

data_environ_social_econ <- left_join(data_environ_social_econ, data_ppcdam, 
                                      by = c("municipality", "year"))

glimpse(data_environ_social_econ)

saveRDS(data_environ_social_econ, "data_environ_social_econ.rds")
write.csv(data_environ_social_econ, "data_environ_social_econ.csv", row.names = FALSE)
write_xlsx(data_environ_social_econ, "data_environ_social_econ.xlsx")

#' EDA
#' To do the Exploratory Data Analysis, I selected a time period from 2006 to 2021. 
#' It is due to the fact that I have more information about the variables of interest.
#' I will use the data_environ_social_econ dataset to explore the trends and patterns
#' of deforestation in the Amazon biome and possible effects related to social and
#' economic aspects. I will also explore the relationship between the variables and
#' the possible effects of the PPCDAm program on deforestation in the Amazon biome.

data_environ_social_econ <- data_environ_social_econ %>% 
  filter(year >= 2006 & year <= 2021) %>% 
  mutate(period = ifelse(year <= 2013, "2006-2013", "2014-2021")
  )

ggplot(data_environ_social_econ, aes(x = year, y = deforestation)) +
  geom_col() +
  labs(title = "Deforestation in the Amazon biome",
       x = "Year",
       y = "Deforestation (ha)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(data_environ_social_econ, aes(x = normalized_deforestation)) +
  geom_histogram(binwidth = 0.5, fill = "red", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Normalized Deforestation",
       x = "Normalized Deforestation",
       y = "Frequency") +
  theme_minimal()

ggplot(data_environ_social_econ, aes(x = normalized_deforestation)) +
  geom_density(fill = "blue", alpha = 0.5) +
  labs(title = "Density Plot of Normalized Deforestation",
       x = "Normalized Deforestation",
       y = "Density") +
  theme_minimal()

ggplot(data_environ_social_econ, aes(x = normalized_deforestation)) +
  geom_density(fill = "blue", alpha = 0.5) +
  facet_wrap(~period) +
  labs(title = "Density Plot of Normalized Deforestation",
       x = "Normalized Deforestation",
       y = "Density") +
  theme_minimal() +
  theme(strip.text = element_text(size = 12, face = "bold"))

#' We continued the EDA loocking for the top 10 municipalities with the biggest and
#' with the smallest deforestation rates

stats_environ_by_municipality_name <- data_environ_social_econ %>%
  group_by(municipality) %>%
  summarize(mean_deforestation = mean(deforestation, na.rm = TRUE),
            sum_deforestation = sum(deforestation, na.rm = TRUE), 
            median_deforestation = median(deforestation, na.rm = TRUE),
            sd_deforestation = sd(deforestation, na.rm = TRUE), 
            mean_normalized = mean(normalized_deforestation, na.rm = TRUE),
            .groups = "drop")

stats_environ_top_10_biggest_deforeestation <- stats_environ_by_municipality_name %>%
  arrange(desc(mean_deforestation)) %>%
  head(10)

stats_environ_top_10_smallest_deforeestation <- stats_environ_by_municipality_name %>%
  arrange(mean_deforestation) %>%
  head(10)

print(stats_environ_top_10_biggest_deforeestation)
print(stats_environ_top_10_smallest_deforeestation)

data_environ_social_econ <- data_environ_social_econ %>%
  mutate(deforestation_pressure_cat = cut(deforestation_pressure, 
                                          breaks = 4, 
                                          labels = c("Low", "Moderate", "High", "Very High")))

glimpse(data_environ_social_econ)

ggplot(data_environ_social_econ, aes(x = deforestation_pressure_cat, y = normalized_deforestation)) +
  geom_point(alpha = 0.7) +
  labs(title = "Scatter Plot of Deforestation Pressure vs Normalized Deforestation",
       x = "Deforestation Pressure",
       y = "Normalized Deforestation") +
  theme_minimal()                                          

ggplot(data_environ_social_econ, aes(x = deforestation_pressure_cat, y = normalized_deforestation)) +
  geom_boxplot() +
  facet_wrap(~period) +
  labs(title = "Box Plot of Normalized Deforestation Increment by Deforestation Pressure",
       x = "Deforestation Pressure",
       y = "Normalized Deforestation Increment") +
  theme_minimal()

# Effect of policies

data_environ_social_econ <- data_environ_social_econ %>%
  mutate(ppcdam_status = ifelse(ppcdam == 1, "PPCDAm", "Not PPCDAm"))

stats_environ_ppcdam <- data_environ_social_econ %>%
  group_by(year, ppcdam_status) %>%
  summarize(count = n())

print(stats_environ_ppcdam, n = 80)

# Drop rows with missing values in the 'PPCDAm' column

data_environ_social_econ_drop_not_ppcdam <- data_environ_social_econ %>%
  drop_na(ppcdam)


ggplot(data_environ_social_econ_drop_not_ppcdam, aes(x = deforestation_pressure_cat, y = normalized_deforestation)) +
  geom_boxplot() +
  facet_wrap(~period) +
  labs(title = "Box Plot of Normalized Deforestation Increment by Deforestation Pressure",
       x = "Deforestation Pressure",
       y = "Normalized Deforestation Increment") +
  theme_minimal()

ggplot(data_environ_social_econ_drop_not_ppcdam, aes(x = normalized_deforestation)) +
  geom_density(fill = "blue", alpha = 0.5) +
  facet_wrap(~period) +
  labs(title = "Density Plot of Normalized Deforestation",
       x = "Normalized Deforestation",
       y = "Density") +
  theme_minimal() +
  theme(strip.text = element_text(size = 12, face = "bold"))


ggplot(data_environ_social_econ_drop_not_ppcdam, aes(x = year, y = deforestation)) +
  geom_col() +
  labs(title = "Deforestation in the Amazon biome",
       x = "Year",
       y = "Deforestation (ha)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


stats_environ_ppcdam_drop_no_ppcdam <- data_environ_social_econ_drop_not_ppcdam %>%
  group_by(year, ppcdam_status) %>%
  summarize(count = n())

print(stats_environ_ppcdam_drop_no_ppcdam, n = 80)

ggplot(data_environ_social_econ_drop_not_ppcdam, aes(x = year, y = deforestation)) +
  geom_col() +
  labs(title = "Deforestation in the Amazon biome",
       x = "Year",
       y = "Deforestation (ha)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

stats_count_ppcdam_minics_by_year <- data_environ_social_econ_drop_not_ppcdam %>%
  group_by(year, ppcdam) %>%
  summarise(count = n(), .groups = "drop")

ggplot(stats_count_ppcdam_minics_by_year, aes(x = as.factor(year), y = count, fill = as.factor(ppcdam))) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("0" = "red", "1" = "blue"), labels = c("Excluded", "Included")) +
  labs(title = "Number of Municipalities in PPCDAM List by Year",
       x = "Year",
       y = "Number of Municipalities",
       fill = "PPCDAM Status") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(data_environ_social_econ_drop_not_ppcdam, aes(x = normalized_deforestation, fill = as.factor(ppcdam_status))) +
  geom_histogram(binwidth = 0.7, alpha = 0.7, position = "identity") +
  facet_wrap(~period, scales = "free") +
  labs(title = "Histogram of Normalized Deforestation",
       x = "Normalized Deforestation",
       y = "Frequency",
       fill = "PPCDAM Status") +
  theme_minimal()


ggplot(data_environ_social_econ_drop_not_ppcdam, aes(x = normalized_deforestation, color = as.factor(ppcdam_status))) +
  geom_density(size = 1) +
  facet_wrap(~period, scales = "free") +
  labs(title = "Density of Normalized Deforestation",
       x = "Normalized Deforestation",
       y = "Frequency",
       color = "PPCDAM Status") + 
  theme_minimal()


ggplot(data_environ_social_econ_drop_not_ppcdam, aes(x = factor(ppcdam_status), y = normalized_deforestation, fill = factor(ppcdam_status))) +
  geom_violin(alpha = 0.5) +
  facet_wrap(~ period) +
  labs(title = "Deforestation Rates by PPCDAM Status",
       x = "PPCDAM Status",
       y = "Normalized Deforestation",
       fill = "PPCDAM") +
  theme_minimal()


ggplot(data_environ_social_econ_drop_not_ppcdam, aes(x = factor(ppcdam_status), y = normalized_deforestation, fill = factor(ppcdam_status))) +
  geom_boxplot(alpha = 0.5) +
  facet_wrap(~ period) +
  labs(title = "Deforestation Rates by PPCDAM Status",
       x = "PPCDAM Status",
       y = "Normalized Deforestation",
       fill = "PPCDAM") +
  theme_minimal()


ggplot(data_environ_social_econ_drop_not_ppcdam, aes(x = factor(period), y = normalized_deforestation, fill = factor(period))) +
  geom_boxplot(alpha = 0.5) +
  labs(title = "Comparison of Deforestation Rates by Period",
       x = "Period",
       y = "Normalized Deforestation",
       fill = "Period") +
  scale_fill_manual(values = c("2006-2013" = "blue", "2014-2021" = "red")) +
  theme_minimal()

ggplot(data_environ_social_econ_drop_not_ppcdam, aes(x = year, y = normalized_deforestation, color = factor(ppcdam), group = factor(ppcdam))) +
  geom_line(stat = "summary", fun = mean, size = 1) +
  labs(title = "Annual Deforestation Trends",
       x = "Year",
       y = "Average Normalized Deforestation",
       color = "PPCDAM Status") +
  scale_color_manual(values = c("0" = "blue", "1" = "red"), labels = c("Not Listed", "Listed")) +
  theme_minimal()

glimpse(data_environ_social_econ_drop_not_ppcdam)

# I calculate the trend of normalized deforestation increment for each municipality
stats_ppcdam_trends <- data_environ_social_econ_drop_not_ppcdam %>%
  group_by(municipality, ppcdam) %>%  
  summarise(trend = cor(year, normalized_deforestation, use = "complete.obs"), .groups = 'drop') %>%  
  filter(!is.na(trend)) %>%
  arrange(desc(trend)) %>%
  mutate(trend = round(trend, 2))

ggplot(stats_ppcdam_trends, aes(x = reorder(municipality, trend), y = trend, fill = factor(ppcdam))) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Trend of Normalized Deforestation Increment by Municipality",
       x = "Municipality",
       y = "Trend (Correlation)",
       fill = "PPCDAM") +
  theme_minimal() +
  scale_fill_manual(values = c("0" = "lightsalmon", "1" = "royalblue"),
                    labels = c("0" = "Monitored", "1" = "Prioritized"))

saveRDS(data_environ_social_econ_drop_not_ppcdam, "data_environ_social_econ_drop_not_ppcdam.rds")
write.csv(data_environ_social_econ_drop_not_ppcdam, "data_environ_social_econ_drop_not_ppcdam.csv", row.names = FALSE)
library(writexl)
write_xlsx(data_environ_social_econ_drop_not_ppcdam, "data_environ_social_econ_drop_not_ppcdam.xlsx")
