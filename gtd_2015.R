# 2015 update to GTD

setwd("~/stories/gtd_2015/")

require(dplyr)
require(readr)
require(tidyr)
require(ggplot2)
require(stringr)
require(lubridate)

# Load 2012-2015 data
raw_new_data <- read.csv("Global Terrorism Database 2012-2015 EMBARGOED.csv", stringsAsFactors = FALSE)

# Load 1970-2014 data (except for 1993)
raw_old_data <- data.frame()

for (f in list.files("./raw-data/")){
  data <- read_csv(paste0(getwd(), "/raw-data/", f))
  raw_old_data <- rbind(raw_old_data, data)
}

# Complete time series, 1970-2014
raw_old_data %>% 
  rename(year = iyear) %>% 
  group_by(year) %>% 
  summarize(attacks = n(),
            fatalities = sum(nkill, na.rm = TRUE)) %>% 
  gather(type, num, 2:3) %>% 
  ggplot(aes(x = year, y = num, group = type, color = type)) + 
  geom_line() + 
  ggtitle ("Terrorism Attacks and Fatalities, 1970-2014
           Note: data from 2012-2014 used different collection techniques")

# Latest data, 2012-2014
new_data <- raw_new_data %>%
  # filter(country_txt == "United States") %>% 
  rename(year = iyear) %>% 
  group_by(year) %>% 
  summarize(attacks = n()/1000,
            fatalities = sum(nkill, na.rm = TRUE)/1000) 

new_data %>% write_csv("new_total_attacks_fatalities.csv")

new_data %>% 
  gather(type, num, 2:3) %>% 
  ggplot(aes(x = year, y = num, fill=type, group=type)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  ggtitle ("Terrorism Attack and Fatalities, 2012-2014, in thousands")

# Incidents by region
data_by_region <- raw_new_data %>% 
  group_by(iyear, region_txt) %>% 
  summarize(attacks = n()/1000,
            fatalities = sum(nkill, na.rm = TRUE)/1000) %>% 
  filter(region_txt %in% c("Middle East & North Africa", "South Asia",
                           "Sub-Saharan Africa", "Western Europe",
                           "Eastern Europe", "North America")) 

data_by_region %>% select(-fatalities) %>% 
  spread(region_txt, attacks) %>% write_csv("attacks_by_region.csv")

data_by_region %>%
  ggplot(aes(x = iyear, y = fatalities, color = region_txt, group = region_txt)) +
  geom_line() +
  ggtitle("Terrorism attacks by region, 2012-2015")

# Most lethal attacks
lethal_attacks <- raw_new_data %>% 
  filter(iyear == 2015) %>% 
  arrange(desc(nkill)) %>% 
  select(iyear, imonth, iday, country_txt, city, nkill, nwound,
         targtype1_txt, gname, summary)

# Attacks table
attacks_table <- lethal_attacks %>% 
  mutate(date = ymd(str_c(iyear, imonth, iday, sep = "-")),
         location = str_c(city, country_txt, sep = ", ")) %>% 
  select(date, location, gname, nkill) %>% 
  rename(Date = date, Location = location, `Number killed` = nkill, `Group responsible` = gname) %>% 
  head(25)

attacks_table %>% write_csv("worst_attacks_table.csv")

# Most lethal attacks in North America or Western Europe
lethal_attacks_west <- raw_new_data %>% 
  filter(iyear == 2015, region_txt %in% c("Western Europe", "North America")) %>% 
  arrange(desc(nkill)) %>% 
  select(iyear, imonth, iday, country_txt, city, nkill, nwound,
         targtype1_txt, gname, summary)

# US attacks
us_attacks <- lethal_attacks %>% 
  filter(iyear == 2015, country_txt == "United States") %>% 
  mutate(date = ymd(str_c(iyear, imonth, iday, sep = "-"))) %>% 
  select(date, city, gname, nkill, summary) %>% 
  rename(Date = date, City = city, `Number killed` = nkill, `Group responsible` = gname)

# Country breakdown
data_by_country <- raw_new_data %>% 
  group_by(iyear, country_txt) %>% 
  summarize(attacks = n(), 
            fatalities = sum(nkill, na.rm = TRUE)) %>% 
  ungroup() %>% 
  arrange(desc(attacks)) %>% 
  filter(iyear == 2015)

# Worst groups
terror_by_groups <- raw_new_data %>% 
  filter(iyear == 2015) %>% 
  group_by(gname) %>% 
  summarize(attacks = n(),
            fatalities = sum(nkill, na.rm = TRUE)) %>% 
  ungroup() %>% 
  arrange(desc(attacks))

terror_by_groups %>% select(-attacks) %>% 
  head(11) %>% write_csv("deadliest_terror_groups.csv")

# Lone wolf attacks
lone_wolf <- raw_new_data %>% 
  filter(gname == "Unaffiliated Individual(s)" |
           gname == "Individual(s)") %>% 
  group_by(iyear, gname) %>% 
  summarize(attacks = n(),
            fatalities = sum(nkill, na.rm = TRUE))

# By attack types
raw_new_data %>% 
  group_by(iyear, attacktype1_txt) %>% 
  summarize(incidents = n()) %>% 
  spread(attacktype1_txt, incidents)

# By country
raw_new_data %>% 
  group_by(iyear, country_txt) %>% 
  summarize(incidents = n()) %>% 
  arrange(desc(incidents)) %>% 
  filter(incidents >= 38) %>% 
  ggplot(aes(x = iyear, y = incidents, group = country_txt, color = country_txt)) + 
  geom_line()

# Weapon type
raw_new_data %>% 
  group_by(iyear, weaptype1_txt) %>% 
  summarize(incidents = n()) %>% 
  spread(iyear, incidents)

# Target type
target_type <- raw_new_data %>% 
  group_by(iyear, targtype1_txt) %>% 
  summarize(incidents = n()) %>% 
  spread(iyear, incidents)