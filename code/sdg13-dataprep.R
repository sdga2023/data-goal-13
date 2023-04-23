library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(countrycode)
library(biscale)
library(WDI)
library(lubridate)

countries <- wbstats::wb_countries()

# Disasters
dis.raw <- read_excel("../inputs/emdat_public_2022_11_03_query_uid-AfoBx5.xlsx", skip = 6)
dis <- select(dis.raw, Year, `Disaster Subgroup`, `Disaster Type`) %>%
  mutate(Year = as.numeric(Year)) %>%
  filter(!`Disaster Type` %in% c("Animal accident", "Insect infestation", "Glacial lake outburst")) %>%
  filter(!`Disaster Type` %in% c("Earthquake", "Epidemic", "Landslide", "Mass movement (dry)", "Volcanic activity", "Impact" )) %>%
  filter(Year > 1969) %>%
  filter(Year < 2022)

dis.agg <- group_by(dis, Year, `Disaster Type`) %>%
  summarise(number = n()) %>%
  rename(year = Year, type = `Disaster Type`) %>%
  mutate(type = tolower(type)) %>%
  mutate(type = recode(type,
                       `extreme temperature` = "temperature",
                       `mass movement (dry)` = "mass",
                       `volcanic activity` = "volcanic"
  ))
write.csv(dis.agg, file="../outputs/disasters.csv", row.names = FALSE)

dis.damage <- filter(dis.raw, `Disaster Subgroup` %in% c("Climatological", "Hydrological", "Meteorological")) %>%
  mutate(year = as.numeric(Year),
         deaths = ifelse(is.na(`Total Deaths`), 0, as.numeric(`Total Deaths`)),
         affected = ifelse(is.na(`Total Affected`), 0, as.numeric(`Total Affected`)),
         damage = ifelse(is.na(`Total Damages, Adjusted ('000 US$)`), 0, as.numeric(`Total Damages, Adjusted ('000 US$)`))) %>%
  filter(year < 2022) %>%
  mutate(victims =  deaths + affected ) %>%
  select(year, damage, victims, deaths) %>%
  group_by(year) %>%
  summarise(across(c(deaths, victims, damage), sum, na.rm=TRUE))

write.csv(dis.damage, file="../outputs/disasters_damage.csv", row.names = FALSE)

# Historical temperatures
temp.0.2000 <- read.csv('../inputs/SPM1_1-2000.csv')
colnames(temp.0.2000) <- c("year", "temp", "x5", "x95")
temp.0.2000$series <- "reconstructed"
# Observed temperatures
temp.1850.2020 <- read.csv('../inputs/SPM1_1850-2020_obs.csv')
temp.1850.2020$x5 <- temp.1850.2020$temp
temp.1850.2020$x95 <- temp.1850.2020$temp
temp.1850.2020$series <- "observed"

temperatures <- rbind(temp.0.2000, temp.1850.2020)
write.csv(temperatures, file="../outputs/temperature.csv", row.names = FALSE)

# GHG by sector and by gas
ghg.by.sector <- read_excel("../inputs/data-sdg13.xlsx", sheet="GHG by sector")
write.csv(ghg.by.sector, file="../outputs/ghg_sectors.csv", row.names = FALSE)

ghg.by.gas.sector <- read_excel("../inputs/data-sdg13.xlsx", sheet="CO2 and CH4 by sector") %>%
  select(gas = Gas, sector = Sector, subsector = `Sub-sector`, mtco2e = `2019`) %>%
  mutate(share = round(mtco2e/sum(ghg.by.sector$mtco2e)*100, 1))
write.csv(ghg.by.sector, file="../outputs/gas_sectors.csv", row.names = FALSE)

# CAIT emissions data from climatewatch
# pop <- WDI(
#   country = 'all',
#   indicator=c('SP.POP.TOTL'),
#   start=2019,
#   end=2019,
#   extra=TRUE
# ) %>%
#   filter(region!="Aggregates") %>%
#   rename(date=year) %>%
#   select(iso3c, pop = SP.POP.TOTL)
# 
# cait.raw <- read.csv("ghg-emissions-total.csv")
# cait <- select(cait.raw, country = Country.Region, emissions = X2019) %>%
#   mutate(iso3c = countrycode(country, origin = "country.name", destination = "iso3c")) %>%
#   filter(!is.na(iso3c), emissions != "false") %>%
#   select(iso3c, emissions) %>%
#   left_join(pop, by = "iso3c") %>%
#   mutate(emispercap = emissions*1000000/pop) %>%
#   arrange(-emispercap) %>%
#   mutate(cumpop = cumsum(pop)) %>%
#   filter(!is.na(pop))
# 
# emis.world <- sum(cait$emissions)
# pop.world <- sum(cait$pop)
# emispercap.world <- emis.world*1000000/pop.world
# world.emissions <- data.frame("WLD", emis.world, pop.world, emispercap.world, 0)
# names(world.emissions) <- c("iso3c", "emissions", "pop", "emispercap", "cumpop")
# 
# countries.income <- select(countries, iso3c, income_level_iso3c)
# emis.income <- left_join(cait, countries.income, by="iso3c") %>%
#   group_by(income_level_iso3c) %>%
#   summarise(emissions = sum(emissions), pop = sum(pop)) %>%
#   filter(income_level_iso3c != "INX") %>%
#   mutate(emispercap = emissions*1000000/pop) %>%
#   arrange(-emispercap) %>%
#   mutate(cumpop = cumsum(pop)) %>%
#   rename(iso3c = income_level_iso3c)
# 
# cait <- rbind(cait, world.emissions, emis.income)

emispop.income.raw <- read_excel("../inputs/data-sdg13.xlsx", sheet="GHG CO2 CH4 by income group")
emispop.income <- select(emispop.income.raw, 1, 5:11) %>%
  filter(iso3c != "others")
colnames(emispop.income) <- c("iso3c", "pop", "ghg", "co2", "ch4", "ghgpercap", "co2percap", "ch4percap")

#emispop.world <- filter(emispop.income, iso3c == "WLD")
emispop.income <- arrange(emispop.income, -ghgpercap) %>%
  mutate(cumpop_ghg = cumsum(pop)) %>%
  arrange(-co2percap) %>%
  mutate(cumpop_co2 = cumsum(pop))%>%
  arrange(-ch4percap) %>%
  mutate(cumpop_ch4 = cumsum(pop))

emispop.countries.raw <- read_excel("../inputs/data-sdg13.xlsx", sheet="GHG CO2 CH4 by country")
emispop.countries <- mutate(emispop.countries.raw, iso3c = countrycode(Country, origin = "country.name", destination = "iso3c")) %>%
  filter(Country != "others") %>%
  mutate(iso3c = if_else(Country == "Türkiye", "TUR", iso3c)) %>%
  select(5:12)

colnames(emispop.countries) <- c("pop", "ghg", "co2", "ch4", "ghgpercap", "co2percap", "ch4percap", "iso3c")
emispop.countries <-relocate(emispop.countries, iso3c) %>%
  filter(!is.na(ghg)) %>%
  arrange(-ghgpercap) %>%
  mutate(cumpop_ghg = cumsum(pop)) %>%
  arrange(-co2percap) %>%
  mutate(cumpop_co2 = cumsum(pop))%>%
  arrange(-ch4percap) %>%
  mutate(cumpop_ch4 = cumsum(pop))

emispop.all <- rbind(emispop.countries, emispop.income)

write.csv(emispop.all, file="../outputs/emissions_population.csv", row.names = FALSE)

# LULUCF
lulu.raw <- read_excel("../inputs/data-sdg13.xlsx", sheet = "co2 production v lulucf")
lulu <- select(lulu.raw, country = Country, production = `CO2 production-based emissions`, lulucf = LULUCF) %>%
  mutate(iso3c = countrycode(country, origin = "country.name", destination = "iso3c")) %>%
  filter(!is.na(iso3c))
countries.regions <- select(countries, iso3c, region_iso3c)
lulu.regions <- left_join(lulu, countries.regions, by="iso3c") %>%
  group_by(region_iso3c) %>%
  summarise(production = sum(production), lulucf = sum(lulucf)) %>%
  filter(!is.na(region_iso3c))
write.csv(lulu.regions, file="../outputs/lulucf.csv", row.names = FALSE)

# Alluvial
netzero.raw <- read_excel("../inputs/net_zero.xlsx", sheet="comprehensive_table")
netzero <- select(netzero.raw, income = 'income group', status = net_zero_status, targetyear = target_year, ghgshare = `share of GHG emissions (2019)`) %>%
  mutate(ghgshare = ifelse(is.na(ghgshare), 0,ghgshare))

netzero.links.1 <- group_by(netzero, income, status) %>%
  summarise(ghgshare = sum(ghgshare*100)) %>%
  rename(source = income, target = status, value = ghgshare)

netzero.links.2 <- group_by(netzero, status, targetyear) %>%
  summarise(ghgshare = sum(ghgshare*100)) %>%
  rename(source = status, target = targetyear, value = ghgshare)

netzero.links <- rbind(netzero.links.1, netzero.links.2) %>%
  mutate(source = recode(source,
                         "High income" = "HIC",
                         "Upper middle income" = "UMC",
                         "Lower middle income" = "LMC",
                         "Low income" = "LIC",
                         "In Law" = "law",
                         "In Policy Document" = "policy",
                         "In Political Pledge" = "pledge",
                         "No Document Submitted" = "nodoc",
                         "Unknown" = "unknown"
  )) %>%
  mutate(target = recode(target, "In Law" = "law",
                         "In Policy Document" = "policy",
                         "In Political Pledge" = "pledge",
                         "No Document Submitted" = "nodoc",
                         "Already achieved" = "achieved")) %>%
  mutate(target = ifelse(is.na(target), "notarget", target)) %>%
  mutate(source = factor(source, levels = c("HIC", "UMC", "LMC", "LIC", "unknown", "law", "policy", "pledge", "nodoc"))) %>%
  mutate(target = factor(target, levels = c("law", "policy", "pledge", "nodoc", "achieved", "≥2030", "≥2040", "≥2050", "notarget"))) %>%
  arrange(source, desc(target))
write.csv(netzero.links, file="../outputs/netzero.csv", row.names = FALSE)

# Readiness vs vulnerability
ready <- read.csv("../inputs/readiness.csv")
vuln <- read.csv("../inputs/vulnerability.csv")

ready.vuln <- left_join(select(ready, ISO3, X2020), select(vuln, ISO3, X2020), by="ISO3")
colnames(ready.vuln) <- c("iso3c", "readiness", "vulnerability")
ready.vuln <- filter(ready.vuln, !is.na(readiness)) %>%
  filter(!is.na(vulnerability))
ready.vuln <- bi_class(ready.vuln, x = vulnerability, y = readiness, style = "quantile", dim = 3)

ready.vuln <- mutate(ready.vuln, vulnerability_class = case_when(
  substr(bi_class,1,1) == "1" ~ "low",
  substr(bi_class,1,1) == "2" ~ "medium",
  substr(bi_class,1,1) == "3" ~ "high")) %>%
  mutate(ready.vuln, readiness_class = case_when(
    substr(bi_class,3,3) == "1" ~ "low",
    substr(bi_class,3,3) == "2" ~ "medium",
    substr(bi_class,3,3) == "3" ~ "high"
  ))
write.csv(ready.vuln, file="../outputs/readiness_vulnerability.csv", row.names = FALSE)

# Ukraine war impact
impact.raw <- read_excel("../inputs/data-sdg13.xlsx", sheet = "Impact of rus-ukr on EU")
impact <- select(impact.raw, -`2023`) %>%
  mutate(month = month(date3)) %>%
  mutate(`2019` = as.numeric(`2019`)) %>%
  mutate(`2021` = as.numeric(`2021`)) %>%
  mutate(`2022` = as.numeric(`2022`)) %>%
  filter(!is.na(`2019`)) %>%
  group_by(month) %>%
  summarise(`2019` = sum(`2019`), `2020` = sum(`2020`), `2021` = sum(`2021`), `2022` = sum(`2022`)) %>%
  mutate(upper = pmax(`2019`, `2020`, `2021`)) %>%
  mutate(lower = pmin(`2019`, `2020`, `2021`))
write.csv(impact, file="../outputs/impact_ukraine.csv", row.names = FALSE)

ggplot(impact, aes(month)) +
  geom_ribbon(aes(ymin = lower, ymax = upper)) +
  geom_path(aes(y = `2022`), colour = "red") +
  theme_minimal()

# Globe
pop <- WDI(
  country = 'all',
  indicator=c('SP.POP.TOTL'),
  start=2019,
  end=2019,
  extra=TRUE
) %>%
  filter(region!="Aggregates" | iso3c == "WLD") %>%
  rename(date=year) %>%
  select(iso3c, pop = SP.POP.TOTL)
emis <- WDI(
  country = 'all',
  indicator=c('EN.ATM.GHGT.KT.CE'),
  start=2019,
  end=2019,
  extra=TRUE
) %>%
  filter(region!="Aggregates"| iso3c == "WLD") %>%
  rename(date=year) %>%
  select(iso3c, emis = EN.ATM.GHGT.KT.CE)
pcapemis <- left_join(emis, pop, by = 'iso3c') %>%
  mutate(pcapemis = round(emis*1000/pop, 1)) %>%
  filter(!is.na(emis))
write.csv(pcapemis, file="../outputs/emissions_globe.csv", row.names = FALSE)

## Everything below this is not used anymore, and can be deleted, I think ##
# GHG emissions
emis <- read_excel('ipcc_ar6_figure_spm_1_archive.xlsx', sheet='panel_a')
colnames(emis) <- c('gas', 'year', 'value')
emis.wide <- pivot_wider(emis, names_from = gas, values_from = value)
colnames(emis.wide) <- c("year", "CO2_FFI", "CO2_LULUCF", "CH4", "N2O", "FGAS")
sheet_write(emis.wide, "1bvAqLRcYsXQb8aS2NfEQjA8aZPddoJSOwSMfFfZbaE4", sheet = "emissions")

# Figure 3, scenarios. What model and scenario to use?
scenarios <- read.csv('export.csv')
scenarios.4sheets <- filter(scenarios,
                            model == "MESSAGEix-GLOBIOM 1.1_downscaled",
                            scenario %in% c("Current Policies ", "Nationally Determined Contributions (NDCs) ", "Net Zero 2050")) %>% 
  mutate(scenario = recode(scenario, `Current Policies ` = "curpol", `Nationally Determined Contributions (NDCs) ` = "ndcs", `Net Zero 2050` = "netzero50")) %>%
  select(-unit, -model, -variable) %>%
  filter(!is.na(Data))

colnames(scenarios.4sheets) <- c("scenario", "year", "value", "iso3c")
sheet_write(scenarios.4sheets, "1bvAqLRcYsXQb8aS2NfEQjA8aZPddoJSOwSMfFfZbaE4", sheet = "scenarios")

# Commitments
commit.raw <- read.csv("net_zero_content_cleaned.csv", sep=";")
commit <- select(commit.raw, iso3c = Country, status = Value) %>%
  mutate(iso3c = countrycode(iso3c, origin = "country.name", destination = "iso3c"))

commit.4sheets <- left_join(select(emits.4sheets, iso3c, emissions), commit, by = "iso3c")
commit.4sheets[is.na(commit.4sheets$status),]$status <- "No document submitted"
sheet_write(commit.4sheets, ss = "1bvAqLRcYsXQb8aS2NfEQjA8aZPddoJSOwSMfFfZbaE4", sheet="net_zero_status")
