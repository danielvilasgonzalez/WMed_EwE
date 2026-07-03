# Load package
library(readxl)
library(dplyr)
library(ggplot2)

#wd
setwd('C:/Documents and Settings/danie/Desktop/iMARES/')

#catch File
file <- "./WMed EwE Model/FutureMares/New fitting/gfcm catch data/FAO-GFCM-CapturepProduction-1970_2020/FAO-GFCM-CapturepProduction-1970_2020.xlsx"
excel_sheets(file)
df1 <- read_excel(file, sheet = 1)
head(df1)
colnames(df1)


#unique(df$`Area (FAO subarea)`)

#west med
west_med <- df1 %>%
  filter(`Area (FAO subarea)` == "Western Med (37.1)" & Year >= 1995)
unique(west_med$Country)

#by year
west_med_year <- west_med %>%
  group_by(Year, Country,`Area (FAO division)`) %>%
  summarise(Catch = sum(Quantity, na.rm = TRUE),
            .groups = "drop")

#plot
ggplot(west_med_year,
       aes(x = Year,
           y = Catch,
           color = Country,
           group = Country)) +
  geom_line() +
  facet_wrap(~ `Area (FAO division)`,
             ncol = 1,
             scales = "free_y") +
  theme_minimal() +
  labs(y = "Catch (t)")

#fg file code
file <- "./WMed EwE Model/FutureMares/New fitting/FG_WMed.xlsx"
excel_sheets(file)
df2 <- read_excel(file, sheet = 4)
head(df2)


#check unmatching specise
unmatched_species <- df1 %>%
  anti_join(
    df2,
    by = c("Species (scientific name)" = "ESPECIE")
  ) %>%
  group_by(`Species (scientific name)`) %>%
  summarise(
    Catch = sum(Quantity, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  rename(
    Species = `Species (scientific name)`
  ) %>%
  arrange(desc(Catch)) %>%
  mutate(
    FG_num = NA,
    FG_name = NA
  ) %>%
  select(Species, Catch, FG_num, FG_name)

#save file
write.csv(
  unmatched_species,
  paste0(gitdir,"data/processed/unmatched_species_FG_assignment.csv"),
  row.names = FALSE
)