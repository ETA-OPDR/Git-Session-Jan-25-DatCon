
## Load some libraries to use

library(here)
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(opdR)

## Load the data

pet_data_path <- here("data", "raw", "household_pets.csv")
copy_from_SP(pet_data_path, office = "DASP")

pet_data <- read_csv(pet_data_path)
copy_to_SP(pet_data_path, office = "DASP")


## Clean the data

pet_data_clean <- pet_data %>%
  mutate(Category = trimws(Category)) %>%
  filter(Category %in% c("Have pets in household", "Total", "Dogs", "Cats")) %>%
  mutate(Estimate = as.numeric(Estimate)) %>%
  pivot_wider(names_from = "Category", values_from = "Estimate") %>%
  mutate('Cat' = (Cats / Total) *100) %>%
  mutate('Dog' = (Dogs / Total) *100) %>%
  mutate('Any Pet' = (`Have pets in household` / Total) *100) %>%
  select(Area, Cat, Dog, 'Any Pet') %>%
  pivot_longer(cols = c(Cat, Dog, 'Any Pet'), names_to = "pet_type", values_to = "pct") %>%
  mutate(pet_type = factor(pet_type, levels = c('Cat', 'Dog', 'Any Pet')),
         Area = factor(Area, levels = c('National', 'Texas', 'California', 'New York')))



ggplot(pet_data_clean, aes(fill=pet_type, y=pct, x=Area)) +
  geom_bar(position='dodge', stat='identity') +
  theme_minimal() +
  theme(plot.title = element_text(size=20, face='bold'),
        plot.subtitle = element_text(size=16, face='bold'),
        axis.title = element_blank(),
        legend.title = element_blank()) +
  scale_fill_manual('Position', values=c('orange', 'blue', 'gray')) +
  ggtitle('Do More HHouseholds Have a Dog or Cat?',
          subtitle = 'Percent of Households With a Pet by Pet Type') +
  labs(caption = 'Data Source: U.S. Census Bureau, American Housing Survey 2021')
