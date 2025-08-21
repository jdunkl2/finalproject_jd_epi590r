install.packages("tidyverse")
install.packages("here")
install.packages("gtsummary")
install.packages("gtsummary", dependencies = TRUE)
install.packages("dplyr")
library(gtsummary)
library(tidyverse)
library(here)
library(dplyr)



penguins <- read_csv(here::here("data", "penguins_size.csv"))
penguins <- penguins %>%
	mutate(sex = na_if(sex, "."))


#Objective 1: Create a {gtsummary} table of descriptive statistics about
#your data (1 pt)

tbl_summary(
	penguins,
	by = species,
	include = c(sex, island,
							culmen_length_mm, culmen_depth_mm, flipper_length_mm),
	label = list(
		sex ~ "Sex",
		island ~ "Island",
		culmen_length_mm ~ "Culmen Length (mm)",
		culmen_depth_mm ~ "Culmen depth (mm)",
		flipper_length_mm ~ "Flipper Length (mm)"
	),
	statistic = list(all_continuous() ~ "{median} ({p25}, {p75})",
									all_categorical() ~ "{n} ({p}%)"),
	missing_text = "Missing") |>
	add_overall(col_label = "**Total**") |>
	bold_labels() |>
	modify_header(label = "**Variable**")




