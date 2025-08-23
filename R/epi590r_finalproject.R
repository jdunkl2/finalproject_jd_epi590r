install.packages("tidyverse")
install.packages("here")
install.packages("gtsummary")
install.packages("gtsummary", dependencies = TRUE)
install.packages("dplyr")
install.packages("ggplot2")
library(gtsummary)
library(tidyverse)
library(here)
library(dplyr)
library(ggplot2)

#Importing and cleaning data
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

#Objective 2: Fit a regression and present well-formatted results from the
#regression (1 pt)
#The regression doesn’t have to be of any particular scientific interest,
#and you don’t have to interpret it in any particular way
#You may use {gtsummary} or {broom} or both

linear_model <- lm(flipper_length_mm ~ culmen_length_mm + culmen_depth_mm,
									 data = penguins)

summary(linear_model)

tbl_regression(
	linear_model,
	intercept = TRUE,
	label = list(
		culmen_length_mm ~ "Culmen Length (mm)",
		culmen_depth_mm ~ "Culmen depth (mm)"
	))

#For each 1-unit increase in culmen length, flipper length increases by 1.4
#if culmen depth is held constant. Significant (p < 0.001)

#Create a figure (1 pt)
#It doesn’t need to look pretty; feel free to adapt some of the examples from
#class, which were as simple as hist(data$variable) and as complicated as the
#forest plot in the {broom} section
#Feel free to look at some of the ggplot2 material from another course I taught

ggplot(penguins, aes(x=island, fill=species))+
	geom_bar(position="fill")
