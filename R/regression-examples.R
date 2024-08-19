library(tidyverse)
library(gtsummary)
library(broom.helpers)

nlsy_cols <- c("glasses", "eyesight", "sleep_wkdy", "sleep_wknd",
							 "id", "nsibs", "samp", "race_eth", "sex", "region",
							 "income", "res_1980", "res_2002", "age_bir")
nlsy <- read_csv(here::here("data", "raw", "nlsy.csv"),
								 na = c("-1", "-2", "-3", "-4", "-5", "-998"),
								 skip = 1, col_names = nlsy_cols) |>
	mutate(region_cat = factor(region, labels = c("Northeast", "North Central", "South", "West")),
				 sex_cat = factor(sex, labels = c("Male", "Female")),
				 race_eth_cat = factor(race_eth, labels = c("Hispanic", "Black", "Non-Black, Non-Hispanic")),
				 eyesight_cat = factor(eyesight, labels = c("Excellent", "Very good", "Good", "Fair", "Poor")),
				 glasses_cat = factor(glasses, labels = c("No", "Yes")))


# Univariate regression

tbl_uvregression(
	nlsy,
	y = income,
	include = c(sex_cat, race_eth_cat,
							eyesight_cat, income, age_bir),
	method = lm)


tbl_uvregression(
	nlsy,
	y = glasses,
	include = c(sex_cat, race_eth_cat,
							eyesight_cat, glasses, age_bir),
	method = glm,
	method.args = list(family = binomial()),
	exponentiate = TRUE)

#Univar regression with x=sex_cat
tbl_uvregression(
	nlsy,
	x = sex_cat,
	include = c(nsibs, sleep_wkdy, sleep_wknd, income),
	method = lm)

## Multivariable regressions

## Some regressions

linear_model <- lm(income ~ sex_cat + age_bir + race_eth_cat,
									 data = nlsy)


linear_model_int <- lm(income ~ sex_cat*age_bir + race_eth_cat,
											 data = nlsy)


logistic_model <- glm(glasses ~ eyesight_cat + sex_cat,
											data = nlsy, family = binomial(link = "log"))

logistic_model_pois <- glm(income ~ sex_cat + race_eth_cat + age_bir,
											data = nlsy, family = poisson())

logistic_model_pois2 <- glm(glasses ~ eyesight_cat + sex_cat,
											data = nlsy, family = poisson(link = "log"))



## Tables

tbl_regression(
	linear_model,
	intercept = TRUE,
	label = list(
		sex_cat ~ "Sex",
		race_eth_cat ~ "Race/ethnicity",
		age_bir ~ "Age at first birth"
	))

# Risk ratio table for log
tbl_logBin <- tbl_regression(
	logistic_model,
	exponentiate = TRUE,
	label = list(
		sex_cat ~ "Sex",
		eyesight_cat ~ "Eyesight"
	))


tbl_no_int <- tbl_regression(
	linear_model,
	intercept = TRUE,
	label = list(
		sex_cat ~ "Sex",
		race_eth_cat ~ "Race/ethnicity",
		age_bir ~ "Age at first birth"
	))

tbl_int <- tbl_regression(
	linear_model_int,
	intercept = TRUE,
	label = list(
		sex_cat ~ "Sex",
		race_eth_cat ~ "Race/ethnicity",
		age_bir ~ "Age at first birth",
		`sex_cat:age_bir` ~ "Sex/age interaction"
	))

#Poisson table
tbl_regression(
	logistic_model_pois,
	exponentiate = TRUE,
	label = list(
		sex_cat ~ "Sex",
		race_eth_cat  ~ "Race/ethnicity",
		age_bir ~ "Age at birth"
	)
)


#Poisson-log table

tbl_logPois <- tbl_regression(
	logistic_model_pois2,
	exponentiate = TRUE,
	label = list(
		eyesight_cat ~ "Eyesight",
		sex_cat ~ "Sex"
	),
	tidy_fun = partial(tidy_robust, vcov = "HC1")
)

## Table comparing the models with and without interaction

tbl_merge(list(tbl_no_int, tbl_int),
					tab_spanner = c("**Model 1**", "**Model 2**"))

# Table comparing Log-binomial and log-Poisson

tbl_merge(list(tbl_logBin, tbl_logPois),
					tab_spanner = c("**Log-Binomial**", "**Log-Poisson**"))
