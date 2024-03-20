## Kaggle credit card example

# Original data obtained from
# https://www.kaggle.com/datasets/arjunbhasin2013/ccdata

# This case requires to develop a customer segmentation to define marketing
# strategy. The sample Dataset summarizes the usage behavior of about 9000
# active credit card holders during the last 6 months. The file is at a customer
# level with 18 behavioral variables.

# CUST_ID : Identification of Credit Card holder (Categorical)
# BALANCE : Balance amount left in their account to make purchases (
# BALANCE_FREQUENCY : How frequently the Balance is updated, score between 0 and 1 (1 = frequently updated, 0 = not frequently updated)
# PURCHASES : Amount of purchases made from account
# ONEOFF_PURCHASES : Maximum purchase amount done in one-go
# INSTALLMENTS_PURCHASES : Amount of purchase done in installment
# CASH_ADVANCE : Cash in advance given by the user
# PURCHASES_FREQUENCY : How frequently the Purchases are being made, score between 0 and 1 (1 = frequently purchased, 0 = not frequently purchased)
# ONEOFFPURCHASESFREQUENCY : How frequently Purchases are happening in one-go (1 = frequently purchased, 0 = not frequently purchased)
# PURCHASESINSTALLMENTSFREQUENCY : How frequently purchases in installments are being done (1 = frequently done, 0 = not frequently done)
# CASHADVANCEFREQUENCY : How frequently the cash in advance being paid
# CASHADVANCETRX : Number of Transactions made with "Cash in Advanced"
# PURCHASES_TRX : Numbe of purchase transactions made
# CREDIT_LIMIT : Limit of Credit Card for user
# PAYMENTS : Amount of Payment done by user
# MINIMUM_PAYMENTS : Minimum amount of payments made by user
# PRCFULLPAYMENT : Percent of full payment paid by user
# TENURE : Tenure of credit card service for user

library(tidyverse)
library(tidymodels)

dat <- read_csv("CC GENERAL.csv.gz")
dat
glimpse(dat)
summary(dat)

## Scale all variables to have 0 mean and unit SD
kdat <- dat |>
    select(-CUST_ID) |>
    ## Replace NA values in every column with their (non-missing) column mean
    mutate(across(everything(),
                  ~ replace_na(.x, mean(.x, na.rm = TRUE)))) |> 
    ## Replace values in every column with their standardized deviation
    mutate(across(everything(), 
                  ~ (.x - mean(.x)) / sd(.x)))
kdat
summary(kdat)

kdat |> 
    pivot_longer(everything()) |> 
    ggplot(aes(x = value)) + 
    geom_histogram(bins = 10) +
    facet_wrap(vars(name))

kdat |> 
    pivot_longer(everything()) |> 
    ggplot(aes(x = value)) + 
    geom_histogram(bins = 10) +
    facet_wrap(vars(name), 
               scales = "free_x")

kdat

kresult <- kmeans(kdat, 5)
kresult
kresult |> 
    glimpse()

kresult |> 
    tidy() |> 
    select(-size, -withinss) |> 
    pivot_longer(cols = -cluster,
                 names_to = "Variable",
                 values_to = "cluster_mean") |>
    ggplot(aes(x = Variable, y = cluster_mean)) +
    geom_col() +
    facet_wrap(vars(cluster)) +
    coord_flip()


kresult |> 
    augment(dat) |> 
    ggplot(aes(PURCHASES, BALANCE)) +
    geom_point(aes(color = factor(.cluster))) 
