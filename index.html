---
title: "Investigating the Effect of Television Advertising on Newspaper Price and Content"
author: "Yinghan Zhang"
date: 2020/12/22
output:
  html_document: default
---
```{r, include=FALSE}
library(haven)
library(tidyverse)
library(kableExtra)
library(broom)
```

```{r, include=FALSE}
raw_data <- read_dta("Angelucci_Cage_AEJMicro_dataset.dta")
```
# Abstract
This report reproduces "Newspapers in Times of Low Advertising Revenues" by Angelucci Cagé, performs three regression models in investigating the effect on national and local newspaper before and after 1967, that is when introduction of television advertising is launched. This report looked that the price of newspapers in three aspects, advertising side, consumer side and productive quality. Results have showned that competition with television advertising has negatively affected newspapers price.


# Keywords
- Difference-in-difference
- Casual Inference
- Observational Study
- Newspaper
- Television Advertising


# Introduction
Newspaper are not as popular as before, lots of substitutions are coming out as technology developed, one big competitor for the survival of newspaper industry is the television, which competes in advertisement profit making. Is the closing of newspaper industry an effect of not being able to charge high price for their advertisement or people like to get news from other sources? This report will focus on the impact brought by television advertising by implementing difference in difference method to figure out to what extent newspapers content and price are affected.

This report refers to Angelucci and Cagé, 2019, and reproduce its main findings. Angelucci and Cagé, 2019, models the consequences on newspapers’ content and prices of a reduction in advertising revenues using difference in difference method. In their paper, they use a dataset of ‘French newspapers between 1960 and 1974’ using ‘a difference-in-differences analysis’ and exploit ‘the introduction of advertising on television’, for the reason that this change ‘affected national newspapers more severely than local ones’. They ‘find robust evidence of a decrease in the amount of journalistic-intensive content produced and the subscription price.’

Two data sets will be used to investigate how difference-in-difference could be used to make inference on the casuality between television advertisment and newspaper production. In the Methodology section, I describe the matching study, the data, and the model that was used to perform the diffference-in-difference analysis. Results of the difference-in-difference analysis are provided in the results section, and inferences of this data along with conclusions are presented in Conclusion section.


# Methodology
The dataset used in this report is based on French daily newspapers and French television built from historical records. This dataset contains annual data on local and national newspapers between 1960 and 1974, along with detailed information on television content. The below tables show variables key variables used in this analysis. The author (Angelucci and Cagé, 2019,) argue that negative shock brought by introduction of televison advertisement to newspaper advertising revenue is greater to national daily newspaper than local daily newspaper, so we use national newspapers as treatment group and local newspapers as control group.
```{r, message=FALSE, echo=FALSE}

data <- raw_data %>% select (year, id_news, after_national, local, national, # Diff in diff variables
                             ads_p4_cst, ln_ads_p4_cst, # advertising side  variables
                             ps_cst, ln_ps_cst,  # reader side  varibles
                             pages, ln_pages) %>%  #content variables
      mutate_at(vars(id_news, after_national, local, national), ~as.factor(.)) %>% 
      mutate(year = as.integer(year))
attach(data)

national_data <- data %>% 
  na.omit()%>%
  filter(national==1)%>%
  select(ps_cst, ads_p4_cst,pages) %>%
  summary()
kbl(national_data, caption = "Table 1: National Data")%>%
  kable_styling(bootstrap_options = "striped",full_width = F)
```
Table 1 displays variables in treatment group (national); 

```{r,echo=FALSE}
local_data <- data %>% 
  na.omit()%>%
  filter(national==0)%>%
  select(ps_cst, ads_p4_cst, pages) %>%
  summary()
kbl(local_data, caption = "Table 2: Local Data")%>%
  kable_styling(bootstrap_options = "striped",full_width = F)
```

Table 2 table displays variables in control group (local), where

* ps_cst is the subscription price per issue;
* ads_p4_cst is the display advertising rate (listed price);
* pages is the number of pages of the newspaper;

The analysis compare the change in advertising revenues of national daily newspapers to the change in advertising revenues of local daily newspapers from 1960 to 1974, using a difference-in-differences design.

The model that we are interested in estimating is:
  $$y_{n,t} = {\beta_0} + {\beta_1}(D_{After}*D_{National}) + \lambda_n + \gamma_t + \epsilon_{y,t}$$
where $y_{n,t}$ is the outcome variable, n indexes newspapers and t indexes years from 1960 to 1974. We used two indicator variables "After" and "National" that classify observations before and after 1967 and national and local newspapers. $\lambda_n$ is a fixed effect for each newspaper, and the $\gamma_t$ is a fixed effect for each year, as a dummy variable. $\epsilon_{y,t}$ is a newspaper-year shock. $\beta_1$ is the coefficient of interest, it measures the yearly effect after the introduction of television advertising on national newspapers compared to local newspapers. Note that in all models, we use the logarithms of the response variable.

This report employs three regression models, with same predictor variables but different response variables, namely subscription price, listed price, and number of newspapers pages.
We first investigate the effect of the introduction of advertising on television on the advertising side of the listed advertising price, then turn to prices on the reader side, which is subscription price and finally we consider the content quality of newspaper pre 1967 and post 1967, which is reflected in number of pages of newspapers in national and local.


# Results

```{r, include=FALSE,}
ad_price <- lm(ln_ads_p4_cst ~ after_national + id_news + year, data = data)
summary(ad_price)

subscription_price <- lm(ln_ps_cst ~ after_national + id_news + year, data = data)
summary(subscription_price)

number_of_pages <- lm(ln_pages ~ after_national + id_news + year, data = data)
summary(number_of_pages)

```

```{r, echo=FALSE}
output_ad_price <-summary(ad_price)$coefficients[2,c(1,4)]
output_subscription_price <-summary(subscription_price)$coefficients[2,c(1,4)]
output_number_of_pages <-summary(number_of_pages)$coefficients[2,c(1,4)]

output <- tibble(output_ad_price, output_subscription_price, output_number_of_pages)
adjusted_r <- c("0.8817", "0.8652", "0.9249")
output <- rbind(output,adjusted_r)
Response_Variable <- c("Estimates","P-value","Adjusted R^2")

output_YZ <- cbind(Response_Variable,output) %>% 
  rename("Response Variable"=Response_Variable, 
         "Listed Ad Price (advertising side)"=output_ad_price,
         "Subscription Price (reader side)"=output_subscription_price,
         "Number of pages (content quality)"=output_number_of_pages)
kable(output_YZ,  caption="Table 3: Regression Output") %>% 
  kable_styling(bootstrap_options = "striped", full_width = F)
```
Table 3 summarises regression outputs from the three regression models we performed. We find that after 1967, national newspapers listed advertising price decreased by 30.9%, national newspapers subscription price decreased by 3.8% and number of pages in national newspapers fell 2.2% approximately.

P-values for interaction term between year and national are below 0.05, indicating they are statiscally significant, p-values in model using number of pages of newspapers as response variable is greater than 0.05, indicating year and national are not statistically significant in predicting number of pages of national newspapers before and after 1967.


# Disscussion
This report investigate the effect of introduction of television advertising on newspapers price and quality using three regression models, looking from the advertising side, comsumer side and its own quality. Specifically, this report use the dataset Angelucci and Cagé (2019) created in their paper and studies the effect brought by television advertising before and after 1967, with respect to national and local newspapers. Results have shown that after 1967, national newspapers price has dropped by approximately 31% in advertising side and 4% in consumer side, this shows that television advertising has a great competence in advertising with newspapers, especially national newspapers. 

Decrease in newspapers price may not solely affected by the introduction of television advertisment. Also, as stated in Angelucci and Cagé's report, newspaper markets are oligopolies, but national newspaper and local newspaper differ in their degree of competition, we can use weighted least square regression.

# References
Angelucci, C., & Cagé, J. (2019). Newspapers in Times of Low Advertising Revenues. American Economic Journal: Microeconomics, 11(3), 319–364. 
  https://doi.org/10.1257/mic.20170306
  
Alexander, R. (2020, November). Difference in differences. Telling Stories With Data. https://www.tellingstorieswithdata.com/

