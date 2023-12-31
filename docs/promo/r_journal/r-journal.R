# Generated by `rjournal_article()` using `knitr::purl()`: do not edit by hand
# Please edit r_journal.Rmd to modify this file

## ----setup_rjournal, include=F------------------------------------------------
knitr::opts_chunk$set(message = F, 
                      warning = F, 
                      comment = "    ", 
                      out.width = "69%")
kabel_format <- "latex"


library(magrittr)
readLines("../../../README.Rmd")  %>%  .[119:length(.)] %>% 
  writeLines("../../../readme_extract.Rmd")


## ----readin_readmeextract, child = "../../../readme_extract.Rmd"--------------

## ----manipulation_status_quo--------------------------------------------------
library(tidyverse)
data.frame(ind_spam = c(TRUE, TRUE, FALSE, FALSE, TRUE)) %>% 
  mutate(cat_spam = ifelse(ind_spam, "spam", "not spam"))


## ----visual_status_quo--------------------------------------------------------
tidytitanic::passengers %>% 
ggplot() + 
  aes(x = age) + 
  geom_histogram() + 
  facet_grid(~ ifelse(survived, 
                      "survived", 
                      "not survived")) 


## ----visual_status_quo_order--------------------------------------------------
data.frame(ind_grad = c(T, F, T, T)) %>% 
  ggplot() + 
  aes(x = fct_rev(ifelse(ind_grad, "grad", "not grad"))) +
  geom_bar()



## ----direct_table_awkward-----------------------------------------------------
tidytitanic::passengers %>% 
  count(survived) 


## ----direct_table_loss--------------------------------------------------------
tidytitanic::passengers %>% 
  janitor::tabyl(sex, survived)


## ----direct_visual_awkward, fig.cap="A. Bar labels + axis label preserves information but is awkward"----
library(tidyverse)

tidytitanic::passengers %>% 
  ggplot() + 
  aes(x = survived) + 
  geom_bar()


## ----direct_visual_loss, fig.cap = "D. Facetting directly on an indicator variable with popular ggplot2 results in information loss"----
tidytitanic::passengers %>% 
ggplot() + 
  aes(x = age) + 
  geom_histogram() + 
  facet_grid(~ survived) 


## ----manipulation_status_quo_reprise, eval = T, message=F, warning=F----------
library(tidyverse)

data.frame(ind_graduated = 
             c(TRUE, TRUE, FALSE))  %>% 
  mutate(cat_graduated  = 
           ifelse(ind_graduated, 
                  "graduated", 
                  "not graduated"))  %>% 
  mutate(cat_graduated = 
           fct_rev(cat_graduated)
         )  


## ----manipulation_ind2cat, eval = T, message=F, warning=F---------------------
library(ind2cat)

data.frame(ind_graduated = 
             c(TRUE, TRUE, FALSE)) %>% 
  mutate(cat_graduated  = 
           ind_recode(ind_graduated)
         )


## ----manipulation_ind2cat_custom----------------------------------------------
data.frame(ind_graduated = c(T,T,F)) %>% 
  mutate(cat_graduated  = ind_recode(ind_graduated, 
                                     cat_false = "current"))


## ----manipulation_ind2cat_negator---------------------------------------------
tibble(ind_grad = c(T,T,F)) %>%
  mutate(cat_grad  = ind_recode(ind_grad, negator = "~"))


## ----manipulation_ind2cat_false_cat-------------------------------------------
tibble(ind_grad = c("Y", "N")) %>%
  mutate(cat_grad  = ind_recode(ind_grad, cat_false = "enrolled"))


## ----manipulation_ind2cat_rev-------------------------------------------------
tibble(ind_grad = c("yes", "no")) %>%
  mutate(cat_grad  = ind_recode(ind_grad, rev = TRUE)) %>% 
  mutate(cat_grad_num = as.numeric(cat_grad))


## ----manipulation_ind2cat_prefix----------------------------------------------
tibble(dummy_grad = c(0, 0, 1, 1, 1 ,0 ,0)) %>%
  mutate(cat_grad  = ind_recode(dummy_grad, 
                                var_prefix = "dummy_"))


## ----visual_ind2cat_customization_in_visualizations, fig.width=12, fig.height=15----
data.frame(ind_spam = c(TRUE, TRUE, FALSE, FALSE, FALSE)) %>% 
ggplot() + 
  aes(x = ind_recode(ind_spam)) + 
  geom_bar() +
  theme_gray(base_size = 15)->
p1

p1 +
  aes(x = ind_recode(ind_spam, cat_true = "suspicious")) ->
p2

p1 +
  aes(x = ind_recode(ind_spam, negator = "~")) ->
p3

p1 +
  aes(x = ind_recode(ind_spam, cat_false = "trustworthy")) ->
p4


p1 +
  aes(x = ind_recode(ind_spam, rev = TRUE)) ->
p5

library(patchwork)

(p1 + p2) /
  (p3 + p4) /
  (p5 + patchwork::plot_spacer())


## ----table_ind2cat_preserves--------------------------------------------------
tidytitanic::passengers %>%
  mutate(cat_survived = ind_recode(survived, 
                                   cat_false = "perished")) %>% 
  janitor::tabyl(sex, cat_survived) %>% 
  janitor::adorn_percentages() %>% 
  janitor::adorn_pct_formatting() %>% 
  janitor::adorn_ns(position = "rear")


## ----read_in_function---------------------------------------------------------
readLines("R/ind_recode.R") -> implementation


## ----display_function, code = implementation, eval= F-------------------------
#> NA


## ----get_chunk_names----------------------------------------------------------
knitr::knit_code$get() |> names()


