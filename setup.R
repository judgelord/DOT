options(stringsAsFactors = FALSE)

requires <- c("gmailr", 
              "dplyr", 
              "ggplot2", 
              "gdata", 
              "magrittr",
              "XML",
              "stats",
              "zoo",
              "stringi",
              "stringr", 
              "stargazer",
              "visreg",
              "reshape2",
              "scales",
              "here",
              "msm",
              "survival",
              "mstate",
              "knitr",
              "kableExtra",
              "tidyverse",
              "broom",
              "survminer",
              "ggfortify"
              )
to_install <- c(requires %in% rownames(installed.packages()) == FALSE)
install.packages(c(requires[to_install], "NA"), repos = "https://cloud.r-project.org/" )

library(tidyverse)
library(dplyr) # in case tydyverse fails (problem on linux)
library(readr)
library(ggplot2); theme_set(theme_bw())
library(magrittr)
library(XML)
library(stringr)
library(stringi)
library(stats)
library(zoo)
library(stargazer)
library(visreg)
library(reshape2)
library(scales)
library(here)
library(msm)        #v1.6.1
library(survival)   #v.2.39.5 (!! Important.)
library(mstate)     #v0.2.9
library(knitr)
# library(kableExtra)
library(survminer)
library(ggfortify)
library(broom)

knitr::opts_chunk$set(echo = TRUE, 
                      cache = F, 
                      fig.width=8.5, 
                      fig.align = 'center',
                      fig.path='Figs/',
                      warning=FALSE, message=FALSE)

# function to fill NAs
CopyIfNA <- function(x, na.rm = FALSE, ...) na.locf(x, na.rm = na.rm, ...)

