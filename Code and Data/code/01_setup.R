# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Setup
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Install Required Packages (Run only during the first time on your R setup)
install.packages("tidyverse")
install.packages("ggthemes")
install.packages("skimr")
install.packages("zoo")
install.packages("moderndive")
install.packages("lubridate")
install.packages("tm")
install.packages("mlogit")
install.packages("stargazer")

# Load packages
library(tidyverse)
library(ggthemes)
library(skimr)
library(zoo)
library(moderndive)
library(lubridate)
library(tm)
library(mlogit)
library(stargazer)


# Read in raw data
patent_inventors <- read_csv(str_interp("${input_dir}/patent_data/patent_inventors_v2.csv"))
patent_cite_counts <- read_csv(str_interp("${input_dir}/patent_data/patent_cite_counts_v2.csv"))
patent_class <- read_csv(str_interp("${input_dir}/patent_data/patent_int_classes_v2.csv"))
canton_year_taxes <- read_csv(str_interp("${input_dir}/tax_data/canton_year_taxes.csv"))