library(dplyr)
library(lubridate)
library(ggplot2)
library(ggtext)
library(scales)
library(tidyr)
library(gt)
library(purrr)


Company_Table <- readxl::read_excel("Report_Dataset.xlsx", sheet = "Company_Table")
Project_Table <- readxl::read_excel("Report_Dataset.xlsx", sheet = "Project_Table")
Grant_Table <- readxl::read_excel("Report_Dataset.xlsx", sheet = "Grant_Table")
Mapdata <- readxl::read_excel("Report_Dataset.xlsx", sheet = "Mapdata")