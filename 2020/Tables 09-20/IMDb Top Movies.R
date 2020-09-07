#Load packages
library(tidyverse)
library(gt)
library(here)

# Source web scraping script
source("IMDb Webscrape.R")

#Create index column and filter to top 20 films only
top250 <- tibble::rowid_to_column(top250, "ID")

top20 <- top250 %>% 
  filter(ID <= 20)

  
#Create Table
top20 %>% 
  gt(
    rowname_col = "ID"
  ) %>% 
  tab_header(
    title = md("**IMDb Top 20 Films**"),
  ) %>% 
  tab_options(
    table.font.names = "Century Gothic",
    row_group.border.top.width = px(3),
    row_group.border.top.color = "black",
    row_group.border.bottom.color = "black",
    table_body.hlines.color = "white",
    table.border.top.color = "white",
    table.border.top.width = px(3),
    table.border.bottom.color = "white",
    table.border.bottom.width = px(3),
    table_body.border.bottom.width = px(2),
    table_body.border.bottom.color = "black",
    column_labels.border.bottom.color = "black",
    column_labels.border.bottom.width = px(3)
  ) %>%
  tab_source_note(md("Table: @jaredbraggins | Source: IMDb"))
