#Load packages
library(tidyverse)
library(gt)
library(here)

# Source web scraping script
source("IMDb Webscrape.R")

#Create index column
top250 <- tibble::rowid_to_column(top250, "Rank")

#Bar chart function
bar_chart <- function(value, colour = colour, display_value = NULL){
  if (is.null(display_value)) {
    display_value <- "&nbsp;"
  } else {
    display_value <- display_value
  }
  glue::glue("<span style=\"display: inline-block; direction: ltr; border-radius: 4px; padding-right: 2px; background-color: {color}; color: {color}; width: {value}%\"> {display_value} </span>")
}

#Add colour and bar chart data
top250 <- top250 %>% 
  mutate(
    bar = Votes/25000,
    colour = factor(case_when(
      Name == "The Lord of the Rings: The Fellowship of the Ring" ~ "#176087",
      Name == "The Lord of the Rings: The Two Towers" ~ "#176087",
      Name == "The Lord of the Rings: The Return of the King" ~ "#176087",
      TRUE ~ "#C5C3C6")), 
    bar_chart = bar_chart(bar, colour),
    bar_chart = map(bar_chart, ~gt::html(as.character(.x))))

#Filter top 20 and create table
IMDb_table <- top250 %>% 
  select(Rank, Name, Director, Year, Rating, Votes, bar_chart) %>% 
  filter(Rank <= 20) %>% 
  gt(
  ) %>% 
  cols_width(vars(bar_chart) ~ px(100),
  ) %>% 
  cols_label(
    bar_chart = ""
  ) %>% 
  cols_align(
    align = "left",
    columns = vars(bar_chart)
  ) %>% 
  fmt_number("Votes", decimals = 0) %>% 
  tab_header(
    title = md("<span style='color:#0C0910'>The <span style='color:#176087'>**Lord of the Rings** <span style='color:#0C0910'>trilogy is highly rated"),
    #title = md("**The Lord of the Rings trilogy sits in the top 20 highest rated films**"),
    subtitle = "Based on data from the IMDb Top 250 Films"
  ) %>% 
  tab_stubhead(label = "Rank") %>% 
  tab_options(
    table.font.names = "Century Gothic",
    #table.font.size = 12,
    row_group.border.top.width = px(1),
    row_group.border.top.color = "black",
    row_group.border.bottom.color = "black",
    stub.border.color = "white",
    table_body.hlines.color = "white",
    table.border.top.color = "white",
    table.border.top.width = px(2),
    table.border.bottom.color = "white",
    table.border.bottom.width = px(2),
    table_body.border.bottom.width = px(1),
    table_body.border.bottom.color = "black",
    column_labels.border.bottom.color = "black",
    column_labels.border.bottom.width = px(2)
  ) %>%
  tab_style(
    style = list(
    cell_fill(color = "white"),
    cell_text(color = "#176087", weight = "bold")
    ),
    locations = list(
      cells_body(
        rows = Name == "The Lord of the Rings: The Fellowship of the Ring"
        ),
      cells_body(
        rows = Name == "The Lord of the Rings: The Two Towers"
        ),
      cells_body(
        rows = Name == "The Lord of the Rings: The Return of the King"
      )
    )
  ) %>% 
  tab_source_note(md("**Table**: @jaredbraggins  | **Data**: IMDb<br>**Inspiration**: @thomas_mock"))

gtsave(IMDb_table, "IMDb Table.png")

