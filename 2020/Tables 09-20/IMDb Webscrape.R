#Load packages
library(rvest)
library(XML)
library(tidyverse)
library(stringr)

#IMDb Top 250 Movies
top_250_url = "https://www.imdb.com/chart/top/?ref_=nv_mv_250"

top_250_page = read_html(top250url)

#Select title column
film_nodes <- html_nodes(top_250_page,'.titleColumn a')

#Get film name and cast
Name = html_text(film_nodes)
Cast = sapply(html_attrs(film_nodes),`[[`,'title')
Cast = gsub("\\s*\\([^\\)]+\\)",'', Cast)

#Get secondary info and release year
Sec <- html_nodes(top_250_page,'.secondaryInfo')
Year = as.numeric(gsub(")","", gsub("\\(","", html_text( sec ))))

#Get ratings
rating.nodes = html_nodes(top_250_page,'.imdbRating')

rating.nodes = html_nodes(top_250_page,'.imdbRating strong')
Votes = as.numeric(gsub(',','',
                        gsub(' user ratings','',
                             gsub('.*?based on ','',
                                  sapply(html_attrs(rating.nodes),`[[`,'title')
                             ))))

Rating = as.numeric(html_text(rating.nodes))

#Create data frame and add Director column
top250 <- data.frame(Name, Cast, Year, Votes, Rating)

top250 <- top250 %>%
  rowwise() %>% 
  mutate(Director = strsplit(Cast, split = ",")[[1]][1]) %>% 
  select(Name, Director, Year, Votes, Rating)

