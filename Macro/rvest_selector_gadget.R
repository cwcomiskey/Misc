# http://blog.rstudio.com/2014/11/24/rvest-easy-web-scraping-with-r/

# CSS is a language that describes the style of an HTML document, and how it should be displayed.

install.packages("rvest")
library(rvest)
lego_movie <- read_html("http://www.imdb.com/title/tt1490017/")

vignette("selectorgadget") # ======
html <- read_html("http://www.imdb.com/title/tt1490017/")

# Guided attempt 1 
cast <- html_nodes(html, "#titleCast .itemprop")
length(cast) # [1] 30
cast[1:2]
html_text(cast)

# Guided attempt 2
cast <- html_nodes(html, ".itemprop .itemprop")
cast <- html_nodes(html, "#titleCast span.itemprop") # not sure why/how this 'span.' works
length(cast) # [1] 15
html_text(cast)

# End selectorgadget vignette ======

lego_movie %>% 
  html_node("strong span") %>% # ".ratingValue span" works
  html_text() %>%
  as.numeric()

lego_movie %>% 
  html_nodes("#titleCast .itemprop span") %>%
  html_text()

lego_movie %>%
  html_nodes("table") %>%
  .[[2]] %>% 
  html_table()


