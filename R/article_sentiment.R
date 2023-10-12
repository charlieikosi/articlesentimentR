#' News Article Sentiment
#'
#' Performs Sentiment Analysis of news articles from the Post-Courier news page
#' @param url Takes character as input. This allows the script to access the website.
#' @param lexicon Takes in character input. Lexicon types can be 'bing', 'loughran', 'afinn' or 'nrc'.
#' @return Returns an object of class 'data.table'.
#' @examples
#' url <- "https://www.postcourier.com.pg/supplementary-budget-not-a-rushed-secret/";
#' df <- article_sentiment(url, "loughran");
#' print(df)
#' @name article_sentiment
#' @import rvest
#' @import tidyverse
#' @import tm
#' @import dplyr
#' @import tidytext
#' @import utils
#' @import textdata
#' @export


library(rvest)
library(tidyverse)
library(dplyr)
library(tidytext)
library(tm)
library(utils)
library(textdata)


article_sentiment <- function(url, lexicon){

  URL <- URLencode(url)
  page <- read_html(URL)
  article <- page %>% html_nodes(".entry-content") %>%
    html_text()

  article <- as.data.frame(article)

  article_tokens <- article %>%
    unnest_tokens(word, article)

  # Load and prepare the sentiment lexicon
  sentiments <- get_sentiments(lexicon)

  sentiments_df <- article_tokens %>%
    inner_join(sentiments, by = "word")

  sentiment_summary <- sentiments_df %>%
    count(sentiment) %>%
    mutate(proportion = n / sum(n))

  # View the sentiment summary
  sentiment_summary <- sentiment_summary[order(sentiment_summary$proportion, decreasing = TRUE),]

}

