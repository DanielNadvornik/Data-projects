library(rtweet)
library(twitteR)
library(tidyverse)
library(tidytext)
library(textdata)
library(easystats)
library(ggplot2)
library(see)
library(stringr)



api_key <- "0C4KmjgwvilNLtGid6PsHNpgR"
api_secret_key <- "YPpIWzmCPAQK6dTBwO7a2CcjxbUljnLA0OxQH5LR53ZP9LKzZI"
access_token <- "1399171154-NPAElXmB4vlcqzmvlo6WYut9FseytM2FAqUlj2Z"
access_token_secret <- "LKVlDb5y7of8HZM3RbW5Zp4inZ1F4Q7p7KY5X4ZzfvOqU"

getwd()

token <- create_token(
   app = "Danko_twitter_analyza",
   consumer_key = api_key,
   consumer_secret = api_secret_key,
   access_token = access_token,
   access_secret = access_token_secret)



swu_2202 <- search_tweets("#StandWithUkraine", type = "recent", include_rts = F, n = 9000)
write_as_csv(swu_2202, "swud_2022")
swu2202 <- read.csv("swud_2022.csv")

putin_2202 <- search_tweets("Putin", type = "recent", include_rts = F, n = 9000, lang = "en")
write_as_csv(putin_2202, "putin_2202")
putin2202 <- read.csv("putin_2202.csv")

#EDA
swu_2202 %>% count(screen_name) %>% arrange(desc(n))
swu


#Time
p1 <- ts_plot(swu_2202, by = "hours", lwd = 0.7) + 
   ggplot2::theme_classic() +
   ggplot2::labs(title = "Frequency of #StandWithUkraine hashtag by hours", subtitle = "19-22/02 2022", x = "Date and time", y = "Number of tweets") +
   ggplot2::theme(title = element_text(size = 8))
p1

#sentiment
clean_tweets <- function(x) {
   x %>%
      # Remove URLs
      str_remove_all(" ?(f|ht)(tp)(s?)(://)(.*)[.|/](.*)") %>%
      # Remove mentions e.g. "@my_account"
      str_remove_all("@[[:alnum:]_]{4,}") %>%
      # Remove hashtags
      str_remove_all("#[[:alnum:]_]+") %>%
      # Replace "&" character reference with "and"
      str_replace_all("&amp;", "and") %>%
      # Remove puntucation, using a standard character class
      str_remove_all("[[:punct:]]") %>%
      # Remove "RT: " from beginning of retweets
      str_remove_all("^RT:? ") %>%
      # Replace any newline characters with a space
      str_replace_all("\\\n", " ") %>%
      # Make everything lowercase
      str_to_lower() %>%
      # Remove any trailing whitespace around the text
      str_trim("both")
} 
words_putin <- putin2202$text %>% clean_tweets() %>% as_tibble()

t_words_putin <- words_putin %>% unnest_tokens(word, value)
stopwords <- as.data.frame(stop_words$word)
ft_words_putin <- t_words_putin %>% filter(!word %in% stop_words$word) 
wdf <- ft_words_putin

sent <- get_sentiments("nrc")

sent_t <- wdf %>% inner_join(sent) %>% count(sentiment, sort = T) %>% mutate(prop = n/sum(n))
p2 <- ggplot(sent_t, aes(reorder(sentiment, prop), prop)) + geom_bar(stat = "identity") +
   coord_flip()

p2 <- p2 + theme_classic() +
   labs(title = "Sentiment analysis of tweets with keyword Putin", 
                          subtitle = "19-22/02 2022",
        x = "sentiment category",
        y = "%")
   
p2                     


locats <- swu2202 %>% select(text, screen_name, location)

report(locats)

