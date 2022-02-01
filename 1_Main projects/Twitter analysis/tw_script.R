library(rtweet)
library(twitteR)
library(tidyverse)
library(tidytext)
library(textdata)
library(easystats)
library(ggplot2)
library(see)



api_key <- "0C4KmjgwvilNLtGid6PsHNpgR"
api_secret_key <- "YPpIWzmCPAQK6dTBwO7a2CcjxbUljnLA0OxQH5LR53ZP9LKzZI"
access_token <- "1399171154-NPAElXmB4vlcqzmvlo6WYut9FseytM2FAqUlj2Z"
access_token_secret <- "LKVlDb5y7of8HZM3RbW5Zp4inZ1F4Q7p7KY5X4ZzfvOqU"


token <- create_token(
   app = "Danko_twitter_analyza",
   consumer_key = api_key,
   consumer_secret = api_secret_key,
   access_token = access_token,
   access_secret = access_token_secret)


dict_ru <- readr::read_csv("https://raw.githubusercontent.com/text-machine-lab/sentimental/master/sentimental/word_list/russian.csv")
dict_us <- get_sentiments("afinn")


tweets_ru_w4m1 <- search_tweets(q = c("???????? OR ??????????????"),
                           n = 10000, 
                           lang = "ru",
                           type = "mixed",
                           include_rts = F)

tweets_us_w4m1 <- search_tweets(q = c("NATO OR Ukraine"),
                                n = 10000, 
                                lang = "en",
                                type = "mixed",
                                include_rts = F)

archived_us_w4m1 <- tweets_ru_w4m1
archived_ru_w4m1 <- tweets_us_w4m1


tweets_ruf_w4m1 <- tweets_ru_w4m1 %>% filter(favorite_count >= 5) %>% select(text)
tweets_usf_w4m1 <- tweets_us_w4m1 %>% filter(favorite_count >= 5) %>% select(text)

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

ruf_w4m1_t <- tweets_ruf_w4m1 %>% unnest_tokens(word, text)
ruf_w4m1 <- ruf_w4m1_t %>% inner_join(dict_ru, by = "word") %>% mutate(lang = "RU")
colnames(ruf_w4m1) <- c("word", "score", "lang")

usf_w4m1_t <- tweets_usf_w4m1 %>% unnest_tokens(word, text)
usf_w4m1 <- usf_w4m1_t %>% inner_join(dict_us, by = "word") %>% mutate(lang = "EN")
colnames(usf_w4m1) <- c("word", "score", "lang")

df_w4m1 <- rbind(ruf_w4m1, usf_w4m1)

p1 <- ggplot(df_w4m1, aes(score, fill = lang)) + geom_density(alpha = 0.5)

p1f <- df_w4m1 %>% group_by(lang) %>% summarize(medianscore = median(score))

p1 <- p1 + geom_vline(data = p1f, aes(xintercept = medianscore, color = lang), linetype = "dashed", size = 2) + theme_abyss() +
   xlab("Sentiment score") +
   labs(title = "Sentiment analysis of tweets that contain words NATO or UKRAINE",
        subtitle = "From 4th week of January, 2022/dashed lines = median of sentiment score for given language",
        caption = "Original tweets with 5+ likes included only")
   
                                                                                                                     
usf_w4m1_words <- usf_w4m1 %>% group_by(word, score) %>% count() %>% arrange(desc(n)) %>% head(10) %>% mutate(lang = "EN")
ruf_w4m1_words <- ruf_w4m1 %>% group_by(word, score) %>% count() %>% arrange(desc(n)) %>% head(10)

words <- c("against",
"new",
"newly",
"Okay",
"fuck",
"need to", 
"interesting",
"fight",
"eat",
"ban")

ruf_w4m1_words_t <- cbind(words, ruf_w4m1_words$score, ruf_w4m1_words$n)
colnames(ruf_w4m1_words_t) <- c("word", "score", "n")
ruf_w4m1_words_t <- as.data.frame(ruf_w4m1_words_t) %>% mutate(lang = "RU")

df2_w4m1 <- rbind(ruf_w4m1_words_t, usf_w4m1_words)
df2_w4m1$n <- as.numeric(df2_w4m1$n)
df2_w4m1 <- df2_w4m1 %>% arrange(desc(n))

p2 <- ggplot(df2_w4m1, aes(reorder(word, n), n, fill = lang)) + geom_bar(stat = "identity") + 
   geom_label(aes(label = score)) +
   coord_flip() +
   theme_abyss()

p2 <- p2 + labs(title = "10 most used words for given language",
          subtitle = "From 4th week of January, 2022/sentiment score = the number at the end of bar",
          caption = "Original tweets with 5+ likes included only") +
   xlab("Word") + 
   ylab("Count of the word")

p1
p2
