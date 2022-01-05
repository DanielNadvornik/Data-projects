library(tidyverse)
library(Hmisc)
library(Amelia)
library(ggplot2)
library(scales)
library(corrplot)
library(fedmatch)
library(gridExtra)

getwd()

df <- read.csv("C:/Users/Daniel/Desktop/Rko/1_Dánsko/Common passwords/common_passwords.csv")


#EDA-------------------------------------

hist.data.frame(df)
missmap(df)
str(df)

ndf <- df[2:9]

corrplot(cor(ndf), method = "number", order = "AOE", type = "lower")

#Visualisation---------------------------

#p1-------------

len_pass <- df %>% count(length) %>% mutate(prop = n/sum(n)) 

colnames(len_pass) <- c("len", "count", "prop")

len_pass_5 <- len_pass[2:6,]

p1 <- ggplot(len_pass_5, aes(len, prop)) + geom_point(size = 5, color = "orange") + 
   geom_segment(aes(xend = len, yend = 0)) + 
   theme_bw() + 
   labs(title = "Length of the most used passwords", 
        x = "Number of char", 
        y = "") + 
   coord_flip() +
   scale_y_continuous(labels = percent) 


#p2------------------


pass <- df[grepl("pass", df$password), , drop = F]

num <- df[grepl("123", df$password), , drop = F]

qwe <- df[grepl("qwe", df$password), , drop = F]

asd <- df[grepl("asd", df$password), , drop = F]

abc <- df[grepl("abc", df$password), , drop = F]

pass$id <- "pass"

num$id <- "123"

qwe$id <- "qwe"

asd$id <- "asd"

abc$id <- "abc"

most_u <- rbind(pass, qwe, asd, abc)


p2 <- ggplot(most_u, aes(factor(id), fill = factor(id))) + geom_bar(width = 1, colour = "black") + coord_polar(start = pi) + theme_bw() + 
   labs(title = "Passwords that contain usual patterns",
        x = "", 
        y = "") + theme(legend.position = "None")


#p3-----------------

df$s_pass <- if_else(df$length > 3 & df$num_chars > 0 & df$num_digits > 0 & df$num_upper > 0, "yes", "no")


p3 <- ggplot(df, aes(s_pass)) + geom_bar(aes(fill = s_pass), show.legend = F) + theme_bw() + coord_flip() + labs(title = "Using atleast one digit and one uppercase",
                                                                                          y = "", 
                                                                                          x = "") + theme(axis.text.y = element_text(face = "bold.italic",
                                                                                                                                     size = 13))


#p4----------------

passworddf$id1 <- 1:10000
namesdf$id2 <- 1:31271

wgt_jaccard_match <- merge_plus(data1 = passworddf, 
                                data2 = namesdf,
                                by.x = "password",
                                by.y = "password", match_type = "fuzzy", 
                                fuzzy_settings = build_fuzzy_settings(method = "wgt_jaccard", nthread = 2,
                                                                      maxDist = 0.8),
                                unique_key_1 = "id1",
                                unique_key_2 = "id2")

matching_names <- wgt_jaccard_match[[1]]

p4 <- ggplot(matching_names, aes(password_1, id1)) + geom_label(aes(label = password_1), size = 3, position = position_dodge(width = .9), color = "blue") + theme_bw() + theme(
                                                                                                                 axis.text.x=element_blank(),
                                                                                                                 axis.ticks.x=element_blank()) +
   labs(title = "Names used as password",
        x = "Names", 
        y = "ID")


plots <- grid.arrange(p1, p4, p2, p3, nrow = 2)
