library(rvest)
library(tidyverse)
library(vbfelix)
library(gtrendsR)
library(fuzzyjoin)
library(see)
library(gridExtra)
library(scales)
library(plotly)
library(ggrepel)

getwd()

#Wikitable + Gtrends + visualization----------


url <- "https://en.wikipedia.org/wiki/List_of_professional_sports_leagues_by_revenue"
webpage <- read_html(url)
tbls <- html_nodes(webpage, "table")
leagues <- html_table(tbls[[2]])

acronyms <- str_split(as.character(leagues$League), " ") %>% 
   unlist()

ac <- regmatches(acronyms, gregexpr("(?<=\\().*?(?=\\))", acronyms, perl=T)) %>% 
   unlist() %>% as.tibble() 
colnames(ac) <- "acronym_league"

j_leagues <- leagues %>% 
   regex_inner_join(ac, by = c(League = "acronym_league"))

fdf <- j_leagues %>% 
   select(Sport, `Country(ies)`, acronym_league, `Reve­nue(??? mil)`)
colnames(fdf) <- c("Sport", "Country", "Acronym_of_league", "Revenue")
fdf$Revenue <- as.numeric(gsub("[^0-9.-]", "", fdf$Revenue))

fdf <- fdf %>% 
   arrange(desc(Revenue))

l_fdf <- split(fdf, factor(sort(rank(row.names(fdf))%%4.5)))
TOP5 <- l_fdf$`0`
TOP5_10 <- l_fdf$`0.5`
TOP11_15 <- l_fdf$`1`

TOP5a <- c("NFL", "MLB", "NBA", "EPL", "NHL")
TOP6_10a <- c("Bundesliga", "UCL", "Premier league", "Brasileirão", "NPB")
TOP11_15a <- c("MLS", "AFL", "CSL", "UFC", "LaLiga")

TOP5tr <- gtrends(TOP5a, time = "2014-01-01 2022-01-01", gprop = c("web", "news", "images", "froogle", "youtube"))
TOP5tr_time <- TOP5tr[[1]]
TOP5tr_time$id <- "T5"
TOP5tr_country <- TOP5tr[[2]]
TOP5tr_country$id <- "T5"

TOP6_10tr <- gtrends(TOP6_10a, time = "2014-01-01 2022-01-01", gprop = c("web", "news", "images", "froogle", "youtube"))
TOP6_10time <- TOP6_10tr[[1]]
TOP6_10time$id <- "T10"
TOP6_10country <- TOP6_10tr[[2]]
TOP6_10country <- "T10"

TOP11_15tr <- gtrends(TOP11_15a, time = "2014-01-01 2022-01-01", gprop = c("web", "news", "images", "froogle", "youtube"))
TOP11_15time <- TOP11_15tr[[1]]
TOP11_15time$id <- "T15"
TOP11_15country <- TOP11_15tr[[2]]
TOP11_15country$id <- "T15"

trends_time <- rbind(TOP5tr_time, TOP6_10time, TOP11_15time)
trends_country <- rbind(TOP5tr_country, TOP6_10country, TOP11_15country)

trends_time$hits <- as.numeric(trends_time$hits)
trendsf_time <- na.omit(trends_time)

trends_country$hits <- as.numeric(trends_country$hits)
trendsf_country <- na.omit(trends_country)

p1 <- trendsf_time %>% 
   filter(id == "T5") %>% 
   ggplot(aes(x = date, y = hits, color = keyword)) + 
   geom_smooth(se = F) + 
   theme_abyss() + labs(title = "TOP 5 leagues in revenue - Google trends",
   subtitle = "Worldwide/2014 - 2022")

p2 <- trendsf_time %>% 
   filter(id == "T10") %>% 
   ggplot(aes(x = date, y = hits, color = keyword)) + 
   geom_smooth(se = F) + 
   theme_abyss() + labs(title = "TOP 6-9 leagues in revenue - Google trends",
                        subtitle = "Worldwide/2014 - 2022")

p3 <- trendsf_time %>% 
   filter(id == "T15") %>% 
   ggplot(aes(x = date, y = hits, color = keyword)) + 
   geom_smooth(se = F) + 
   theme_abyss() + labs(title = "TOP 10-14 leagues in revenue - Google trends",
                        subtitle = "Worldwide/2014 - 2022")

time_plot <- grid.arrange(p1, p2, p3)


regions <- c("America", "Carribean", "Australia", "America", "Africa", "America", "Europe", "Australia", "Asia", "Asia", "Africa", "Carribean", "Africa", "Africa", "Africa", "Africa", "Africa")

plot_country <- trendsf_country %>% 
   group_by(location) %>% 
   summarise(s_hits = sum(hits)) %>% 
   filter(s_hits >=75) %>% 
   arrange(desc(s_hits)) %>% 
   cbind(regions)

country_plot <- ggplot(plot_country, aes(reorder(location, s_hits), s_hits, fill = regions)) +
   geom_bar(stat = "identity") + 
   coord_flip() + 
   theme_abyss() + labs(title = "Sum of league hits by region",
                        subtitle = "By countries/2014-2022",
                        x = "Country", 
                        y = "Sum of hits")


#Worldbank dataset-------------
wb_df <- read.csv("worldbank.csv")

int_p <- wb_df %>%
   filter(Indicator.Code == "IT.NET.USER.ZS") %>% 
   select(Country.Code, X1994:X2020) %>% 
   pivot_longer(!Country.Code, names_to = "Year", values_to = "int_users") %>% 
   na.omit()

int_p$Year <- as.numeric(gsub("[^0-9.-]", "", int_p$Year))   

int_plot <- ggplot(int_p, aes(Year, int_users, color = Country.Code)) + geom_line() +
   labs(title = "Internet penetration", 
        subtitle = "By country/1994-2020",
        y = "Percentage of internet users") + 
   theme_abyss()

int_plot

mob_p <- wb_df %>% 
   filter(Indicator.Code == "IT.CEL.SETS.P2") %>% 
   select(Country.Code, X1990:X2020) %>% 
   pivot_longer(!Country.Code, names_to = "Year", values_to = "mob_sub100") %>% 
   filter(mob_sub100 >= 0) %>% 
   na.omit()

mob_p$Year <- as.numeric(gsub("[^0-9.-]", "", mob_p$Year))
   
mob_plot <- ggplot(mob_p, aes(Year, mob_sub100, color = Country.Code)) + geom_line() +
   labs(title = "Mobile subscriptions per 100 people", 
        subtitle = "By country/1990-2020",
        y = "Number of subs per 100 ppl") + 
   theme_abyss()

mob_plot


ele_p <- wb_df %>% 
   filter(Indicator.Code == "EG.ELC.ACCS.ZS") %>% 
   select(Country.Code, X1980:X2020) %>% 
   pivot_longer(!Country.Code, names_to = "Year", values_to = "access_electricity") %>% 
   na.omit()

ele_p$Year <- as.numeric(gsub("[^0-9.-]", "", ele_p$Year))

ele_plot <- ggplot(ele_p, aes(Year, access_electricity, color = Country.Code)) + geom_line() +
   labs(title = "Access to electricity", 
        subtitle = "By country/1993-2020",
        y = "Percentage of ppl with access to electricity") + 
   theme_abyss()

ele_plot

cost_df <- read.csv("data_cost_2020.csv")

co <- cost_df %>% 
   select(Name, Average.price.of.1GB..USD., Cheapest.1GB.for.30.days..USD., Most.expensive.1GB..USD.)

colnames(co) <- c("Country", "AVG", "MIN", "MAX")

cost_K <- co %>% 
   filter(Country == "Kenya")
cost_P <- co %>% 
   filter(Country == "Philippines")

cost_p <- rbind(cost_K, cost_P)

cost_p$MAX <- as.numeric(gsub("[^0-9.-]", "", cost_p$MAX))

cost_p <- cost_p %>% 
   pivot_longer(!Country, names_to = "Type_of_cost", values_to = "USD_price")

ggplot(cost_p, aes(Country, USD_price)) + 
   geom_point(color = "white", size = 4) + 
   geom_line(color = "white") + 
   geom_label_repel(aes(label = Type_of_cost), color = "black", size = 3) + 
   labs(title = "Cost of 1GB internet data", 
        subtitle = "Kaggle dataset/2021",
        y = "Price in USD") +
   theme_abyss()
   
