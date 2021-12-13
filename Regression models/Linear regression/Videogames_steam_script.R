library(ggplot2)
library(tidyverse)
library(ggeffects)
library(lme4)
library(forcats)
library(rockchalk)
library(plotly)
library(ggfortify)
library(lspline)
library(splines)
library(ggformula)
library(plotly)
library(Amelia)


#Data manipulation
video_games <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-07-30/video_games.csv")


video_games <- video_games %>% filter(!is.na(metascore))

video_games$owners <- as.factor(video_games$owners)

unique(video_games$owners)

video_games <- video_games %>% mutate(owners_l = factor(owners, labels = c("do dvaceti", 
                                                                           "milion az dva", 
                                                                           "deset az dvacet m",
                                                                           "sto az dve ste t",
                                                                           "sto az dve ste m",
                                                                           "dva az pet m",
                                                                           "dvacet az padesat t",
                                                                           "dvacet az padesat m",
                                                                           "dve ste az petset t",
                                                                           "pet az deset m",
                                                                           "padesat az sto t",
                                                                           "padesat az sto m",
                                                                           "petset tisic az jeden m")))

video_games$Owners <- combineLevels(video_games$owners_l, 
                                    levs = c("do dvaceti", "dvacet az padesat t", 
                                             "padesat az sto t", "sto az dve ste t",
                                             "dve ste az petset t", 
                                             "petset tisic az jeden m"), 
                                    newLabel = "0 - 1mil vlastníkù") 

video_games$Owners <- combineLevels(video_games$Owners, levs = c("milion az dva", 
                                                                 "dva az pet m"), 
                                    newLabel = "1mil - 5mil vlastníkù")
              
              
video_games$Owners <- combineLevels(video_games$Owners, levs = c("pet az deset m", 
                                             "deset az dvacet m", 
                                             "dvacet az padesat m",
                                             "padesat az sto m",
                                             "sto az dve ste m"), 
                                    newLabel = "5mil - 200mil vlastníkù")

video_games$class <- case_when(
   video_games$Owners == "0-1 mil owners" ~ "1",
   video_games$Owners == "1 - 5 mil owners" ~ "2",
   video_games$Owners == "5 - 200 mil owners" ~ "3")


#checking the missing values in the dataset
missmap(video_games, main = "Missing vars map", col = c("yellow", "black"), legend = FALSE)


#checking the scewness of the metascore variable
vgm <- filter(video_games, metascore >= mean(metascore))
vgm2 <- filter(video_games, metascore <= mean(metascore))


#Imputation of the data based on the mean value of Class(Owners)
m1 <- video_games %>% filter(class=="1")
pr1 <- mean(m1$price, na.rm = T)
pr1 <- round(pr1)

m2 <- video_games %>% filter(class=="2")
pr2 <- mean(m2$price, na.rm = T)
pr2 <- round(pr2)

m3 <- video_games %>% filter(class=="3")
pr3 <- mean(m3$price, na.rm = T)
pr3 <- round(pr3)

impute_price <- function(price,class){
      out <- price
      for (i in 1:length(price)){
         
         if (is.na(price[i])){
            
            if (class[i] == 1){
               out[i] <- pr1
               
            }else if (class[i] == 2){
               out[i] <- pr2
               
            }else{
               out[i] <- pr3
            }
         }else{
            out[i]<-price[i]
         }
      }
      return(out)
}

video_games$price <- impute_price(video_games$price, video_games$class)

#Checking the data after imputation of missing values
missmap(video_games, main = "Missing vars map", col = c("yellow", "black"), legend = FALSE)


#Visualizing the data
n_vg <- video_games %>% dplyr::select(where(is.numeric))

n_vg <- n_vg %>% select(-number)

summary_all <- function(x) {
   summary(x)
}

sapply(n_vg, summary_all)


op <- par(mfrow=c(2, 2))
lapply(seq(n_vg), function(x) 
   hist(x=n_vg[[x]], scales = "free", bins = 20, xlab=names(n_vg)[x],
        main=paste("Hist", names(n_vg)[x])))



#Building the models
mod1 <- lm(metascore ~ price, data = video_games)
mod2 <- lm(metascore ~ Owners + ns(price, df = 2), data = video_games)

summary(mod1)
summary(mod2)


#Marginal effects graph for mod2
marg1 <- plot(ggpredict(mod2, terms = c("price", "Owners")))
marg1

#Visualizing mod1
p <- ggplot(video_games, aes(x = price, y = metascore))

p <- p + geom_point(position=position_jitter(w=1, h=0), aes(colour = factor(Owners)),  alpha=0.6, size = 2.5)

p <- p + scale_color_manual(values=c("lightblue", "greenyellow", "darkblue"))

p <- p + geom_smooth(method = lm, formula = y ~ splines::ns(x, 2), color = "darkblue")

p <- p + scale_y_continuous(limits = c(40,100), 
                            breaks = seq(40,100, by = 10))


p <- p + labs(title = "Price as a predictor of metascore",
         subtitle = "Dataset from steam",
         caption = "year 18/19",
         color = "Number of the owners")

p <- p + theme(legend.title = element_text(size = 5), 
               legend.text = element_text(size = 5))

p <- p + theme_classic()

p

#Diagnostics of the models
autoplot(mod1, which = 1:4)
autoplot(mod2, which = 1:4)





