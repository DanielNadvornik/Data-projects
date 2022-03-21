library(easystats)
library(tidyverse)
library(haven)
library(data.table)
library(psych)
library(naniar)
library(mice)
library(missForest)
library(sjmisc)
library(gridExtra)
library(ggstatsplot)

getwd()


#wrangling--------------------

dc <- read_sav("C:/Users/Daniel/Desktop/Rko/1_Dánsko/Digital capital/Jup-dig/dig_kap.sav")

dc <- data.table(dc)

dc <- dc %>% filter(ResStatus == 2)

dcc <- dc[,(1:15):= NULL]


colnames(dcc)[1:21] <- c("Age", "Gender", "Edu", "size_of_muni", "region", "work_stat_fulltime", "work_stat_parttime", "work_stat_parent", "work_stat_unemployed", "work_stat_enterp", "work_stat_student", "work_stat_retired",
                         "years_of_internetUsage", "mobile_user", "laptop_user", "tablet_user", "PC_user", "console_user", "smartTV_user", "other_user", "always_able_int")

not <- c("QP4_1", "QP4_2", "QP4_3", "QP5", "QP5_2")

dcc <- dcc %>% select(-not)

dcc <- dcc %>% mutate(y_of_internetUsage = Age - years_of_internetUsage)

dcc$y_of_internetUsage_f <- replace(dcc$y_of_internetUsage,dcc$y_of_internetUsage<=0, NA)
dcc$y_of_internetUsage_ff <- replace(dcc$y_of_internetUsage_f,dcc$y_of_internetUsage_f>25, NA)
dcc <- subset(dcc, select = -c(y_of_internetUsage_f, y_of_internetUsage))
report(dcc$y_of_internetUsage_ff)

dcc <- as.data.frame.array(dcc)

dcc$ID <- cbind(1:450)

socio_demo <- dcc %>% select(Age:always_able_int)

report_eda <- report(as.data.frame.array(socio_demo))
report_eda


#Factor Analysis------------------

#battery4 - clustering based on cultural dimension (factor scores) of internet usage
set.seed(123)
battery_4 <- dcc[, 71:84]

battery_4 <- replace_with_na_all(battery_4, condition = ~.x == 6)
report(battery_4)

imp_b4 <- mice(battery_4, method = "pmm", maxit = 3)
cimp_b4 <- merge_imputations(battery_4, imp_b4, summary = "hist")
cimp_b4

df_b4 <- as.data.frame(cimp_b4["data"])
df_b4 <- df_b4 %>% select(-data.QPo1_12)

check_factorstructure(df_b4)

nf4 <- parameters::n_factors(df_b4)
plot(nf4)

fca_4 <- psych::fa(df_b4, nfactors = 4, rotate = "oblimin")

fca_a4 <- fca_4 %>% model_parameters(sort = TRUE, threshold = "max")
fca_a4

lfs4 <- factor.scores(df_b4, fca_4)
fs4 <- as.data.frame(lfs4["scores"])
colnames(fs4) <- c("fs_escape", "fs_tool", "fs_untrust", "fs_problem")

dcc <- cbind(dcc, fs4)

#Clustering
n <- n_clusters(fs4, package = c("easystats", "NbClust", "mclust"))
plot(n)
n

rez_kmeans <- cluster_analysis(fs4, n = 5, method = "kmeans")
rez_kmeans
rez_hkmeans <- cluster_analysis(fs4, n = 5, method = "hkmeans")
rez_hkmeans
rez_pam <- cluster_analysis(fs4, n = 5, method = "pam")
rez_pam

dcc$clus_g5 <- predict(rez_hkmeans)
dcc$clus_g5 <- as.factor(dcc$clus_g5)




#battery1 - #skills with digital media 
battery_1 <- dcc[, 22:39]
battery_1 <- replace_with_na_all(battery_1, condition = ~.x == 5)
report(battery_1)

p1b <- cbind(battery_1)

p1b[sapply(p1b, is.numeric)] <- lapply(p1b[sapply(p1b, is.numeric)], 
                                                     as.factor)
p1b[] <- lapply(p1b, factor, 
                       levels=c(1,2,3,4),
                       labels = c("It fits perfectly", "It rather fits", "Rather, it doesn't fit", "It definitely doesn't fit"))

p1b <- cbind(p1b, dcc$clus_g5)
p1b <- p1b %>% rename(clusg5 =`dcc$clus_g5`)

p1bb <- p1b %>% 
   pivot_longer(!clusg5, names_to = "Question", values_to = "Answer") %>% 
   mutate(Answer = fct_relevel(Answer,"It fits perfectly", "It rather fits", "Rather, it doesn't fit", "It definitely doesn't fit")) %>% 
   count(Question, Answer) %>% 
   group_by(Question)


   


mutate(order_value = n[Answer == "It fits perfectly"]) %>%
   ungroup() %>% 
   mutate(Question = fct_reorder(Question, order_value),
          Answer = fct_rev(Answer)) %>% 
   ggplot(aes(x = n, y = Question, fill = Answer)) +
   geom_col(position = "fill") +
   scale_fill_brewer(palette = "Spectral")





gather(key = Activity, value = Answer) %>% 
   mutate(Answer = fct_relevel(Answer, "Multiple times per day", "Atleast once a day", "Atleast once a week", "Atleast once a month", "Less than once a month", "Never")) %>%
   count(Activity, Answer) %>% 
   group_by(Activity) %>% 
   mutate(order_value = n[Answer == "Multiple times per day"]) %>%
   ungroup() %>% 
   mutate(Activity = fct_reorder(Activity, order_value),
          Answer = fct_rev(Answer)) %>% 
   ggplot(aes(x = n, y = Activity, fill = Answer)) +
   geom_col(position = "fill") +
   scale_fill_brewer(palette = "Spectral") +
   #scale_y_continuous(labels = scales::percent_format()) + 
   labs(title = "Frequency of on-line activities in czech population") + 
   guides(fill = guide_legend(title = "Frequency")) + 
   ylab("Percent of respondents") +
   theme_abyss()


ggplot(p1b, aes(Question, Answer, fill = Answer)) + geom_bar(stat = "identity", aes(position = "fill")) + facet_grid(~p1b$clusg5)




imp_b1 <- mice(battery_1, minbucket = 10, method = "cart", maxit = 3)
cimp_b1 <- merge_imputations(battery_1, imp_b1, summary = "hist")
df_b1 <- as.data.frame(cimp_b1["data"])

check_factorstructure(df_b1)

nf <- parameters::n_factors(df_b1)
plot(nf)

fca_a <- psych::fa(df_b1, nfactors = 4)

fca_aa <- fca_a %>% model_parameters(sort = TRUE, threshold = "max")
fca_aa

fs <- factor.scores(df_b1, fca_a)



ind1_batt1 <- c("QD1A_18", "QD1A_3", "QD1A_8", "QD1A_16", "QD1A_15", "QD1A_10", "QD1A_13", "QD1A_14", "QD1A_1", "QD1A_5")
ind2_batt1 <- c("QD1A_17", "QD1A_4", "QD1A_6", "QD1A_12")
ind3_batt1 <- c("QD1A_9", "QD1A_2", "QD1A_11", "QD1A_7")

ind1_b1 <- dcc %>% select(ind1_batt1)
ind1_b1 <- ind1_b1 %>% rename_with(~ paste0("f", .x))
ind1_b1 <- replace_with_na_all(ind1_b1, condition = ~.x == 5) 

ind2_b1 <- dcc %>% select(ind2_batt1)
ind2_b1 <- ind2_b1 %>% rename_with(~ paste0("f", .x))
ind2_b1 <- replace_with_na_all(ind2_b1, condition = ~.x == 5)

ind3_b1 <- dcc %>% select(ind3_batt1)
ind3_b1 <- ind3_b1 %>% rename_with(~ paste0("f", .x))
ind3_b1 <- replace_with_na_all(ind3_b1, condition = ~.x == 5)

fs_df <- cbind(ind1_b1, ind2_b1, ind3_b1)

battery_1 <- dcc %>% select(starts_with("f"))

report(battery_1)
check_factorstructure(battery_1)

nf <- parameters::n_factors(battery_1)
plot(nf)

fca_a <- psych::fa(battery_1, nfactors = 3)

fca_aa <- fca_a %>% model_parameters(sort = TRUE, threshold = "max")
fca_aa


#battery1 - according to literature
ind_safety <- c("QD1A_1", "QD1A_9", "QD1A_11", "QD1A_12")
ind_content <- c("QD1A_2", "QD1A_4", "QD1A_6")
ind_problems <- c("QD1A_3", "QD1A_5", "QD1A_7", "QD1A_8", "QD1A_10", "QD1A_13", "QD1A_14", "QD1A_15", "QD1A_16")

i_safety<- dcc %>% select(ind_safety) 
i_safety <- replace_with_na_all(i_safety, condition = ~.x == 5) 
dcc$ind_safety <- i_safety

#Nepouzivat zscory z indexu, ale pouzit rovnou faktorove skore (psych::factor.scores) 

ind1_b1<- dcc %>% select(ind1_batt1) 
ind1_b1 <- replace_with_na_all(ind1_b1, condition = ~.x == 5) 
dcc$ind_techsavvy <- ind1_b1


ind2_b1 <- dcc %>% select(ind2_batt1)
ind2_b1 <- replace_with_na_all(ind2_b1, condition = ~.x == 5)
dcc$ind_frequser <- ind2_b1

ind3_b1 <- dcc %>% select(ind3_batt1)
ind3_b1 <- replace_with_na_all(ind3_b1, condition = ~.x == 5) 
dcc$ind_ethicuser <- ind3_b1

#battery2
battery_2 <- dcc[, 39:46]
check_factorstructure(battery_2)

nf2 <- parameters::n_factors(battery_2)
plot(nf2)

pca_a_b <- psych::pca(battery_2, nfactors = 1) %>%  model_parameters(sort = TRUE, threshold = "max")
pca_a_b

dcc$ind_compMedium <- dcc %>% select(QD1b_1:QD1b_8) %>% rowSums() %>% scale()

#battery3
battery_3 <- dcc[, 47:69]

check_factorstructure(battery_3)

nf3 <- parameters::n_factors(battery_3)
plot(nf3)

pca_a_c <- psych::pca(battery_3, nfactors = 4) %>% model_parameters(sort = T, threshold = "max")
pca_a_c

ind1_batt3 <- c("QH1_13", "QH1_10", "QH1_9", "QH1_11", "QH1_12", "QH1_14", "QH1_15")
ind2_batt3 <- c("QH1_3", "QH1_21", "QH1_22", "QH1_5", "QH1_20", "QH1_17", "QH1_19")
ind3_batt3 <- c("QH1_7", "QH1_18", "QH1_16", "QH1_8")
ind4_batt3 <- c("QH1_2", "QH1_6", "QH1_1", "QH1_4")


ind1_b3 <- dcc %>% select(ind1_batt3)
ind1_b3 <- replace_with_na_all(ind1_b3, condition = ~.x == 6) %>% rowSums() %>% scale()
dcc$ind_socd <- ind1_b3

ind2_b3 <- dcc %>% select(ind2_batt3)
ind2_b3 <- replace_with_na_all(ind2_b3, condition = ~.x == 6) %>% rowSums() %>% scale()
dcc$ind_culd <- ind2_b3

ind3_b3 <- dcc %>% select(ind3_batt3)
ind3_b3 <- replace_with_na_all(ind3_b3, condition = ~.x == 6) %>% rowSums() %>% scale()
dcc$ind_perd <- ind3_b3

ind4_b3 <- dcc %>% select(ind4_batt3)
ind4_b3 <- replace_with_na_all(ind4_b3, condition = ~.x == 6) %>% rowSums() %>% scale()
dcc$ind_ecod <- ind4_b3

cul_clusters <- dcc %>% select(ind_ecod, ind_culd, ind_perd, ind_socd)

#Dobry je balicek mice pro imputaci chybejicich hodnot
#Pro exploraci je fajn laniar



report(cul_clusters)

cul_clusters <- as.matrix(cul_clusters)

cul_clusters <- missForest(cul_clusters)

df_clus <- cul_clusters$ximp

report(df_clus)

df_clus <-as.matrix(df_clus)

#Kouknout se na data pred a po imputaci, ve scatteru si zvyraznit outliery
#Ivan Petrusek - analyza chybejicich hodnot
#Alternativa pro FA - item response theory






#battery4
battery_4 <- dcc[, 70:83]

battery_4 <- replace_with_na_all(battery_4, condition = ~.x == 6)
report(battery_4)

imp_b4 <- mice(battery_4, minbucket = 10, method = "cart", maxit = 3)
cimp_b4 <- merge_imputations(battery_4, imp_b4, summary = "hist")
df_b4 <- as.data.frame(cimp_b4["data"])

df_b4 <- df_b4 %>% select(-data.QPo1_12)

check_factorstructure(df_b4)

nf4 <- parameters::n_factors(df_b4)
plot(nf4)

fca_4 <- psych::fa(df_b4, nfactors = 4, rotate = "oblimin")

fca_a4 <- fca_4 %>% model_parameters(sort = TRUE, threshold = "max")
fca_a4

lfs4 <- factor.scores(df_b4, fca_4)
fs4 <- as.data.frame(lfs4["scores"])
colnames(fs4) <- c("fs_escape", "fs_tool", "fs_untrust", "fs_problem")

dcc <- cbind(dcc, fs4)





check_factorstructure(battery_4)

nf4 <- parameters::n_factors(battery_4)
plot(nf4)


ind1_batt4 <- c("QPo1_1", "QPo1_2", "QPo1_4", "QPo1_13", "QPo1_3")
ind2_batt4 <- c("QPo1_6", "QPo1_7", "QPo1_5")
ind3_batt4 <- c("QPo1_11", "QPo1_8", "QPo1_9")
ind4_batt4 <- c("QPo1_10", "QPo1_14")

ind1_b4 <- dcc %>% select(ind1_batt4)
ind1_b4 <- replace_with_na_all(ind1_b4, condition = ~.x == 6) %>% rowSums() %>% scale()
dcc$ind_escape <- ind1_b4


ind2_b4 <- dcc %>% select(ind2_batt4)
ind2_b4 <- replace_with_na_all(ind2_b4, condition = ~.x == 6) %>% rowSums() %>% scale()
dcc$ind_tool <- ind2_b4

ind3_b4 <- dcc %>% select(ind3_batt4)
ind3_b4 <- replace_with_na_all(ind3_b4, condition = ~.x == 6) %>% rowSums() %>% scale()
dcc$ind_problem <- ind3_b4

ind4_b4 <- dcc %>% select(ind4_batt4)
ind4_b4 <-  replace_with_na_all(ind4_b4, condition = ~.x == 6) %>% rowSums() %>% scale()
dcc$ind_unethical <- ind4_b4

clus_s <- dcc %>% select(starts_with("ind"))

clus_s <- as.data.table(clus_s)



clus_s <- clus_s[,(9:12):= NULL]

clus_s <- as.data.frame(clus_s)

clus_s <- clus_s[-c(102, 243, 390),]

clus_s <- as.matrix(clus_s)

#Imputing missing values
imp_data <-missForest(clus_s)

clus_s <- imp_data$ximp

clus_s <- as.data.frame(clus_s)


#Cluster analysis-----------------------

set.seed(123)
n <- n_clusters(clus_s, package = c("easystats", "NbClust", "mclust"))
plot(n)
n

rez_kmeans <- cluster_analysis(clus_s, n = 5, method = "kmeans")
rez_kmeans

rez_hkmeans <- cluster_analysis(clus_s, method = "hkmeans")
rez_hkmeans

rez_pam <- cluster_analysis(clus_s, method = "pam")
rez_pam

plot(summary(rez_kmeans))

dcc <- dcc[-c(102, 243, 390),]

dcc$clus_g2 <- predict(rez_hkmeans)
dcc$clus_g2 <- as.factor(dcc$clus_g2)

dcc$clus_g5 <- predict(rez_kmeans)
dcc$clus_g5 <- as.factor(dcc$clus_g5)


#EDA--------------------------

#rowtables
factors <- c("Gender", "Edu", "size_of_muni", "region", "work_stat_fulltime", "work_stat_parttime", "work_stat_parent", "work_stat_unemployed", "work_stat_enterp",
             "work_stat_student", "work_stat_retired", "mobile_user", "laptop_user", "tablet_user", "PC_user", "console_user", "smartTV_user")

dcc_factors <- dcc %>% select(factors)

dcc_factors[sapply(dcc_factors, is.numeric)] <- lapply(dcc_factors[sapply(dcc_factors, is.numeric)], 
                                       as.factor)

dcc_factors$clus_g2 <- cbind(dcc$clus_g2)
dcc_factors$clus_g5 <- cbind(dcc$clus_g5)

perc_clusters_2 <- lapply(dcc_factors, 
                        function(x){ 
                           return(prop.table(table(dcc_factors$clus_g2, x),
                                             margin = 1))
                        })

perc_clusters_5 <- lapply(dcc_factors, 
                          function(x){ 
                             return(prop.table(table(dcc_factors$clus_g5, x),
                                               margin = 1))
                          })

perc_clusters_2
perc_clusters_5

p_df <- dcc %>% select(ind_escape:ind_unethical, QV1_1:QV1_22, clus_g2, clus_g5)

#ANOVA------------------------------------------
#1
aov_age <- aov(Age ~ clus_g2, data = dcc)
report(aov_age)

#2
aov_yointernet <- aov(y_of_internetUsage ~ clus_g2, data = dcc)
report(aov_yointernet)

#3
aov_iescape <- aov(ind_escape ~ clus_g2, data = p_df)
report(aov_iescape)

#4 
aov_itool <- aov(ind_tool ~ clus_g2, data = p_df)
report(aov_itool)

#5
aov_iproblem <- aov(ind_problem ~ clus_g2, data = p_df)
report(aov_iproblem)

#6
aov_iunethical <- aov(ind_unethical ~ clus_g2, data = p_df)
report(aov_iunethical)

#plots
p1 <- ggplot(p_df, aes(clus_g2, ind_escape)) + geom_boxplot() + theme_bw()
p2 <- ggplot(p_df, aes(clus_g2, ind_tool)) + geom_boxplot() + theme_bw()
p3 <- ggplot(p_df, aes(clus_g2, ind_problem)) + geom_boxplot() + theme_bw()
p4 <- ggplot(p_df, aes(clus_g2, ind_unethical)) + geom_boxplot() + theme_bw()

grid_plot_2 <- grid.arrange(p1, p2, p3, p4, ncol = 2)

#ggplotstats
ap1 <- ggbetweenstats(p_df, clus_g2, ind_escape, title = "Distribution of Z-scored ESCAPE index across clusters")
ap2 <- ggbetweenstats(p_df, clus_g2, ind_tool, title = "Distribution of Z-scored TOOL index across clusters")

grid_plot_s2 <- grid.arrange(ap1, ap2, ncol = 1)



#activitiesplots
V1_names <- c("Checking e-mail", "Chatting", "Commenting on platforms", "Calling on platforms", "Sharing content", "Uploading content", "Debating on platforms",
              "Global news", "Information about products", "Memes and humour", "Looking for a job", "Information about traveling", "Information about health", "Reading blogs",
              "Local news", "Porn sites", "Piracy acitivites", "Playing games", "Watching video-content", "Surfing in general", "Betting", "Listening to music")

V1_factors <- dcc %>% select(QV1_1:QV1_22)

V1_factors[sapply(V1_factors, is.numeric)] <- lapply(V1_factors[sapply(V1_factors, is.numeric)], 
                                                       as.factor)
V1_factors[] <- lapply(V1_factors, factor, 
               levels=c(1,2,3,4,5,6),
               labels = c("Multiple times per day", "Atleast once a day", "Atleast once a week", "Atleast once a month", "Less than once a month", "Never"))

colnames(V1_factors) <- V1_names
report(V1_factors)


V1_factors %>%  
   gather(key = Activity, value = Answer) %>% 
   mutate(Answer = fct_relevel(Answer, "Multiple times per day", "Atleast once a day", "Atleast once a week", "Atleast once a month", "Less than once a month", "Never")) %>% 
   ggplot(aes(Activity)) +
   geom_bar(aes(fill = fct_rev(Answer)), position = "fill") + coord_flip() -> p1


V1_factors %>%  
   gather(key = Activity, value = Answer) %>% 
   mutate(Answer = fct_relevel(Answer, "Multiple times per day", "Atleast once a day", "Atleast once a week", "Atleast once a month", "Less than once a month", "Never")) %>%
   count(Activity, Answer) %>% 
   group_by(Activity) %>% 
   mutate(order_value = n[Answer == "Multiple times per day"]) %>%
   ungroup() %>% 
   mutate(Activity = fct_reorder(Activity, order_value),
          Answer = fct_rev(Answer)) %>% 
   ggplot(aes(x = n, y = Activity, fill = Answer)) +
   geom_col(position = "fill") +
   scale_fill_brewer(palette = "Spectral") +
   #scale_y_continuous(labels = scales::percent_format()) + 
   labs(title = "Frequency of on-line activities in czech population") + 
   guides(fill = guide_legend(title = "Frequency")) + 
   ylab("Percent of respondents") +
   theme_abyss()


V1_factors %>%  
   pivot_longer(cols = -c("Checking e-mail"), names_to = "Activity", values_to = "Answer")

p1 + scale_fill_brewer(palette = "Spectral") + scale_y_continuous(labels = scales::percent) + labs(title = "Frequency of on-line activities in czech population") + 
   guides(fill = guide_legend(title = "Frequency")) + 
   ylab("Percent of respondents") +
   theme_abyss()


#Co chybi - Jak dobre ta faktorova struktura replikuje korelacni matici (pouziva se RMSE, SRMR - kouknout na predpoklady (mira chybovosti > 0.06 moc vysoka, zkontrolovat))
#Jeste zkontrolovat, DLE TEORIE, jakou navaznost maji socioekodemo znaky na faktory
#Overit reliabilitu mereni - cronbach (nemelo by se pouzivat), Mcdonaldovo Omega, Hierarchicka verze techto nastroju, zase v balicku psych - hierarchicke verze cronabcha a Mcdonalda

#Balicek lavaan - muze udelat jeden metafaktor, vice kouknout 





