library(qgraph)
library(tidyverse)
library(psych)
library(psychonetrics)

data(bfi)
head(bfi)


#Data manipulation, exploratory analysis
d <- bfi[,1:25]

d$A1 <- abs(d$A1 - 7)

d$C4 <- abs(d$C4 - 7)
d$C5 <- abs(d$C5 - 7)

d$E1 <- abs(d$E1 - 7)
d$E2 <- abs(d$E2 - 7)

d$O2 <- abs(d$O2 - 7)
d$O5 <- abs(d$O5 - 7)

efa <- fa(d, 5, rotate = "oblimin")

corPlot(efa)
fa.diagram(efa)

library(psychonetrics)

model <- ggm(d)

model <- model %>% runmodel()

net <- getmatrix(model, "omega")

# plot the network:
Label <- c("A1","A2","A3","A4","A5",
           "C1","C2","C3","C4","C5",
           "E1","E2","E3","E4","E5",
           "N1","N2","N3","N4","N5",
           "O1","O2","O3","O4","O5")

Color <- c("red","red","red","red","red",
           "green","green","green","green","green",
           "yellow","yellow","yellow","yellow","yellow",
           "blue","blue","blue","blue","blue",
           "violet","violet","violet","violet","violet")

L<-averageLayout(net)


graph_net<-qgraph(net, cut=0,layout = L, 
                  title.cex = 2,title = "Big Five Inventory network (n = 2800)", 
                  labels = Label,
                  color = Color, esize=10, shape = "rectangle", vsize = 8, vsize2 = 5,
                  label.cex=2, 
                  theme="colorblind", # change to colorblind, gray, Fried, classic, Reddit, Borkulo
                  curveAll=T) 

# prune the network:
model2 <- model %>% prune(alpha = 0.01, recursive = FALSE)
net_pruned <- getmatrix(model2, "omega")
qgraph(net_pruned, cut=0,layout = L, # change to spring.cycle
       title.cex = 2,title = "Big Five Inventory network (n = 2800)", 
       labels = Label,
       color = Color, esize=10, shape = "rectangle", vsize = 8, vsize2 = 5,
       label.cex=2, 
       theme="colorblind", # change to colorblind, gray, Fried, classic, Reddit, Borkulo
       curveAll = F)

#Model fit
model %>% fit
model2 %>% fit

model2 %>% parameters
net_pruned




