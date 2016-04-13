setwd("~/Dropbox/STAT222/projects/redwoods/data")

library(dplyr)
library(ggplot2)
### read in the file
mote_all <- read.csv("sonoma-data-all.csv", header = TRUE, stringsAsFactors = FALSE)
mote_loc <- read.table("mote-location-data.txt", header = TRUE, stringsAsFactors = FALSE)
#mote_net <- read.csv("sonoma-data-net.csv", header = TRUE)
#mote_all <- mote_all %>% filter(nodeid != 65535 & nodeid != 135 & nodeid != 100)
mote_all <- merge(mote_all, mote_loc, all.x = FALSE,
                  all.y = FALSE, by.x = "nodeid", by.y = "ID")
summary(mote_all)
mote_all$Tree <- as.factor(mote_all$Tree)
mote_all$Direc <- as.factor(mote_all$Direc)
table(mote_all$Dist)
mote_all <- mote_all %>% filter(epoch <= 10288 & Dist <= 1 & !is.na(humidity))


### ggplot to examine all the variables
## humid_adj
ggplot(mote_all) + geom_line(mapping = aes(x = epoch, y = humid_adj, color = as.factor(nodeid))) +
  scale_colour_discrete()
mote_all %>% filter(humid_adj < -100) %>% group_by(nodeid) %>% summarise(n=n())
ggplot(mote_all) + geom_line(mapping = aes(x = epoch, y = humid_adj, color = as.factor(nodeid))) +
  ylim(c(-20,125))
# filter out node 198 and 29; filter out the humid_adj oustside the reasonable range
mote_all <- mote_all %>% filter(humid_adj <= 100 & humid_adj >= 0)

## humid_temp
ggplot(mote_all) + geom_line(mapping = aes(x = epoch, y = humid_temp, color = as.factor(nodeid))) +
  scale_colour_discrete()
mote_all %>% filter(humid_temp > 50) %>% group_by(nodeid) %>% summarise(n=n()) # 78 and 141

## voltage
ggplot(mote_all[mote_all$voltage < 100 ,]) + geom_line(mapping = aes(x = epoch, y = voltage, color = as.factor(nodeid))) +
  scale_colour_discrete()
mote_all %>% filter(voltage < 1) %>% group_by(nodeid) %>% summarise(n=n())
volt_abnorm_ep_id <- mote_all %>% filter(voltage < 2.4) %>% select(epoch, nodeid)
# remove the abnormal node ids
mote_all <- anti_join(mote_all, volt_abnorm_ep_id, by = c("epoch", "nodeid"))
ggplot(mote_all) + geom_line(mapping = aes(x = epoch, y = humid_temp, color = as.factor(nodeid))) +
  scale_colour_discrete()
mote_all %>% filter(humid_temp > 40) %>% group_by(nodeid) %>% summarise(n=n())
node_78 <- mote_all %>% filter(nodeid == 78)
node_141 <- mote_all %>% filter(nodeid == 141)
ggplot(node_78) + geom_line(mapping = aes(x = epoch, y = humid_temp, color = as.factor(nodeid)))
ggplot(node_78) + geom_line(mapping = aes(x = epoch, y = voltage, color = as.factor(nodeid)))
ggplot(node_141) + geom_line(mapping = aes(x = epoch, y = humid_temp, color = as.factor(nodeid)))
ggplot(node_141) + geom_line(mapping = aes(x = epoch, y = voltage, color = as.factor(nodeid)))
ggplot(node_141) + geom_line(mapping = aes(x = epoch, y = humid_adj, color = as.factor(nodeid)))
hist(node_78$voltage)
ggplot(mote_all[mote_all$voltage > 100 & mote_all$voltage < 500,]) + geom_point(mapping = aes(x = voltage, y = humid_temp, color = as.factor(nodeid)))
ggplot(mote_all[mote_all$voltage > 100 & mote_all$voltage < 500 & mote_all$humid_temp < 50,]) + geom_point(mapping = aes(x = voltage, y = humid_temp, color = as.factor(nodeid)))
ggplot(mote_all[mote_all$voltage < 100,]) + geom_point(mapping = aes(x = voltage, y = humid_temp, color = as.factor(nodeid)))
ggplot(node_78[node_78$voltage > 100,]) + geom_point(mapping = aes(x = epoch, y = humid_temp, color = voltage))+
  scale_color_gradient(low="blue", high="red")
# interior
ggplot(mote_all[mote_all$Tree == "interior",])+ geom_line(mapping = aes(x = epoch, y = humid_temp, color = as.factor(nodeid))) +
  scale_colour_discrete()