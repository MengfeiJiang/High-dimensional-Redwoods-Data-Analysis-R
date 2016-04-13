setwd("~/Dropbox/STAT222/projects/redwoods/data")

library(dplyr)
library(ggplot2)
library(reshape2)
### read in the file
mote_net <- read.csv("sonoma-data-net.csv", header = TRUE)
mote_log <- read.csv("sonoma-data-log.csv", header = TRUE)
mote_all <- rbind(mote_log, mote_net)
# remove duplicated (epoch, nodeid) entries
mote_all <- mote_all %>% filter(!duplicated(rbind(mote_log[,2:3], mote_net[,2:3])))

mote_loc <- read.table("mote-location-data.txt", header = TRUE)
mote_int_loc <- mote_loc %>% filter(Tree == "interior")
# select only sensors on the interior tree
mote_all <- merge(mote_all, mote_int_loc, all.x = FALSE,
                  all.y = FALSE, by.x = "nodeid", by.y = "ID")
# count the original number of entries
# mote_ct_raw <- mote_all %>% filter(!is.na(humidity)) %>% group_by(nodeid) %>% summarise(n = n())
gg_mote_ct_raw <- ggplot(data = mote_all %>% filter(nodeid %in% nodes_cleaned$nodeid)) +
  geom_bar(mapping = aes(x = as.factor(Height))) +
  labs(x = "Nodes", y = "Number of data entries")

# examine voltage in the two datasets
ggplot(mote_net %>% filter(voltage < 1000)) +
  geom_density(aes(x=voltage),na.rm = TRUE)
ggplot(mote_log) +
  geom_density(aes(x=voltage),na.rm = TRUE)

# convert voltage in net
mote_dups <- merge(mote_log[,c("epoch","nodeid","voltage")], 
                   mote_net[,c("epoch","nodeid","voltage")],
                   by = c("epoch", "nodeid"), all.x = FALSE, all.y = FALSE)
mote_dups <- unique(mote_dups) %>% filter(voltage.x < 1000 & voltage.y < 1000)
volt_lm <- lm(voltage.x ~ voltage.y, data = mote_dups)
ggplot(mote_dups,mapping = aes(x = voltage.y, y = voltage.x, color = as.factor(nodeid))) + 
  geom_point(size = 0.7) +
  geom_smooth(method = lm, se=TRUE, size = 0.7, color = "black") +
  labs(x = "Voltage from net data", y = "Voltage from log data", color = "Nodes") +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 8))
mote_all$voltage[mote_all$voltage>100] <- coef(volt_lm)[1] + coef(volt_lm)[2] * mote_all$voltage[mote_all$voltage>100]
# mote_all %>% filter(voltage < 0) %>% group_by(nodeid) %>% summarise(n=n())

### examine and pre-process data
summary(mote_all)
## remove all NA's, Dist w/i 1m, day as or before 6/2
mote_all <- mote_all %>% 
  filter(!is.na(humidity) & Dist <= 1 & epoch <= max(mote_net$epoch) &
           humid_adj >= 0 & humid_adj <= 100) %>% 
  arrange(epoch, nodeid)

## remove outliers based on humid_temp & voltage
ggplot(mote_all) + geom_line(mapping = aes(x = epoch, y = humid_adj, color = as.factor(nodeid))) +
  scale_colour_discrete()
ggplot(mote_all) + geom_line(mapping = aes(x = epoch, y = humid_temp, group = as.factor(nodeid), color = as.factor(nodeid))) +
  scale_colour_discrete()
ggplot(mote_all) + geom_line(mapping = aes(x = epoch, y = voltage, color = as.factor(nodeid))) +
  scale_colour_discrete()
ggplot(mote_all) + 
  geom_boxplot(mapping = aes(x = as.factor(nodeid), y = voltage), 
               outlier.size = 0.5, outlier.colour = "#003366", fill = "#6699CC") +
  ylim(1.9, 3) +
  labs(x = "Node ID", y = "Voltage")

# temperature trends
node_78 <- mote_all %>% filter(nodeid == 78)
node_141 <- mote_all %>% filter(nodeid == 141)
node_134 <- mote_all %>% filter(nodeid == 134)
node_109 <- mote_all %>% filter(nodeid == 109)
node_138 <- mote_all %>% filter(nodeid == 138)
out <- 
ggplot(data = NULL, mapping = aes(x = cvt_time(epoch), y = humid_temp)) + 
  geom_line(data = mote_all %>% filter(!nodeid %in% c(78,141,134,138,109)),
            mapping = aes(color = "0"), size = 2) +
  geom_line(data = node_78, mapping = aes(color = as.factor(nodeid)), size = 0.5) +
  geom_line(data = node_141, mapping = aes(color = as.factor(nodeid)), size = 0.5) +
  geom_line(data = node_134, mapping = aes(color = as.factor(nodeid)), size = 0.5) +
  geom_line(data = node_138, mapping = aes(color = as.factor(nodeid)), size = 0.5) +
  geom_line(data = node_109, mapping = aes(color = as.factor(nodeid)), size = 0.5) +
  geom_vline(xintercept = as.numeric(cvt_time(node_78_maxdate[2])),
             linetype=4) +
  scale_colour_manual(breaks = c("0", "78", "109", "134","138","141"),
                      labels=c("Others", "78", "109","134", "138","141"),
                      values = c("grey", "chocolate2", "red", "royalblue1", "olivedrab4", "lightseagreen")) +
  labs(color = "Nodes", x = "Date", y = "Temperature")
  # annotate('text',x = as.POSIXct("2004-05-06 00:00:00"),y=115,label='node 78',size=4) +
  # annotate('text',x = as.POSIXct("2004-05-26 12:00:00"),y=115,label='node 141',size=4)
out +
  annotate('text',x = as.POSIXct("2004-05-20 12:30:00"),y=1,
           label='Node 78 last log reading: 05-07 14:30',size=3.5)+
  geom_segment(aes(xend = as.POSIXct("2004-05-07 20:00:00"), yend = 1, 
                   x = as.POSIXct(c("2004-05-10 06:30:00")), 
                   y = 1), colour='black', size=0.5,
               arrow = arrow(length = unit(0.15, "cm"))) +
  geom_vline(xintercept = as.numeric(cvt_time(node_141_maxdate[2])),
             linetype=4, size = 0.4) +
  annotate('text',x = as.POSIXct("2004-05-18 00:30:00"),y=75,
           label='Node 141 last log reading:\n05-26 11:20',size=3.5)+
  geom_segment(aes(xend = as.POSIXct("2004-05-26 03:20:00"), yend = 73, 
                   x = as.POSIXct(c("2004-05-23 16:30:00")), 
                   y = 73), colour='black', size=0.5,
               arrow = arrow(length = unit(0.15, "cm")))

# humid_adj
ggplot(data = NULL, mapping = aes(x = cvt_time(epoch), y = humid_adj)) + 
  geom_line(data = mote_all %>% filter(!nodeid %in% c(78,141,134,138,109)),
            mapping = aes(color = "0"), size = 2) +
  geom_line(data = node_78, mapping = aes(color = as.factor(nodeid)), size = 0.5) +
  geom_line(data = node_141, mapping = aes(color = as.factor(nodeid)), size = 0.5) +
  geom_line(data = node_134, mapping = aes(color = as.factor(nodeid)), size = 0.5) +
  geom_line(data = node_138, mapping = aes(color = as.factor(nodeid)), size = 0.5) +
  geom_line(data = node_109, mapping = aes(color = as.factor(nodeid)), size = 0.5) +
  geom_vline(xintercept = as.numeric(cvt_time(node_78_maxdate[2])),
             linetype=4) +
  scale_colour_manual(breaks = c("0", "78", "109", "134","138","141"),
                      labels=c("Other nodes", "78", "109","134", "138","141"),
                      values = c("grey", "chocolate2", "red", "royalblue1", "olivedrab4", "lightseagreen")) +
  labs(color = "Nodes", x = "Date", y = "Humidity") +
  annotate('text',x = as.POSIXct("2004-05-21 00:30:00"),y=1,
           label='Node 78 last log reading: 05-07 14:30',size=3.5)+
  geom_segment(aes(xend = as.POSIXct("2004-05-07 20:00:00"), yend = 1, 
                   x = as.POSIXct(c("2004-05-10 06:30:00")), 
                   y = 1), colour='black', size=0.5,
               arrow = arrow(length = unit(0.15, "cm"))) +
  geom_vline(xintercept = as.numeric(cvt_time(node_141_maxdate[2])),
             linetype=4, size = 0.4) +
  annotate('text',x = as.POSIXct("2004-05-17 00:30:00"),y=75,
           label='Node 141 last log reading:\n05-26 11:20',size=3.5)+
  geom_segment(aes(xend = as.POSIXct("2004-05-26 03:20:00"), yend = 73, 
                   x = as.POSIXct(c("2004-05-23 16:30:00")), 
                   y = 73), colour='black', size=0.5,
               arrow = arrow(length = unit(0.15, "cm")))
# incident PAR
ggplot(data = NULL, mapping = aes(x = cvt_time(epoch), y = hamatop)) + 
  geom_line(data = mote_all %>% filter(!nodeid %in% c(78,141,134,138,109)),
            mapping = aes(color = "0"), size = 2) +
  geom_line(data = node_78, mapping = aes(color = as.factor(nodeid)), size = 0.5) +
  geom_line(data = node_141, mapping = aes(color = as.factor(nodeid)), size = 0.5) +
  geom_line(data = node_134, mapping = aes(color = as.factor(nodeid)), size = 0.5) +
  geom_line(data = node_138, mapping = aes(color = as.factor(nodeid)), size = 0.5) +
  geom_line(data = node_109, mapping = aes(color = as.factor(nodeid)), size = 0.5) +
  geom_vline(xintercept = as.numeric(cvt_time(node_78_maxdate[2])),
             linetype=4) +
  scale_colour_manual(breaks = c("0", "78", "109", "134","138","141"),
                      labels=c("Other nodes", "78", "109","134", "138","141"),
                      values = c("grey", "chocolate2", "red", "royalblue1", "olivedrab4", "lightseagreen")) +
  labs(color = "Nodes", x = "Date", y = "Incident PAR") +
  annotate('text',x = as.POSIXct("2004-05-21 00:30:00"),y=1,
           label='Node 78 last log reading: 05-07 14:30',size=3.5)+
  geom_segment(aes(xend = as.POSIXct("2004-05-07 20:00:00"), yend = 1, 
                   x = as.POSIXct(c("2004-05-10 06:30:00")), 
                   y = 1), colour='black', size=0.5,
               arrow = arrow(length = unit(0.15, "cm"))) +
  geom_vline(xintercept = as.numeric(cvt_time(node_141_maxdate[2])),
             linetype=4, size = 0.4) +
  annotate('text',x = as.POSIXct("2004-05-17 00:30:00"),y=75,
           label='Node 141 last log reading:\n05-26 11:20',size=3.5)+
  geom_segment(aes(xend = as.POSIXct("2004-05-26 03:20:00"), yend = 73, 
                   x = as.POSIXct(c("2004-05-23 16:30:00")), 
                   y = 73), colour='black', size=0.5,
               arrow = arrow(length = unit(0.15, "cm")))
# reflected PAR
ggplot(data = NULL, mapping = aes(x = cvt_time(epoch), y = hamabot)) + 
  geom_line(data = mote_all %>% filter(!nodeid %in% c(78,141,134,138,109)),
            mapping = aes(color = "0"), size = 2) +
  geom_line(data = node_78, mapping = aes(color = as.factor(nodeid)), size = 0.5) +
  geom_line(data = node_141, mapping = aes(color = as.factor(nodeid)), size = 0.5) +
  geom_line(data = node_134, mapping = aes(color = as.factor(nodeid)), size = 0.5) +
  geom_line(data = node_138, mapping = aes(color = as.factor(nodeid)), size = 0.5) +
  geom_line(data = node_109, mapping = aes(color = as.factor(nodeid)), size = 0.5) +
  geom_vline(xintercept = as.numeric(cvt_time(node_78_maxdate[2])),
             linetype=4) +
  scale_colour_manual(breaks = c("0", "78", "109", "134","138","141"),
                      labels=c("Other nodes", "78", "109","134", "138","141"),
                      values = c("grey", "chocolate2", "red", "royalblue1", "olivedrab4", "lightseagreen")) +
  labs(color = "Nodes", x = "Date", y = "Reflected PAR") +
  annotate('text',x = as.POSIXct("2004-05-21 00:30:00"),y=1,
           label='Node 78 last log reading: 05-07 14:30',size=3.5)+
  geom_segment(aes(xend = as.POSIXct("2004-05-07 20:00:00"), yend = 1, 
                   x = as.POSIXct(c("2004-05-10 06:30:00")), 
                   y = 1), colour='black', size=0.5,
               arrow = arrow(length = unit(0.15, "cm"))) +
  geom_vline(xintercept = as.numeric(cvt_time(node_141_maxdate[2])),
             linetype=4, size = 0.4) +
  annotate('text',x = as.POSIXct("2004-05-17 00:30:00"),y=75,
           label='Node 141 last log reading:\n05-26 11:20',size=3.5)+
  geom_segment(aes(xend = as.POSIXct("2004-05-26 03:20:00"), yend = 73, 
                   x = as.POSIXct(c("2004-05-23 16:30:00")), 
                   y = 73), colour='black', size=0.5,
               arrow = arrow(length = unit(0.15, "cm")))

mote_all %>% filter(voltage < 0) %>% group_by(nodeid) %>% summarise(n=n())

ggplot(mote_all) + 
  geom_line(mapping = aes(x = cvt_time(epoch), y = voltage, color = as.factor(nodeid))) +
  ylim(1.8, 3.2) +
  scale_colour_discrete()

# Upper part of voltage
node_78_maxdate <- mote_log %>% filter(nodeid == 78)%>% group_by(nodeid) %>% 
  summarise(maxdate = max(epoch)) %>% as.numeric()
names(node_78_maxdate) <- c("", "epoch")
ggplot(data = NULL, mapping = aes(x = cvt_time(epoch), y = voltage)) + 
  geom_line(data = mote_all %>% filter(!nodeid %in% c(78,141,134,138,109)),
            mapping = aes(color = "0"), size = 1) +
  geom_line(data = node_78, mapping = aes(color = as.factor(nodeid)), size = 0.5) +
  geom_line(data = node_138, mapping = aes(color = as.factor(nodeid)), size = 0.5) +
  geom_line(data = node_109, mapping = aes(color = as.factor(nodeid)), size = 0.5) +
  geom_vline(xintercept = as.numeric(cvt_time(node_78_maxdate[2])),
             linetype=4) +
  scale_colour_manual(breaks = c("0", "78", "109", "138"),
                      labels=c("Others excl.\n134 & 141", "78", "109","138"),
                      values = c("grey", "chocolate2", "royalblue1", "lightseagreen")) +
  labs(color = "Nodes", x = "Date", y = "Voltage") +
  annotate('text',x = as.POSIXct("2004-05-20 20:30:00"),y=1.9,
           label='Node 78 last log reading: 05-07 14:30',size=3.5)+
  geom_segment(aes(xend = as.POSIXct("2004-05-07 20:00:00"), yend = 1.9, 
                   x = as.POSIXct(c("2004-05-10 06:30:00")), 
                   y = 1.9), colour='black', size=0.5,
               arrow = arrow(length = unit(0.15, "cm")))


# Lower part of voltage
node_141_maxdate <- mote_log %>% filter(nodeid == 141)%>% group_by(nodeid) %>% 
  summarise(maxdate = max(epoch)) %>% as.numeric()
ggplot(data = NULL, mapping = aes(x = cvt_time(epoch))) + 
  geom_line(data = node_134, mapping = aes(y = voltage + runif(nrow(node_134), 0,0.3), color = as.factor(nodeid)), size = 0.1) +
  geom_line(data = node_141, mapping = aes(y = voltage + runif(nrow(node_141), 0,0.3), color = as.factor(nodeid)), size = 0.1) +
  geom_vline(xintercept = as.numeric(cvt_time(node_141_maxdate[2])),
             linetype=4, size = 0.4) +
  scale_colour_manual(breaks = c("134", "141"),
                      labels=c("134   ", "141   "),
                      values = c("red", "olivedrab4")) +
  labs(color = "Nodes", x = "Date", y = "Voltage") +
  annotate('text',x = as.POSIXct("2004-05-14 00:30:00"),y=-6.5,
           label='Node 141 last log reading: 05-26 11:20',size=3.5)+
  geom_segment(aes(xend = as.POSIXct("2004-05-26 03:20:00"), yend = -6.5, 
                   x = as.POSIXct(c("2004-05-23 16:30:00")), 
                   y = -6.5), colour='black', size=0.5,
               arrow = arrow(length = unit(0.15, "cm")))

mote_all %>% filter(voltage < 2.25) %>% group_by(nodeid) %>% summarise(n=n())
mote_int %>% filter(voltage < 2.75, epoch < 600) %>% group_by(nodeid) %>% 
  summarise(min_volt = min(voltage)) %>% arrange(min_volt)
mote_all %>% filter(voltage < 0) %>% group_by(nodeid) %>% summarise(n=n())



# examine nodes 78 & 141 & 134 & 138 & 109
abnorm_volt_motes <- mote_all %>% filter(nodeid %in% c(78,141,134,138,109))
ggplot(abnorm_volt_motes, mapping = aes(x = epoch, y = humid_temp, color = voltage, shape = as.factor(nodeid))) +
  geom_point() +
  scale_shape_manual(breaks = c("78", "109", "134", "138", "141"), values=c(16, 3, 1, 2, 17)) +
  scale_color_gradient(low="blue", high="red")
ggplot(abnorm_volt_motes, mapping = aes(x = epoch, y = humid_temp, color = as.factor(nodeid))) +
  geom_line()

ggplot( mote_all %>% filter(nodeid == 78)) + geom_line(mapping = aes(x = epoch, y = voltage, color = as.factor(nodeid))) +
  geom_line(data = mote_all %>% filter(nodeid == 141), mapping = aes(x = epoch, y = voltage, color = as.factor(nodeid)))
# relationship between voltage & temp => use only log data for the two nodes
max_log_ep_78 <- mote_log %>% filter(nodeid == 78) %>% select(epoch) %>% max()
max_log_ep_141 <- mote_log %>% filter(nodeid == 141) %>% select(epoch) %>% max()
mote_all <- mote_all %>% filter(nodeid != 78 | epoch <= max_log_ep_78) %>%
  filter(nodeid != 141 | epoch <= max_log_ep_141)

# convert epoch
cvt_time <- function(epoch) {
  origin_epoch <- 2
  result <- strptime("2004-4-27 17:15:00", format = "%Y-%m-%d %H:%M:%S")
  result$min <- result$min + 5 * (epoch - origin_epoch)
  return(result)
}
mote_all$actual_time <- cvt_time(mote_all$epoch)
write.csv(mote_all, file = "sonoma-data-interior",row.names = FALSE, quote=FALSE)

# entry count after cleaning
nodes_cleaned <- mote_all[,-16] %>% group_by(nodeid) %>% summarise(n = n())
ggplot(data = mote_all) +
  geom_bar(aes(x = as.factor(Height))) +
  labs(x = "Nodes", y = "Number of data entries")


## examine hamatop & hamabot
mean_tmp <- mote_all[mote_all$actual_time >= as.POSIXct("2004-05-01 00:00:00") & mote_all$actual_time <= as.POSIXct("2004-05-02 00:00:00"),] %>%
  select(hamatop,epoch) %>% group_by(epoch) %>% summarise(avg = mean(hamatop))
ggplot(mote_all) + geom_point(mapping = aes(x = actual_time, y = hamatop), size = 0.4, color = "orange") +
  geom_line(data = mean_tmp, mapping = aes(x = cvt_time(epoch), y = avg)) +
  xlim(as.POSIXct("2004-05-01 00:00:00"),as.POSIXct("2004-05-02 00:00:00")) +
  labs(x = "Time", y = "Incident PAR")
  scale_colour_discrete()
ggplot(mote_all) + geom_line(mapping = aes(x = actual_time, y = hamabot, color = as.factor(nodeid))) +
  xlim(cvt_time(2), cvt_time(286)) +
  scale_colour_discrete()
ggplot(mote_all) + geom_boxplot(aes(x=as.factor(nodeid), y = hamatop))

ggplot(mote_all) + 
  geom_point(mapping = aes(x = actual_time, y = 1 + hamabot/100, color = Height), 
                              size = 0.4) +
  labs(x = "Time", y = "Incident PAR") +
  scale_y_log10() +
  scale_color_gradient(low="brown", high = "skyblue")

# hamatop vs. hamabot
cor(mote_all$hamatop, mote_all$hamabot)
ggplot(mote_all) + 
  geom_point(mapping = aes(x = hamatop, y = hamabot, color = Height), 
             size = 0.4) +
  scale_color_gradient(low="brown", high = "skyblue") +
  labs(x = "Incident PAR", y = "Reflected PAR")
