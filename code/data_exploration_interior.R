setwd("~/Dropbox/STAT222/projects/redwoods/data")

library(ggplot2)
library(reshape2)
library(dplyr)
library(gridExtra)
library(Rmisc)
library(scales)
### data exploration
mote_int <- read.csv("../data/sonoma-data-interior", header = TRUE)
mote_int$actual_time <- as.POSIXct(mote_int$actual_time)
summary(mote_int)
## count the number of entries
ggplot(data = mote_int) +
  geom_bar(data = mote_int, aes(x = as.factor(Height)), fill = "blue", alpha = 0.5) +
  labs(x = "Nodes", y = "Number of data entries")
fill_mote_all <- rep("all", nrow(mote_all))
fill_mote_int <- rep("int", nrow(mote_int))
cols <- c("Before cleaning"="darkgrey", "After cleaning"="royalblue1")
ggplot(data = NULL) +
  geom_bar(data = mote_all, mapping = aes(x = as.factor(Height), fill = "Before cleaning"), alpha = 1) +
  geom_bar(data = mote_int, mapping = aes(x = as.factor(Height), fill = "After cleaning"), alpha = 0.5) +
  labs(x = "Nodes", y = "Number of data entries") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  scale_fill_manual(name = NULL, values = cols) +
  guides(fill = guide_legend()) +
  theme(legend.position=c(0.15, 0.9), 
        legend.background = element_rect(fill = "transparent"),
        legend.key.size = unit(5, "mm"))

tmp <- mote_all %>% filter(Direc %in% unique(mote_all$Direc)) %>% group_by(Height) %>% summarise(n=n())

## Dist of number of data entries with Height
p1 <- ggplot(data = mote_int, 
              aes(x=actual_time,fill=as.factor(Height), order = -as.factor(Height))) +
  geom_histogram(binwidth = 86400, alpha = 0.9) + 
  labs(x = "Date", y = "Number of data entries", fill = "Height") + 
  theme(legend.key.size = unit(0.35, "cm"),
        legend.margin = unit(0, "cm")) +
  geom_vline(xintercept = as.numeric(as.POSIXct(c("2004/05/06 17:15:00",
                                                  "2004/05/10 17:15:00",
                                                  "2004/05/25 17:15:00"))),
             linetype = 4) +
  annotate('text',x = as.POSIXct("2004/05/04 08:15:00"),y=0,
           label="May 7",size=3.5) +
  annotate('text',x = as.POSIXct("2004/05/11 08:15:00"),y=0,
           label="May 11",size=3.5) + 
  annotate('text',x = as.POSIXct("2004/05/25 08:15:00"),y=0,
           label="May 26",size=3.5)

p2 <- mote_int %>% 
  ggplot(mapping = aes(x = actual_time, y = as.factor(Height), 
                       color = Direc)) +
  geom_point() +
  scale_color_manual(values = direc_fill) +
  geom_vline(xintercept = as.numeric(as.POSIXct(c("2004/05/06 17:15:00",
                                                  "2004/05/10 17:15:00",
                                                  "2004/05/25 17:15:00"))),
             linetype = 4) +
  annotate('text',x = as.POSIXct("2004/05/04 08:15:00"),y=20.3,
           label="May 7",size=3.5) +
  annotate('text',x = as.POSIXct("2004/05/11 08:15:00"),y=20.3,
           label="May 11",size=3.5) + 
  annotate('text',x = as.POSIXct("2004/05/25 08:15:00"),y=20.3,
           label="May 26",size=3.5) +
  labs(x = "Date", y = "Height", color = "Direction") + 
  theme(legend.key.size = unit(0.35, "cm"),
        legend.margin = unit(0, "cm"))

# Dist of data with Direc
direc_fill <- c("ESE"="chocolate1", "S"="lightskyblue3", "SW"="#6699CC", "WSW"="steelblue4")
ggplot(data = mote_int, 
       aes(x=actual_time,fill=as.factor(Direc))) +
  geom_histogram(binwidth = 86400, alpha = 0.9) + 
  labs(x = "Date", y = "Number of data entries", fill = "Direction") + 
  theme(legend.key.size = unit(0.35, "cm"),
        legend.margin = unit(0, "cm")) +
  geom_vline(xintercept = as.numeric(as.POSIXct(c("2004/05/06 17:15:00",
                                                  "2004/05/10 17:15:00",
                                                  "2004/05/25 17:15:00"))),
             linetype = 4) +
  annotate('text',x = as.POSIXct("2004/05/04 08:15:00"),y=0,
           label="May 7",size=3.5) +
  annotate('text',x = as.POSIXct("2004/05/11 08:15:00"),y=0,
           label="May 11",size=3.5) + 
  annotate('text',x = as.POSIXct("2004/05/25 08:15:00"),y=0,
           label="May 26",size=3.5) +
  scale_fill_manual(values = direc_fill)

# examine what happens on May 11
p1 <- mote_int %>% filter((actual_time >= as.POSIXct("2004/05/11 00:00:00") &
                    actual_time <= as.POSIXct("2004/05/11 23:55:00")) |
                      (actual_time >= as.POSIXct("2004/05/01 00:00:00") &
                         actual_time <= as.POSIXct("2004/05/01 23:55:00"))) %>%
  mutate(date = format(actual_time, "%Y/%m/%d"),
         time = format(actual_time, "%H:%M:%S")) %>%
  ggplot(mapping = aes(x = strptime(time, format = "%H:%M:%S", tz = 'PST'), y = humid_temp, 
                       color = Height, order = -Height)) +
  scale_color_continuous(low = "coral4", high = "lightskyblue", guide = FALSE) + 
  geom_point(size = 0.1) +
  facet_grid(. ~ date) +
  labs(y = "Temperature") +
  scale_x_datetime(breaks=date_breaks('2 hour'),
                   labels=date_format('%H:%M', tz = 'PST')) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5),
        axis.title.x = element_blank())

p2 <- mote_int %>% filter((actual_time >= as.POSIXct("2004/05/11 00:00:00") &
                       actual_time <= as.POSIXct("2004/05/11 23:55:00")) |
                      (actual_time >= as.POSIXct("2004/05/01 00:00:00") &
                         actual_time <= as.POSIXct("2004/05/01 23:55:00"))) %>%
  mutate(date = format(actual_time, "%Y/%m/%d"),
         time = format(actual_time, "%H:%M:%S")) %>%
  ggplot(mapping = aes(x = strptime(time, format = "%H:%M:%S", tz = 'PST'), y = humid_adj, 
                       color = Height, order = -Height)) +
  scale_color_continuous(low = "coral4", high = "lightskyblue", guide = FALSE) + 
  geom_point(size = 0.1) +
  facet_grid(. ~ date) +
  labs(y = "Adjusted humidity") +
  scale_x_datetime(breaks=date_breaks('2 hour'),
                   labels=date_format('%H:%M', tz = 'PST')) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5),
        axis.title.x = element_blank())

p3 <- mote_int %>% filter((actual_time >= as.POSIXct("2004/05/11 06:00:00") &
                       actual_time <= as.POSIXct("2004/05/11 21:00:00")) |
                      (actual_time >= as.POSIXct("2004/05/01 06:00:00") &
                         actual_time <= as.POSIXct("2004/05/01 21:00:00"))) %>%
  mutate(date = format(actual_time, "%Y/%m/%d"),
         time = format(actual_time, "%H:%M:%S")) %>%
  ggplot(mapping = aes(x = strptime(time, format = "%H:%M:%S", tz = 'PST'), y = hamatop, 
                       color = Height, order = -Height)) +
  scale_color_continuous(low = "coral4", high = "lightskyblue", guide = FALSE) + 
  geom_point(size = 0.1) +
  facet_grid(. ~ date) +
  coord_trans(y = "sqrt") +
  labs(y = "Incident PAR") +
  scale_x_datetime(breaks=date_breaks('2 hour'),
                   labels=date_format('%H:%M', tz = 'PST')) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5),
        axis.title.x = element_blank())

p4 <- mote_int %>% filter((actual_time >= as.POSIXct("2004/05/11 06:00:00") &
                       actual_time <= as.POSIXct("2004/05/11 21:00:00")) |
                      (actual_time >= as.POSIXct("2004/05/01 06:00:00") &
                         actual_time <= as.POSIXct("2004/05/01 21:00:00"))) %>%
  mutate(date = format(actual_time, "%Y/%m/%d"),
         time = format(actual_time, "%H:%M:%S")) %>%
  ggplot(mapping = aes(x = strptime(time, format = "%H:%M:%S", tz = 'PST'), y = hamabot, 
                       color = Height)) +
  scale_color_continuous(low = "coral4", high = "lightskyblue", guide = FALSE) + 
  geom_point(size = 0.1) +
  facet_grid(. ~ date) +
  coord_trans(y = "sqrt") +
  labs(x = "Time", y = "Reflected PAR") +
  scale_x_datetime(breaks=date_breaks('2 hour'),
                   labels=date_format('%H:%M', tz = 'PST')) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5))

## 4 variables of all data
p5 <- mote_int %>%
  ggplot(mapping = aes(x = actual_time, y = humid_temp, color = Height)) +
  geom_point(size = 0.1) +
  scale_color_continuous(low = "coral4", high = "lightskyblue") +
  labs(y = "Temperature") +
  theme(axis.title.x = element_blank())
p6 <- mote_int %>%
  ggplot(mapping = aes(x = actual_time, y = humid_adj, color = Height)) +
  geom_point(size = 0.1) +
  scale_color_continuous(low = "coral4", high = "lightskyblue") +
  labs(y = "Adjusted humidity") +
  theme(axis.title.x = element_blank())
p7 <- mote_int %>%
  ggplot(mapping = aes(x = actual_time, y = hamatop, color = Height)) +
  geom_point(size = 0.1) +
  scale_color_continuous(low = "coral4", high = "lightskyblue") +
  coord_trans(y = "sqrt") +
  labs(y = "Incident PAR") +
  theme(axis.title.x = element_blank())
p8 <- mote_int %>%
  ggplot(mapping = aes(x = actual_time, y = hamabot, color = Height)) +
  geom_point(size = 0.1) +
  scale_color_continuous(low = "coral4", high = "lightskyblue") +
  coord_trans(y = "sqrt") +
  labs(x = "Date", y = "Reflected PAR")

multiplot(p1,p5,p2,p6,p3,p7,p4,p8, 
          layout = matrix(c(1,1,2,2,
                            3,3,4,4,
                            5,5,6,6,
                            7,7,8,8), nrow=4, byrow=TRUE))

## Temperal trend of spatial gradients
# temperature
int_melt <- mote_int %>% 
  mutate(actual_hours = format(actual_time, "%H:%M:%S")) %>%
  select(humid_temp, humid_adj, hamatop, hamabot, Height, actual_hours) %>%
  melt(id = c("Height", "actual_hours"))
int_melt_mean_temp <- dcast(int_melt %>% filter(variable == "humid_temp"), Height + actual_hours ~ variable,
                       fun.aggregate = mean)
p1 <- int_melt_mean_temp %>%
  ggplot() + 
  geom_tile(mapping = aes(x=strptime(actual_hours, format = "%H:%M:%S", tz = 'PST'),
                            y=as.factor(Height), fill = humid_temp)) +
  scale_fill_gradient(low = "gold1", high = "red3") +
  scale_x_datetime(breaks=date_breaks('2 hour'),
                   labels=date_format('%H:%M', tz = 'PST')) +
  labs(y = "Height", fill = "Temperature") +
  theme(axis.text.x = element_text(angle = 90, size = 10, hjust =1, vjust = 0.5),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 16),
        legend.title = element_text(size =14),
        legend.key.size = unit(0.5, "cm"),
        legend.margin = unit(0, "cm"))

# humidity
int_melt_mean_humid <- dcast(int_melt %>% filter(variable == "humid_adj"), Height + actual_hours ~ variable,
                            fun.aggregate = mean)
p2 <- int_melt_mean_humid %>%
  ggplot() + 
  geom_tile(mapping = aes(x=strptime(actual_hours, format = "%H:%M:%S", tz = 'PST'),
                          y=as.factor(Height), fill = humid_adj)) +
  scale_fill_gradient(low = "lightblue1", high = "dodgerblue4") +
  scale_x_datetime(breaks=date_breaks('2 hour'),
                   labels=date_format('%H:%M', tz = 'PST')) +
  labs(y = "Height", fill = "Humidity       ") +
  theme(axis.text.x = element_text(angle = 90, size = 10, hjust =1, vjust = 0.5),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 16),
        legend.title = element_text(size =14),
        legend.key.size = unit(0.5, "cm"),
        legend.margin = unit(0, "cm"))

# hamatop
int_melt_mean_hamatop <- dcast(int_melt %>% filter(variable == "hamatop"), Height + actual_hours ~ variable,
                             fun.aggregate = mean, na.rm = TRUE)
p3 <- int_melt_mean_hamatop %>%
  ggplot() + 
  geom_tile(mapping = aes(x=strptime(actual_hours, format = "%H:%M:%S", tz = 'PST'),
                          y=as.factor(Height), fill = hamatop)) +
  scale_fill_gradient(low = "midnightblue", high = "gold1") +
  scale_x_datetime(breaks=date_breaks('2 hour'),
                   labels=date_format('%H:%M', tz = 'PST')) +
  labs(x = "Time", y = "Height", fill = "Incident PAR") +
  theme(axis.text.x = element_text(angle = 90, size = 10, hjust =1, vjust = 0.5),
        axis.text.y = element_text(size = 10),
        axis.title = element_text(size = 16),
        legend.title = element_text(size =14),
        legend.key.size = unit(0.5, "cm"),
        legend.margin = unit(0, "cm"))

# hamabot
int_melt_mean_hamabot <- dcast(int_melt %>% filter(variable == "hamabot"), Height + actual_hours ~ variable,
                               fun.aggregate = mean, na.rm = TRUE)
p4 <- int_melt_mean_hamabot %>%
  ggplot() + 
  geom_tile(mapping = aes(x=strptime(actual_hours, format = "%H:%M:%S", tz = 'PST'),
                          y=as.factor(Height), fill = hamabot)) +
  scale_fill_gradient(low = "midnightblue", high = "gold1") +
  scale_x_datetime(breaks=date_breaks('2 hour'),
                   labels=date_format('%H:%M', tz = 'PST')) +
  labs(x = "Time", y = "Height", fill = "Reflected PAR") +
  theme(axis.text.x = element_text(angle = 90, size = 10, hjust =1, vjust = 0.5),
        axis.text.y = element_text(size = 10),
        axis.title = element_text(size = 16),
        legend.title = element_text(size =14),
        legend.key.size = unit(0.5, "cm"),
        legend.margin = unit(0, "cm"))
multiplot(p1,p3,p2,p4,cols = 2)


### Findings
## Weekly trend of average spatial gradients
get_week <- function(epoch) {
  return(as.character(epoch %/% 2016))
}

week_labeller <- c("0"="04/27 - 05/03",
                   "1"="05/04 - 05/10",
                   "2"="05/11 - 05/17",
                   "3"="05/18 - 05/24",
                   "4"="05/25 - 05/31",
                   "5"="06/01 - 06/02")

int_melt <- mote_int %>% 
  mutate(actual_hours = format(actual_time, "%H:%M:%S"),
         week = get_week(epoch)) %>%
  select(humid_temp, humid_adj, hamatop, hamabot, Height, actual_hours, week) %>%
  melt(id = c("Height", "week", "actual_hours"))
# temperature
int_melt_mean_temp <- dcast(int_melt %>% filter(variable == "humid_temp"), 
                            Height + week + actual_hours ~ variable,
                            fun.aggregate = mean)
int_melt_mean_temp %>% 
  ggplot() + 
  geom_point(mapping = aes(strptime(actual_hours, format = "%H:%M:%S", tz = "PST"),
                          y=humid_temp, color = Height), size = 0.1) +
  scale_color_gradient(low = "coral4", high = "lightskyblue") +
  scale_x_datetime(breaks=date_breaks('2 hour'),
                   labels=date_format('%H:%M', tz = 'PST')) +
  labs(x = "Time", y = "Temperature", color = "Height") +
  theme(axis.text.x = element_text(angle = 90, size = 10, hjust =1, vjust = 0.5),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        legend.title = element_text(size =14),
        legend.key.size = unit(0.6, "cm"),
        legend.margin = unit(0, "cm"),
        strip.text.x = element_text(size = 12)) +
  facet_wrap(~week, ncol = 3, labeller = as_labeller(week_labeller))
# 
#   geom_tile(mapping = aes(strptime(actual_hours, format = "%H:%M:%S", tz = "PST"),
#                           y=as.factor(Height), fill = humid_temp)) +
#   scale_fill_gradient(low = "gold1", high = "red3") +
#   scale_x_datetime(breaks=date_breaks('2 hour'),
#                    labels=date_format('%H:%M', tz = 'PST')) +
#   labs(x = "Time", y = "Temperature", fill = "Temperature") +
#   theme(axis.text.x = element_text(angle = 90, size = 10, hjust =1, vjust = 0.5),
#         axis.text.y = element_text(size = 10),
#         axis.title.x = element_text(size = 16),
#         axis.title.y = element_text(size = 16),
#         legend.title = element_text(size =14),
#         legend.key.size = unit(0.6, "cm"),
#         legend.margin = unit(0, "cm"),
#         strip.text.x = element_text(size = 12)) +
#   facet_wrap(~week, ncol = 3, labeller = as_labeller(week_labeller))

# humidity
int_melt_mean_humid <- dcast(int_melt %>% filter(variable == "humid_adj"), 
                            Height + week + actual_hours ~ variable,
                            fun.aggregate = mean)
int_melt_mean_humid %>% 
  ggplot() + 
  geom_point(mapping = aes(strptime(actual_hours, format = "%H:%M:%S", tz = "PST"),
                           y=humid_adj, color = Height), size = 0.1) +
  scale_color_gradient(low = "coral4", high = "lightskyblue") +
  scale_x_datetime(breaks=date_breaks('2 hour'),
                   labels=date_format('%H:%M', tz = 'PST')) +
  labs(x = "Time", y = "Adjusted humidity", color = "Height") +
  theme(axis.text.x = element_text(angle = 90, size = 10, hjust =1, vjust = 0.5),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        legend.title = element_text(size =14),
        legend.key.size = unit(0.6, "cm"),
        legend.margin = unit(0, "cm"),
        strip.text.x = element_text(size = 12)) +
  facet_wrap(~week, ncol = 3, labeller = as_labeller(week_labeller))
# int_melt_mean_humid %>% 
#   ggplot() + 
#   geom_tile(mapping = aes(strptime(actual_hours, format = "%H:%M:%S", tz = "PST"),
#                           y=as.factor(Height), fill = humid_adj)) +
#   scale_fill_gradient(low = "lightblue1", high = "dodgerblue4") +
#   scale_x_datetime(breaks=date_breaks('2 hour'),
#                    labels=date_format('%H:%M', tz = 'PST')) +
#   labs(x = "Time", y = "Height", fill = "Humidity") +
#   theme(axis.text.x = element_text(angle = 90, size = 10, hjust =1, vjust = 0.5),
#         axis.text.y = element_text(size = 10),
#         axis.title.x = element_text(size = 16),
#         axis.title.y = element_text(size = 16),
#         legend.title = element_text(size =14),
#         legend.key.size = unit(0.6, "cm"),
#         legend.margin = unit(0, "cm"),
#         strip.text.x = element_text(size = 12)) +
#   facet_wrap(~week, ncol = 3, labeller = as_labeller(week_labeller))

# hamatop
int_melt_mean_hamatop <- dcast(int_melt %>% filter(variable == "hamatop"), 
                             Height + week + actual_hours ~ variable,
                             fun.aggregate = mean)
int_melt_mean_hamatop %>% 
  ggplot() + 
  geom_point(mapping = aes(strptime(actual_hours, format = "%H:%M:%S", tz = "PST"),
                           y=hamatop, color = Height), size = 0.1) +
  scale_color_gradient(low = "coral4", high = "lightskyblue") +
  scale_x_datetime(breaks=date_breaks('2 hour'),
                   labels=date_format('%H:%M', tz = 'PST')) +
  labs(x = "Time", y = "Incident PAR", color = "Height") +
  theme(axis.text.x = element_text(angle = 90, size = 10, hjust =1, vjust = 0.5),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        legend.title = element_text(size =14),
        legend.key.size = unit(0.6, "cm"),
        legend.margin = unit(0, "cm"),
        strip.text.x = element_text(size = 12)) +
  facet_wrap(~week, ncol = 3, labeller = as_labeller(week_labeller)) +
  coord_trans(y = "sqrt")
# int_melt_mean_hamatop %>% 
#   ggplot() + 
#   geom_tile(mapping = aes(strptime(actual_hours, format = "%H:%M:%S", tz = "PST"),
#                           y=as.factor(Height), fill = hamatop)) +
#   scale_fill_gradient(low = "midnightblue", high = "gold1") +
#   scale_x_datetime(breaks=date_breaks('2 hour'),
#                    labels=date_format('%H:%M', tz = 'PST')) +
#   labs(x = "Time", y = "Height", fill = "Incident PAR") +
#   theme(axis.text.x = element_text(angle = 90, size = 10, hjust =1, vjust = 0.5),
#         axis.text.y = element_text(size = 10),
#         axis.title.x = element_text(size = 16),
#         axis.title.y = element_text(size = 16),
#         legend.title = element_text(size =14),
#         legend.key.size = unit(0.6, "cm"),
#         legend.margin = unit(0, "cm"),
#         strip.text.x = element_text(size = 12)) +
#   facet_wrap(~week, ncol = 3, labeller = as_labeller(week_labeller))

# hamabot
int_melt_mean_hamabot <- dcast(int_melt %>% filter(variable == "hamabot"), 
                               Height + week + actual_hours ~ variable,
                               fun.aggregate = mean)
int_melt_mean_hamabot %>% 
  ggplot() + 
  geom_point(mapping = aes(strptime(actual_hours, format = "%H:%M:%S", tz = "PST"),
                           y=hamabot, color = Height), size = 0.1) +
  scale_color_gradient(low = "coral4", high = "lightskyblue") +
  scale_x_datetime(breaks=date_breaks('2 hour'),
                   labels=date_format('%H:%M', tz = 'PST')) +
  labs(x = "Time", y = "Reflected PAR", color = "Height") +
  theme(axis.text.x = element_text(angle = 90, size = 10, hjust =1, vjust = 0.5),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        legend.title = element_text(size =14),
        legend.key.size = unit(0.6, "cm"),
        legend.margin = unit(0, "cm"),
        strip.text.x = element_text(size = 12)) +
  facet_wrap(~week, ncol = 3, labeller = as_labeller(week_labeller)) +
  coord_trans(y = "sqrt")
# int_melt_mean_hamabot %>% 
#   ggplot() + 
#   geom_point(mapping = aes(strptime(actual_hours, format = "%H:%M:%S", tz = "PST"),
#                           y=hamabot, color = Height), size = 0.1) +
#   scale_color_gradient(low = "midnightblue", high = "gold1") +
#   scale_x_datetime(breaks=date_breaks('2 hour'),
#                    labels=date_format('%H:%M', tz = 'PST')) +
#   labs(x = "Time", y = "Height", color = "Reflected PAR") +
#   theme(axis.text.x = element_text(angle = 90, size = 10, hjust =1, vjust = 0.5),
#         axis.text.y = element_text(size = 10),
#         axis.title.x = element_text(size = 16),
#         axis.title.y = element_text(size = 16),
#         legend.title = element_text(size =14),
#         legend.key.size = unit(0.6, "cm"),
#         legend.margin = unit(0, "cm"),
#         strip.text.x = element_text(size = 12)) +
#   facet_wrap(~week, ncol = 3, labeller = as_labeller(week_labeller))

## Relationships
# sampling function
get_samples <- function(mote_data, size = 10000) {
  mote_ids <- unique(mote_data$nodeid)
  n <- nrow(mote_data)
  result <- mote_data[1:size,]
  i <- 1
  for (mote_id in mote_ids) {
    if (mote_id != mote_ids[length(mote_ids)]) {
      mote_size <- sum(mote_data$nodeid == mote_id)
      sample_mote_size <- ceiling(mote_size / n * size)
    } else {
      sample_mote_size <- size - i + 1
    }
    result[i:(i+sample_mote_size-1),] <- mote_data %>% filter(nodeid == mote_id) %>%
      sample_n(size=sample_mote_size)
    i <- i + sample_mote_size
  }
  return(result)
}
# undebug(get_samples)
# table(get_samples(mote_int)$nodeid)

# get day boolean
get_day <- function(mote_data, sunrise = "06:00:00", sunset = "20:00:00") {
  time <- format(mote_data$actual_time, "%H:%M:%S")
  return(time >= sunrise & time <= sunset)
}

# get the bin of height
get_height_bin <- function(height) {
  if (height < 45) {return("1")}
  else {
    if (height < 60) {return("2")}
  }
  return("3")
}
# get_height_bin(20)

# sampling
mote_int_sample <- get_samples(mote_int, size = 10000)
# get data during the day time
mote_int_sample_day <- mote_int_sample %>% 
  mutate(day = get_day(mote_int_sample)) %>%
  filter(day)
table(format(mote_int_sample_day$actual_time, "%H:%M:%S"))

# correlations among all the data
all_cor_mat <- mote_int %>% 
  select(humid_temp, humid_adj, hamatop, hamabot, Height) %>% 
  mutate(hamatop = sqrt(hamatop), hamabot = sqrt(hamabot)) %>% cor()
all_cor_mat[lower.tri(all_cor_mat)] <- NA
all_cor_mat_melt <- melt(all_cor_mat) %>% filter(!is.na(value))
levels(all_cor_mat_melt$Var1) <- list("Temperature" = "humid_temp", 
                                      "Adjusted humidity" = "humid_adj",
                                      "Incident PAR" = "hamatop", 
                                      "Relfected PAR" = "hamabot", 
                                      "Height" = "Height")
levels(all_cor_mat_melt$Var2) <- rev(list("Temperature" = "humid_temp", 
                                          "Adjusted humidity" = "humid_adj",
                                          "Incident PAR" = "hamatop", 
                                          "Relfected PAR" = "hamabot", 
                                          "Height" = "Height"))
all_cor_mat_melt %>%
  ggplot(mapping = aes(x = Var1, y = Var2, fill = value, label = round(value,2))) +
  geom_tile(color = "white") +
  geom_text() +
  scale_fill_gradient2(low = "steelblue", high = "darkred",
                       midpoint = 0, lim = c(-1,1)) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  labs(x = "", y = "", fill = "Correlation") + coord_fixed(ratio=1) +
  theme(axis.text.x = element_text(size = 12, angle = 30, vjust = 1, hjust = 1), 
        axis.text.y = element_text(size = 12),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        legend.position = c(.75, .8),
        legend.direction = "horizontal",
        plot.margin = unit(c(0.1, 0, 0, 0), "cm"))

mote_int %>% mutate(day = get_day(mote_int)) %>%
  filter(day) %>%
  select(humid_temp, humid_adj, hamatop, hamabot, Height) %>% 
  mutate(hamatop = sqrt(hamatop), hamabot = sqrt(hamabot)) %>% cor()
mote_int %>% mutate(day = get_day(mote_int)) %>%
  filter(!day) %>%
  select(humid_temp, humid_adj, hamatop, hamabot) %>% 
  mutate(hamatop = sqrt(hamatop), hamabot = sqrt(hamabot)) %>% cor()
mote_int %>% mutate(hamatop_bin = hamatop <= mean(hamatop)) %>%
  filter(hamatop_bin) %>%
  select(humid_temp, humid_adj, hamatop, hamabot) %>% 
  mutate(hamatop = sqrt(hamatop), hamabot = sqrt(hamabot)) %>% cor()
mote_int %>% mutate(hamatop_bin = hamatop <= mean(hamatop)) %>%
  filter(!hamatop_bin) %>%
  select(humid_temp, humid_adj, hamatop, hamabot) %>% 
  mutate(hamatop = sqrt(hamatop), hamabot = sqrt(hamabot)) %>% cor()
mote_int %>% mutate(day = get_day(mote_int)) %>%
  filter(day) %>%
  select(humid_temp, humid_adj) %>%
  ggplot(mapping = aes(x = humid_adj, y = humid_temp)) +
  stat_bin2d(bins = 100)
# correlations among the sample
sample_cor_mat <- mote_int_sample %>% 
  select(humid_temp, humid_adj, hamatop, hamabot, Height) %>% 
  mutate(hamatop = sqrt(hamatop), hamabot = sqrt(hamabot)) %>% cor()
sample_cor_mat[lower.tri(sample_cor_mat)] <- NA
sample_cor_mat_melt <- melt(sample_cor_mat) %>% filter(!is.na(value))
levels(sample_cor_mat_melt$Var1) <- list("Temperature" = "humid_temp", 
                                      "Adjusted humidity" = "humid_adj",
                                      "Incident PAR" = "hamatop", 
                                      "Relfected PAR" = "hamabot", 
                                      "Height" = "Height")
levels(sample_cor_mat_melt$Var2) <- rev(list("Temperature" = "humid_temp", 
                                          "Adjusted humidity" = "humid_adj",
                                          "Incident PAR" = "hamatop", 
                                          "Relfected PAR" = "hamabot", 
                                          "Height" = "Height"))
sample_cor_mat_melt %>%
  ggplot(mapping = aes(x = Var1, y = Var2, fill = value, label = round(value,2))) +
  geom_tile(color = "white") +
  geom_text() +
  scale_fill_gradient2(low = "steelblue", high = "darkred",
                       midpoint = 0, lim = c(-1,1)) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  labs(x = "", y = "", fill = "Correlation") + coord_fixed(ratio=1) +
  theme(axis.text.x = element_text(size = 12, angle = 30, vjust = 1, hjust = 1), 
        axis.text.y = element_text(size = 12),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        legend.position = c(.75, .8),
        legend.direction = "horizontal",
        plot.margin = unit(c(0.1, 0, 0, 0), "cm"))

mote_int_sample_day %>%
  select(humid_temp, humid_adj, hamatop, hamabot) %>% 
  mutate(hamatop = sqrt(hamatop), hamabot = sqrt(hamabot)) %>% cor()
mote_int_sample %>% mutate(day = get_day(mote_int_sample)) %>%
  filter(!day) %>%
  select(humid_temp, humid_adj, hamatop, hamabot) %>% 
  mutate(hamatop = sqrt(hamatop), hamabot = sqrt(hamabot)) %>% cor()

# all the sample
mote_int_sample %>% 
  ggplot(mapping = aes(x = humid_adj, y = humid_temp, color = hamabot)) +
  geom_point(size = 0.2, alpha = 0.5) +
  scale_color_gradient(low = "coral4", high = "blue")
mote_int_sample %>%
  ggplot(mapping = aes(x = humid_adj, y = humid_temp, color = Direc)) +
  geom_point(size = 0.5) +
  scale_color_manual(values = direc_fill)
# day time data in the sample
mote_int_sample_day %>% 
  ggplot(mapping = aes(x = humid_adj, y = humid_temp, color = hamatop)) +
  geom_point(size = 0.2) +
  geom_smooth(method="lm") +
  scale_color_gradient(low = "coral4", high = "blue")
par_color <- c("TRUE"="midnightblue", "FALSE"="gold1")
par_label <- c("FALSE"=expression(phantom(x) > " average"), 
               "TRUE"=expression(phantom(x) <= " average"))
plot_lm <- mote_int_sample_day %>% 
  mutate(hamatop_bin = (hamatop <= mean(hamatop))) %>%
  ggplot(mapping = aes(x = humid_adj, y = humid_temp, color = hamatop_bin)) +
  geom_point(size = 0.2, alpha = 0.6) + 
  geom_smooth(method = "lm", aes(group = hamatop_bin)) +
  scale_color_manual(values = par_color, labels = par_label) + 
  labs(x = "Adjusted humidity", y = "Temperature", color = "Incident PAR") +
  theme(legend.position=c(1,1),legend.justification=c(1,1),
        legend.background = element_rect(fill = "transparent")) 
#marginal density of x - plot on top
plot_top <- mote_int_sample_day %>% 
  mutate(hamatop_bin = (hamatop <= mean(hamatop))) %>%
  filter(hamatop_bin) %>%
  ggplot(mapping = aes(x = humid_adj, fill=hamatop_bin, color = "transparent")) + 
  geom_density(alpha=.8, color = "transparent") + 
  mote_int_sample_day %>% 
  mutate(hamatop_bin = (hamatop <= mean(hamatop))) %>%
  filter(!hamatop_bin) %>%
  geom_density(mapping = aes(x = humid_adj, fill=hamatop_bin, color = "transparent"), 
               alpha=.8, color = "transparent") +
  scale_fill_manual(values = par_color, labels = par_label) + 
  theme(legend.position = "none",
        axis.title = element_blank()) 

#marginal density of y - plot on the right
plot_right <- mote_int_sample_day %>% 
  mutate(hamatop_bin = (hamatop <= mean(hamatop))) %>%
  filter(hamatop_bin) %>%
  ggplot(mapping = aes(x = humid_temp, fill=hamatop_bin, color = "transparent")) + 
  geom_density(alpha=.8, color = "transparent") + 
  mote_int_sample_day %>% 
  mutate(hamatop_bin = (hamatop <= mean(hamatop))) %>%
  filter(!hamatop_bin) %>%
  geom_density(mapping = aes(x = humid_temp, fill=hamatop_bin, color = "transparent"), 
               alpha=.8, color = "transparent") +
  scale_fill_manual(values = par_color, labels = par_label) + 
  coord_flip() +
  theme(legend.position = "none",
        axis.title = element_blank())

#arrange the plots together, with appropriate height and width for each row and column
# placeholder for plotting nothing at all
empty <- ggplot()+geom_point(aes(1,1), colour="white") +
  theme(                              
    plot.background = element_blank(), 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), 
    panel.border = element_blank(), 
    panel.background = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank()
  )
grid.arrange(plot_top, empty, plot_lm, plot_right, ncol=2, nrow=2, widths=c(4, 1), heights=c(1, 4))


mote_int_sample_day %>%
  ggplot(mapping = aes(x = humid_adj, y = humid_temp, color = Direc)) +
  geom_point(size = 0.5) +
  geom_smooth(method="lm", se = TRUE) +
  scale_color_manual(values = direc_fill)
mote_int_sample_day %>% 
  ggplot(mapping = aes(x = humid_adj, y = humid_temp, color = Height)) +
  geom_point(size = 0.2) +
  geom_smooth(method="lm") +
  scale_color_gradient(low = "coral4", high = "blue")

height_bin_labels <- c("1"=expression(phantom(x) <= "45"),
                      "2"="(45, 60]",
                      "3"=expression(phantom(x) > "60"))
height_bin_colors <- c("1"="coral4",
                       "2"="olivedrab3",
                       "3"="deepskyblue2")
mote_int_sample_day %>% 
  mutate(height_bin = sapply(Height, function(height) get_height_bin(height))) %>% 
  ggplot(mapping = aes(x = humid_adj, y = humid_temp, color = height_bin)) +
  geom_point(size = 0.2) +
  geom_smooth(method="lm", se = TRUE) +
  scale_color_manual(values = height_bin_colors,
                     labels = height_bin_labels) +
  labs(x = "Adjusted humidity", y = "Temperature", color = "Height")
mote_int_sample %>% mutate(day = get_day(mote_int_sample)) %>%
  filter(!day) %>%
  mutate(height_bin = sapply(Height, function(height) get_height_bin(height))) %>% 
  ggplot(mapping = aes(x = humid_adj, y = humid_temp, color = height_bin)) +
  geom_point(size = 0.2) +
  geom_smooth(method="lm", se = TRUE) +
  scale_color_manual(values = height_bin_colors,
                     labels = height_bin_labels)+
  labs(x = "Adjusted humidity", y = "Temperature", color = "Height")
