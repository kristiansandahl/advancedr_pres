
# Required packages ----
library(tidyverse)


summary(tornados)

ggplot(data = tornados, aes(x = inj))+
    geom_boxplot()
# How many tornados per year? ----
tornados %>%
    group_by(yr) %>%
    summarise(
        number.tornados = n_distinct(om)) %>%
    ggplot(., aes(x = yr, y = number.tornados))+
    geom_point()+
    theme_bw()

# How many tornados per year? ----
tornados %>%
    group_by(yr) %>%
    summarise(
        mean.mag = mean(mag, na.rm = T),
        number.tornados = n_distinct(om)) %>%
    ggplot(., aes(x = yr, y = number.tornados, color = mean.mag, size = mean.mag))+
    scale_color_viridis_c(direction = -1)+
    geom_point()+
    theme_bw()

# How many tornados per year? ----
tornados %>%
    group_by(yr, mag) %>%
    mutate(
        number.tornados = n_distinct(om)) %>%
    ggplot(., aes(x = yr, y = number.tornados, color = as.factor(mag)))+
    geom_point()+
    geom_smooth(method = "lm", se = T)+
    theme_bw()

# How Many injuries per year? ----

tornados %>%
    group_by(yr) %>%
    summarise(
        number.tornados = n_distinct(om),
        count.injuries = sum(inj),
        median.injuries = median(inj, na.rm = T),
        mean.injuries = mean(inj, na.rm = T)
    ) %>%
    ggplot(., aes(x = yr, y = count.injuries))+
    geom_point()+
    geom_point(aes(y = median.injuries, color = "red"))+
    geom_smooth()+
    theme_bw()

# Injuries per year my Magnitude
tornados %>%
    group_by(yr) %>%
    summarise(
        number.tornados = n_distinct(om),
        count.injuries = sum(inj),
        median.injuries = median(inj, na.rm = T),
        mean.injuries = mean(inj, na.rm = T)
    ) %>%
    ggplot(., aes(x = yr, y = count.injuries))+
    geom_point()+
    geom_point(aes(y = median.injuries, color = "red"))+
    geom_smooth()+
    theme_bw()



# DataExplorer ----
library(DataExplorer)

create_report(tornados)






