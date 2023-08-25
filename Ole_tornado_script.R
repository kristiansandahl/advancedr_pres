
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
        number.tornados = n_distinct(om)) %>%
    ggplot(., aes(x = yr, y = number.tornados))+
    geom_point()+
    geom_smooth(mehthod = "lm")+
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

#Seasonality ---- Months overall
tornados %>%
    filter(!is.na(mag))%>%
    group_by(mo) %>%
    summarise(
        number.tornados = n_distinct(om), .groups = "keep") %>%
    ggplot(., aes(x = mo, y = number.tornados))+
    geom_point()+
    scale_x_continuous(breaks = seq(1,12,1))+
    geom_smooth()+
    theme_bw()

#Seasonality ---- Months by decade
tornados %>%
    filter(!is.na(mag))%>%
    mutate(decade = as.integer((yr - 1950)/10) * 10 + 1950,
           decade = as.factor(decade)) %>%
    group_by(mo, decade) %>%
    summarise(
        number.tornados = n_distinct(om), .groups = "keep") %>%
    ggplot(., aes(x = mo, y = number.tornados))+
    geom_point()+
    scale_x_continuous(breaks = seq(1,12,1))+
    geom_smooth()+
    facet_wrap(~decade)
    theme_bw()


# Seasonality faceted by magnitude ----

tornados %>%
    filter(!is.na(mag))%>%
    mutate(decade = as.integer((yr - 1950)/10) * 10 + 1950,
           decade = as.factor(decade)) %>%
    group_by(mo, decade, mag) %>%
    summarise(
        number.tornados = n_distinct(om), .groups = "keep") %>%
    ggplot(., aes(x = mo, y = number.tornados, color = as.factor(mag)))+
    geom_point()+
    scale_x_continuous(breaks = seq(1,12,1))+
    scale_y_continuous(breaks = seq(0, 1500, 500))+
    geom_smooth(span = 1, se = T)+
    facet_wrap(~decade)+
    labs(title = "Number and Magnitude of Tornados per Decade", 
         x = "Month",
         y = "Number of Tornados",
         color = "Magnitude")+
    theme_bw()

# DataExplorer ----
library(DataExplorer)

create_report(tornados)





