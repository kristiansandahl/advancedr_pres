# load libraries ----
library(tidyverse)
library(ggplot2)
library(DataExplorer)
library(ggridges)
library(hrbrthemes)
library(ggstatsplot)
library(palmerpenguins)
library(viridis)


# load data ----
    # https://github.com/rfordatascience/tidytuesday/blob/master/data/2023/2023-05-16/readme.md

tornados <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-05-16/tornados.csv')

str(tornados)

# data explorer ----
tornados %>% 
    create_report()

# data wrangling ----
tornados <- tornados %>% 
    group_by(yr) %>% 
    mutate(
        no_tornados = n_distinct(om),
        count_injuries = sum(inj),
        median_injuries = median(inj),
        mean_injuries = mean(inj),
        stf = as.factor(stf),
        mean_mag = mean(mag),
        
        count_fat = sum(fat)
        
           ) %>% 
    mutate(
        mag = as.factor(mag)
        )

# descriptive statistics ----

## histogram ----
tornados %>% 
    ggplot(aes(x=yr, fill = mag)) +
    geom_histogram() +
    facet_wrap(~mag) +
    theme_bw()


## barplot ----
tornados %>%
    mutate(decade = as.integer((yr - 1950) / 10) * 10 + 1950) %>% 
    mutate(decade = as.factor(decade)
    ) %>% 
    group_by(decade, mag) %>% 
    ggplot(aes(x = decade, y = no_tornados, group = mag, fill = mag)) +
    geom_bar(stat = "identity", width=0.4 ) +
    theme_bw() +
    labs(
        title = "No of Tornados pr decade",
        x = "Decade",
        y = "No of tornados",
        fill = "Magnitude"
    )




## all tornados x = year, y = length ----
tornados %>% 
    ggplot(aes(x = yr, y = len, group = mag, color = mag)) +
    geom_point(alpha = 0.3) + 
    labs(title = "All Tornadoes Over Time", x = "Year", y = "Tornado Path Length (miles)") +
    theme_bw()


## tornados pr year ----
tornados %>%
    group_by(yr, mag) %>%
    summarize(TornadoCount = n(), .groups = "keep") %>% 
    ggplot(aes(x = yr, y = TornadoCount, group = mag, color = mag)) +
    geom_point() + 
    geom_smooth(method = "lm") + 
    labs(title = "Tornado Development Over Time",
         x = "Year",
         y = "Number of Tornadoes/year") +
    theme_bw()

## injuries by year ----
tornados %>%
    group_by(yr) %>%
    ggplot(aes(x = yr, y = count_injuries, group = mag, color = mag)) +
    geom_point() + 
    geom_smooth(method = "lm") + 
    labs(
        title = "Tornado Associated Injuries Over Time",
        x = "Year",
        y = "Number of Injuries") +
    theme_bw()


## fatalities by year ----
tornados %>%
    group_by(yr) %>%
    ggplot(aes(x = yr, y = count_fat, group = mag, color = mag)) +
    geom_point() + 
    geom_smooth(method = "lm") + 
    labs(
        title = "Tornado Associated Fatalities Over Time",
        x = "Year",
        y = "Number of fatalities") +
    theme_bw()



## violoin plot ----

plt <- ggbetweenstats(
    data = tornados,
    x = mag,
    y = inj
)

print(plt)


tornados %>% 
    ggplot(aes(x=mag, y=inj, fill = mag)) +
    geom_violin(width=1.4) +
    geom_boxplot(width=0.1, color="grey", alpha=0.2) +
    scale_fill_viridis(discrete = TRUE) +
    theme_ipsum() +
    theme(
        legend.position="none",
        plot.title = element_text(size=11)
    ) +
    ggtitle("A") +
    xlab("")










tornados %>% 
    group_by(yr) %>% 
    summarise(
        no_tornados = n_distinct(om),
        count_injuries = sum(inj),
        median_injuries = median(inj),
        mean_injuries = mean(inj),
        stf = as.factor(stf),
        mean_mag = mean(mag)
    ) %>% 
    ggplot(aes(
        x = yr, y = no_tornados)) + 
    geom_point() +
    geom_smooth(method = "lm") +
    theme_bw()



tornados %>% 
    ggplot(aes(x = yr, y = no_tornados)) + 
    geom_point() +
    geom_smooth(method = "lm") + 
    theme_bw()




# PCA ----
library(FactoMineR)
library(factoextra)
library(missMDA)

df_pca <- tornados %>% 
    ungroup() %>% 
    select(mag, inj, fat, len, wid, ns) %>% # loss (many missing)
    filter(!is.na(mag))

ncp_dim <- estim_ncpFAMD(df_pca)

df_pca <- imputeFAMD(df_pca, 
                     ncp = ncp_dim,
                     method = c("Regularized"))


res.pca <- PCA (df_pca,
                #quanti.sup = c(),
                quali.sup = c(1),
                ncp = 10,
                graph = F)

dimdesc(res.pca)


fviz_pca_biplot(res.pca,
                axes = c(1,2),
                geom.var = c("arrow", "text"),
                habillage = "mag",
                repel = F,
                legend = "bottom",
                addEllipses = T,
                ellipse.level = 0.5,
                title = "PCA Biplot: Magnitude - Dim 1-2")


df_pca_redu <- df_pca %>% 
    filter(mag %in% c(3,4,5))

res.pca <- PCA (df_pca_redu,
                #quanti.sup = c(),
                quali.sup = c(1),
                ncp = 10,
                graph = F)

fviz_pca_biplot(res.pca,
                axes = c(1,2),
                geom.var = c("arrow", "text"),
                habillage = "mag",
                repel = F,
                legend = "bottom",
                addEllipses = T,
                ellipse.level = 0.5,
                title = "PCA Biplot: Magnitude - Dim 1-2")



















