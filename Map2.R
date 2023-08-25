library(tidyverse)
library(mapdata)
library(ggplot2)
#create the unique ID for each tornado

data <- tornados %>% 
        select(om,yr,date,st,slat,slon,mag) %>% 
        mutate(id = paste(date,om,sep = "-")) %>% 
        select(id,yr,st,slat,slon,mag) 
# count the total number of tornados for each states since 1950

freq_st <- data %>% 
    group_by(st, mag = as.factor(mag), .drop = F) %>% 
    summarise(total_tornados = n(), .groups = "drop") %>% 
    arrange(desc(total_tornados))


### Map

# Load the usastates data
usastates <- map_data("state")


# Starting location of the tornados 
tornados_points <- data.frame(
    lon = data$slon,
    lat = data$slat,
    Magnitude = data$mag
)
# Map
ggplot() + 
    geom_polygon(data = usastates, aes(x = long, y = lat, group = group), fill = "white", color = "black") +
    geom_point(data = tornados_points, aes(x = lon, y = lat, colour = Magnitude),  size = 0.05) +
    scale_colour_gradient(low = "lightblue", high = "darkblue") +
    labs(title = "US Map with Tornados occurence", subtitle = "Tornados' starting location ") +
    theme_minimal()



# Convert the state name
usastates <- usastates %>% 
    mutate(state = state.abb[match(str_to_title((usastates$region)),state.name,)])
# Merge the data
# merged_data <- merge(usastates, freq_st, by.x = "state", by.y = "st",all.x = TRUE)
# sometimes it will change the order of rows
# left join
merged_data2 <-left_join(usastates, freq_st, by =c("state"= "st"))
usastates <- usastates %>% 
    mutate(id=row_number())


# Calculate state centers loctions for text display
state_centers <- merged_data2 %>%
    filter(!is.na(mag)) %>%
    select(state,long,lat,total_tornados, mag)%>%
    group_by(state, mag, total_tornados) %>%
    mutate(center_long = mean(long), center_lat = mean(lat))

# Map ----
merged_data2 %>% 
    filter(!is.na(mag)) %>%
    ggplot() +
    geom_polygon(aes(x = long, y = lat, group = group), fill = "white", color = "black") +
    geom_polygon(aes(x = long, y = lat, group = group, fill = total_tornados), color = "black") +
    scale_fill_gradient(low = "lightblue", high = "darkblue", na.value = "#D2D7D9", trans = "log") +
    labs(title = "US Map", subtitle = "Color shades represent the frequency of tornados") +
    theme_minimal() +
    theme(legend.position = "none")+
    facet_wrap(~as.factor(mag))+
    geom_text(data = state_centers, aes(x = center_long, y = center_lat - 0.8, label = total_tornados), hjust = 0.7, color = "yellow")
    
# Reference Map ----
url <- "https://scied.ucar.edu/sites/default/files/interactives/where-tornadoes/tornadoMap.png"
image_scale(image_read(url), "700")
    

