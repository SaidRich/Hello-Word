
library(dplyr)
library(data.table)
library(plotly)

# Importing data 
#link <- "https://assets.datacamp.com/production/repositories/2166/datasets/1367560ab66f0b7006da2075a5a97a99b5184bf7/state_economic_data.csv"
 Datei <- "C:/Users/saidr/Desktop/R/DataCamp/Plotly/1_Data/state_economic_data.csv"
 dt_link <- read.csv2(Datei, header = T, sep = ',')

options('stringsAsFactors'=FALSE)
#dt_link <- read.csv2(link, header = T, sep = ',')

colClasse <- c("year" = "numeric", "gdp" = "numeric", 'employement'= "numeric",
               'home_owners'= "numeric",'house_price'= "numeric",'population' = "numeric")
dt_link <- as.data.table(dt_link)
dt_link$year <- as.integer(dt_link$year)
dt_link$gdp <- as.numeric(dt_link$gdp)
dt_link$employment <- as.numeric(dt_link$employment)
dt_link$home_owners <- as.numeric(dt_link$home_owners)
dt_link$house_price <- as.numeric(dt_link$house_price)
dt_link$population <- as.numeric(dt_link$population)


# Create an animated bubble chart of house_price against gdp
dt_link %>%
  plot_ly(x = ~ gdp, y = ~ house_price) %>%
  add_markers(size = ~population, color = ~ region, 
              frame = ~ year, #ids = ~ state,
              marker = list(sizemode = 'diameter'))

# Nice! Bubble charts are often used to display either three or four variables on a single plot. 
#Here, we display five variables through animation: gdp, house_price, population, region, and year. 
#That said, more isn't always better, 
#when designing a multivariate graphic be sure that the observer can perceive all of the variables! 


# Animate a bubble chart of house_price against gdp over region
ani <- dt_link %>%
            filter(year == 2017) %>%
            plot_ly(x = ~gdp, y = ~house_price) %>%
            add_markers(size = ~population, color = ~region, 
                        frame = ~ region, ids = ~state,
                        marker = list(sizemode = "diameter")) 

    # You can use any variable in your data set to define the frame aesthetic. 
    # Typically, time variable and groupping factors make the most sense. 

# A full list of the easing options can be found in here. 
#Notice that you can append -in, -out, and -in-out to the easing options 
# outlined in the lesson to further customize the transitions between frames.
ani %>% 
    animation_opts(
      frame = 2000, 
      transition = 300, 
      easing = 'elastic'
    )
# Remove the prefix from the slider and change the font color to "red"
ani %>% 
  animation_opts(
    frame = 2000, 
    transition = 300, 
    easing = "elastic"
  ) %>%
  animation_slider(
    currentvalue = list(
      prefix = NULL, 
      font = list(color = 'red')
    )
  ) %>%
  layout(
    xaxis = list(title = ' Real GDP (Mio.$) ', type = 'log'),
    yaxis = list(title = ' House price index')
  )

# Add the year as background text and remove the slider
dt_link %>%
  plot_ly(x = ~gdp, y = ~house_price, hoverinfo = "text", text = ~state) %>%
  add_text(x = 1, y = 400, text = ~ year, frame = ~ year,
           textfont = list(color = toRGB('green'), size = 150)) %>%
  add_markers(size = ~population, color = ~region, 
              frame = ~year, ids = ~state,
              marker = list(sizemode = "diameter", sizeref = 3)) %>%
  layout(xaxis = list(title = "Real GDP (millions USD)", type = "log10"),
         yaxis = list(title = "Housing price index")) %>%
  animation_slider(hide = F)#, #font = list(color = 'yellow'),currentvalue = list(
                              #prefix = NULL, font = list(color = 'red')))

library(crosstalk)
# Create a shared data object keyed by region
shared_region <- SharedData$new(dt_link, key = ~region)

# Create a dotplot of avg house_price by region in 2017
dp_chart <- shared_region %>%
  plot_ly() %>%
  filter(year == 2017) %>%
  group_by(region) %>%
  summarize(avg.hpi = mean(house_price, na.rm = TRUE)) %>%
  add_markers(x = ~avg.hpi, y = ~region)

# Code for time series plot
ts_chart <- shared_region %>%
  plot_ly(x = ~year, y = ~house_price) %>%
  group_by(state) %>%
  add_lines()
