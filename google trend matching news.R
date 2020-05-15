#######  Google trend graph   BCQ
###########Hand Sanitizer & Disinfectent Wipe
###########################################################
## Data Cleanning
library(lubridate)
library(plotly)
health_gt <- read.csv("health_google_trend.csv")
library(tidyverse)
library(dplyr)
health_gt$Date <- mdy(as.character(health_gt$Date))
health_gt <- health_gt[order(health_gt$Date),]

health_gt_tidy <- pivot_longer(health_gt,
                               cols = 2:6,
                               names_to = "health_type",
                               values_to = "Search_rate")
health_gt_tidy1 <- health_gt_tidy[health_gt_tidy["health_type"]==c("Hand.Sanitizer","Disinfectant.Wipes"),]
new_row0 <- data.frame("Date"=mdy("1/30/2020"),"health_type"="Hand.Sanitizer","Search_rate" = 2)
new_row1 <- data.frame("Date"=mdy("03/13/2020"),"health_type"="Disinfectant.Wipes","Search_rate" = 81)
new_row2 <- data.frame("Date"=mdy("03/21/2020"),"health_type"="Disinfectant.Wipes","Search_rate" = 67.5)
new_row3 <- data.frame("Date"=mdy("04/03/2020"),"health_type"="Hand.Sanitizer","Search_rate" = 34.5)

health_gt_tidy1 <-rbind(health_gt_tidy1,new_row0,new_row1,new_row2,new_row3)
health_gt_tidy1 <- health_gt_tidy1 %>% arrange(Date)
## Annotation 
data <- data.frame("Date"= c(mdy("1/30/2020"),mdy("2/25/2020"),mdy("3/13/2020"),mdy("3/21/2020"),mdy("04/03/2020")),
                   "Word" = c('WHO declares coronavirus outbreak a global health emergency\nUS Confirmed Cases: 5\nWorld Confirmed Cases: 8234',
                              'US Confirmed Cases: 15\nWorld Confirmed Cases: 80,370',
                              'US Confirmed Cases: 2,100\nWorld Confirmed Cases: 145,187',
                              'Day 2 of Stay-at-Home Order of CA, NY, and IL\nUS Confirmed Cases: 25,700\nWorld Confirmed Cases: 304,680',
                              'US Confirmed Cases: 275,600\nWorld Confirmed Cases: 1,0960,000'))
## graph code
fig3 <- plot_ly(health_gt_tidy1, x = ~Date, y = ~Search_rate,color = ~health_type)
fig3 <- fig3 %>% add_lines()
fig3 <- fig3 %>% add_trace(
  type = 'scatter',
  name = 'Disinfectant_Wipes',
  mode = 'lines+markers',
  x = mdy("1/30/2020"),
  y = 4,
  color = 'green',
  text = 'WHO declares coronavirus outbreak a global health emergency\nUS Confirmed Cases: 5\nWorld Confirmed Cases: 8234',
  showledgent = FALSE
)%>% add_trace(
  type = 'scatter',
  mode = 'lines+markers',
  name = 'Hand_Sanitizer',
  x = mdy("1/30/2020"),
  y = 2,
  color = 'orange',
  text = 'WHO declares coronavirus outbreak a global health emergency\nUS Confirmed Cases: 5\nWorld Confirmed Cases: 8234',
  showledgent = FALSE
)%>% add_trace(
  type = 'scatter',
  name = 'Disinfectant_Wipes',
  mode = 'lines+markers',
  color = 'green',
  x = mdy("2/25/2020"),
  y = 2,
  text = 'US Confirmed Cases: 15\nWorld Confirmed Cases: 80,370',
  showlengent = FALSE
)%>% add_trace(
  type = 'scatter',
  name = 'Hand_Sanitizer',
  mode = 'lines+markers',
  color = 'orange',
  x = mdy("2/25/2020"),
  y = 1,
  text = 'US Confirmed Cases: 15\nWorld Confirmed Cases: 80,370',
  showledgent = FALSE
)%>% add_trace(
  type = 'scatter',
  name = 'Disinfectant_Wipes',
  mode = 'lines+markers',
  color = 'green',
  x = mdy("3/12/2020"),
  y = 84.0,
  text = 'US Confirmed Cases: 1,600\nWorld Confirmed Cases: 128,246',
  showlegent = FALSE
)%>% add_trace(
  type = 'scatter',
  name = 'Hand_Sanitizer',
  mode = 'lines+markers',
  color = 'orange',
  x = mdy("3/13/2020"),
  y = 93.0,
  text = 'US Confirmed Cases: 2,100\nWorld Confirmed Cases: 145,187',
  showlegent = FALSE
)%>% add_trace(
  type = 'scatter',
  name = 'Disinfectant_Wipes',
  mode = 'lines+markers',
  color = 'green',
  x = mdy("3/22/2020"),
  y = 75.0,
  text = 'Day 3 of Stay-at-Home Order of CA, NY, and IL\nUS Confirmed Cases: 33,600\nWorld Confirmed Cases: 337,376',
  showlegent = FALSE
)%>% add_trace(
  type = 'scatter',
  name = 'Hand_Sanitizer',
  mode = 'lines+markers',
  color = 'orange',
  x = mdy("3/21/2020"),
  y = 45.0,
  text = 'Day 2 of Stay-at-Home Order of CA, NY, and IL\nUS Confirmed Cases: 25,700\nWorld Confirmed Cases: 304,680',
  showlegent = FALSE
)%>% add_trace(
  type = 'scatter',
  name = 'Disinfectant_Wipes',
  mode = 'lines+markers',
  color = 'green',
  x = mdy("4/3/2020"),
  y = 88.0,
  text = 'US Confirmed Cases: 275,600\nWorld Confirmed Cases: 1,0960,000',
  showlegent = FALSE
)%>% add_trace(
  type = 'scatter',
  name = 'Hand_Sanitizer',
  mode = 'lines+markers',
  color = 'orange',
  x = mdy("4/4/2020"),
  y = 36.0,
  text = 'US Confirmed Cases: 308,800\nWorld Confirmed Cases: 1,1760,000',
  showlegent = FALSE
)
fig3

##### N95 Mask and Disinfectant Spray
health_gt_tidy2 <- health_gt_tidy[health_gt_tidy["health_type"]==c("N95.Mask","Disinfectant.Spray"),]
row1 <- data.frame("Date"=mdy("03/13/2020"),"health_type"="Disinfectant.Wipes","Search_rate" = 82)
fig4 <- plot_ly(health_gt_tidy2, x = ~Date, y = ~Search_rate,color = ~health_type)
fig4 <- fig4 %>% add_lines()
fig4 <- fig4 %>% add_trace(
  type = 'scatter',
  mode = 'lines+markers',
  name = 'N95 Mask',
  color = '6666FF',
  x = mdy("1/30/2020"),
  y = 13.5,
  text = 'WHO declares coronavirus outbreak a global health emergency\nUS Confirmed Cases: 5\nWorld Confirmed Cases: 8234',
  showledgent = FALSE
) %>% add_trace(
  type = 'scatter',
  mode = 'lines+markers',
  name = 'Disinfectant Spray',
  color = '66FFB2',
  x = mdy("1/31/2020"),
  y = 3,
  text = 'Day 2 of WHO declares coronavirus outbreak a global health emergency\nUS Confirmed Cases: 5\nWorld Confirmed Cases: 8234',
  showledgent = FALSE
)%>% add_trace(
  type = 'scatter',
  mode = 'lines+markers',
  name = 'N95 Mask',
  color = '6666FF',
  x = mdy("2/26/2020"),
  y = 72,
  text = 'US Confirmed Cases: 15\nWorld Confirmed Cases: 81,346',
  showledgent = FALSE
) %>% add_trace(
  type = 'scatter',
  mode = 'lines+markers',
  name = 'Disinfectant Spray',
  color = '66FFB2',
  x = mdy("3/7/2020"),
  y = 13,
  text = 'US Confirmed Cases: 336\nWorld Confirmed Cases: 105,781',
  showledgent = FALSE
)%>% add_trace(
  type = 'scatter',
  mode = 'lines+markers',
  name = 'N95 Mask',
  color = '6666FF',
  x = mdy("3/13/2020"),
  y = 25,
  text = 'US Confirmed Cases: 2,100\nWorld Confirmed Cases: 145,187',
  showledgent = FALSE
) %>% add_trace(
  type = 'scatter',
  mode = 'lines+markers',
  name = 'Disinfectant Spray',
  color = '66FFB2',
  x = mdy("3/13/2020"),
  y = 44,
  text = 'US Confirmed Cases: 2,100\nWorld Confirmed Cases: 145,187',
  showledgent = FALSE
)%>% add_trace(
  type = 'scatter',
  mode = 'lines+markers',
  name = 'N95 Mask',
  color = '6666FF',
  x = mdy("3/21/2020"),
  y = 63,
  text = 'Day 2 of Stay-at-Home Order of CA, NY, and IL\nUS Confirmed Cases: 25,700\nWorld Confirmed Cases: 304,680',
  showledgent = FALSE
) %>% add_trace(
  type = 'scatter',
  mode = 'lines+markers',
  name = 'Disinfectant Spray',
  color = '66FFB2',
  x = mdy("3/21/2020"),
  y = 46,
  text = 'Day 2 of Stay-at-Home Order of CA, NY, and IL\nUS Confirmed Cases: 25,700\nWorld Confirmed Cases: 304,680',
  showledgent = FALSE
)%>% add_trace(
  type = 'scatter',
  mode = 'lines+markers',
  name = 'N95 Mask',
  color = '6666FF',
  x = mdy("4/4/2020"),
  y = 100,
  text = 'US Confirmed Cases: 308,800\nWorld Confirmed Cases: 1,1760,000',
  showledgent = FALSE
) %>% add_trace(
  type = 'scatter',
  mode = 'lines+markers',
  name = 'Disinfectant Spray',
  color = '66FFB2',
  x = mdy("4/4/2020"),
  y = 100,
  text = 'US Confirmed Cases: 308,800\nWorld Confirmed Cases: 1,1760,000',
  showledgent = FALSE
)
fig4
