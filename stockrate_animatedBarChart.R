############# Out of Stock Rate animated Bar chart    Bcq
## Data Clean
outofstock <- read.csv("out_of_stock_pct.csv")
outofstock_tidy <- pivot_longer(outofstock,
                                cols = 2:20,
                                names_to = "category",
                                values_to = "stockrate")
ncol(outofstock)
view(outofstock_tidy)
view(stock_formatted)

stock_formatted <- outofstock_tidy %>% group_by(ymd(date))%>%
  mutate(rank = rank(-stockrate),stk_right = round(stockrate,digits = 3)) %>% 
  group_by(category)%>%
  filter(rank <=10)%>%
  ungroup()
staticplot = ggplot(stock_formatted, aes(rank, group = category, 
                                         fill = as.factor(category), color = as.factor(category))) +
  geom_tile(aes(y = stockrate/2,
                height = stockrate,
                width = 0.9), alpha = 0.8, color = NA) +
  geom_text(aes(y = 0, label = paste(category, " ")), vjust = 0.2, hjust = 1,size = 5) +
  geom_text(aes(y=stockrate,label = stk_right, hjust=0)) +
  coord_flip(clip = "off", expand = FALSE) +
  ylim(0,1) +
  scale_x_reverse() +
  guides(color = FALSE, fill = FALSE) +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.grid.major.x = element_line( size=.1, color="grey" ),
        panel.grid.minor.x = element_line( size=.1, color="grey" ),
        plot.title=element_text(size=25, hjust=0.5, face="bold", colour="grey", vjust=-1),
        plot.subtitle=element_text(size=18, hjust=0.5, face="italic", color="grey"),
        plot.caption =element_text(size=8, hjust=0.5, face="italic", color="grey"),
        plot.background=element_blank(),
        plot.margin = margin(2,2, 2, 4, "cm"))
anim = staticplot + transition_states(ymd(date), transition_length = 4, state_length = 1) +
  labs(title = 'Out of Stock Percentage per Day : {closest_state}',  
       subtitle  =  "Top 10 Products")
animate(anim, 500, fps = 10,  width = 1200, height = 1000, 
        renderer = gifski_renderer("gganim.gif"))
