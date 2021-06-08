library("tidyverse")
library("ggtext")
library("gridGraphics")

library("showtext")
showtext_auto()

# load data
library("survivoR")
data <- season_summary

# Color palette
background <- "#19005B"
grid <- "#4000DA"
start <- "#FF00F4"
end <- "yellow"
labels <- "#B161FF"

# Paired T-Test (does the average viewer return for the reunion?)
ttest <- t.test(data$viewers_mean, 
                data$viewers_reunion, 
                paired = TRUE, 
                alternative = "two.sided"
                )
pvalue <- ttest$p.value
estimate <- ttest$estimate


# print("The average number of viewers during a season dropped by about ", 
#       estimate,
#       " million for each season reunion (p = ",
#       pvalue,
#       ").")

# Plot
ggplot(data = data)+
  geom_segment(aes(x = season, xend = season, y = viewers_premier, yend = viewers_finale),
               color = "#FF00F4",
               size = 1.8) +
  geom_point(aes(x = season, y = viewers_premier), 
             color = "#FF00F4", 
             size = 1) +
  geom_point(aes(x= season, y = viewers_finale), 
             color = "yellow", 
             size = 1) +
  geom_point(aes(x= season, y = viewers_reunion), 
             color = "red",
             shape = 4,
             size = 2) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 41)) + # start x at 0 and run 41 so we can see season 40 data clearly.
  scale_y_continuous(expand = c(0, 0), limits = c(0, 55)) +
     labs(x='Season',
       y = 'Viewers (millions)',
       title = "Survivor viewership range from 
       <span style = 'color: #FF00F4;'>premier</span> to 
       <span style = 'color: yellow;'>finale</span> by season",
       caption = "Visualization by MarcellCadney.com | Source: survivoR by @danoehm | TidyTuesday Week 23")+
  theme(plot.title = element_markdown(size = 45, family = "mono", hjust = 0.5, color = "white"),
        panel.background = element_rect(fill = "#19005B"),
        plot.background = element_rect(fill = "#19005B"),
        panel.grid = element_line(color = grid),
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
        plot.caption = element_markdown(size = 40, family = "mono", color = start),
        axis.line = element_line(color = grid, size = 1, linetype = "solid"),
        axis.title = element_markdown(size = 40, family = "mono", color = "white"),
        axis.text = element_markdown(size = 35, family = "mono", color = "white"),
        axis.ticks = element_blank()
        )

# Save data - Make sure to add alt text.
ggsave(dpi = 700, width = 5, height = 5, units = "in", 
       filename = "210601_MC.jpeg", device = "jpeg")
