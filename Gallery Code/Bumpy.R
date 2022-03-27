#' Creating a bumpy chart using R and ggplot2.
#' A bumpy chart is useful in showing how teams have moved around in the league 
#' standings in a particular period of time.

#' Libraries. Use install.packages(packagename) to install any packages that you do not have. 
#' Some packages have to be installed through github, thus search "package name github" 
#' and follow the instructions on the github page for install. 

library(tidyverse)
library(worldfootballR)
library(ggtext)
library(extrafont)
library(MetBrewer)
library(ggbump)

# Data Scraping

data <- tm_matchday_table(country_name="England", start_year="2021", matchday=c(1:30))

# Data Wrangling

## Cleaning up the column names
data <- data %>%
  rename(Rank = rk) %>%
  rename(Matchday = matchday) %>%
  rename(Squad = squad)

## Creating a new dataset of the teams we want to highlight in the viz
data1 <- data %>%
  filter(Squad == "Man Utd" | Squad == "Chelsea" | 
           Squad == "Liverpool" | Squad == "Man City" |
           Squad == "Spurs" | Squad == "Arsenal")

#' Custom Theme Function. These functions help save time. They work in the same 
#' principle of the other ggplot2 theme functions.

theme_athletic <- function() {
  theme_minimal() +
    theme(plot.background = element_rect(colour = "#151515", fill = "#151515"),
          panel.background = element_rect(colour = "#151515", fill = "#151515")) +
    theme(plot.title = element_text(colour = "white", size = 24, hjust = 0.5, face = "bold"),
          plot.subtitle = element_markdown(colour = "#525252", size = 18, hjust = 0.5),
          plot.caption = element_text(colour = "white", size = 12, hjust = 1),
          axis.title.x = element_text(colour = "white", face = "bold", size = 14),
          axis.title.y = element_text(colour = "white", face = "bold", size = 14),
          axis.text.x = element_text(colour = "white", size = 12),
          axis.text.y = element_text(colour = "white", size = 12)) +
    theme(panel.grid.major = element_line(colour = "#525252", size = 0.4, linetype = "dashed"),
          panel.grid.minor = element_line(colour = "#525252", size = 0.4, linetype = "dashed")) +
    theme(panel.grid.major.x = element_line(colour = "#525252", size = 0.4, linetype = "dashed"),
          panel.background = element_blank()) +
    theme(legend.title = element_text(colour = "white"),
          legend.text = element_text(colour = "white"),
          legend.position = "top")
}

# Plot 
  
ggplot() +
  geom_bump(data = data, aes(x = Matchday, y = Rank, group = Squad), size = 2, colour = "#525252") +
  geom_point(data = data, aes(x = Matchday, y = Rank, group = Squad), size = 4, colour = "#525252") +
  geom_bump(data = data1, aes(x = Matchday, y = Rank, group = Squad, colour = Squad), size = 2) +
  geom_point(data = data1, aes(x = Matchday, y = Rank, group = Squad, colour = Squad), size = 4) +
  scale_colour_manual(values = met.brewer(name = "Signac", n = 6, type = "discrete")) +
  scale_y_reverse(breaks = c(1:20)) +
  theme_athletic() +
  labs(title = "Matchday Rankings",
       subtitle = "Premier League Big 6 [2021/22]",
       caption = "Created by ggshakeR\nData from Transfermrkt")

# Save

ggsave("big6.png", width = 4000, height = 2000, units = "px")
