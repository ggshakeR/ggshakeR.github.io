#' Creating a dumbbell chart using R and ggplot2.
#' A dumbbell chart is used for showing the difference in value between two quantities.  

#' Libraries. Use install.packages(packagename) to install any packages that you do not have. 
#' Some packages have to be installed through github, thus search "package name github" 
#' and follow the instructions on the github page for install. 

library(tidyverse)
library(worldfootballR)
library(ggtext)
library(ggalt)

# Scrape Data

league_table <- get_season_team_stats(country = "ENG", gender = "M", season_end_year = "2022", tier = "1st", stat_type = "league_table_home_away")

# Data Wrangling  

data <- league_table %>%
  select(Squad, GD_Home, GD_Away, xGD_Home, xGD_Away, MP_Home, MP_Away) %>%
  mutate(across(-Squad, ~ as.numeric(.))) %>%
  mutate(xGDiff_Home = GD_Home - xGD_Home,
         xGDiff_Away = GD_Away - xGD_Away) %>%
  mutate(xGDiff_Home = xGDiff_Home/MP_Home,
         xGDiff_Away = xGDiff_Away/MP_Away) %>%
  select(Squad, xGDiff_Home, xGDiff_Away)

# Ordering Squads by league position

df <- league_table %>%
  mutate(Rk = as.numeric(Rk)) %>%
  arrange(desc(Rk))

data$Squad <- factor(data$Squad, levels = as.character(df$Squad))

#' Custom Theme Function. These functions help save time. They work in the same 
#' principle of the other ggplot2 theme functions.

theme_custom <- function() {
  theme_minimal() +
    theme(plot.background = element_rect(colour = "#EAEDED", fill = "#EAEDED"),
          panel.background = element_rect(colour = "#EAEDED", fill = "#EAEDED")) +
    theme(plot.title = element_text(colour = "black", size = 24, hjust = 0.5, face = "bold"),
          plot.subtitle = element_markdown(colour = "#525252", size = 18, hjust = 0.5),
          plot.caption = element_text(colour = "black", size = 12, hjust = 1),
          axis.title.x = element_text(colour = "black", face = "bold", size = 14),
          axis.title.y = element_text(colour = "black", face = "bold", size = 14),
          axis.text.x = element_text(colour = "black", size = 12),
          axis.text.y = element_text(colour = "black", size = 12)) +
    theme(panel.grid.major = element_line(colour = "#525252", size = 0.4, linetype = "dashed"),
          panel.grid.minor = element_line(colour = "#525252", size = 0.4, linetype = "dashed")) +
    theme(panel.grid.major.x = element_line(colour = "#525252", size = 0.4, linetype = "dashed"),
          panel.background = element_blank())
}

# Plotting

ggplot(data) +
  geom_segment(aes(x = xGDiff_Home, xend = xGDiff_Away, y = Squad, yend = Squad), color = "#b2b2b2", size = 3) +
  geom_dumbbell(aes(x = xGDiff_Home, xend = xGDiff_Away, y = Squad), 
                color = NA, size_x = 10, size_xend = 10, colour_x = "#2E86C1", colour_xend = "#CB4335") +
  labs(title = "Overperformance or Underperformance ? (2021/22)", 
       subtitle = "<b style='color:#000000'>How have Premier League teams performed on their Goal Difference</b> 
                   <b style='color:#2E86C1'>Home</b> <b style='color:#000000'>&</b> 
                   <b style='color:#CB4335'>Away</b> <b style='color:#000000'>?</b>",
       caption = "Data from FBref via StatsBomb (WorldFootballR)\nCreated by @veryharshtakes",
       x = "GD - xGD Per 90") +
  theme_custom()

# Save 

ggsave("dumb.png", width = 4000, height = 2500, units = "px")
