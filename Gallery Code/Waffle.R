#' Creating a waffle chart using R and ggplot2.
#' A waffle chart is useful in showing the components of whole.  
#' It is a fancier and better version of a pie chart. 

#' Libraries. Use install.packages(packagename) to install any packages that you do not have. 
#' Some packages have to be installed through github, thus search "package name github" 
#' and follow the instructions on the github page for install. 

library(tidyverse)
library(worldfootballR)
library(ggtext)
library(extrafont)
library(waffle)
library(MetBrewer)

# Scraping

data <- fb_big5_advanced_season_stats(season_end_year = 2022, stat_type = "gca", team_or_player = "player")

# Data Wrangling 

data1 <- data %>%
  filter(Mins_Per_90 >= 20) %>%
  select(Player, Mins_Per_90, contains("SCA")) %>%
  arrange(desc(SCA90_SCA)) %>%
  slice(1:20)

df <- data1 ## copying dataset for reordering levels for player names

# Per90 values across all columns

data2 <- data1 %>% 
  mutate(across(-c(Player, Mins_Per_90, SCA90_SCA), ~ . / Mins_Per_90)) 

# Percentage of all column values. Creating a total column of all variables

data3 <- data2 %>%
  mutate(across(-c(Player, Mins_Per_90, SCA90_SCA), ~ (. / SCA_SCA) * 100 )) %>% 
  mutate(Total = PassLive_SCA + PassDead_SCA + Drib_SCA + Sh_SCA + Fld_SCA + Def_SCA) %>% 
  select(Player, Mins_Per_90, Total, contains("SCA"))

# Custom function to get a complete total of hundred after rounding for percentages

round_preserve_sum <- function(x, digits = 0) {
  up <- 10^digits
  x <- x * up
  y <- floor(x)
  indices <- tail(order(x - y), round(sum(x)) - sum(y))
  y[indices] <- y[indices] + 1
  y / up
}

data4 <- data3 %>% 
  select(-Mins_Per_90, -SCA90_SCA, -SCA_SCA, -Total) %>% 
  pivot_longer(cols = -Player) %>% 
  pivot_wider(names_from = Player, values_from = value) %>% 
  mutate(across(-name, ~ round_preserve_sum(.))) %>% 
  pivot_longer(names_to = "Player", values_to = "thing", cols = -name) %>% 
  pivot_wider(names_from = name, values_from = thing) %>% 
  mutate(Total = PassLive_SCA + PassDead_SCA + Drib_SCA + Sh_SCA + Fld_SCA + Def_SCA)

# Cleaning up dataset for the final viz

data5 <- data4 %>%
  select(-Total) %>% 
  pivot_longer(!Player, values_to = "SCAp90", names_to = "SCATypes") %>%
  mutate(SCATypes = case_when(
    SCATypes == "Def_SCA" ~ "Defensive Action SCA",
    SCATypes == "Drib_SCA" ~ "Dribble SCA",
    SCATypes == "Fld_SCA" ~ "Fouled SCA",
    SCATypes == "PassDead_SCA" ~ "Dead-ball Pass SCA",
    SCATypes == "PassLive_SCA" ~ "Pass SCA",
    SCATypes == "Sh_SCA" ~ "Shot SCA",
    TRUE ~ SCATypes
  )) %>% 
  count(Player, SCATypes, wt = SCAp90)

data5$Player <- factor(data5$Player, levels = print(df$Player)) ## reorder player names 

#' Custom Theme Function. These functions help save time. They work in the same 
#' principle of the other ggplot2 theme functions.

theme_custom <- function() {
  theme_minimal() +
    theme(plot.background = element_rect(colour = "#0d1117", fill = "#0d1117"),
          panel.background = element_rect(colour = "#0d1117", fill = "#0d1117")) +
    theme(plot.title = element_text(colour = "white", size = 24, hjust = 0.5, face = "bold"),
          plot.subtitle = element_markdown(colour = "#44A2A7", size = 18, hjust = 0.5),
          plot.caption = element_text(colour = "white", size = 15, hjust = 1),
          axis.title.x = element_text(colour = "white", face = "bold", size = 14),
          axis.title.y = element_text(colour = "white", face = "bold", size = 14),
          axis.text.x = element_blank(),
          axis.text.y = element_blank()) +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank()) +
    theme(legend.title = element_text(colour = "white"),
          legend.text = element_text(colour = "white"))
}

# Plotting

data5 %>%
  ggplot(aes(fill = SCATypes, values = n)) +
  geom_waffle(nrows = 10, size = 1.5, colour = "#0d1117", flip = TRUE) +
  scale_fill_manual(values = met.brewer(name = "Signac", n = 6, type = "discrete")) +
  facet_wrap(~Player) +
  labs(title = "Big 5 Leagues Shot-Creating Actions Share [2021/22]",
       subtitle = "Top 20 Players with the most SCA per 90 so far",
       caption = "Minimum 9 90's Played\nData from FBref\nCreated by @placeholder2004") +
  theme_custom() +
  theme(aspect.ratio = 1,
        strip.background = element_blank(),
        strip.text = element_text(colour = "white", size = 14),
        legend.position = "bottom",
        legend.text = element_text(size = 14))

# Save

ggsave("wafflebig5.png", width = 3100, height = 3500, units = "px")
