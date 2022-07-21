#' Creating a flow/timeline chart using R and ggplot2.
#' These graphs can show how the momentum flowed in a particular match.  
#' Can be used for a variety of metrics from xT, xG and EPV.

#' Libraries. Use install.packages(packagename) to install any packages that you do not have. 
#' Some packages have to be installed through github, thus search "package name github" 
#' and follow the instructions on the github page for install. 

library(tidyverse)
library(ggtext)
library(ggbraid)
library(ggshakeR)

#' Reading in data. The only columns required are:-
#' x, y, endX, endY, teamId, minute and type (optional column)

optadata <- read.csv("livmanutd.csv")

# Basic Wrangling

optadata <- optadata %>%
  select(x, y, endX, endY, teamId, minute, type.displayName) %>%
  mutate(teamId = case_when(teamId == 26 ~ "Liverpool",
                            teamId == 32 ~ "Man Utd")) %>%
  rename(finalX = endX,
         finalY = endY,
         type = type.displayName) %>%
  filter(type == "Pass")

## Calculate xT and clean data
data <- optadata %>%
  ggshakeR::calculate_threat() %>%
  mutate(xT = xTEnd - xTStart) %>%
  mutate(xT = tidyr::replace_na(xT, 0))

## Further manipulation based on team
data1 <- data %>%
  filter(teamId == "Liverpool") %>%
  mutate(xT = ifelse(xT > 0, xT, -xT))
data2 <- data %>%
  filter(teamId == "Man Utd") %>%
  mutate(xT = ifelse(xT > 0, -xT, xT))

## Combine datasets and group by minute
data <- rbind(data1,data2) %>%
  group_by(minute) %>%
  summarise(xT = sum(xT))

## For a smoother line which goes through all plotted points, we use 
spline_int <- as.data.frame(spline(data$minute, data$xT)) %>%
  mutate(teamId = ifelse(y > 0, "Liverpool", "Man Utd"))

#' Custom Theme Function. These functions help save time. They work in the same 
#' principle of the other ggplot2 theme functions.
  
theme_custom <- function() {
  theme_minimal() +
    theme(plot.background = element_rect(colour = "#14171A", fill = "#14171A"),
          panel.background = element_rect(colour = "#14171A", fill = "#14171A")) +
    theme(plot.title = element_text(colour = "white", size = 16, hjust = 0.5, face = "bold"),
          plot.subtitle = element_markdown(colour = "white", size = 12, hjust = 0.5),
          plot.caption = element_text(colour = "white", size = 10, hjust = 1),
          axis.title.x = element_text(colour = "white", face = "bold", size = 10),
          axis.title.y = element_text(colour = "white", face = "bold", size = 10),
          axis.text.x = element_text(colour = "white", size = 9),
          axis.text.y = element_text(colour = "white", size = 9)) +
    theme(panel.grid.major = element_line(colour = "#525252", size = 0.4, linetype = "dashed"),
          panel.grid.minor = element_line(colour = "#525252", size = 0.4, linetype = "dashed")) +
    theme(panel.grid.major.x = element_line(colour = "#525252", size = 0.4, linetype = "dashed"),
          panel.background = element_blank()) +
    theme(legend.position = "bottom",
          legend.text = element_text(colour = "white"),
          legend.title = element_text(colour = "white"))
}

# Plot

ggplot(spline_int) +
  geom_braid(aes(x = x, ymin = 0, ymax = y, fill = teamId)) +
  scale_fill_manual(values = c("#E74C3C", "#3498DB")) +
  geom_line(aes(x = x, y = y), linetype = "longdash", colour = "white") +
  geom_hline(yintercept = 0, colour = "white", linetype = "solid", size = 0.5) +
  ylim(-0.5, 0.5) +
  theme_custom() +
  labs(x = "Minute", y = "xT Difference", 
       title = "Liverpool vs. Man Utd xT Flow", 
       subtitle = "2021/22 Premier League",
       fill = "Team") +
  scale_x_continuous(breaks = seq(from = 0, to = 90, by = 10))

# Save

ggsave("xTFlow.png", bg = "#14171A", width = 2600, height = 1400, units = "px")
