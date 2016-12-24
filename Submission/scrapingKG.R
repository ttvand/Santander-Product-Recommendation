# Clear the workspace
rm(list=ls())

# Load the required libraries
library(rvest)
library(ggplot2)

# Set the target page
targetPage <- "https://www.kaggle.com/c/santander-product-recommendation/leaderboard"

# Restrict plot to the top teams
topTeams <- 10

# Scrape target page
page <- read_html(targetPage)

# Extract the ranking
rankingList <- page %>%
  html_table()
ranking <- rankingList[[1]][,-2]

# Trim team names
ranking$`Team Name` <- substring(ranking$`Team Name`,1,20)

# Add the score label
ranking$ScoreLab <- round(100*ranking$Score, 3)

# Plot the ranking as a bar chart
ranking$`Team Name` <- factor(ranking$`Team Name`,
                              levels = ranking$`Team Name`[order(ranking$Score,
                                                        decreasing = TRUE)])
p <- ggplot(ranking[1:topTeams,], aes(x = `Team Name`,
                                y = Score,
                                fill = `Team Name`)) + 
  geom_bar(stat = "identity") +
  # geom_hline(yintercept = 0.6212) +
  # geom_hline(yintercept = 0.6225) +
  coord_cartesian(ylim=c(0.0305, 0.031)) +
  theme(legend.position="none",
        axis.text.x=element_text(angle = -90, vjust = 0.4, hjust = 1)) +
  geom_text(aes(label=ScoreLab), position=position_dodge(width=0.9),
            vjust=-0.25)
print(p)
