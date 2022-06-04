load("/Users/sebastiandodt/OneDrive/Uni/SOAS University of London/Modules/year 3/Environment and Climate Crisis/AS1/UNVotes.RData")
library(tidyverse)
library(ggplot2)
storage <- matrix(nrow = 74, ncol = 6)
storage[,1] <- seq(1946,2019,1)
names(storage) <- c("Year", "Alignment")
for (i in 1:74) {
  passed <- completeVotes %>%
    filter(Country == "USA") %>%
    filter(year == storage[i,1]) %>%
    mutate(passed = yes > no) %>%
    pull(passed)
  vote <- completeVotes %>%
    filter(Country == "USA") %>%
    filter(year == storage[i,1]) %>%
    pull(vote)
  storage[i,2] <- mean((vote == 1) == passed, na.rm=TRUE)
  vote <- completeVotes %>%
    filter(Country == "GBR") %>%
    filter(year == storage[i,1]) %>%
    pull(vote)
  storage[i,3] <- mean((vote == 1) == passed, na.rm=TRUE)
  vote <- completeVotes %>%
    filter(Country == "CHN") %>%
    filter(year == storage[i,1]) %>%
    pull(vote)
  storage[i,4] <- mean((vote == 1) == passed, na.rm=TRUE)
  vote <- completeVotes %>%
    filter(Country == "RUS") %>%
    filter(year == storage[i,1]) %>%
    pull(vote)
  storage[i,5] <- mean((vote == 1) == passed, na.rm=TRUE)
  vote <- completeVotes %>%
    filter(Country == "FRA") %>%
    filter(year == storage[i,1]) %>%
    pull(vote)
  storage[i,6] <- mean((vote == 1) == passed, na.rm=TRUE)
}
storage <- data.frame(storage)
storage <- storage[-19,]
storage <- storage[-74,]

ggplot(storage) +
  geom_line(aes(x = X1, y = X3, colour = "UK")) +
  geom_line(aes(x = X1, y = X6, colour = "FRA")) +
  geom_line(aes(x = X1, y = X2, colour = "USA")) +
  scale_y_continuous(labels = scales::percent) +
  scale_colour_manual("", 
                      breaks = c("USA", "UK", "CHN", "RUS", "FRA"),
                      values = c("black", "darkgrey", "lightblue", "pink", "lightgrey")) +
  xlab("Year") +
  ylab("Votes aligned with outcome") +
  ggtitle("UN General Assembly votes")

ggplot(storage) +
  geom_line(aes(x = X1, y = X4, colour = "CHN")) +
  geom_line(aes(x = X1, y = X5, colour = "RUS")) +
  scale_y_continuous(labels = scales::percent) +
  scale_colour_manual("", 
                      breaks = c("USA", "UK", "CHN", "RUS", "FRA"),
                      values = c("black", "lightgreen", "blue", "red", "lightgrey")) +
  xlab("Year") +
  ylab("Votes aligned with outcome") +
  ggtitle("UN General Assembly votes")

ggsave("plot1.png")