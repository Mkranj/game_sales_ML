library(ggplot2)

summary(original)

# Distributions
hist(original$NA_Sales)
table(original$NA_Sales)

# Sales --- millions
table(original$Platform, useNA = "always")
# Multiple ones?

table(original$Developer, useNA = "always")

table(original$Rating, useNA = "always")
# E, E10+, M and T separate categories, others under Other

# General sales by year

yearly <- group_by(original, Year_release_n) %>%
  summarise(
    no_games = n(),
    sum_money = sum(NA_Sales),
    avg_sales = mean(NA_Sales)
            )

# Years go up to 2016, 4 games assigned later -> probably mistake, make NA
# The very latest year's sales are probably inaccurate, not yet sold as much

ggplot(yearly, aes(x = Year_release_n, y = avg_sales) ) +
  geom_bar(stat = "identity")

# 2000's and later:
ggplot(yearly %>%
         filter(Year_release_n >= 2000,
                Year_release_n  != 2016),
       aes(x = Year_release_n, y = avg_sales) ) +
  geom_bar(stat = "identity")

# Calculate trend, include as extra variable?

ggplot(yearly, aes(x = Year_release_n, y = sum_money) ) +
  geom_bar(stat = "identity")

# Useful stat - general pricing. A, AA or AAA game.

# Sales by name features