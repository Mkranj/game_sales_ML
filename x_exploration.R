
summary(original)

# Distributions
hist(original$NA_Sales)
table(original$NA_Sales)

# Sales --- millions
table(original$Platform, useNA = "always")
# Multiple ones?

table(original$Developer, useNA = "always")
# TODO units sold per each developer (average per number of games). Grab top 10%, new categorical: is_top_selling_company
# Same, but grab numbers of games made. So "top outputer" is a new variable. Could also include
# interaction top sales x top output

table(original$Rating, useNA = "always")
# E, E10+, M and T separate categories, others under Other