library(lubridate)
library(dplyr)
library(RColorBrewer)
library(gtable, grid)

# Variables to store locations and titles
# Must point to directory with data files
file_location <- "directory_pointing_to_csv_file"
title <- "Expense report February 2023"
save_as <- "expense_report_feb_2023.pdf"


raw_data <- read.csv(
    file_location,
    header = TRUE,
    stringsAsFactors = FALSE,
    )
# Remove duplicate rows
raw_data <- distinct(raw_data)
# Also want to include field to customize plot title
plot_title <- title

raw_data <- raw_data[, c(1:5)]

raw_data$Date <- ymd(raw_data$Date)

# Group by category using dplyr
totals_by_cat <- raw_data %>%
    group_by(Category) %>%
    summarise(sum_expenses = sum(Amount),
    .groups = "drop") %>%
    arrange(desc(sum_expenses)) %>%
    top_n(20)

# Convert tibble to df
df_1 <- totals_by_cat %>% as.data.frame()

# Group by date using dplyr
totals_by_date <- raw_data %>%
            group_by(Date) %>%
            summarise(sum_expenses = sum(Amount),
            .groups = "drop") %>%
            arrange(desc(sum_expenses))

# Convert tibble to df
df_2 <- totals_by_date %>% as.data.frame()

df_3 <- raw_data %>%
    group_by(Category) %>%
    filter(n() > 4) %>%
    arrange(desc(Amount))
df_3 <- df_3 %>% as.data.frame()

# Group by merchant using dplyr
totals_by_merch <- raw_data %>%
    group_by(Description) %>%
    summarise(sum_expenses = sum(Amount),
    .groups = "drop") %>%
    arrange(desc(sum_expenses)) %>%
    top_n(20)
# Convert tibble to df
df_4 <- totals_by_merch %>% as.data.frame()

df_5 <- raw_data
df_5 <- df_5 %>% select(c("Description", "Amount"))

# Group by sum using dplyr
totals_by_merchant <- raw_data %>%
            group_by(Description) %>%
            summarise(sum_expenses = sum(Amount),
            .groups = "drop") %>%
            arrange(desc(sum_expenses))
# totals_by_merchant

# Getting sum of the entire Amount column
amount_sum <- sum(totals_by_merchant$sum_expenses)

# Perform calculation on column to get proportion
totals_by_merchant$proportion <- with(
                                totals_by_merchant,
                                totals_by_merchant$sum_expenses / amount_sum
)

# Select first 20 rows of the df
df_5 <- totals_by_merchant[1:20, c("Description", "proportion")]

df_6 <- raw_data
df_6 <- df_6 %>% select(c("Day", "Amount"))
# Group by day of week using dplyr
df_6 <- df_6 %>%
            group_by(Day) %>%
            summarise(sum_expenses = sum(Amount),
            .groups = "drop") %>%
            arrange(match(Day, c(
                            "Monday",
                            "Tuesday",
                            "Wednesday",
                            "Thursday",
                            "Friday",
                            "Saturday",
                            "Sunday"
                            )
                        )
                    )
df_6 <- df_6 %>% as.data.frame()

coul <- brewer.pal(5, "Accent") ## variable to store plot style
coul_2 <- brewer.pal(5, "Spectral")
coul_3 <- brewer.pal(5, "Dark2")
coul_4 <- brewer.pal(6, "Set3")
coul_5 <- brewer.pal(5, "Set2")
coul_6 <- brewer.pal(7, "Set1")
pdf(
    save_as,
    width = 8.5,
    height = 11,
    bg = "white",
    paper = "US" ## setting it to letter in landscape
)  ## Device with dimensions of letter paper
par(omi = rep(.5, 4))                      ## 1/2 inch outer margins
par(mfrow = c(3, 2))                        ## 3x2 grid of plotting areas
barplot(
    df_1$sum_expenses,
    names.arg = df_1$Category,
    ylab = "dollars ($)",
    main = "Total spent per category",
    las = 2,
    col = coul,
    cex.names = 0.75
) ## Plot 1
plot(
    df_2$sum_expenses ~ df_2$Date,
    main = "Amount spent per day",
    xlab = "",
    ylab = "dollars ($)",
    type = "p",
    col = coul_2
) ## Plot 2
boxplot(
    Amount ~ Category,
    data = df_3,
    main = "Transactions n > 3",
    xlab = "",
    ylab = "dollars ($)",
    las = 3,
    cex.axis = 0.75,
    frame = FALSE,
    col = coul_4
) ## Plot 3
hist(
    raw_data$Amount,
    breaks = 100,
    xlim = c(0, 200),
    col = coul_3,
    main = "Histogram of amount spent - 10 dollar bins",
    xlab = "Amount ($)"
) ## Plot 4
barplot(
    df_5$proportion,
    main = "Proportion of total spent",
    names.arg = df_5$Description,
    ylab = "Proportion (unitless)",
    las = 2,
    col = coul_5,
    cex.names = 0.80
) ## Plot 6
plot(
    df_6$sum_expenses,
    main = "Amount spent per day of week",
    xlab = "Day of the week",
    ylab = "dollars ($)",
    type = "p",
    xaxt = "n",
    pch = 17,
    col = #darkmagenta
)
axis(
    side = 1,
    at = 1:7,
    labels = c(
        "mon",
        "tues",
        "wed",
        "thur",
        "fri",
        "sat",
        "sun"
    )
)
title <- plot_title
grid::grid.text(plot_title, x = (0.5), y = (0.95))
dev.off()
