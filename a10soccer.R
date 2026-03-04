# a10soccer.R
# Generates TWO plots:
# 1) CPCT vs Scholarship Dollars (two Davidson points: Athletic Only + With Aid)
# 2) CPCT vs Number of Scholarships (one Davidson point: Athletic Only)
# Also prints three correlation (r) values for Plot 1 (dollars) to the console:
#   - both Davidson points included
#   - only Davidson (Athletic Only)
#   - only Davidson (With Aid)

library(tidyverse)
library(ggimage)
library(googlesheets4)
library(scales)

sheet_url <- "https://docs.google.com/spreadsheets/d/17ve6Orx7NRok2XsllC52on6BPVtvAI02jeuWrgZSC-M/edit?gid=0#gid=0"
sheet_range <- "Q2:U15"   # includes: School, CPCT., Dollars, Number of Scholarships

# gs4_auth() # comment/uncomment after first authorization

df_raw <- read_sheet(
  ss = sheet_url,
  range = sheet_range,
  col_types = "cdddd"
)

df <- df_raw %>%
  select(School, `CPCT.`, `Men's Soccer Sch. Dollars`, `Number of Scholarships`) %>%
  rename(
    school = School,
    cpct = `CPCT.`,
    scholarship = `Men's Soccer Sch. Dollars`,
    scholarships = `Number of Scholarships`
  ) %>%
  filter(!is.na(school), school != "") %>%
  mutate(
    scholarship = parse_number(as.character(scholarship)),
    scholarships = as.numeric(scholarships)
  )

# Base dataset: one Davidson point (Athletic Only) — used for plot 2 and for baseline r
df_base <- df %>%
  mutate(school = if_else(school == "Davidson", "Davidson (Athletic Only)", school))

# Extra Davidson scenario: With Aid — used only for plot 1 (dollars)
davidson_with_aid <- df %>%
  filter(school == "Davidson") %>%
  mutate(
    school = "Davidson (With Aid)",
    scholarship = 667432  
  )

# Dataset for plot 1: includes BOTH Davidson points
df_plot_dollars <- bind_rows(df_base, davidson_with_aid)

# Logo paths + labels
slugify <- function(x) {
  x %>%
    str_to_lower() %>%
    str_replace_all("\\(.*\\)", "") %>%
    str_replace_all("[^a-z0-9]", "")
}

df_base <- df_base %>%
  mutate(
    logo = file.path("A10 Logos", paste0(slugify(school), ".png")),
    logo = if_else(str_detect(school, "^Davidson"), file.path("A10 Logos", "davidson.png"), logo)
  )

df_plot_dollars <- df_plot_dollars %>%
  mutate(
    logo = file.path("A10 Logos", paste0(slugify(school), ".png")),
    logo = if_else(str_detect(school, "^Davidson"), file.path("A10 Logos", "davidson.png"), logo),
    label = case_when(
      school == "Davidson (Athletic Only)" ~ "(Athletic Only)",
      school == "Davidson (With Aid)" ~ "(With Aid)",
      TRUE ~ ""
    )
  )

missing1 <- df_plot_dollars %>% filter(!file.exists(logo)) %>% distinct(school, logo)
missing2 <- df_base %>% filter(!file.exists(logo)) %>% distinct(school, logo)
missing <- bind_rows(missing1, missing2) %>% distinct()

if (nrow(missing) > 0) {
  print("Missing logo files:")
  print(missing)
}

# Correlations (r) for Plot 1 (dollars)
r_both <- cor(df_plot_dollars$scholarship, df_plot_dollars$cpct, use = "complete.obs")
r_athletic_only <- cor(df_base$scholarship, df_base$cpct, use = "complete.obs")

df_with_aid_only <- df %>%
  filter(school != "Davidson") %>%
  bind_rows(davidson_with_aid %>% select(school, cpct, scholarship) %>%
              mutate(school = "Davidson (With Aid)"))

r_with_aid_only <- cor(df_with_aid_only$scholarship, df_with_aid_only$cpct, use = "complete.obs")

cat("\nCorrelation (r) Results (CPCT vs Scholarship Dollars):\n")
cat("Both Davidson points included:  ", round(r_both, 3), "\n")
cat("Only Davidson (Athletic Only):  ", round(r_athletic_only, 3), "\n")
cat("Only Davidson (With Aid):       ", round(r_with_aid_only, 3), "\n\n")

# Plot 1: CPCT vs Scholarship Dollars (two Davidson points)
p1 <- ggplot(df_plot_dollars, aes(x = scholarship, y = cpct)) +
  geom_image(aes(image = logo), size = 0.07) +
  geom_text(aes(label = label), nudge_y = -0.05, hjust = 0.5, size = 2) +
  scale_x_continuous(
    labels = dollar_format(),
    expand = expansion(mult = c(0.03, 0.10))
  ) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    title = "A10 Men's Soccer: CPCT vs Scholarship Spending",
    x = "Scholarship Dollars Spent",
    y = "Winning / Points % (CPCT)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.margin = margin(10, 35, 25, 10),
    plot.title = element_text(hjust = 0.5),
    plot.title.position = "plot"
  )

print(p1)

# Plot 2: CPCT vs Number of Scholarships (one Davidson point)
p2 <- ggplot(df_base, aes(x = scholarships, y = cpct)) +
  geom_image(aes(image = logo), size = 0.07) +
  scale_x_continuous(expand = expansion(mult = c(0.03, 0.10))) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    title = "A10 Men's Soccer: CPCT vs Number of Scholarships",
    x = "Number of Scholarships",
    y = "Winning / Points % (CPCT)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.margin = margin(10, 35, 25, 10),
    plot.title = element_text(hjust = 0.5),
    plot.title.position = "plot"
  )

print(p2)