library(tidyverse)
library(agricolae)
library(readxl)
library(jtools)
library(ggtext)
library(extrafont)
library(scales)
loadfonts(device = "win")

data_1 <- read_excel("eoso.xlsx",sheet = "07022024")

labels <- read_excel("eoso.xlsx",sheet = "labels")


data_long <- data_1 |>
  pivot_longer(
    6:8,
    names_to = "Variable",
    values_to = "Value"
  )
variables <- unique(data_long$Variable)

stats_vars <- data_long |>
  group_by( treatment, Variable) |>
  summarise(
    Mean = mean(Value, na.rm = T),
    Sd = sd(Value, na.rm = T),
    N = n(),
    tcalc = qt(0.975, N-1),
    eem = tcalc*Sd/sqrt(N),
    Min = Mean - eem,
    Max = Mean + eem
  )


groups <- data.frame(
  treatment = 0,
  Variable = "x",
  groups = "x"
)

  for(j in seq_along(variables)){
    df <- data_long |>
      filter(Variable == variables[j])
    mod1 <- aov(Value ~ treatment, df)
    grps_mod1 <- HSD.test(mod1, "treatment")$groups
    grps_mod1 <- grps_mod1 |>
      rownames_to_column(var = "treatment") |>
      select(-2) |>
      mutate(
        Variable = variables[j]
      ) 
    groups <- rbind(groups, grps_mod1)
  }

groups <- groups |>
  filter(treatment != 0)

stats_vars$treatment <- as.character(stats_vars$treatment)
stats_vars <- left_join(stats_vars, groups)

for(i in seq_along(variables)){
  df <- data_long |>
    filter(Variable == variables[i])
  stats_vars_filtrados <- stats_vars |>
    filter(Variable == variables[i])
  name1 <- paste0("graphics/all/",variables[i],"_box.png")
  name3 <- paste0("graphics/all/",variables[i],"_bars.png")
  label <- paste0(labels[i,2])
  graphic1 <- ggplot(df, aes(x = factor(treatment), y = Value)) +
    geom_boxplot() +
    theme_apa() +
    labs(x = "Treatment", y = label) +
    theme(
      legend.position = "bottom"
    )
  grapchic3 <- ggplot(stats_vars_filtrados, aes(x = treatment, Mean)) +
    geom_bar(position=position_dodge(), stat="identity") +
    geom_errorbar(aes(ymin=Min, ymax=Max),
                  width=.3,
                  position=position_dodge(.9)) +
    scale_fill_grey(start = 0.3, end = .8)  +
    theme_apa() +
    ylab(label) +
    xlab("Treatment") +
    theme(
      legend.position = "bottom"
    )
  maximum = 1.1*(max(stats_vars_filtrados$Mean) + max(stats_vars_filtrados$eem))
  graphic3 <- grapchic3 + geom_text(data = stats_vars_filtrados, aes(x = treatment, y = maximum, label = groups),
                                   position = position_dodge(width = .75))
  ggsave(name1, graphic1, width = 8, height = 4)
  ggsave(name3, graphic3, width = 8, height = 4)
}
writexl::write_xlsx(x = stats_vars, "stats_all.xlsx")


df_wpw <- data_long %>%
  filter(Variable == "wpw")

stats_vars_wpw <- stats_vars %>%
  filter(Variable == "wpw")

name1_wpw <- "graphics/all/wpw_box.png"
name3_wpw <- "graphics/all/wpw_bars.png"

label_wpw <- "label for wpw"

graphic1_wpw <- ggplot(df_wpw, aes(x = factor(treatment), y = Value)) +
  geom_boxplot() +
  theme_apa() +
  labs(x = "Treatment", y = label_wpw) +
  theme(legend.position = "bottom")

graphic3_wpw <- ggplot(stats_vars_wpw, aes(x = treatment, Mean)) +
  geom_bar(position = position_dodge(), stat = "identity") +
  geom_errorbar(aes(ymin = Min, ymax = Max),
                width = 0.3,
                position = position_dodge(0.9)) +
  scale_fill_grey(start = 0.3, end = 0.8)  +
  theme_apa() +
  ylab(label_wpw) +
  xlab("Treatment") +
  theme(legend.position = "bottom")

maximum_wpw <- 1.1 * (max(stats_vars_wpw$Mean) + max(stats_vars_wpw$eem))
graphic3_wpw <- graphic3_wpw + geom_text(data = stats_vars_wpw, aes(x = treatment, y = maximum_wpw, label = groups),
                                         position = position_dodge(width = 0.75))

ggsave(name1_wpw, graphic1_wpw, width = 8, height = 4)
ggsave(name3_wpw, graphic3_wpw, width = 8, height = 4)

