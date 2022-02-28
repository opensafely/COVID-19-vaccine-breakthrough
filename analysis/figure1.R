######################################

# This script 
# - produces a cumulative incidence plot of follow-up-time
# - saves plot as svg

######################################


# Preliminaries ----

## Import libraries
library('tidyverse')
library('lubridate')
library('reshape2')
library('here')
library('survival')
library('survminer')
library(readr)

## Create output directory
dir.create(here::here("released_outputs", "output", "figures"), showWarnings = FALSE, recursive=TRUE)

## Import data
surv_data_all <- read_csv(here::here("released_outputs", "output", "data", "surv_data_all.csv")) %>% filter(time < 290)
surv_data_groups <- read_csv(here::here("released_outputs", "output", "data", "surv_data_groups.csv")) %>% 
  filter(time < 290) %>%
  mutate(group = ifelse(group == "Shielding (age 16-69) (priority group 4)", "CEV (age 16-69) (priority group 4)", group))
surv_data_risk_table <- read_csv(here::here("released_outputs", "output", "data", "surv_data_risk_table.csv")) %>%
  mutate(group = ifelse(group == "Shielding (age 16-69) (priority group 4)", "CEV (age 16-69) (priority group 4)", group))


## Plot
surv_plot_ci <- surv_data_groups %>%
  ggplot(aes(x = time, y = cum.in, colour = group)) +
  geom_step(size = 0.5) +
  geom_ribbon(aes(ymin = lci, ymax = uci, fill = group), alpha=0.2, colour = "transparent") +
  geom_step(data = surv_data_all, aes(x = time, y = cum.in, colour = "All"), size = 0.5, linetype = 2) +
  geom_ribbon(data = surv_data_all, aes(ymin = lci, ymax = uci), alpha=0.2, colour="transparent") +
  scale_x_continuous(breaks = seq(0,300,50)) +
  scale_y_continuous(expand = expansion(mult=c(0,0.01))) +
  coord_cartesian(xlim=c(0, 300)) +
  labs(
    x = "Days since being fully vaccinated",
    y = "Cumulative incidence of COVID-19 infection",
    colour = "Priority Group",
    title = "") +
  theme_minimal(base_size = 9) +
  theme(
    legend.position = c(0.2,0.65),
    legend.background = element_rect(colour = "white"),
    legend.box.background = element_rect(colour = "black"),
    axis.line.x = element_line(colour = "black"),
    panel.grid.minor.x = element_blank(),
    legend.title = element_blank(),
    legend.box.margin = margin(t = 1, l = 1, b = 1, r = 1))+ 
  guides(fill = "none") +
  scale_color_manual(values = c("All" = "black",
                                "Care home (priority group 1)" = "#00BFC4",
                                "80+ (priority group 2)" = "#7CAE00",
                                "Health/care workers (priority groups 1-2)" = "#00A9FF",
                                "70-79 (priority groups 3-4)" = "#CD9600",
                                "CEV (age 16-69) (priority group 4)" = "#FF61CC",
                                "50-69 (priority groups 5-9)" = "#F8766D",
                                "Others not in the above groups (under 50)" = "#C77CFF"))


surv_table <- ggplot(surv_data_risk_table %>% filter(!(time %in% c(25,75,125,175,225,275))), aes(time, group)) + 
  geom_text(aes(label = n.risk), size = 2) +
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(fill= "white", size = 1),
        axis.text.x = element_text(color = "black", size = 6),
        axis.text.y = element_text(color = c("#C77CFF", "#F8766D", "#FF61CC", "#CD9600", "#00A9FF",
                                             "#7CAE00", "#00BFC4"), size = 4),
        axis.title = element_text(color = "black", size = 8),
        strip.text = element_text(color = "black", size = 3)) +
    scale_x_continuous(breaks = seq(0,300,50)) +
    scale_y_discrete(limits = c("Others not in the above groups (under 50)",
                              "50-69 (priority groups 5-9)",
                              "CEV (age 16-69) (priority group 4)",
                              "70-79 (priority groups 3-4)",
                              "Health/care workers (priority groups 1-2)",
                              "80+ (priority group 2)",
                              "Care home (priority group 1)")) +
  xlab("") + 
  ylab("Priority group") +
  ggtitle("Number at Risk")

gA <- ggplotGrob(surv_plot_ci)
gB <- ggplotGrob(surv_table)
maxWidth = grid::unit.pmax(gA$widths[2:5], gB$widths[2:5])
gA$widths[2:5] <- as.list(maxWidth)
gB$widths[2:5] <- as.list(maxWidth)
#gridExtra::grid.arrange(gA, gB, ncol=1,heights = c(4, 1))
surv_plot_table <- gridExtra::arrangeGrob(gA, gB, ncol=1,heights = c(4, 1))


## CI
surv_data_groups %>%
  filter(time == 275) %>%
  select(cum.in, group)

## Save plot
ggsave(
  here::here("released_outputs", "output", "figures", "figure1.png"),
  surv_plot_table,
  units = "cm", width = 35, height = 20
)

