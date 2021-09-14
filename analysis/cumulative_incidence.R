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

## Create output directory
dir.create(here::here("output", "figures"), showWarnings = FALSE, recursive=TRUE)

## Custome function
fct_case_when <- function(...) {
  # uses dplyr::case_when but converts the output to a factor,
  # with factors ordered as they appear in the case_when's  ... argument
  args <- as.list(match.call())
  levels <- sapply(args[-1], function(f) f[[3]])  # extract RHS of formula
  levels <- levels[!is.na(levels)]
  factor(dplyr::case_when(...), levels=levels)
}

## Import data
data_processed <- read_rds(here::here("output", "data", "data_processed.rds"))

## Format groups
data_processed <- data_processed %>%
  mutate(group = ifelse(care_home_65plus == 1, 1, NA),
         group = ifelse(is.na(group) & ageband == 3, 2, group),
         group = ifelse(is.na(group) & hscworker == 1, 3, group),
         group = ifelse(is.na(group) & ageband == 2, 4, group),
         group = ifelse(is.na(group) & shielded == 1, 5, group),
         group = ifelse(is.na(group) & age >=50 & age <70, 6, group),
         group = ifelse(is.na(group), 7, group),
         
         group = fct_case_when(
           group == "1" ~ "Care home (priority group 1)",
           group == "2" ~ "80+ (priority group 2)",
           group == "3" ~ "Health/care workers (priority groups 1-2)",
           group == "4" ~ "70-79 (priority groups 3-4)",
           group == "5" ~ "Shielding (age 16-69) (priority group 4)",
           group == "6" ~ "50-69 (priority groups 5-9)",
           group == "7" ~ "Others not in the above groups (under 50)",
           #TRUE ~ "Unknown"
           TRUE ~ NA_character_))


# Plot ----
threshold <- 7

## Data
surv_data_all <- survfit(Surv(time = time_to_positive_test, event = covid_positive_test) ~ 1, 
                         data = data_processed)  %>% 
  broom::tidy() %>% 
  filter(estimate > 0) %>%
  mutate(
    estimate = pmin(1,plyr::round_any(estimate, threshold/max(n.risk)), na.rm=TRUE),
    conf.low = pmin(1, plyr::round_any(conf.low, threshold/max(n.risk)), na.rm=TRUE),
    conf.high = pmin(1, plyr::round_any(conf.high, threshold/max(n.risk)), na.rm=TRUE),
    cum.in = 1 - estimate,
    lci = 1- conf.high,
    uci = 1 - conf.low
  )

surv_data_groups <- survfit(Surv(time = time_to_positive_test, event = covid_positive_test) ~ group, 
                            data = data_processed) %>% 
  broom::tidy() %>% 
  filter(estimate > 0) %>%
  mutate(
    estimate = pmin(1,plyr::round_any(estimate, threshold/max(n.risk)), na.rm=TRUE),
    conf.low = pmin(1, plyr::round_any(conf.low, threshold/max(n.risk)), na.rm=TRUE),
    conf.high = pmin(1, plyr::round_any(conf.high, threshold/max(n.risk)), na.rm=TRUE),
    cum.in = 1 - estimate,
    lci = 1- conf.high,
    uci = 1 - conf.low
  ) %>%
  mutate(group = gsub(".*=","", strata),
         group = factor(group, levels = c("All",
                                          "Care home (priority group 1)",
                                          "80+ (priority group 2)",
                                          "Health/care workers (priority groups 1-2)",
                                          "70-79 (priority groups 3-4)",
                                          "Shielding (age 16-69) (priority group 4)",
                                          "50-69 (priority groups 5-9)",
                                          "Others not in the above groups (under 50)")))

surv_data_risk_table <- ggsurvplot(survfit(Surv(time = time_to_positive_test, event = covid_positive_test) ~ group, 
                   data = data_processed), risk.table = TRUE)$data.survtable %>%
  select(group, time, n.risk) %>%
  mutate(`n.risk` = ifelse(`n.risk` < 8, "<8", `n.risk`),
         group = factor(group, levels = c("Care home (priority group 1)",
                                          "80+ (priority group 2)",
                                          "Health/care workers (priority groups 1-2)",
                                          "70-79 (priority groups 3-4)",
                                          "Shielding (age 16-69) (priority group 4)",
                                          "50-69 (priority groups 5-9)",
                                          "Others not in the above groups (under 50)")))
  

## Plot
surv_plot <- surv_data_groups %>%
  ggplot(aes(x = time, y = cum.in, colour = group)) +
  geom_step(data = surv_data_all, aes(x = time, y = cum.in, colour = "All"), size = 1, linetype = 1) +
  geom_step(size = 0.5) +
  #geom_ribbon(aes(ymin = lci, ymax = uci, fill = group), alpha=0.2, colour = "transparent") +
  #geom_ribbon(data = surv_data_all, aes(ymin = lci, ymax = uci), alpha=0.2, colour="transparent") +
  scale_x_continuous(breaks = seq(0, max(surv_data_groups$time),50)) +
  scale_y_continuous(expand = expansion(mult=c(0,0.01))) +
  coord_cartesian(xlim=c(0, max(surv_data_groups$time))) +
  labs(
    x = "Days since being fully vaccinated",
    y = "Cumulative incidence of positive SARS-CoV-2 test",
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
    legend.box.margin = margin(t = 1, l = 1, b = 1, r = 1)) + 
  scale_color_manual(values = c("All" = "black",
                                "Care home (priority group 1)" = "#00BFC4",
                                "80+ (priority group 2)" = "#7CAE00",
                                "Health/care workers (priority groups 1-2)" = "#00A9FF",
                                "70-79 (priority groups 3-4)" = "#CD9600",
                                "Shielding (age 16-69) (priority group 4)" = "#FF61CC",
                                "50-69 (priority groups 5-9)" = "#F8766D",
                                "Others not in the above groups (under 50)" = "#C77CFF")) +
  guides(col = guide_legend(order = 1)) 

surv_table <- ggplot(surv_data_risk_table, aes(time, group)) + 
  geom_text(aes(label = n.risk), size = 2) +
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(fill= "white", size = 1),
        axis.text.x = element_text(color = "black", size = 6),
        axis.text.y = element_text(color = c("#00BFC4", "#7CAE00", "#00A9FF", "#CD9600", "#FF61CC",
                                             "#F8766D", "#C77CFF"), size = 4),
        axis.title = element_text(color = "black", size = 8),
        strip.text = element_text(color = "black", size = 3)) +
  scale_y_discrete(limits = c("Others not in the above groups (under 50)",
                              "50-69 (priority groups 5-9)",
                              "Shielding (age 16-69) (priority group 4)",
                              "70-79 (priority groups 3-4)",
                              "Health/care workers (priority groups 1-2)",
                              "80+ (priority group 2)",
                              "Care home (priority group 1)")) +
  xlab("") + 
  ylab("Priority group") +
  ggtitle("Number at Risk")

gA <- ggplotGrob(surv_plot)
gB <- ggplotGrob(surv_table)
maxWidth = grid::unit.pmax(gA$widths[2:5], gB$widths[2:5])
gA$widths[2:5] <- as.list(maxWidth)
gB$widths[2:5] <- as.list(maxWidth)
#gridExtra::grid.arrange(gA, gB, ncol=1,heights = c(4, 1))
surv_plot_table <- gridExtra::arrangeGrob(gA, gB, ncol=1,heights = c(4, 1))

surv_plot_ci <- surv_data_groups %>%
  ggplot(aes(x = time, y = cum.in, colour = group)) +
  geom_step(size = 0.5) +
  geom_ribbon(aes(ymin = lci, ymax = uci, fill = group), alpha=0.2, colour = "transparent") +
  geom_step(data = surv_data_all, aes(x = time, y = cum.in, colour = "All"), size = 0.5, linetype = 2) +
  geom_ribbon(data = surv_data_all, aes(ymin = lci, ymax = uci), alpha=0.2, colour="transparent") +
  scale_x_continuous(breaks = seq(0,140,25)) +
  scale_y_continuous(expand = expansion(mult=c(0,0.01))) +
  coord_cartesian(xlim=c(0, 100)) +
  labs(
    x = "Days since being fully vaccinated",
    y = "Cumulative incidence of COVID-19 infection",
    colour = "Priority Group",
    title = "") +
  theme_minimal(base_size = 9) +
  theme(
    legend.position = "right",
    axis.line.x = element_line(colour = "black"),
    panel.grid.minor.x = element_blank(),
    legend.title = element_blank()) + 
  guides(fill = "none") +
  scale_color_manual(values = c("All" = "black",
                                "Care home (priority group 1)" = "#00BFC4",
                                "80+ (priority group 2)" = "#7CAE00",
                                "Health/care workers (priority groups 1-2)" = "#00A9FF",
                                "70-79 (priority groups 3-4)" = "#CD9600",
                                "Shielding (age 16-69) (priority group 4)" = "#FF61CC",
                                "50-69 (priority groups 5-9)" = "#F8766D",
                                "Others not in the above groups (under 50)" = "#C77CFF"))


## CI
surv_data_groups %>%
  filter(time == 140) %>%
  select(cum.in, group)

## Save plot
ggsave(
  here::here("output", "figures", "figure1.png"),
  surv_plot_table,
  units = "cm", width = 35, height = 20
)

ggsave(
  here::here("output", "figures", "figure1_cis.svg"),
  surv_plot_ci,
  units = "cm", width = 30, height = 15
)

## SAve data behind plots
write_csv(surv_data_all, here::here("output", "data", "surv_data_all.csv"), compress = "gz")
write_csv(surv_data_groups, here::here("output", "data", "surv_data_groups.csv"), compress = "gz")

