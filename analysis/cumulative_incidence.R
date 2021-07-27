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
data_processed <- read_rds(here::here("output", "data", "data_all.rds"))

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
           TRUE ~ NA_character_)) %>%
  mutate(time_to_positive_test = ifelse(time_to_positive_test < 0, follow_up_time_vax2, time_to_positive_test))


# Plot ----
threshold <- 7

## Data
surv_data_all <- survfit(Surv(time = time_to_positive_test, event = covid_positive_post_2vacc) ~ 1, 
                     data = data_processed) %>% 
  broom::tidy() %>% 
  mutate(
    estimate = pmin(1,plyr::round_any(estimate, threshold/max(n.risk)), na.rm=TRUE),
    conf.low = pmin(1, plyr::round_any(conf.low, threshold/max(n.risk)), na.rm=TRUE),
    conf.high = pmin(1, plyr::round_any(conf.high, threshold/max(n.risk)), na.rm=TRUE),
    cum.in = 1 - estimate,
    lci = 1- conf.high,
    uci = 1 - conf.low
  )

print(surv_data_all %>% 
        filter(n.event < 5))

surv_data_groups <- survfit(Surv(time = time_to_positive_test, event = covid_positive_post_2vacc) ~ group, 
                   data = data_processed) %>% 
  broom::tidy() %>% 
  mutate(
    estimate = pmin(1,plyr::round_any(estimate, threshold/max(n.risk)), na.rm=TRUE),
    conf.low = pmin(1, plyr::round_any(conf.low, threshold/max(n.risk)), na.rm=TRUE),
    conf.high = pmin(1, plyr::round_any(conf.high, threshold/max(n.risk)), na.rm=TRUE),
    cum.in = 1 - estimate,
    lci = 1- conf.high,
    uci = 1 - conf.low
  ) %>%
  mutate(group = gsub(".*=","", strata),
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
  geom_step(data = surv_data_all, aes(x = time, y = cum.in, colour = "All"), size = 0.5, linetype = 2) +
  geom_step(size = 0.5) +
  #geom_ribbon(aes(ymin = lci, ymax = uci, fill = group), alpha=0.2, colour = "transparent") +
  #geom_ribbon(data = surv_data_all, aes(ymin = lci, ymax = uci), alpha=0.2, colour="transparent") +
  #scale_x_continuous(breaks = seq(0,175,25)) +
  scale_y_continuous(expand = expansion(mult=c(0,0.01))) +
  coord_cartesian(xlim=c(0, max(surv_data_groups$time))) +
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
  guides(col = guide_legend(order = 2)) +
  scale_color_manual(values = c("All" = "black",
                                "Care home (priority group 1)" = "#00BFC4",
                                "80+ (priority group 2)" = "#7CAE00",
                                "Health/care workers (priority groups 1-2)" = "#00A9FF",
                                "70-79 (priority groups 3-4)" = "#CD9600",
                                "Shielding (age 16-69) (priority group 4)" = "#FF61CC",
                                "50-69 (priority groups 5-9)" = "#F8766D",
                                "Others not in the above groups (under 50)" = "#C77CFF"))

surv_plot_ci <- surv_data_groups %>%
  ggplot(aes(x = time, y = cum.in, colour = group)) +
  geom_step(size = 0.5) +
  geom_ribbon(aes(ymin = lci, ymax = uci, fill = group), alpha=0.2, colour = "transparent") +
  geom_step(data = surv_data_all, aes(x = time, y = cum.in, colour = "All"), size = 0.5, linetype = 2) +
  geom_ribbon(data = surv_data_all, aes(ymin = lci, ymax = uci), alpha=0.2, colour="transparent") +
  scale_x_continuous(breaks = seq(0,250,25)) +
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


## Save plot
ggsave(
  here::here("output", "figures", "cumulative_incidence_positive_test.svg"),
  surv_plot,
  units = "cm", width = 30, height = 15
)

ggsave(
  here::here("output", "figures", "cumulative_incidence_positive_test_cis.svg"),
  surv_plot_ci,
  units = "cm", width = 30, height = 15
)
