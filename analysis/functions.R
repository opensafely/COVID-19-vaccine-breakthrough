# Import libraries ----
library('tidyverse')
library('lubridate')
#library('gt')

## Factorise
fct_case_when <- function(...) {
  # uses dplyr::case_when but converts the output to a factor,
  # with factors ordered as they appear in the case_when's  ... argument
  args <- as.list(match.call())
  levels <- sapply(args[-1], function(f) f[[3]])  # extract RHS of formula
  levels <- levels[!is.na(levels)]
  factor(dplyr::case_when(...), levels=levels)
}

## Rates
calculate_rates = function(group = "covid_positive_test",
                           follow_up = "time_to_positive_test",
                           data = data_cohort_over80,
                           Y = 1000,
                           dig = 2,
                           variables = c("ageband2",
                                         "sex",
                                         "bmi",
                                         "smoking_status")){
  
  rates <- list()
  
  for (i in 1:length(variables)) {
    
    rates.ind <- data %>%
      mutate(time_since_fully_vaccinated = cut(follow_up_time_vax2 - 14,
                                               breaks = c(0, 28, 56, 84, Inf),
                                               labels = c("0-4 weeks", "4-8 weeks", "8-12 weeks", "12+ weeks"),
                                               right = FALSE),
             
             time_between_vaccinations = cut(tbv,
                                             breaks = c(0, 42, 84, Inf),
                                             labels = c("6 weeks or less", "6-12 weeks", "12 weeks or more"),
                                             right = FALSE),
             
             smoking_status = ifelse(is.na(smoking_status), "N&M", smoking_status),
             asthma = ifelse(asthma == 1, "asthma", NA),
             asplenia = ifelse(asplenia == 1, "asplenia", NA),
             cancer = ifelse(cancer == 1, "cancer", NA),
             haem_cancer = ifelse(haem_cancer == 1, "haem_cancer", NA),
             chd = ifelse(chd == 1, "chd", NA),
             chronic_neuro_dis_inc_sig_learn_dis = ifelse(chronic_neuro_dis_inc_sig_learn_dis == 1, "chronic_neuro_dis_inc_sig_learn_dis", NA),
             chronic_resp_dis = ifelse(chronic_resp_dis == 1, "chronic_resp_dis", NA),
             #chronic_kidney_disease = ifelse(chronic_kidney_disease == 1, "chronic_kidney_disease", NA),
             #end_stage_renal = ifelse(end_stage_renal == 1, "end_stage_renal", NA),
             cld = ifelse(cld == 1, "cld", NA),
             diabetes = ifelse(diabetes == 1, "diabetes", NA),
             immunosuppression = ifelse(immunosuppression == 1, "immunosuppression", NA),
             learning_disability = ifelse(learning_disability == 1, "learning_disability", NA),
             #organ_transplant = ifelse(organ_transplant == 1, "organ_transplant", NA),
             sev_mental_ill = ifelse(sev_mental_ill == 1, "sev_mental_ill", NA)) %>%
      select(group = paste0(group),
             person_time = paste0(follow_up),
             variable = paste0(variables[i])) %>%
      filter(person_time > -1) %>%
      mutate(variable = as.character(variable),
             variable = ifelse(variable == "", "Unknown", variable),
             variable = ifelse(is.na(variable), "Unknown", variable)) %>%
      group_by(variable) %>%
      summarise(n_postest = sum(group==1, na.rm = T),
                n_postest = ifelse(is.na(n_postest), 0, n_postest),
                person_time = sum(person_time, na.rm = T),
                person_time = ifelse(is.na(person_time), 0, person_time)) %>%
      rowwise() %>%
      mutate(est = glm(n_postest ~ 1 + offset(log(person_time/(365.25*Y))), family = "poisson")$coefficients,
             se = coef(summary(glm(n_postest ~ 1 + offset(log(person_time/(365.25*Y))), family = "poisson")))[, "Std. Error"],
             Rate_py = round(exp(est), digits = 2),
             lower_py = round(exp(est - se),  digits = 2),
             upper_py = round(exp(est + se),  digits = 2),
             person_time = round(person_time/365.25,  digits = 0)) %>%
      select(variable, n_postest, person_time, Rate_py, lower_py, upper_py) %>%
      mutate(group = variables[i])
    
    rates <- rbind(rates, rates.ind)
    
  }
  rates %>% 
    distinct()
}

calculate_age_adjusted_rates = function(group = "covid_positive_test",
                           follow_up = "time_to_positive_test",
                           data = data_cohort_over80,
                           Y = 1000,
                           dig = 2,
                           variables = c("ageband2",
                                         "sex",
                                         "bmi",
                                         "smoking_status")){
  
  rates <- list()
  
  for (i in 1:length(variables)) {
    
    rates.ind <- data %>%
      mutate(time_since_fully_vaccinated = cut(follow_up_time_vax2 - 14,
                                               breaks = c(0, 28, 56, 84, Inf),
                                               labels = c("0-4 weeks", "4-8 weeks", "8-12 weeks", "12+ weeks"),
                                               right = FALSE),
             
             time_between_vaccinations = cut(tbv,
                                             breaks = c(0, 42, 84, Inf),
                                             labels = c("6 weeks or less", "6-12 weeks", "12 weeks or more"),
                                             right = FALSE),
             
             smoking_status = ifelse(is.na(smoking_status), "N&M", smoking_status),
             asthma = ifelse(asthma == 1, "asthma", NA),
             asplenia = ifelse(asplenia == 1, "asplenia", NA),
             cancer = ifelse(cancer == 1, "cancer", NA),
             haem_cancer = ifelse(haem_cancer == 1, "haem_cancer", NA),
             chd = ifelse(chd == 1, "chd", NA),
             chronic_neuro_dis_inc_sig_learn_dis = ifelse(chronic_neuro_dis_inc_sig_learn_dis == 1, "chronic_neuro_dis_inc_sig_learn_dis", NA),
             chronic_resp_dis = ifelse(chronic_resp_dis == 1, "chronic_resp_dis", NA),
             #chronic_kidney_disease = ifelse(chronic_kidney_disease == 1, "chronic_kidney_disease", NA),
             #end_stage_renal = ifelse(end_stage_renal == 1, "end_stage_renal", NA),
             cld = ifelse(cld == 1, "cld", NA),
             diabetes = ifelse(diabetes == 1, "diabetes", NA),
             immunosuppression = ifelse(immunosuppression == 1, "immunosuppression", NA),
             learning_disability = ifelse(learning_disability == 1, "learning_disability", NA),
             #organ_transplant = ifelse(organ_transplant == 1, "organ_transplant", NA),
             sev_mental_ill = ifelse(sev_mental_ill == 1, "sev_mental_ill", NA)) %>%
      select(group = paste0(group),
             person_time = paste0(follow_up),
             variable = paste0(variables[i])) %>%
      filter(person_time > -1) %>%
      mutate(variable = as.character(variable),
             variable = ifelse(variable == "", "Unknown", variable),
             variable = ifelse(is.na(variable), "Unknown", variable)) %>%
      group_by(variable) %>%
      summarise(n_postest = sum(group==1, na.rm = T),
                n_postest = ifelse(is.na(n_postest), 0, n_postest),
                person_time = sum(person_time, na.rm = T),
                person_time = ifelse(is.na(person_time), 0, person_time)) %>%
      rowwise() %>%
      mutate(est = glm(n_postest ~ 1 + offset(log(person_time/(365.25*Y))), family = "poisson")$coefficients,
             se = coef(summary(glm(n_postest ~ 1 + offset(log(person_time/(365.25*Y))), family = "poisson")))[, "Std. Error"],
             Rate_py = round(exp(est), digits = 2),
             lower_py = round(exp(est - se),  digits = 2),
             upper_py = round(exp(est + se),  digits = 2),
             person_time = round(person_time/365.25,  digits = 0)) %>%
      select(variable, n_postest, person_time, Rate_py, lower_py, upper_py) %>%
      mutate(group = variables[i])
    
    rates.adj <- data %>%
      mutate(time_since_fully_vaccinated = cut(follow_up_time_vax2 - 14,
                                               breaks = c(0, 28, 56, 84, Inf),
                                               labels = c("0-4 weeks", "4-8 weeks", "8-12 weeks", "12+ weeks"),
                                               right = FALSE),
             
             time_between_vaccinations = cut(tbv,
                                             breaks = c(0, 42, 84, Inf),
                                             labels = c("6 weeks or less", "6-12 weeks", "12 weeks or more"),
                                             right = FALSE),
             
             smoking_status = ifelse(is.na(smoking_status), "N&M", smoking_status),
             asthma = ifelse(asthma == 1, "asthma", NA),
             asplenia = ifelse(asplenia == 1, "asplenia", NA),
             cancer = ifelse(cancer == 1, "cancer", NA),
             haem_cancer = ifelse(haem_cancer == 1, "haem_cancer", NA),
             chd = ifelse(chd == 1, "chd", NA),
             chronic_neuro_dis_inc_sig_learn_dis = ifelse(chronic_neuro_dis_inc_sig_learn_dis == 1, "chronic_neuro_dis_inc_sig_learn_dis", NA),
             chronic_resp_dis = ifelse(chronic_resp_dis == 1, "chronic_resp_dis", NA),
             #chronic_kidney_disease = ifelse(chronic_kidney_disease == 1, "chronic_kidney_disease", NA),
             #end_stage_renal = ifelse(end_stage_renal == 1, "end_stage_renal", NA),
             cld = ifelse(cld == 1, "cld", NA),
             diabetes = ifelse(diabetes == 1, "diabetes", NA),
             immunosuppression = ifelse(immunosuppression == 1, "immunosuppression", NA),
             learning_disability = ifelse(learning_disability == 1, "learning_disability", NA),
             #organ_transplant = ifelse(organ_transplant == 1, "organ_transplant", NA),
             sev_mental_ill = ifelse(sev_mental_ill == 1, "sev_mental_ill", NA)) %>%
      select(group = paste0(group),
             age,
             person_time = paste0(follow_up),
             variable = paste0(variables[i])) %>%
      filter(person_time > -1) %>%
      mutate(variable = as.character(variable),
             variable = ifelse(variable == "", "Unknown", variable),
             variable = ifelse(is.na(variable), "Unknown", variable)) %>%
      group_by(variable, age) %>%
      summarise(n_postest = sum(group==1, na.rm = T),
                n_postest = ifelse(is.na(n_postest), 0, n_postest),
                person_time = sum(person_time, na.rm = T),
                person_time = ifelse(is.na(person_time), 0, person_time))
    
    rates.adj.mod <- glm(n_postest ~ 1 + variable + age + offset(log(person_time/(365.25*Y))), 
                         family = "poisson", data = rates.adj)
    
    rates.adj.est <- data.frame(Variable = row.names(data.frame(coef(summary(rates.adj.mod)))),
                                coefficients = data.frame(coef(summary(rates.adj.mod)))$Estimate,
                                row.names = NULL,
                                est = NA,
                                se = coef(summary(rates.adj.mod))[, "Std. Error"]) %>%
      filter(Variable != "age")
    
    levels <- unique(rates.adj$variable)
    
    for (j in 2:length(levels)) {
      rates.adj.est[j,]$Variable <- levels[j]
      rates.adj.est[j,]$est <- subset(rates.adj.est, Variable == "(Intercept)")$coefficients + rates.adj.est[j,]$coefficients 
    }
    
    rates.adj.est <- rates.adj.est %>%
      mutate(est = ifelse(Variable == "(Intercept)", coefficients, est),
             variable = ifelse(Variable == "(Intercept)", unique(rates.adj$variable)[1], Variable))

    rates.adj <- rates.adj %>%
      group_by(variable) %>%
      summarise(n_postest = sum(n_postest, na.rm = T),
                person_time = sum(person_time, na.rm = T)) %>%
      left_join(rates.adj.est, by = "variable") %>%
      mutate(Rate_adj = round(exp(est), digits = 2),
             lower_adj = round(exp(est - se),  digits = 2),
             upper_adj = round(exp(est + se),  digits = 2),
             person_time = round(person_time/365.25,  digits = 0)) %>%
      select(variable, n_postest, person_time, Rate_adj, lower_adj, upper_adj) %>%
      mutate(group = variables[i])
    
    rates.all <- left_join(rates.ind, rates.adj)
    
    rates <- rbind(rates, rates.all)
    
  }
  
  print(rates.adj.mod)

  rates %>% 
    distinct()
  
}


## Redaction
redactor <- function(n, threshold){
  
  # given a vector of frequencies, this returns a boolean vector that is TRUE if
  # a) the frequency is <= the redaction threshold and
  # b) if the sum of redacted frequencies in a) is still <= the threshold, then the
  # next largest frequency is also redacted
  
  n <- as.integer(n)
  leq_threshold <- dplyr::between(n, 1, threshold)
  n_sum <- sum(n)
  
  # redact if n is less than or equal to redaction threshold
  redact <- leq_threshold
  
  # also redact next smallest n if sum of redacted n is still less than or equal to threshold
  if((sum(n*leq_threshold) <= threshold) & any(leq_threshold)){
    redact[which.min(dplyr::if_else(leq_threshold, n_sum+1L, n))] = TRUE
  }
  
  redact
}


redactor2 <- function(n, threshold=5, x=NULL){
  
  # given a vector of frequencies (n), this returns a redacted vector (if x is NULL) or
  # reaction of a secondary vector based on frequencies in the first (if x is not nULL).
  # using the following rules:
  # a) the frequency is <= the redaction threshold and
  # b) if the sum of redacted frequencies in a) is still <= the threshold, then the
  # next largest frequency is also redacted
  
  
  stopifnot("n must be non-missing" = any(!is.na(n)))
  stopifnot("n must non-negative" = any(n>=0))
  stopifnot("n must non-negative" = any(n>=0))
  
  if(is.null(x)){
    x <- n
  }
  
  if(!is.null(x)){
    stopifnot("x must be same length as n" = length(n) == length(x))
  }
  
  
  
  n <- as.integer(n)
  leq_threshold <- dplyr::between(n, 1, threshold)
  n_sum <- sum(n)
  
  # redact if n is less than or equal to redaction threshold
  redact <- leq_threshold
  
  # also redact next smallest n if sum of redacted n is still less than or equal to threshold
  if((sum(n*leq_threshold) <= threshold) & any(leq_threshold)){
    redact[which.min(dplyr::if_else(leq_threshold, n_sum+1L, n))] = TRUE
  }
  
  
  typedNA <- NA
  mode(typedNA) <- typeof(x)
  
  redacted <- dplyr::if_else(redact, typedNA, x)
  
  redacted
}


## summary table functions (outputs a data.frame) ----

testdata <- tibble(
  a = sample(c("a","b","c"), size=1000, replace=TRUE),
  b = sample(c("x","y","z"), size=1000, replace=TRUE),
  c= rnorm(1000),
  d= rnorm(1000),
  date = as.Date(runif(1000,0,99999), origin="1970-01-01")
) %>%
  mutate(across(
    .cols = -date,
    ~{
      type <- typeof(.x)
      typedNA <- NA
      mode(typedNA) <- type
      ifelse(runif(n())>0.1, ., typedNA)
    }
  )) %>%
  add_row(
    a=rep("d",5),
    b=rep("w",5),
    c=0,
    d=0,
    date = as.Date(NA, origin="1970-01-01")
  )

### categorical data ----

redacted_summary_cat <- function(
  variable,
  .missing_name = "(missing)",
  .redacted_name="redacted",
  redaction_threshold=5,
  redaction_accuracy=1L
){
  
  
  stopifnot("redaction_accuracy must be a strictly-positive integer" = ((redaction_accuracy>=1) | (redaction_accuracy %% 1)==0))
  
  if (is.logical(variable)){
    variable <- if_else(variable, "yes", "no")
  }
  
  dat_freq <- tibble(
    .level = (fct_explicit_na(variable, na_level=.missing_name)),
  ) %>%
    group_by(.level, .drop=FALSE) %>%
    tally() %>%
    mutate(
      n = as.integer(round(n/redaction_accuracy)*redaction_accuracy),
      pct=n/sum(n),
      n_nonmiss=if_else(.level==.missing_name, 0L, n),
      pct_nonmiss = (n_nonmiss/sum(n_nonmiss, na.rm=TRUE)),
    ) %>% select(-n_nonmiss)
  
  dat_freq[[.redacted_name]] <- redactor(dat_freq$n, redaction_threshold)
  
  dat_redacted <- dat_freq %>%
    mutate(across(
      .cols = -all_of(c(".level", .redacted_name)),
      ~{
        if_else(dat_freq[[.redacted_name]], .x+NA, .x) # .x+NA rather than NA to ensure correct type
      }
    ))
  
  dat_redacted
}

#test_cat <- redacted_summary_cat(testdata$a)



### categorical * categorical data ----

redacted_summary_catcat <- function(
  variable1,
  variable2,
  .missing_name = "(missing)",
  .redacted_name="redacted",
  redaction_threshold=5,
  redaction_accuracy=1L,
  .total_name=NULL
){
  
  
  stopifnot("redaction_accuracy must be a strictly-positive integer" = ((redaction_accuracy>=1) | (redaction_accuracy %% 1)==0))
  
  if (is.logical(variable1)){
    variable1 <- if_else(variable1, "yes", "no")
  }
  
  if (is.logical(variable2)){
    variable2 <- if_else(variable2, "yes", "no")
  }
  
  dat_freq <- tibble(
    .level1 = (fct_explicit_na(variable1, na_level=.missing_name)),
    .level2 = (fct_explicit_na(variable2, na_level=.missing_name)),
  ) %>%
    group_by(.level2, .level1, .drop=FALSE) %>%
    tally() %>%
    mutate(
      pct = n/sum(n),
      n = as.integer(round(n/redaction_accuracy)*redaction_accuracy),
      n_nonmiss = if_else(.level1==.missing_name, 0L, n),
      pct_nonmiss = (n_nonmiss/sum(n_nonmiss, na.rm=TRUE)),
    ) %>%
    select(-n_nonmiss)
  
  
  
  dat_freq_redact0 <- dat_freq %>%
    group_by(.level1) %>%
    mutate(
      .redacted_name1 = redactor(n, redaction_threshold),
    ) %>%
    group_by(.level2) %>%
    mutate(
      .redacted_name2 = redactor(n, redaction_threshold)
    ) %>%
    ungroup() %>%
    mutate(
      {{.redacted_name}} := .redacted_name1 | .redacted_name2
    ) %>%
    select(-.redacted_name1, -.redacted_name2)
  
  print(dat_freq_redact0)
  
  dat_redacted <- dat_freq_redact0 %>%
    mutate(across(
      .cols = -all_of(c(".level1", ".level2", .redacted_name)),
      ~{
        if_else(dat_freq_redact0[[.redacted_name]], .x+NA, .x) # .x+NA rather than NA to ensure correct type
      }
    ))
  
  
  if(!is.null(.total_name)){
    dat_freq_total_redacted <- redacted_summary_cat(
      variable1,
      .missing_name = .missing_name,
      .redacted_name=.redacted_name,
      redaction_threshold=redaction_threshold
    ) %>%
      rename(.level1=.level) %>%
      mutate(
        .level2 = factor(.total_name)
      )
    dat_redacted <- bind_rows(dat_redacted, dat_freq_total_redacted)
  }
  
  dat_redacted %>%
    select(.level1, .level2, everything()) %>%
    arrange(.level2)
  
}

#test_catcat <- redacted_summary_catcat(testdata$a, testdata$b, .total_name="Total")





### continuous data ----

redacted_summary_num <- function(variable, .redacted_name="redacted", redaction_threshold=5){
  
  # TODO add custom_function argument that takes a list of formulas and appends to `summary_fns`.
  
  
  stats_wide <- as_tibble_col(
    variable, column_name="variable"
  ) %>%
    summarise(
      n = length(variable),
      n_nonmiss = sum(!is.na(variable)),
      pct_nonmiss = sum(!is.na(variable))/length(variable),
      n_miss = sum(is.na(variable)),
      pct_miss = sum(is.na(variable))/length(variable),
      
      unique = n_distinct(variable, na.rm=TRUE),
      
      mean = mean(variable, na.rm=TRUE),
      sd = sd(variable, na.rm=TRUE),
      
      min = min(variable, na.rm=TRUE),
      p10 = quantile(variable, p=0.1, na.rm=TRUE, type=1),
      p25 = quantile(variable, p=0.25, na.rm=TRUE, type=1),
      p50 = quantile(variable, p=0.5, na.rm=TRUE, type=1),
      p75 = quantile(variable, p=0.75, na.rm=TRUE, type=1),
      p90 = quantile(variable, p=0.9, na.rm=TRUE, type=1),
      max = max(variable, na.rm=TRUE)
      
      
    )
  
  stats_wide[[.redacted_name]] <- redactor(stats_wide$n, redaction_threshold)
  dat_redacted <- stats_wide %>%
    mutate(across(
      .cols = -all_of(c(.redacted_name)),
      ~{
        if_else(stats_wide[[.redacted_name]], .x+NA, .x) # .x+NA rather than NA to ensure correct type
      }
    ))
  
  dat_redacted
}

#test_num <- redacted_summary_num(testdata$c)



redacted_summary_date <- function(variable, .redacted_name="redacted", redaction_threshold=5){
  
  # TODO add custom_function argument that takes a list of formulas and appends to `summary_fns`.
  
  stopifnot("input vector is not a date" = is.Date(variable))
  
  stats_wide <- as_tibble_col(
    variable, column_name="variable"
  ) %>%
    summarise(
      n = length(variable),
      n_nonmiss = sum(!is.na(variable)),
      pct_nonmiss = sum(!is.na(variable))/length(variable),
      n_miss = sum(is.na(variable)),
      pct_miss = sum(is.na(variable))/length(variable),
      
      unique = n_distinct(variable, na.rm=TRUE),
      
      mean = mean(variable, na.rm=TRUE),
      sd = sd(variable, na.rm=TRUE),
      
      min = min(variable, na.rm=TRUE),
      p10 = quantile(variable, p=0.1, na.rm=TRUE, type=1),
      p25 = quantile(variable, p=0.25, na.rm=TRUE, type=1),
      p50 = quantile(variable, p=0.5, na.rm=TRUE, type=1),
      p75 = quantile(variable, p=0.75, na.rm=TRUE, type=1),
      p90 = quantile(variable, p=0.9, na.rm=TRUE, type=1),
      max = max(variable, na.rm=TRUE)
      
    )
  
  stats_wide[[.redacted_name]] <- redactor(stats_wide$n, redaction_threshold)
  dat_redacted <- stats_wide %>%
    mutate(across(
      .cols = -all_of(c(.redacted_name)),
      ~{
        if_else(stats_wide[[.redacted_name]], .x+NA, .x) # .x+NA rather than NA to ensure correct type
      }
    ))
  
  
  # this step is in case date attribute is lost (eg when only NAs are returned).
  dat_redacted_date <- dat_redacted %>%
    mutate(across(
      .cols = all_of(c("mean","min", "p10", "p25", "p50", "p75", "p90", "max")),
      ~as.Date(.x, "1970-01-01")
    ))
  
  dat_redacted
}

#test_date<- redacted_summary_date(as.Date(c(NA,NA,NA), origin="1970-01-01"))




### categorical * numeric data ----

redacted_summary_catnum <- function(variable_cat, variable_num, .missing_name = "(missing)", .redacted_name="redacted", redaction_threshold=5){
  
  stats_wide <- tibble(
    .variable_cat = (fct_explicit_na(variable_cat, na_level=.missing_name)),
    .variable_num = variable_num
  ) %>%
    group_by(.variable_cat) %>%
    summarise(.groups="keep",
              n = length(.variable_num),
              n_nonmiss = sum(!is.na(.variable_num)),
              pct_nonmiss = sum(!is.na(.variable_num))/length(.variable_num),
              n_miss = sum(is.na(.variable_num)),
              pct_miss = sum(is.na(.variable_num))/length(.variable_num),
              
              mean = mean(.variable_num, na.rm=TRUE),
              sd = sd(.variable_num, na.rm=TRUE),
              
              min = min(.variable_num, na.rm=TRUE),
              p10 = quantile(.variable_num, p=0.1, na.rm=TRUE, type=1),
              p25 = quantile(.variable_num, p=0.25, na.rm=TRUE, type=1),
              p50 = quantile(.variable_num, p=0.5, na.rm=TRUE, type=1),
              p75 = quantile(.variable_num, p=0.75, na.rm=TRUE, type=1),
              p90 = quantile(.variable_num, p=0.9, na.rm=TRUE, type=1),
              max = max(.variable_num, na.rm=TRUE),
              unique = n_distinct(.variable_num, na.rm=TRUE)
    )
  
  stats_wide[[.redacted_name]] <- redactor(stats_wide$n, redaction_threshold)
  
  dat_redacted <- stats_wide %>%
    ungroup() %>%
    mutate(across(
      .cols = -all_of(c(".variable_cat", .redacted_name)),
      ~{
        if_else(stats_wide[[.redacted_name]], .x+NA, .x) # .x+NA rather than NA to ensure correct type
      }
    ))
  
  dat_redacted
}

#test_catnum <- redacted_summary_catnum(testdata$a, testdata$c)




## ASCII printing of tables (in markdown format) ----

### categorical ----

options(knitr.kable.NA = '-')

print_cat <- function(x, name, ...) {
  cat("-------------------------------------------------------------------------\n")
  cat("-------------------------------------------------------------------------\n")
  cat(name)
  print(
    knitr::kable(
      x,
      col.names= c("-               -", "        n", "% (of all)", "% (of non-missing)", "redacted"),
      format="pipe",
      digits=3
    )
  )
  cat("\n\n\n")
}
#print_cat(test_cat, "sex")

### categorical * categorical ----


print_catcat <- function(x, name1, name2, ...) {
  
  summary_wide <- x %>%
    arrange(.level2) %>%
    pivot_wider(
      id_cols=c(.level1),
      values_from=c(n, pct),
      names_from=.level2,
      names_glue="{.level2}__{.value}"
    )
  
  col_selector <- levels(x$.level2)
  old_names <- summary_wide %>% select(-.level1) %>% names()
  
  col_renamer <- old_names %>%
    set_names(
      . %>%
        #str_replace("__n", str_c("__","N")) %>%
        str_replace("__pct", str_c("__","%")) %>%
        str_replace("__", " ")
      
    )
  
  x_wide <- summary_wide %>%
    select(.level1, starts_with(paste0(col_selector, "__"))) %>%
    rename(!!!col_renamer)
  
  col_names_new <- names(x_wide)
  
  cat("-------------------------------------------------------------------------\n")
  cat("-------------------------------------------------------------------------\n")
  cat(name1, "*", name2)
  print(
    knitr::kable(
      x_wide,
      col.names= c("-              -", names(x_wide)[-1]),
      format="pipe",
      digits=3
    )
  )
  cat("\n\n\n")
}

#print_catcat(test_catcat, "sex", "var2")

### numeric ----

print_num <- function(x, name, ...) {
  cat("--------------------------------------------------------------------------\n")
  cat("--------------------------------------------------------------------------\n")
  cat(name)
  print(
    knitr::kable(
      x %>% select(n, n_nonmiss, pct_nonmiss, n_miss, pct_miss, unique),
      col.names= c(
        "        n",
        "n non-missing", "% non-missing",
        "n missing", "% missing",
        "n unique"
      ),
      format="pipe",
      digits=3
    )
  )
  
  cat("\n")
  
  print(
    knitr::kable(
      x %>% select(mean, sd, min, p10, p25, p50, p75, p90, max),
      col.names= c(
        "mean", "sd",
        "min", "p10", "p25", "p50", "p75", "p90", "max"
      ),
      format="pipe",
      digits=3
    )
  )
  cat("\n\n\n")
}

#print_num(test_num, "sex")