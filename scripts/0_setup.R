# this script defines functions and settings
# for the HRV-LC project
# written by shelby bachman, sbachman@usc.edu


# functions to format demographic table data ------------------------------

# format mean & sd as `mean (SD)`
format_mean_sd <- function(mean, SD) {
  mean_round <- round(mean, digits = 2)
  SD_round <- round(SD, digits = 2)
  paste(mean_round, ' (', SD_round, ')', sep = '')
}

# format range as `min - max`
format_range <- function(min, max) {
  paste(min, '-', max, sep = '')
}

# format n & % as `n (%)`
format_n_pct <- function(n, frac) {
  pct_formatted = round(frac*100, digits = 1)
  paste(n, ' (', pct_formatted, '%)', sep = '')
}


# function to format ICC results ------------------------------------------

format_icc_results <- function(icc_result) {
  
  icc_val <- paste(round(icc_result$value, digits = 3),
                   ' (',
                   icc_result$df1,
                   ', ',
                   icc_result$df2,
                   ')',
                   sep = '')
  
  ci_95 <- paste(round(icc_result$lbound, digits = 3),
                 ', ',
                 round(icc_result$ubound, digits = 3),
                 sep = '')
  
  icc_df <- data.frame(icc_val, ci_95)
  
  return(icc_df)
}


# function to format icc results for text ---------------------------------

format_icc_results_text <- function(icc_result) {
  return(
    paste('ICC(',
        icc_result$df1, 
        ', ',
        icc_result$df2,
        ') = ',
        round(icc_result$value, digits = 3), 
        ', 95% CI = ',
        round(icc_result$lbound, digits = 3),
        ', ',
        round(icc_result$ubound, digits = 3),
        sep = '')
  )
}

# function to format bootstrap t-test results -----------------------------

format_boot_t_results <- function(boot_t_result) {
  t <- paste(round(boot_t_result$statistic, digits = 2),
             ' (',
             boot_t_result$parameter,
             ')',
             sep = '')
  
  p <- round(boot_t_result$p.value, digits = 3)
  
  p_boot <- round(boot_t_result$boot.p.value, digits = 3)
  
  est <- round(as.numeric(boot_t_result$estimate), digits = 3)
  
  ci_95 <- paste(round(boot_t_result$boot.conf.int[1], digits = 3),
                 ', ',
                 round(boot_t_result$boot.conf.int[2], digits = 3),
                 sep = '')
  
  t_df <- data.frame(t, est, ci_95, p, p_boot)
  
  return(t_df)
}


# function to format correlation analysis results -------------------------

format_corr_results <- function(corr_result) {
  r <- as.numeric(corr_result$estimate)
  df <- as.numeric(corr_result$parameter)
  p <- as.numeric(corr_result$p.value)
  ci <- paste(round(corr_result$conf.int[1], 3), ' - ', round(corr_result$conf.int[2], 3), sep = '')
  
  corr_df <- data.frame(r, ci, df, p)
  
  return(corr_df)
}


# function to format lme4 results as table --------------------------------

format_lme4_results <- function(lme4_params) {
  
  # select only relevant columns
  df <- lme4_params %>%
    dplyr::select(Parameter, Coefficient, SE, CI_low, CI_high, t, p)
  
  # set column names
  names(df) <- c('Predictor', 'Estimate', 'SE', 'CI_lower', 'CI_upper', 't', 'p')
  
  # create 95% CI column
  df <- df %>%
    rowwise() %>%
    mutate(`95% CI` = str_c(
      round(CI_lower, 3), ', ', round(CI_upper, 3), sep = ''
    ), .before = CI_lower) %>%
    select(-c(CI_lower, CI_upper))
  
  # fix row names
  rn <- df$Predictor
  for (i in 1:length(rn)) {
    if (rn[i] == "(Intercept)") {
      rn[i] <- str_remove_all(rn[i], "[()]")
    } else {
      rn[i] <- str_remove_all(rn[i], "[1]")
      
      if (str_detect(rn[i], 'condition')) {
        rn[i] <- str_replace_all(rn[i], pattern = 'condition', replacement = 'Condition')
      }
      if (str_detect(rn[i], 'timepoint')) {
        rn[i] <- str_replace_all(rn[i], pattern = 'timepoint', replacement = 'Timepoint')
      }
      if (str_detect(rn[i], 'label-session')) {
        rn[i] <- str_replace_all(rn[i], pattern = 'label-session', replacement = 'Timepoint')
      }
      if (str_detect(rn[i], 'label_session')) {
        rn[i] <- str_replace_all(rn[i], pattern = 'label_session', replacement = 'Timepoint')
      }
      if (str_detect(rn[i], 'age_group')) {
        rn[i] <- str_replace_all(rn[i], pattern = 'age_group', replacement = 'Age group')
      }
      if (str_detect(rn[i], 'hemi')) {
        rn[i] <- str_replace_all(rn[i], pattern = 'hemi', replacement = 'Hemisphere')
      }
      if (str_detect(rn[i], 'gender')) {
        rn[i] <- str_replace_all(rn[i], pattern = 'gender', replacement = 'Sex')
      }
      if (str_detect(rn[i], 'Res_logPower_AR_train')) {
        rn[i] <- str_replace_all(rn[i], pattern = 'Res_logPower_AR_train', replacement = 'Training power')
      }
      
      rn[i] <- str_replace_all(rn[i], 
                               pattern = "[:]", 
                               replacement = " x ")
      
    }
  }
  df$Predictor <- rn
  
  # fix p-value
  df$p <- fix_pval_table(df$p)

  return(df)
}


# function to format a column of p-values for a table ---------------------

fix_pval_table <- function(p, digits = 3) {
  new_p <- vector(mode = 'character', length = length(p))
  for (ii in 1:length(p)) {
    if (p[ii] < .001) {
      new_p[ii] <- '<.001' 
    } else {
      new_p[ii] <- as.character(printnum(p[ii], digits = digits))
    }
  }

  return(new_p)
}


# function for half-violin plot -------------------------------------------

source("https://raw.githubusercontent.com/datavizpyr/data/master/half_flat_violinplot.R")


# function to create crossbar ---------------------------------------------

stat_sum_df <- function(fun, geom="crossbar", ...) {
  stat_summary(fun.data = fun, colour = "black", 
               alpha = 0.6, geom = geom, width = 0.1, ...)
}



# function to compute mean & SD of time between sessions ------------------

compute_weeks_between_sessions <- function(df) {
  
  # (input data should contain these columns in long format: 
  # label_subject, label_session, date)
  df <- df %>%
    pivot_wider(names_from = label_session,
                names_prefix = 'date_',
                values_from = date) %>%
    na.omit() %>%
    rowwise() %>%
    mutate(diff =`date_ses-post` - `date_ses-pre`) %>%
    mutate(diff = as.numeric(diff) / 7)
  
  # store results as list
  res <- vector(mode = 'list', length = 2)
  res[[1]] <- mean(df$diff)
  res[[2]] <- sd(df$diff)
  return(res)
  
}


# function to identify & report number of outliers ------------------------

summary_outliers <- function(x) {
  # compute sample median & mad
  sample_median <- median(x, na.rm = TRUE)
  sample_mad <- mad(x, na.rm = TRUE)
  # identify outliers based on mad & median
  comp <- abs(x - sample_median) / (1.483 * sample_mad)
  out <- ifelse(comp > 2.24, 1, 0)
  # return number of outliers
  n_out <- sum(out == 1, na.rm = TRUE)
  return(n_out)
}


# function to identify & remove outliers ----------------------------------

madmedianrule <- function(x) {
  # compute sample median & mad
  sample_median <- median(x, na.rm = TRUE)
  sample_mad <- mad(x, na.rm = TRUE)
  # identify outliers based on mad & median
  comp <- abs(x - sample_median) / (1.483 * sample_mad)
  out <- ifelse(comp > 2.24, 1, 0)
  # return data with outliers removed
  x_rmout <- x
  x_rmout[out == 1] <- NA
  return(x_rmout)
}


# function to convert nifti to xyz dataframe ------------------------------

convert_nii_to_df <- function(nifti_obj) {
  data_nii <- as.data.frame(reshape2::melt(nifti_obj))
  names(data_nii) <- c('x', 'y', 'z', 'value')
  data_nii$value[data_nii$value == 'NaN'] <- 0
  data_nii <- data_nii %>%
    filter(value > 0)
  return(data_nii)
}


# figure settings ---------------------------------------------------------

# color palette
palette_condition <- c('Osc+' = '#f94144', 'Osc-' = '#577590')

# set font size larger
theme_font <- theme(axis.text.x = element_text(size = 12), 
                    axis.title.x = element_text(size = 12),
                    axis.text.y = element_text(size = 12), 
                    axis.title.y = element_text(size = 12), 
                    legend.text = element_text(size = 12), 
                    text = element_text(size = 12))

