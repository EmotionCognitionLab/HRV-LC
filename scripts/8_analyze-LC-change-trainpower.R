# this script visualizes & analyzes
# associations between change in LC contrast and training power
# for the HRV-LC project
# written by shelby bachman, sbachman@usc.edu


# join training power data to LC change values ----------------------------

LC_ratios_alt <- LC_ratios_alt %>%
  left_join(data_power, by = 'label_subject')

# make a copy of the data for plotting & analysis
LC_ratios_alt_power <- LC_ratios_alt


# figure: change in LC contrast vs. training power ------------------------

# set factor levels and names for plotting
LC_ratios_alt_power$hemi <- as.character(LC_ratios_alt_power$hemi)
LC_ratios_alt_power$hemi[LC_ratios_alt_power$hemi == 'left'] <- 'Left LC'
LC_ratios_alt_power$hemi[LC_ratios_alt_power$hemi == 'right'] <- 'Right LC'
LC_ratios_alt_power$hemi <- factor(LC_ratios_alt_power$hemi, levels = c('Left LC', 'Right LC'))
LC_ratios_alt_power$condition <- factor(LC_ratios_alt_power$condition, levels = c('Osc+', 'Osc-'))

# create figure
fig_LC_change_trainpower_YA <- ggplot(data = LC_ratios_alt_power %>% 
           filter(age_group == 'YA'), 
         aes(x = Res_logPower_AR_train, y = LC_ratio_maxref_chg)) +
    geom_point(aes(fill = condition),
               pch = 21,
               size = 2,
               colour = 'black') +
    geom_smooth(method = 'lm', colour = 'darkgray') +
    coord_cartesian(ylim = ylims_LCchange) +
    scale_colour_manual(values = palette_condition) +
    scale_fill_manual(values = palette_condition) +
    labs(x = 'Training oscillatory power',
         y = 'Change in LC contrast',
         subtitle = 'Younger adults') +
    facet_wrap(~hemi) +
    theme_apa() +
    theme(text = element_text(size = 14),
          axis.text.x = element_text(size = 10),
          axis.text.y = element_text(size = 10),
          plot.subtitle = element_text(hjust = 0.5))
  
fig_LC_change_trainpower_OA <- ggplot(data = LC_ratios_alt_power %>% 
           filter(age_group == 'OA'), 
         aes(x = Res_logPower_AR_train, y = LC_ratio_maxref_chg)) +
    geom_point(aes(fill = condition),
               pch = 21,
               size = 2,
               colour = 'black') +
    geom_smooth(method = 'lm', colour = 'darkgray') +
    coord_cartesian(ylim = ylims_LCchange) +
    scale_colour_manual(values = palette_condition) +
    scale_fill_manual(values = palette_condition) +
    labs(x = 'Training oscillatory power',
         y = '',
         subtitle = 'Older adults') +
    facet_wrap(~hemi) +
    theme_apa() +
    theme(text = element_text(size = 14),
          axis.text.x = element_text(size = 10),
          axis.text.y = element_text(size = 10),
          plot.subtitle = element_text(hjust = 0.5))


fig_LC_change_trainpower <- ggarrange(
  fig_LC_change_trainpower_YA, 
  fig_LC_change_trainpower_OA,
  common.legend = TRUE,
  legend = 'right'
)


# figure 2: LC contrast by timepoint & change vs. training power ----------

figure2 <- ggarrange(
  fig_LC_YA + labs(subtitle = '', x = ''),
  
  ggplot(data = LC_ratios_alt_power %>% 
           filter(age_group == 'YA'), 
         aes(x = Res_logPower_AR_train, y = LC_ratio_maxref_chg)) +
    geom_point(aes(fill = condition),
               pch = 21,
               size = 2,
               colour = 'black') +
    geom_smooth(method = 'lm', colour = 'darkgray') +
    coord_cartesian(ylim = ylims_LCchange) +
    scale_colour_manual(values = palette_condition) +
    scale_fill_manual(values = palette_condition) +
    labs(x = expression(paste("Log training oscillatory power (", ms^2, ")", sep = '')),
         y = 'Change in LC contrast',
         subtitle = '') +
    facet_wrap(~hemi) +
    guides(colour = FALSE, fill = FALSE) +
    theme_apa() +
    theme(text = element_text(size = 14, family = 'Arial'),
          axis.title.x = element_text(size = 12),
          axis.title.y = element_text(size = 12),
          axis.text.x = element_text(size = 10),
          axis.text.y = element_text(size = 10),
          strip.text.x = element_text(size = 12)),
  
  fig_LC_OA + labs(subtitle = '', x = ''),
  
  ggplot(data = LC_ratios_alt_power %>% 
           filter(age_group == 'OA'), 
         aes(x = Res_logPower_AR_train, y = LC_ratio_maxref_chg)) +
    geom_point(aes(fill = condition),
               pch = 21,
               size = 2,
               colour = 'black') +
    geom_smooth(method = 'lm', colour = 'darkgray') +
    coord_cartesian(ylim = ylims_LCchange) +
    scale_colour_manual(values = palette_condition) +
    scale_fill_manual(values = palette_condition) +
    labs(x = expression(paste("Log training oscillatory power (", ms^2, ")", sep = '')),
         y = 'Change in LC contrast',
         subtitle = '') +
    facet_wrap(~hemi) +
    guides(colour = FALSE, fill = FALSE) +
    theme_apa() +
    theme(text = element_text(size = 14, family = 'Arial'),
          axis.title.x = element_text(size = 12),
          axis.title.y = element_text(size = 12),
          axis.text.x = element_text(size = 10),
          axis.text.y = element_text(size = 10),
          strip.text.x = element_text(size = 12)),
  
  nrow = 2, ncol = 2,
  
  widths = c(1.5, 1),
  
  labels = c('A', 'B', '', ''),
  font.label = list(size = 16),
  common.legend = TRUE, legend = 'right'
  
)

ggsave(filename = here('figures', 'figure2_LC-change.svg'), 
       plot = figure2,
       device = 'svg',
       width = 12, height = 7, dpi = 400, bg = 'white')


# correlation analyses ----------------------------------------------------

# perform spearman correlation analyses
corr_YA_left <- cor.test(~LC_ratio_maxref_chg + Res_logPower_AR_train, 
                         data = LC_ratios_alt %>%
                           filter(age_group == 'YA',
                                  hemi == 'left'), 
                         method = 'pearson')

corr_YA_right <- cor.test(~LC_ratio_maxref_chg + Res_logPower_AR_train, 
                          data = LC_ratios_alt %>%
                            filter(age_group == 'YA',
                                   hemi == 'right'), 
                          method = 'pearson')

corr_OA_left <- cor.test(~LC_ratio_maxref_chg + Res_logPower_AR_train, 
                         data = LC_ratios_alt %>%
                           filter(age_group == 'OA',
                                  hemi == 'left'), 
                         method = 'pearson')

corr_OA_right <- cor.test(~LC_ratio_maxref_chg + Res_logPower_AR_train, 
                          data = LC_ratios_alt %>%
                            filter(age_group == 'OA',
                                   hemi == 'right'), 
                          method = 'pearson')

# format results as table
corr_chg_power <- as.data.frame(
  rbind(
    format_corr_results(corr_YA_left) %>%
      mutate(age_group = 'YA',
             hemi = 'Left LC'),
    
    format_corr_results(corr_YA_right) %>%
      mutate(age_group = 'YA',
             hemi = 'Right LC'),
    
    format_corr_results(corr_OA_left) %>%
      mutate(age_group = 'OA',
             hemi = 'Left LC'),
    
    format_corr_results(corr_OA_right) %>%
      mutate(age_group = 'OA',
             hemi = 'Right LC')
    
  )
) %>%
  select(age_group, hemi, r, df, ci, p)


# remove unneeded variables -----------------------------------------------

rm(corr_YA_left, corr_YA_right, corr_OA_left, corr_OA_right)


# model: change in LC contrast vs. training power -------------------------

### set factor levels for model
LC_ratios_alt$hemi <- factor(LC_ratios_alt$hemi, levels = c('right', 'left'))
contrasts(LC_ratios_alt$hemi) <- c(-0.5, 0.5) 
LC_ratios_alt$age_group <- factor(LC_ratios_alt$age_group, levels = c('YA', 'OA'))
contrasts(LC_ratios_alt$age_group) <- c(-0.5, 0.5) 

### fit model
mod.LC_change_trainpower <- lmer(LC_ratio_maxref_chg ~ Res_logPower_AR_train * hemi * age_group + (1 | label_subject),
                                 data = LC_ratios_alt)

params.LC_change_trainpower <- model_parameters(mod.LC_change_trainpower, 
                                   effects = 'fixed', 
                                   df_method = 'satterthwaite') %>%
  as.data.frame()

