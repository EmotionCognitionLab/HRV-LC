# this script visualizes & analyzes
# potential sex differences change in LC contrast
# for the HRV-LC project
# written by shelby bachman, sbachman@usc.edu


# bind sex data to LC change values ---------------------------------------

LC_ratios_alt <- LC_ratios_alt %>%
  left_join(data_demo_affect_cog %>% 
              select(label_subject, gender),
            by = 'label_subject')


# set y-axis limits for LC contrast figures -------------------------------

# for figures with pre- and post-training LC contrast
#range(c(LC_ratios_alt$LC_ratio_maxref_pre[LC_ratios_alt$age_group == 'YA'], LC_ratios_alt$LC_ratio_maxref_post[LC_ratios_alt$age_group == 'YA']))
ylims_LC_sex <- c(-0.15, 0.24)

# for figures with change in LC contrast
# range(LC_ratios_alt$LC_ratio_maxref_chg[LC_ratios_alt$age_group == 'YA'])
ylims_LCchange_sex <- c(-0.20, 0.18)


# figure: LC contrast change, by sex --------------------------------------

# set factor levels for plotting
LC_ratios_alt$hemi <- as.character(LC_ratios_alt$hemi)
LC_ratios_alt_fig <- LC_ratios_alt  # copy of data for plotting
LC_ratios_alt_fig$hemi[LC_ratios_alt_fig$hemi == 'left'] <- 'Left LC'
LC_ratios_alt_fig$hemi[LC_ratios_alt_fig$hemi == 'right'] <- 'Right LC'

LC_ratios_alt_fig$gender[LC_ratios_alt_fig$gender == 'female'] <- 'Female'
LC_ratios_alt_fig$gender[LC_ratios_alt_fig$gender == 'male'] <- 'Male'

LC_ratios_alt_fig$condition <- factor(LC_ratios_alt_fig$condition, levels = c('Osc+', 'Osc-'))
LC_ratios_alt_fig$hemi <- factor(LC_ratios_alt_fig$hemi, levels = c('Left LC', 'Right LC'))

# create figure
fig_LC_change_sex <- ggplot(data = LC_ratios_alt_fig %>%
                              filter(age_group == 'YA'),
                            aes(x = gender, y = LC_ratio_maxref_chg, 
                                colour = condition, fill = condition)) +
  geom_hline(yintercept = 0, colour = 'darkgray') +
  geom_boxplot(outlier.shape = NA, width = 0.5,
               colour = 'black') +
  geom_point(alpha = 1, 
             position = position_jitterdodge(jitter.width = 0.1, dodge.width = 0.5),
             pch = 21,
             colour = 'white') +
  scale_colour_manual(values = palette_condition) +
  scale_fill_manual(values = palette_condition) +
  labs(x = '', y = 'Change in LC contrast') +
  facet_wrap(~hemi) +
  theme_pubr()


# figure: LC contrast change vs. training power, by sex -------------------

fig_LC_change_trainpower_f <- 
  ggplot(data = LC_ratios_alt_fig %>% 
           filter(age_group == 'YA', 
                  gender == 'Female'), 
         aes(x = Res_logPower_AR_train, y = LC_ratio_maxref_chg)) +
  geom_point(aes(fill = condition),
             pch = 21,
             size = 2,
             colour = 'black') +
  geom_smooth(method = 'lm', colour = 'darkgray') +
  coord_cartesian(ylim = ylims_LCchange_sex) +
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
        plot.subtitle = element_text(hjust = 0.5),
        strip.text.x = element_text(size = 12))

fig_LC_change_trainpower_m <- 
  ggplot(data = LC_ratios_alt_fig %>% 
           filter(age_group == 'YA', 
                  gender == 'Male'), 
         aes(x = Res_logPower_AR_train, y = LC_ratio_maxref_chg)) +
  geom_point(aes(fill = condition),
             pch = 21,
             size = 2,
             colour = 'black') +
  geom_smooth(method = 'lm', colour = 'darkgray') +
  coord_cartesian(ylim = ylims_LCchange_sex) +
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
        plot.subtitle = element_text(hjust = 0.5),
        strip.text.x = element_text(size = 12))


# figure: LC contrast by timepoint and sex --------------------------------

LC_ratios_alt_fig <- LC_ratios_alt_fig %>%
  select(label_subject, hemi, age_group, condition, gender,
         LC_ratio_maxref_pre = LC_ratio_maxref_pre, LC_ratio_maxref_post = LC_ratio_maxref_post) %>%
  pivot_longer(cols = LC_ratio_maxref_pre:LC_ratio_maxref_post, 
               names_to = 'timepoint', 
               names_prefix = 'LC_ratio_maxref_', 
               values_to = 'LC_ratio')

LC_ratios_alt_fig$timepoint <- factor(LC_ratios_alt_fig$timepoint, 
                                      levels = c('pre', 'post'))

LC_ratios_alt_fig$hemi <- factor(LC_ratios_alt_fig$hemi,
                                 levels = c('Left LC', 'Right LC'))

LC_ratios_alt_fig$gender <- factor(LC_ratios_alt_fig$gender,
                                   levels = c('Female', 'Male'))

fig_LC_change_v2_f <- ggplot(data = LC_ratios_alt_fig %>% 
                                filter(age_group == 'YA',
                                       gender == 'Female'),
                              aes(x = timepoint, y = LC_ratio, 
                                  fill = condition, colour = condition)) +
  geom_flat_violin(scale = 'width', trim = TRUE,
                   alpha = 0.4,
                   width = 0.7,
                   position = position_nudge(x = 0.1)) +
  geom_line(aes(group = label_subject),
            alpha = 0.3) +
  geom_line(data = LC_ratios_alt_fig %>%
              filter(age_group == 'YA',
                     gender == 'Female') %>%
              group_by(hemi, condition, timepoint) %>%
              summarize(mean_LC_ratio = mean(LC_ratio, na.rm = TRUE)) %>%
              select(hemi, condition, timepoint, LC_ratio = mean_LC_ratio),
            aes(x = timepoint, y = LC_ratio,
                colour = condition, group = condition),
            position = position_nudge(x = 0.2),
            lwd = 1.5) +
  geom_point(pch = 21,
             size = 2,
             colour = 'black') + 
  stat_sum_df('mean_cl_boot',
              position = position_nudge(x = 0.20)) +
  scale_fill_manual(values = palette_condition) +
  scale_colour_manual(values = palette_condition) +
  coord_cartesian(ylim = ylims_LC_sex) +
  labs(x = '', y = 'LC contrast',
       fill = '', colour = '',
       subtitle = '') +
  facet_wrap(~hemi*condition, ncol = 4, nrow = 1, strip.position = 'bottom') +
  theme_apa() +
  theme(text = element_text(size = 14, family = 'Arial'),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        strip.text.x = element_blank(),
        plot.subtitle = element_text(hjust = 0),
        panel.spacing.x = unit(0, 'lines'))

fig_LC_change_v2_m <- ggplot(data = LC_ratios_alt_fig %>% 
                               filter(age_group == 'YA',
                                      gender == 'Male'),
                             aes(x = timepoint, y = LC_ratio, 
                                 fill = condition, colour = condition)) +
  geom_flat_violin(scale = 'width', trim = TRUE,
                   alpha = 0.4,
                   width = 0.7,
                   position = position_nudge(x = 0.1)) +
  geom_line(aes(group = label_subject),
            alpha = 0.3) +
  geom_line(data = LC_ratios_alt_fig %>%
              filter(age_group == 'YA',
                     gender == 'Male') %>%
              group_by(hemi, condition, timepoint) %>%
              summarize(mean_LC_ratio = mean(LC_ratio, na.rm = TRUE)) %>%
              select(hemi, condition, timepoint, LC_ratio = mean_LC_ratio),
            aes(x = timepoint, y = LC_ratio,
                colour = condition, group = condition),
            position = position_nudge(x = 0.2),
            lwd = 1.5) +
  geom_point(pch = 21,
             size = 2,
             colour = 'black') + 
  stat_sum_df('mean_cl_boot',
              position = position_nudge(x = 0.20)) +
  scale_fill_manual(values = palette_condition) +
  scale_colour_manual(values = palette_condition) +
  coord_cartesian(ylim = ylims_LC_sex) +
  labs(x = '', y = 'LC contrast',
       fill = '', colour = '',
       subtitle = '') +
  facet_wrap(~hemi*condition, ncol = 4, nrow = 1, strip.position = 'bottom') +
  theme_apa() +
  theme(text = element_text(size = 14, family = 'Arial'),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        strip.text.x = element_blank(),
        plot.subtitle = element_text(hjust = 0),
        panel.spacing.x = unit(0, 'lines'))


# figure 3: sex differences in LC contrast change -------------------------

figure3 <- ggarrange(
  
  fig_LC_change_v2_f,
  
  fig_LC_change_trainpower_f,
  
  fig_LC_change_v2_m,
  
  fig_LC_change_trainpower_m,
  
  nrow = 2, ncol = 2,
  
  widths = c(1.5, 1),
  
  labels = c('A', 'B', '', ''),
  font.label = list(size = 16),
  common.legend = TRUE, legend = 'right'
  
  
)

ggsave(filename = here('figures', 'figure3_LC-change-sex.svg'), 
       plot = figure3,
       device = 'svg',
       width = 12, height = 7, dpi = 400, bg = 'white')


# mixed effects model: LC contrast by condition, time, hemi & sex ---------

### convert data to long for model
LC_ratios_alt_long <- LC_ratios_alt %>%
  select(label_subject, age_group, condition, hemi, gender, 
         LC_ratio_maxref_pre = LC_ratio_maxref_pre, LC_ratio_maxref_post = LC_ratio_maxref_post) %>%
  pivot_longer(cols = LC_ratio_maxref_pre:LC_ratio_maxref_post, 
               names_to = 'timepoint', 
               names_prefix = 'LC_ratio_maxref_', 
               values_to = 'LC_ratio')

### set factor levels
LC_ratios_alt_long$hemi <- factor(LC_ratios_alt_long$hemi, levels = c('right', 'left'))
contrasts(LC_ratios_alt_long$hemi) <- c(-0.5, 0.5)
LC_ratios_alt_long$label_subject <- as.factor(LC_ratios_alt_long$label_subject)
LC_ratios_alt_long$condition <- factor(LC_ratios_alt_long$condition, levels = c('Osc-', 'Osc+'))
contrasts(LC_ratios_alt_long$condition) <- c(-0.5, 0.5)
LC_ratios_alt_long$timepoint <- factor(LC_ratios_alt_long$timepoint, levels = c('pre', 'post'))
contrasts(LC_ratios_alt_long$timepoint) <- c(-0.5, 0.5)
LC_ratios_alt_long$gender <- factor(LC_ratios_alt_long$gender, levels = c('male', 'female'))
contrasts(LC_ratios_alt_long$gender) <- c(-0.5, 0.5)

### fit model
mod.LC_time_sex <- lmer(LC_ratio ~ timepoint * condition * hemi * gender + (1 | label_subject), 
                           data = LC_ratios_alt_long %>% filter(age_group == 'YA'))

params.LC_time_sex <- model_parameters(mod.LC_time_sex,
                                       effects = 'fixed', 
                                       df_method = 'satterthwaite') %>%
  as.data.frame()

### effect sizes for mixed model
# convert t to eta-squared:
eta2.LC_time_sex <- t_to_eta2(params.LC_time_sex$t, 
                              df_error = params.LC_time_sex$df_error, 
                              ci = .95)
params.LC_time_sex$eta2 <- eta2.LC_time_sex$Eta2_partial
# convert t to d:
d.LC_time_sex <- t_to_d(params.LC_time_sex$t, 
                        df_error = params.LC_time_sex$df_error, 
                        ci = .95)
params.LC_time_sex$d <- d.LC_time_sex$d


# mixed model: change in LC contrast vs. training hemisphere,  by  --------

### set factor levels
LC_ratios_alt$hemi <- factor(LC_ratios_alt$hemi, levels = c('right', 'left'))
contrasts(LC_ratios_alt$hemi) <- c(-0.5, 0.5)
LC_ratios_alt$label_subject <- as.factor(LC_ratios_alt$label_subject)
LC_ratios_alt$condition <- factor(LC_ratios_alt$condition, levels = c('Osc-', 'Osc+'))
contrasts(LC_ratios_alt$condition) <- c(-0.5, 0.5)
LC_ratios_alt$gender <- factor(LC_ratios_alt$gender, levels = c('male', 'female'))
contrasts(LC_ratios_alt$gender) <- c(-0.5, 0.5)

### fit model
mod.LC_change_trainpower_sex <- lmer(LC_ratio_maxref_chg ~ Res_logPower_AR_train * hemi * gender + (1 | label_subject),
             data = LC_ratios_alt %>% filter(age_group == 'YA'))

params.LC_change_trainpower_sex <- model_parameters(mod.LC_change_trainpower_sex,
                                                    effects = 'fixed', 
                                                    df_method = 'satterthwaite') %>%
  as.data.frame()

### effect sizes for mixed model
# convert t to eta-squared:
eta2.LC_change_trainpower_sex <- t_to_eta2(params.LC_change_trainpower_sex$t, 
                                           df_error = params.LC_change_trainpower_sex$df_error, 
                                           ci = .95)
params.LC_change_trainpower_sex$eta2 <- eta2.LC_change_trainpower_sex$Eta2_partial

# convert t to d:
d.LC_change_trainpower_sex <- t_to_d(params.LC_change_trainpower_sex$t, 
                                           df_error = params.LC_change_trainpower_sex$df_error, 
                                           ci = .95)
params.LC_change_trainpower_sex$d <- d.LC_change_trainpower_sex$d
