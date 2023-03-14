# this script computes change in rostral and caudal LC contrast
# between the pre- and post-training timepoints
# and visualizes & analyze the resulting metrics
# for the HRV-LC project
# written by shelby bachman, sbachman@usc.edu


# identify rostral and caudal coordinates ---------------------------------

# current range of z-slices
slices_z <- range(data_LC_ratios_slicewise$z)

# caudal cluster
# based on percentiles from Bachman et al., 2021
#100-(100/17)*(c(1,6))
start_caudal <- 1-0.941 # caudal end
end_caudal <- 1-0.647 # caudal --> rostral
slices_caudal <- as.numeric(round(quantile(slices_z, c(start_caudal, end_caudal))))
slices_caudal_range <- slices_caudal[1]:slices_caudal[2]

# rostral cluster
# based on percentiles from Bachman et al., 2021
#100-(100/17)*c(10,12)
start_rostral <- 1-0.412 # caudal --> rostral
end_rostral <- 1-0.294 # rostral end
slices_rostral <- as.numeric(round(quantile(slices_z, c(start_rostral, end_rostral))))
slices_rostral_range <- slices_rostral[1]:slices_rostral[2]

rm(slices_z, start_caudal, end_caudal, slices_caudal,
   start_rostral, end_rostral, slices_rostral)


# compute pre- and post-training rostral/caudal LC contrast ---------------

LC_ratios_topo <- left_join(
  data_LC_ratios_slicewise %>%
    # filter to include only rostral slices
    filter(z %in% slices_rostral_range) %>%
    # average ratios across slices
    group_by(label_subject, label_session) %>%
    summarize(LC_ratio_rostral = mean(LC_ratio_meta, na.rm = TRUE)),
  
    data_LC_ratios_slicewise %>%
      # filter to include only caudal slices
      filter(z %in% slices_caudal_range) %>%
      # average ratios across slices
      group_by(label_subject, label_session) %>%
      summarize(LC_ratio_caudal = mean(LC_ratio_meta, na.rm = TRUE)),
  
    by = c('label_subject', 'label_session')
)

# bind demographic information
LC_ratios_topo <- LC_ratios_topo %>%
  left_join(data_demo_affect_cog %>%
            select(label_subject, age_group, condition),
          by = 'label_subject')


# figure: change in rostral/mid/caudal LC ---------------------------------

# make a copy of the data for plotting
LC_ratios_topo_fig <- LC_ratios_topo

# set factor levels for plotting
LC_ratios_topo_fig$condition <- factor(LC_ratios_topo_fig$condition, levels = c('Osc+', 'Osc-'))
LC_ratios_topo_fig$timepoint <- LC_ratios_topo_fig$label_session
LC_ratios_topo_fig$timepoint[LC_ratios_topo_fig$timepoint == 'ses-pre'] <- 'pre'
LC_ratios_topo_fig$timepoint[LC_ratios_topo_fig$timepoint == 'ses-post'] <- 'post'
LC_ratios_topo_fig$timepoint <- factor(LC_ratios_topo_fig$timepoint, 
                                      levels = c('pre', 'post'))

# convert wide to long
LC_ratios_topo_fig <- LC_ratios_topo_fig %>%
  pivot_longer(cols = LC_ratio_rostral:LC_ratio_caudal,
               names_to = 'topo', names_prefix = 'LC_ratio_', values_to = 'LC_ratio')
LC_ratios_topo_fig$topo <- factor(LC_ratios_topo_fig$topo, levels = c('caudal', 'rostral'))

### plot - YA
fig_LC_topo_YA <- ggplot(data = LC_ratios_topo_fig %>% 
                      filter(age_group == 'YA'),
                    aes(x = timepoint, y = LC_ratio, 
                        fill = condition, colour = condition)) +
  geom_flat_violin(scale = 'width', trim = TRUE,
                   alpha = 0.4,
                   width = 0.7,
                   position = position_nudge(x = 0.1)) +
  geom_line(aes(group = label_subject),
            alpha = 0.3) +
  geom_line(data = LC_ratios_topo_fig %>%
              filter(age_group == 'YA') %>%
              group_by(condition, timepoint, topo) %>%
              summarize(mean_LC_ratio = mean(LC_ratio, na.rm = TRUE)) %>%
              select(condition, timepoint, topo, LC_ratio = mean_LC_ratio),
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
  coord_cartesian(ylim = ylims_LC) +
  labs(x = '', y = 'LC contrast',
       fill = '', colour = '',
       subtitle = 'Younger adults') +
  facet_wrap(~topo*condition, ncol = 6, nrow = 1, strip.position = 'bottom') +
  theme_apa() +
  theme(text = element_text(size = 14, family = 'Arial'),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        strip.text.x = element_blank(),
        plot.subtitle = element_text(hjust = 0),
        panel.spacing.x = unit(0, 'lines'))

### plot - OA
fig_LC_topo_OA <- ggplot(data = LC_ratios_topo_fig %>% 
                           filter(age_group == 'OA'),
                         aes(x = timepoint, y = LC_ratio, 
                             fill = condition, colour = condition)) +
  geom_flat_violin(scale = 'width', trim = TRUE,
                   alpha = 0.4,
                   width = 0.7,
                   position = position_nudge(x = 0.1)) +
  geom_line(aes(group = label_subject),
            alpha = 0.3) +
  geom_line(data = LC_ratios_topo_fig %>%
              filter(age_group == 'OA') %>%
              group_by(condition, timepoint, topo) %>%
              summarize(mean_LC_ratio = mean(LC_ratio, na.rm = TRUE)) %>%
              select(condition, timepoint, topo, LC_ratio = mean_LC_ratio),
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
  coord_cartesian(ylim = ylims_LC) +
  labs(x = '', y = 'LC contrast',
       fill = '', colour = '',
       subtitle = 'Older adults') +
  facet_wrap(~topo*condition, ncol = 6, nrow = 1, strip.position = 'bottom') +
  theme_apa() +
  theme(text = element_text(size = 14, family = 'Arial'),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        strip.text.x = element_blank(),
        plot.subtitle = element_text(hjust = 0),
        panel.spacing.x = unit(0, 'lines'))

# create composite figure 2
fig_LC_change_topo <- ggarrange(
  
  fig_LC_topo_YA,
  fig_LC_topo_OA,
  
  nrow = 2, ncol = 1, common.legend = TRUE, legend = 'right'
  
)


# mixed effects model: caudal LC contrast by time and condition -----------

# set factor levels and contrasts for mixed models
LC_ratios_topo$label_subject <- as.factor(LC_ratios_topo$label_subject)
LC_ratios_topo$age_group <- factor(LC_ratios_topo$age_group, levels = c('YA', 'OA'))
contrasts(LC_ratios_topo$age_group) <- c(-0.5, 0.5)
LC_ratios_topo$condition <- factor(LC_ratios_topo$condition, levels = c('Osc-', 'Osc+'))
contrasts(LC_ratios_topo$condition) <- c(-0.5, 0.5)
LC_ratios_topo$label_session <- factor(LC_ratios_topo$label_session, levels = c('ses-pre', 'ses-post'))
contrasts(LC_ratios_topo$label_session) <- c(-0.5, 0.5)

### fit models
mod.LC_time_caudal <- lmerTest::lmer(LC_ratio_caudal ~ label_session * condition * age_group + (1 | label_subject), 
                              data = LC_ratios_topo)
params.LC_time_caudal <- model_parameters(mod.LC_time_caudal, 
                                   effects = 'fixed', 
                                   df_method = 'satterthwaite') %>%
  as.data.frame()

mod.LC_time_YA_caudal <- lmerTest::lmer(LC_ratio_caudal ~ label_session * condition + (1 | label_subject), 
                                     data = LC_ratios_topo %>% filter(age_group == 'YA'))
params.LC_time_YA_caudal <- model_parameters(mod.LC_time_YA_caudal, 
                                          effects = 'fixed', 
                                          df_method = 'satterthwaite') %>%
  as.data.frame()

mod.LC_time_OA_caudal <- lmerTest::lmer(LC_ratio_caudal ~ label_session * condition + (1 | label_subject), 
                                        data = LC_ratios_topo %>% filter(age_group == 'OA'))
params.LC_time_OA_caudal <- model_parameters(mod.LC_time_OA_caudal, 
                                             effects = 'fixed', 
                                             df_method = 'satterthwaite') %>%
  as.data.frame()


# mixed effects model: rostral LC contrast by time and condition -----------

### fit models
mod.LC_time_rostral <- lmerTest::lmer(LC_ratio_rostral ~ label_session * condition * age_group + (1 | label_subject), 
                                     data = LC_ratios_topo)
params.LC_time_rostral <- model_parameters(mod.LC_time_rostral, 
                                          effects = 'fixed', 
                                          df_method = 'satterthwaite') %>%
  as.data.frame()

mod.LC_time_YA_rostral <- lmerTest::lmer(LC_ratio_rostral ~ label_session * condition + (1 | label_subject), 
                                        data = LC_ratios_topo %>% filter(age_group == 'YA'))
params.LC_time_YA_rostral <- model_parameters(mod.LC_time_YA_rostral, 
                                             effects = 'fixed', 
                                             df_method = 'satterthwaite') %>%
  as.data.frame()

mod.LC_time_OA_rostral <- lmerTest::lmer(LC_ratio_rostral ~ label_session * condition + (1 | label_subject), 
                                        data = LC_ratios_topo %>% filter(age_group == 'OA'))
params.LC_time_OA_rostral <- model_parameters(mod.LC_time_OA_rostral, 
                                             effects = 'fixed', 
                                             df_method = 'satterthwaite') %>%
  as.data.frame()

### post hoc comparisons of estimated marginal means
emm.LC_time_rostral <- emmeans::emmeans(mod.LC_time_rostral, 
                                ~ label_session | condition * age_group,
                                adjust = 'bonferroni',
                                lmer.df = 'satterthwaite')

contrasts.LC_time_rostral <- pairs(
  emm.LC_time_rostral,
  reverse = TRUE # reverse to get the timepoint factor contrast as: post - pre
) %>% 
  as.data.frame() %>%
  select(-contrast) %>%
  select(`Age Group` = age_group,
         Condition = condition,
         Estimate = estimate,
         SE, df, 
         t = t.ratio,
         p = p.value) %>%
  arrange(`Age Group`,
          desc(Condition))

# join training power to rostral/caudal LC change values ------------------

LC_ratios_topo <- LC_ratios_topo %>%
  # convert from long to wide to compute post-pre rostral/caudal values
  pivot_wider(names_from = label_session,
              values_from = c(LC_ratio_rostral, LC_ratio_caudal)) %>%
  rowwise() %>%
  mutate(LC_rostral_chg = `LC_ratio_rostral_ses-post` - `LC_ratio_rostral_ses-pre`,
         LC_caudal_chg = `LC_ratio_caudal_ses-post` - `LC_ratio_caudal_ses-pre`) %>%
  select(-c(`LC_ratio_rostral_ses-post`, `LC_ratio_rostral_ses-pre`,
            `LC_ratio_caudal_ses-post`, `LC_ratio_caudal_ses-pre`)) %>%
  left_join(data_power, by = 'label_subject') %>%
  # convert from wide to long to get topo in its own column
  pivot_longer(cols = LC_rostral_chg:LC_caudal_chg,
               names_to = 'topo', values_to = 'LC_ratio_chg',
               names_pattern = "LC_(.*)_chg")


# figure: change in rostral/caudal LC contrast vs. training power ---------

# make a copy of the data for plotting & analysis
LC_ratios_topo_power <- LC_ratios_topo

# set factor levels and names for plotting
LC_ratios_topo_power$condition <- factor(LC_ratios_topo_power$condition, levels = c('Osc+', 'Osc-'))
LC_ratios_topo_power$topo <- factor(LC_ratios_topo_power$topo, levels = c('caudal', 'rostral'))

# create figure
fig_LC_change_trainpower_YA_topo <- ggplot(data = LC_ratios_topo_power %>% 
                                             filter(age_group == 'YA'), 
                                           aes(x = Res_logPower_AR_train, y = LC_ratio_chg)) +
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
  facet_wrap(~topo) +
  guides(colour = FALSE, fill = FALSE) +
  theme_apa() +
  theme(text = element_text(size = 14, family = 'Arial'),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        strip.text.x = element_text(size = 12))

fig_LC_change_trainpower_OA_topo <- ggplot(data = LC_ratios_topo_power %>% 
                                             filter(age_group == 'OA'), 
                                           aes(x = Res_logPower_AR_train, y = LC_ratio_chg)) +
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
  facet_wrap(~topo) +
  guides(colour = FALSE, fill = FALSE) +
  theme_apa() +
  theme(text = element_text(size = 14, family = 'Arial'),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        strip.text.x = element_text(size = 12))

fig_LC_change_trainpower_topo <- ggarrange(
  fig_LC_change_trainpower_YA_topo, 
  fig_LC_change_trainpower_OA_topo,
  nrow = 2,
  
  common.legend = TRUE,
  legend = 'right'
)

# create composite rostral/caudal figure
figureS3 <- ggarrange(
  
  fig_LC_change_topo,
  fig_LC_change_trainpower_topo,
  
  widths = c(1.5, 1),
  
  labels = c('A', 'B'),
  font.label = list(size = 16),
  common.legend = TRUE, legend = 'right'
  
)

ggsave(filename = here('figures', 'figureS3_LC-change_topo.svg'), 
       plot = figureS3,
       device = 'svg',
       width = 12, height = 7, dpi = 400, bg = 'white')


# correlation analyses: rostral and caudal LC contrast --------------------

# perform pearson correlation analyses
corr_YA_rostral <- cor.test(~LC_ratio_chg + Res_logPower_AR_train, 
                            data = LC_ratios_topo %>%
                              filter(age_group == 'YA',
                                     topo == 'rostral'), 
                            method = 'pearson')

corr_YA_caudal <- cor.test(~LC_ratio_chg + Res_logPower_AR_train, 
                           data = LC_ratios_topo %>%
                             filter(age_group == 'YA',
                                    topo == 'caudal'), 
                           method = 'pearson')

corr_OA_rostral <- cor.test(~LC_ratio_chg + Res_logPower_AR_train, 
                            data = LC_ratios_topo %>%
                              filter(age_group == 'OA',
                                     topo == 'rostral'), 
                            method = 'pearson')

corr_OA_caudal <- cor.test(~LC_ratio_chg + Res_logPower_AR_train, 
                           data = LC_ratios_topo %>%
                             filter(age_group == 'OA',
                                    topo == 'caudal'), 
                           method = 'pearson')

# format results as table
corr_chg_power_topo <- as.data.frame(
  rbind(
    format_corr_results(corr_YA_caudal) %>%
      mutate(age_group = 'YA',
             topo = 'Caudal LC'),
    
    format_corr_results(corr_YA_rostral) %>%
      mutate(age_group = 'YA',
             topo = 'Rostral LC'),
    
    format_corr_results(corr_OA_caudal) %>%
      mutate(age_group = 'OA',
             topo = 'Caudal LC'),
    
    format_corr_results(corr_OA_rostral) %>%
      mutate(age_group = 'OA',
             topo = 'Rostral LC')
    
  )
) %>%
  select(age_group, topo, r, df, ci, p)
