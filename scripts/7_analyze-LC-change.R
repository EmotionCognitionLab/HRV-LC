# this script computes a metric of change in LC contrast
# between the pre- and post-training timepoints
# and visualizes & analyze the resulting metrics
# for the HRV-LC project
# written by shelby bachman, sbachman@usc.edu


# compute pre- and post-training LC contrast ------------------------------

# compute peak LC coordinates for each ID for ses-pre and ses-post separately
LC_coords_peak_meta <- left_join(
  data_LC_intensity_meta %>%
    filter(label_session == 'ses-pre') %>%
    group_by(label_subject, hemi) %>%
    summarize(x_peak_pre = x[which.max(value)],
              y_peak_pre = y[which.max(value)],
              z_peak_pre = z[which.max(value)]),
  
  data_LC_intensity_meta %>%
    filter(label_session == 'ses-post') %>%
    group_by(label_subject, hemi) %>%
    summarize(x_peak_post = x[which.max(value)],
              y_peak_post = y[which.max(value)],
              z_peak_post = z[which.max(value)]),
  
  by = c('label_subject', 'hemi')
)

# extract LC intensity ratios at ses-pre peak coordinates
LC_ratios_alt <- left_join(
  data_LC_intensity_meta %>%
    right_join(LC_coords_peak_meta %>%
                 select(label_subject, hemi, x = x_peak_pre, y = y_peak_pre, z = z_peak_pre),
               by = c('label_subject', 'hemi', 'x', 'y', 'z')) %>%
    select(label_subject, label_session, hemi, value_LC_precoord = value),
  
  # extract LC intensity values at ses-post peak coordinates
  data_LC_intensity_meta %>%
    right_join(LC_coords_peak_meta %>%
                 select(label_subject, hemi, x = x_peak_post, y = y_peak_post, z = z_peak_post),
               by = c('label_subject', 'hemi', 'x', 'y', 'z')) %>%
    select(label_subject, label_session, hemi, value_LC_postcoord = value),

  by = c('label_subject', 'label_session', 'hemi')
) %>% left_join(
  data_ref_intensity %>%
    group_by(label_subject, label_session) %>%
    summarize(max_ref = max(value),
              mean_ref = mean(value)),
  by = c('label_subject', 'label_session')
) %>%
  rowwise() %>%
  
  # compute LC ratios
  mutate(LC_ratio_precoord_maxref = (value_LC_precoord - max_ref)/max_ref,
         LC_ratio_postcoord_maxref = (value_LC_postcoord - max_ref)/max_ref) %>%
  
  # convert to wide
  select(label_subject, label_session, hemi, LC_ratio_precoord_maxref, LC_ratio_postcoord_maxref) %>%
  pivot_wider(names_from = label_session,
              names_glue = '{.value}_{label_session}',
              values_from = c(LC_ratio_precoord_maxref, LC_ratio_postcoord_maxref)) %>%
  
  # compute alt LC change metric for each participant & hemi
  rowwise() %>%
  mutate(
         # description of change metric: compute change at each pre- and post-peak location, then average over changes
         # e.g. average change in ratios
         LC_ratio_maxref_chg = ((`LC_ratio_precoord_maxref_ses-post` - `LC_ratio_precoord_maxref_ses-pre`) + 
                                 (`LC_ratio_postcoord_maxref_ses-post` - `LC_ratio_postcoord_maxref_ses-pre`)) / 2,
         LC_ratio_maxref_pre = (`LC_ratio_precoord_maxref_ses-pre` + `LC_ratio_postcoord_maxref_ses-pre`) / 2,
         LC_ratio_maxref_post = (`LC_ratio_precoord_maxref_ses-post` + `LC_ratio_postcoord_maxref_ses-post`) / 2
         
        ) %>%
  
  select(label_subject, hemi, LC_ratio_maxref_chg, LC_ratio_maxref_pre, LC_ratio_maxref_post)
  

# bind condition & age group  ---------------------------------------------

LC_ratios_alt <- LC_ratios_alt %>%
  left_join(data_demo_affect_cog %>%
              select(label_subject, age_group, condition),
            by = 'label_subject')


# keep only relevant dataframes -------------------------------------------

rm(LC_coords_peak_meta)


# set y-axis limits for LC contrast figures -------------------------------

# for figures with pre- and post-training LC contrast
ylims_LC <- c(-0.15, 0.30)

# for figures with change in LC contrast
ylims_LCchange <- c(-0.20, 0.18)


# figure: LC contrast change ----------------------------------------------

# set factor levels for plotting
LC_ratios_alt_fig <- LC_ratios_alt  # copy of data for plotting
LC_ratios_alt_fig$condition <- factor(LC_ratios_alt_fig$condition, levels = c('Osc+', 'Osc-'))
LC_ratios_alt_fig$hemi[LC_ratios_alt_fig$hemi == 'left'] <- 'Left LC'
LC_ratios_alt_fig$hemi[LC_ratios_alt_fig$hemi == 'right'] <- 'Right LC'

fig_LC_change_YA <- ggplot(data = LC_ratios_alt_fig %>% 
                             filter(age_group == 'YA'),
                           aes(x = condition, y = LC_ratio_maxref_chg, 
                               fill = condition, colour = condition)) +
  geom_hline(yintercept = 0, colour = 'darkgray') +
  geom_flat_violin(scale = 'width', trim = TRUE,
                   alpha = 0.4,
                   width = 0.7,
                   position = position_nudge(x = 0.1)) +
  geom_point(pch = 21,
             size = 2,
             colour = 'black', 
             position = position_jitter(width = 0.05)) +
  stat_sum_df('mean_cl_boot', position = position_nudge(x = 0.20)) +
  scale_fill_manual(values = palette_condition) +
  scale_colour_manual(values = palette_condition) +
  coord_cartesian(ylim = ylims_LCchange) +
  labs(x = '', y = 'Change in LC contrast',
       fill = '', colour = '',
       title = 'YA') +
  facet_wrap(~hemi, ncol = 2, nrow = 1, strip.position = 'bottom') +
  theme_apa() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(size = 10),
        strip.placement = 'outside',
        plot.title = element_text(hjust = 0.5))

fig_LC_change_OA <- ggplot(data = LC_ratios_alt_fig %>% 
                               filter(age_group == 'OA'),
                             aes(x = condition, y = LC_ratio_maxref_chg, 
                                 fill = condition, colour = condition)) +
  geom_hline(yintercept = 0, colour = 'darkgray') +
  geom_flat_violin(scale = 'width', trim = TRUE,
                   alpha = 0.4,
                   width = 0.7,
                   position = position_nudge(x = 0.1)) +
  geom_point(pch = 21,
             size = 2,
             colour = 'black', 
             position = position_jitter(width = 0.05)) +
  stat_sum_df('mean_cl_boot', position = position_nudge(x = 0.20)) +
  scale_fill_manual(values = palette_condition) +
  scale_colour_manual(values = palette_condition) +
  coord_cartesian(ylim = ylims_LCchange) +
  labs(x = '', y = 'Change in LC contrast',
       fill = '', colour = '',
       title = 'OA') +
  facet_wrap(~hemi, ncol = 2, nrow = 1, strip.position = 'bottom') +
  theme_apa() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(size = 10),
        strip.placement = 'outside',
        plot.title = element_text(hjust = 0.5))

# create figure (option 2: dot/violin plots)
fig_LC_change <- ggarrange(
  
  fig_LC_change_YA,
  fig_LC_change_OA,
  
  nrow = 1, ncol = 2, common.legend = TRUE
  
)


# figure: LC contrast by timepoint ----------------------------------------

### convert data to long for plotting
LC_ratios_alt_fig <- LC_ratios_alt_fig %>%
  select(label_subject, hemi, age_group, condition,
         LC_ratio_maxref_pre = LC_ratio_maxref_pre, LC_ratio_maxref_post = LC_ratio_maxref_post) %>%
  pivot_longer(cols = LC_ratio_maxref_pre:LC_ratio_maxref_post, 
               names_to = 'timepoint', 
               names_prefix = 'LC_ratio_maxref_', 
               values_to = 'LC_ratio')

### set factor levels
LC_ratios_alt_fig$timepoint <- factor(LC_ratios_alt_fig$timepoint, 
                                      levels = c('pre', 'post'))

LC_ratios_alt_fig$hemi <- factor(LC_ratios_alt_fig$hemi,
                                 levels = c('Left LC', 'Right LC'))

### plot - YA
fig_LC_YA <- ggplot(data = LC_ratios_alt_fig %>% 
                                filter(age_group == 'YA'),
                              aes(x = timepoint, y = LC_ratio, 
                                  fill = condition, colour = condition)) +
  geom_flat_violin(scale = 'width', trim = TRUE,
                   alpha = 0.4,
                   width = 0.7,
                   position = position_nudge(x = 0.1)) +
  geom_line(aes(group = label_subject),
            alpha = 0.3) +
  geom_line(data = LC_ratios_alt_fig %>%
              filter(age_group == 'YA') %>%
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
  coord_cartesian(ylim = ylims_LC) +
  labs(x = '', y = 'LC contrast',
       fill = '', colour = '',
       subtitle = 'Younger adults') +
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

### plot - OA
fig_LC_OA <- ggplot(data = LC_ratios_alt_fig %>% 
                                filter(age_group == 'OA'),
                              aes(x = timepoint, y = LC_ratio, 
                                  fill = condition, colour = condition)) +
  geom_flat_violin(scale = 'width', trim = TRUE,
                   alpha = 0.4,
                   width = 0.7,
                   position = position_nudge(x = 0.1)) +
  geom_line(aes(group = label_subject),
            alpha = 0.3) +
  geom_line(data = LC_ratios_alt_fig %>%
              filter(age_group == 'OA') %>%
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
  coord_cartesian(ylim = ylims_LC) +
  labs(x = '', y = 'LC contrast',
       fill = '', colour = '',
       subtitle = 'Older adults') +
  facet_wrap(~hemi*condition, ncol = 4, nrow = 1, strip.position = 'bottom') +
  theme_apa() +
  theme(text = element_text(size = 14, family = 'Arial'),
        strip.text.x = element_blank(),
        plot.subtitle = element_text(hjust = 0),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        panel.spacing.x = unit(0, 'lines'))

fig_LC <- ggarrange(
  
  fig_LC_YA,
  fig_LC_OA,
  
  nrow = 2, common.legend = TRUE, legend = 'right'
  
)

rm(LC_ratios_alt_fig)


# mixed effects model: LC contrast by condition & timepoint ---------------

# convert data to long for model
LC_ratios_alt_long <- LC_ratios_alt %>%
  select(label_subject, age_group, condition, hemi,
         LC_ratio_maxref_pre = LC_ratio_maxref_pre, LC_ratio_maxref_post = LC_ratio_maxref_post) %>%
  pivot_longer(cols = LC_ratio_maxref_pre:LC_ratio_maxref_post, 
               names_to = 'timepoint', 
               names_prefix = 'LC_ratio_maxref_', 
               values_to = 'LC_ratio')

# set factor levels and contrasts for mixed models
LC_ratios_alt_long$hemi <- factor(LC_ratios_alt_long$hemi, levels = c('right', 'left'))
contrasts(LC_ratios_alt_long$hemi) <- c(-0.5, 0.5)
LC_ratios_alt_long$label_subject <- as.factor(LC_ratios_alt_long$label_subject)
LC_ratios_alt_long$age_group <- factor(LC_ratios_alt_long$age_group, levels = c('YA', 'OA'))
contrasts(LC_ratios_alt_long$age_group) <- c(-0.5, 0.5)
LC_ratios_alt_long$condition <- factor(LC_ratios_alt_long$condition, levels = c('Osc-', 'Osc+'))
contrasts(LC_ratios_alt_long$condition) <- c(-0.5, 0.5)
LC_ratios_alt_long$timepoint <- factor(LC_ratios_alt_long$timepoint, levels = c('pre', 'post'))
contrasts(LC_ratios_alt_long$timepoint) <- c(-0.5, 0.5)

### fit models
mod.LC_time <- lmerTest::lmer(LC_ratio ~ timepoint * condition * hemi * age_group + (1 | label_subject), 
                    data = LC_ratios_alt_long)
params.LC_time <- model_parameters(mod.LC_time, 
                                   effects = 'fixed', 
                                   df_method = 'satterthwaite') %>%
  as.data.frame()

mod.LC_time_YA <- lmerTest::lmer(LC_ratio ~ timepoint * condition * hemi + (1 | label_subject), 
                       data = LC_ratios_alt_long %>% filter(age_group == 'YA'))
params.LC_time_YA <- model_parameters(mod.LC_time_YA, 
                                      effects = 'fixed', 
                                      df_method = 'satterthwaite') %>%
  as.data.frame()

mod.LC_time_OA <- lmer(LC_ratio ~ timepoint * condition * hemi + (1 | label_subject),
                       data = LC_ratios_alt_long %>% filter(age_group == 'OA'))
params.LC_time_OA <- model_parameters(mod.LC_time_OA, 
                                      effects = 'fixed', 
                                      df_method = 'satterthwaite') %>%
  as.data.frame()

### post hoc comparisons of estimated marginal means
emm.LC_time <- emmeans::emmeans(mod.LC_time, 
                                ~ timepoint | hemi * condition * age_group,
                                adjust = 'bonferroni',
                                lmer.df = 'satterthwaite')

contrasts.LC_time <- pairs(
  emm.LC_time,
  reverse = TRUE # reverse to get the timepoint factor contrast as: post - pre
) %>% 
  as.data.frame() %>%
  select(-contrast) %>%
  select(`Age Group` = age_group,
         Condition = condition,
         Hemisphere = hemi,
         Estimate = estimate,
         SE, df, 
         t = t.ratio,
         p = p.value) %>%
  arrange(`Age Group`,
          desc(Condition), 
          desc(Hemisphere))

emm.LC_time_YA <- emmeans::emmeans(mod.LC_time_YA, 
                                   ~ timepoint | hemi * condition,
                                   adjust = 'bonferroni',
                                   lmer.df = 'satterthwaite')

contrasts.LC_time_YA <- pairs(
  emm.LC_time_YA,
  reverse = TRUE # reverse to get the timepoint factor contrast as: post - pre
  ) %>% 
  as.data.frame() %>%
  select(-contrast) %>%
  select(Condition = condition,
         Hemisphere = hemi,
         Estimate = estimate,
         SE, df, 
         t = t.ratio,
         p = p.value) %>%
  arrange(desc(Condition), 
          desc(Hemisphere))

emm.LC_time_OA <- emmeans::emmeans(mod.LC_time_OA, 
                                   ~ timepoint | hemi * condition,
                                   adjust = 'bonferroni',
                                   lmer.df = 'satterthwaite')

contrasts.LC_time_OA <- pairs(
  emm.LC_time_OA,
  reverse = TRUE # reverse to get the timepoint factor contrast as: post - pre
) %>% 
  as.data.frame() %>%
  select(-contrast) %>%
  select(Condition = condition,
         Hemisphere = hemi,
         Estimate = estimate,
         SE, df, 
         t = t.ratio,
         p = p.value) %>%
  arrange(desc(Condition), 
          desc(Hemisphere))

### effect sizes for mixed effects models
eta2.LC_time <- t_to_eta2(params.LC_time$t, 
                          df_error = params.LC_time$df_error, 
                          ci = .95)
params.LC_time$eta2 <- eta2.LC_time$Eta2_partial

d.LC_time <- t_to_d(params.LC_time$t, 
                    df_error = params.LC_time$df_error, 
                    ci = .95)
params.LC_time$d <- d.LC_time$d

eta2.LC_time_YA <- t_to_eta2(params.LC_time_YA$t, 
                             df_error = params.LC_time_YA$df_error, 
                             ci = .95)
params.LC_time_YA$eta2 <- eta2.LC_time_YA$Eta2_partial

d.LC_time_YA <- t_to_d(params.LC_time_YA$t, 
                    df_error = params.LC_time_YA$df_error, 
                    ci = .95)
params.LC_time_YA$d <- d.LC_time_YA$d

eta2.LC_time_OA <- t_to_eta2(params.LC_time_OA$t, 
                             df_error = params.LC_time_OA$df_error, 
                             ci = .95)
params.LC_time_OA$eta2 <- eta2.LC_time_OA$Eta2_partial

d.LC_time_OA <- t_to_d(params.LC_time_OA$t, 
                       df_error = params.LC_time_OA$df_error, 
                       ci = .95)
params.LC_time_OA$d <- d.LC_time_OA$d

### effect sizes for post-hoc comparisons (YA only)
# NOTE: we reversed the time factor above to perform the correct contrast (post-pre)
# this isn't possible for the effect size calculation,
# so we multiply relevant columns by -1
cohensd.LC_time_YA <- emmeans::eff_size(
  object = emm.LC_time_YA,
  sigma = sigma(mod.LC_time_YA),
  edf = df.residual(mod.LC_time_YA)
  ) %>%
  as.data.frame() %>%
  select(contrast, Condition = condition,
         Hemisphere = hemi,
         d = effect.size,
         SE, df, 
         CI_lower_d = lower.CL,
         CI_upper_d = upper.CL) %>%
  mutate(d = -1*d,
         CI_lower_d = -1*CI_lower_d,
         CI_upper_d = -1*CI_upper_d) %>%
  arrange(desc(Condition), 
          desc(Hemisphere))
# since we reversed the contrast, we update its label:
cohensd.LC_time_YA$contrast <- 'post - pre'


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

# set y-axis limits for figure
ylims_LC_topo <- 

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

ggsave(filename = here('figures', 'figureSX_LC-change-topo.svg'), 
       plot = fig_LC_change_topo,
       device = 'svg',
       width = 6, height = 7, dpi = 400, bg = 'white')


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
