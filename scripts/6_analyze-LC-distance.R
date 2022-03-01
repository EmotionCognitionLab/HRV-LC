# this script computes 3D distances between peak LC locations
# at pre- and post-training timepoints
# for the HRV-LC project
# written by shelby bachman, sbachman@usc.edu


# function to compute 3-dimensional distance ------------------------------

compute_3D_dist <- function(x1, y1, z1, x2, y2, z2) {
  
  # note: calculation assumes difference between each coordinate pair
  # is in the units of 0.5mm
  
  x_dist <- (x1 - x2)*.5
  y_dist <- (y1 - y2)*.5
  z_dist <- (z1 - z2)*.5
  
  dist <- sqrt((x_dist)^2 + (y_dist)^2 + (z_dist)^2)
  return(dist)
}


# compute 3D distance between pre/post peak LC locations ------------------

LC_peak_distance <- data_intensity_peak %>%
  select(label_subject, label_session, hemi, x = x_peak, y = y_peak, z = z_peak) %>%
  pivot_wider(names_from = label_session, 
              names_glue = "{.value}_{label_session}", 
              values_from = c(x, y, z)) %>%
  select(label_subject, hemi,
         x_pre = `x_ses-pre`, y_pre = `y_ses-pre`, z_pre = `z_ses-pre`,
         x_post = `x_ses-post`, y_post = `y_ses-post`, z_post = `z_ses-post`) %>%
  rowwise() %>%
  mutate(dist_prepost = compute_3D_dist(x_pre, y_pre, z_pre, x_post, y_post, z_post)) %>%
  mutate(hemi = ifelse(hemi == 'left', 'Left LC',
                       ifelse(hemi == 'right', 'Right LC', NA))) %>%
  left_join(data_demo_affect_cog %>% 
              select(label_subject, condition, age_group), 
            by = 'label_subject')


# normality check of 3D distance values -----------------------------------

normalitycheck_dist <- shapiro.test(LC_peak_distance$dist_prepost)

LC_peak_distance <- LC_peak_distance %>%
  rowwise() %>%
  mutate(dist_prepost_sqrt = sqrt(dist_prepost))


# figure: 3D distance between pre/post peak LC locations ------------------

# set factor levels for plotting
LC_peak_distance$condition <- factor(LC_peak_distance$condition, levels = c('Osc+', 'Osc-'))

# create figure (density plot version)
fig_LC_peak_distance <- ggplot(data = LC_peak_distance, 
                               aes(x = dist_prepost, fill = condition)) +
  geom_density(alpha = 0.4) +
  labs(x = '3D distance between peak LC intensity,\npre- vs. post-training (mm)',
       fill = '') +
  scale_fill_manual(values = palette_condition) +
  facet_wrap(~hemi) +
  theme_apa() + theme_font


# figure: 3D distance between pre/post LC locations by age group  --------

# reorder hemi to be in line with hemi order on 3D plot
LC_peak_distance$hemi <- factor(LC_peak_distance$hemi, levels = c('Left LC', 'Right LC'))

fig_LC_peak_distance_v2 <- ggarrange(
  ggplot(data = LC_peak_distance %>% 
           filter(age_group == 'OA'), 
         aes(x = dist_prepost, fill = condition)) +
    geom_density(alpha = 0.4) +
    labs(x = '3D Distance (mm)',
         y = 'Density',
         fill = '',
         subtitle = 'Younger adults') +
    scale_fill_manual(values = palette_condition) +
    facet_wrap(~hemi) +
    theme_apa() + theme_font +
    theme(text = element_text(size = 14),
          plot.subtitle = element_text(hjust = 0)),
  
  ggplot(data = LC_peak_distance %>% 
           filter(age_group == 'OA'), 
         aes(x = dist_prepost, fill = condition)) +
    geom_density(alpha = 0.4) +
    labs(x = '3D Distance (mm)',
         y = 'Density',
         fill = '',
         subtitle = 'Older adults') +
    scale_fill_manual(values = palette_condition) +
    facet_wrap(~hemi) +
    theme_apa() + theme_font +
    theme(text = element_text(size = 14),
          plot.subtitle = element_text(hjust = 0)),
  
  nrow = 1, ncol = 2, common.legend = TRUE, legend = 'right'
)


# figure: peak LC locations by age group & timepoint ----------------------

# add variables
LC_peak_locations <- data_intensity_peak %>%
  select(label_subject, label_session, hemi, x = x_peak, y = y_peak, z = z_peak) %>%
  mutate(hemi = ifelse(hemi == 'left', 'Left LC',
                       ifelse(hemi == 'right', 'Right LC', NA)),
         session_name = ifelse(label_session == 'ses-pre', 'Pre-training',
                               ifelse(label_session == 'ses-post', 'Post-training', NA))) %>%
  left_join(data_demo_affect_cog %>% 
              select(label_subject, condition, age_group), 
            by = 'label_subject')


# make x-coordinates negative to show left on left, right on right
LC_peak_locations <- LC_peak_locations %>%
  mutate(x = x*-1)

# 3D scatterplot settings for rotation, tilt
theta <- 10
phi <- 20

# code to set x, y, z axis limits
fix_bounds_3D <- axis_labs_3D(theta = theta, phi = phi,
                              inherit.aes = FALSE,
                              data=data.frame(
                                x=c(min(LC_peak_locations$x), max(LC_peak_locations$x)),
                                y=c(min(LC_peak_locations$y), max(LC_peak_locations$y)),
                                z=c(min(LC_peak_locations$z), max(LC_peak_locations$z))),
                              aes(x = x, y = y, z = z),
                              geom = 'text', size=3, 
                              hjust=c(-1,-1,1.2,1.2,-1.2,-1.2), 
                              vjust=c(-0.5,-0.5,-.2,-.2,0.2,0.2))

# create 3D scatterplot for each age group & timepoint
fig_LC_peak_locations <- ggarrange(
  
  ggplot(data = LC_peak_locations %>% 
           filter(age_group == 'YA', label_session == 'ses-pre'), 
         aes(x = x, y = y, z = z, 
             colour = condition)) +
    axes_3D(theta = theta, phi = phi) +
    stat_3D(theta = theta, phi = phi, alpha = 0.6) +
    labs_3D(theta = theta, phi = phi, size = 5,
            labs=c("x", "y", "z"),
            hjust=c(0,1,1), vjust=c(1, 1, -0.2), angle=c(0, 0, 0)) +
    fix_bounds_3D +
    scale_colour_manual(values = palette_condition) +
    labs(colour = '',
         title = 'Pre-intervention',
         subtitle = 'Younger adults') +
    theme_void() +
    theme(text = element_text(size = 14), 
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0)),
  
  ggplot(data = LC_peak_locations %>% 
           filter(age_group == 'YA', label_session == 'ses-post'), 
         aes(x = x, y = y, z = z, 
             colour = condition)) +
    axes_3D(theta = theta, phi = phi) +
    stat_3D(theta = theta, phi = phi, alpha = 0.6) +
    labs_3D(theta = theta, phi = phi, size = 5,
            labs=c("x", "y", "z"),
            hjust=c(0,1,1), vjust=c(1, 1, -0.2), angle=c(0, 0, 0)) +
    fix_bounds_3D +
    scale_colour_manual(values = palette_condition) +
    labs(colour = '',
         title = 'Post-intervention',
         subtitle = '') +
    theme_void() +
    theme(text = element_text(size = 14),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0)),
  
  ggplot(data = LC_peak_locations %>% 
           filter(age_group == 'OA', label_session == 'ses-pre'), 
         aes(x = x, y = y, z = z, 
             colour = condition)) +
    axes_3D(theta = theta, phi = phi) +
    stat_3D(theta = theta, phi = phi, alpha = 0.6) +
    labs_3D(theta = theta, phi = phi, size = 5,
            labs=c("x", "y", "z"),
            hjust=c(0,1,1), vjust=c(1, 1, -0.2), angle=c(0, 0, 0)) +
    fix_bounds_3D +
    scale_colour_manual(values = palette_condition) +
    labs(colour = '',
         title = '',
         subtitle = 'Older adults') +
    theme_void() +
    theme(text = element_text(size = 14),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0)),
  
  ggplot(data = LC_peak_locations %>% 
           filter(age_group == 'OA', label_session == 'ses-post'), 
         aes(x = x, y = y, z = z, 
             colour = condition)) +
    axes_3D(theta = theta, phi = phi) +
    stat_3D(theta = theta, phi = phi, alpha = 0.6) +
    labs_3D(theta = theta, phi = phi, size = 5,
            labs=c("x", "y", "z"),
            hjust=c(0,1,1), vjust=c(1, 1, -0.2), angle=c(0, 0, 0)) +
    fix_bounds_3D +
    scale_colour_manual(values = palette_condition) +
    labs(colour = '',
         title = '',
         subtitle = '') +
    theme_void() +
    theme(text = element_text(size = 14),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0)),
  
  nrow = 2, ncol = 2, common.legend = TRUE, legend = 'right'
  
)


# figure S2 ---------------------------------------------------------------

# change hemi order to match 3D plot
LC_peak_distance$hemi <- factor(LC_peak_distance$hemi, levels = c('Left LC', 'Right LC'))

figureS2 <- ggarrange(
  ggplot(data = LC_peak_locations %>% 
           filter(age_group == 'YA', label_session == 'ses-pre'), 
         aes(x = x, y = y, z = z, 
             colour = condition)) +
    axes_3D(theta = theta, phi = phi) +
    stat_3D(theta = theta, phi = phi, alpha = 0.6) +
    labs_3D(theta = theta, phi = phi, size = 5,
            labs=c("x", "y", "z"),
            hjust=c(0,1,1), vjust=c(0.8, 1.2, -0.2), angle=c(0, 0, 0)) +
    fix_bounds_3D +
    scale_colour_manual(values = palette_condition) +
    labs(colour = '',
         title = '',
         subtitle = 'Pre-training') +
    theme_void() +
    theme(text = element_text(size = 14, family = 'Arial'),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5)),
  
  ggplot(data = LC_peak_locations %>% 
           filter(age_group == 'YA', label_session == 'ses-post'), 
         aes(x = x, y = y, z = z, 
             colour = condition)) +
    axes_3D(theta = theta, phi = phi) +
    stat_3D(theta = theta, phi = phi, alpha = 0.6) +
    labs_3D(theta = theta, phi = phi, size = 5,
            labs=c("x", "y", "z"),
            hjust=c(0,1,1), vjust=c(0.8, 1.2, -0.2), angle=c(0, 0, 0)) +
    fix_bounds_3D +
    scale_colour_manual(values = palette_condition) +
    labs(colour = '',
         title = '',
         subtitle = 'Post-training') +
    theme_void() +
    theme(text = element_text(size = 14, family = 'Arial'),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5)),
  
  ggplot(data = LC_peak_distance %>% 
           filter(age_group == 'YA'), 
         aes(x = dist_prepost, fill = condition)) +
    geom_density(alpha = 0.4) +
    labs(x = '3D Distance (mm)',
         y = 'Density',
         fill = '',
         subtitle = '') +
    coord_cartesian(ylim = c(0, 0.3)) +
    scale_fill_manual(values = palette_condition) +
    facet_wrap(~hemi) +
    theme_apa() + 
    theme(text = element_text(size = 14, family = 'Arial'),
          axis.text.x = element_text(size = 10),
          axis.text.y = element_text(size = 10),
          axis.title = element_text(size = 12)),
  
  ggplot(data = LC_peak_locations %>% 
           filter(age_group == 'OA', label_session == 'ses-pre'), 
         aes(x = x, y = y, z = z, 
             colour = condition)) +
    axes_3D(theta = theta, phi = phi) +
    stat_3D(theta = theta, phi = phi, alpha = 0.6) +
    labs_3D(theta = theta, phi = phi, size = 5,
            labs=c("x", "y", "z"),
            hjust=c(0,1,1), vjust=c(0.8, 1.2, -0.2), angle=c(0, 0, 0)) +
    fix_bounds_3D +
    scale_colour_manual(values = palette_condition) +
    labs(colour = '',
         title = '',
         subtitle = '') +
    theme_void() +
    theme(text = element_text(size = 14, family = 'Arial'),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5)),
  
  ggplot(data = LC_peak_locations %>% 
           filter(age_group == 'OA', label_session == 'ses-post'), 
         aes(x = x, y = y, z = z, 
             colour = condition)) +
    axes_3D(theta = theta, phi = phi) +
    stat_3D(theta = theta, phi = phi, alpha = 0.6) +
    labs_3D(theta = theta, phi = phi, size = 5,
            labs=c("x", "y", "z"),
            hjust=c(0,1,1), vjust=c(0.8, 1.2, -0.2), angle=c(0, 0, 0)) +
    fix_bounds_3D +
    scale_colour_manual(values = palette_condition) +
    labs(colour = '',
         title = '',
         subtitle = '') +
    theme_void() +
    theme(text = element_text(size = 14, family = 'Arial'),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5)),
  
  ggplot(data = LC_peak_distance %>% 
           filter(age_group == 'OA'), 
         aes(x = dist_prepost, fill = condition)) +
    geom_density(alpha = 0.4) +
    labs(x = '3D Distance (mm)',
         y = 'Density',
         fill = '',
         subtitle = '') +
    coord_cartesian(ylim = c(0, 0.3)) +
    scale_fill_manual(values = palette_condition) +
    facet_wrap(~hemi) +
    theme_apa() +
    theme(text = element_text(size = 14, family = 'Arial'),
          axis.text.x = element_text(size = 10),
          axis.text.y = element_text(size = 10),
          axis.title = element_text(size = 12)),
  
  nrow = 2, ncol = 3, common.legend = TRUE, legend = 'right',
  
  labels = c('A', '', 
             'B', '', '', ''),
  label.x = c(0, 0, 0, 0, 0, 0),
  font.label = list(size = 16),
  
  widths = c(1, 1, 1.5)
  
)

ggsave(filename = here('figures', 'figureS2_LC-peak.svg'), 
       plot = figureS2,
       device = 'svg',
       width = 12, height = 6, dpi = 400, bg = 'white')


# mixed effects model: 3D distance between pre/post peak LC ---------------

# set factor levels
LC_peak_distance$hemi <- factor(LC_peak_distance$hemi, levels = c('Right LC', 'Left LC'))
contrasts(LC_peak_distance$hemi) <- c(-0.5, 0.5)
LC_peak_distance$label_subject <- as.factor(LC_peak_distance$label_subject)
LC_peak_distance$age_group <- factor(LC_peak_distance$age_group, levels = c('YA', 'OA'))
contrasts(LC_peak_distance$age_group) <- c(-0.5, 0.5)
LC_peak_distance$condition <- factor(LC_peak_distance$condition, levels = c('Osc-', 'Osc+'))
contrasts(LC_peak_distance$condition) <- c(-0.5, 0.5)

# fit model
mod.LC_distance <- lmer(dist_prepost_sqrt ~ condition * hemi * age_group + (1 | label_subject), 
                           data = LC_peak_distance)

params.LC_distance <- model_parameters(mod.LC_distance,
                                       effects = 'fixed', 
                                       df_method = 'satterthwaite') %>%
  as.data.frame()

# effect sizes (t to eta-squared)
eta2.LC_distance <- t_to_eta2(params.LC_distance$t, 
                          df_error = params.LC_distance$df_error, 
                          ci = .95)
params.LC_distance$eta2 <- eta2.LC_distance$Eta2_partial

# effect sizes (t to d)
d.LC_distance <- t_to_d(params.LC_distance$t,
                        df_error = params.LC_distance$df_error,
                        ci = .95)
params.LC_distance$d <- d.LC_distance$d
