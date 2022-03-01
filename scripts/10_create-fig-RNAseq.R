# this script creates a figure
# depicting fold-difference CREB activity values
# for the HRV-LC project
# written by shelby bachman, sbachman@usc.edu


# organize CREB activity data ---------------------------------------------

data_CREB <- data.frame(
  hemi = c('Left LC contrast\nchange', 'Right LC contrast\nchange'),
  fold_mean = c(1.34, 1.17),
  fold_SE = c(1.16, 1.28),
  p = c(0.0493, 0.5296)
) %>%
  rowwise() %>%
  mutate(SE = (fold_mean*fold_SE) - fold_mean) %>%
  select(hemi, fold_mean, SE, p)


# create figure4 ----------------------------------------------------------

figure4 <- ggplot(data = data_CREB,
                  aes(x = hemi, y = fold_mean)) +
  geom_crossbar(aes(ymin = fold_mean - SE, 
                    ymax = fold_mean + SE),
                width = 0.1,
                fill = 'lightgray') +
  geom_hline(yintercept = 1, colour = 'black', 
             linetype = 'dotted') +
  labs(x = '', y = 'Fold-difference in CREB activity,
       pre vs. post-training') +
  geom_text(aes(label = hemi, y = 1.68)) +
  coord_cartesian(ylim = c(0.8, 1.7)) +
  theme_apa() +
  theme(text = element_text(size = 14, family = 'Arial'),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.y = element_text(size = 10),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        plot.margin=unit(c(1,1,-5,1), "mm"))


# save figure -------------------------------------------------------------
  
ggsave(filename = here('figures', 'figure4_CREB-change.svg'), 
       plot = figure4,
       device = 'svg',
       width = 5, height = 3, dpi = 400, bg = 'white')
