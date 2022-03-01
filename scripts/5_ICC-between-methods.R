# this script performs ICC analyses
# reflecting correspondence between peak LC intensities
# determined by the manual & semiautomated methods
# for the HRV-LC project
# written by shelby bachman, sbachman@usc.edu

# Note: because we assess different scans with each method
# (manual uses native scans for LC intensity, semi-auto uses warped FSE scans for LC intensity)
# ICC analyses are type = consistency


# join data for ICC analyses between methods ------------------------------

# (since we previously saw high correspondence of intensity values
# across raters, for this step we first average manual intensity ratings across raters)

data_ICC <- left_join(data_intensity_peak %>% 
                        select(label_subject, label_session, hemi, LC_intensity_peak_meta) %>%
                        pivot_wider(names_from = hemi, 
                                    values_from = LC_intensity_peak_meta, 
                                    names_prefix = 'LC_intensity_peak_meta_'),
                      data_LC_manual %>% select(label_subject, label_session, LC_left_manual_avg, LC_right_manual_avg),
                      by = c('label_subject', 'label_session'))

# number of scans missing manual ratings
nrow(data_ICC %>%
       filter(is.na(LC_left_manual_avg) | is.na(LC_right_manual_avg)))


# perform ICC analysis between methods, left LC ---------------------------

icc_methods_leftLC <- irr::icc(data_ICC %>% ungroup() %>% select(LC_intensity_peak_meta_left, LC_left_manual_avg) %>% na.omit,
                          type = 'consistency',
                          model = 'twoway')


# perform ICC analysis between methods, right LC --------------------------

icc_methods_rightLC <- irr::icc(data_ICC %>% ungroup() %>% select(LC_intensity_peak_meta_right, LC_right_manual_avg) %>% na.omit,
                           type = 'consistency',
                           model = 'twoway')

rm(data_ICC)
