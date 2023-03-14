# this script computes peak LC metrics for each scan
# (used for ICC calculations)
# for the HRV-LC project
# written by shelby bachman, sbachman@usc.edu


# identify peak LC intensities --------------------------------------------

# (used for ICC calculations)

data_intensity_peak <- left_join(
  data_LC_intensity_meta %>%
    group_by(label_subject, label_session, hemi) %>%
    summarize(LC_intensity_peak_meta = max(value, na.rm = TRUE),
              x_peak = x[which.max(value)],
              y_peak = y[which.max(value)],
              z_peak = z[which.max(value)]),
  
  data_ref_intensity %>%
    group_by(label_subject, label_session) %>%
    summarize(ref_peak_meta = max(value, na.rm = TRUE)),
) 


# identify slice-wise peak LC intensities ---------------------------------

# (used for calculating rostral and caudal LC contrast)

data_LC_ratios_slicewise <- data_LC_intensity_meta %>%
  
  # compute slice-wise peak intensities for each hemi
    group_by(label_subject, label_session, hemi, z) %>%
    summarize(LC_intensity_peak_meta = max(value, na.rm = TRUE)) %>%
  
  pivot_wider(names_from = hemi,
              names_prefix = 'LC_intensity_peak_meta_',
              values_from = LC_intensity_peak_meta) %>%
  
  # join peak reference intensities for each slice
  left_join(
    data_ref_intensity %>%
      group_by(label_subject, label_session, z) %>%
      summarize(ref_peak_meta = max(value, na.rm = TRUE)),
    by = c('label_subject', 'label_session', 'z')
    
  ) %>%
  
  # for each hemisphere and z-slice, compute LC ratio
  mutate(LC_ratio_left = (LC_intensity_peak_meta_left - ref_peak_meta) / ref_peak_meta,
         LC_ratio_right = (LC_intensity_peak_meta_right - ref_peak_meta) / ref_peak_meta) %>%
  
  # average LC ratios over hemispheres
  mutate(LC_ratio_meta = (LC_ratio_left + LC_ratio_right) / 2)
