# this script computes peak LC metrics for each scan
# (used for ICC calculations)
# for the HRV-LC project
# written by shelby bachman, sbachman@usc.edu


# identify peak LC intensities --------------------------------------------

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
