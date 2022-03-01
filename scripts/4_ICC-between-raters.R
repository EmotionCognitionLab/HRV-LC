# this script performs ICC analyses
# reflecting correspondence between peak LC intensities
# determined by each rater using the manual delineation method
# for the HRV-LC project
# written by shelby bachman, sbachman@usc.edu


# Note: because raters rate the same scans, ICC analyses use type = agreement

# perform ICC analysis between raters, left LC ----------------------------

icc_raters_leftLC <- irr::icc(data_LC_manual %>% select(LC_left_JL, LC_left_SA), 
                              type = "agreement",
                              model = "twoway")


# perform ICC analysis between raters, right LC ---------------------------

icc_raters_rightLC <- irr::icc(data_LC_manual %>% select(LC_right_JL, LC_right_SA), 
                               type = "agreement",
                               model = "twoway")


# perform ICC analysis between raters, by hemi & timepoint ----------------

icc_raters_leftLC_pre <- irr::icc(data_LC_manual %>% 
                               filter(label_session == 'ses-pre') %>%
                               select(LC_left_JL, LC_left_SA), 
                               type = "agreement",
                               model = "twoway")
icc_raters_rightLC_pre <- irr::icc(data_LC_manual %>% 
                                filter(label_session == 'ses-pre') %>%
                                select(LC_right_JL, LC_right_SA), 
                                type = "agreement",
                                model = "twoway")
icc_raters_leftLC_post <- irr::icc(data_LC_manual %>% 
                                filter(label_session == 'ses-post') %>%
                                select(LC_left_JL, LC_left_SA), 
                                type = "agreement",
                                model = "twoway")
icc_raters_rightLC_post <- irr::icc(data_LC_manual %>% 
                                 filter(label_session == 'ses-post') %>%
                                 select(LC_right_JL, LC_right_SA), 
                                 type = "agreement",
                                 model = "twoway")
