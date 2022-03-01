# this script calculates participant summaries & performs exclusions
# for the HRV-LC project
# written by shelby bachman, sbachman@usc.edu


# IDs missing pre- or post-training scans ---------------------------------

subs_onescan <- data_inclexcl %>%
  filter(completed_MRI == 1) %>%
  group_by(label_subject, age_group) %>%
  summarize(n_sessions = n()) %>%
  filter(n_sessions == 1)

subs_onlypre <- data_inclexcl %>%
  filter(completed_MRI == 1) %>%
  filter(label_subject %in% subs_onescan$label_subject) %>%
  filter(label_session == 'ses-pre') %>%
  group_by(age_group) %>%
  summarize(n = n())

subs_onlypost <- data_inclexcl %>%
  filter(completed_MRI == 1) %>%
  filter(label_subject %in% subs_onescan$label_subject) %>%
  filter(label_session == 'ses-post') %>%
  group_by(age_group) %>%
  summarize(n = n())


# IDs with pre-/post-training LC contrast values --------------------------

subs_LC <- data_inclexcl %>%
  filter(completed_MRI == 1) %>%
  filter(has_LCcontrast == 1) %>%
  group_by(label_subject, age_group, condition) %>%
  summarize(n_sessions = n()) %>%
  filter(n_sessions == 2)


# IDs with pre-/post-training PAX values ----------------------------------

subs_PAX <- data_inclexcl %>%
  filter(has_PAX == 1) %>%
  group_by(label_subject, age_group, condition) %>%
  summarize(n_sessions = n()) %>%
  filter(n_sessions == 2)


# IDs with pre-/post-training PAX OR LC values ----------------------------

subs_LC_PAX <- union(subs_LC, subs_PAX)


# IDs with pre-/post-training PAX AND LC values ---------------------------

subs_LC_and_PAX <- intersect(subs_LC, subs_PAX)


# restrict data to include only IDs with pre- and post- LC ----------------

# to include only participants with:
# pre- and post-training MRI
# high-quality FSE (i.e. included in LC pipeline)
# LC contrast value available (i.e. not excluded after LC pipeline)

data_LC_manual <- data_LC_manual %>%
  filter(label_subject %in% subs_LC$label_subject)
data_LC_intensity_meta <- data_LC_intensity_meta %>%
  filter(label_subject %in% subs_LC$label_subject)
data_ref_intensity <- data_ref_intensity %>%
  filter(label_subject %in% subs_LC$label_subject)


# compute correspondence between visual qc and manual rateability -------------

data_qc <- data_inclexcl %>%
  filter(completed_MRI == 1) %>%
  # create new variable that aggregates across raters
  # with value = 1 if one or both raters delineated the LC on the scan,
  # and with value = 0 if neither rated delineated the LC on the scan
  mutate(rated_agg = ifelse(LC_manually_rated_by_rater1 == 1 & LC_manually_rated_by_rater2 == 1, 1,
                            ifelse(LC_manually_rated_by_rater1 == 1 & LC_manually_rated_by_rater2 == 0, 1, 
                                   ifelse(LC_manually_rated_by_rater1 == 0 & LC_manually_rated_by_rater2 == 1, 1, 
                                          ifelse(LC_manually_rated_by_rater1 == 0 & LC_manually_rated_by_rater2 == 0, 0, NA))))) %>%
  # only perform calculation for cases 
  # which trained researcher did not identify as having: 
  # different resolution, incorrect scan positioning, susceptibility artifact
  # (manual raters were not trained to look for these)
  filter(!exclude_LCpipeline_reason %in% c('different FSE resolution', 'incorrect scan positioning', 'susceptibility artifact overlapping LC'))

# report proportion of cases in agreement
corr_visualqc_manualrating <- sum(data_qc$include_LCpipeline == data_qc$rated_agg, na.rm = TRUE)/nrow(data_qc)
rm(data_qc)


# table 1: demographics ---------------------------------------------------

table1 <- data_demo_affect_cog %>%
  filter(label_subject %in% subs_LC_PAX$label_subject) %>%
  group_by(age_group, condition) %>%
  summarize(n = n(),
            n_female = format_n_pct(sum(gender == 'female'), sum(gender == 'female')/n),
            age_meansd = format_mean_sd(mean(age), sd(age)),            
            age_range = format_range(min(age), max(age)),
            edu_meansd = format_mean_sd(mean(edu), sd(edu)),
            edu_range = format_range(min(edu), max(edu))) %>%
  arrange(desc(age_group), 
          desc(condition))

table1$age_group[table1$age_group == 'YA'] <- 'Younger'
table1$age_group[table1$age_group == 'OA'] <- 'Older'

names(table1) <- c('Age group', 'Condition', 'N', 'N (%) Female', 
                   'Age, mean (SD)', 'Age, range',
                   'Edu, mean (SD)', 'Edu, range')

