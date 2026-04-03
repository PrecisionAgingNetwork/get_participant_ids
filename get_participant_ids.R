# This script has the function which pulls the list of participants included in the data freeze

# 03-April-26 update: added filter in query for consent form which filters out anyone 
                      # with 1 for wd_remove_data which means the participant wants their 
                      # data removed from the study

get_participant_ids <- function(freeze_date, connection){
  con <- connection
  
  beta_freeze_date <- as.Date("2024-12-31")
  freeze_date <- as.Date(freeze_date)
  
  # Load tables --------------------
  # Get consent dates
  p2_redcap_consent_form <- dbGetQuery(con, "SELECT hml_id, main_consent_date FROM p2_redcap_consent_form WHERE wd_remove_data = 0 OR wd_remove_data IS NULL") %>%
    mutate(main_consent_date = as.Date(main_consent_date))
  
  # Get HML ID creation dates
  info_hml_id_data <- dbGetQuery(con, "SELECT hml_id, hml_id_created_date FROM info_hml_id_data") %>%
    # One of the dates isn't in the right format
    mutate(hml_id_created_date = ifelse(hml_id_created_date == "9/5/24",
                                        as.Date("2024-09-05"),
                                        hml_id_created_date),
           hml_id_created_date = as.Date(hml_id_created_date))
  
  # Get people in biobank manifests
  info_biobank_manifests <- dbGetQuery(con, "SELECT hml_id, manifest_file FROM info_biobank_manifests") %>%
    # Get biobank shipment dates
    mutate(last_manifest_date = ifelse(str_detect(manifest_file, "April 2023"), "April 2023",
                                       str_extract(manifest_file, "[^_]*_[^_]*")),
           last_manifest_date = paste0("01 ", str_remove(last_manifest_date, "Manifest_")),
           last_manifest_date = as.Date(last_manifest_date, format = "%d %B %Y")) %>%
    # Filter manifests including or before the freeze date
    filter(last_manifest_date <= freeze_date) %>%
    # Get latest manifest date for each person
    slice_max(n = 1, by = hml_id, order_by = last_manifest_date, with_ties = F)
  
  # People consented before freeze date (people from first Dec 31 2024 freeze- soft launch, beta freeze)
  beta_freeze_dat_ids <- subset(p2_redcap_consent_form, main_consent_date <= beta_freeze_date)$hml_id
  
  # Put together list of participants in freeze:
  freeze_dat_ids <- info_hml_id_data %>%
    arrange(desc(hml_id)) %>%
    left_join(p2_redcap_consent_form, by = "hml_id") %>%
    left_join(select(info_biobank_manifests, hml_id, last_manifest_date), by = "hml_id") %>%
    mutate(in_biobank_manifests = ifelse(hml_id %in% info_biobank_manifests$hml_id, "yes", "no"),
           in_beta_freeze = ifelse(hml_id %in% beta_freeze_dat_ids, "yes", "no")) %>%
    # Filter people with consents
    filter(!is.na(main_consent_date)) %>%
    # Filter out people in manifests
    filter(in_biobank_manifests == "yes") %>%
    # Filter out people from beta freeze
    filter(!hml_id %in% beta_freeze_dat_ids) %>%
    # Filter people before freeze date end
    filter(hml_id_created_date < freeze_date) %>%
    # Add back in people from beta freeze
    select(hml_id) %>%
    rbind(data.frame(hml_id = beta_freeze_dat_ids))
  
  return(freeze_dat_ids$hml_id)
}

