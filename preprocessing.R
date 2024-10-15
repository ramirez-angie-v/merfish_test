library(rhdf5)
library(fst)
library(dplyr)
library(ggplot2)

setwd("Set/Local/Directory/jilab_merfish/jilab_merfish_dev")

# Path to master HDF5 file and read metadata fst
h5_file <- "./data/ns-atlas.merfish.annotated.skinny_updated.h5"
cell_meta_data_hs_skin_atlas <- fst::read_fst("./data/ns-atlas.merfish.annotated.skinny_metadata.fst") 

# List all groups and datasets within the file
original_h5_contents <- h5ls(h5_file)
print(original_h5_contents)


# -------------------------------------------------------- # 
# ----- Summary of the data available for each donor ----- # 

# Get the unique donors and order them
all_donors <- sort(unique(cell_meta_data_hs_skin_atlas$donor_id))

# Initialize an empty data frame to store the results
all_donors_summary <- data.frame(Donor = character(),
                            Num_Anatomical_Sites = integer(),
                            Anatomical_Sites = character(),
                            stringsAsFactors = FALSE)

# Loop over each donor
for (donor in all_donors) {
  # Filter metadata by donor
  donor_metadata <- cell_meta_data_hs_skin_atlas %>% filter(donor_id == donor)
  # Get the unique anatomical sites for this donor
  donor_anatomical_sites <- unique(donor_metadata$anatomic_site)
  # Create a row for this donor
  donor_row <- data.frame(Donor = donor,
                          Num_Anatomical_Sites = length(donor_anatomical_sites),
                          Anatomical_Sites = paste(donor_anatomical_sites, collapse = ", "),
                          stringsAsFactors = FALSE)
  # Append the row to the summary data frame
  all_donors_summary <- rbind(all_donors_summary, donor_row)
}

print(all_donors_summary)
# write.csv(donor_summary, "./data/donor_anatomical_sites_summary.csv", row.names = FALSE)


# ----------------------------------------------------------------------- # 
# ----- Define anatomical sites and donors with 12 available sites  ----- # 

# Filter rows to have only donors with 12 sites
donors_with_12_sites <- all_donors_summary %>% filter(Num_Anatomical_Sites == 12)
donors <- sort(unique(donors_with_12_sites$Donor))

# Define the list of anatomical sites of interest
anatomical_sites <- c("antecubital fossa", "postauricular", "central scalp", "occipital scalp", 
                      "knee", "abdomen", "elbow", "inguinal fold", "back", "buttocks", 
                      "popliteal fossae", "sole")

# ------------------------------------------------------------------------------------------ # 
# ----- Create directories for each donor and metadata files filtered for each patient ----- # 

# donor <- "D149"
# site <- "antecubital fossa"

# Iterate over each donor
for (donor in donors) {
  
  # Define the directory for the current donor
  donor_dir <- paste0("./data/", donor, "/")
  
  # Check if the directory exists, if not, create it
  if (!dir.exists(donor_dir)) {
    dir.create(donor_dir, recursive = TRUE)
  }
  
  # Filter the metadata for the current donor
  donor_metadata <- cell_meta_data_hs_skin_atlas %>% filter(donor_id == donor)
  
  # Define the output path for the donor's metadata file
  metadata_file <- paste0(donor_dir, donor, "_human_skin_merfish_atlas_skinny_metadata.fst")
  
  # Write the filtered metadata to the .fst file
  write_fst(donor_metadata, metadata_file)
  print(paste("Metadata for donor", donor, "saved to", metadata_file))
}

print("All donor directories and metadata files have been created.")

