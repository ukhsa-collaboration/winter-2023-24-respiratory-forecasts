# Script to create the MRF adjacency neighbour lists we want



# AGE
# define the groupings of age wanted by the `cut` function
# note - our args to `cut` are end *exclusive*
age_breakdowns <- c(0, 1, 2, 5, 18, 65, 75, 85, 120)
groups <- c()
for (i in 1:length(age_breakdowns)) {
  # don't want to add a version for the end element
  if (i != length(age_breakdowns)) {
    # format produced by `cut` function by default
    group <- glue::glue("[{age_breakdowns[i]},{age_breakdowns[i+1]})")
    groups <- append(groups, group)
  }
}

# generate age structure using adjacent age groups

age_nb <- list()

for (i in 1:length(groups)) {
  # left edge case
  if (i != 1) {
    # add previous age group
    age_nb[[groups[i]]] <- append(groups[i - 1], age_nb[[groups[i]]])
  }
  # right edge case
  if (i != length(groups)) {
    # add next age group
    age_nb[[groups[i]]] <- append(groups[i + 1], age_nb[[groups[i]]])
  }

}



# NHS region neighbours, easier (person in chair more efficient) than loading in whole shapefiles
# https://geoportal.statistics.gov.uk/documents/nhs-england-regions-july-2022-map-in-england/explore
nhs_nb <- list(
  "North West" = c("North East and Yorkshire", "Midlands"),
  "North East and Yorkshire" = c("North West", "Midlands"),
  "Midlands" = c("North West", "North East and Yorkshire", "South West", "South East", "East of England"),
  "South West" = c("South East", "Midlands"),
  "South East" = c("South West", "Midlands", "London", "East of England"),
  "London" = c("South East", "East of England"),
  "East of England" = c("London", "South East", "Midlands")
)
