library(here)
library(googlesheets4)


data_sheet_id <- "1nhIK803fCtdt3FAWqW4tiYPKdUqq77YYUOCMV1G9zPw"
demog_sheet_id <- "1CAZe0nxS3soDSK0uSn813LXrE4-RQNdClj17X-1esgo"


us_data <- read_sheet(data_sheet_id, sheet = "US")
cn_data <- read_sheet(data_sheet_id, sheet = "CN")
demog_data <- read_sheet(demog_sheet_id, sheet = "demog")


write_csv(us_data, here("data/00_raw_data/hand_coded/us_data.csv"))
write_csv(cn_data, here("data/00_raw_data/hand_coded/cn_data.csv"))
write_csv(demog_data, here("data/00_raw_data/hand_coded/demog_data.csv"))
