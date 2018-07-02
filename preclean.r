## this file is only run locally by survey creator, since potentially sensitive 
## information comes from different files and their respective timestamp fields

library(plyr)
library(dplyr)
library(reshape2)

DATA_DIRECTORY = 'data'

data_frame_list = list()

for (file in dir(DATA_DIRECTORY)){
  df = read.csv(file.path(DATA_DIRECTORY, file), stringsAsFactors=FALSE)
  
  # filter invalid entries out
  df = df %>% filter(What.is.your.age.group. != 'Under 18')
  df$Timestamp = NULL
  data_frame_list[[file]] = df
}

combined_data_frame = bind_rows(data_frame_list)
n = nrow(combined_data_frame)
combined_data_frame = combined_data_frame[sample(n, n),]

write.csv(combined_data_frame, 'combined_survey_data.csv', row.names=FALSE)
