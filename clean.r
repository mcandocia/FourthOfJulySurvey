library(plyr)
library(dplyr)
library(reshape2)
source('utility_functions.r')
source('raking.r')
source('population_constants.r')

survey = read.csv('combined_survey_data.csv', stringsAsFactors=FALSE)
survey$id=1:nrow(survey)

# clean demographics, firstly
survey_cleaned_demographics = survey %>%
  rename(
    age_group=What.is.your.age.group.,
    gender=What.is.your.gender.,
    is_hispanic=Are.you.Hispanic.Latino.a..,
    race=How.do.you.describe.your.race.,
    region=Which.region.of.the.US.do.you.live.in.
  ) %>%
  mutate(region=mapvalues(region,from='I do not live in the United States',to='Outside US')) %>%
  mutate(
    race=ifelse(grepl(';',race), 'Multiple Races', race),
    is_hispanic=ifelse(is_hispanic=='', 'No', is_hispanic) #made a mistake when making survey
  )

# add political support

survey_cleaned_political = survey_cleaned_demographics %>%
  rename(
    Democrat_party_opinion=What.is.your.opinion.of.the.current.Democratic.Party.,
    Republican_party_opinion=What.is.your.opinion.of.the.current.Republican.Party.,
    Donald_Trump_opinion=What.is.your.current.opinion.of.President.Donald.Trump.
  ) %>%
  mutate(
    trump_support=c('Disapprove','Disapprove','Neutral','Approve','Approve')[Donald_Trump_opinion],
    democrat_support=c('Disapprove','Disapprove','Neutral','Approve','Approve')[Democrat_party_opinion],
    republican_support=c('Disapprove','Disapprove','Neutral','Approve','Approve')[Republican_party_opinion]
  )

# merge three words question into one field

survey_cleaned = survey_cleaned_political %>%
  mutate(
    three_words=paste0(Describe.America.in.three.words,Describe.America.in.3.words)
  ) %>%
  rename(
    celebrates=Do.you.celebrate.the.Fourth.of.July.,
    watches_fireworks=Do.you.watch.firework.shows.on.the.4th.,
    celebration_location=Do.you.primarily.celebrate.indoors.or.outdoors.,
    what_do_you_wear=Which.of.the.following.do.you.usually.wear.plan.on.wearing.on.fourth.of.July.,
    devices=What.kinds.of.devices.do.you.typically.use.on.the.4th.of.July.
  ) %>%
  mutate(
    devices = gsub(' *\\([^)]+\\) *','', devices),
    what_do_you_wear = gsub(' *\\([^)]+\\) *','', what_do_you_wear),
    devices=mapvalues(devices, from='',to='None of the above'),
    what_do_you_wear = mapvalues(what_do_you_wear, from='', to='None of the above')
  )

# add weights

survey_weighted = survey_cleaned %>% 
  mutate(
    weight=rake_data(
      survey_cleaned, 
      names(general_demographic_weights), 
      general_demographic_weights,
      max_iter=25
    ),
    
    political_weight=rake_data(
      survey_cleaned, 
      names(political_demographic_weights), 
      political_demographic_weights,
      max_iter=25
    )
  )

write.csv(survey_weighted, 'cleaned_survey.csv', row.names=FALSE)

# split data

devices_frame = split_columns(
  survey_weighted,
  'devices'
) %>% 
  gather_category('devices_')

write.csv(devices_frame, 'devices.csv', row.names=FALSE)

clothing_frame = split_columns(
  survey_weighted,
  'what_do_you_wear'
) %>%
  gather_category('what_do_you_wear_')

write.csv(clothing_frame, 'clothing.csv', row.names=FALSE)

