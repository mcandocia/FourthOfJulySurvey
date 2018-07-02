library(plyr)
library(dplyr)
library(reshape2)
library(ggplot2)
library(cetcolor)
library(scales)
library(ape)
source('raking.r')
source('utility_functions.r')

devices = read.csv('devices.csv')
survey = read.csv('cleaned_survey.csv')

device_summary = devices %>%
  group_by(devices) %>%
  summarize(
    average=mean(devices_value),
    weighted_average=mean(devices_value*weight),
    political_weighted_average=mean(devices_value*political_weight),
    average_sigma = weighted.var.sigma(devices_value, rep(1, n())),
    weighted_sigma=weighted.var.sigma(devices_value, weight),
    political_weighted_sigma=weighted.var.sigma(devices_value, political_weight)
  ) %>%
  ungroup() %>%
  filter(
    devices != "I don't feel comfortable answering this question"
  ) %>%
  mutate(
    devices = factor(as.character(devices)),
    devices = factor(devices, levels=levels(devices)[order(average - 100*(devices=='None of the above'))])
  )

tall_device_summary = rbind(
  device_summary %>% transmute(devices=devices, average=average,
                               sigma=average_sigma, weight='unweighted'),
  device_summary %>% transmute(devices=devices, average=weighted_average, 
                               sigma=weighted_sigma, weight='weighted'),
  device_summary %>% transmute(devices=devices, average=political_weighted_average, 
                               sigma=political_weighted_sigma, weight='politically weighted')
) %>%
  mutate(
    label=ifelse(weight=='unweighted', paste0(percent(average), ' (n=', round(average* nrow(survey)), ')'), percent(average))
  )

ggplot(tall_device_summary) + geom_bar(aes(x=devices, y=average, fill=weight), stat='identity', position='dodge') +
  facet_grid(.~weight) + coord_flip() + 
  geom_errorbar(aes(x=devices, ymin=average-1.95*sigma, ymax=average+1.95*sigma, color=weight)) + 
  scale_fill_manual('weighting method', values=c('politically weighted'='red', 'unweighted'='white','weighted'='#4444FF')) + 
  theme_dark() + 
  scale_y_continuous(label=percent) + 
  geom_text(aes(x=devices,y=average, label=label), hjust='inward', fontface='bold') + 
  scale_color_manual('weighting method' ,values=c('politically weighted'='white', 'unweighted'='red','weighted'='white') ) +
  xlab('') + ylab('Estimated Proportion of US Population') + 
  ggtitle('Percentage of Firework Usage by Americans on Fourth of July', subtitle='n=137; different weighting methods used') + 
  theme(panel.background = element_rect(fill = "#CCCCCC"),
        panel.grid.major = element_line(colour = "#AAAAAA", size=0.25),
        panel.grid.minor = element_line(colour = "#AAAAAA", size=0.25),
        axis.text.y=element_text(size=rel(1.3))
        ) + 
  guides(fill=FALSE, color=FALSE)



#clustering
calculate_distance_matrix <- function(data, formula, value.var, sim.function=jaccard_similarity_){
  wide_matrix = acast(data, formula, value.var = value.var)
  distmat = 1 - outer(1:nrow(wide_matrix), 1:nrow(wide_matrix), 
                      FUN=Vectorize(cosine_similarity_, vectorize.args=c('i','j')), 
                      m=wide_matrix)
  rownames(distmat) = colnames(distmat) = rownames(wide_matrix)
  return(as.dist(distmat))
}

general_firework_stats = calculate_group_stats(devices %>% filter(devices != "I don't feel comfortable answering this question"), 'devices', group_variables='id')

firework_distance_matrix = calculate_distance_matrix(general_firework_stats, devices~id, value.var='prop', jaccard_similarity)

firework_clusters = hclust(firework_distance_matrix, method='ward.D2')
firework_cluster_labels = cutree(firework_clusters, 4)



plot(as.phylo(firework_clusters), 
     main="Clusters of Fireworks by Usage Among Individuals", 
     tip.color=c('red','blue','blue','red')[firework_cluster_labels])




ggplot(melt(as.matrix(1-as.matrix(firework_distance_matrix))) %>% 
         refactor_by_cluster(firework_clusters)) + 
  geom_tile(aes(x=Var1, y=Var2, fill=value)) + 
  scale_fill_gradientn('similarity\n', colors=cet_pal(9, 'coolwarm'),
                       limits = 0:1) + 
  xlab('') + ylab('') + 
  ggtitle('Similarity of  Fireworks by Usage Among Individuals') + 
  theme(axis.text.x = element_text(angle=90, hjust=1)) + 
  geom_text(aes(x=Var1, y=Var2, label=round(value,2)))
