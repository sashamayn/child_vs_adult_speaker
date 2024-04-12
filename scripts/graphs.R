library('dplyr')
library('tidyr')
library('ggplot2')
library('ggpubr')

df <- read.csv('../data/experiment_data.csv')
df <- rename(df, speaker = speaker_identity, condition = itemtype_coarser)
strategies <- read.csv('../data/strategies.csv')

se <- function(x) sd(x)/sqrt(length(x))

### average certainty rating by condition ###
avg_scores <- df %>% group_by(condition,speaker) %>% summarize(mean_target = mean(prob_target), 
                                   se_target = se(prob_target),
                                   mean_competitor = mean(prob_competitor), se_competitor = se(prob_competitor),
                                   mean_distractor = mean(prob_distr), se_distractor = se(prob_distr))

average_certainty_plot <- ggplot(data = avg_scores, aes(x = speaker, y = mean_target, color = condition, fill = condition)) + 
  geom_bar(stat="identity") +  geom_errorbar( aes(x=speaker, ymin=mean_target-se_target, ymax=mean_target+se_target), 
                                              width=0.4,color="black") +
  facet_wrap(~factor(condition,levels=c('filler unambiguous','critical','filler ambiguous'))) + 
  xlab('speaker type') + ylab('target certainty') + theme(text = element_text(size=20),
                                                            axis.text.x = element_text(size=20),
                                                          legend.text = element_text(size=20)) +
  theme_minimal()

#############################################

########### annotation tag plot ############
targetprob_by_partic <- subset(df, df$condition == 'critical') %>% group_by(participant_id) %>% summarize(mean_target = mean(prob_target),
                                                                                                    sd_target = sd(prob_target))
strategies$speaker <- df$speaker[match(strategies$participant_id,df$participant_id)]
strategies <- merge(strategies,targetprob_by_partic,by='participant_id')

tag_counts <- strategies %>% group_by(speaker, strategy_tag) %>% summarize(n=n(),
                                            mean_prob = mean(mean_target), se_prob = se(mean_target))

tag_counts <- subset(tag_counts, tag_counts$strategy_tag != 'exclude')
tag_counts <- rename(tag_counts, strategy = strategy_tag)

tag_count_plot <- ggplot(data=tag_counts, aes(x=strategy,y=n,fill=strategy)) +   
  geom_bar(stat="identity",position="dodge") +
  facet_wrap(~speaker)  + xlab('strategy')+ylab('count')+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),text = element_text(size=20),
        legend.text = element_text(size=20))+
  theme_minimal()

avg_probs_per_tag_plot <- ggplot(data = tag_counts, aes(x=strategy,y=mean_prob,fill=strategy)) +   
  geom_bar(stat="identity",position="dodge") +
  geom_errorbar( aes(x=strategy, ymin=mean_prob-se_prob, ymax=mean_prob+se_prob), 
                 width=0.4,color="black")+
  facet_wrap(~speaker)  + xlab('strategy')+ylab('probability')+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),text = element_text(size=20),
        legend.text = element_text(size=20))+
  theme_minimal()

combined_tag_plot <- ggarrange(tag_count_plot, avg_probs_per_tag_plot, nrow=2,ncol=1)

#############################################

### plot of individual participants' target ratings (descending) with simulated data ###

#############################################
targetprob_by_partic$speaker <- strategies$speaker[match(targetprob_by_partic$participant_id,
                                                         strategies$participant_id)]

adult_targetprobs <- subset(targetprob_by_partic,targetprob_by_partic$speaker =='adult')
adult_targetprobs_sorted <- adult_targetprobs[order(-adult_targetprobs$mean_target),]
adult_targetprobs_sorted$nrow <- seq.int(nrow(adult_targetprobs_sorted))

child_targetprobs <- subset(targetprob_by_partic,targetprob_by_partic$speaker =='child')
child_targetprobs_sorted <- child_targetprobs[order(-child_targetprobs$mean_target),]
child_targetprobs_sorted$nrow <- seq.int(nrow(child_targetprobs_sorted))

all_targetprobs_sorted <- rbind(adult_targetprobs_sorted,child_targetprobs_sorted)
all_targetprobs_sorted$speaker <- factor(all_targetprobs_sorted$speaker, levels=c('adult','child'))

#now simulate what the distribution would look like if it were unimodal for both children and adults
adult_mean <- mean(adult_targetprobs$mean_target)
adult_sd <- sd(adult_targetprobs$sd_target)
adult_samples <- rnorm(40, mean = adult_mean, sd = adult_sd)
adult_simulated_df <- as.data.frame(adult_samples)
names(adult_simulated_df)[names(adult_simulated_df) == 'adult_samples'] <- 'mean_target'
adult_simulated_df$speaker <- 'adult_sim'
adult_simulated_df$participant_id <- NA
adult_simulated_df$sd_target <- NA
adult_simulated_df_sorted <- adult_simulated_df[order(-adult_simulated_df$mean_target),]
adult_simulated_df_sorted$nrow <- seq.int(nrow(adult_simulated_df_sorted))

child_mean <- mean(child_targetprobs$mean_target)
child_sd <- sd(child_targetprobs$sd_target)
child_samples <- rnorm(40, mean = child_mean, sd = child_sd)
child_simulated_df <- as.data.frame(child_samples)
names(child_simulated_df)[names(child_simulated_df) == 'child_samples'] <- 'mean_target'
child_simulated_df$speaker <- 'child_sim'
child_simulated_df$participant_id <- NA
child_simulated_df$sd_target <- NA
child_simulated_df_sorted <- child_simulated_df[order(-child_simulated_df$mean_target),]
child_simulated_df_sorted$nrow <- seq.int(nrow(child_simulated_df_sorted))

all_targetprobs_sorted_with_sim <- rbind(all_targetprobs_sorted,adult_simulated_df_sorted,child_simulated_df_sorted)
all_targetprobs_sorted_with_sim$adult <- ifelse((all_targetprobs_sorted_with_sim$speaker == 'adult' | 
                                                   all_targetprobs_sorted_with_sim$speaker == 'adult_sim'),'a','c')
all_targetprobs_sorted_with_sim$type <- ifelse((all_targetprobs_sorted_with_sim$speaker == 'adult_sim' | 
                                                  all_targetprobs_sorted_with_sim$speaker == 'child_sim'),'simulated','data')

all_targetprobs_sorted_with_sim$speaker <- factor(all_targetprobs_sorted_with_sim$speaker,
                                                  levels=c('child_sim','adult_sim','child','adult'))
all_targetprobs_sorted_with_sim <- all_targetprobs_sorted_with_sim[order(all_targetprobs_sorted_with_sim$speaker), ]

sorted_plot_with_simulated <- ggplot(data=all_targetprobs_sorted_with_sim,
                                     aes(x=nrow,y=mean_target,color=speaker,shape=type))+
  geom_line()+
  geom_errorbar(aes(x=nrow, ymin=mean_target-sd_target, ymax=mean_target+sd_target,color=speaker), 
                width=0.4,position = position_dodge(width = 0.5)) +
  geom_point(aes(size=3),position = position_dodge(width = 0.5))+
  xlab('participant (sorted)')+ylab('mean target probability')+
  scale_color_manual(values = c("adult" = "#336c52", "adult_sim" = "#99b5a9", 
                                "child" = "#a32857", "child_sim" = "#c87e9a"))+
  theme(text = element_text(size=25),
        legend.text = element_text(size=25))+
  guides(size="none")+
  guides(shape = guide_legend(override.aes = list(size = 5)),color = guide_legend(override.aes = list(size = 5)))+
  theme_minimal()

