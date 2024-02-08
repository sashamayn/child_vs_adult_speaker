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
                                                          legend.text = element_text(size=20)) 

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
        legend.text = element_text(size=20))

avg_probs_per_tag_plot <- ggplot(data = tag_counts, aes(x=strategy,y=mean_prob,fill=strategy)) +   
  geom_bar(stat="identity",position="dodge") +
  geom_errorbar( aes(x=strategy, ymin=mean_prob-se_prob, ymax=mean_prob+se_prob), 
                 width=0.4,color="black")+
  facet_wrap(~speaker)  + xlab('strategy')+ylab('probability')+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),text = element_text(size=20),
        legend.text = element_text(size=20))

combined_tag_plot <- ggarrange(tag_count_plot, avg_probs_per_tag_plot, nrow=2,ncol=1)

#############################################

### plot of individual participants' target ratings (descending) ###

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

sorted_participant_plot <- ggplot(data=all_targetprobs_sorted,aes(x=nrow,y=mean_target,color=speaker))+geom_line()+geom_point()+
  xlab('participant (sorted)')+ylab('mean target probability')+
  theme(text = element_text(size=25),
        legend.text = element_text(size=25))

