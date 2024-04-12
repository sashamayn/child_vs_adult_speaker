library(dplyr)
library(lme4)
library(lmerTest)

adult_vs_child_df <- read.csv('../data/experiment_data.csv')

adult_vs_child_bypartic <- adult_vs_child_df %>% subset(itemtype=='critical') %>% 
  group_by(participant_id,speaker_identity) %>% summarize(mean_target = mean(prob_target))

adult_vs_child_df <- subset(adult_vs_child_df, adult_vs_child_df$itemtype_coarser != 'filler ambiguous')

adult_vs_child_df$condition <- adult_vs_child_df$itemtype_coarser
adult_vs_child_df$condition[adult_vs_child_df$itemtype_coarser == 'filler unambiguous'] <- 'control'
adult_vs_child_df$condition <- factor(adult_vs_child_df$condition, levels = c('critical','control'))

adult_vs_child_df$msgtype <- as.factor(ifelse(adult_vs_child_df$msg %in% c('re','gr'),'color','shape'))
adult_vs_child_df$speaker_identity <- factor(adult_vs_child_df$speaker_identity, levels = c('adult','child'))
mean_trialid <- mean(adult_vs_child_df$trialid)
adult_vs_child_df$trialid_centered <- adult_vs_child_df$trialid - mean_trialid
adult_vs_child_df$targetpos <- factor(adult_vs_child_df$targetpos, levels = c(2,1,3))

contrasts(adult_vs_child_df$speaker_identity) = contr.sum(2)
contrasts(adult_vs_child_df$condition) = contr.sum(2)

model_adult_vs_child <- lmer(prob_target ~ speaker_identity + condition + speaker_identity:condition + 
                             trialid_centered + msgtype + targetpos + (1 + trialid_centered | participant_id), 
                             data = adult_vs_child_df, control=lmerControl(optimizer="bobyqa"))

summary(model_adult_vs_child)


