library('dplyr')
library('tidyr')
library('ggplot2')
library('ggpubr')
library('ggh4x')

setwd('../data/')

### PLOTTING FUNCTIONS ###
# plots average probability per condition (unambiguous, critical, ambiguous)
average_probability_plot <- function(df){
  se <- function(x){
    return(sd(x)/sqrt(length(x)))
  }
  
  df$condition <- df$itemtype
  df$condition[df$itemtype=='filler type a'] <- 'filler unambiguous'
  df$condition[df$itemtype=='filler type b'] <- 'filler unambiguous'
  
  avg_scores <- df %>% group_by(condition,speaker,experiment) %>% summarize(mean_target = mean(prob_target), 
                                                                 se_target = se(prob_target),
                                                                 mean_competitor = mean(prob_competitor), se_competitor = se(prob_competitor),
                                                                 mean_distractor = mean(prob_distr), se_distractor = se(prob_distr))
  
  p <- ggplot(data = avg_scores, aes(x = speaker, y = mean_target, color = condition, fill = condition)) + 
    geom_bar(stat="identity") +  geom_errorbar( aes(x=speaker, ymin=mean_target-se_target, ymax=mean_target+se_target), 
                                                width=0.4,color="black") +
    facet_grid(experiment~factor(condition,levels=c('filler unambiguous','critical','filler ambiguous')),axes = "all", axis.labels = "all_x") + 
    xlab('speaker') + ylab('average target rating') + theme_minimal()+
    theme(
      text = element_text(size = 16),  
      axis.title = element_text(size = 16),
      axis.text = element_text(size = 14),
      legend.text = element_text(size = 14),
      axis.ticks.y=element_blank()
    )
  
  return(p)
}

# plots the distribution of annotation tags and their relative probabilities
annotation_tag_plot <- function(df){
  se <- function(x){
    return(sd(x)/sqrt(length(x)))
  }
  
  targetprob_by_partic <- subset(df, df$itemtype == 'critical') %>% group_by(participant_id,speaker,experiment,strategy) %>% summarize(mean_target = mean(prob_target),
                                                                                                                                       sd_target = sd(prob_target))
  
  tag_counts <- targetprob_by_partic %>% group_by(speaker, strategy,experiment) %>% summarize(n=n(),
                                                                                              mean_prob = mean(mean_target), se_prob = se(mean_target)) %>% subset(strategy != 'changed_mind') %>% 
    group_by(speaker,experiment) %>% mutate(total_n = sum(n),prop= n/total_n)
  
  tag_counts$strategy <- factor(tag_counts$strategy,levels=c('correct_reasoning','guess','meta_reasoning','salience/preference','unclear'))
  
  tag_count_plot <- ggplot(data = tag_counts, aes(x = strategy, y = prop, fill = strategy)) + 
    ylim(0, 0.6) +
    facet_nested(~experiment+speaker) +
    geom_bar(stat = "identity", position = position_dodge(preserve = "single"),show.legend=FALSE) + 
    ylab('proportion of participants') +
    scale_x_discrete(drop = FALSE) +
    scale_fill_discrete(drop = FALSE) + 
    theme_bw() +
    labs(x = "") +
    theme(
      text = element_text(size = 16),  
      axis.title = element_text(size = 16),
      axis.text = element_text(size = 14),
      legend.text = element_text(size = 14),
      axis.text.x = element_blank(),
      axis.ticks.x=element_blank()
    )
  
  avg_probs_per_tag_plot <- ggplot(data = tag_counts, aes(x=strategy,y=mean_prob,fill=strategy)) +   
    geom_bar(stat="identity",position="dodge") +
    geom_errorbar( aes(x=strategy, ymin=mean_prob-se_prob, ymax=mean_prob+se_prob), 
                   width=0.4,color="black")+
    facet_nested(~experiment+speaker)+
    ylab('average target probability')+
    theme_bw()+
    labs(x = "") +
    theme(
      text = element_text(size = 16),  
      axis.title = element_text(size = 16),
      axis.text = element_text(size = 14),
      legend.text = element_text(size = 14),
      axis.text.x = element_blank(),
      axis.ticks.x=element_blank(),
      legend.position = "bottom",
      legend.spacing.x = unit(3, "cm"),  
      legend.box = "horizontal",
      legend.box.margin = margin(10, 0, 0, 0)
    )
  
  combined_tag_plot <- ggarrange(tag_count_plot, avg_probs_per_tag_plot, nrow=2,ncol=1)
 return(combined_tag_plot)
  
}

# plots average target ratings in the critical condition (descending) for individual participants 
sorted_partic_plot <-function(df){
  targetprob_by_partic <- subset(df, df$itemtype == 'critical') %>% group_by(participant_id,speaker,experiment,strategy) %>% 
    summarize(mean_target = mean(prob_target),sd_target = sd(prob_target))
  
  all_targetprobs_sorted <- targetprob_by_partic %>% group_by(speaker,experiment) %>%
    arrange(desc(mean_target)) %>%
    mutate(nrow = row_number()) %>%
    ungroup()
  
  all_targetprobs_sorted$speaker <- factor(all_targetprobs_sorted$speaker, levels=c('adult','child'))
  all_targetprobs_sorted$strategy <- factor(all_targetprobs_sorted$strategy,levels=c('correct_reasoning','meta_reasoning','guess','salience/preference','unclear','changed_mind'))
  
  sorted_plot <- ggplot(data=all_targetprobs_sorted,
                        aes(x=nrow,y=mean_target,color=speaker,shape=strategy,ymin=0,ymax=125))+
    geom_line(aes(group=speaker))+
    facet_wrap(~experiment,nrow=2,ncol=1,scales = "free_x")+
    geom_errorbar(aes(x=nrow, ymin=mean_target-sd_target, ymax=mean_target+sd_target,color=speaker), 
                  width=0.4,position = position_dodge(width = 0.5)) +
    geom_point(aes(size=3),position = position_dodge(width = 0.5))+
    xlab('participant (sorted)')+ylab('mean target probability')+
    scale_color_manual(values = c("adult" = "#336c52", 
                                  "child" = "#a32857"))+
    scale_shape_manual(values = c("correct_reasoning" = 16, "meta_reasoning" = 15, "guess" = 17, "salience/preference" =  18, "unclear" = 1, "changed_mind"=2))+
    theme_bw()+
    theme(
      text = element_text(size = 16),  
      axis.title = element_text(size = 16),
      axis.text = element_text(size = 14),
      legend.text = element_text(size = 14),
      axis.ticks.y=element_blank()
    )+
    guides(size="none")+
    guides(shape = guide_legend(override.aes = list(size = 5)),color = guide_legend(override.aes = list(size = 5)))
  
  return(sorted_plot)
}

# plot average ratings of individual participants including simulated data using mean and SD from each condition and experiment
sorted_partic_plot_wsim <-function(df){
  df <- refgame_df_both_exps
  targetprob_by_partic <- subset(df, df$itemtype == 'critical') %>% group_by(participant_id,speaker,experiment,strategy) %>% 
    summarize(mean_target = mean(prob_target),sd_target = sd(prob_target))
  
  all_targetprobs_sorted <- targetprob_by_partic %>% group_by(speaker,experiment) %>%
    arrange(desc(mean_target)) %>%
    mutate(nrow = row_number()) %>%
    ungroup()
  
  all_targetprobs_sorted$speaker <- factor(all_targetprobs_sorted$speaker, levels=c('adult','child'))
  all_targetprobs_sorted$strategy <- factor(all_targetprobs_sorted$strategy,levels=c('correct_reasoning','meta_reasoning','guess','salience/preference','unclear','changed_mind'))
  
  simulated_data <- all_targetprobs_sorted %>%
    group_by(experiment, speaker) %>%
    summarise(sim_mean = mean(mean_target), 
              sim_sd = mean(sd_target), 
              n = n(), 
              .groups = 'drop') %>%
    rowwise() %>%
    mutate(mean_target = list(rnorm(n, mean = sim_mean, sd = sim_sd))) %>%
    unnest(mean_target) %>%  
    group_by(experiment,speaker) %>%
    arrange(desc(mean_target)) %>%
    mutate(nrow = row_number()) %>%
    ungroup()
  
  all_targetprobs_sorted$type <- 'data'
  simulated_data$type <- 'simulated'
  simulated_data$strategy <- 'simulated'
  simulated_data$sd_target <- 0
  all_targetprobs_wsim <- rbind(all_targetprobs_sorted[,c('experiment','speaker','mean_target','sd_target','nrow','strategy','type')],
                                simulated_data[,c('experiment','speaker','mean_target','sd_target','nrow','strategy','type')])
  

  sorted_plot <- ggplot(data = all_targetprobs_wsim, aes(x = nrow, y = mean_target, color = speaker, shape = type, alpha=type)) +
    geom_line() +
    geom_errorbar(aes(x = nrow, ymin = mean_target - sd_target, ymax = mean_target + sd_target, color = speaker), 
                  width = 0.4, position = position_dodge(width = 0.5)) +
    geom_point(aes(size = 3), position = position_dodge(width = 0.5)) +
    facet_wrap(~experiment,nrow=2,ncol=1,scales = "free_x")+
    xlab('participant (sorted)') + 
    ylab('mean target probability') +
    scale_color_manual(values = c("adult" = "#336c52", "child" = "#a32857")) +
    scale_alpha_manual(values = c("data" = 1, "simulated" = 0.5)) +
    theme_bw()+
    theme(
      text = element_text(size = 16),  
      axis.title = element_text(size = 16),
      axis.text = element_text(size = 14),
      legend.text = element_text(size = 14),
      axis.ticks.y=element_blank()
    )+
    guides(size="none")+
    guides(shape = guide_legend(override.aes = list(size = 5)),color = guide_legend(override.aes = list(size = 5)))
  
  return(sorted_plot)
}

### PLOTS ###

exp1_ids_toexclude <- read.csv('experiment 1/exp1_exclusion_list.csv')$participant_id
exp1_refgame_df <- read.csv('experiment 1/exp1_results.csv') %>% subset(!(participant_id %in% exp1_ids_toexclude)) %>% mutate(experiment = "Experiment 1")
exp1_strategies <- read.csv('experiment 1/exp1_strategies.csv') %>% subset(!(participant_id %in% exp1_ids_toexclude))
exp1_refgame_df$strategy <- exp1_strategies$strategy_tag[match(exp1_refgame_df$participant_id,exp1_strategies$participant_id)]

exp2_ids_toexclude <- read.csv('experiment 2/exp2_exclusion_list.csv')$participant_id
exp2_refgame_df <- read.csv('experiment 2/exp2_results.csv') %>% subset(!(participant_id %in% exp2_ids_toexclude)) %>% mutate(experiment = "Experiment 2")
exp2_strategies <- read.csv('experiment 2/exp2_strategies.csv') %>% subset(!(participant_id %in% exp2_ids_toexclude))
exp2_refgame_df$strategy <- exp2_strategies$strategy_tag[match(exp2_refgame_df$participant_id,exp2_strategies$participant_id)]

refgame_df_both_exps <- rbind(exp1_refgame_df,exp2_refgame_df)

#### AVERAGE PROBABILITY PLOT ###
average_probability_plot <- average_probability_plot(refgame_df_both_exps)

### ANNOTATION TAG PLOT ###
combined_annotation_tag_plot <- annotation_tag_plot(refgame_df_both_exps)

### INDIVIDUAL PARTICIPANTS SORTED PLOT
combined_sorted_partic_plot <- sorted_partic_plot(refgame_df_both_exps)
  
#### INDIV WITH SIM -- MAKE INTO A FUNCTION ###
combined_sorted_partic_plot_wsim <- sorted_partic_plot_wsim(refgame_df_both_exps)
