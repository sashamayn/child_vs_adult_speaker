library(dplyr)
library(lme4)
library(lmerTest)
library(brms)
library(tibble)

setwd('../data/')

preprocess_data <- function(df) {
  df <- subset(df, df$condition != 'filler ambiguous')
  df$condition[df$condition == 'filler unambiguous'] <- 'control'
  df$condition <- factor(df$condition, levels = c('critical','control'))
  df$msgtype <- as.factor(ifelse(df$msg %in% c('re','gr'),'color','shape'))
  df$speaker <- factor(df$speaker, levels = c('adult','child'))
  mean_trialid <- mean(df$trialid)
  df$trialid_centered <- df$trialid - mean_trialid
  df$targetpos <- factor(df$targetpos, levels = c(2,1,3))
  
  contrasts(df$msgtype) = contr.sum(2)
  contrasts(df$speaker) = contr.sum(2)
  contrasts(df$condition) = contr.sum(2)
  
  return(df)
}

run_pp_check <- function(df,priors){
  m_brms_prior <- brm(
    prob_target ~ speaker*condition + trialid_centered + msgtype + targetpos + 
      (1+condition+msgtype+trialid_centered | participant_id) + (1 | itemid),
    data = df, prior = priors, family = gaussian, sample_prior = "only", 
    iter = 2000, warmup = 1000, chains = 4, cores = 4
  )
  
  return(m_brms_prior)
}

run_model <- function(df, priors) {
  m_brms <- brm(
    prob_target ~ speaker*condition + trialid_centered + msgtype + targetpos + 
      (1+condition+msgtype+trialid_centered | participant_id) + (1 | itemid),
    data = df, prior = priors, family = gaussian, iter = 4000, cores = 4
  )
  
  return(m_brms)
}

priors <- c(
  prior_string("normal(87.5,12.5)", class="Intercept", lb=0, ub=100),
  prior_string("normal(0,12.5)", class="b", coef="speaker1"), 
  prior_string("normal(-25,12.5)", class="b", coef="condition1"),
  prior_string("normal(0,12.5)", class="b", coef="speaker1:condition1"),
  prior_string("normal(0,5)", class="b", coef="trialid_centered"),
  prior_string("normal(0,5)", class="b", coef="msgtype1"),
  prior_string("normal(0,5)", class="b", coef="targetpos1"),
  prior_string("normal(0,5)", class="b", coef="targetpos3"),
  prior_string("student_t(3, 0, 12.5)", class = "sd", group = "participant_id"),
  prior_string("student_t(3, 0, 2.5)", class = "sd", group = "itemid"),
  prior_string("student_t(3, 0, 2.5)", class = "sigma"),
  prior_string("lkj(2)", class="cor")
)

### Experiment 1 ###
exp1_ids_toexclude <- read.csv('experiment 1/exp1_exclusion_list.csv')$participant_id
exp1_df <- read.csv('experiment 1/exp1_results.csv') %>% subset(!(participant_id %in% exp1_ids_toexclude))
exp1_df <- preprocess_data(exp1_df)

m_prior_exp1 <- run_pp_check(exp1_df,priors)
pp_check(m_prior_exp1, prefix = "ppd")
m_exp1 <- run_model(exp1_df,priors)

summary(m_exp1)
plot(m_exp1)


### Experiment 2 ###
exp2_ids_toexclude <- read.csv('experiment 2/exp2_exclusion_list.csv')$participant_id
exp2_df <- read.csv('experiment 2/exp2_results.csv') %>% subset(!(participant_id %in% exp2_ids_toexclude))
exp2_df <- preprocess_data(exp2_df)

m_prior_exp2 <- run_pp_check(exp2_df,priors)
pp_check(m_prior_exp2, prefix = "ppd")
m_exp2 <- run_model(exp2_df,priors)

summary(m_exp2)
plot(m_exp2)





