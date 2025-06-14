rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(ggplot2)
library(Rmisc)
library(readxl)
library(performance)
library(lattice)
library(sjPlot)
library(ggpubr)
library(EMAtools)
library(sjPlot)
library(rstatix)
library(purrr)
library(readxl)
library(tidyr)
library(dplyr)
library(ggstatsplot)

load("dataRL.Rda")
load('cluster_id.rda')

# model fitting -----------------------------------------------------------
source('create_data.R')

source ("0_RWmodel.r")
source("model_coupling_decay_corrected.R")
source("model_coupling_decay_URI_orig.R")
meeting_directoty = "G:/Shared drives/AdmonPsy - Chronic Stress Project/RL analysis/meetings/13-02-2024"
prefix = "/final_params_set"

data_unfiltered = create_measures()
tmp$subject=tmp$participant

# change to true if re-estimation is needed 
run_param_estimation = F

  #get number of subjects participated in the experiment
  num_subjects <- length(unique(data$subject))
  subj_names = unique(data$subject)
  if (run_param_estimation){
    
  # BIC function
  BIC = function(ll, NumTrials, NumParam) {
    log(NumTrials) * NumParam + 2 * ll
  }
  
  # AIC function
  AIC = function(ll, NumTrials, NumParam) {
    2* NumParam + 2 * ll
  }
  
  nitr =100 
  set.seed(101) # for reproducibility 
  
  # initiate data frame: 
  par_rw_all = data.frame()
  par_couplling_decay_all = data.frame()
  bic_sub_all = data.frame()
  aic_sub_all = data.frame()
  ll_sub_all = data.frame()
  
  #per each subject
  for (itr in 1:nitr){
    
    #model fitting initiations
    par_rw <- setNames(data.frame(matrix(ncol =  2, nrow = num_subjects)), c('beta','alpha'))
    par_couplling_decay <- setNames(data.frame(matrix(ncol =  4 ,nrow = num_subjects)), c('beta','alpha_current','alpha_gen', 'coupling_decay'))

    bic_sub <- setNames(data.frame(matrix(ncol = 2, nrow = num_subjects)), c('RW','Coupling_decay'))
    ll_sub <- setNames(data.frame(matrix(ncol = 2, nrow = num_subjects)), c('RW','Coupling_decay'))
    aic_sub <- setNames(data.frame(matrix(ncol = 2, nrow = num_subjects)), c('RW','Coupling_decay'))
    
    for (i in 1:num_subjects){
      print(i)
      subject_choices <- data %>% filter(subject == subj_names[i])
      
      #fit models per each subject, initial values beta=1, alpha=0.5
      resultRW = optim(par = c(5, rbeta(1,0.4,0.4)), RWmodel, data = subject_choices)
      resultsCoupling_decay = optim(par = c(4,rbeta(4,0.4,0.4,1)), Gen_coup_dec_model, data = subject_choices)
      
      
      #aggregate individual parameters beta and alpha(s)
      par_rw[i,] = resultRW$par
      # par_couplling[i, ] = resultsCoupling$par
      
      par_couplling_decay[i, ] = resultsCoupling_decay$par
      
      #aggregate BIC scores
      bic_sub$RW[i] = BIC(resultRW$value, length(subject_choices$subject), 2)
      # bic_sub$Coupling[i] =BIC(resultsCoupling$value, length(subject_choices$subject), 4) #log likelihood value,num of trials, num of params
      bic_sub$Coupling_decay[i] =BIC(resultsCoupling_decay$value, length(subject_choices$subject), 5) #log likelihood value,num of trials, num of params
      
      #aggregate AIC scores
      aic_sub$RW[i] = AIC(resultRW$value, length(subject_choices$subject), 2)
      # aic_sub$Coupling[i] =AIC(resultsCoupling$value, length(subject_choices$subject), 4) #log likelihood value,num of trials, num of params
      aic_sub$Coupling_decay[i] =AIC(resultsCoupling_decay$value, length(subject_choices$subject), 5) #log likelihood value,num of trials, num of params
      
      #aggregate BIC scores
      ll_sub$RW[i] = resultRW$value
      # ll_sub$Coupling[i] =resultsCoupling$value 
      ll_sub$Coupling_decay[i] =resultsCoupling_decay$value 
      
    }
    
    bic_sub$subject=subj_names
    ll_sub$subject=subj_names
    par_couplling_decay$subject=subj_names
    par_rw$subject = subj_names
    aic_sub$subject = subj_names
    
    # add iter num and aggregate 
    bic_sub$itr=itr
    ll_sub$itr=itr
    par_couplling_decay$itr=itr
    par_rw$itr = itr
    
    par_rw_all = rbind(par_rw_all, par_rw)
    par_couplling_decay_all = rbind(par_couplling_decay_all, par_couplling_decay )
    bic_sub_all = rbind(bic_sub_all,bic_sub)
    aic_sub_all = rbind(aic_sub_all,aic_sub)
    ll_sub_all = rbind(ll_sub_all,ll_sub)
    
    # save results of the for loop 
    save(resultRW, resultsCoupling_decay, par_rw, par_couplling_decay, bic_sub, aic_sub, ll_sub,
         par_rw_all,par_couplling_decay_all, bic_sub_all,ll_sub_all,aic_sub_all,  
         file = "params.Rda")
    
    
  }
}else{
  load("params.Rda") 
}
  

# check ll before averaging model parameters. if subject's ll deviates 7 points - omit this iteration
ggplot(ll_sub_all, aes(x = itr, y = Coupling_decay, group = subject)) + geom_line()
ggplot(ll_sub_all, aes(x = itr, y = RW, group = subject)) + geom_line()

ll_sub_all$Coupling_decay_pcenter = pcenter(ll_sub_all$subject,ll_sub_all$Coupling_decay) 
ggplot(ll_sub_all, aes(x = itr, y = Coupling_decay_pcenter, group = subject)) + geom_line()

subj_extreme <- ll_sub_all %>%filter(Coupling_decay_pcenter >7 )
extreme = unique(subj_extreme$subject )
ggplot(ll_sub_all %>% filter(subject %in% extreme) , aes(x = itr, y = Coupling_decay_pcenter, color = subject)) + geom_line()

itr_to_remove = ll_sub_all$Coupling_decay_pcenter >7

par_rw = aggregate(par_rw_all, list(subject = par_rw_all$subject), mean) 
par_rw<- janitor::remove_empty(par_rw, which = "cols")
par_couplling_decay = aggregate(par_couplling_decay_all[!itr_to_remove,], list( subject = par_couplling_decay_all$subject[!itr_to_remove]), mean) 
par_couplling_decay<- janitor::remove_empty(par_couplling_decay, which = "cols")

bic_sub = aggregate(bic_sub_all[!itr_to_remove,], list(subject = bic_sub_all$subject[!itr_to_remove]), mean) 
bic_sub<- janitor::remove_empty(bic_sub, which = "cols")

aic_sub = aggregate(aic_sub_all[!itr_to_remove,], list(subject = aic_sub_all$subject[!itr_to_remove]), mean) 
aic_sub<- janitor::remove_empty(aic_sub, which = "cols")

ll_sub = aggregate(ll_sub_all[!itr_to_remove,], list(subject = ll_sub_all$subject[!itr_to_remove]), mean) 
ll_sub<- janitor::remove_empty(ll_sub, which = "cols")


#t test to compare BICs, see if significant
t_aic = t.test(aic_sub$RW, aic_sub$Coupling_decay, paired = TRUE, alternative = "two.sided")

#make the columns to be rows to benefit from pipes
aic_sub_gathered <- gather(aic_sub, key = 'model', value ='aic',factor_key=TRUE, -subject)

# Model Comparison - calculate mean of bic and sem per each model
mean_aic <-
  aic_sub_gathered %>% dplyr::group_by(model) %>% dplyr::summarise(mean = mean(aic), sem = sd(aic) / sqrt(num_subjects))
mean_performance = ggplot(mean_aic, aes(x = model, y = mean, fill = model)) +
  geom_errorbar(aes(ymin = mean - sem, ymax = mean + sem), width = 0.2) +
  geom_point(shape = 21, size = 10) +
  labs(title = paste("Models Comparison p =", format(t_aic$p.value, digits = 2))) + 
  scale_x_discrete(name="Model")+
  scale_y_continuous(name = "AIC",limits = c(100, 160))+
  theme(legend.title = element_blank())

svg(filename = paste0(meeting_directoty,prefix,'_model_comparison.svg'), height = 5, width = 5)
print(mean_performance)
dev.off()
aic_sub$best = factor(ifelse(aic_sub$Coupling_decay  - aic_sub$RW < 0,1,0 ))

# plot final parameters distribution - coupling decay model
params = ggplot(par_couplling_decay%>% pivot_longer(cols = c('beta','alpha_current','alpha_gen', 'coupling_decay'),names_to = 'parameter',values_to = 'value')
                %>%filter(parameter!='
                 beta'),
                aes(x=value))+
  facet_wrap(~parameter, scales = "free")+
  geom_histogram()
svg(filename = paste0(meeting_directoty,prefix,'_params_hist.svg'))
print(params)
dev.off()

# plot final parameters distribution - RW model
params_RW = ggplot(par_rw%>%pivot_longer(cols = c('beta','alpha'),names_to = 'parameter',values_to = 'value')
                   %>%filter(parameter!='
                 beta'),
                   aes(x=value))+
  facet_wrap(~parameter, scales = "free")+
  geom_histogram()
svg(filename = paste0(meeting_directoty,prefix,'_params_hist_RW.svg'))
print(params_RW)
dev.off()

# plot individual AIC values - color by which model best fit the data
aic_sub = left_join(aic_sub, tmp, by = c("subject" = "participant"))
aic_sub$cluster = factor(aic_sub$cluster)
ind_aic<-ggplot(aic_sub,aes(x=Coupling_decay,y=RW, color = best))+
  geom_point()+
  geom_abline(slope=1,intercept=0) + 
  scale_x_continuous(limits = c(40,180))+
  scale_y_continuous(limits = c(40,180))

svg(filename = paste0(meeting_directoty,prefix,'_ind_aic.svg'))
print(ind_aic)
dev.off()

##################################
data<-merge(data,par_couplling_decay,by='subject')


data2<-data%>%dplyr::group_by(subject,condition)%>%dplyr::mutate(trial=row_number())

data2<-data2%>%mutate(GenAlpha=ifelse(alpha_gen<median(data$alpha_gen),'LowG','HighG'))
data2<-data2%>%mutate(LR=ifelse(alpha_current<median(data$alpha_current),'LowLR','HighLR'))
data2<-data2%>%mutate(DecayRate=ifelse(coupling_decay<median(data$coupling_decay),'LowDR','HighDR'))
par_rw$alpha_rw = par_rw$alpha
par_rw$beta_rw = par_rw$beta
par_rw$beta = NULL 

data2 <- merge(data2, par_rw,by='subject')
data2<-data2%>%mutate(alphaRW=ifelse(alpha_rw<median(data2$alpha_rw),'LowAlphaRW','HighAlphaRW'))

# create and omit first block 
ntrial_per_subject =  data2 %>% group_by(subject) %>% summarize(n = n())
ntrial_per_subject$extraBlock = ifelse(ntrial_per_subject$n == 140, 1,0) 

data2 <- full_join(data2,ntrial_per_subject)

data2a <- data2 %>% filter(extraBlock == 1)
data2a <- data2a%>% filter(trial >10)
data2a$trial = data2a$trial-10
data2b <- data2 %>% filter(extraBlock == 0)

data2c <- rbind(data2a, data2b) %>% ungroup()

#alpha_gen_plot
alpha_gen_plot <- ggplot(data2c%>%filter(trial<60),aes(x=trial,y=choice,group = as.factor(condition),color=as.factor(condition),fill=as.factor(condition)))+
  facet_wrap(~GenAlpha)+
  stat_summary(fun.data="mean_cl_boot",position=position_dodge(width=0),  
               geom="ribbon",color=NA,alpha=0.5) +
  stat_summary(fun="mean",position=position_dodge(width=0.1), 
               geom="line" ,size=2  ) +
  labs(title=paste0('Choices- alpha gen ',round(median(data$alpha_gen), digits = 3)),x='trial',y='choice')+
  theme_classic() 

# LR plot 
LR_plot <- ggplot(data2c%>%filter(trial<60),aes(x=trial,y=choice,group = as.factor(condition),color=as.factor(condition),fill=as.factor(condition)))+
  facet_wrap(~LR)+
  stat_summary(fun.data="mean_cl_boot",position=position_dodge(width=0),  
               geom="ribbon",color=NA,alpha=0.5) +
  stat_summary(fun="mean",position=position_dodge(width=0.1), 
               geom="line" ,size=2  ) +
  labs(title=paste0('Choices- alpha current ',round(median(data$alpha_current), digits = 3)),x='trial',y='choice')+
  theme_classic() 



DecayRate_plot <- ggplot(data2c%>%filter(trial<60),aes(x=trial,y=choice,group = as.factor(condition),color=as.factor(condition),fill=as.factor(condition)))+
  facet_wrap(~DecayRate)+
  stat_summary(fun.data="mean_cl_boot",position=position_dodge(width=0),  
               geom="ribbon",color=NA,alpha=0.5) +
  stat_summary(fun="mean",position=position_dodge(width=0.1), 
               geom="line" ,size=2  ) +
  labs(title=paste0('Choices- coupling decay ',round(median(data$coupling_decay), digits = 3)),x='trial',y='choice')+
  theme_classic() 




# save parameters plot 
params_behav_plots = ggarrange(alpha_gen_plot, LR_plot, DecayRate_plot, ncol = 2, nrow = 2)
svg(filename = paste0(meeting_directoty,prefix,'_params_behav.svg'), height = 7, width = 17)
print(params_behav_plots)
dev.off()


# RW alpha plot : 

AlphaRW_plot <- ggplot(data2c%>%filter(trial<60),aes(x=trial,y=choice,group = as.factor(condition),color=as.factor(condition),fill=as.factor(condition)))+
  facet_wrap(~alphaRW)+
  stat_summary(fun.data="mean_cl_boot",position=position_dodge(width=0),  
               geom="ribbon",color=NA,alpha=0.5) +
  stat_summary(fun="mean",position=position_dodge(width=0.1), 
               geom="line" ,size=2  ) +
  labs(title=paste0('Choices- Alpha RW ',round(median(data2$alpha_rw), digits = 3)),x='trial',y='choice')+
  theme_classic() 

svg(filename = paste0(meeting_directoty,prefix,'_params_behav_RW.svg'), height = 9, width = 16)
print(AlphaRW_plot)
dev.off()

counts = data2c %>% group_by(DecayRate,GenAlpha) %>% summarise(groupSize = n()/120) 

a <- ggplot(data2c%>%filter(trial<60),aes(x=trial,y=choice,group = as.factor(condition),color=as.factor(condition),fill=as.factor(condition)))+
  facet_wrap(~DecayRate*GenAlpha)+
  stat_summary(fun.data="mean_cl_boot",position=position_dodge(width=0),
               geom="ribbon",color=NA,alpha=0.5) +
  stat_summary(fun="mean",position=position_dodge(width=0.1),
               geom="line" ,size=2  ) +
  labs(title=paste0('Choices- coupling_decay = ', round(median(data2c$coupling_decay),digits = 3),
                    ' & median alpha gen = ',round(median(data2c$alpha_gen), digits = 3)),x='trial',y='choice')+
  theme_classic()

svg(filename = paste0(meeting_directoty,prefix,'_params_behav_plots_intersection1.svg'), height = 7, width = 17)
print(a)
dev.off()


counts = data2c %>% group_by(DecayRate,LR) %>% summarise(groupSize = n()/120) 

b <- ggplot(data2c%>%filter(trial<60),aes(x=trial,y=choice,group = as.factor(condition),color=as.factor(condition),fill=as.factor(condition)))+
  facet_wrap(~DecayRate*LR)+
  stat_summary(fun.data="mean_cl_boot",position=position_dodge(width=0),
               geom="ribbon",color=NA,alpha=0.5) +
  stat_summary(fun="mean",position=position_dodge(width=0.1),
               geom="line" ,size=2  ) +
  labs(title=paste0('Choices- coupling_decay = ', round(median(data2c$coupling_decay),digits = 3),
                    ' & alpha current = ',round(median(data2c$alpha_current), digits = 3)),x='trial',y='choice')+
  theme_classic()

svg(filename = paste0(meeting_directoty,prefix,'_params_behav_plots_intersection2.svg'), height = 7, width = 17)
print(b)
dev.off()

data2c <- left_join(data2c, aic_sub)
data2c$best <- as.numeric(data2c$best)-1
# 
m=lm(best~coupling_decay+alpha_gen+alpha_current   +beta,data=data2c%>%group_by(subject)%>%
       summarise(coupling_decay=first(coupling_decay),beta=first(beta),alpha_current=first(alpha_current),alpha_gen=first(alpha_gen),best=first(best)))
tab_model(m)


######################### plot parameters and clusters 
data2<-merge(data2,tmp,by='subject')
mean(data2$alpha_gen[data2$cluster==1])
mean(data2$alpha_gen[data2$cluster==2])


t.test(alpha_gen~cluster,data=data2%>%group_by(subject)%>%
         summarise(alpha_gen=first(alpha_gen),cluster=first(cluster)))

aa <- ggplot(data=data2%>%group_by(subject)%>%
         summarise(alpha_gen=first(alpha_gen),cluster=first(cluster)),
       aes(x=alpha_gen,group=cluster,fill=as.factor(cluster)))+
  geom_histogram(alpha = 0.5,position = 'identity') + 
  scale_y_continuous(expand = c(0,0)) + 
  scale_x_continuous(expand = c(0,0), limits = c(0,1), labels = c("0","0.25", "0.5", "0.75", "1"))+
  scale_fill_discrete(labels = c("non-flex", "flex")) + 
  theme_classic() +
  theme(legend.title = element_blank(),
        text = element_text(face="bold", size = 20),
        legend.text = element_text(size = 24))



t.test(coupling_decay~cluster,data=data2%>%group_by(subject)%>%
         summarise(coupling_decay=first(coupling_decay),cluster=first(cluster)))

bb <- ggplot(data=data2%>%group_by(subject)%>%
         summarise(coupling_decay=first(coupling_decay),cluster=first(cluster)),
       aes(x=coupling_decay,group=cluster,fill=as.factor(cluster)))+
  geom_histogram(alpha = 0.5,position = 'identity') + 
  scale_y_continuous(expand = c(0,0)) + 
  scale_x_continuous(expand = c(0,0))+
  scale_fill_discrete(labels = c("non-flex", "flex")) + 
  theme_classic() +
  theme(legend.title = element_blank(),
        text = element_text(face="bold", size = 20),
        legend.text = element_text(size = 24))



t.test(alpha_current~cluster,data=data2%>%group_by(subject)%>%
         summarise(alpha_current=first(alpha_current),cluster=first(cluster)))

cc <- ggplot(data=data2%>%group_by(subject)%>%
         summarise(alpha_current=first(alpha_current),cluster=first(cluster)),
       aes(x=alpha_current,group=cluster,fill=as.factor(cluster)))+
  geom_histogram(alpha = 0.5,position = 'identity') + 
  scale_y_continuous(expand = c(0,0)) + 
  scale_x_continuous(expand = c(0,0), limits = c(0,1), labels = c("0","0.25", "0.5", "0.75", "1"))+
  scale_fill_discrete(labels = c("non-flex", "flex")) + 
  theme_classic() +
  theme(legend.title = element_blank(),
        text = element_text(face="bold", size = 20),
        legend.text = element_text(size = 24))




t.test(beta~cluster,data=data2%>%group_by(subject)%>%
         summarise(beta=first(beta),cluster=first(cluster)))

dd<- ggplot(data=data2%>%group_by(subject)%>%
         summarise(beta=first(beta),cluster=first(cluster)),
       aes(x=beta,group=cluster,fill=as.factor(cluster)))+
  geom_histogram(alpha = 0.5,position = 'identity') + 
  scale_y_continuous(expand = c(0,0)) + 
  scale_x_continuous(expand = c(0,0))+
  scale_fill_discrete(labels = c("non-flex", "flex")) + 
  theme_classic() +
  theme(legend.title = element_blank(),
        text = element_text(face="bold", size = 20),
        legend.text = element_text(size = 24))

params_plot_by_cluster <- ggarrange(aa, bb, cc, dd, labels=c("a)", "b)", "c)", "d)"), common.legend = TRUE,
                                    font.label = list(size = 22, face = "bold"))

svg(filename = paste0(meeting_directoty,prefix,'_params_hist_by_cluster.svg'), width = 9, height = 9)
print(params_plot_by_cluster)
dev.off()



data_cluster =data2%>%group_by(subject)%>%
  summarise(coupling_decay=first(coupling_decay),beta=first(beta),alpha_current=first(alpha_current),alpha_gen=first(alpha_gen),cluster=first(cluster))
data_cluster$cluster = data_cluster$cluster-1
data_cluster$alpha_current_Z = scale(data_cluster$alpha_current)[,1]
data_cluster$alpha_gen_Z = scale(data_cluster$alpha_gen) [,1]
data_cluster$coupling_decay_Z = scale(data_cluster$coupling_decay)[,1]
data_cluster$beta_Z = scale(data_cluster$beta)[,1]

mClust=glm(cluster~coupling_decay_Z+alpha_gen_Z+alpha_current_Z+beta_Z,data=data_cluster, family = "binomial")
mClust2=glm(cluster~coupling_decay*alpha_gen+alpha_current   +beta,data=data_cluster, family = "binomial")

data_cluster = left_join(data_cluster, par_rw, suffix = c("","rw"))
tab_model(mClust, mClust2, file = paste0(meeting_directoty,prefix,'table2_cluster&modelParams.doc'))
mean(data2$cluster[data2$GenAlpha=='LowG'])
mean(data2$cluster[data2$GenAlpha=='HighG'])
mean(data2$cluster[data2$GenAlpha=='HighG' & data2$DecayRate=='HighDR'])
mean(data2$cluster[data2$GenAlpha=='LowG' & data2$DecayRate=='HighDR'])
mean(data2$cluster[data2$GenAlpha=='HighG' & data2$DecayRate=='LowDR'])
mean(data2$cluster[data2$GenAlpha=='LowG' & data2$DecayRate=='LowDR'])

data_cluster$alpha_rw_Z = scale(data_cluster$alpha_rw)[,1]
data_cluster$beta_rw_Z = scale(data_cluster$beta_rw)[,1]

mClustRW=glm(cluster~alpha_rw_Z + beta_rw_Z,data=data_cluster, family = "binomial")
tab_model(mClustRW, file = paste0(meeting_directoty,prefix,'tableS2_cluster&modelParams_RW.doc'))


# declerative estimation: remember that we are omitting the first block 
df_est=  create_estimation_data() 
df_est$pair_type= ifelse(df_est$choice == "choice_1" | df_est$choice == "choice_2", 'nonReversed', 'reversed')
df_est2 = create_estimation_data(long = F) 
df_est2$nonReversed = df_est2$choice_1 - df_est2$choice_2
df_est2$reversed = df_est2$choice_3 - df_est2$choice_4
df_est2$choice_1 = NULL
df_est2$choice_2 = NULL
df_est2$choice_3 = NULL
df_est2$choice_4 = NULL

df_est2 = pivot_longer(df_est2, cols = c(nonReversed, reversed), names_to = "pair_type", values_to = "choice_a")

#omit non leraners:
df_est = df_est[df_est$participant %in% data2$subject,]
p_est = ggplot(df_est, aes(x = block, y = prob_estimation, group = choice, color = choice, fill = choice)) +
  facet_wrap(~pair_type) +
  stat_summary(fun.data="mean_cl_boot",position=position_dodge(width=0),geom = "ribbon", color=NA,alpha=0.5) +
  stat_summary(fun="mean",position=position_dodge(width=0.1), 
               geom="line" ,size=2  ) + 
  stat_summary(fun="mean",position=position_dodge(width=0.1),  geom="point" ,size=3 , color = "black" ) + 
  geom_hline(yintercept = 50,linetype = "dashed" )+
  labs(title = "what are the odds of recieveing reward when seeing this tree?")+
  theme_classic() + 
  theme(legend.position = "none")

svg(filename = paste0(meeting_directoty,prefix,'_declare_est.svg'), height = 7, width = 10)
print(p_est)
dev.off()


# plot as "choice a" 
df_est2 = df_est2[df_est2$participant %in% data2$subject,]

dat_summary3 <- summarySEwithin(df_est2,
                                measurevar = "choice_a",
                                withinvars = c("pair_type", "block"), 
                                idvar = "participant")

p_est =  ggplot(df_est2,aes(x=block,y=choice_a,group = as.factor(pair_type),color=as.factor(pair_type),fill=as.factor(pair_type)))+
  scale_fill_manual(values = c("#9c68bf", "#68BF9C"))+ 
  scale_color_manual(values = c("#9c68bf", "#68BF9C"))+
  geom_hline(yintercept = 0.5, linetype = "dashed", color= "gray88", size = 1)+ 
  geom_line(aes(y = choice_a, color = pair_type), data = dat_summary3, size = 2
  )+ 
  theme(legend.position = "none")+
  # geom_point(aes(y = choice_a), data = dat_summary3, color = "white", size = 9) +
  geom_errorbar(aes(y = choice_a, ymin = choice_a - ci, ymax = choice_a + ci),
                width = 0.1, data = dat_summary3, size = 1, color = "gray35") +
  geom_point(aes(y = choice_a), data = dat_summary3, size =4, shape=22) +
  scale_y_continuous(limits = c(-100,100), breaks = c(-100, -50, 0, 50 , 100), labels = c("-100%", "-50%", "0%", "50%", "100%")) + 
  labs(y = "(choice a) - (choice b)", x = "block")+
  theme_classic()+
  theme(legend.position = "none",
        strip.background = element_blank(),
        strip.text = element_text(size = 12, face = "bold"),
        text = element_text(size = 10, colour = "black"),
        axis.title = element_text(face = "bold"),
        axis.text = element_text(colour = "black", size = 10), 
        title = element_text(size = 12, face = "bold"))

svg(filename = paste0(meeting_directoty,prefix,'_declare_est_choice_a.svg'), height = 3, width = 3)
print(p_est)
dev.off()

# plot per cluster:
df_est2 = merge(df_est2,tmp, by = "participant")
df_est2$cluster = ifelse(df_est2$cluster== 1, "cluster1", "cluster2") 
dat_summary3 <- summarySEwithin(df_est2,
                                measurevar = "choice_a",
                                withinvars = c("pair_type", "block"), 
                                betweenvars = "cluster",
                                idvar = "participant")

p_est_cluster= ggplot(df_est2,aes(x=block,y=choice_a,group = as.factor(pair_type),color=as.factor(pair_type),fill=as.factor(pair_type)))+
  facet_wrap(~cluster)+
  scale_fill_manual(values = c("#9c68bf", "#68BF9C"))+ 
  scale_color_manual(values = c("#9c68bf", "#68BF9C"))+
  geom_hline(yintercept = 0.5, linetype = "dashed", color= "gray88", size = 1)+ 
  geom_line(aes(y = choice_a, color = pair_type), data = dat_summary3, size = 2
  )+ 
  theme(legend.position = "none")+
  # geom_point(aes(y = choice_a), data = dat_summary3, color = "white", size = 9) +
  geom_errorbar(aes(y = choice_a, ymin = choice_a - ci, ymax = choice_a + ci),
                width = 0.1, data = dat_summary3, size = 1, color = "gray35") +
  geom_point(aes(y = choice_a), data = dat_summary3, size =4, shape=22) +
  scale_y_continuous(limits = c(-100,100), breaks = c(-100, -50, 0, 50 , 100), labels = c("-100%", "-50%", "0%", "50%", "100%")) + 
  labs(y = "", x = "block")+
  theme_classic()+
  theme(legend.position = "none",
        strip.background = element_blank(),
        strip.text = element_blank(),
        text = element_text(size = 10, colour = "black"),
        axis.title = element_text(face = "bold"),
        axis.text = element_text(colour = "black", size = 10), 
        panel.grid.major.y = element_blank(),
        axis.text.y   = element_blank(),
        axis.ticks.y = element_blank(),
        title = element_text(size = 12, face = "bold"))

svg(filename = paste0(meeting_directoty,prefix,'_declare_est_choice_a_cluster.svg'), height = 3, width = 6)
print(p_est_cluster)
dev.off()

# do stats 
# no extreme outliers
# 9 levels with sig shapiro 
# qqplot show divergent from normality 

df_est2 %>%
  group_by(block, pair_type) %>%
  identify_outliers(choice_a)

shapiro <- df_est2 %>%
  group_by(block, pair_type) %>%
  shapiro_test(choice_a)

ggqqplot(df_est2, "choice_a", ggtheme = theme_bw()) +
  facet_grid(pair_type ~ block, labeller = "label_both")

# run repeated measures anova: 
res.aov <- anova_test(
  data = df_est2 , dv = choice_a, wid = participant, within = c(block, pair_type)
)
get_anova_table(res.aov)

# compute simple main effect
block.effect <- df_est2 %>%
  group_by(block) %>%
  anova_test(dv = choice_a, wid = participant, within = pair_type, detailed = T) %>%
  get_anova_table()



pwc <- df_est2 %>%
  group_by(block) %>%
  pairwise_t_test(
    choice_a ~ pair_type, paired = TRUE, 
    p.adjust.method = "bonferroni", detailed = T
  )


desc = df_est2 %>%
  group_by(pair_type, block) %>%
  get_summary_stats(choice_a, type = "mean_sd")

# compute effect size 
df_est2 %>% filter(block == 4) %>% cohens_d(choice_a ~ pair_type, paired = TRUE)
df_est2 %>% filter(block == 5) %>% cohens_d(choice_a ~ pair_type, paired = TRUE)
df_est2 %>% filter(block == 6) %>% cohens_d(choice_a ~ pair_type, paired = TRUE)





# by cluster
# 2 extreme outliers 
# 11 levels violates the normality assumtion shapiro is sig
# but qqplot looks OK
# levene test is significant for 1 level - block 6 non-reversed
# 



df_est2 %>%
  group_by(block, pair_type, cluster) %>%
  identify_outliers(choice_a)

shapiro <- df_est2 %>%
  group_by(block, pair_type, cluster) %>%
  shapiro_test(choice_a)
ggqqplot(df_est2, "choice_a", ggtheme = theme_bw()) +
  facet_grid(pair_type + block ~ cluster, labeller = "label_both")

df_est2 %>%
  group_by(block, pair_type) %>%
  levene_test(choice_a ~ cluster)

# run mixed anova: 
res.aov <- anova_test(
  data = df_est2 , dv = choice_a, wid = participant,
  between = cluster, within = c(block, pair_type)
)
get_anova_table(res.aov)

# the three way int. was sig - post hoc 
# Two-way ANOVA at each cluster level

two.way <- df_est2 %>%
  group_by(cluster) %>%
  anova_test(dv = choice_a, wid = participant, within = c(block, pair_type))
two.way

# Extract anova table
get_anova_table(two.way)

# compute simple simple main effect
block.effect <- df_est2 %>%
  group_by(cluster, block) %>%
  anova_test(dv = choice_a, wid = participant, within = pair_type, detailed = T) %>%
  get_anova_table()



pwc <- df_est2 %>%
  group_by(cluster, block) %>%
  pairwise_t_test(
    choice_a ~ pair_type, paired = TRUE, 
    p.adjust.method = "bonferroni", detailed = T
  )


df_est2$block_fct = factor(df_est2$block)


desc = df_est2 %>%
  group_by(pair_type, block, cluster) %>%
  get_summary_stats(choice_a, type = "mean_sd")

# compute effect size 
df_est2 %>% filter(cluster == "cluster2" & block == 4) %>% cohens_d(choice_a ~ pair_type, paired = TRUE)
df_est2 %>% filter(cluster == "cluster2" & block == 5) %>% cohens_d(choice_a ~ pair_type, paired = TRUE)
df_est2 %>% filter(cluster == "cluster2" & block == 6) %>% cohens_d(choice_a ~ pair_type, paired = TRUE)

df_est2 %>% filter(cluster == "cluster1" & block == 4) %>% cohens_d(choice_a ~ pair_type, paired = TRUE)
df_est2 %>% filter(cluster == "cluster1" & block == 5) %>% cohens_d(choice_a ~ pair_type, paired = TRUE)
df_est2 %>% filter(cluster == "cluster1" & block == 6) %>% cohens_d(choice_a ~ pair_type, paired = TRUE)





# questionaires -----------------------------------------------------------



main_path = "G:/Shared drives/AdmonPsy - Chronic Stress Project/"
questionaires_path = paste0(main_path, "questionaires_analysis/")
subjects_list = read_xlsx(paste0(questionaires_path,"subjects list - chronic stress study_18_5_2023.xlsx"),sheet = "Questionaires") 
subjects_list <-subjects_list %>% select(id,sex,age, BMI, hand)
subjects_list <- subjects_list %>% filter(id != "s_061" & id != "s_084")

quest = read_xlsx("G:/Shared drives/AdmonPsy - Chronic Stress Project/questionaires_analysis/TP1 Questionnaires (Responses)15-02-2023.xlsx",
                  sheet = "Questionaires")
IUS_items = read_xlsx("G:/Shared drives/AdmonPsy - Chronic Stress Project/questionaires_analysis/TP1 Questionnaires (Responses)15-02-2023.xlsx",
                      sheet = "Form Responses 1", range = "CR1:DC102", col_names = T)
IUS_items = IUS_items[quest$id %in% data2$subject,]
library(readr)
IUS_items1 = as.data.frame(sapply(IUS_items, function(x) parse_number(x)))    # not generally a useful result
cronbachs_alpha(IUS_items1)

quest = quest[quest$id %in% data2$subject,]
quest$subject = quest$id
quest = left_join(quest, subjects_list)
quest <-quest %>%  filter(id != "s_061"& id != "s_084")


quest = quest[quest$id %in% data2$subject,]
quest$subject = quest$id
quest = left_join(quest, subjects_list)
quest <-quest %>%  filter(id != "s_061"& id != "s_084")
quest = left_join(quest, tmp)

# table 1: 
library(table1)
subjects_list$group = ifelse(subjects_list$id %in% subj_names,"learners","non-learners")
extra_block = data_unfiltered$df %>% group_by(participant) %>% summarise(extra_block = factor(first(extra_block))) %>% ungroup()
subjects_list = left_join(subjects_list, extra_block, by = c("id" = "participant"))
table1(~ sex + age + BMI + hand + extra_block | group, data = subjects_list)
data3<-merge(par_couplling_decay,quest,by='subject')
data3$cluster = factor(data3$cluster)
table1(~ alpha_current + alpha_gen + coupling_decay + beta| cluster, data = data3)

best = data2c%>%group_by(subject)%>%
  summarise(best=first(best))
data3 = merge(data3,best, by = "subject")
data3 = left_join(data3, par_rw[c("subject",  "alpha_rw" ,"beta_rw" )])
colnames(quest)
data3$alpha_current_Z = scale(data3$alpha_current)[,1]
data3$alpha_gen_Z = scale(data3$alpha_gen) [,1]
data3$coupling_decay_Z = scale(data3$coupling_decay)[,1]
data3$beta_Z = scale(data3$beta)[,1]
m = lm(`IUS-12`~coupling_decay+alpha_gen+alpha_current+beta,data=data3)

m1 = lm(`IUS-12_Prospective_Anxiety`~coupling_decay+alpha_gen+alpha_current   +beta,data=data3)

m2 = lm(`IUS-12_Inhibitory_Anxiety`~coupling_decay+alpha_gen+alpha_current   +beta,data=data3)

m3 = lm(`IUS-12_Inhibitory_Anxiety`~coupling_decay+alpha_gen+alpha_current  +beta_Z,data=data3)

tab_model(m,m1,m2, m3,file = paste0(meeting_directoty,prefix,'IUS.doc'))

# using Z scores and interaction 
m1 = lm(`IUS-12`~coupling_decay_Z*alpha_gen_Z+alpha_current_Z  +beta_Z,data=data3)

m2 = lm(`IUS-12_Prospective_Anxiety`~coupling_decay_Z*alpha_gen_Z+alpha_current_Z  +beta_Z,data=data3)

m3 = lm(`IUS-12_Inhibitory_Anxiety`~coupling_decay_Z*alpha_gen_Z+alpha_current_Z  +beta_Z,data=data3)

tab_model(m1,m2, m3, file = paste0(meeting_directoty,prefix,'IUS_Z.doc'))

# plot figure 4 
f4 = plot_model(m1, type = "int") + theme_classic() +
  theme(axis.title = element_blank(), 
        text = element_text(colour = "black", size = 12), 
        title = element_blank(), legend.title = element_blank(), 
        axis.text = element_text(colour = "black")) 

# save figure 
svg(filename = paste0(meeting_directoty,prefix,'_f4.svg'), height = 3, width = 3.96)
print(f4)
dev.off()



# using Z scores and interaction - using alpha RW 
data3$alpha_rw_Z = scale(data3$alpha_rw)[,1]
data3$beta_rw_Z = scale(data3$beta_rw)[,1]
m1 = lm(`IUS-12`~alpha_rw_Z  +beta_rw_Z,data=data3)

m2 = lm(`IUS-12_Prospective_Anxiety`~alpha_rw_Z  +beta_rw_Z,data=data3)

m3 = lm(`IUS-12_Inhibitory_Anxiety`~alpha_rw_Z  +beta_rw_Z,data=data3)

tab_model(m1,m2, m3, file = paste0(meeting_directoty,prefix,'IUS_Z_RW.doc'))

# other questionaires 
m3 = lm(DASS_ANX~coupling_decay_Z*alpha_gen_Z+alpha_current_Z,data=data3)
m4 = lm(DASS_Dep~coupling_decay_Z*alpha_gen_Z+alpha_current_Z,data=data3)
m5 = lm(DASS_STRESS~coupling_decay_Z*alpha_gen_Z+alpha_current_Z,data=data3)
m6 = lm(DASS_total ~coupling_decay_Z*alpha_gen_Z+alpha_current_Z,data=data3)
m7 = lm(SOC~coupling_decay_Z*alpha_gen_Z+alpha_current_Z,data=data3)
m8 = lm(`CD-RISC`~coupling_decay_Z*alpha_gen_Z+alpha_current_Z,data=data3)
m9 = lm(PSS ~ coupling_decay_Z*alpha_gen_Z+alpha_current_Z,data=data3)
m10 = lm(PANAS_Pos ~ coupling_decay_Z*alpha_gen_Z+alpha_current_Z,data=data3)

tab_model(m3,m4,m5, m6, m7, m8, m9,file = paste0(meeting_directoty,prefix,'lm_otherScales.doc'))


# TICS, multiple comparison alpha level - (0.05/9 ??? 0.0056)
m1 = lm(TICS_WOOV~coupling_decay_Z*alpha_gen_Z+alpha_current_Z,data=data3)
m2 = lm(TICS_WODI~coupling_decay_Z*alpha_gen_Z+alpha_current_Z,data=data3)
m3 = lm(TICS_WORY~coupling_decay_Z*alpha_gen_Z+alpha_current_Z,data=data3)
m4 = lm(TICS_PRPE ~coupling_decay_Z*alpha_gen_Z+alpha_current_Z,data=data3)
m5 = lm(TICS_SOIS~coupling_decay_Z*alpha_gen_Z+alpha_current_Z,data=data3)
m6 = lm(TICS_SOOV~coupling_decay_Z*alpha_gen_Z+alpha_current_Z,data=data3)
m7 = lm(TICS_SOTE ~ coupling_decay_Z*alpha_gen_Z+alpha_current_Z,data=data3)
m8 = lm(TICS_LACK ~ coupling_decay_Z*alpha_gen_Z+alpha_current_Z,data=data3)
m9 = lm(TICS_EXWO ~ coupling_decay_Z*alpha_gen_Z+alpha_current_Z,data=data3)

tab_model(m1,m2,m3,m4,m5,m6,m7,m8,m9, file = paste0(meeting_directoty,prefix,'lm_TICS.doc'))
# aic cluster 
mAic = lm(`IUS-12`~best,data=data3)

m1aic = lm(`IUS-12_Prospective_Anxiety`~best,data=data3)

m2aic = lm(`IUS-12_Inhibitory_Anxiety`~best,data=data3)

tab_model(mAic,m1aic,m2aic,file = paste0(meeting_directoty,prefix,'IUS_aic_cluster.doc'))

m3aic = lm(DASS_ANX~best,data=data3)
m4aic = lm(DASS_Dep~best,data=data3)
m5aic = lm(DASS_STRESS~best,data=data3)
m6aic = lm(DASS_total ~best,data=data3)
m7aic = lm(SOC~best,data=data3)
m8aic = lm(`CD-RISC`~best,data=data3)

tab_model(m3aic,m4aic,m5aic, m6aic, m7aic, m8aic,file = paste0(meeting_directoty,prefix,'lm_otherScales_aic_cluster.doc'))


p1 = ggbetweenstats(data3, x = "cluster", y = "DASS_ANX", bf.message = FALSE, xlab = "")
p2 = ggbetweenstats(data3, x = "cluster", y = "SOC", bf.message = FALSE, xlab = "")
p3 = ggbetweenstats(data3, x = "cluster", y = "HRV_pNN50", bf.message = FALSE, xlab = "")
p4 = ggbetweenstats(data3, x = "cluster", y = "HRV_pNN50", bf.message = FALSE, xlab = "")
p5 = ggbetweenstats(data3, x = "cluster", y = "HRV_HF", bf.message = FALSE, xlab = "")
p6 = ggbetweenstats(data3, x = "cluster", y = "HRV_HFn", bf.message = FALSE, xlab = "")
p7 = ggbetweenstats(data3, x = "cluster", y = "HRV_LF", bf.message = FALSE, xlab = "")
p8 = ggbetweenstats(data3, x = "cluster", y = "HRV_LFn", bf.message = FALSE, xlab = "")
p9 = ggbetweenstats(data3, x = "cluster", y = "HRV_LFHF", bf.message = FALSE, xlab = "")
p10 = ggbetweenstats(data3, x = "cluster", y = "TP..total.spectrum.power.", bf.message = FALSE, xlab = "")
p11 = ggbetweenstats(data3, x = "cluster", y = "HRV_ApEn", bf.message = FALSE, xlab = "")
p12 = ggbetweenstats(data3, x = "cluster", y = "HRV_SampEn", bf.message = FALSE, xlab = "")
p13 = ggbetweenstats(data3, x = "cluster", y = "Mean.HR..beats.min.", bf.message = FALSE, xlab = "")



# HRV ---------------------------------------------------------------------


HRVrest_unfilterred = read.csv("G:/Shared drives/AdmonPsy - Chronic Stress Project/DATA/HRV_totals/HRV_aggregated_rest_2023-02-27_22-41-04.csv")
HRVrest_unfilterred$subject = HRVrest_unfilterred$id
HRVrest_unfilterred = left_join(HRVrest_unfilterred, subjects_list)
HRVrest_unfilterred <- HRVrest_unfilterred %>% filter(id != "s_061"& id != "s_084" & id != "s_087" & id != "s_053")
HRVrest  =HRVrest_unfilterred %>% filter(HRV_RMSSD<100 & HRV_RMSSD>10 )# &HRV_SDNN <139 & HRV_pNN50 <98 & HRV_HF<4000 & HRV_LF < 5000  &
                                           #HRV_LFn> 0.15 & HRV_HFn > 0.1 & HRV_LFHF <10 )

HRVrest = left_join(HRVrest,tmp)
HRVrest$group[HRVrest$cluster == 1] = "non-flex"
HRVrest$group[HRVrest$cluster == 2] = "flex"
HRVrest$group[is.na(HRVrest$cluster)] = "non-learners"
table1(~HRV_RMSSD + HRV_HF| group, data = HRVrest)
# omit non learners
HRVrest = HRVrest[HRVrest$id %in% data2$subject,]

data4<-merge(par_couplling_decay,HRVrest,by='subject')
best = data2c%>%group_by(subject)%>%
  summarise(best=first(best))
data4 = merge(data4,best, by = "subject")
data4 = merge(data4,par_rw, by = "subject")
median_split_df = data2c%>%group_by(subject)%>%
  summarise(GenAlpha=first(GenAlpha),LR=first(LR),DecayRate=first(DecayRate))
data5= merge(data4, median_split_df, by = "subject")



data5$alpha_current_Z = scale(data5$alpha_current)[,1]
data5$alpha_gen_Z = scale(data5$alpha_gen) [,1]
data5$coupling_decay_Z = scale(data5$coupling_decay)[,1]
data5$beta_Z = scale(data5$beta)[,1]
data5$bpm_Z = scale(data5$Mean.HR..beats.min.)[,1]
data5$age_Z = scale(data5$age)[,1]
data5$BMI_z = scale(data5$BMI)[,1]
data5$alpha_rw_Z = scale(data5$alpha_rw)[,1]
data5$beta_rw_Z = scale(data5$beta_rw)[,1]

m_RMSSD = lm(HRV_RMSSD~coupling_decay_Z*alpha_gen_Z+alpha_current_Z + beta_Z + age,data=data5)
m_RMSSD_rw = lm(HRV_RMSSD~alpha_rw_Z +beta_rw_Z,data=data5)
tab_model(m_RMSSD_rw,file = paste0(meeting_directoty,prefix,'m_RMSSD_rw.doc'))
m_SDNN = lm(HRV_SDNN~coupling_decay_Z*alpha_gen_Z+alpha_current_Z+beta_Z,data=data5)

m_pNN50 = lm(HRV_pNN50~coupling_decay_Z*alpha_gen_Z+alpha_current_Z+beta_Z,data=data5)

data5$HRV_HF_log = log(data5$HRV_HF)
m_HF = lm(HRV_HF_log~coupling_decay_Z*alpha_gen_Z+alpha_current_Z+beta_Z,data=data5)


data5$HRV_LF_log = log(data5$HRV_LF)
m_LF = lm(HRV_LF~coupling_decay_Z*alpha_gen_Z+alpha_current_Z+beta_Z,data=data5)


m_HFn = lm(HRV_HFn~coupling_decay_Z*alpha_gen_Z+alpha_current_Z+beta_Z,data=data5)


m_LFn = lm(HRV_LFn~coupling_decay_Z*alpha_gen_Z+alpha_current_Z+beta_Z,data=data5)



data5$HRV_LFHF_log = log(data5$HRV_LFHF)
m_LFHF = lm(HRV_LFHF_log~coupling_decay_Z*alpha_gen_Z+alpha_current_Z+beta_Z,data=data5)


m_TP_log = lm(TP..total.spectrum.power.~coupling_decay_Z*alpha_gen_Z+alpha_current_Z+beta_Z,data=data5)


m_SampEn = lm(HRV_SampEn~coupling_decay_Z*alpha_gen_Z+alpha_current_Z+beta_Z ,data=data5)
# 
# m_BPM = lm(Mean.HR..beats.min.~coupling_decay_Z*alpha_gen_Z+alpha_current_Z+beta_Z +,data=data5)
# check_model(m_BPM)


tab_model(m_SDNN, m_RMSSD, m_pNN50, file = paste0(meeting_directoty,prefix,'time_domain.doc'))
tab_model(m_HF, m_LF, m_LFHF, m_HFn, m_LFn, m_TP_log, file = paste0(meeting_directoty,prefix,'freq_domain.doc'))
tab_model(m_SampEn, file = paste0(meeting_directoty,prefix,'non_linear_domain.doc'))
tab_model(m_RMSSD,m_HF,m_HFn, file = paste0(meeting_directoty,prefix,'time_freq_for_paper.doc'))




# cluster 
data5 = left_join(data5, tmp)
p1 = ggbetweenstats(data5, x = "cluster", y = "HRV_RMSSD", bf.message = FALSE, xlab = "")
p2 = ggbetweenstats(data5, x = "cluster", y = "HRV_SDNN", bf.message = FALSE, xlab = "")
p3 = ggbetweenstats(data5, x = "cluster", y = "HRV_pNN50", bf.message = FALSE, xlab = "")
p4 = ggbetweenstats(data5, x = "cluster", y = "HRV_pNN50", bf.message = FALSE, xlab = "")
p5 = ggbetweenstats(data5, x = "cluster", y = "HRV_HF", bf.message = FALSE, xlab = "")
p6 = ggbetweenstats(data5, x = "cluster", y = "HRV_HFn", bf.message = FALSE, xlab = "")
p7 = ggbetweenstats(data5, x = "cluster", y = "HRV_LF", bf.message = FALSE, xlab = "")
p8 = ggbetweenstats(data5, x = "cluster", y = "HRV_LFn", bf.message = FALSE, xlab = "")
p9 = ggbetweenstats(data5, x = "cluster", y = "HRV_LFHF", bf.message = FALSE, xlab = "")
p10 = ggbetweenstats(data5, x = "cluster", y = "TP..total.spectrum.power.", bf.message = FALSE, xlab = "")
p11 = ggbetweenstats(data5, x = "cluster", y = "HRV_ApEn", bf.message = FALSE, xlab = "")
p12 = ggbetweenstats(data5, x = "cluster", y = "HRV_SampEn", bf.message = FALSE, xlab = "")
p13 = ggbetweenstats(data5, x = "cluster", y = "Mean.HR..beats.min.", bf.message = FALSE, xlab = "")

pcluster = ggarrange(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, 
                     p11, p12, p13)

svg(filename = paste0(meeting_directoty,prefix,'_kmeans_clusters_HRV.svg'), height = 9, width = 16)
print(pcluster)
dev.off()

# plot only freq domain plots 

pcluster = ggarrange(p10, p5, p7, p6, p8)

svg(filename = paste0(meeting_directoty,prefix,'_kmeans_clusters_HRV_FREQ_ONLY.svg'), height = 9, width = 16)
print(pcluster)
dev.off()
# # compute phasic HRV: HRV reactivity  -----------------------------------


HRVreversal = read.csv("G:/Shared drives/AdmonPsy - Chronic Stress Project/DATA/HRV_totals/HRV_aggregated_reversal_2023-02-27_22-41-43.csv")
HRVreversal = HRVreversal[HRVreversal$id %in% HRVrest$subject,]
HRVreversal$subject = HRVreversal$id
HRVreversal <- HRVreversal %>% filter(id != "s_061"& id != "s_084")
HRVreversal = HRVreversal%>% filter(HRV_RMSSD<100 & HRV_RMSSD>10 &HRV_SDNN <139 & HRV_pNN50 <98 & HRV_HF<4000 & HRV_LF < 5000  &
                                      HRV_LFn> 0.15 & HRV_HFn > 0.1 & HRV_LFHF <10 )
HRVrest$cluster = factor(HRVrest$cluster)
HRVrest = HRVrest[HRVrest$subject %in% HRVreversal$id,]
HRVrest_num = HRVrest %>% select(where(is.numeric))
HRVrest_num$age <- NULL
HRVrest_num$BMI <- NULL
HRVreversal_num = HRVreversal %>% select(where(is.numeric))

deltaHRV = HRVreversal_num-HRVrest_num
colnames(deltaHRV)<- paste0(colnames(deltaHRV),"_delta")
deltaHRV$subject = HRVrest$subject

data6 = inner_join(data5, deltaHRV)

# compute scaled variables 
data6$alpha_current_Z = scale(data6$alpha_current)[,1]
data6$alpha_gen_Z = scale(data6$alpha_gen) [,1]
data6$coupling_decay_Z = scale(data6$coupling_decay)[,1]
data6$beta_Z = scale(data6$beta)[,1]

data6$HRV_RMSSD_Z = scale(data6$HRV_RMSSD)[,1]
m_RMSSD_delta = lm(HRV_RMSSD_delta~HRV_RMSSD_Z + coupling_decay_Z*alpha_gen_Z+alpha_current_Z+beta_Z,data=data6)
check_model(m_RMSSD_delta)

data6$HRV_SDNN_Z = scale(data6$HRV_SDNN)[,1]
m_SDNN_delta = lm(HRV_SDNN_delta~HRV_SDNN_Z + coupling_decay_Z*alpha_gen_Z+alpha_current_Z+beta_Z,data=data6)
check_model(m_SDNN_delta)

data6$HRV_HF_delta = log(data6$HRV_HF_delta)
data6$HRV_HF_log_delta_Z = scale(data6$HRV_HF_delta)[,1]
m_HF_delta = lm(HRV_HF_delta~ + HRV_HF_delta_Z + coupling_decay_Z*alpha_gen_Z+alpha_current_Z+beta_Z,data=data6)
check_model(m_HF_delta)


m_HFn_delta = lm(HRV_HFn_delta~coupling_decay_Z*alpha_gen_Z+alpha_current_Z+beta_Z,data=data6)
check_model(m_HFn_delta)


m_LFn_delta = lm(HRV_LFn_delta~coupling_decay_Z*alpha_gen_Z+alpha_current_Z+beta_Z,data=data6)
check_model(m_LFn_delta)



data6$HRV_LFHF_log_delta = log(data6$HRV_LFHF_delta)
m_LFHF = lm(HRV_LFHF_log_delta~coupling_decay_Z*alpha_gen_Z+alpha_current_Z+beta_Z,data=data6)
check_model(m_LFHF_delta)


data6$TP_log_delta = log(data6$TP..total.spectrum.power._delta)
m_TP_log_delta= lm(TP_log_delta~coupling_decay_Z*alpha_gen_Z+alpha_current_Z+beta_Z,data=data6)
check_model(m_TP_log_delta)


m_SampEn_delta = lm(HRV_SampEn_delta~coupling_decay_Z*alpha_gen_Z+alpha_current_Z+beta_Z,data=data6)
check_model(m_SampEn_delta)


m_BPM_delta = lm(Mean.HR..beats.min._delta~coupling_decay_Z*alpha_gen_Z+alpha_current_Z+beta_Z,data=data6)
check_model(m_BPM_delta)




tab_model(m_BPM_delta, m_SDNN_delta, m_RMSSD_delta, file = paste0(meeting_directoty,prefix,'time_domain_RL.doc'))
tab_model(m_HF_delta, m_LFn_delta, m_HFn_delta, m_LFn_delta, m_TP_log_delta, file = paste0(meeting_directoty,prefix,'freq_domain_RL.doc'))
tab_model(m_SampEn_delta, file = paste0(meeting_directoty,prefix,'non_linear_domain_RL.doc'))



