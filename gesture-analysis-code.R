#----Set-up----

library(lme4); library(readr); library(dplyr); library(ggplot2); library(sjPlot)
library(leaps); library(glmmTMB); library(multcomp)
library(lmerTest) # can use for step function, but don't trust/report p-values

# set working directory & load data file

#----Data Cleaning----

# remove CB002 - neurotypical participant with undiagnosed mild apraxia
final_df_lrt <- final_df_lrt[!(final_df_lrt$subject=="blank"),]
final_df_lrt <- final_df_lrt[!(final_df_lrt$subject=="CB002"),]
# remove AB121 & AB137 due to missing data - only have MLES 
final_df_lrt <- final_df_lrt[!(final_df_lrt$subject=="AB121"),]
final_df_lrt <- final_df_lrt[!(final_df_lrt$subject=="AB137"),]

#----Subsets----

control_df_lrt <- subset(final_df_lrt, group == 'controls')
apraxia_df_lrt <- subset(final_df_lrt, group == 'apraxia')
NMFUL_lrt <- subset(final_df_lrt, task == 'NMFUL')
UMFUL_lrt <- subset(final_df_lrt, task == 'UMFUL')
MLES_lrt <- subset(final_df_lrt, task == 'MLES')

#-----1. Group models-----

### Total Accuracy ###

groupTotal.MaxModel <- lmer(total ~ 1 + group*task + (1|subject) + (1|item), data=final_df_lrt, control=lmerControl(optCtrl = list(maxfun=20000)))
step(groupTotal.MaxModel, direction="backward")
groupTotal.LRTmodel <- lmer(total ~ 1 + group + task + (1|subject) + (1|item), data=final_df_lrt, control=lmerControl(optCtrl = list(maxfun=20000)))
anova(groupTotal.MaxModel, groupTotal.LRTmodel) # no significant difference
summary(glht(groupTotal.LRTmodel, mcp(task="Tukey")))
emmeans(groupTotal.MaxModel, pairwise~group*task, adjust="tukey")
# compare effect of group
groupTotal.noG <- lmer(total ~ 1 + task + (1|subject) + (1|item), data=final_df_lrt, control=lmerControl(optCtrl = list(maxfun=20000)))
anova(groupTotal.MaxModel, groupTotal.noG)
# compare effect of task
groupTotal.noT <- lmer(total ~ 1 + group + (1|subject) + (1|item), data=final_df_lrt, control=lmerControl(optCtrl = list(maxfun=20000)))
anova(groupTotal.MaxModel, groupTotal.noT)
# compare effect of interaction
groupTotal.noI <- lmer(total ~ 1 + group+task + (1|subject), data=final_df_lrt, control=lmerControl(optCtrl = list(maxfun=20000)))
anova(groupTotal.MaxModel, groupTotal.noI)

## Total Accuracy as Percent Correct ##
final_df_lrt$accuracy <- ((final_df_lrt$total)/4)*100
levels(final_df_lrt$group) <- c("NT group", "LCVA group")
levels(final_df_lrt$task) <- c("named", "meaningful", "meaningless")

groupTotal.PC <- lmer(accuracy ~ 1 + group*task + (1|subject) + (1|item), data=final_df_lrt, control=lmerControl(optCtrl = list(maxfun=20000)))
plot_model(groupTotal.PC, type="pred", terms=c("group","task"), title="             Group x Task Interaction - 
     Gesture Imitation (Percent Accuracy)
           ") + theme_classic() + theme(text = element_text(size = 18))
ggsave("fig2.tiff", dpi=300)


### Hand Posture Accuracy ###

groupHP.MaxModel <- glmer(HP ~ 1 + group*task + (1|subject) + (1|item), data=final_df_lrt, family=binomial) 
# note- step function doesn't work with GLMR models
groupHP.LRTmodel <- glmer(HP ~ 1 + group*task + (1|subject) + (1|item), data=final_df_lrt, family=binomial)
anova(groupHP.MaxModel, groupHP.LRTmodel) # no significant difference
summary(glht(groupHP.MaxModel, mcp(task="Tukey")))
summary(glht(groupHP.LRTmodel, mcp(task="Tukey"))) 
emmeans(groupHP.MaxModel, pairwise~group*task, adjust="tukey")
# compare effect of group
groupHP.noG <- glmer(HP ~ 1 + task + (1|subject) + (1|item), data=final_df_lrt, family=binomial)
anova(groupHP.MaxModel, groupHP.noG)
# compare effect of task
groupHP.noT <- glmer(HP ~ 1 + group + (1|subject) + (1|item), data=final_df_lrt, family=binomial)
anova(groupHP.MaxModel, groupHP.noT)
# compare effect of interaction
groupHP.noI <- glmer(HP ~ 1 + group+task + (1|subject) + (1|item), data=final_df_lrt, family=binomial)
anova(groupHP.MaxModel, groupHP.noI)

## can these plots be interpreted correctly if task is not contrast/sum coded?
plot_model(groupTotal.LRTmodel, type="pred", terms=c("group","task"), title="Group x Task Interaction - Total Accuracy") + theme_classic() + theme(text = element_text(size = 20)) 
plot_model(groupHP.LRTmodel, type="pred", terms=c("group","task"), title="Group x Task Interaction - HP Accuracy") + theme_classic() + theme(text = element_text(size = 20)) 
# plot_model(groupTotal.MaxModel, type="pred", terms=c("group","task")) 
# plot_model(groupHP.MaxModel, type="pred", terms=c("group","task"))

#-----1b. Apraxia task models-----

### Total Accuracy ###

apxTotal.MaxModel <- lmer(total ~ 1 + task + (1|subject) + (1|item), data=apraxia_df_lrt, control=lmerControl(optCtrl = list(maxfun=20000)))
step(apxTotal.MaxModel, direction="backward")
summary(glht(apxTotal.MaxModel, mcp(task="Tukey")))
# compare effect of task
apxTotal.noT <- lmer(total ~ 1 + (1|subject) + (1|item), data=apraxia_df_lrt, control=lmerControl(optCtrl = list(maxfun=20000)))
anova(apxTotal.MaxModel, apxTotal.noT)

### Hand Posture Accuracy ###

apxHP.MaxModel <- glmer(HP ~ 1 + task + (1|subject) + (1|item), data=apraxia_df_lrt, family=binomial) 
summary(glht(apxHP.MaxModel, mcp(task="Tukey")))
# compare effect of task
apxHP.noT <- glmer(HP ~ 1 + (1|subject) + (1|item), data=apraxia_df_lrt, family=binomial)
anova(apxHP.MaxModel, apxHP.noT)

## can these plots be interpreted correctly if task is not contrast/sum coded?
a<-plot_model(apxTotal.MaxModel, type="pred", terms=c("task"), title="Patient Task Effect - Total Accuracy") + theme_classic() + theme(text = element_text(size = 20)) 
b<-plot_model(apxHP.MaxModel, type="pred", terms=c("task"), title="Patient Task Effect - HP Accuracy") + theme_classic() + theme(text = element_text(size = 20)) 
egg::ggarrange(a,b)

#-----2. Task predictor models-----

#-----2a. Language comprehension-----

apraxia_df_lrt2 <- apraxia_df_lrt[!is.na(apraxia_df_lrt$ppvtPercCorr),] # remove subjects who did not complete PPVT

# Predicting Total

langComp.MaxModel <- lmer(total ~ 1 + ppvtPercCorr*task + (1|subject) + (1|item), data=apraxia_df_lrt2)
step(langComp.MaxModel, direction="backward")
langComp.LRTmodel <- lmer(total ~ 1 + ppvtPercCorr:task + (1|subject) + (1|item), data=apraxia_df_lrt2)
anova(langComp.MaxModel, langComp.LRTmodel)  # no significant difference
summary(glht(langComp.MaxModel, mcp(task="Tukey")))
# summary(glht(langComp.LRTmodel, mcp(task="Tukey")))
# no need to examine pairwise comparisons of task because the model already tells us which levels of task interact with PPVT
plot_model(langComp.LRTmodel, type="pred", terms=c("ppvtPercCorr","task"), title="PPVT x Task Interaction - Total Accuracy") + theme_classic() + theme(text = element_text(size = 14)) 
emmeans::emmeans(langComp.MaxModel, pairwise~task*ppvtPercCorr, adjust="tukey")

# compare effect of PPVT
langComp.noP <- lmer(total ~ 1 + task + (1|subject) + (1|item), data=apraxia_df_lrt2, control=lmerControl(optCtrl = list(maxfun=20000)))
anova(langComp.MaxModel, langComp.noP)
# compare effect of task
langComp.noT <- lmer(total ~ 1 + ppvtPercCorr + (1|subject) + (1|item), data=apraxia_df_lrt, control=lmerControl(optCtrl = list(maxfun=20000)))
anova(langComp.MaxModel, langComp.noT)
# compare effect of interaction
langComp.noI <- lmer(total ~ 1 + ppvtPercCorr+task + (1|subject) + (1|item), data=apraxia_df_lrt2, control=lmerControl(optCtrl = list(maxfun=20000)))
anova(langComp.MaxModel, langComp.noI)
emmeans::emmeans(langComp.MaxModel, pairwise~task*ppvtPercCorr, adjust="tukey")

# Predicting HP

#apraxia_df_lrt2 <- apraxia_df_lrt[!is.na(apraxia_df_lrt$ppvtPercCorr),]

## would not converge
# langComp.HP.MaxModel <- glmer(HP ~ 1 + ppvtPercCorr*task + (1|subject) + (1|item), data=apraxia_df_lrt2, family=binomial(link="logit"),
#                              control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
# rescaled PPVT:
apraxia_df_lrt2$ppvtScaled <- scale(apraxia_df_lrt2$ppvtPercCorr, scale=TRUE)

langComp.HP.MaxModel <- glmer(HP ~ 1 + ppvtScaled*task + (1|subject) + (1|item), data=apraxia_df_lrt2, family=binomial)
langComp.HP.LRTmodel <- glmer(HP ~ 1 + ppvtPercCorr + task + (1|subject) + (1|item), data=apraxia_df_lrt2, family=binomial)
anova(langComp.HP.MaxModel, langComp.HP.LRTmodel) # no significant difference
summary(glht(langComp.HP.LRTmodel, mcp(task="Tukey")))
summary(glht(langComp.HP.MaxModel, mcp(task="Tukey")))
emmeans::emmeans(langComp.HP.MaxModel, pairwise~task*ppvtScaled, adjust="tukey")
plot_model(langComp.HP.LRTmodel, type="pred", terms=c("ppvtPercCorr","task"), ci.nvl=NULL,
           title="PPVT (scaled) x Task Interaction - HP Accuracy") + theme_classic() + 
  theme(text = element_text(size = 14)) 

# compare effect of PPVT
langComp.HP.noP <- glmer(HP ~ 1 + task + (1|subject) + (1|item), data=apraxia_df_lrt2, family=binomial)
anova(langComp.HP.MaxModel, langComp.HP.noP)
# compare effect of task
langComp.HP.noT <- glmer(HP ~ 1 + ppvtScaled + (1|subject) + (1|item), data=apraxia_df_lrt2, family=binomial)
anova(langComp.HP.MaxModel, langComp.HP.noT)
# compare effect of interaction
langComp.HP.noI <- glmer(HP ~ 1 + ppvtScaled+task + (1|subject) + (1|item), data=apraxia_df_lrt2, family=binomial)
anova(langComp.HP.MaxModel, langComp.HP.noI)

#-----2b. Semantic gesture recognition-----

apraxia_df_lrt3 <- apraxia_df_lrt[!is.na(apraxia_df_lrt$GR_SemGR.Overall),] #remove subjects with missing data

# Predicting Total

semGR.total.MaxModel <- lmer(total ~ 1 + GR_SemGR.Overall*task + (1|subject) + (1|item), data=apraxia_df_lrt3)
step(semGR.total.MaxModel, direction="backward")
semGR.total.LRTmodel <- lmer(total ~ 1 + GR_SemGR.Overall + task + (1|subject) + (1|item), data=apraxia_df_lrt3)
anova(semGR.total.MaxModel, semGR.total.LRTmodel) # no significant difference
summary(glht(semGR.total.LRTmodel, mcp(task="Tukey")))
summary(glht(semGR.total.MaxModel, mcp(task="Tukey")))

# compare effect of Sem_GR
semGR.total.noP <- lmer(total ~ 1 + task + (1|subject) + (1|item), data=apraxia_df_lrt3, control=lmerControl(optCtrl = list(maxfun=20000)))
anova(semGR.total.MaxModel, semGR.total.noP)
# compare effect of task
semGR.total.noT <- lmer(total ~ 1 + GR_SemGR.Overall + (1|subject) + (1|item), data=apraxia_df_lrt3, control=lmerControl(optCtrl = list(maxfun=20000)))
anova(semGR.total.MaxModel, semGR.total.noT)
# compare effect of interaction
semGR.total.noI <- lmer(total ~ 1 + GR_SemGR.Overall+task + (1|subject) + (1|item), data=apraxia_df_lrt3, control=lmerControl(optCtrl = list(maxfun=20000)))
anova(semGR.total.MaxModel, semGR.total.noI)

# Predicting HP

semGR.HP.MaxModel <- glmer(HP ~ 1 + GR_SemGR.Overall*task + (1|subject) + (1|item), data=apraxia_df_lrt3, family=binomial)
step(semGR.HP.MaxModel, direction="backward")
semGR.HP.LRTmodel <- glmer(HP ~ 1 + GR_SemGR.Overall + task + (1|subject) + (1|item), data=apraxia_df_lrt3, family=binomial)
anova(semGR.HP.MaxModel, semGR.HP.LRTmodel) # no significant difference
summary(glht(semGR.HP.LRTmodel, mcp(task="Tukey")))
summary(glht(semGR.HP.MaxModel, mcp(task="Tukey")))

# compare effect of Sem_GR
semGR.HP.noP <- glmer(HP ~ 1 + task + (1|subject) + (1|item), data=apraxia_df_lrt3, family=binomial)
anova(semGR.HP.MaxModel, semGR.HP.noP)
# compare effect of task
semGR.HP.noT <- glmer(HP ~ 1 + GR_SemGR.Overall + (1|subject) + (1|item), data=apraxia_df_lrt3, family=binomial)
anova(semGR.HP.MaxModel, semGR.HP.noT)
# compare effect of interaction
semGR.HP.noI <- glmer(HP ~ 1 + GR_SemGR.Overall+task + (1|subject) + (1|item), data=apraxia_df_lrt3, family=binomial)
anova(semGR.HP.MaxModel, semGR.HP.noI)


#-----2c. Spatial gesture recognition-----

spatGR.total.MaxModel <- lmer(total ~ 1 + GR_SpatGR.Overall*task + (1|subject) + (1|item), data=apraxia_df_lrt3)

# Predicting Total

step(spatGR.total.MaxModel, direction="backward")
spatGR.total.LRTmodel <- lmer(total ~ 1 + GR_SpatGR.Overall + task + (1|subject) + (1|item), data=apraxia_df_lrt3)
anova(spatGR.total.MaxModel, spatGR.total.LRTmodel) # no significant difference
summary(glht(spatGR.total.LRTmodel, mcp(task="Tukey")))
summary(glht(spatGR.total.MaxModel, mcp(task="Tukey")))

# compare effect of Spat_GR
spatGR.total.noP <- lmer(total ~ 1 + task + (1|subject) + (1|item), data=apraxia_df_lrt3, control=lmerControl(optCtrl = list(maxfun=20000)))
anova(spatGR.total.MaxModel, spatGR.total.noP)
# compare effect of task
spatGR.total.noT <- lmer(total ~ 1 + GR_SpatGR.Overall + (1|subject) + (1|item), data=apraxia_df_lrt3, control=lmerControl(optCtrl = list(maxfun=20000)))
anova(spatGR.total.MaxModel, spatGR.total.noT)
# compare effect of interaction
spatGR.total.noI <- lmer(total ~ 1 + GR_SpatGR.Overall+task + (1|subject) + (1|item), data=apraxia_df_lrt3, control=lmerControl(optCtrl = list(maxfun=20000)))
anova(spatGR.total.MaxModel, spatGR.total.noI)

# Predicting HP

spatGR.HP.MaxModel <- glmer(HP ~ 1 + GR_SpatGR.Overall*task + (1|subject) + (1|item), data=apraxia_df_lrt3, family=binomial,
                            control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
step(spatGR.HP.MaxModel, direction="backward")
spatGR.HP.LRTmodel <- glmer(HP ~ 1 + GR_SpatGR.Overall + task + (1|subject) + (1|item), data=apraxia_df_lrt3, family=binomial)
anova(spatGR.HP.MaxModel, spatGR.HP.LRTmodel) # no significant difference
summary(glht(spatGR.HP.LRTmodel, mcp(task="Tukey")))
summary(glht(spatGR.HP.MaxModel, mcp(task="Tukey")))

# compare effect of Spat_GR
spatGR.HP.noP <- glmer(HP ~ 1 + task + (1|subject) + (1|item), data=apraxia_df_lrt3, family=binomial)
anova(spatGR.HP.MaxModel, spatGR.HP.noP)
# compare effect of task
spatGR.HP.noT <- glmer(HP ~ 1 + GR_SpatGR.Overall + (1|subject) + (1|item), data=apraxia_df_lrt3, family=binomial)
anova(spatGR.HP.MaxModel, spatGR.HP.noT)
# compare effect of interaction
spatGR.HP.noI <- glmer(HP ~ 1 + GR_SpatGR.Overall+task + (1|subject) + (1|item), data=apraxia_df_lrt3, family=binomial)
anova(spatGR.HP.MaxModel, spatGR.HP.noI)

plot_model(spatGR.HP.LRTmodel, type="pred", terms=c("GR_SpatGR.Overall","task"), title="Spatial Gesture Recognition x Task Interaction - HP Accuracy", ci.lvl = NA) + theme_classic() + theme(text = element_text(size = 14)) 


#-----2d. Exploratory language predictors-----

lang_df <- read_csv("lang_df.csv")
lang_df$subject <- as.factor(lang_df$subject)
lang_df$langprof <- as.factor(lang_df$langprof)

lang_df <- merge(apraxia_df_lrt, lang_df, by="subject") 

wabAQ.model <- lmer(total ~ 1 + WABAQ*task + (1|subject) + (1|item), data=lang_df)
wabIC.model <- lmer(total ~ 1 + WABic*task + (1|subject) + (1|item), data=lang_df)
wabFlcy.model <- lmer(total ~ 1 + WABflcy*task + (1|subject) + (1|item), data=lang_df) # marginal interaction
wabTotal.model <- lmer(total ~ 1 + WABsstotal*task + (1|subject) + (1|item), data=lang_df)
wabComp.model <- lmer(total ~ 1 + WABcomp*task + (1|subject) + (1|item), data=lang_df)
wabRep.model <- lmer(total ~ 1 + WABrep*task + (1|subject) + (1|item), data=lang_df)
wabName.model <- lmer(total ~ 1 + WABnaming*task + (1|subject) + (1|item), data=lang_df) # significant interaction
sWt.model <- lmer(total ~ 1 + sWt_original*task + (1|subject) + (1|item), data=lang_df) # significant interaction
pWt.model <- lmer(total ~ 1 + pWt_original*task + (1|subject) + (1|item), data=lang_df) # significant interaction
pnt.model <- lmer(total ~ 1 + pntPerCor*task + (1|subject) + (1|item), data=lang_df) # marginal interaction
ppt.model <- lmer(total ~ 1 + pptPerCor*task + (1|subject) + (1|item), data=lang_df) # significant interaction
cct.model <- lmer(total ~ 1 + cctPerCor*task + (1|subject) + (1|item), data=lang_df) # significant interaction
sWt.model2 <- lmer(total ~ 1 + sWt_refit*task + (1|subject) + (1|item), data=lang_df) # significant interaction
bothWts.model <- lmer(total ~ 1 + WABAQ + sWt_original*task + pWt_original*task + (1|subject) + (1|item), data=lang_df) 
bothWts.model2 <- lmer(total ~ 1 + sWt_original*task + pWt_original*task + (1|subject) + (1|item), data=lang_df) 


### both REFIT weights WITH WABAQ ###

refitWts.model <- lmer(total ~ 1 + WABAQ + sWt_refit*task + pWt_refit*task + (1|subject) + (1|item), data=lang_df) 
# compare main effect of s-weight
refitWts.model.noS <- lmer(total ~ 1 + WABAQ + task + pWt_refit*task + (1|subject) + (1|item), data=lang_df, control=lmerControl(optCtrl = list(maxfun=20000)))
anova(refitWts.model, refitWts.model.noS)
# compare main effect of p-weight
refitWts.model.noP <- lmer(total ~ 1 + WABAQ + task + sWt_refit*task + (1|subject) + (1|item), data=lang_df, control=lmerControl(optCtrl = list(maxfun=20000)))
anova(refitWts.model, refitWts.model.noP)
# compare effect of s-interaction
refitWts.model.noSI <- lmer(total ~ 1 + WABAQ + task + sWt_refit + pWt_refit + pWt_refit:task + (1|subject) + (1|item), data=lang_df, control=lmerControl(optCtrl = list(maxfun=20000)))
anova(refitWts.model, refitWts.model.noSI)
# compare effect of p-interaction
refitWts.model.noPI <- lmer(total ~ 1 + WABAQ + task + sWt_refit + pWt_refit + sWt_refit:task + (1|subject) + (1|item), data=lang_df, control=lmerControl(optCtrl = list(maxfun=20000)))
anova(refitWts.model, refitWts.model.noPI)
# compare main effect of task
refitWts.model.noT <- lmer(total ~ 1 + WABAQ + sWt_refit + pWt_refit + sWt_refit:task + pWt_refit:task + (1|subject) + (1|item), data=lang_df, control=lmerControl(optCtrl = list(maxfun=20000)))
anova(refitWts.model, refitWts.model.noT)
# posthoc
emmeans::emmeans(refitWts.model, pairwise~sWt_refit*task, adjust="tukey")
emmeans::emmeans(refitWts.model, pairwise~pWt_refit*task, adjust="tukey")
emtrends(refitWts.model, pairwise ~ task, var = "sWt_refit")
emtrends(refitWts.model, pairwise ~ task, var = "pWt_refit")
plot_model(refitWts.model, type="pred", terms=c("sWt_refit","task"), se = NULL, ci.lvl = NULL,
           title="S Weight x Task Interaction - Total Accuracy") + 
  theme_classic() + theme(text = element_text(size = 14)) 


### both weights WITH WABAQ ###

# compare main effect of s-weight
bothWt.model.noS <- lmer(total ~ 1 + WABAQ + task + pWt_original*task + (1|subject) + (1|item), data=lang_df, control=lmerControl(optCtrl = list(maxfun=20000)))
anova(bothWts.model, bothWt.model.noS)
# compare main effect of p-weight
bothWt.model.noP <- lmer(total ~ 1 + WABAQ + task + sWt_original*task + (1|subject) + (1|item), data=lang_df, control=lmerControl(optCtrl = list(maxfun=20000)))
anova(bothWts.model, bothWt.model.noP)
# compare effect of s-interaction
bothWt.model.noSI <- lmer(total ~ 1 + WABAQ + task + sWt_original + pWt_original + pWt_original:task + (1|subject) + (1|item), data=lang_df, control=lmerControl(optCtrl = list(maxfun=20000)))
anova(bothWts.model, bothWt.model.noSI)
# compare effect of p-interaction
bothWt.model.noPI <- lmer(total ~ 1 + WABAQ + task + sWt_original + pWt_original + sWt_original:task + (1|subject) + (1|item), data=lang_df, control=lmerControl(optCtrl = list(maxfun=20000)))
anova(bothWts.model, bothWt.model.noPI)
# compare main effect of task
bothWt.model.noT <- lmer(total ~ 1 + WABAQ + sWt_original + pWt_original + sWt_original:task + pWt_original:task + (1|subject) + (1|item), data=lang_df, control=lmerControl(optCtrl = list(maxfun=20000)))
anova(bothWts.model, bothWt.model.noT)
# posthoc
emmeans::emmeans(bothWts.model, pairwise~sWt_original*task, adjust="tukey")
emtrends(bothWts.model, pairwise ~ task, var = "sWt_original")
emtrends(bothWts.model, pairwise ~ task, var = "pWt_original")



### both weights ###

# compare effect of s-weight
bothWt.model.noS <- lmer(total ~ 1 + task + pWt_original*task + (1|subject) + (1|item), data=lang_df, control=lmerControl(optCtrl = list(maxfun=20000)))
anova(bothWts.model2, bothWt.model.noS)
# compare effect of p-weight
bothWt.model.noP <- lmer(total ~ 1 + task + sWt_original*task + (1|subject) + (1|item), data=lang_df, control=lmerControl(optCtrl = list(maxfun=20000)))
anova(bothWts.model2, bothWt.model.noP)
# compare effect of s-interaction
bothWt.model.noSI <- lmer(total ~ 1 + task + sWt_original + pWt_original + pWt_original:task + (1|subject) + (1|item), data=lang_df, control=lmerControl(optCtrl = list(maxfun=20000)))
anova(bothWts.model2, bothWt.model.noSI)
# compare effect of p-interaction
bothWt.model.noPI <- lmer(total ~ 1 + task + sWt_original + pWt_original + sWt_original:task + (1|subject) + (1|item), data=lang_df, control=lmerControl(optCtrl = list(maxfun=20000)))
anova(bothWts.model2, bothWt.model.noPI)
# compare main effect of task
bothWt.model.noT <- lmer(total ~ 1 + sWt_original*task + pWt_original*task + (1|subject) + (1|item), data=lang_df, control=lmerControl(optCtrl = list(maxfun=20000)))
anova(bothWts.model, bothWt.model.noT)
# posthoc
emmeans::emmeans(bothWts.model2, pairwise~sWt_original:task, adjust="tukey")



### s weight ###

# compare effect of s-weight
sWt.model.noS <- lmer(total ~ 1 + task + (1|subject) + (1|item), data=lang_df, control=lmerControl(optCtrl = list(maxfun=20000)))
anova(sWt.model, sWt.model.noS)
# compare effect of task
sWt.model.noT <- lmer(total ~ 1 + sWt_original + (1|subject) + (1|item), data=lang_df, control=lmerControl(optCtrl = list(maxfun=20000)))
anova(sWt.model, sWt.model.noT)
# compare effect of interaction
sWt.model.noI <- lmer(total ~ 1 + sWt_original+task + (1|subject) + (1|item), data=lang_df, control=lmerControl(optCtrl = list(maxfun=20000)))
anova(sWt.model, sWt.model.noI)
# posthoc
emmeans::emmeans(sWt.model, pairwise~sWt_original*task, adjust="tukey")



### p weight ###

# compare effect of p-weight
pWt.model.noP <- lmer(total ~ 1 + task + (1|subject) + (1|item), data=lang_df, control=lmerControl(optCtrl = list(maxfun=20000)))
anova(pWt.model, pWt.model.noP)
# compare effect of task
pWt.model.noT <- lmer(total ~ 1 + pptPerCor + (1|subject) + (1|item), data=lang_df, control=lmerControl(optCtrl = list(maxfun=20000)))
anova(pWt.model, pWt.model.noT)
# compare effect of interaction
pWt.model.noI <- lmer(total ~ 1 + pptPerCor+task + (1|subject) + (1|item), data=lang_df, control=lmerControl(optCtrl = list(maxfun=20000)))
anova(pWt.model, pWt.model.noI)
# posthoc
emmeans::emmeans(pWt.model, pairwise~pWt_original*task, adjust="tukey")



### PPT ###

# compare effect of PPT
ppt.model.noP <- lmer(total ~ 1 + task + (1|subject) + (1|item), data=lang_df, control=lmerControl(optCtrl = list(maxfun=20000)))
anova(ppt.model, ppt.model.noP)
# compare effect of task
ppt.model.noT <- lmer(total ~ 1 + pptPerCor + (1|subject) + (1|item), data=lang_df, control=lmerControl(optCtrl = list(maxfun=20000)))
anova(ppt.model, ppt.model.noT)
# compare effect of interaction
ppt.model.noI <- lmer(total ~ 1 + pptPerCor+task + (1|subject) + (1|item), data=lang_df, control=lmerControl(optCtrl = list(maxfun=20000)))
anova(ppt.model, ppt.model.noI)
# posthoc
emmeans::emmeans(ppt.model, pairwise~pptPerCor*task, adjust="tukey")


### CCT ###

cct.model <- lmer(total ~ 1 + WABAQ + cctPerCor*task + (1|subject) + (1|item), data=lang_df) # significant interaction

# compare effect of CCT
cct.model.noP <- lmer(total ~ 1 + WABAQ + task + (1|subject) + (1|item), data=lang_df, control=lmerControl(optCtrl = list(maxfun=20000)))
anova(cct.model, cct.model.noP)
# compare effect of task
cct.model.noT <- lmer(total ~ 1 + WABAQ + cctPerCor + (1|subject) + (1|item), data=lang_df, control=lmerControl(optCtrl = list(maxfun=20000)))
anova(cct.model, cct.model.noT)
# compare effect of interaction
cct.model.noI <- lmer(total ~ 1 + WABAQ + cctPerCor+task + (1|subject) + (1|item), data=lang_df, control=lmerControl(optCtrl = list(maxfun=20000)))
anova(cct.model, cct.model.noI)
# posthoc
emmeans::emmeans(cct.model, pairwise~cctPerCor*task, adjust="tukey")
emmeans::emtrends(cct.model, pairwise ~ task, var = "cctPerCor")
plot_model(cct.model, type="pred", terms=c("cctPerCor","task"), se = NULL,
           title="CCT x Task Interaction - Total Accuracy") + 
  theme_classic() + theme(text = element_text(size = 14)) 


### NWR ###

# missing data 
lang_df2 <- lang_df[!(lang_df$subject=="AB047"),]
lang_df3 <- lang_df2[!(lang_df2$subject=="AB131"),]

nwr.model <- lmer(total ~ 1 + WABAQ + nwrPerCor*task + (1|subject) + (1|item), data=lang_df3) # no significant interaction

# compare effect of NWR
nwr.model.noP <- lmer(total ~ 1 + WABAQ + task + (1|subject) + (1|item), data=lang_df3, control=lmerControl(optCtrl = list(maxfun=20000)))
anova(nwr.model, nwr.model.noP)
# compare effect of task
nwr.model.noT <- lmer(total ~ 1 + WABAQ + nwrPerCor + (1|subject) + (1|item), data=lang_df3, control=lmerControl(optCtrl = list(maxfun=20000)))
anova(nwr.model, nwr.model.noT)
# compare effect of interaction
nwr.model.noI <- lmer(total ~ 1 + WABAQ + nwrPerCor+task + (1|subject) + (1|item), data=lang_df3, control=lmerControl(optCtrl = list(maxfun=20000)))
anova(nwr.model, nwr.model.noI)
# posthoc
emmeans::emmeans(nwr.model, pairwise~nwrPerCor*task, adjust="tukey")
emtrends(nwr.model, pairwise ~ task, var = "nwrPerCor")
plot_model(nwr.model, type="pred", terms=c("nwrPerCor","task"), se = NULL,
           title="NWR x Task Interaction - Total Accuracy") + 
  theme_classic() + theme(text = element_text(size = 14)) 

#-----2e. Predicting task-specific accuracy-----

NMFUL_lang <- merge(NMFUL_lrt, lang_df, by="subject") 
UMFUL_lang <- merge(UMFUL_lrt, lang_df, by="subject") 
MLES_lang <- merge(MLES_lrt, lang_df, by="subject") 

full.nmful.lang <- lmer(total.x ~ 1 + WABAQ + sWt_original + pWt_original + (1|subject) + (1|item.x), data=NMFUL_lang) 
full.umful.lang <- lmer(total.x ~ 1 + WABAQ + sWt_original + pWt_original + (1|subject) + (1|item.x), data=UMFUL_lang) 
full.mles.lang <- lmer(total.x ~ 1 + WABAQ + sWt_original + pWt_original + (1|subject) + (1|item.x), data=MLES_lang) 
full.mles.lang.noP <- lmer(total.x ~ 1 + WABAQ + sWt_original + (1|subject) + (1|item.x), data=MLES_lang) 
anova(full.mles.lang,full.mles.lang.noP)


#-----BayesFactor-----

library(BayesFactor)

### p weight ###
lang_df$subject <- factor(lang_df$subject) #random effects must be factors
lang_df$item <- factor(lang_df$item)
# pWt.mod <- lmer(total ~ 1 + WABAQ + pWt_original*task + (1|subject) + (1|item), data=lang_df) # can also try refit pWt
full_BFpwt <- lmBF(total ~ 1 + WABAQ + pWt_original:task + pWt_original + task, data=lang_df, whichRandom = c('subject', 'id'))
# include interaction here?
null_BFpwt <- lmBF(total ~ 1 + WABAQ + pWt_original:task + task, data=lang_df, whichRandom = c('subject', 'id'))
full_BFpwt / null_BFpwt  # The Bayes factor in favor of the full model

# BRMS method:
# pwt.brm <- brm(total ~ 1 + WABAQ + pWt_original*task + (1|subject) + (1|item), data=lang_df, 
#                iter=5000, 
#                control = list(adapt_delta = .95),
#                cores = 4,
#                save_all_pars = TRUE)
# pp_check(pwt.brm, nsamples = 100)
# diagnostics (see code below)

### NWR ###
lang_df3$subject <- factor(lang_df3$subject) 
lang_df3$item <- factor(lang_df3$item)
nwr.mod <- lmer(total ~ 1 + WABAQ + nwrPerCor*task + (1|subject) + (1|item), data=lang_df3) 
full_BFnwr <- lmBF(total ~ 1 + WABAQ + nwrPerCor + task + nwrPerCor:task, data=lang_df3, whichRandom = c('subject', 'id'))
null_BFnwr <- lmBF(total ~ 1 + WABAQ + task + nwrPerCor:task, data=lang_df3, whichRandom = c('subject', 'id'))
full_BFnwr / null_BFnwr

nwr.brm <- brm(total ~ 1 + WABAQ + nwrPerCor*task + (1|subject) + (1|item), data=lang_df3, 
               iter=5000, 
               control = list(adapt_delta = .95),
               cores = 4,
               save_all_pars = TRUE)
# diagnostics
pp_check(nwr.brm, nsamples = 100) # posterior predictive check
plot(nwr.brm) # trace and density plots for all model parameters
# posterior distribution check for interaction effects
nwr.brm.nwr <- posterior_samples(nwr.brm, "nwrPerCor")
hist(tot.gr.brm.semGR$`b_taskNMFUL:semGR`)
ecdf(tot.gr.brm.semGR$`b_taskNMFUL:semGR`)(0) 
tot.gr.brm.spatGR <- posterior_samples(nwr.brm, "spatGR")
hist(tot.gr.brm.spatGR$`b_taskNMFUL:spatGR`)
ecdf(tot.gr.brm.spatGR$`b_taskNMFUL:spatGR`)(0) 

### controls ###

ctrl.mod <- lmer(total ~ 1 + group*task + (1|subject) + (1|item), data=final_df_lrt, control=lmerControl(optCtrl = list(maxfun=20000)))
final_df_lrt$subject <- factor(final_df_lrt$subject) 
final_df_lrt$item <- factor(final_df_lrt$item)
full_BFctrl <- lmBF(total ~ 1 + group:task + group + task, data=final_df_lrt, whichRandom = c('subject', 'id'))
null_BFctrl <- lmBF(total ~ 1 + group + task, data=final_df_lrt, whichRandom = c('subject', 'id'))
full_BFctrl / null_BFctrl

# control_df_lrt$subject <- factor(control_df_lrt$subject) 
# control_df_lrt$item <- factor(control_df_lrt$item)
# full_BFctrl2 <- lmBF(total ~ 1 + task, data=control_df_lrt, whichRandom = c('subject', 'item'))
# null_BFctrl2 <- lmBF(total ~ 1, data=control_df_lrt, whichRandom = c('subject', 'item'))
# full_BFctrl2 / null_BFctrl2

#-----Residuals for LSM-----

df <- read_csv("gestSem2.csv", col_types="fnnnnnnnnn")
#df <- na.omit(df)
#df <- df[!(df$subject==c("AB044","AB049","AB121")),] # pull out subjects who don't have UMFUL data (needed for both contrasts of interest)

tot.lm.NvU <- lm(NMFUL_total ~ UMFUL_total, data=df)
df$predicted.tNvU <- predict(tot.lm.NvU)   # Save the predicted values
df$residuals.tNvU <- residuals(tot.lm.NvU) # Save the residual values
tot.lm.UvM <- lm(UMFUL_total ~ MLES_total, data=df)
df$predicted.tUvM <- predict(tot.lm.UvM)    
df$residuals.tUvM <- residuals(tot.lm.UvM) 
tot.lm.NvM <- lm(NMFUL_total ~ MLES_total, data=df)
df$predicted.tNvM <- predict(tot.lm.NvM)    
df$residuals.tNvM <- residuals(tot.lm.NvM) 

HP.lm.NvU <- lm(NMFUL_HP ~ UMFUL_HP, data=df)
df$predicted.hpNvU <- predict(HP.lm.NvU)    
df$residuals.hpNvU <- residuals(HP.lm.NvU)  
HP.lm.UvM <- lm(UMFUL_HP ~ MLES_HP, data=df)
df$predicted.hpUvM <- predict(HP.lm.UvM)    
df$residuals.hpUvM <- residuals(HP.lm.UvM) 
HP.lm.NvM <- lm(NMFUL_HP ~ MLES_HP, data=df)
df$predicted.hpNvM <- predict(HP.lm.NvM)    
df$residuals.hpNvM <- residuals(HP.lm.NvM) 

write_csv(df, "~/OneDrive - Einstein Healthcare Network/Postdoc/MRRI Research/Semantics in Action/gestSem_LSM.csv")



library(ggpubr)
title <- "Total Accuracy Residual Distributions by Task Condition"
tgrob <- text_grob(title,size=14)
p0 <- as_ggplot(tgrob) #+ theme(plot.margin = margin(0,3,0,0, "cm"))
p1 <- ggplot(df, aes(x = MLES_total, y = UMFUL_total)) + theme_bw() + 
  #ggtitle("Meaningless vs. Unnamed Meaningful Total Accuracy") +
  xlab("Meaningless") + ylab("Unnamed Meaningful") +
  geom_point(size=2) + # Plot actual points
  geom_point(aes(y = predicted.tUvM), shape = 1, size=2) + # Plot predicted points
  geom_segment(aes(xend = MLES_total, yend = predicted.tUvM)) +
  geom_smooth(method = "lm", se = FALSE, color = "darkgrey", linetype="dotted") # Plot regression slope
p2 <- ggplot(df, aes(x = MLES_total, y = NMFUL_total)) + theme_bw() + 
  #ggtitle("Meaningless vs. Named Meaningful") +
  xlab("Meaningless") + ylab("Named Meaningful") +
  geom_point(size=2) + # Plot actual points
  geom_point(aes(y = predicted.tNvM), shape = 1, size=2) + # Plot predicted points
  geom_segment(aes(xend = MLES_total, yend = predicted.tNvM)) +
  geom_smooth(method = "lm", se = FALSE, color = "darkgrey", linetype="dotted") # Plot regression slope
p3 <- ggplot(df, aes(x = UMFUL_total, y = NMFUL_total)) + theme_bw() + 
  #ggtitle("Unnamed Meaningful vs. Named Meaningful Total Accuracy") +
  xlab("Unnamed Meaningful") + ylab("Named Meaningful") +
  geom_point(size=2) + # Plot actual points
  geom_point(aes(y = predicted.tNvU), shape = 1, size=2) + # Plot predicted points
  geom_segment(aes(xend = UMFUL_total, yend = predicted.tNvU)) +
  geom_smooth(method = "lm", se = FALSE, color = "darkgrey", linetype="dotted") # Plot regression slope
ggarrange(p0,p1,p2,p3,ncol=1,heights=c(1,5,5,5))


title2 <- "HP Accuracy Residual Distributions by Task Condition"
tgrob2 <- text_grob(title2,size=14)
p4 <- as_ggplot(tgrob2) #+ theme(plot.margin = margin(0,3,0,0, "cm"))
p5 <- ggplot(df, aes(x = MLES_HP, y = UMFUL_HP)) + theme_bw() + 
  #ggtitle("Meaningless vs. Unnamed Meaningful Total Accuracy") +
  xlab("Meaningless") + ylab("Unnamed Meaningful") +
  geom_point(size=2) + # Plot actual points
  geom_point(aes(y = predicted.hpUvM), shape = 1, size=2) + # Plot predicted points
  geom_segment(aes(xend = MLES_HP, yend = predicted.hpUvM)) +
  geom_smooth(method = "lm", se = FALSE, color = "darkgrey", linetype="dotted") # Plot regression slope
p6 <- ggplot(df, aes(x = MLES_HP, y = NMFUL_HP)) + theme_bw() + 
  #ggtitle("Meaningless vs. Named Meaningful") +
  xlab("Meaningless") + ylab("Named Meaningful") +
  geom_point(size=2) + # Plot actual points
  geom_point(aes(y = predicted.hpNvM), shape = 1, size=2) + # Plot predicted points
  geom_segment(aes(xend = MLES_HP, yend = predicted.hpNvM)) +
  geom_smooth(method = "lm", se = FALSE, color = "darkgrey", linetype="dotted") # Plot regression slope
p7 <- ggplot(df, aes(x = UMFUL_HP, y = NMFUL_HP)) + theme_bw() + 
  #ggtitle("Unnamed Meaningful vs. Named Meaningful Total Accuracy") +
  xlab("Unnamed Meaningful") + ylab("Named Meaningful") +
  geom_point(size=2) + # Plot actual points
  geom_point(aes(y = predicted.hpNvU), shape = 1, size=2) + # Plot predicted points
  geom_segment(aes(xend = UMFUL_HP, yend = predicted.hpNvU)) +
  geom_smooth(method = "lm", se = FALSE, color = "darkgrey", linetype="dotted") # Plot regression slope
ggarrange(p4,p5,p6,p7,ncol=1,heights=c(1,5,5,5))

title3 <- "Total Accuracy Residual Distributions"
tgrob3 <- text_grob(title3,size=14)
p8 <- as_ggplot(tgrob3) #+ theme(plot.margin = margin(0,3,0,0, "cm"))
p9 <- ggplot(df, aes(x=residuals.tNvU)) + geom_histogram() + ggtitle("Unnamed Meaningful vs Named Meaningful") +
  xlab("Residuals") + ylab("Frequency") + theme_classic() + xlim(-1.5,1.3)
p10 <- ggplot(df, aes(x=residuals.tUvM)) + geom_histogram() + ggtitle("Unnamed Meaningful vs Meaningless") +
  xlab("Residuals") + ylab("Frequency") + theme_classic() + xlim(-1.3,1.3)
p11 <- ggplot(df, aes(x=residuals.tNvM)) + geom_histogram() + ggtitle("Named Meaningful vs Meaningless") +
  xlab("Residuals") + ylab("Frequency") + theme_classic() + xlim(-1.3,1.3)
ggarrange(p8,p9,p10,p11,ncol=1,heights=c(1,5,5,5))

title4 <- "HP Accuracy Residual Distributions"
tgrob4 <- text_grob(title4,size=14)
p12 <- as_ggplot(tgrob4) #+ theme(plot.margin = margin(0,3,0,0, "cm"))
p13 <- ggplot(df, aes(x=residuals.hpNvU)) + geom_histogram() + ggtitle("Unnamed Meaningful vs Named Meaningful") +
  xlab("Residuals") + ylab("Frequency") + theme_classic() + xlim(-.8,.5)
p14 <- ggplot(df, aes(x=residuals.hpUvM)) + geom_histogram() + ggtitle("Unnamed Meaningful vs Meaningless") +
  xlab("Residuals") + ylab("Frequency") + theme_classic() + xlim(-.8,.5)
p15 <- ggplot(df, aes(x=residuals.hpNvM)) + geom_histogram() + ggtitle("Named Meaningful vs Meaningless") +
  xlab("Residuals") + ylab("Frequency") + theme_classic() + xlim(-.8,.5)
ggarrange(p12,p13,p14,p15,ncol=1,heights=c(1,5,5,5))



d <- read_csv("lm_data.csv", col_types = "ffnn")
d <- na.omit(d)
library(ggpubr)
px<-ggscatter(d2, x = "UMFUL_total", y = "NMFUL_total", 
              add = "reg.line", conf.int = TRUE, 
              cor.coef = TRUE, cor.method = "pearson",
              xlab = "Unnamed Meaningful", ylab = "Named Meaningful", title = "Total Accuracy") 
py<-ggscatter(d2, x = "UMFUL_HP", y = "NMFUL_HP", 
              add = "reg.line", conf.int = TRUE, 
              cor.coef = TRUE, cor.method = "pearson",
              xlab = "Unnamed Meaningful", ylab = "Named Meaningful", title = "HP Accuracy") 
egg::ggarrange(px,py)

tot.lm <- lm(total ~ task, data=d)
d$predicted <- predict(tot.lm)   # Save the predicted values
d$residuals <- residuals(tot.lm) # Save the residual values
# Quick look at the actual, predicted, and residual values
d %>% select(tot.lm, predicted, residuals) %>% head()
ggplot(d, aes(x = task, y = total)) + theme_bw() + ggtitle("Total Accuracy Residual Distribution by Task Condition") +
  geom_point() + # Plot actual points
  geom_point(aes(y = predicted), shape = 1, color="blue") + # Plot predicted points
  geom_segment(aes(xend = task, yend = predicted)) +
  geom_smooth(method = "lm", se = FALSE, color = "lightgrey") # Plot regression slope

d.mles <- subset(d, task=="MLES")
d.umful <- subset(d, task=="UMFUL")
d.nmful <- subset(d, task=="NMFUL")

p1 <- ggplot(d.mles, aes(x=residuals)) + geom_histogram() + ggtitle("Unnamed Meaningless Residual Distribution") +
  xlab("Residuals") + ylab("Frequency") + xlim(-3,1.1) + ylim(0,7) + theme_classic()
p2 <- ggplot(d.umful, aes(x=residuals)) + geom_histogram() + ggtitle("Unnamed Meaningful Residual Distribution") +
  xlab("Residuals") + ylab("Frequency") + xlim(-3,1.1) + ylim(0,7) + theme_classic()
p3 <- ggplot(d.nmful, aes(x=residuals)) + geom_histogram() + ggtitle("Named Meaningful Residual Distribution") +
  xlab("Residuals") + ylab("Frequency") + xlim(-3,1.1) + ylim(0,7) + theme_classic()
egg::ggarrange(p1,p2,p3)
d3 <- merge(d.umful$residuals, d.nmful$residuals)
p4 <- ggscatter(d3,x = "x", y = "y", 
                add = "reg.line", conf.int = TRUE, 
                cor.coef = TRUE, cor.method = "pearson",
                xlab = "Unnamed Meaningful", ylab = "Named Meaningful", title = "Residual Correlations") 

ggplot(d, aes(x = hp, y = mpg)) +
  geom_smooth(method = "lm", se = FALSE, color = "lightgrey") +  # Plot regression slope
  geom_segment(aes(xend = hp, yend = predicted), alpha = .2) +  # alpha to fade lines
  geom_point() +
  geom_point(aes(y = predicted), shape = 1) +
  theme_bw()  # Add theme for cleaner look

ggplot(d, aes(x = hp, y = mpg)) +
  geom_segment(aes(xend = hp, yend = predicted)) +
  geom_point() +
  geom_point(aes(y = predicted), shape = 1)

par(mfrow = c(2, 2))  # Split the plotting panel into a 2 x 2 grid
plot(tot.lm)  # Plot the model information
resid(tot.lm)
HP.lm <- lm(HP ~ task, data=d)
ggplot(d, aes(x = task, y = HP)) + theme_minimal() +
  geom_point() + # Plot actual points
  geom_point(aes(y = predicted), shape = 1) + # Plot predicted points
  geom_segment(aes(xend = task, yend = predicted, alpha=.2)) +
  geom_smooth(method = "lm", se = FALSE, color = "lightgrey") # Plot regression slope
residual(HP.lm)

#-----Trial-level gesture recognition-----

library(brms); library(bayesplot)
gr <- read_csv("GRtrial_followUp.csv", col_types="fffnnnn", na="NA")

total.gr <- lmer(total ~ task*spatGR + task*semGR + (1|subject), data=gr) 
HP.gr <- lmer(HP ~ task*spatGR + task*semGR + (1|subject), data=gr) 

tot.gr.brm <- brm(total ~ 1 + task*spatGR + task*semGR + (1|subject), data=gr, 
                  iter=5000, 
                  control = list(adapt_delta = .95),
                  cores = 4,
                  save_all_pars = TRUE)

# diagnostics
pp_check(tot.gr.brm, nsamples = 100) # posterior predictive check
plot(tot.gr.brm) # trace and density plots for all model parameters
# posterior distribution check for interaction effects
tot.gr.brm.semGR <- posterior_samples(tot.gr.brm, "semGR")
hist(tot.gr.brm.semGR$`b_taskNMFUL:semGR`)
ecdf(tot.gr.brm.semGR$`b_taskNMFUL:semGR`)(0) 
tot.gr.brm.spatGR <- posterior_samples(tot.gr.brm, "spatGR")
hist(tot.gr.brm.spatGR$`b_taskNMFUL:spatGR`)
ecdf(tot.gr.brm.spatGR$`b_taskNMFUL:spatGR`)(0) 

HP.gr.brm <- brm(HP ~ 1 + task*spatGR + task*semGR + (1|subject), data=gr, 
                 iter=5000, 
                 control = list(adapt_delta = .95),
                 cores = 4,
                 save_all_pars = TRUE)

# diagnostics
pp_check(HP.gr.brm, nsamples = 100) # posterior predictive check
plot(HP.gr.brm) # trace and density plots for all model parameters
# posterior distribution check for interaction effects
HP.gr.brm.semGR <- posterior_samples(HP.gr.brm, "semGR")
hist(HP.gr.brm.semGR$`b_taskNMFUL:semGR`)
ecdf(HP.gr.brm.semGR$`b_taskNMFUL:semGR`)(0) 
HP.gr.brm.spatGR <- posterior_samples(HP.gr.brm, "spatGR")
hist(HP.gr.brm.spatGR$`b_taskNMFUL:spatGR`)
ecdf(HP.gr.brm.spatGR$`b_taskNMFUL:spatGR`)(0) 




# old code:

HP.gr <- lmer(HP ~ task*spatGR + task*semGR + (1|subject), data=gr) 
mHP.gr <- lm(mHP ~ task*spatGR + task*semGR + item, data=gr) 

mHP.gr <- glmer(mHP ~ task*spatGR + task*semGR + (1|subject) + (1|item), data=gr, family=binomial) 
mHP.gr_noSp <- glmer(mHP ~ task*semGR + (1|subject) + (1|item), data=gr, family=binomial) 
mHP.gr_noSm <- glmer(mHP ~ task*spatGR + (1|subject) + (1|item), data=gr, family=binomial) 
mHP.gr_noT <- glmer(mHP ~ spatGR + semGR + (1|subject) + (1|item), data=gr, family=binomial) 
anova(mHP.gr,mHP.gr_noSp)
anova(mHP.gr,mHP.gr_noSm)
anova(mHP.gr,mHP.gr_noT)

mtot.gr <- lmer(mtot ~ task*spatGR + task*semGR + (1|subject) + (1|item), data=gr) 
mtot.gr_noSp <- lmer(mtot ~ task*semGR + (1|subject) + (1|item), data=gr) 
mtot.gr_noSm <- lmer(mtot ~ task*spatGR + (1|subject) + (1|item), data=gr) 
mtot.gr_noT <- lmer(mtot ~ spatGR + semGR + (1|subject) + (1|item), data=gr) 
anova(mtot.gr,mtot.gr_noSp)
anova(mtot.gr,mtot.gr_noSm)
anova(mtot.gr,mtot.gr_noT)
emmeans::emmeans(mtot.gr , pairwise~task*semGR, adjust="tukey")

gr %>%
  ggplot( aes(x=item, y=mtot, fill=item)) +
  geom_violin() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  geom_jitter(color="black", size=0.4, alpha=0.9) 

gr %>%
  ggplot( aes(x=item, y=mHP, fill=item)) +
  geom_violin() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  geom_jitter(color="black", size=0.4, alpha=0.9) 

# spatGR <- read_csv("spatialGR_trialAcc.csv")
# spatGR$Subject <- as.factor(spatGR$Subject)
# dSpat <- spatGR %>% group_by(Subject) %>%
#     summarise(across(2:27, mean)) 
# 
# dSpat <- spatGR %>% group_by(Subject) %>%
#   summarise(across(
#     .cols = c(2:29),  
#     .fns = list(Mean=mean, SD=sd, Min=min, Max=max), na.rm = TRUE, 
#     .names = "{col}_{fn}"
#   ))
# hist(spatGR)
# semGR <- read_csv("semanticGR_trialAcc.csv")
# semGR$Subject <- as.factor(semGR$Subject)


