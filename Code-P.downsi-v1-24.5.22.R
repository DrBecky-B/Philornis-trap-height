###### Code accompanying "Using modified trapping regimes to understand the spatial and nutritional ecology of the Avian vampire fly, Philornis downsi"
###### v1-24.5.22

library(mctest)
library(glmmTMB)
library(DHARMa)
library(car)
library(stats)
library(dplyr)
library(ggplot2)
library(mgcv)
library(lme4)
library(binom)

#Read in the data 
Q1.Q3.T3T4.weather <- read.delim("~/Q1.Q3-T3T4-weather-amalg.March.2021.txt")
#Convert trap ID into a factor
Q1.Q3.T3T4.weather$TrapIDF <- as.factor(Q1.Q3.T3T4.weather$TrapID)

### PHILORNIS DOWNSI
# Test for effects of trap height, bait type and weather on P.DOWNSI trap rates
# first check weather variables for multicollinearity
weather.glmm <- lm(P.downsi ~ Temp + Precip + Humidity, data=Q1.Q3.T3T4.weather)
omcdiag(weather.glmm)
#Only 1/6 tests detected collinearity suggests multicollinearity probs not an issue 

# Full model for P. downsi
PD.mod.full <- glmmTMB(P.downsi ~ Trap*Bait + Trap*Temp + Trap*Precip + Trap*Humidity + Study.week + (1|TrapID), data=Q1.Q3.T3T4.weather,
                            family = nbinom1)
#test model fit using DHARMa
simulationOutputPd <- simulateResiduals(fittedModel = PD.mod.full, plot = T, use.u = T)
#test whether zero-inflated
testZeroInflation(simulationOutputPd)

#Get test statistics and P values
car::Anova(PD.mod.full)

#Make figures

#fig 1a

P.downsi_sum <- Q1.Q3.T3T4.weather %>%
  group_by(Trap, Bait) %>%
  summarise( 
    n=n(),
    mean=mean(P.downsi),
    sd=sd(P.downsi)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1))

P.downsi.fig <-  ggplot(P.downsi_sum, aes(x=Trap, y=mean, colour=Bait)) +
	geom_errorbar( aes(x=Trap, ymin=mean-ic, ymax=mean+ic), width=0.1) +
		geom_point() + xlab("Trap height") + ylab("#P. downsi") + scale_x_discrete(labels = c('3.1-5.5m', '5.1-7.5m', '7.1-9.5m', '9.1-11.5m', '11.1-13m'))

P.downsi.fig <- P.downsi.fig + theme_classic()
P.downsi.fig + theme(axis.title=element_text(face="bold.italic",
	size="12", color="black"), legend.position="top")
                                                                                   
#fig 2a
Temp.P.downsi.fig <- ggplot(data = Q1.Q3.T3T4.weather, aes(x = Temp, y = P.downsi, colour=Trap)) +
	geom_smooth(method = "glm", alpha=0.2)
Temp.P.downsi.fig <- Temp.P.downsi.fig + theme_classic()
Temp.P.downsi.fig + theme(axis.title=element_text(face="bold.italic", size="12", color="black"), legend.position="top")
cols <- c("A" = "red", "B" = "green", "C" = "yellow", "D" = "blue", "E" = "pink")
Temp.P.downsi.fig  + labs(colour = "Trap height") + xlab("Temperature (°C)") + ylab("# P. downsi") + scale_colour_manual(values = cols,
	labels = c("3.1-5.5m", "5.1-7.5m", "7.1-9.5m", "9.1-11.5m", "11.1-13m")) + 
		theme(axis.title=element_text(face="bold.italic",  size="12", color="black"), legend.position="top")     

#fig 2b                                                                                                                                                                                                                          #fig 2b
Rain.P.downsi.fig <- ggplot(data = Q1.Q3.T3T4.weather, aes(x = Precip, y = P.downsi, colour=Trap)) +
	geom_smooth(method = "glm", alpha=0.2)
Rain.P.downsi.fig <- Rain.P.downsi.fig + theme_classic()
Rain.P.downsi.fig + theme(axis.title=element_text(face="bold.italic", size="12", color="black"), legend.position="top")
cols <- c("A" = "red", "B" = "green", "C" = "yellow", "D" = "blue", "E" = "pink")
Rain.P.downsi.fig  + labs(colour = "Trap height") + xlab("Rainfall (mm)") + ylab("# P. downsi") + scale_colour_manual(values = cols, 
labels = c("3.1-5.5m", "5.1-7.5m", "7.1-9.5m", "9.1-11.5m", "11.1-13m")) + 
		theme(axis.title=element_text(face="bold.italic",  size="12", color="black"), legend.position="top")  

#fig 2c
Humid.P.downsi.fig <- ggplot(data = Q1.Q3.T3T4.weather, aes(x = Humidity, y = P.downsi, colour=Trap)) +
  geom_smooth(method = "glm", alpha=0.2)
Humid.P.downsi.fig <- Humid.P.downsi.fig + theme_classic()
cols <- c("A" = "red", "B" = "green", "C" = "yellow", "D" = "blue", "E" = "pink")
Humid.P.downsi.fig  + labs(colour = "Trap height") + xlab("% Humidity") + ylab("# P. downsi") + scale_colour_manual(values = cols,
	labels = c("3.1-5.5m", "5.1-7.5m", "7.1-9.5m", "9.1-11.5m", "11.1-13m")) + 
		theme(axis.title=element_text(face="bold.italic",  size="12", color="black"), legend.position="top")       
                                                                                                                                                                                                         
###MOTHS
# Test for effects of trap height, bait type and weather on MOTH trap rates
# first check weather variables for multicollinearity
weather.glmm <- lm(Moths ~ Temp + Precip + Humidity, data=Q1.Q3.T3T4.weather)
omcdiag(weather.glmm)
#Only 1/6 tests detected collinearity suggests multicollinearity probs not an issue 

#full model for moths
Moths.mod.full <- glmmTMB(Moths ~ Trap*Bait + Trap*Precip + Trap*Humidity + Trap*Temp + Study.week + (1|TrapID) , family = nbinom1, data=Q1.Q3.T3T4.weather)
#test model fit using DHARMa
SimulationOutputMoths <- simulateResiduals(Moths.mod.full, plot = T)
#test whether zero-inflated
testZeroInflation(SimulationOutputMoths)
#Get test statistics and P values
car::Anova(Moths.mod.full) 

#Make moth figures

#fig 1b
moth_sum <- Q1.Q3.T3T4.weather %>%
  group_by(Trap, Bait) %>%
  summarise( 
    n=n(),
    mean=mean(Moths),
    sd=sd(Moths)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1))

Moths.fig <-  ggplot(moth_sum, aes(x=Trap, y=mean, colour=Bait)) +
	geom_errorbar( aes(x=Trap, ymin=mean-ic, ymax=mean+ic), width=0.1) +
	geom_point() + xlab("Trap height") + ylab("# Moths") + scale_x_discrete(labels = c('3.1-5.5m', '5.1-7.5m', '7.1-9.5m', '9.1-11.5m', '11.1-13m'))
Moths.fig <- Moths.fig + theme_classic()
Moths.fig + theme(axis.title=element_text(face="bold.italic",
                                         size="12", color="black"), legend.position = "top")

#fig 2e
Temp.Moths.fig <- ggplot(data = Q1.Q3.T3T4.weather, aes(x = Temp, y = Moths, colour=Trap)) +
	geom_smooth(method = "glm", alpha=0.2)
Temp.Moths.fig <- Temp.Moths.fig + theme_classic()
Temp.Moths.fig + theme(axis.title=element_text(face="bold.italic", size="12", color="black"), legend.position="top")
cols <- c("A" = "red", "B" = "green", "C" = "yellow", "D" = "blue", "E" = "pink")
Temp.Moths.fig  + labs(colour = "Trap height") + xlab("Temperature (°C)") + ylab("# P. downsi") + scale_colour_manual(values = cols,
	labels = c("3.1-5.5m", "5.1-7.5m", "7.1-9.5m", "9.1-11.5m", "11.1-13m")) + 
		theme(axis.title=element_text(face="bold.italic",  size="12", color="black"), legend.position="top")     
                                                                                                                                                                                                                          
#fig 2f
Rain.Moths.fig <- ggplot(data = Q1.Q3.T3T4.weather, aes(x = Precip, y = Moths, colour=Trap)) +
	geom_smooth(method = "glm", alpha=0.2)
Rain.Moths.fig <- Rain.Moths.fig + theme_classic()
Rain.Moths.fig + theme(axis.title=element_text(face="bold.italic", size="12", color="black"), legend.position="top")
cols <- c("A" = "red", "B" = "green", "C" = "yellow", "D" = "blue", "E" = "pink")
Rain.Moths.fig  + labs(colour = "Trap height") + xlab("Rainfall (mm)") + ylab("# P. downsi") + scale_colour_manual(values = cols, 
labels = c("3.1-5.5m", "5.1-7.5m", "7.1-9.5m", "9.1-11.5m", "11.1-13m")) + 
		theme(axis.title=element_text(face="bold.italic",  size="12", color="black"), legend.position="top")  

#fig 2g
Humid.Moths.fig <- ggplot(data = Q1.Q3.T3T4.weather, aes(x = Humidity, y = Moths, colour=Trap)) +
  geom_smooth(method = "glm", alpha=0.2)
Humid.Moths.fig <- Humid.Moths.fig + theme_classic()
cols <- c("A" = "red", "B" = "green", "C" = "yellow", "D" = "blue", "E" = "pink")
Humid.Moths.fig  + labs(colour = "Trap height") + xlab("% Humidity") + ylab("# P. downsi") + scale_colour_manual(values = cols,
	labels = c("3.1-5.5m", "5.1-7.5m", "7.1-9.5m", "9.1-11.5m", "11.1-13m")) + 
		theme(axis.title=element_text(face="bold.italic",  size="12", color="black"), legend.position="top")  

                   
####DIPTERA
# Test for effects of trap height, bait type and weather on DIPTERA trap rates
# first check weather variables for multicollinearity
weather.glmm <- lm(Diptera ~ Temp + Precip + Humidity, data=Q1.Q3.T3T4.weather)
omcdiag(weather.glmm)
#Only 1/6 tests detected collinearity suggests multicollinearity probs not an issue 
#note that one missing value for other Diptera; date 22/5/2018, trap E (top trap) pole 10, originally Diptera = a; typo from original dataset, now NA

#full model for Diptera
Diptera.mod.full <- glmmTMB(Diptera ~ Trap*Bait + Trap*Precip + Trap*Humidity + Trap*Temp + Study.week + (1|TrapID), family = nbinom2, data=Q1.Q3.T3T4.weather)
#test model fit using DHARMa
SimulationOutputDiptera <- simulateResiduals(Diptera.mod.full, plot = T)
#test whether zero-inflated
testZeroInflation(SimulationOutputDiptera)
#Get test statistics and P values
car::Anova(Diptera.mod.full)

#Make Diptera figures

#fig 1c
Diptera_sum <- Q1.Q3.T3T4.weather %>%
  group_by(Trap, Bait) %>%
  summarise( 
    n=n(),
    mean=mean(Diptera),
    sd=sd(Diptera)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1))

Diptera.fig <-  ggplot(Diptera_sum, aes(x=Trap, y=mean, colour=Bait)) +
  geom_errorbar( aes(x=Trap, ymin=mean-ic, ymax=mean+ic), width=0.1) +
  geom_point() + xlab("Trap height") + ylab("# Moths") + scale_x_discrete(labels = c('3.1-5.5m', '5.1-7.5m', '7.1-9.5m', '9.1-11.5m', '11.1-13m'))
Diptera.fig <- Diptera.fig + theme_classic()
Diptera.fig + theme(axis.title=element_text(face="bold.italic",
                                          size="12", color="black"), legend.position = "top")

#fig 2h
Temp.Diptera.fig <- ggplot(data = Q1.Q3.T3T4.weather, aes(x = Temp, y = Diptera, colour=Trap)) +
	geom_smooth(method = "glm", alpha=0.2)
Temp.Diptera.fig <- Temp.Diptera.fig + theme_classic()
Temp.Diptera.fig + theme(axis.title=element_text(face="bold.italic", size="12", color="black"), legend.position="top")
cols <- c("A" = "red", "B" = "green", "C" = "yellow", "D" = "blue", "E" = "pink")
Temp.Diptera.fig  + labs(colour = "Trap height") + xlab("Temperature (°C)") + ylab("# P. downsi") + scale_colour_manual(values = cols,
	labels = c("3.1-5.5m", "5.1-7.5m", "7.1-9.5m", "9.1-11.5m", "11.1-13m")) + 
		theme(axis.title=element_text(face="bold.italic",  size="12", color="black"), legend.position="top")     

                                                                                                                                                                                                                   
#fig 2i
Rain.Diptera.fig <- ggplot(data = Q1.Q3.T3T4.weather, aes(x = Precip, y = Diptera, colour=Trap)) +
	geom_smooth(method = "glm", alpha=0.2)
Rain.Diptera.fig <- Rain.Diptera.fig + theme_classic()
Rain.Diptera.fig + theme(axis.title=element_text(face="bold.italic", size="12", color="black"), legend.position="top")
cols <- c("A" = "red", "B" = "green", "C" = "yellow", "D" = "blue", "E" = "pink")
Rain.Diptera.fig  + labs(colour = "Trap height") + xlab("Rainfall (mm)") + ylab("# P. downsi") + scale_colour_manual(values = cols, 
labels = c("3.1-5.5m", "5.1-7.5m", "7.1-9.5m", "9.1-11.5m", "11.1-13m")) + 
		theme(axis.title=element_text(face="bold.italic",  size="12", color="black"), legend.position="top")  

#fig 2j
Humid.Diptera.fig <- ggplot(data = Q1.Q3.T3T4.weather, aes(x = Humidity, y = Diptera, colour=Trap)) +
  geom_smooth(method = "glm", alpha=0.2)
Humid.Diptera.fig <- Humid.Diptera.fig + theme_classic()
cols <- c("A" = "red", "B" = "green", "C" = "yellow", "D" = "blue", "E" = "pink")
Humid.Diptera.fig  + labs(colour = "Trap height") + xlab("% Humidity") + ylab("# P. downsi") + scale_colour_manual(values = cols,
	labels = c("3.1-5.5m", "5.1-7.5m", "7.1-9.5m", "9.1-11.5m", "11.1-13m")) + 
		theme(axis.title=element_text(face="bold.italic",  size="12", color="black"), legend.position="top")  

#POLISTES VERSICOLOR
# Test for effects of trap height, bait type and weather on POLISTES VERSICOLOR trap rates
# first check weather variables for multicollinearity
weather.glmm.P..versicolor <- lm(P..versicolor ~ Temp + Precip + Humidity, data=Q1.Q3.T3T4.weather)
omcdiag(weather.glmm.P..versicolor)
#Only 1/6 tests detected collinearity suggests multicollinearity not an issue 
#full model for P.versicolor
Full.wasp.mod <- glmmTMB(P..versicolor ~ Trap*Bait + Trap*Precip + Trap*Humidity + Trap*Temp + Study.week + (1|TrapID), family = nbinom1, data=Q1.Q3.T3T4.weather)
#test model fit using DHARMa
SimulationOutputWasps <- simulateResiduals(Full.wasp.mod, plot = T)
#Test whether zero-inflated
testZeroInflation(SimulationOutputWasps)
#get test statistics and P values
car::Anova(Full.wasp.mod)


#Make P. versicolor figures
#fig 1d
P..versicolor_sum <- Q1.Q3.T3T4.weather %>%
  group_by(Trap, Bait) %>%
  summarise( 
    n=n(),
    mean=mean(P..versicolor),
    sd=sd(P..versicolor)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1))


P..versicolor.fig <-  ggplot(P..versicolor_sum, aes(x=Trap, y=mean, colour=Bait)) +
  geom_errorbar( aes(x=Trap, ymin=mean-ic, ymax=mean+ic), width=0.1) +
  geom_point() + xlab("Trap height") + ylab("# P. versicolor") + scale_x_discrete(labels = c('3.1-5.5m', '5.1-7.5m', '7.1-9.5m', '9.1-11.5m', '11.1-13m'))
P..versicolor.fig <- P..versicolor.fig + theme_classic()
P..versicolor.fig + theme(axis.title=element_text(face="bold.italic",
                                                  size="12", color="black"), legend.position = "top")

#fig 2k
Temp.P..versicolor.fig <- ggplot(data = Q1.Q3.T3T4.weather, aes(x = Temp, y = P..versicolor, colour=Trap)) +
	geom_smooth(method = "glm", alpha=0.2)
Temp.P..versicolor.fig <- Temp.P..versicolor.fig + theme_classic()
Temp.P..versicolor.fig + theme(axis.title=element_text(face="bold.italic", size="12", color="black"), legend.position="top")
cols <- c("A" = "red", "B" = "green", "C" = "yellow", "D" = "blue", "E" = "pink")
Temp.P..versicolor.fig  + labs(colour = "Trap height") + xlab("Temperature (°C)") + ylab("# P. downsi") + scale_colour_manual(values = cols,
	labels = c("3.1-5.5m", "5.1-7.5m", "7.1-9.5m", "9.1-11.5m", "11.1-13m")) + 
		theme(axis.title=element_text(face="bold.italic",  size="12", color="black"), legend.position="top")     

                                                                                                                                                                                                                   
#fig 2l
Rain.P..versicolor.fig <- ggplot(data = Q1.Q3.T3T4.weather, aes(x = Precip, y = P..versicolor, colour=Trap)) +
	geom_smooth(method = "glm", alpha=0.2)
Rain.P..versicolor.fig <- Rain.P..versicolor.fig + theme_classic()
Rain.P..versicolor.fig + theme(axis.title=element_text(face="bold.italic", size="12", color="black"), legend.position="top")
cols <- c("A" = "red", "B" = "green", "C" = "yellow", "D" = "blue", "E" = "pink")
Rain.P..versicolor.fig  + labs(colour = "Trap height") + xlab("Rainfall (mm)") + ylab("# P. downsi") + scale_colour_manual(values = cols, 
labels = c("3.1-5.5m", "5.1-7.5m", "7.1-9.5m", "9.1-11.5m", "11.1-13m")) +
		theme(axis.title=element_text(face="bold.italic",  size="12", color="black"), legend.position="top")  

#fig 2m
Humid.P..versicolor.fig <- ggplot(data = Q1.Q3.T3T4.weather, aes(x = Humidity, y = P..versicolor, colour=Trap)) +
  geom_smooth(method = "glm", alpha=0.2)
Humid.P..versicolor.fig <- Humid.P..versicolor.fig + theme_classic()
cols <- c("A" = "red", "B" = "green", "C" = "yellow", "D" = "blue", "E" = "pink")
Humid.P..versicolor.fig  + labs(colour = "Trap height") + xlab("% Humidity") + ylab("# P. downsi") + scale_colour_manual(values = cols,
	labels = c("3.1-5.5m", "5.1-7.5m", "7.1-9.5m", "9.1-11.5m", "11.1-13m")) + 
		theme(axis.title=element_text(face="bold.italic",  size="12", color="black"), legend.position="top")  
                     
##### P.downsi sex ratio
# Test whether the sex ratio of P.downsi varies across trap height or with bait type
#Full sex ratio model for P. downsi
P.downsi.sex.ratio <- glmer(cbind(MPD,FPD) ~ Trap*Bait + Precip + Humidity + Temp + Study.week + (1|TrapID), family =  binomial(), data=Q1.Q3.T3T4.weather)
#test model fit using DHARMa
SimulationOutputsr <- simulateResiduals(P.downsi.sex.ratio, plot = T)
#Get test statistics and P values
car::Anova(P.downsi.sex.ratio)

#make figures
#fig 3

P.downsi_males<- Q1.Q3.T3T4.weather %>%
  group_by(Trap, Bait) %>%
  summarise( 
    sum=sum(MPD))
P.downsi_total<- Q1.Q3.T3T4.weather %>%
  group_by(Trap, Bait) %>%
  summarise( 
    sum=sum(P.downsi))

P.downsi_total$binomial_CI <- binom.confint(P.downsi_males$sum, P.downsi_total$sum, conf.level = 0.95, methods = "logit")
write.table(P.downsi_total, "~/SR.txt")
#Need to edit this doc in excel to remove weird symbols

SR <- read.delim("~/SR.txt")
P.downsi.SR <- ggplot(SR, aes(x=Trap, y=binomial_CI.mean, colour=Bait)) +
    geom_errorbar(aes(x=Trap, ymin=binomial_CI.lower, ymax=binomial_CI.upper), width=0.1) +
           geom_point() + xlab("Trap height") + ylab("Sex ratio") + scale_x_discrete(labels = c('3.1-5.5m', '5.1-7.5m', '7.1-9.5m', '9.1-11.5m', '11.1-13m'))
P.downsi.SR <- P.downsi.SR + theme_classic()
P.downsi.SR + theme(axis.title=element_text(face="bold.italic",
size="12", color="black"), legend.position="top") + labs(colour = "Sex ratio") + xlab("Trap height")      
       
###Effect of temperature on the sex ratio 
#fig 4

P.downsi_males.2<- Q1.Q3.T3T4.weather %>%
  group_by(Temp) %>%
  summarise( 
    sum=sum(MPD))
P.downsi_total.2<- Q1.Q3.T3T4.weather %>%
  group_by(Temp) %>%
  summarise( 
    sum=sum(P.downsi))

P.downsi_total.2$binomial_CI <- binom.confint(P.downsi_males.2$sum, P.downsi_total.2$sum, conf.level = 0.95, methods = "logit")
write.table(P.downsi_total.2, "~/SR.temp.txt")
#Need to edit this doc in excel to remove weird symbols
SR.temp <- read.delim("~/SR.temp.txt")

P.downsi.SR.temp <- ggplot(data = SR.temp, aes(x = Temp, y = binomial_CI.mean, size = sum)) + geom_point() +
        geom_smooth(method = "glm", alpha=0.2)
P.downsi.SR.temp  <- P.downsi.SR.temp  + theme_classic()
P.downsi.SR.temp + theme(axis.title=element_text(face="bold.italic",
        size="12", color="black"), legend.position="top") + ylab("Sex ratio") + xlab("Temperature (°C)")


####### Seasonal changes in trap rates at different heights using GAMMs
###### Seasonal changes in P. downsi trap rates
# Best model fit is negbin, theta = 0.1
# run 2 gamms, first (PD.gamm.1) has only week as a predictor, 2nd (PD.gamm.2) has trap height as a factor
PD.gamm.1 <- gamm(P.downsi ~ s(WoY, fx = FALSE, k=-1, bs = "ds"), random = list(TrapID=~1), 
                     family = negbin(0.1), data = Q1.Q3.T3T4.weather)
PD.gamm.2 <- gamm(P.downsi ~ s(WoY, fx = FALSE, k=-1, bs = "ds", by = Trap.Height), random = list(TrapID=~1), 
                         family = negbin(0.1), data = Q1.Q3.T3T4.weather)
#Test which gamm fits better based on lowest AIC
AIC(PD.gamm.1$lme,PD.gamm.2$lme)

#get diagnostics for gams
gam.check(PD.gamm.1$gam)
gam.check(PD.gamm.2$gam)

#get summary statistics for gamms
summary(PD.gamm.1$gam)
summary(PD.gamm.2$gam)
#use summary of PD.gamm.2 for table 2

#make figure 5a
P.downsi.season.fig <- ggplot(data = Q1.Q3.T3T4.weather, aes(x = WoY, y = P.downsi, colour=Trap)) +
  geom_smooth(method = "loess", alpha=0.2)
P.downsi.season.fig <- P.downsi.season.fig + theme_classic() + 
  theme(axis.title=element_text(face="bold.italic", size="12", color="black"), legend.position="top")
cols <- c("A" = "red", "B" = "green", "C" = "yellow", "D" = "blue", "E" = "pink")
P.downsi.season.fig + labs(colour = "Trap height") +  xlab("Week of Year") + ylab("# P. downsi") + scale_colour_manual(values = cols, labels = c("3.1-5.5m", "5.1-7.5m", "7.1-9.5m", "9.1-11.5m", "11.1-13m"))              


#Test whether male and female P. downsi have different seasonal patterns using gamms
Sex.GAMM <- read.delim("~/Sex.GAMM-3.4.txt")
Sex.GAMM$P.downsin <- as.numeric(Sex.GAMM$PD)
# run 2 gamms, first (PD.gamm.sex.1) has only week as a predictor, 2nd (PD.gamm.sex.2) has sex (male or female) as a factor
PD.GAMM.sex.1 <- gamm(P.downsin ~ s(WoY, fx = FALSE, k=-1, bs = "ds"), random = list(TrapID=~1), 
                      family = negbin(0.1), data = Sex.GAMM)
PD.GAMM.sex.2 <- gamm(P.downsin ~ s(WoY, fx = FALSE, k=-1, bs = "ds", by = Sex), random = list(TrapID=~1), 
                      family = negbin(0.1), data = Sex.GAMM)

#Test which gamm fits better based on lowest AIC
AIC(PD.GAMM.sex.1$lme, PD.GAMM.sex.2$lme) 

#get diagnostics for gams
gam.check(PD.GAMM.sex.1$gam)
gam.check(PD.GAMM.sex.2$gam)

#get summary statistics for gamms
summary(PD.GAMM.sex.1$gam)
summary(PD.GAMM.sex.2$gam)

#MOTHS
###### Seasonal changes in moth trap rates
# Best model fit is poisson with OLRE
# run 2 gamms, first (Moths.GAMM.1) has only week as a predictor, 2nd (Moths.GAMM.2) has trap height as a factor
Moths.GAMM.1 <- gamm(Moths ~ s(WoY, fx = FALSE, k=-1, bs = "ds"), random = list(TrapID=~1, OLRE=~1),
                     family = poisson(link = "log"), data = Q1.Q3.T3T4.weather)
Moths.GAMM.2 <- gamm(Moths ~ s(WoY, fx = FALSE, k=-1, bs = "ds", by = Trap.Height), random = list(TrapID=~1, OLRE=~1),
                     family = poisson(link = "log"), data = Q1.Q3.T3T4.weather)

#Test which gamm fits better based on lowest AIC
AIC(Moths.GAMM.1$lme,Moths.GAMM.2$lme)

#get diagnostics for gams
gam.check(Moths.GAMM.1$gam)
gam.check(Moths.GAMM.2$gam)

#get summary statistics for gamms
summary(Moths.GAMM.1$gam)
summary(Moths.GAMM.2$gam)
#use summary of Moths.GAMM.2 for table 2

#fig 5b
Moths.season.fig <- ggplot(data = Q1.Q3.T3T4.weather, aes(x = WoY, y = Moths, colour=Trap)) +
    geom_smooth(method = "loess", alpha=0.2)
Moths.season.fig <- Moths.season.fig + theme_classic() + theme(axis.title=element_text(face="bold.italic",
    size="12", color="black"), legend.position="top")
cols <- c("A" = "red", "B" = "green", "C" = "yellow", "D" = "blue", "E" = "pink")
Moths.season.fig + labs(colour = "Trap height") +  xlab("Week of Year") + ylab("# Moths") + scale_colour_manual(values = cols, labels = c("3.1-5.5m", "5.1-7.5m", "7.1-9.5m", "9.1-11.5m", "11.1-13m"))              


#DIPTERA
###### Seasonal changes in trap rates of other Diptera
# Best model fit is poisson with OLRE
# run 2 gamms, first (Diptera.GAMM.1) has only week as a predictor, 2nd (Diptera.GAMM.2) has trap height as a factor

Diptera.GAMM.1 <- gamm(Diptera ~ s(WoY, fx = FALSE, k=-1, bs = "ds"), random = list(TrapID=~1, OLRE=~1),
                     family = poisson(link = "log"), data = Q1.Q3.T3T4.weather)
Diptera.GAMM.2 <- gamm(Diptera ~ s(WoY, fx = FALSE, k=-1, bs = "ds", by = Trap.Height), random = list(TrapID=~1, OLRE=~1),
                     family = poisson(link = "log"), data = Q1.Q3.T3T4.weather)

#Test which gamm fits better based on lowest AIC
AIC(Diptera.GAMM.1$lme,Diptera.GAMM.2$lme)

#get diagnostics for gams
gam.check(Diptera.GAMM.1$gam)
gam.check(Diptera.GAMM.2$gam)

#get summary statistics for gamms
summary(Diptera.GAMM.1$gam)
summary(Diptera.GAMM.2$gam)
#use summary of Moths.GAMM.2 for table 2

#fig5c
Diptera.season.fig <- ggplot(data = Q1.Q3.T3T4.weather, aes(x = WoY, y = Diptera, colour=Trap)) +
    geom_smooth(method = "loess", alpha=0.2)
Diptera.season.fig <- Diptera.season.fig + theme_classic() + theme(axis.title=element_text(face="bold.italic",
    size="12", color="black"), legend.position="top")
cols <- c("A" = "red", "B" = "green", "C" = "yellow", "D" = "blue", "E" = "pink")
Diptera.season.fig + labs(colour = "Trap height") +  xlab("Week of Year") + ylab("# Diptera") + scale_colour_manual(values = cols, labels = c("3.1-5.5m", "5.1-7.5m", "7.1-9.5m", "9.1-11.5m", "11.1-13m"))   
#note that one missing value for other Diptera; date 22/5/2018, trap E (top trap) pole 10, originally Diptera = a; typo from original dataset, now NA


#POLISTES VERSICOLOR
###### Seasonal changes in trap rates of other P. versicolor
# Best model fit is negbin with theta = 0.4
# run 2 gamms, first (Wasp.gamm.1) has only week as a predictor, 2nd (Wasp.gamm.2) has trap height as a factor

Wasp.gamm.1 <- gamm(P..versicolor ~ s(WoY, fx = FALSE, k=-1, bs = "ds"), random = list(TrapID=~1), 
                  family = negbin(0.4), data = Q1.Q3.T3T4.weather)
Wasp.gamm.2 <- gamm(P..versicolor ~ s(WoY, fx = FALSE, k=-1, bs = "ds", by = Trap.Height), random = list(TrapID=~1), 
                  family = negbin(0.4), data = Q1.Q3.T3T4.weather)


#Test which gamm fits better based on lowest AIC
AIC(Wasp.gamm.1$lme,Wasp.gamm.2$lme)

#get diagnostics for gams
gam.check(Wasp.gamm.1$gam)
gam.check(Wasp.gamm.2$gam)

#get summary statistics for gamms
summary(Wasp.gamm.1$gam)
summary(Wasp.gamm.2$gam)
#use summary of Moths.GAMM.2 for table 2

#fig 5d
Wasp.season.fig <- ggplot(data = Q1.Q3.T3T4.weather, aes(x = WoY, y = P..versicolor, colour=Trap)) +
    geom_smooth(method = "loess", alpha=0.2)
Wasp.season.fig <- Wasp.season.fig + theme_classic() + theme(axis.title=element_text(face="bold.italic",
    size="12", color="black"), legend.position="top")
cols <- c("A" = "red", "B" = "green", "C" = "yellow", "D" = "blue", "E" = "pink")
Wasp.season.fig + labs(colour = "Trap height") +  xlab("Week of Year") + ylab("# P. versicolor") + scale_colour_manual(values = cols, labels = c("3.1-5.5m", "5.1-7.5m", "7.1-9.5m", "9.1-11.5m", "11.1-13m"))   

