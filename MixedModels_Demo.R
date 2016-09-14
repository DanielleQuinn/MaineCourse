# ---- Description ----
# This is demonstration code for mixed effect models

# ---- Load Packages ----
library(lmtest)
library(ggplot2)
library(dplyr)
library(nlme)

# ---- Import Data ----
rikz<-read.delim("RIKZ.txt")

# ---- Manipulate Data ----
# Change Beach to factor
rikz$fBeach<-as.factor(rikz$Beach)

# Filter out NAs
rikz<-rikz%>%
  filter(!is.na(Beach))%>%
  filter(!is.na(NAP))%>%
  filter(!is.na(Richness))

# ---- Research Question ----
# How does species richness relate to NAP levels, 
# and how does this relationship vary across
# different beaches?

# ---- Basic Visualization ----
base.rikz<-ggplot(rikz)+
  geom_point(aes(x=NAP, y=Richness, col=fBeach), size=2)+
  theme_bw(20)

# ---- Linear Model: Set Up ----
m0<-lm(Richness~NAP, data=rikz)
summary(m0)

# ---- Linear Model: Diagnostics ----
## Diagnostics 1: Residuals vs Fitted Values ##
# Create data frame of residuals and fitted values
diag.m0<-data.frame(residuals=resid(m0), fitted=fitted(m0))

# Visualize fitted values vs residuals
ggplot(diag.m0)+
  geom_point(aes(x=fitted, y=residuals), size=3)+
  geom_hline(yintercept=0, linetype="dashed", col="blue")+
  theme_bw(16)+ylab("Residuals")+xlab("Fitted Values)")

## Diagnostics 2: Dispersion ##
dispersion(m0)

# ---- Adding a Factor ----
# fBeach
pr.fac(m0,rikz$fBeach, factorname="fBeach")
# Recall that these should be each normally 
# distributed around zero

# ---- Linear Model + Factor: Set Up ----
m1<-lm(Richness~NAP+fBeach, data=rikz)
summary(m1)

# ---- Linear Model + Factor: Diagnostics ----
## Diagnostics 1: Residuals vs Fitted Values ##
# Create data frame of residuals and fitted values
diag.m1<-data.frame(residuals=resid(m1), fitted=fitted(m1))

# Visualize fitted values vs residuals
ggplot(diag.m1)+
  geom_point(aes(x=fitted, y=residuals), size=3)+
  geom_hline(yintercept=0, linetype="dashed", col="blue")+
  theme_bw(16)+ylab("Residuals")+xlab("Fitted Values)")

## Diagnostics 2: Dispersion ##
dispersion(m1)

# ---- Linear Model + Factor: Fitted Line ----
output.m1<-expand.grid(fBeach=unique(rikz$fBeach),
                       NAP=seq(from=min(rikz$NAP),
                               to=max(rikz$NAP),
                               length=4))
output.m1$predicted<-predict(m1, output.m1)

base.rikz+
  geom_line(aes(x=NAP, y=predicted, col=fBeach), data=output.m1)

# ---- Random Intercept Model: Set Up ----
rim<-lme(Richness~NAP,
         random=~1|fBeach,
         data=rikz)
summary(rim)

# ---- Random Intercept Model: Diagnostics ----
## Diagnostics 1: Residuals vs Fitted Values ##
# Create data frame of residuals and fitted values
diag.rim<-data.frame(residuals=resid(rim), fitted=fitted(rim))

# Visualize fitted values vs residuals
ggplot(diag.rim)+
  geom_point(aes(x=fitted, y=residuals), size=3)+
  geom_hline(yintercept=0, linetype="dashed", col="blue")+
  theme_bw(16)+ylab("Residuals")+xlab("Fitted Values)")

# ---- Random Intercept Model: Fitted Lines ----
output.rim<-output.m1
output.rim$predicted<-predict(rim,output.rim,
                              type="response",
                              level=0)
output.rim$random<-predict(rim,output.rim,
                           type="response",
                           level=1)

rim.plot<-base.rikz+
  geom_line(aes(x=NAP, y=predicted),size=2, data=output.rim)
rim.plot

# Add Lines for Random Component #
rim.plot+
  geom_line(aes(x=NAP, y=random, col=fBeach), data=output.rim)

# ---- Random Intercept and Slope Model: Set Up ----
rism<-lme(Richness~NAP,
          random=~1+NAP|fBeach,
          data=rikz)
summary(rism)

# ---- Random Intercept and Slope Model: Diagnostics ----
## Diagnostics 1: Residuals vs Fitted Values ##
# Create data frame of residuals and fitted values
diag.rism<-data.frame(residuals=resid(rism), fitted=fitted(rism))

# Visualize fitted values vs residuals
ggplot(diag.rism)+
  geom_point(aes(x=fitted, y=residuals), size=3)+
  geom_hline(yintercept=0, linetype="dashed", col="blue")+
  theme_bw(16)+ylab("Residuals")+xlab("Fitted Values)")

# ---- Random Intercept and Slope Model: Fitted Line ----
output.rism<-output.m1
output.rism$predicted<-predict(rism, output.rism,
                              type="response",
                              level=0)
output.rism$random<-predict(rism, output.rism,
                           type="response",
                           level=1)

rism.plot<-base.rikz+
  geom_line(aes(x=NAP, y=predicted),size=2, data=output.rism)
rism.plot

# Add lines for random component
rism.plot+
  geom_line(aes(x=NAP, y=random, col=fBeach), data=output.rism)
