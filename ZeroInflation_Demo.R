# ---- Description ----
# This is demonstration code for zero inflated models

# ---- Load Packages ----
library(lmtest)
library(dplyr)
library(pscl)
library(ggplot2)
library(gridExtra)

# ---- Import Data ----
cod<-read.delim("ParasiteCod.txt")

# ---- Data Manipulation ----
# Set factors
cod$fArea<-as.factor(cod$Area)
cod$fYear<-as.factor(cod$Year)

# Remove records with NA in variables of interest
cod<-cod%>%
  filter(!is.na(Intensity))%>%
  filter(!is.na(fYear))%>%
  filter(!is.na(fArea))%>%
  filter(!is.na(Length))

# ---- Research Question ----
# How does intensity relate to length
# across years and area?

# ---- Basic Visualization ----
ggplot(cod)+
  geom_point(aes(x=Length, y=Intensity, col=fArea))+
  facet_grid(fYear~.)+
  theme_bw(15)

# Why zero inflated models?
plot(table(cod$Intensity), ylab="Frequency", xlab="Observed Intensity")

# ---- Poisson Zero Inflated: Set Up ----
zip1<-zeroinfl(Intensity~fArea*fYear+Length|fArea*fYear+Length,
               dist="poisson", link="logit",data=cod)
zip1
summary(zip1)

# ---- Poisson Zero Inflated: Diagnostics ----
dispersion(zip1, modeltype='zp')

# ---- Negative Binomial Zero Inflated: Set Up ----
zinb1<-zeroinfl(Intensity~fArea*fYear+Length|fArea*fYear+Length,
               dist="negbin", link="logit",data=cod)
zinb1
summary(zinb1)

# ---- Negative Binomial Zero Inflated: Diagnostics ----
dispersion(zinb1, modeltype='znb')

# ---- Model Selection: zip1, zinb1 ----
# Likelihood Ratio Test
lrtest(zip1, zinb1) # We see that the negative binomial model (zip2) is better

# ---- zinb1: More (Nested) Models ----
# zinb2: Drop interaction from count model
zinb2<-zeroinfl(Intensity~fArea+fYear+Length|fArea*fYear+Length,
               dist="negbin", link="logit",data=cod)
summary(zinb2)
# zinb2: Diagnostics
dispersion(zinb2, modeltype='znb')

# zinb3: Drop interaction from count model
zinb3<-zeroinfl(Intensity~fArea*fYear+Length|fArea+fYear+Length,
               dist="negbin", link="logit",data=cod)
summary(zinb3)
# zinb3: Diagnostics
dispersion(zinb3, modeltype='znb')

# ---- Model Selection: zinb1, zinb2, zinb3 ----
# Likelihood Ratio Test
lrtest(zinb1, zinb2)
lrtest(zinb1, zinb3)
# Akaike's Information Criterion
AIC(zinb1, zinb2, zinb3)

# The optimal model is zinb2 #
# Intensity ~ fArea + fYear + Length | fArea * fYear + Length #
summary(zinb2)

# ---- zinb2: Full Diagnostics ----
## Diagnostics 1: Residuals vs Fitted Values ##
# Create data frame of residuals and fitted values
diag.zinb2<-data.frame(residuals=resid(zinb2, type="pearson"),
                       fitted=fitted(zinb2, type="response"))

# Visualize fitted values vs residuals
ggplot(diag.zinb2)+
  geom_point(aes(x=fitted, y=residuals), size=3)+
  geom_hline(yintercept=0, linetype="dashed", col="blue")+
  theme_bw(16)+ylab("Residuals")+xlab("Fitted Values)")

## Diagnostics 2: Dispersion ##
dispersion(zinb2, modeltype='znb')

# ---- zinb2: Fitted Line ----
# Step 1: Create a data frame with independent variable (depth_km) 
output.zinb2<-expand.grid(fArea=as.factor(1:4),
                          fYear=as.factor(1999:2001),
                          Length=seq(from=15, to=100, by=5))

# Step 2: Populate a column of predictions based on the
# model zinb2 and the values given by our output.zinb2 data frame
output.zinb2$predicted<-predict(zinb2, output.zinb2, type="response")

# Step 3: Visualize the fitted model line
zinb2.plot<-ggplot(output.zinb2)+
  geom_line(aes(x=Length, y=predicted, col=fArea), size=1)+
  facet_grid(fYear~., scales='free')+theme_bw(22)+ylab("Predicted")
zinb2.plot

# Overlay data points
zinb2.plot+geom_point(aes(x=Length, y=Intensity, col=fArea), data=cod)
  

# ---- How does this compare to GLMs? ----
# Poisson
pcod<-glm(Intensity~fArea+fYear+Length, family='poisson',data=cod)
output.pcod<-output.zinb2
output.pcod$predicted<-predict(pcod, output.pcod, type="response")

pcod.plot<-ggplot(output.pcod)+
  geom_path(aes(x=Length, y=predicted, col=fArea), size=1)+
  facet_grid(fYear~., scales='free')+theme_bw(22)+ylab("Predicted")
pcod.plot

# Negative Binomial
nbcod<-glm.nb(Intensity~fArea+fYear+Length, data=cod)
output.nbcod<-output.zinb2
output.nbcod$predicted<-predict(nbcod, output.nbcod, type="response")

nbcod.plot<-ggplot(output.nbcod)+
  geom_path(aes(x=Length, y=predicted, col=fArea), size=1)+
  facet_grid(fYear~., scales='free')+theme_bw(22)+ylab("Predicted")
nbcod.plot

# Add titles to each
zinb2.plot<-zinb2.plot+ggtitle("Zero Inflated")
pcod.plot<-pcod.plot+ggtitle("Poisson GLM")
nbcod.plot<-nbcod.plot+ggtitle("NB GLM")

# Plot together
grid.arrange(zinb2.plot, pcod.plot,nbcod.plot, nrow=2, ncol=2)

# Bind output data frames for direct comparison
output.zinb2$type<-"zinb"
output.pcod$type<-"pglm"
output.nbcod$type<-"nbglm"

allmods<-rbind(output.zinb2, output.pcod, output.nbcod)

ggplot(allmods)+
  geom_line(aes(x=Length, y=predicted, col=type))+
  theme_bw(15)+ylab("Predicted")+
  facet_grid(fArea~fYear)
