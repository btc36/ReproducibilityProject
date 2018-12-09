library("haven")
library("tidyverse")
library("reshape2")
data = read_sav("Data_AutomaticImitation-Mimicry .sav")
noseTwNosev = data$NoseVideo_NoseTouching
hairTwNosev = data$NoseVideo_HairTouching
noseTwHairv = data$HairVideo_NoseTouching
hairTwHairv = data$HairVideo_HairTouching
meanNWH = mean(noseTwHairv)
meanNWN = mean(noseTwNosev)
meanHWH = mean(hairTwHairv)
meanHWN = mean(hairTwNosev)

getSE = function(values){
  n = length(values)
  st_dev = sd(values)
  SE = st_dev/sqrt(n)
  return(SE)
}

getConfIntvl = function(values){
  n = length(values)
  mn = mean(values)
  SE = getSE(values)
  df = n-1
  t = qt(0.05,df)
  lower = mn -(t*SE)
  upper = mn + (t*SE)
  return(c(lower,upper))
}

NWHConf = getConfIntvl(noseTwHairv)
NWNConf = getConfIntvl(noseTwNosev)
HWHConf = getConfIntvl(hairTwHairv)
HWNConf = getConfIntvl(hairTwNosev)

data = tribble(
  ~action, ~touched, ~video, ~lower, ~upper,
  meanNWH, "Nose Touching", "Hair Video", NWHConf[1],NWHConf[2],
  meanHWH, "Hair Touching", "Hair Video",HWHConf[1],NWHConf[2],
  meanNWN, "Nose Touching", "Nose Video",NWNConf[1],NWHConf[2],
  meanHWN, "Hair Touching", "Nose Video", HWNConf[1],NWHConf[2]
)
data$touched = factor(data$touched)
data$touched = factor(data$touched, levels = rev(levels(data$touched)))
gg = melt(data,id =2:3)
ggplot(gg) +
  geom_bar(aes(x=video, y = value, fill = touched),stat = "identity",position = "dodge")+
  #geom_errorbar(aes(x = video, ymin=lower, ymax=upper ),color = "black", width =0.05) +
  ylab("Amount of Performed Action ")+
  xlab("")+
  theme_bw()+
  scale_x_discrete(limits = rev(levels(factor(gg$video))))
ggsave("Test.pdf",height = 12, width = 12)
