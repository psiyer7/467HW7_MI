# Mobility Index 

# Clean workspace
rm(list=ls())
library(writexl)

CensusBlock = 2006
County_censusdata = read.csv("NewJersey34013Module1NN2ndRun.csv")
aggroHH = aggregate(list(County_censusdata$Income.Amount), by=list(County_censusdata$HH.ID, 
                                                                   County_censusdata$Block.Code,County_censusdata$Tract.Code), FUN = sum)
names(aggroHH) <- c("HH.ID", "CensusBlock", "CensusTract", "Income_Sum")

MobilityIndex <- merge(County_censusdata,aggroHH,by="HH.ID")
MobilityIndex$`Census Block` = NULL
MobilityIndex$`Census Tract` = NULL
# Write 2 Excel 
write_xlsx(MobilityIndex, "{NJ,Essex}MobilityIndex.xlsx")

# Isolating block data 
aggroHH <- aggroHH[ which(aggroHH$CensusBlock== CensusBlock),]
# sort by Income
aggroHH <- aggroHH[order(aggroHH$Income_Sum),] 

# Assigning Income Deciles
IncomeDecile = unname(quantile(aggroHH$Income_Sum, c(.1, .2, .3, .4, .5, .6, .7, .8, .9)))
aggroHH$HHIncomeDecile = 0
for (i in 1:nrow(aggroHH)) {
  if (aggroHH$Income_Sum[i] < IncomeDecile[1]) aggroHH$HHIncomeDecile[i] = 1
  if (aggroHH$Income_Sum[i] > IncomeDecile[1] & aggroHH$Income_Sum[i] < IncomeDecile[2]) aggroHH$HHIncomeDecile[i] = 2
  if (aggroHH$Income_Sum[i] > IncomeDecile[2] & aggroHH$Income_Sum[i] < IncomeDecile[3]) aggroHH$HHIncomeDecile[i] = 3
  if (aggroHH$Income_Sum[i] > IncomeDecile[3] & aggroHH$Income_Sum[i] < IncomeDecile[4]) aggroHH$HHIncomeDecile[i] = 4
  if (aggroHH$Income_Sum[i] > IncomeDecile[4] & aggroHH$Income_Sum[i] < IncomeDecile[5]) aggroHH$HHIncomeDecile[i] = 5
  if (aggroHH$Income_Sum[i] > IncomeDecile[5] & aggroHH$Income_Sum[i] < IncomeDecile[6]) aggroHH$HHIncomeDecile[i] = 6
  if (aggroHH$Income_Sum[i] > IncomeDecile[6] & aggroHH$Income_Sum[i] < IncomeDecile[7]) aggroHH$HHIncomeDecile[i] = 7
  if (aggroHH$Income_Sum[i] > IncomeDecile[7] & aggroHH$Income_Sum[i] < IncomeDecile[8]) aggroHH$HHIncomeDecile[i] = 8
  if (aggroHH$Income_Sum[i] > IncomeDecile[8] & aggroHH$Income_Sum[i] < IncomeDecile[9]) aggroHH$HHIncomeDecile[i] = 9
  if (aggroHH$Income_Sum[i] > IncomeDecile[9]) aggroHH$HHIncomeDecile[i] = 10
}
LowestIncomeDecileVal = IncomeDecile[1] 

# Assinging Auto Ownership 
values <- c(0,1,2,3,4)
aggroHH$AutoOwnership <- sample(values, nrow(aggroHH), replace = TRUE, prob = c(.229,.393,.275,.076,.027))

# Bus/ Train Stops
aggroHH$PublicTrans = 5

# MI
aggroHH$MI = aggroHH$HHIncomeDecile + aggroHH$AutoOwnership + aggroHH$PublicTrans
plot(ecdf(aggroHH[,"MI"]), xlab = "Mobility Index", main = "CDF of Mobility Index {NJ, Essex} Block 1002", col = "blue")

# Assigning MI Deciles
MIDecile = unname(quantile(aggroHH$MI, c(.1, .2, .3, .4, .5, .6, .7, .8, .9)))

aggroHH$MIDecile = 0
# adjusted based on knowing quantile array goes up by 1 for MI deciles 
for (i in 1:nrow(aggroHH)) {
  if (aggroHH$MI[i] < MIDecile[1])  aggroHH$MIDecile[i] = 1
  if (aggroHH$MI[i] == MIDecile[1]) aggroHH$MIDecile[i] = 2
  if (aggroHH$MI[i] == MIDecile[2]) aggroHH$MIDecile[i] = 3
  if (aggroHH$MI[i] == MIDecile[3]) aggroHH$MIDecile[i] = 4
  if (aggroHH$MI[i] == MIDecile[4]) aggroHH$MIDecile[i] = 5
  if (aggroHH$MI[i] == MIDecile[5]) aggroHH$MIDecile[i] = 6
  if (aggroHH$MI[i] == MIDecile[6]) aggroHH$MIDecile[i] = 7
  if (aggroHH$MI[i] == MIDecile[7]) aggroHH$MIDecile[i] = 8
  if (aggroHH$MI[i] == MIDecile[8]) aggroHH$MIDecile[i] = 9
  if (aggroHH$MI[i] == MIDecile[9]) aggroHH$MIDecile[i] = 10
}

# parallel walk -- extracting all lowest decile of MI __________________________
LowestDecileVal = MIDecile[1] 
LowestMIDecileHouses = subset(aggroHH, aggroHH$MIDecile ==1)
LowestMIDecileHouses <- data.frame(LowestMIDecileHouses$HH.ID, LowestMIDecileHouses$MIDecile)
names(LowestMIDecileHouses) <- c("HH.ID", "MIDecile")

County_TRIPdata = read.csv("NewJersey_34013_Module6NN1stRun.csv")
# sort by HHID
County_TRIPdata <- County_TRIPdata[order(County_TRIPdata$HH.ID),] 
Trips_lowestMIs <- merge(County_TRIPdata,LowestMIDecileHouses,by="HH.ID")
# Write 2 Excel 
write_xlsx(Trips_lowestMIs, "{NJ,Essex}_Module6NN_LowestMI.xlsx")

