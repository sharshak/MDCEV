##Based on Prof. Chandra Bhat's Gauss Code
##I would like to be acknowledged if you use code in part or full
rm(list = ls()) #clears the console
options(digits=2)
options(max.print = 10000)

options("scipen" = 3) #set to 3 decimals for results
# Save the Data & Code in the same folder. No need to link the data to code.
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #finds directory where the code is saved and makes it as working directory
z=read.csv("C:\\Users\\sxk152031\\Box Sync\\first_year_paper_All_CODES_Papers\\Choice_Sets\\New_Code_05252019\\test_CS_secondweek_05162019.csv",header=TRUE) #read the raw data
units=levels(factor(which(colnames(z)=="UNIT")))# find the list of unique individuals - for random effects
nunits=length(units) #number of individuals

nrows=nrow(z) #size of the dataset
nobs=nrow(z) #size of the dataset
config=1
alp0to1=1 #satiation parameter is set to be between 0 and 1
price=1 #price is not used in this code
nc=14 #number of choices

po=which( colnames(z)=="caseid" ) #pointer 
ivuno=which( colnames(z)=="UNO" ) #Column of ones
ivsero=which( colnames(z)=="SERO" ) #Column of zeros
wtind=which( colnames(z)=="UNO" ) #weight
f=match(c("lnICGe",	"lnCake",	"lnCcan",	"lnChip",	"lnCook",	"lnCrac",	"lnBFBa",	"lnNsee",	"lnOth",	"lnPcor",	"lnPDMu",	"lnPrSna",	"lnPuff",	"lnYog"),names(z))
#above line points to the quantity consumed
fp=matrix(which(colnames(z)=="UNO"),nrow = 1,ncol=length(f)) #matrix of ones
flagprcm = (fp) # set to 1s 
flagavm=fp # set to 1s 
flag_avl=match(c("CSICGe",	"CSCake",	"CSCcan",	"CSChip",	"CSCook",	"CSCrac",	"CSBFBa",	"CSNsee",	"CSOth",	"CSPcor",	"CSPDMu",	"CSPrSna",	"CSPuff",	"CSYog"),names(z))

ConsumptionMatrix = z[,f] # create the consumption matrix
#ChoiceSetMatrix=z[,c("Choice_SetCake","Choice_SetCcan",	"Choice_SetChip",	"Choice_SetCook",	"Choice_SetCrac","Choice_SetBFBa",	"Choice_SetNsee",	"Choice_SetOth",	"Choice_SetPcor",	"Choice_SetPDMu",	"Choice_SetPrSna",	"Choice_SetPuff",	"Choice_SetYog")]  
flagchm=f
consumption_occasions=z[,flagchm]# create the choice occasions matrix - same as consumption matrix with 1s and 0s instead as set in the next steps
c_occ=consumption_occasions
consumption_occasions[consumption_occasions[,]>0] <- 1 #
b=consumption_occasions
z$IntMed = z$TotFat*z$SatMed #create interaction terms here
z$IntHi = z$TotFat*z$SatHi
z$IntLow = z$TotFat*z$SatLow
#z$IntGenMed=z$Female*z$SatMed - Didn't work Gender * SatFat
#z$IntGenHi=z$Female*z$SatHi - Didn't work Gender * SatFat
#Define Equations for Utility here

z$OBSatHi=z$OBESE*z$SatHi
z$OBSatMed=z$OBESE*z$SatMed
z$NorSatHi=z$NWEIGHT*z$SatHi
z$NorSatMed=z$NWEIGHT*z$SatMed

z$LowENothing=z$LowEx*z$Nothing
z$LowEComputer=z$LowEx*z$Computer
z$LowEWork=z$LowEx*z$Work
z$LowESpor=z$LowEx*z$Sports

z$MidENothing=z$MIDEx*z$Nothing
z$MidEComputer=z$MIDEx*z$Computer
z$MidEWork=z$MIDEx*z$Work
z$MidESpor=z$MIDEx*z$Sports

z$LowEStudy=z$LowEx*z$Study
z$LowEEatM=z$LowEx*z$Eat_Meal
z$LowETrav=z$LowEx*z$Travel2

z$MidEStudy=z$MIDEx*z$Study
z$MidEEatM=z$MIDEx*z$Eat_Meal
z$MidETrav=z$MIDEx*z$Travel2

#14 equations for the choice part of utility. You can add remove covariates below. Just ensure that the number of columns in each equation are same
# x1	=	cbind(z$SERO,	z$SERO,	     z$SERO,	z$SERO,	     z$SERO,    z$SERO,	z$SERO,	  	   z$SERO,	    z$SERO,	 z$SERO,	 z$SERO, z$SERO,	z$SERO,	   z$SERO,	 z$SERO,	     z$SERO,	z$SERO,	  z$SERO,	    z$SERO,	    z$SERO,	   z$SERO,	  z$SERO,	  z$SERO,	z$SERO,	  z$SERO,	z$SERO,	  z$SERO,	  z$SERO,	  z$SERO,	    z$SERO,   z$SERO,	  z$SERO,z$SERO,z$SERO,z$SERO,    z$SERO,    z$SERO,      z$SERO,	      z$SERO,	      z$SERO,	  z$SERO,     z$SERO,      z$SERO,   z$SERO,     z$SERO,    z$SERO)#,    z$SERO ,z$SERO,z$SERO,z$SERO)#,	   z$SERO,z$SERO,  	z$SERO)
# x2	=	cbind(z$UNO,	z$NWEIGHT,	z$OBESE,	z$age_182,	z$Age_65,	z$Female,	z$Favourite,	z$Healthy,	z$Groups,	z$Relax, 	z$Bored, z$Boost,	z$OnItsOwn,z$Study,z$Eat_Meal,		 z$CakeA,	z$CakeW,	z$IntCakeA,	z$IntCakeW,	z$CakeLD,	z$CakeLDW,	z$LowEx,	z$MIDEx,z$SatMed,z$SatHi,	z$IntMed, z$IntHi,	z$TotFat,	z$TotCarb,	z$TotFib,	z$TotProt,	z$BF,	z$BL,	z$LD,	z$PostDin,	z$Weekend2,z$Computer,	z$Work_ONLY,	z$Travel2,		z$Sports,	z$Nothing,	z$Socialize, z$OBSatHi,z$OBSatMed, z$NorSatHi,z$NorSatMed)#,z$LowEComputer,z$LowEWork,z$LowESpor)#, z$MidEStudy, z$MidEEatM)#,#z$LowENothing,          #z$LowEComputer,z$LowEWork,z$LowESpor,z$MidENothing,z$MidEComputer,z$MidEWork,z$MidESpor,z$LowEStudy, z$LowEEatM, z$LowETrav, )
# x3	=	cbind(z$UNO,	z$NWEIGHT,	z$OBESE,	z$age_182,	z$Age_65,	z$Female,	z$Favourite,	z$Healthy,	z$Groups,	z$Relax, 	z$Bored, z$Boost,	z$OnItsOwn,z$Study,z$Eat_Meal,		 z$CcanA,	z$CcanW,	z$IntCcanA,	z$IntCcanW,	z$CcanHC,	z$CcanHCW,	z$LowEx,	z$MIDEx,z$SatMed,z$SatHi,	z$IntMed, z$IntHi,	z$TotFat,	z$TotCarb,	z$TotFib,	z$TotProt,	z$BF,	z$BL,	z$LD,	z$PostDin,	z$Weekend2,z$Computer,	z$Work_ONLY,	z$Travel2,		z$Sports,	z$Nothing,	z$Socialize, z$OBSatHi,z$OBSatMed, z$NorSatHi,z$NorSatMed)#,z$LowEComputer,z$LowEWork,z$LowESpor)#,z$MidEStudy, z$MidEEatM)#,#z$LowENothing,     z$LowEComputer,z$LowEWork,z$LowESpor,z$MidENothing,z$MidEComputer,z$MidEWork,z$MidESpor.z$LowEStudy, z$LowEEatM, z$LowETrav, )
# x4	=	cbind(z$UNO,	z$NWEIGHT,	z$OBESE,	z$age_182,	z$Age_65,	z$Female,	z$Favourite,	z$Healthy,	z$Groups,	z$Relax, 	z$Bored, z$Boost,	z$OnItsOwn,z$Study,z$Eat_Meal,		 z$ChipA,	z$ChipW,	z$IntChipA,	z$IntChipW,	z$ChipL,	z$ChipLW,	  z$LowEx,	z$MIDEx,z$SatMed,z$SatHi,	z$IntMed, z$IntHi,	z$TotFat,	z$TotCarb,	z$TotFib,	z$TotProt,	z$BF,	z$BL,	z$LD,	z$PostDin,	z$Weekend2,z$Computer,	z$Work_ONLY,	z$Travel2,		z$Sports,	z$Nothing,	z$Socialize, z$OBSatHi,z$OBSatMed, z$NorSatHi,z$NorSatMed)#,z$LowEComputer,z$LowEWork,z$LowESpor)#,z$MidEStudy, z$MidEEatM)#,#z$LowENothing,           z$LowEComputer,z$LowEWork,z$LowESpor,z$MidENothing,z$MidEComputer,z$MidEWork,z$MidESpor,z$LowEStudy, z$LowEEatM, z$LowETrav, )
# x5	=	cbind(z$UNO,	z$NWEIGHT,	z$OBESE,	z$age_182,	z$Age_65,	z$Female,	z$Favourite,	z$Healthy,	z$Groups,	z$Relax, 	z$Bored, z$Boost,	z$OnItsOwn,z$Study,z$Eat_Meal,		 z$CookA,	z$CookW,	z$IntCookA,	z$IntCookW,	z$CookCA,	z$CookCAW,	z$LowEx,	z$MIDEx,z$SatMed,z$SatHi,	z$IntMed, z$IntHi,	z$TotFat,	z$TotCarb,	z$TotFib,	z$TotProt,	z$BF,	z$BL,	z$LD,	z$PostDin,	z$Weekend2,z$Computer,	z$Work_ONLY,	z$Travel2,		z$Sports,	z$Nothing,	z$Socialize, z$OBSatHi,z$OBSatMed, z$NorSatHi,z$NorSatMed)#,z$LowEComputer,z$LowEWork,z$LowESpor)#,z$MidEStudy, z$MidEEatM)#,#z$LowENothing,           z$LowEComputer,z$LowEWork,z$LowESpor,z$MidENothing,z$MidEComputer,z$MidEWork,z$MidESpor,z$LowEStudy, z$LowEEatM, z$LowETrav, )
# x6	=	cbind(z$UNO,	z$NWEIGHT,	z$OBESE,	z$age_182,	z$Age_65,	z$Female,	z$Favourite,	z$Healthy,	z$Groups,	z$Relax, 	z$Bored, z$Boost,	z$OnItsOwn,z$Study,z$Eat_Meal,		 z$CracA,	z$CracW,	z$IntCracA,	z$IntCracW,	z$CracN,	z$CracNW,	  z$LowEx,	z$MIDEx,z$SatMed,z$SatHi,	z$IntMed, z$IntHi,	z$TotFat,	z$TotCarb,	z$TotFib,	z$TotProt,	z$BF,	z$BL,	z$LD,	z$PostDin,	z$Weekend2,z$Computer,	z$Work_ONLY,	z$Travel2,		z$Sports,	z$Nothing,	z$Socialize, z$OBSatHi,z$OBSatMed, z$NorSatHi,z$NorSatMed)#,z$LowEComputer,z$LowEWork,z$LowESpor)#,z$MidEStudy, z$MidEEatM)#,#z$LowENothing,           z$LowEComputer,z$LowEWork,z$LowESpor,z$MidENothing,z$MidEComputer,z$MidEWork,z$MidESpor,z$LowEStudy, z$LowEEatM, z$LowETrav, )
# x7	=	cbind(z$UNO,	z$NWEIGHT,	z$OBESE,	z$age_182,	z$Age_65,	z$Female,	z$Favourite,	z$Healthy,	z$Groups,	z$Relax, 	z$Bored, z$Boost,	z$OnItsOwn,z$Study,z$Eat_Meal,		 z$BFBaA,	z$BFBaW,	z$IntBFBaA,	z$IntBFBaW,	z$BFBaQC,	z$BFBaQCW,	z$LowEx,	z$MIDEx,z$SatMed,z$SatHi,	z$IntMed, z$IntHi,	z$TotFat,	z$TotCarb,	z$TotFib,	z$TotProt,	z$BF,	z$BL,	z$LD,	z$PostDin,	z$Weekend2,z$Computer,	z$Work_ONLY,	z$Travel2,		z$Sports,	z$Nothing,	z$Socialize, z$OBSatHi,z$OBSatMed, z$NorSatHi,z$NorSatMed)#,z$LowEComputer,z$LowEWork,z$LowESpor)#,z$MidEStudy, z$MidEEatM)#,#z$LowENothing,           z$LowEComputer,z$LowEWork,z$LowESpor,z$MidENothing,z$MidEComputer,z$MidEWork,z$MidESpor,z$LowEStudy, z$LowEEatM, z$LowETrav, )
# x8	=	cbind(z$UNO,	z$NWEIGHT,	z$OBESE,	z$age_182,	z$Age_65,	z$Female,	z$Favourite,	z$Healthy,	z$Groups,	z$Relax, 	z$Bored, z$Boost,	z$OnItsOwn,z$Study,z$Eat_Meal,		 z$NseeA,	z$NseeW,	z$IntNseeA,	z$IntNseeW,	z$NseePR,	z$NseePRW,	z$LowEx,	z$MIDEx,z$SatMed,z$SatHi,	z$IntMed, z$IntHi,	z$TotFat,	z$TotCarb,	z$TotFib,	z$TotProt,	z$BF,	z$BL,	z$LD,	z$PostDin,	z$Weekend2,z$Computer,	z$Work_ONLY,	z$Travel2,		z$Sports,	z$Nothing,	z$Socialize, z$OBSatHi,z$OBSatMed, z$NorSatHi,z$NorSatMed)#,z$LowEComputer,z$LowEWork,z$LowESpor)#,z$MidEStudy, z$MidEEatM)#,#z$LowENothing,           z$LowEComputer,z$LowEWork,z$LowESpor,z$MidENothing,z$MidEComputer,z$MidEWork,z$MidESpor,z$LowEStudy, z$LowEEatM, z$LowETrav, )
# x9	=	cbind(z$UNO,	z$NWEIGHT,	z$OBESE,	z$age_182,	z$Age_65,	z$Female,	z$Favourite,	z$Healthy,	z$Groups,	z$Relax, 	z$Bored, z$Boost,	z$OnItsOwn,z$Study,z$Eat_Meal,		 z$OthA,	z$OthW,	  z$IntOthA,	z$IntOthW,	z$OthPL,	z$OthPLW,	  z$LowEx,	z$MIDEx,z$SatMed,z$SatHi,	z$IntMed, z$IntHi,	z$TotFat,	z$TotCarb,	z$TotFib,	z$TotProt,	z$BF,	z$BL,	z$LD,	z$PostDin,	z$Weekend2,z$Computer,	z$Work_ONLY,	z$Travel2,		z$Sports,	z$Nothing,	z$Socialize, z$OBSatHi,z$OBSatMed, z$NorSatHi,z$NorSatMed)#,z$LowEComputer,z$LowEWork,z$LowESpor)#,z$MidEStudy, z$MidEEatM)#,#z$LowENothing,           z$LowEComputer,z$LowEWork,z$LowESpor,z$MidENothing,z$MidEComputer,z$MidEWork,z$MidESpor,z$LowEStudy, z$LowEEatM, z$LowETrav, )
# x10	=	cbind(z$UNO,	z$NWEIGHT,	z$OBESE,	z$age_182,	z$Age_65,	z$Female,	z$Favourite,	z$Healthy,	z$Groups,	z$Relax, 	z$Bored, z$Boost,	z$OnItsOwn,z$Study,z$Eat_Meal,		 z$PcorA,	z$PcorW,	z$IntPcorA,	z$IntPcorW,	z$PcorORB,z$PcorORBW, z$LowEx,	z$MIDEx,z$SatMed,z$SatHi,	z$IntMed, z$IntHi,	z$TotFat,	z$TotCarb,	z$TotFib,	z$TotProt,	z$BF,	z$BL,	z$LD,	z$PostDin,	z$Weekend2,z$Computer,	z$Work_ONLY,	z$Travel2,		z$Sports,	z$Nothing,	z$Socialize, z$OBSatHi,z$OBSatMed, z$NorSatHi,z$NorSatMed)#,z$LowEComputer,z$LowEWork,z$LowESpor)#,z$MidEStudy, z$MidEEatM)#,#z$LowENothing,            z$LowEComputer,z$LowEWork,z$LowESpor,z$MidENothing,z$MidEComputer,z$MidEWork,z$MidESpor,z$LowEStudy, z$LowEEatM, z$LowETrav, )
# x11	=	cbind(z$UNO,	z$NWEIGHT,	z$OBESE,	z$age_182,	z$Age_65,	z$Female,	z$Favourite,	z$Healthy,	z$Groups,	z$Relax, 	z$Bored, z$Boost,	z$OnItsOwn,z$Study,z$Eat_Meal,		 z$PDMuA,	z$PDMuW,	z$IntPDMuA,	z$IntPDMuW,	z$PDMuKP,	z$PDMuKPW,	z$LowEx,	z$MIDEx,z$SatMed,z$SatHi,	z$IntMed, z$IntHi,	z$TotFat,	z$TotCarb,	z$TotFib,	z$TotProt,	z$BF,	z$BL,	z$LD,	z$PostDin,	z$Weekend2,z$Computer,	z$Work_ONLY,	z$Travel2,		z$Sports,	z$Nothing,	z$Socialize, z$OBSatHi,z$OBSatMed, z$NorSatHi,z$NorSatMed)#,z$LowEComputer,z$LowEWork,z$LowESpor)#,z$MidEStudy, z$MidEEatM)#,#z$LowENothing,            z$LowEComputer,z$LowEWork,z$LowESpor,z$MidENothing,z$MidEComputer,z$MidEWork,z$MidESpor,z$LowEStudy, z$LowEEatM, z$LowETrav, )
# x12	=	cbind(z$UNO,	z$NWEIGHT,	z$OBESE,	z$age_182,	z$Age_65,	z$Female,	z$Favourite,	z$Healthy,	z$Groups,	z$Relax, 	z$Bored, z$Boost,	z$OnItsOwn,z$Study,z$Eat_Meal,		 z$PrSnaA,z$PrSnaW, z$IPrSnaA,	z$IPrSnaW,	z$PrSnaSO,z$PrSnaSOW, z$LowEx,	z$MIDEx,z$SatMed,z$SatHi,	z$IntMed, z$IntHi,	z$TotFat,	z$TotCarb,	z$TotFib,	z$TotProt,	z$BF,	z$BL,	z$LD,	z$PostDin,	z$Weekend2,z$Computer,	z$Work_ONLY,	z$Travel2,		z$Sports,	z$Nothing,	z$Socialize, z$OBSatHi,z$OBSatMed, z$NorSatHi,z$NorSatMed)#,z$LowEComputer,z$LowEWork,z$LowESpor)#,z$MidEStudy, z$MidEEatM)#,#z$LowENothing,            z$LowEComputer,z$LowEWork,z$LowESpor,z$MidENothing,z$MidEComputer,z$MidEWork,z$MidESpor,z$LowEStudy, z$LowEEatM, z$LowETrav, )
# x13	=	cbind(z$UNO,	z$NWEIGHT,	z$OBESE,	z$age_182,	z$Age_65,	z$Female,	z$Favourite,	z$Healthy,	z$Groups,	z$Relax, 	z$Bored, z$Boost,	z$OnItsOwn,z$Study,z$Eat_Meal,		 z$PuffA,	z$PuffW,	z$IntPuffA,	z$IntPuffW,	z$PuffCH,	z$PuffCHW,	z$LowEx,	z$MIDEx,z$SatMed,z$SatHi,	z$IntMed, z$IntHi,	z$TotFat,	z$TotCarb,	z$TotFib,	z$TotProt,	z$BF,	z$BL,	z$LD,	z$PostDin,	z$Weekend2,z$Computer,	z$Work_ONLY,	z$Travel2,		z$Sports,	z$Nothing,	z$Socialize, z$OBSatHi,z$OBSatMed, z$NorSatHi,z$NorSatMed)#,z$LowEComputer,z$LowEWork,z$LowESpor)#,z$MidEStudy, z$MidEEatM)#,#z$LowENothing,            z$LowEComputer,z$LowEWork,z$LowESpor,z$MidENothing,z$MidEComputer,z$MidEWork,z$MidESpor,z$LowEStudy, z$LowEEatM, z$LowETrav, )
# x14	=	cbind(z$UNO,	z$NWEIGHT,	z$OBESE,	z$age_182,	z$Age_65,	z$Female,	z$Favourite,	z$Healthy,	z$Groups,	z$Relax, 	z$Bored, z$Boost,	z$OnItsOwn,z$Study,z$Eat_Meal,		 z$YogA, 	z$YogW,	  z$IntYogA,	z$IntYogW,	z$YogYp,	z$YogYpW,	  z$LowEx,	z$MIDEx,z$SatMed,z$SatHi,	z$IntMed, z$IntHi,	z$TotFat,	z$TotCarb,	z$TotFib,	z$TotProt,	z$BF,	z$BL,	z$LD,	z$PostDin,	z$Weekend2,z$Computer,	z$Work_ONLY,	z$Travel2,		z$Sports,	z$Nothing,	z$Socialize, z$OBSatHi,z$OBSatMed, z$NorSatHi,z$NorSatMed)#,z$LowEComputer,z$LowEWork,z$LowESpor)#,z$MidEStudy, z$MidEEatM)#,#z$LowENothing,            z$LowEComputer,z$LowEWork,z$LowESpor,z$MidENothing,z$MidEComputer,z$MidEWork,z$MidESpor,z$LowEStudy, z$LowEEatM, z$LowETrav, )
# 
# #labels for these covariates
# par_choice = c("Intercept",	"NWEIGHT",	"OBESE",	"age_182",	"Age_65",	"Female",	"Favourite",	"Healthy",	"Groups",	"Relax",	"Bored",	"Boost",	"OnItsOwn","Study","Eat_Meal",	"Across-SD",	"Within-SD",	"IntA-SD",	"IntW-SD",	"Brand-Sd",	"Brand-W-SD",	"LowEx",	"MIDEx","SatMed","SatHi",	"IntMed","IntHi",	"TotFat",	"TotCarb",	"TotFib",	"TotProt",	"BF",	"BL",	"LD",	"PostDin",	"Weekend2"	,		"Computer","Work_ONLY","Travel2",		"Sports",	"Nothing",	"Socialize","OBSatHi","OBSatMed","NorSatHi","NorSatMed")#, "LowEComputer", "LowEWork", "LowESpor")#, "LowEComputer","LowEWork","LowESpor")#"MidEStudy", "MidEEatM", "MidETrav" ) #,,"MidENothing","MidEComputer","MidEWork","MidESpor")
# 
#   if (length(x1) != length(x2))   {stop("The value is TRUE, so the script must end here")}
#   
#   print("Script did NOT end!")
# 
# 
# 
# #Define 14 Equations for Satiation here. Same as choice for covariates - keep them same
# ax1  = cbind(z$UNO,z$Relax,  z$OnItsOwn,z$age_182,z$Age_65,z$Female,z$NWEIGHT,z$OBESE,z$SERO  ,z$SERO,   z$SERO,  z$SERO,   z$TotFat,z$TotCarb,z$TotFib,z$TotProt,z$SERO,z$SERO,z$SERO,z$SERO,   z$SERO,   z$SERO, z$SERO,    z$SERO,    z$SERO,     z$SERO,        z$SERO,  z$SERO,        z$SERO )#,    z$SERO,   z$SERO,    z$SERO,      z$SERO,        z$SERO,    z$SERO,     z$SERO,        z$SERO,     z$SERO,        z$SERO )
# ax2  = cbind(z$UNO,z$Relax,  z$OnItsOwn,z$age_182,z$Age_65,z$Female,z$NWEIGHT,z$OBESE,z$SatMed,z$SatHi,	z$IntMed, z$IntHi,	z$TotFat,z$TotCarb,z$TotFib,z$TotProt,z$BF,  z$BL,  z$LD, z$PostDin,z$Weekend2,z$Study,z$Eat_Meal,z$Travel2,z$Computer,	z$Work_ONLY,		z$Sports,	z$Nothing,	z$Socialize)# ,z$NorSatMed)#,z$OBSatHi,z$NorSatMed)#,z$LowEComputer,z$LowEWork,z$LowESpor,z$MidENothing,z$MidEComputer,z$MidEWork,z$MidESpor)
# ax3  = cbind(z$UNO,z$Relax,	 z$OnItsOwn,z$age_182,z$Age_65,z$Female,z$NWEIGHT,z$OBESE,z$SatMed,z$SatHi,	z$IntMed, z$IntHi,	z$TotFat,z$TotCarb,z$TotFib,z$TotProt,z$BF,  z$BL,  z$LD, z$PostDin,z$Weekend2,z$Study,z$Eat_Meal,z$Travel2,z$Computer,	z$Work_ONLY,		z$Sports,	z$Nothing,	z$Socialize)# ,z$NorSatMed)#,z$NorSatMed)#,z$LowEComputer,z$LowEWork,z$LowESpor,z$MidENothing,z$MidEComputer,z$MidEWork,z$MidESpor)
# ax4  = cbind(z$UNO,z$Relax,	 z$OnItsOwn,z$age_182,z$Age_65,z$Female,z$NWEIGHT,z$OBESE,z$SatMed,z$SatHi,	z$IntMed, z$IntHi,	z$TotFat,z$TotCarb,z$TotFib,z$TotProt,z$BF,  z$BL,  z$LD, z$PostDin,z$Weekend2,z$Study,z$Eat_Meal,z$Travel2,z$Computer,	z$Work_ONLY,		z$Sports,	z$Nothing,	z$Socialize)# ,z$NorSatMed)#,z$NorSatMed)#,z$LowEComputer,z$LowEWork,z$LowESpor,z$MidENothing,z$MidEComputer,z$MidEWork,z$MidESpor)
# ax5  = cbind(z$UNO,z$Relax,	 z$OnItsOwn,z$age_182,z$Age_65,z$Female,z$NWEIGHT,z$OBESE,z$SatMed,z$SatHi,	z$IntMed, z$IntHi,	z$TotFat,z$TotCarb,z$TotFib,z$TotProt,z$BF,  z$BL,  z$LD, z$PostDin,z$Weekend2,z$Study,z$Eat_Meal,z$Travel2,z$Computer,	z$Work_ONLY,		z$Sports,	z$Nothing,	z$Socialize)# ,z$NorSatMed)#,z$NorSatMed)#,z$LowEComputer,z$LowEWork,z$LowESpor,z$MidENothing,z$MidEComputer,z$MidEWork,z$MidESpor)
# ax6  = cbind(z$UNO,z$Relax,	 z$OnItsOwn,z$age_182,z$Age_65,z$Female,z$NWEIGHT,z$OBESE,z$SatMed,z$SatHi,	z$IntMed, z$IntHi,	z$TotFat,z$TotCarb,z$TotFib,z$TotProt,z$BF,  z$BL,  z$LD, z$PostDin,z$Weekend2,z$Study,z$Eat_Meal,z$Travel2,z$Computer,	z$Work_ONLY,		z$Sports,	z$Nothing,	z$Socialize)# ,z$NorSatMed)#,z$NorSatMed)#,z$LowEComputer,z$LowEWork,z$LowESpor,z$MidENothing,z$MidEComputer,z$MidEWork,z$MidESpor)
# ax7  = cbind(z$UNO,z$Relax,	 z$OnItsOwn,z$age_182,z$Age_65,z$Female,z$NWEIGHT,z$OBESE,z$SatMed,z$SatHi,	z$IntMed, z$IntHi,	z$TotFat,z$TotCarb,z$TotFib,z$TotProt,z$BF,  z$BL,  z$LD, z$PostDin,z$Weekend2,z$Study,z$Eat_Meal,z$Travel2,z$Computer,	z$Work_ONLY,		z$Sports,	z$Nothing,	z$Socialize)# ,z$NorSatMed)#,z$NorSatMed)#,z$LowEComputer,z$LowEWork,z$LowESpor,z$MidENothing,z$MidEComputer,z$MidEWork,z$MidESpor)
# ax8  = cbind(z$UNO,z$Relax,	 z$OnItsOwn,z$age_182,z$Age_65,z$Female,z$NWEIGHT,z$OBESE,z$SatMed,z$SatHi,	z$IntMed, z$IntHi,	z$TotFat,z$TotCarb,z$TotFib,z$TotProt,z$BF,  z$BL,  z$LD, z$PostDin,z$Weekend2,z$Study,z$Eat_Meal,z$Travel2,z$Computer,	z$Work_ONLY,		z$Sports,	z$Nothing,	z$Socialize)# ,z$NorSatMed)#,z$NorSatMed)#,z$LowEComputer,z$LowEWork,z$LowESpor,z$MidENothing,z$MidEComputer,z$MidEWork,z$MidESpor)
# ax9  = cbind(z$UNO,z$Relax,	 z$OnItsOwn,z$age_182,z$Age_65,z$Female,z$NWEIGHT,z$OBESE,z$SatMed,z$SatHi,	z$IntMed, z$IntHi,	z$TotFat,z$TotCarb,z$TotFib,z$TotProt,z$BF,  z$BL,  z$LD, z$PostDin,z$Weekend2,z$Study,z$Eat_Meal,z$Travel2,z$Computer,	z$Work_ONLY,		z$Sports,	z$Nothing,	z$Socialize)# ,z$NorSatMed)#,z$NorSatMed)#,z$LowEComputer,z$LowEWork,z$LowESpor,z$MidENothing,z$MidEComputer,z$MidEWork,z$MidESpor)
# ax10 = cbind(z$UNO,z$Relax,	 z$OnItsOwn,z$age_182,z$Age_65,z$Female,z$NWEIGHT,z$OBESE,z$SatMed,z$SatHi,	z$IntMed, z$IntHi,	z$TotFat,z$TotCarb,z$TotFib,z$TotProt,z$BF,  z$BL,  z$LD, z$PostDin,z$Weekend2,z$Study,z$Eat_Meal,z$Travel2,z$Computer,	z$Work_ONLY,		z$Sports,	z$Nothing,	z$Socialize)# ,z$NorSatMed)#,z$NorSatMed)#,z$LowEComputer,z$LowEWork,z$LowESpor,z$MidENothing,z$MidEComputer,z$MidEWork,z$MidESpor)
# ax11 = cbind(z$UNO,z$Relax,	 z$OnItsOwn,z$age_182,z$Age_65,z$Female,z$NWEIGHT,z$OBESE,z$SatMed,z$SatHi,	z$IntMed, z$IntHi,	z$TotFat,z$TotCarb,z$TotFib,z$TotProt,z$BF,  z$BL,  z$LD, z$PostDin,z$Weekend2,z$Study,z$Eat_Meal,z$Travel2,z$Computer,	z$Work_ONLY,		z$Sports,	z$Nothing,	z$Socialize)# ,z$NorSatMed)#,z$NorSatMed)#,z$LowEComputer,z$LowEWork,z$LowESpor,z$MidENothing,z$MidEComputer,z$MidEWork,z$MidESpor)
# ax12 = cbind(z$UNO,z$Relax,	 z$OnItsOwn,z$age_182,z$Age_65,z$Female,z$NWEIGHT,z$OBESE,z$SatMed,z$SatHi,	z$IntMed, z$IntHi,	z$TotFat,z$TotCarb,z$TotFib,z$TotProt,z$BF,  z$BL,  z$LD, z$PostDin,z$Weekend2,z$Study,z$Eat_Meal,z$Travel2,z$Computer,	z$Work_ONLY,		z$Sports,	z$Nothing,	z$Socialize)# ,z$NorSatMed)#,z$NorSatMed)#,z$LowEComputer,z$LowEWork,z$LowESpor,z$MidENothing,z$MidEComputer,z$MidEWork,z$MidESpor)
# ax13 = cbind(z$UNO,z$Relax,	 z$OnItsOwn,z$age_182,z$Age_65,z$Female,z$NWEIGHT,z$OBESE,z$SatMed,z$SatHi,	z$IntMed, z$IntHi,	z$TotFat,z$TotCarb,z$TotFib,z$TotProt,z$BF,  z$BL,  z$LD, z$PostDin,z$Weekend2,z$Study,z$Eat_Meal,z$Travel2,z$Computer,	z$Work_ONLY,		z$Sports,	z$Nothing,	z$Socialize)# ,z$NorSatMed)#,z$NorSatMed)#,z$LowEComputer,z$LowEWork,z$LowESpor,z$MidENothing,z$MidEComputer,z$MidEWork,z$MidESpor)
# ax14 = cbind(z$UNO,z$Relax,	 z$OnItsOwn,z$age_182,z$Age_65,z$Female,z$NWEIGHT,z$OBESE,z$SatMed,z$SatHi,	z$IntMed, z$IntHi,	z$TotFat,z$TotCarb,z$TotFib,z$TotProt,z$BF,  z$BL,  z$LD, z$PostDin,z$Weekend2,z$Study,z$Eat_Meal,z$Travel2,z$Computer,	z$Work_ONLY,		z$Sports,	z$Nothing,	z$Socialize)# ,z$NorSatMed)#,z$NorSatHi,z$NorSatMed)#,z$LowEComputer,z$LowEWork,z$LowESpor,z$MidENothing,z$MidEComputer,z$MidEWork,z$MidESpor)
# 
# 
# par_satiation = c("Intercept","Relax",	"OnItsOwn",		"age_182",	"Age_65",	"Female",	"NormalWeight",	"Obese","SatMed","SatHi","IntMed","IntHi",		"Fat",	"Carb",	"Fib",	"Protein",	"BF",	"BL",	"LD",	"PostDin",	"Weekend2",	"Study",	"Eat_Meal",	"Travel",	"Computer",	"Work",		"Sports_Physical","Nothing","Socialize")#,"NorSatHi","NorSatMed","OBSatHi","OBSatMed" ,"LowEComputer","LowEWork","LowESpor","MidENothing","MidEComputer","MidEWork","MidESpor")
# 
# #Define Equations for Gamma here - set to 1. Not estimated in this model.
# gx1 = cbind(z$UNO)
# gx2 = cbind(z$UNO)
# gx3 = cbind(z$UNO)
# gx4 = cbind(z$UNO)
# gx5 = cbind(z$UNO)
# gx6 = cbind(z$UNO)
# gx7 = cbind(z$UNO)
# gx8 = cbind(z$UNO)
# gx9 = cbind(z$UNO)
# gx10 = cbind(z$UNO)
# gx11 = cbind(z$UNO)
# gx12 = cbind(z$UNO)
# gx13 = cbind(z$UNO)
# gx14 = cbind(z$UNO)

#14 equations for the choice part of utility. You can add remove covariates below. Just ensure that the number of columns in each equation are same
x1	=	cbind(z$SERO,	z$SERO,	   z$SERO,    z$SERO,	  z$SERO,	  z$SERO, 	       z$SERO,	z$SERO,	  z$SERO,	    z$SERO,	    z$SERO,	   z$SERO,	z$SERO,	  z$SERO,	z$SERO,	  z$SERO,	  z$SERO,	  z$SERO,	    z$SERO,   z$SERO,	  z$SERO)#,z$SERO,z$SERO,z$SERO,    z$SERO,    z$SERO,      z$SERO,	      z$SERO,	      z$SERO,	  z$SERO,     z$SERO,      z$SERO,   z$SERO,     z$SERO,    z$SERO)#,    z$SERO ,z$SERO,z$SERO,z$SERO)#,	   z$SERO,z$SERO,  	z$SERO)
x2	=	cbind(z$UNO,	z$age_182,z$age_65,		z$Female,	z$NWEIGHT,z$OBESE,	  		 z$CakeA,	z$CakeW,	z$IntCakeA,	z$IntCakeW,	z$CakeLD,	z$CakeLDW,	z$TotFat,	z$TotCarb,	z$TotFib,	z$TotProt,	z$BF,	z$BL,	z$LD,	z$PostDin,	z$Weekend2)#,z$Computer,	z$Work_ONLY,	z$Travel2,		z$Sports,	z$Nothing,	z$Socialize, z$OBSatHi,z$OBSatMed, z$NorSatHi,z$NorSatMed)#,z$LowEComputer,z$LowEWork,z$LowESpor)#, z$MidEStudy, z$MidEEatM)#,#z$LowENothing,          #z$LowEComputer,z$LowEWork,z$LowESpor,z$MidENothing,z$MidEComputer,z$MidEWork,z$MidESpor,z$LowEStudy, z$LowEEatM, z$LowETrav, )
x3	=	cbind(z$UNO,	z$age_182,z$age_65,		z$Female,	z$NWEIGHT,z$OBESE,				 z$CcanA,	z$CcanW,	z$IntCcanA,	z$IntCcanW,	z$CcanHC,	z$CcanHCW,	z$TotFat,	z$TotCarb,	z$TotFib,	z$TotProt,	z$BF,	z$BL,	z$LD,	z$PostDin,	z$Weekend2)#,z$Computer,	z$Work_ONLY,	z$Travel2,		z$Sports,	z$Nothing,	z$Socialize, z$OBSatHi,z$OBSatMed, z$NorSatHi,z$NorSatMed)#,z$LowEComputer,z$LowEWork,z$LowESpor)#,z$MidEStudy, z$MidEEatM)#,#z$LowENothing,     z$LowEComputer,z$LowEWork,z$LowESpor,z$MidENothing,z$MidEComputer,z$MidEWork,z$MidESpor.z$LowEStudy, z$LowEEatM, z$LowETrav, )
x4	=	cbind(z$UNO,	z$age_182,z$age_65,		z$Female,	z$NWEIGHT,z$OBESE,				 z$ChipA,	z$ChipW,	z$IntChipA,	z$IntChipW,	z$ChipL,	z$ChipLW,	 	z$TotFat,	z$TotCarb,	z$TotFib,	z$TotProt,	z$BF,	z$BL,	z$LD,	z$PostDin,	z$Weekend2)#,z$Computer,	z$Work_ONLY,	z$Travel2,		z$Sports,	z$Nothing,	z$Socialize, z$OBSatHi,z$OBSatMed, z$NorSatHi,z$NorSatMed)#,z$LowEComputer,z$LowEWork,z$LowESpor)#,z$MidEStudy, z$MidEEatM)#,#z$LowENothing,           z$LowEComputer,z$LowEWork,z$LowESpor,z$MidENothing,z$MidEComputer,z$MidEWork,z$MidESpor,z$LowEStudy, z$LowEEatM, z$LowETrav, )
x5	=	cbind(z$UNO,	z$age_182,z$age_65,		z$Female,	z$NWEIGHT,z$OBESE,				 z$CookA,	z$CookW,	z$IntCookA,	z$IntCookW,	z$CookCA,	z$CookCAW,	z$TotFat,	z$TotCarb,	z$TotFib,	z$TotProt,	z$BF,	z$BL,	z$LD,	z$PostDin,	z$Weekend2)#,z$Computer,	z$Work_ONLY,	z$Travel2,		z$Sports,	z$Nothing,	z$Socialize, z$OBSatHi,z$OBSatMed, z$NorSatHi,z$NorSatMed)#,z$LowEComputer,z$LowEWork,z$LowESpor)#,z$MidEStudy, z$MidEEatM)#,#z$LowENothing,           z$LowEComputer,z$LowEWork,z$LowESpor,z$MidENothing,z$MidEComputer,z$MidEWork,z$MidESpor,z$LowEStudy, z$LowEEatM, z$LowETrav, )
x6	=	cbind(z$UNO,	z$age_182,z$age_65,		z$Female,	z$NWEIGHT,z$OBESE,				 z$CracA,	z$CracW,	z$IntCracA,	z$IntCracW,	z$CracN,	z$CracNW,	 	z$TotFat,	z$TotCarb,	z$TotFib,	z$TotProt,	z$BF,	z$BL,	z$LD,	z$PostDin,	z$Weekend2)#,z$Computer,	z$Work_ONLY,	z$Travel2,		z$Sports,	z$Nothing,	z$Socialize, z$OBSatHi,z$OBSatMed, z$NorSatHi,z$NorSatMed)#,z$LowEComputer,z$LowEWork,z$LowESpor)#,z$MidEStudy, z$MidEEatM)#,#z$LowENothing,           z$LowEComputer,z$LowEWork,z$LowESpor,z$MidENothing,z$MidEComputer,z$MidEWork,z$MidESpor,z$LowEStudy, z$LowEEatM, z$LowETrav, )
x7	=	cbind(z$UNO,	z$age_182,z$age_65,		z$Female,	z$NWEIGHT,z$OBESE,				 z$BFBaA,	z$BFBaW,	z$IntBFBaA,	z$IntBFBaW,	z$BFBaQC,	z$BFBaQCW,	z$TotFat,	z$TotCarb,	z$TotFib,	z$TotProt,	z$BF,	z$BL,	z$LD,	z$PostDin,	z$Weekend2)#,z$Computer,	z$Work_ONLY,	z$Travel2,		z$Sports,	z$Nothing,	z$Socialize, z$OBSatHi,z$OBSatMed, z$NorSatHi,z$NorSatMed)#,z$LowEComputer,z$LowEWork,z$LowESpor)#,z$MidEStudy, z$MidEEatM)#,#z$LowENothing,           z$LowEComputer,z$LowEWork,z$LowESpor,z$MidENothing,z$MidEComputer,z$MidEWork,z$MidESpor,z$LowEStudy, z$LowEEatM, z$LowETrav, )
x8	=	cbind(z$UNO,	z$age_182,z$age_65,		z$Female,	z$NWEIGHT,z$OBESE,				 z$NseeA,	z$NseeW,	z$IntNseeA,	z$IntNseeW,	z$NseePR,	z$NseePRW,	z$TotFat,	z$TotCarb,	z$TotFib,	z$TotProt,	z$BF,	z$BL,	z$LD,	z$PostDin,	z$Weekend2)#,z$Computer,	z$Work_ONLY,	z$Travel2,		z$Sports,	z$Nothing,	z$Socialize, z$OBSatHi,z$OBSatMed, z$NorSatHi,z$NorSatMed)#,z$LowEComputer,z$LowEWork,z$LowESpor)#,z$MidEStudy, z$MidEEatM)#,#z$LowENothing,           z$LowEComputer,z$LowEWork,z$LowESpor,z$MidENothing,z$MidEComputer,z$MidEWork,z$MidESpor,z$LowEStudy, z$LowEEatM, z$LowETrav, )
x9	=	cbind(z$UNO,	z$age_182,z$age_65,		z$Female,	z$NWEIGHT,z$OBESE,				 z$OthA,	z$OthW,	  z$IntOthA,	z$IntOthW,	z$OthPL,	z$OthPLW,	 	z$TotFat,	z$TotCarb,	z$TotFib,	z$TotProt,	z$BF,	z$BL,	z$LD,	z$PostDin,	z$Weekend2)#,z$Computer,	z$Work_ONLY,	z$Travel2,		z$Sports,	z$Nothing,	z$Socialize, z$OBSatHi,z$OBSatMed, z$NorSatHi,z$NorSatMed)#,z$LowEComputer,z$LowEWork,z$LowESpor)#,z$MidEStudy, z$MidEEatM)#,#z$LowENothing,           z$LowEComputer,z$LowEWork,z$LowESpor,z$MidENothing,z$MidEComputer,z$MidEWork,z$MidESpor,z$LowEStudy, z$LowEEatM, z$LowETrav, )
x10	=	cbind(z$UNO,	z$age_182,z$age_65,		z$Female,	z$NWEIGHT,z$OBESE,				 z$PcorA,	z$PcorW,	z$IntPcorA,	z$IntPcorW,	z$PcorORB,z$PcorORBW,	z$TotFat,	z$TotCarb,	z$TotFib,	z$TotProt,	z$BF,	z$BL,	z$LD,	z$PostDin,	z$Weekend2)#,z$Computer,	z$Work_ONLY,	z$Travel2,		z$Sports,	z$Nothing,	z$Socialize, z$OBSatHi,z$OBSatMed, z$NorSatHi,z$NorSatMed)#,z$LowEComputer,z$LowEWork,z$LowESpor)#,z$MidEStudy, z$MidEEatM)#,#z$LowENothing,            z$LowEComputer,z$LowEWork,z$LowESpor,z$MidENothing,z$MidEComputer,z$MidEWork,z$MidESpor,z$LowEStudy, z$LowEEatM, z$LowETrav, )
x11	=	cbind(z$UNO,	z$age_182,z$age_65,		z$Female,	z$NWEIGHT,z$OBESE,				 z$PDMuA,	z$PDMuW,	z$IntPDMuA,	z$IntPDMuW,	z$PDMuKP,	z$PDMuKPW,	z$TotFat,	z$TotCarb,	z$TotFib,	z$TotProt,	z$BF,	z$BL,	z$LD,	z$PostDin,	z$Weekend2)#,z$Computer,	z$Work_ONLY,	z$Travel2,		z$Sports,	z$Nothing,	z$Socialize, z$OBSatHi,z$OBSatMed, z$NorSatHi,z$NorSatMed)#,z$LowEComputer,z$LowEWork,z$LowESpor)#,z$MidEStudy, z$MidEEatM)#,#z$LowENothing,            z$LowEComputer,z$LowEWork,z$LowESpor,z$MidENothing,z$MidEComputer,z$MidEWork,z$MidESpor,z$LowEStudy, z$LowEEatM, z$LowETrav, )
x12	=	cbind(z$UNO,	z$age_182,z$age_65,		z$Female,	z$NWEIGHT,z$OBESE,				 z$PrSnaA,z$PrSnaW, z$IPrSnaA,	z$IPrSnaW,	z$PrSnaSO,z$PrSnaSOW,	z$TotFat,	z$TotCarb,	z$TotFib,	z$TotProt,	z$BF,	z$BL,	z$LD,	z$PostDin,	z$Weekend2)#,z$Computer,	z$Work_ONLY,	z$Travel2,		z$Sports,	z$Nothing,	z$Socialize, z$OBSatHi,z$OBSatMed, z$NorSatHi,z$NorSatMed)#,z$LowEComputer,z$LowEWork,z$LowESpor)#,z$MidEStudy, z$MidEEatM)#,#z$LowENothing,            z$LowEComputer,z$LowEWork,z$LowESpor,z$MidENothing,z$MidEComputer,z$MidEWork,z$MidESpor,z$LowEStudy, z$LowEEatM, z$LowETrav, )
x13	=	cbind(z$UNO,	z$age_182,z$age_65,		z$Female,	z$NWEIGHT,z$OBESE,				 z$PuffA,	z$PuffW,	z$IntPuffA,	z$IntPuffW,	z$PuffCH,	z$PuffCHW,	z$TotFat,	z$TotCarb,	z$TotFib,	z$TotProt,	z$BF,	z$BL,	z$LD,	z$PostDin,	z$Weekend2)#,z$Computer,	z$Work_ONLY,	z$Travel2,		z$Sports,	z$Nothing,	z$Socialize, z$OBSatHi,z$OBSatMed, z$NorSatHi,z$NorSatMed)#,z$LowEComputer,z$LowEWork,z$LowESpor)#,z$MidEStudy, z$MidEEatM)#,#z$LowENothing,            z$LowEComputer,z$LowEWork,z$LowESpor,z$MidENothing,z$MidEComputer,z$MidEWork,z$MidESpor,z$LowEStudy, z$LowEEatM, z$LowETrav, )
x14	=	cbind(z$UNO,	z$age_182,z$age_65,		z$Female,	z$NWEIGHT,z$OBESE,				 z$YogA, 	z$YogW,	  z$IntYogA,	z$IntYogW,	z$YogYp,	z$YogYpW,	 	z$TotFat,	z$TotCarb,	z$TotFib,	z$TotProt,	z$BF,	z$BL,	z$LD,	z$PostDin,	z$Weekend2)#,z$Computer,	z$Work_ONLY,	z$Travel2,		z$Sports,	z$Nothing,	z$Socialize, z$OBSatHi,z$OBSatMed, z$NorSatHi,z$NorSatMed)#,z$LowEComputer,z$LowEWork,z$LowESpor)#,z$MidEStudy, z$MidEEatM)#,#z$LowENothing,            z$LowEComputer,z$LowEWork,z$LowESpor,z$MidENothing,z$MidEComputer,z$MidEWork,z$MidESpor,z$LowEStudy, z$LowEEatM, z$LowETrav, )

#labels for these covariates
par_choice = c("Intercept",	"NWEIGHT",	"OBESE",	"age_182",	"Age_65",	"Female",	"Favourite",	"Healthy",	"Groups",	"Relax",	"Bored",	"Boost",	"Across-SD",	"Within-SD",	"IntA-SD",	"IntW-SD",	"Brand-Sd",	"Brand-W-SD",	"LowEx",	"MIDEx",	"TotFat",	"TotCarb",	"TotFib",	"TotProt",	"BF",	"BL",	"LD",	"PostDin",	"Weekend2"	)#,		"Computer","Work_ONLY","Travel2",		"Sports",	"Nothing",	"Socialize","OBSatHi","OBSatMed","NorSatHi","NorSatMed")#, "LowEComputer", "LowEWork", "LowESpor")#, "LowEComputer","LowEWork","LowESpor")#"MidEStudy", "MidEEatM", "MidETrav" ) #,"MidENothing","MidEComputer","MidEWork","MidESpor")

if (length(x1) != length(x2))   {stop("The value is TRUE, so the script must end here")}

print("Script did NOT end!")



#Define 14 Equations for Satiation here. Same as choice for covariates - keep them same
ax1  = cbind(z$UNO,  z$age_182,z$age_65,z$Female,z$NWEIGHT,z$OBESE,  z$TotFat,z$TotCarb,z$TotFib,z$TotProt,z$SERO,z$SERO,z$SERO,z$SERO,   z$SERO)#,z$SERO,   z$SERO, z$SERO,    z$SERO,     z$SERO,          z$SERO,  z$SERO,        z$SERO ,    z$SERO,   z$SERO,    z$SERO,      z$SERO,        z$SERO,    z$SERO,     z$SERO,        z$SERO,     z$SERO,        z$SERO )
ax2  = cbind(z$UNO,  z$age_182,z$age_65,z$Female,z$NWEIGHT,z$OBESE,	z$TotFat,z$TotCarb,z$TotFib,z$TotProt,z$BF,  z$BL,  z$LD, z$PostDin,z$Weekend2)#,z$Study,z$Eat_Meal,z$Travel2,z$Computer,	z$Work_ONLY,		z$Sports,	z$Nothing,	z$Socialize ,z$NorSatMed,z$OBSatHi,z$NorSatMed,z$LowEComputer,z$LowEWork,z$LowESpor,z$MidENothing,z$MidEComputer,z$MidEWork,z$MidESpor)
ax3  = cbind(z$UNO,	 z$age_182,z$age_65,z$Female,z$NWEIGHT,z$OBESE,	z$TotFat,z$TotCarb,z$TotFib,z$TotProt,z$BF,  z$BL,  z$LD, z$PostDin,z$Weekend2)#,z$Study,z$Eat_Meal,z$Travel2,z$Computer,	z$Work_ONLY,		z$Sports,	z$Nothing,	z$Socialize ,z$NorSatMed,z$NorSatMed,z$LowEComputer,z$LowEWork,z$LowESpor,z$MidENothing,z$MidEComputer,z$MidEWork,z$MidESpor)
ax4  = cbind(z$UNO,	 z$age_182,z$age_65,z$Female,z$NWEIGHT,z$OBESE,	z$TotFat,z$TotCarb,z$TotFib,z$TotProt,z$BF,  z$BL,  z$LD, z$PostDin,z$Weekend2)#,z$Study,z$Eat_Meal,z$Travel2,z$Computer,	z$Work_ONLY,		z$Sports,	z$Nothing,	z$Socialize ,z$NorSatMed,z$NorSatMed,z$LowEComputer,z$LowEWork,z$LowESpor,z$MidENothing,z$MidEComputer,z$MidEWork,z$MidESpor)
ax5  = cbind(z$UNO,	 z$age_182,z$age_65,z$Female,z$NWEIGHT,z$OBESE,	z$TotFat,z$TotCarb,z$TotFib,z$TotProt,z$BF,  z$BL,  z$LD, z$PostDin,z$Weekend2)#,z$Study,z$Eat_Meal,z$Travel2,z$Computer,	z$Work_ONLY,		z$Sports,	z$Nothing,	z$Socialize ,z$NorSatMed,z$NorSatMed,z$LowEComputer,z$LowEWork,z$LowESpor,z$MidENothing,z$MidEComputer,z$MidEWork,z$MidESpor)
ax6  = cbind(z$UNO,	 z$age_182,z$age_65,z$Female,z$NWEIGHT,z$OBESE,	z$TotFat,z$TotCarb,z$TotFib,z$TotProt,z$BF,  z$BL,  z$LD, z$PostDin,z$Weekend2)#,z$Study,z$Eat_Meal,z$Travel2,z$Computer,	z$Work_ONLY,		z$Sports,	z$Nothing,	z$Socialize ,z$NorSatMed,z$NorSatMed,z$LowEComputer,z$LowEWork,z$LowESpor,z$MidENothing,z$MidEComputer,z$MidEWork,z$MidESpor)
ax7  = cbind(z$UNO,	 z$age_182,z$age_65,z$Female,z$NWEIGHT,z$OBESE,	z$TotFat,z$TotCarb,z$TotFib,z$TotProt,z$BF,  z$BL,  z$LD, z$PostDin,z$Weekend2)#,z$Study,z$Eat_Meal,z$Travel2,z$Computer,	z$Work_ONLY,		z$Sports,	z$Nothing,	z$Socialize ,z$NorSatMed,z$NorSatMed,z$LowEComputer,z$LowEWork,z$LowESpor,z$MidENothing,z$MidEComputer,z$MidEWork,z$MidESpor)
ax8  = cbind(z$UNO,	 z$age_182,z$age_65,z$Female,z$NWEIGHT,z$OBESE,	z$TotFat,z$TotCarb,z$TotFib,z$TotProt,z$BF,  z$BL,  z$LD, z$PostDin,z$Weekend2)#,z$Study,z$Eat_Meal,z$Travel2,z$Computer,	z$Work_ONLY,		z$Sports,	z$Nothing,	z$Socialize ,z$NorSatMed,z$NorSatMed,z$LowEComputer,z$LowEWork,z$LowESpor,z$MidENothing,z$MidEComputer,z$MidEWork,z$MidESpor)
ax9  = cbind(z$UNO,	 z$age_182,z$age_65,z$Female,z$NWEIGHT,z$OBESE,	z$TotFat,z$TotCarb,z$TotFib,z$TotProt,z$BF,  z$BL,  z$LD, z$PostDin,z$Weekend2)#,z$Study,z$Eat_Meal,z$Travel2,z$Computer,	z$Work_ONLY,		z$Sports,	z$Nothing,	z$Socialize ,z$NorSatMed,z$NorSatMed,z$LowEComputer,z$LowEWork,z$LowESpor,z$MidENothing,z$MidEComputer,z$MidEWork,z$MidESpor)
ax10 = cbind(z$UNO,	 z$age_182,z$age_65,z$Female,z$NWEIGHT,z$OBESE,	z$TotFat,z$TotCarb,z$TotFib,z$TotProt,z$BF,  z$BL,  z$LD, z$PostDin,z$Weekend2)#,z$Study,z$Eat_Meal,z$Travel2,z$Computer,	z$Work_ONLY,		z$Sports,	z$Nothing,	z$Socialize ,z$NorSatMed,z$NorSatMed,z$LowEComputer,z$LowEWork,z$LowESpor,z$MidENothing,z$MidEComputer,z$MidEWork,z$MidESpor)
ax11 = cbind(z$UNO,	 z$age_182,z$age_65,z$Female,z$NWEIGHT,z$OBESE,	z$TotFat,z$TotCarb,z$TotFib,z$TotProt,z$BF,  z$BL,  z$LD, z$PostDin,z$Weekend2)#,z$Study,z$Eat_Meal,z$Travel2,z$Computer,	z$Work_ONLY,		z$Sports,	z$Nothing,	z$Socialize ,z$NorSatMed,z$NorSatMed,z$LowEComputer,z$LowEWork,z$LowESpor,z$MidENothing,z$MidEComputer,z$MidEWork,z$MidESpor)
ax12 = cbind(z$UNO,	 z$age_182,z$age_65,z$Female,z$NWEIGHT,z$OBESE,	z$TotFat,z$TotCarb,z$TotFib,z$TotProt,z$BF,  z$BL,  z$LD, z$PostDin,z$Weekend2)#,z$Study,z$Eat_Meal,z$Travel2,z$Computer,	z$Work_ONLY,		z$Sports,	z$Nothing,	z$Socialize ,z$NorSatMed,z$NorSatMed,z$LowEComputer,z$LowEWork,z$LowESpor,z$MidENothing,z$MidEComputer,z$MidEWork,z$MidESpor)
ax13 = cbind(z$UNO,	 z$age_182,z$age_65,z$Female,z$NWEIGHT,z$OBESE,	z$TotFat,z$TotCarb,z$TotFib,z$TotProt,z$BF,  z$BL,  z$LD, z$PostDin,z$Weekend2)#,z$Study,z$Eat_Meal,z$Travel2,z$Computer,	z$Work_ONLY,		z$Sports,	z$Nothing,	z$Socialize ,z$NorSatMed,z$NorSatMed,z$LowEComputer,z$LowEWork,z$LowESpor,z$MidENothing,z$MidEComputer,z$MidEWork,z$MidESpor)
ax14 = cbind(z$UNO,	 z$age_182,z$age_65,z$Female,z$NWEIGHT,z$OBESE,	z$TotFat,z$TotCarb,z$TotFib,z$TotProt,z$BF,  z$BL,  z$LD, z$PostDin,z$Weekend2)#,z$Study,z$Eat_Meal,z$Travel2,z$Computer,	z$Work_ONLY,		z$Sports,	z$Nothing,	z$Socialize ,z$NorSatMed,z$NorSatHi,z$NorSatMed,z$LowEComputer,z$LowEWork,z$LowESpor,z$MidENothing,z$MidEComputer,z$MidEWork,z$MidESpor)


par_satiation = c("Intercept","Relax",		"age_182",	"Age_65",	"Female",	"NormalWeight",	"Obese","Fat",	"Carb",	"Fib",	"Protein",	"BF",	"BL",	"LD",	"PostDin",	"Weekend2")#,	"Study",	"Eat_Meal",	"Travel",	"Computer",	"Work",		"Sports_Physical","Nothing","Socialize")#,"NorSatHi","NorSatMed","OBSatHi","OBSatMed" ,"LowEComputer","LowEWork","LowESpor","MidENothing","MidEComputer","MidEWork","MidESpor")

#Define Equations for Gamma here - set to 1. Not estimated in this model.
gx1 = cbind(z$UNO)
gx2 = cbind(z$UNO)
gx3 = cbind(z$UNO)
gx4 = cbind(z$UNO)
gx5 = cbind(z$UNO)
gx6 = cbind(z$UNO)
gx7 = cbind(z$UNO)
gx8 = cbind(z$UNO)
gx9 = cbind(z$UNO)
gx10 = cbind(z$UNO)
gx11 = cbind(z$UNO)
gx12 = cbind(z$UNO)
gx13 = cbind(z$UNO)
gx14 = cbind(z$UNO)


input_data=z
xsigm = matrix(1,1,1) #set to 1

## Define Coefficients Here

#Number of Variables with Varying Betas Across Alternatives Choice:
n_vbeta = ncol(x2[,c(1:6)])
#Number of Variables with Fixed Betas Across Alternatives - Choice:
n_fbeta = ncol(x2[,c((n_vbeta+1):ncol(x2))])

#Number of Variables with Varying Betas Across Alternatives Satiation:
n_vsbeta = ncol(as.matrix(ax2[,1:1]))
#Number of Variables with Fixed Betas Across Alternatives - Satiation:
n_fsbeta = ncol(ax2[,c((1+n_vsbeta):ncol(ax2))])

#Fixed Betas Across Alternatives - Gamma:
n_fgbeta = ncol(as.matrix(gx2))
n_xsigm = ncol(as.matrix(xsigm))

betas = list(c(matrix(0.0,n_vbeta*(nc-1)+n_fbeta+n_vsbeta*(nc)+n_fsbeta)))#+n_fgbeta),matrix(1,n_xsigm,1))) #these enter the likelihood function

nvarm = n_vbeta*(nc-1)+n_fbeta   #number of parameters in choice equations
nvardel = n_vsbeta*(nc)+n_fsbeta #number of parameters in satiation equation
nvargam = 1


###Likelihood function starts here.

lik_fun = function(betas,input_data){
  input_data=z
  e1 = nrow(input_data)
  wt = input_data[,"UNO"]
  beta_matrix = as.data.frame(betas)
  beta_u = as.matrix(beta_matrix[1:nvarm,])
  beta_s = as.matrix(beta_matrix[(nvarm+1):(nvarm+nvardel),])
  beta_g = 1.0e-200
  xsigm = 1
  
  vbeta = as.matrix(beta_u[1:(n_vbeta*(nc-1)),])
  fbeta = as.matrix(unlist(beta_u[(n_vbeta*(nc-1)+1):((n_vbeta*(nc-1))+n_fbeta),]))
  vbetas = as.matrix(beta_s[1:((n_vsbeta*(nc))),])
  fbetas = as.matrix(beta_s[((n_vsbeta*nc)+1):((n_vsbeta*nc)+n_fsbeta),])
  
  v1 = as.matrix(rowSums(x1*0))
  v2 = x2%*%rbind(as.matrix(vbeta[seq(1, nrow(vbeta), 13),]),fbeta)
  v3 = x3%*%rbind(as.matrix(vbeta[seq(2, nrow(vbeta), 13),]),fbeta)
  v4 = x4%*%rbind(as.matrix(vbeta[seq(3, nrow(vbeta), 13),]),fbeta)
  v5 = x5%*%rbind(as.matrix(vbeta[seq(4, nrow(vbeta), 13),]),fbeta)
  v6 = x6%*%rbind(as.matrix(vbeta[seq(5, nrow(vbeta), 13),]),fbeta)
  v7 = x7%*%rbind(as.matrix(vbeta[seq(6, nrow(vbeta), 13),]),fbeta)
  v8 = x8%*%rbind(as.matrix(vbeta[seq(7, nrow(vbeta), 13),]),fbeta)
  v9 = x9%*%rbind(as.matrix(vbeta[seq(8, nrow(vbeta), 13),]),fbeta)
  v10 = x10%*%rbind(as.matrix(vbeta[seq(9, nrow(vbeta), 13),]),fbeta)
  v11 = x11%*%rbind(as.matrix(vbeta[seq(10, nrow(vbeta), 13),]),fbeta)
  v12 = x12%*%rbind(as.matrix(vbeta[seq(11, nrow(vbeta), 13),]),fbeta)
  v13 = x13%*%rbind(as.matrix(vbeta[seq(12, nrow(vbeta), 13),]),fbeta)
  v14 = x14%*%rbind(as.matrix(vbeta[seq(13, nrow(vbeta), 13),]),fbeta)
  
  
  w1 = ax1%*%rbind(as.matrix(vbetas[seq(1, nrow(vbetas), 14),]),fbetas)
  w2 = ax2%*%rbind(as.matrix(vbetas[seq(2, nrow(vbetas), 14),]),fbetas)
  w3 = ax3%*%rbind(as.matrix(vbetas[seq(3, nrow(vbetas), 14),]),fbetas)
  w4 = ax4%*%rbind(as.matrix(vbetas[seq(4, nrow(vbetas), 14),]),fbetas)
  w5 = ax5%*%rbind(as.matrix(vbetas[seq(5, nrow(vbetas), 14),]),fbetas)
  w6 = ax6%*%rbind(as.matrix(vbetas[seq(6, nrow(vbetas), 14),]),fbetas)
  w7 = ax7%*%rbind(as.matrix(vbetas[seq(7, nrow(vbetas), 14),]),fbetas)
  w8 = ax8%*%rbind(as.matrix(vbetas[seq(8, nrow(vbetas), 14),]),fbetas)
  w9 = ax9%*%rbind(as.matrix(vbetas[seq(9, nrow(vbetas), 14),]),fbetas)
  w10 = ax10%*%rbind(as.matrix(vbetas[seq(10, nrow(vbetas), 14),]),fbetas)
  w11 = ax11%*%rbind(as.matrix(vbetas[seq(11, nrow(vbetas), 14),]),fbetas)
  w12 = ax12%*%rbind(as.matrix(vbetas[seq(12, nrow(vbetas), 14),]),fbetas)
  w13 = ax13%*%rbind(as.matrix(vbetas[seq(13, nrow(vbetas), 14),]),fbetas)
  w14 = ax14%*%rbind(as.matrix(vbetas[seq(14, nrow(vbetas), 14),]),fbetas)
  
  u1 = gx1*beta_g
  u2 = gx2*beta_g
  u3 = gx3*beta_g
  u4 = gx4*beta_g
  u5 = gx5*beta_g
  u6 = gx6*beta_g
  u7 = gx7*beta_g
  u8 = gx8*beta_g
  u9 = gx9*beta_g
  u10 = gx10*beta_g
  u11 = gx11*beta_g
  u12 = gx12*beta_g
  u13 = gx13*beta_g
  u14 = gx14*beta_g
  
  xsigm = matrix(1,1,1)
  
  v=cbind(v1,v2,v3,v4,v5,v6,v7,v8,v9,v10,v11,v12,v13,v14)  
  a = 1/(1+exp(cbind(w1,w2,w3,w4,w5,w6,w7,w8,w9,w10,w11,w12,w13,w14)))
  f = exp(cbind(u1,u2,u3,u4,u5,u6,u7,u8,u9,u10,u11,u12,u13,u13))
  
  m=rowSums(b)
  
  c=a*b/((c_occ+f))
  c=c/z[,flagprcm]
  c[(b == 0)] <- 1
  e=(1/c)*b
  d=as.matrix(colSums(t(e)))
  c=as.matrix(apply(c, 1, prod))*d
  v=v-a*log((z[,flagchm]+f)/f)-log(z[,flagprcm])
  ut=v/xsigm
# p1=exp(ut)
  
  p1=exp(ut)
  p2=(p1*z[,flagavm])/rowSums(p1*z[,flagavm])
  p3=p2
  p3[(b == 0)] <- 1
  p4=as.matrix(apply((p3),1,prod))*c*factorial(m-1)
  p4=p4/((xsigm)^(m-1))
  logl=p4
  wx=matrix(0,e1,1)
  log_like=-sum(log(logl))
  print(c(as.matrix(unlist(betas))[79:106,],log_like))
  return(log_like)
}


###Gradient Function
grad_fun = function(betas,input_data){
  input_data=z
  e1=nrow(input_data)
  wt=input_data[,"UNO"]
  popass=input_data[,"UNO"]
  p0=matrix(0,e1,1)
  beta_matrix = as.data.frame(betas)
  beta_u = as.matrix(beta_matrix[1:nvarm,])
  beta_s = as.matrix(beta_matrix[(nvarm+1):(nvarm+nvardel),])
  beta_g = 1.0e-200
  xsigm = 1
  #xsigm = matrix(1,1,1)
  
  
  vbeta = as.matrix(beta_u[1:(n_vbeta*(nc-1)),])
  fbeta = as.matrix(unlist(beta_u[(n_vbeta*(nc-1)+1):((n_vbeta*(nc-1))+n_fbeta),]))
  vbetas = as.matrix(beta_s[1:((n_vsbeta*(nc))),])
  fbetas = as.matrix(beta_s[((n_vsbeta*nc)+1):((n_vsbeta*nc)+n_fsbeta),])
  
  v1 = as.matrix(rowSums(x1*0))
  v2 = x2%*%rbind(as.matrix(vbeta[seq(1, nrow(vbeta), 13),]),fbeta)
  v3 = x3%*%rbind(as.matrix(vbeta[seq(2, nrow(vbeta), 13),]),fbeta)
  v4 = x4%*%rbind(as.matrix(vbeta[seq(3, nrow(vbeta), 13),]),fbeta)
  v5 = x5%*%rbind(as.matrix(vbeta[seq(4, nrow(vbeta), 13),]),fbeta)
  v6 = x6%*%rbind(as.matrix(vbeta[seq(5, nrow(vbeta), 13),]),fbeta)
  v7 = x7%*%rbind(as.matrix(vbeta[seq(6, nrow(vbeta), 13),]),fbeta)
  v8 = x8%*%rbind(as.matrix(vbeta[seq(7, nrow(vbeta), 13),]),fbeta)
  v9 = x9%*%rbind(as.matrix(vbeta[seq(8, nrow(vbeta), 13),]),fbeta)
  v10 = x10%*%rbind(as.matrix(vbeta[seq(9, nrow(vbeta), 13),]),fbeta)
  v11 = x11%*%rbind(as.matrix(vbeta[seq(10, nrow(vbeta), 13),]),fbeta)
  v12 = x12%*%rbind(as.matrix(vbeta[seq(11, nrow(vbeta), 13),]),fbeta)
  v13 = x13%*%rbind(as.matrix(vbeta[seq(12, nrow(vbeta), 13),]),fbeta)
  v14 = x14%*%rbind(as.matrix(vbeta[seq(13, nrow(vbeta), 13),]),fbeta)
  
  
  w1 = ax1%*%rbind(as.matrix(vbetas[seq(1, nrow(vbetas), 14),]),fbetas)
  w2 = ax2%*%rbind(as.matrix(vbetas[seq(2, nrow(vbetas), 14),]),fbetas)
  w3 = ax3%*%rbind(as.matrix(vbetas[seq(3, nrow(vbetas), 14),]),fbetas)
  w4 = ax4%*%rbind(as.matrix(vbetas[seq(4, nrow(vbetas), 14),]),fbetas)
  w5 = ax5%*%rbind(as.matrix(vbetas[seq(5, nrow(vbetas), 14),]),fbetas)
  w6 = ax6%*%rbind(as.matrix(vbetas[seq(6, nrow(vbetas), 14),]),fbetas)
  w7 = ax7%*%rbind(as.matrix(vbetas[seq(7, nrow(vbetas), 14),]),fbetas)
  w8 = ax8%*%rbind(as.matrix(vbetas[seq(8, nrow(vbetas), 14),]),fbetas)
  w9 = ax9%*%rbind(as.matrix(vbetas[seq(9, nrow(vbetas), 14),]),fbetas)
  w10 = ax10%*%rbind(as.matrix(vbetas[seq(10, nrow(vbetas), 14),]),fbetas)
  w11 = ax11%*%rbind(as.matrix(vbetas[seq(11, nrow(vbetas), 14),]),fbetas)
  w12 = ax12%*%rbind(as.matrix(vbetas[seq(12, nrow(vbetas), 14),]),fbetas)
  w13 = ax13%*%rbind(as.matrix(vbetas[seq(13, nrow(vbetas), 14),]),fbetas)
  w14 = ax14%*%rbind(as.matrix(vbetas[seq(14, nrow(vbetas), 14),]),fbetas)
  
  u1 = gx1*beta_g
  u2 = gx2*beta_g
  u3 = gx3*beta_g
  u4 = gx4*beta_g
  u5 = gx5*beta_g
  u6 = gx6*beta_g
  u7 = gx7*beta_g
  u8 = gx8*beta_g
  u9 = gx9*beta_g
  u10 = gx10*beta_g
  u11 = gx11*beta_g
  u12 = gx12*beta_g
  u13 = gx13*beta_g
  u14 = gx14*beta_g
  

  v=cbind(v1,v2,v3,v4,v5,v6,v7,v8,v9,v10,v11,v12,v13,v14)  
  a = 1/(1+exp(cbind(w1,w2,w3,w4,w5,w6,w7,w8,w9,w10,w11,w12,w13,w14)))
  f = exp(cbind(u1,u2,u3,u4,u5,u6,u7,u8,u9,u10,u11,u12,u13,u13))
  
  m=rowSums(b)
  
  c=a*b/((c_occ+f))
  c=c/z[,flagprcm]
  c[(b == 0)] <- 1
  e=(1/c)*b
  d=as.matrix(colSums(t(e)))
  c=as.matrix(apply(c, 1, prod))*d
  v=v-a*log((z[,flagchm]+f)/f)-log(z[,flagprcm])
  ut=v/xsigm
  uts = -ut/xsigm
  p1=exp(ut)
  p2=(p1*z[,flagavm])/rowSums(p1*z[,flagavm])
  p3=p2
  p3[(b == 0)] <- 1
  p4=as.matrix(apply((p3),1,prod))*c*factorial(m-1)
  p4=p4/((xsigm)^(m-1))
  

  g2 = b*1-m*p2
  g2s = g2*uts
  h = z[,flagchm]+f
  
  g2 = (1/xsigm)*((g2))*as.vector(p4)
  g2s = (rowSums(g2s)-((m-1)/xsigm))
                   
  
  g2d = g2*(((log(h/f))*(a*(1-a))))+as.vector(p4)*(a-1)*as.vector(b)+as.vector(p4)*(e/d)*(1-a)
  g2g = (g2*(-a)*(1/h)*(-1/f)*z[,flagchm]+p4*(-1/h)*b+p4*(e/d)*(1/h))*f
  

# Create the dataset for choice model - we stack the data and the reshape it for the varying parameters
  ylargev_varying=rbind(x2[,1:n_vbeta],x3[,1:n_vbeta],x4[,1:n_vbeta],x5[,1:n_vbeta],x6[,1:n_vbeta],x7[,1:n_vbeta],x8[,1:n_vbeta],x9[,1:n_vbeta],x10[,1:n_vbeta],x11[,1:n_vbeta],x12[,1:n_vbeta],x13[,1:n_vbeta],x14[,1:n_vbeta])
  dim(ylargev_varying)=c(e1,(nc-1)*(n_vbeta))
# For the fixed parameters, we concatenate the matrices
  ylargev_fixed=cbind(x2[,(n_vbeta+1):(n_vbeta+n_fbeta)],x3[,(n_vbeta+1):(n_vbeta+n_fbeta)],x4[,(n_vbeta+1):(n_vbeta+n_fbeta)],x5[,(n_vbeta+1):(n_vbeta+n_fbeta)],x6[,(n_vbeta+1):(n_vbeta+n_fbeta)],x7[,(n_vbeta+1):(n_vbeta+n_fbeta)],x8[,(n_vbeta+1):(n_vbeta+n_fbeta)],x9[,(n_vbeta+1):(n_vbeta+n_fbeta)],x10[,(n_vbeta+1):(n_vbeta+n_fbeta)],x11[,(n_vbeta+1):(n_vbeta+n_fbeta)],x12[,(n_vbeta+1):(n_vbeta+n_fbeta)],x13[,(n_vbeta+1):(n_vbeta+n_fbeta)],x14[,(n_vbeta+1):(n_vbeta+n_fbeta)])
     
# Multiply the corresponding gradient calculations of each equation with the corresponding data
  # For varying parameters, we use the stacked data
  # For fixed parameters, we use the concatenated data and add every kth column (k= being number of fixed parameters)
  gvx_varying=matrix(rep(as.matrix(g2[,2:nc]),n_vbeta),ncol=ncol(as.matrix(g2[,2:nc]))*(n_vbeta),byrow=FALSE)
  gvx_fixed<-as.matrix(g2[,2:nc][rep(rownames(g2[,2:nc]),n_fbeta),]) #use rep function while doing indexing 
  dim(gvx_fixed)=c(e1,n_fbeta*(nc-1))
  gv_varying = gvx_varying*ylargev_varying
  gv_fixed   = gvx_fixed*ylargev_fixed
  gv_fixed=t(rowsum(t(gv_fixed),gl(n_fbeta,1,ncol(gv_fixed))))

### This part is for calculating the values of gradient of satiation parameters   
  
  ylargevs_varying=rbind(ax1[,1:n_vsbeta],ax2[,1:n_vsbeta],ax3[,1:n_vsbeta],ax4[,1:n_vsbeta],ax5[,1:n_vsbeta],ax6[,1:n_vsbeta],ax7[,1:n_vsbeta],ax8[,1:n_vsbeta],ax9[,1:n_vsbeta],ax10[,1:n_vsbeta],ax11[,1:n_vsbeta],ax12[,1:n_vsbeta],ax13[,1:n_vsbeta],ax14[,1:n_vsbeta])
  dim(ylargevs_varying)=c(e1,(nc)*(n_vsbeta))
  ylargevs_fixed=cbind(ax1[,(n_vsbeta+1):(n_vsbeta+n_fsbeta)],ax2[,(n_vsbeta+1):(n_vsbeta+n_fsbeta)],ax3[,(n_vsbeta+1):(n_vsbeta+n_fsbeta)],ax4[,(n_vsbeta+1):(n_vsbeta+n_fsbeta)],ax5[,(n_vsbeta+1):(n_vsbeta+n_fsbeta)],ax6[,(n_vsbeta+1):(n_vsbeta+n_fsbeta)],ax7[,(n_vsbeta+1):(n_vsbeta+n_fsbeta)],ax8[,(n_vsbeta+1):(n_vsbeta+n_fsbeta)],ax9[,(n_vsbeta+1):(n_vsbeta+n_fsbeta)],ax10[,(n_vsbeta+1):(n_vsbeta+n_fsbeta)],ax11[,(n_vsbeta+1):(n_vsbeta+n_fsbeta)],ax12[,(n_vsbeta+1):(n_vsbeta+n_fsbeta)],ax13[,(n_vsbeta+1):(n_vsbeta+n_fsbeta)],ax14[,(n_vsbeta+1):(n_vsbeta+n_fsbeta)])

  gvxs_varying=matrix(rep(as.matrix(g2d[,1:nc]),n_vsbeta),ncol=ncol(as.matrix(g2d[,1:nc]))*(n_vsbeta),byrow=FALSE)
  gvxs_fixed  =as.matrix(g2d[,1:nc][rep(rownames(g2d[,1:nc]),n_fsbeta),])
  dim(gvxs_fixed)=c(e1,n_fsbeta*nc)
  gvs_varying = gvxs_varying*ylargevs_varying
  gvs_fixed   = gvxs_fixed*ylargevs_fixed
  gvs_fixed=t(rowsum(t(gvs_fixed),gl(n_fsbeta,1,ncol(gvs_fixed))))
  
  gv=cbind(gv_varying,gv_fixed)
  gd=cbind(gvs_varying,gvs_fixed)
    
  gradient_value = cbind(gv/as.vector(p4),gd/as.vector(p4))
  #dim(gradient_value)=c(e1*length(c(vbeta,fbeta,vbetas,fbetas)),1)
  #gradient_value=unlist(gradient_value)
  grad=unlist(colSums(gradient_value))
    
return(-grad)  
}



#first process the likelihood and gradient functions. Select the likelihood and gradient function and hit CTRL+ENTER
time1=Sys.time()
#final_op=optim(par = unlist(betas),hessian = TRUE,fn = lik_fun, gr = grad_fun(betas,input_data),method = "BFGS",control=list(trace=TRUE))
final_op=optim(unlist(betas),lik_fun,gr = grad_fun,hessian = TRUE,method = "BFGS",control=list(trace=TRUE,maxit=1000,reltol=1e-8))
time2=Sys.time()
time3=time2-time1
print(time3)
#final_op calls the optim function, which in turn refers to likelihood and gradient function.
#We use BFGS algorithm. trace=TRUE shows the iteration number and likelihood value. Max iterations set to 1000. Tolerance to 1e-8

#generates the final results
generate_stuff = function(x){
  vcov=solve(final_op$hessian)
  se=sqrt(diag(vcov))
  
  vchoice=t(rep((((par_choice[1:n_vbeta]))),n_vbeta,n_vbeta*(nc-1)))
  dim(vchoice)=c(n_vbeta,nc-1)
  #tn=1:(nc-1)
  #tk=1:n_vbeta
  #vchoice[tn,]=(t(vchoice)[,tk])
  vchoice=t(vchoice)
  dim(vchoice)=c((nc-1)*n_vbeta,1)
  #dim(vchoice)=c(n_vbeta*(nc-1),1)
  
  vchoicesat=t(rep(par_satiation[1:n_vsbeta],n_vsbeta,n_vsbeta*(nc)))
  dim(vchoicesat)=c(n_vsbeta,nc)
  tn=1:n_vsbeta
  vchoicesat=t(vchoicesat)
  dim(vchoicesat)=c(n_vsbeta*(nc),1)
  
  varying_choice = vchoice
  fixed_choice   = as.matrix(par_choice[(n_vbeta+1):(n_vbeta+n_fbeta)])
  varying_sat    = vchoicesat
  fixed_sat      = as.matrix(par_satiation[(n_vsbeta+1):(n_vsbeta+n_fsbeta)])
  
  par_names = matrix(rbind(varying_choice,fixed_choice,varying_sat,fixed_sat))
  ll = final_op$value
  est=final_op$par
  tstat=final_op$par/se
  pvalue=2*pt(q = abs(tstat),df = length(z)-1,lower.tail = FALSE)
  
  colnames(par_names) = "Parameter"
  print(as.data.frame(cbind(par_names,est,se,tstat,pvalue)))
  print (c(ll,nrows))


  return(list(print(as.data.frame(cbind(par_names,est,se,tstat,pvalue))) , print(c(ll,nrows))))    
}

if(!require(beepr)){ #alerts user by playing a short note/sound that the code has run.
  install.packages("beepr")
  library(beepr)
}
beep(sound = 5,expr = "Done!")

t1=generate_stuff(final_op) #t1 stores the results. First build the function - select and hit CTRL+ENTER
##############################END##################################################