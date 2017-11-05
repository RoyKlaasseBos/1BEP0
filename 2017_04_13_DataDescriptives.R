#DATA DESCRIPTIVES
#Thesis: Perk Pricing Strategies for Reward Based Crowdfunding
#Author: R.J. Klaasse Bos 
#Educational Institution: Eindhoven University of Technology
#Email: r.j.klaasse.bos@student.tue.nl

#========================================================
#   IMPORT DATA 
#========================================================
generalStats = read.csv("Kickstarter_preprocessed.csv")

#========================================================
#   OUTLIER REMOVAL 
#========================================================
outlierRemoval = function(column, dataframe){
  outliersHypothesis = quantile(dataframe[,column])
  hypothesisLowerLimit = max(outliersHypothesis[2] - 1.5*(outliersHypothesis[4]-outliersHypothesis[2]),0)
  hypothesisUpperLimit = max(outliersHypothesis[4] + 1.5*(outliersHypothesis[4]-outliersHypothesis[2]),0)
  print(hypothesisLowerLimit) #lower limit
  print(hypothesisUpperLimit) #upper limit
  return(dataframe[dataframe[,column] >= hypothesisLowerLimit & dataframe[,column] <= hypothesisUpperLimit, ])
}


#CAMPAIGNS BACKED BY CREATOR
campaignsBacked = outlierRemoval("backed", generalStats)

mean(campaignsBacked[campaignsBacked$success == 2,"backed"]) #1.00 campaigns backed
mean(campaignsBacked[campaignsBacked$success == 1,"backed"]) #0.35 campaigns backed
mean(as.numeric(campaignsBacked[campaignsBacked$backed >= 1,"success"])) #59,18% success
mean(as.numeric(campaignsBacked[campaignsBacked$backed == 0,"success"])) #27,04% success

binsCampaignsBacked = factor(c("0","1","2","3","4", "5 or more"))
successCampaignsBacked = data.frame(backed=binsCampaignsBacked, success=NA)

for(i in 0:4){
  successCampaignsBacked[i+1,2] = (mean(as.numeric(generalStats[generalStats$backed == i,"success"]))) * 100    
}
successCampaignsBacked[6,2] = (mean(as.numeric(generalStats[generalStats$backed >= 5,"success"]))) * 100
successCampaignsBacked[,2] = round(successCampaignsBacked[,2]) 

ggplot(successCampaignsBacked, aes(x=backed, y=success)) +
  geom_bar(stat="identity", fill="#23305a", color="black") + ylab("Average success rate (%)") + xlab("Number of campaigns backed") + ggtitle("Success vs #Campaigns a Founder Backed") + geom_text(aes(label=paste(success,"%"), y = success/2), size=7, color="white") + 
  theme(legend.position = "none",
        text = element_text(family="Lato"),
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        axis.title.x = element_text(size=22),
        axis.title.y = element_text(size=22),
        legend.title = element_text(size=20),
        legend.text = element_text(size=16),
        plot.title = element_text(size=27, face="bold"))


#AVERAGE BACKERS REQUIRED
averageBackersRequired = outlierRemoval("averageBackersRequired", generalStats)
mean(averageBackersRequired[averageBackersRequired$success == 0, "averageBackersRequired"]) #143.8
mean(averageBackersRequired[averageBackersRequired$success == 1, "averageBackersRequired"]) #101.2

#========================================================
#   CORRELATION TABLE
#========================================================
#install.packages("Hmisc")
library(Hmisc)
res2 <- rcorr(as.matrix(generalStats[,c("success","goal","totalNumberBackers","numUpdates","numComments", "duration", "numCollaborators", "numImages", "facebookFriends", "averageMoneyReward", "lowestPerk", "highestPerk", "numPerks", "estimatedDelivery", "totWordCount", "created", "backed", "pastSuccessRate", "experience", "numCompetitors")]))

#========================================================
#   ESTIMATED DELIVERY
#========================================================
estimatedDelivery = outlierRemoval("estimatedDelivery", generalStats)
ggplot(estimatedDelivery, aes(x=factor(success), y=estimatedDelivery)) + 
  geom_jitter(alpha=0.5, aes(color=factor(success)), size=2) + geom_boxplot(aes(fill=success), alpha=0.1, outlier.color=NA) + 
  coord_cartesian(ylim=c(0,30)) + ggtitle("Dollar value of cheapest perk for unsuccessful (0) and successful campaigns (1)") +
  xlab("Campaign success") + ylab("Cheapest perk ($)")

median(estimatedDelivery[estimatedDelivery$success == 1, "estimatedDelivery"])

#Estimated delivery vs number of backers
perkStatsComplete = merge(perkStats, generalStats, by.x="url", by.y="url")
perkStatsComplete = outlierRemoval("percentageGoal", perkStatsComplete)
perkStatsComplete$estimatedDeliveryPerks = (perkStatsComplete$estimatedDelivery.x - perkStatsComplete$endDate)/60/60/24
perkStatsComplete$estimatedDeliveryPerks = ifelse(perkStatsComplete$estimatedDeliveryPerks < 0, 0, perkStatsComplete$estimatedDeliveryPerks)

perkStatsComplete$perkPriceDollarBin = ifelse(perkStatsComplete$perkPriceDollar < 50, "$0-$50", ifelse(perkStatsComplete$perkPriceDollar < 100, 2, ifelse(perkStatsComplete$perkPriceDollar < 500, 3, ifelse(perkStatsComplete$perkPriceDollar < 1000, 4, ifelse(perkStatsComplete$perkPriceDollar >= 1000, 5, 6)))))


ggplot(data=perkStatsComplete, aes(x=estimatedDeliveryPerks, y=percentageGoal)) + geom_point(aes(color=success),alpha=.1) + coord_cartesian(xlim=(c(0,365)), ylim=c(0,200)) + geom_smooth() + 
  ggtitle("Percentage Goal vs Estimated Delivery") + xlab("Estimated delivery (days)") + ylab("Percentage Goal (%)") +
  theme(text = element_text(family="Lato"),
        legend.position = "none",
      axis.text.x = element_text(size=12),
      axis.text.y = element_text(size=12),
      axis.title.x = element_text(size=15),
      axis.title.y = element_text(size=15),
      legend.title = element_text(size=18),
      legend.text = element_text(size=14),
      plot.title = element_text(size=23, face="bold"))

#The longer the estimated delivery perks, the higher price, the higher the observed value
ggplot(data=perkStatsComplete, aes(x=estimatedDeliveryPerks, y=perkPriceDollar)) + geom_point(alpha=.1) + geom_smooth() + coord_cartesian(xlim=(c(0,365)), ylim=c(0,1000)) + 
  ggtitle("Perk Price ($) vs Estimated Delivery") + xlab("Estimated delivery (days)") + ylab("Perk Price ($)") + 
  theme(text = element_text(family="Lato"),
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        axis.title.x = element_text(size=15),
        axis.title.y = element_text(size=15),
        legend.title = element_text(size=18),
        legend.text = element_text(size=14),
        plot.title = element_text(size=23, face="bold"))

ggplot(data=perkStatsComplete, aes(x=estimatedDeliveryPerks, y=goal)) + geom_point(alpha=.1) + geom_smooth() + coord_cartesian(xlim=(c(0,365)), ylim=c(0,100000))
ggplot(data=perkStatsComplete, aes(x=estimatedDeliveryPerks, y=highestPerk)) + geom_point(alpha=.1) + geom_smooth() + coord_cartesian(xlim=(c(0,365)), ylim=c(0,5000))
ggplot(data=perkStatsComplete, aes(x=estimatedDeliveryPerks, y=duration)) + geom_point(alpha=.1) + geom_smooth() + coord_cartesian(xlim=(c(0,365)), ylim=c(0,60))

#Determine unique number of estimated deliveries
for(url in perkStatsComplete$url){
  perkStatsComplete[perkStatsComplete$url == url, "uniqueEstimatedDelivery"] = length(unique(perkStatsComplete[perkStatsComplete$url == url, "estimatedDelivery.x"]))
}

for(url in perkStatsComplete$url){
  generalStats[generalStats$url == url, "uniqueEstimatedDelivery"] = unique(perkStatsComplete[perkStatsComplete$url == url, "uniqueEstimatedDelivery"])
}

#Modellen fitten
library(caTools)
set.seed(123)
split = sample.split(perkStatsComplete$estimatedDeliveryPerks, SplitRatio = 0.8)
training_set = subset(perkStatsComplete, split == TRUE)
test_set = subset(perkStatsComplete, split == FALSE)

# Feature Scaling
# training_set = scale(training_set)
# test_set = scale(test_set)

# Fitting Multiple Linear Regression to the Training set
regressor = lm(formula = percentageGoal ~  perkPriceDollar + estimatedDeliveryPerks + duration + goal + numUpdates + numComments + totWordCount + backed , data = training_set)
regressor = lm(formula = numBackers ~  estimatedDeliveryPerks + numUpdates + numComments, data = training_set)

#Logistic Regression Model
regressor = glm(formula = success ~ perkPriceDollar + estimatedDelivery.y + duration + goal + numUpdates + numComments + totWordCount + backed,
    family = binomial,
    data = training_set)

y_pred = predict(regressor, newdata = test_set)
y_pred = prediction(y_pred, test_set$success)

library(nnet)
#Build model
mymodel = multinom(success~perkPriceDollar + estimatedDelivery.y + duration + goal + numUpdates + numComments + totWordCount + backed, data=training_set)
pred = predict(mymodel, training_set, type="prob")

#histogram of predictions
hist(pred)

#Create plot of accuracy vs cut-off level 
pred = prediction(pred, training_set$success)
eval = performance(pred, "acc")
plot(eval)
abline(h=0.802, v=0.4395)

#Determine peak numerically
max = which.max(slot(eval, "y.values")[[1]])
accuracy = slot(eval, "y.values")[[1]][max]
cut = slot(eval, "x.values")[[1]][max]

#ROC curve (Receiver Operating Characteristic)
#Als positives belangrijker zijn dan overall accuracy (bijv. faillisementen bij een bank)
pred = prediction(pred, training_set$success)
roc = performance(pred, "tpr", "fpr")
plot(roc,
     colorize= T, 
     main = "ROC Curve")
abline(a=0, b=1)
#true positive rate (tpr) = true positive / (true positive + false negative) = van alle positieven: welk deel classificeer je correct?
#fpr = false positive / (true negative + false positive) = van alle negatieven: welk deel classificeer je correct?

#Als de FPR toeneemt, zeg je eigenlijk dat meer records positief zijn terwijl ze eigenlijk negatief zijn (te optimistisch)
#Als de TPR toeneemt, zeg je eigenlijk dat meer records positief zijn terwijl ze ook positief zijn
#De lijn omhoog kan dus verklaard worden doordat je de cut-off waarden lager kiest zul je meer records positief classificeren waardoor zowel het aantal TP als FP toeneemt

#AUC (Area Under Curve)
#Onder de diagonaal is al 50% dus het moet sowieso meer dan dat zijn
auc = performance(pred, "auc")
auc = unlist(slot(auc, "y.values"))



#Check for multicollenearity: 
library(usdm)
df = generalStats[,c("created", "pastSuccessRate")]
vif(df)

split = sample.split(generalStats$url, SplitRatio = 0.8)
training_set = subset(generalStats, split == TRUE)
test_set = subset(generalStats, split == FALSE)

training_set[,c("numCompetitors")] = scale(training_set[,c("numCompetitors")])
test_set[,c("numCompetitors")] = scale(test_set)

regressor = glm(formula = success ~ category + numCompetitors + goal + numCollaborators + estimatedDelivery + pastSuccessRate + numUpdates + projectsWeLove + created + facebookFriends + comments + medianPerk + totWordCount + mainVideo + numImages + noFacebookFriends,
                family = binomial,
                data = training_set)

#facebook friends
regressor = glm(formula = success ~ facebookFriends + mainVideo + numImages + noFacebookFriends + category,
                family = binomial,
                data = training_set)




#========================================================
#   OUTLIER REMOVAL 
#========================================================
generalStats$averageMoneyReward = generalStats$goal / generalStats$numPerks
generalStats$experience = as.numeric(generalStats$experience)
column = "numImages"
#descriptives = outlierRemoval(column, generalStats)
mean(generalStats[,column])
median(generalStats[,column])
Mode(generalStats[,column])
sd(generalStats[,column])

min(generalStats[,column])
max(generalStats[,column])

#Mode dollar value of lowest perk for unsuccessful and successful projects
Mode = function(x) {
  ux = unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}



#========================================================
#   IMPORT DATA 
#========================================================
generalStats = read.csv('GeneralStats.csv')
perkStats = read.csv('PerkStats.csv')
states = read.csv("Abbreviation_State.csv")
collaborators = read.csv("Collaborators.csv")
creatorBio = read.csv("CreatorBio.csv")
creatorProfileGeneral = read.csv("CreatorProfileGeneral.csv")
pastSuccessful = read.csv("SuccessfulCampaigns.csv")
liveCampaigns = read.csv("LiveCampaigns.csv")
campaignQuality = read.csv("CampaignQuality.csv")
viewGallery = read.csv("ViewGallery.csv")

#========================================================
#   MERGE DATA
#========================================================
#Number of projects backed by author, number of comments by author, total number of campaigns created, registration date KS
generalStats = merge(generalStats, creatorProfileGeneral, by.x="urlProfile", by.y="urlCreated") 
generalStats[,c("urlProjects","lengthURL","slashLocation","slashLocation2","X_cached_page_id","X_template","X_type", "username")] = NULL

#Number of Facebook friends, websites
generalStats = merge(generalStats, creatorBio, by.x="url", by.y="url") 
generalStats[,c("X_cached_page_id", "generalStats$X_template", "slashLocation", "createdBacked","generalStats$X_type", "X_type", "X_template", "urlBare", "generalStats$urlBare", "generalStats$slashLocation")] = NULL


#========================================================
#   LOCATION CAMPAIGN
#========================================================
generalStats$location = as.character(generalStats$location)

for(url in generalStats$url){
  #Determine comma location for "Non-rojects we love"
  generalStats$commaLocation[generalStats$url == url] = gregexpr(pattern =",",generalStats$location[generalStats$url == url])[[1]]
  
  #Determine country / state by taking a substring of the location field
  generalStats$countryState[generalStats$url == url] = substring(generalStats[generalStats$url == url, "location"], generalStats[generalStats$url == url, "commaLocation"][[1]]+2)
  
  #Determine city
  generalStats$city[generalStats$url == url] = substring(generalStats[generalStats$url == url, "location"], first=1, last=generalStats[generalStats$url == url, "commaLocation"][[1]]-1)
}

#Update location info (due to "Projects we love" column)
missingLocationsURL = generalStats[generalStats$commaLocation == -1,"url"]
location = c("London, UK",	"Winnipeg, Canada",	"Seoul, South Korea",	"New York, NY",	"Eugene, OR",	"Guanajuato, Mexico",	"New York, NY",	"Sydney, AU",	"Athens, Greece",	"Beaufort, SC",	"Brooklyn, NY",	"Rotterdam, Netherlands",	"Ottawa, Canada",	"Lafayette, LA",	"Milan, Italy",	"Estado de Mexico, Mexico",	"Tokyo, Japan",	"Rotterdam, Netherlands",	"London, UK",	"New York, NY",	"London, UK",	"San Mauro Pascoli, Italy",	"Concord, CA",	"Preston, UK", "Burbank, CA",	"Portland, OR",	"Ottawa, Canada",	"Vancouver, Canada",	"Boulder, CO",	"Portland, OR",	"Brighton and Hove City, UK",	"Brisbane, AU",	"Phnom Penh, Cambodia",	"Oakland, CA",	"Palo Alto, CA",	"Jersey City, NJ")
missingLocationsMatrix = cbind(url = as.character(missingLocationsURL), location=location)

#Add "Projects we love" as a attribute
for(url in missingLocationsURL){
  generalStats$projectsWeLove[generalStats$url == url] = TRUE  
}

#Replace NAs in projectsWeLove column by FALSE
generalStats[is.na(generalStats$projectsWeLove), "projectsWeLove"] = FALSE

#Determine location of comma for "Projects we love"
for(i in 1:nrow(missingLocationsMatrix)){
  generalStats$location[generalStats$url == missingLocationsMatrix[i,1]] = missingLocationsMatrix[i,2]
  generalStats$commaLocation[generalStats$url == missingLocationsMatrix[i,1]] = gregexpr(pattern =",",generalStats$location[generalStats$url == missingLocationsMatrix[i,1]])[[1]]
}

#Determine position of comma for fields with two commas
for(url in generalStats$url){
  generalStats$commaLocationMultipleCommas[generalStats$url == url] = gregexpr(pattern =",",generalStats$countryState[generalStats$url == url])[[1]]
  
  #Determine country / state by taking a substring of the location field (Projects We Love)
  generalStats$countryState[generalStats$url == url] = substring(generalStats[generalStats$url == url, "location"], generalStats[generalStats$url == url, "commaLocation"][[1]]+2)
  
  #Determine city (Projects We Love)
  generalStats$city[generalStats$url == url] = substring(generalStats[generalStats$url == url, "location"], first=1, last=generalStats[generalStats$url == url, "commaLocation"][[1]]-1)
}

#Create subset of fields with multiple commas
missingLocationsMultipleCommas = generalStats[generalStats$commaLocationMultipleCommas != -1,"url"]

#Determine country / state for fields with two commas
for(url in missingLocationsMultipleCommas){
  generalStats$countryState[generalStats$url == url] = substring(generalStats[generalStats$url == url, "countryState"],generalStats[generalStats$url == url, "commaLocationMultipleCommas"][[1]]+2)
}

#Determine city for field with two commas 
generalStats$secondComma  = generalStats$commaLocation + generalStats$commaLocationMultipleCommas
for(url in missingLocationsMultipleCommas){
  generalStats$city[generalStats$url == url] = substring(generalStats[generalStats$url == url, "location"], first=generalStats[generalStats$url == url, "commaLocation"][[1]]+2, last=generalStats[generalStats$url == url, "secondComma"][[1]])
}

#Convert abbreviations of states and countries into full state/country names
states[,1] = as.character(states[,1])
states[,2] = as.character(states[,2])
states[,3] = NULL
for(i in 1:nrow(states)){
  generalStats$countryState[generalStats$countryState == states[i,1]] = states[i,2]
}  

#Remove redundant columns
generalStats[,c("commaLocation", "commaLocationMultipleCommas", "secondComma", "username", "lengthURL", "slashLocation")] = NULL


#========================================================
#CONVERT NUMBER OF BACKERS CHARACTERS TO INTEGERS
#========================================================
generalStats$totalNumberBackers = gsub(",", "", generalStats$totalNumberBackers)
generalStats$totalNumberBackers = gsub(" backer", "", generalStats$totalNumberBackers)
generalStats$totalNumberBackers = gsub("s", "", generalStats$totalNumberBackers)
generalStats$totalNumberBackers = as.integer(generalStats$totalNumberBackers)

perkStats$numBackers = gsub(",", "", perkStats$numBackers)
perkStats$numBackers = gsub("Limited ", "", perkStats$numBackers)
perkStats$numBackers = gsub("Reward no longer available ", "", perkStats$numBackers)
perkStats$numBackers = gsub(" backer", "", perkStats$numBackers)
perkStats$numBackers = gsub("s", "", perkStats$numBackers)
perkStats$numBackers = as.integer(perkStats$numBackers)


#========================================================
#CONVERT CURRENCIES
#========================================================
#Exchange rates (13th of April 2017)
aud_exch = 0.7575
cad_exch = 0.7511
dkk_exch = 0.142657
eur_exch = 1.06122
hkd_exch = 0.128638
mxd_exch = 0.053804
nzd_exch = 0.7003
nok_exch = 0.116595
gbp_exch = 1.2506
sd_exch = 0.715052
sek_exch = 0.110624
chf_exch = 0.994036

#Convert from integer to character
generalStats$goal = as.character(generalStats$goal)
generalStats$totalPledge = as.character(generalStats$totalPledge)
perkStats$perkPriceOriginal = as.character(perkStats$perkPriceOriginal)

#Create currency matrix
valutaRaw = c("AU\\$", "CA\\$", "DKK", "â‚¬", "HK\\$", "MX\\$", "NZ\\$", "NOK", "Â£", "S\\$", "SEK", "CHF", "\\$") 
valutaRawSpacing = c("AU\\$ ", "CA\\$ ", "DKK ", "â‚¬", "HK\\$ ", "MX\\$ ", "NZ\\$ ", "NOK ", "Â£", "S\\$ ", "SEK ", "CHF ", "\\$") #to remove additional spaces
valutaShort = c("aud", "cad", "dkk", "eur", "hkd", "mxd", "nzd", "nok", "gbp", "sd", "sek", "chf", "usd")
valutaExchange = c(aud_exch, cad_exch, dkk_exch, eur_exch, hkd_exch, mxd_exch, nzd_exch, nok_exch, gbp_exch, sd_exch, sek_exch, chf_exch, 1)

valutaMatrix = cbind(valutaRaw, valutaRawSpacing, valutaShort, valutaExchange)

for(counter in 1:nrow(valutaMatrix)){
  valutaRaw = valutaMatrix[counter, 1]
  valutaRawSpacing = valutaMatrix[counter, 2]
  valutaShort = valutaMatrix[counter, 3]
  
  #Determine currency 
  generalStats[grepl(valutaRaw, generalStats$goal),"valuta"] = valutaShort
  
  #Remove currency in goal column
  generalStats[grepl(valutaRaw, generalStats$goal),"goal"] = sub(valutaRawSpacing, "", generalStats[grepl(valutaRaw, generalStats$goal),"goal"])
  
  #Remove currency in totalPledge column
  generalStats[grepl(valutaRaw, generalStats$totalPledge),"totalPledge"] = sub(valutaRawSpacing, "", generalStats[grepl(valutaRaw, generalStats$totalPledge),"totalPledge"])
  
  #Remove currency in perkPriceOriginal column
  perkStats[grepl(valutaRaw, perkStats$perkPriceOriginal),"perkPriceOriginal"] = sub(valutaRawSpacing, "", perkStats[grepl(valutaRaw, perkStats$perkPriceOriginal),"perkPriceOriginal"])
}

#Remove commas and transform to double type
generalStats[, "goal"] = gsub(",", "", generalStats[,"goal"])
generalStats[, "totalPledge"] = gsub(",", "", generalStats[,"totalPledge"])
perkStats[,"perkPriceOriginal"] = gsub(",","",perkStats$perkPriceOriginal)
generalStats$goal = as.double(generalStats$goal)
generalStats$totalPledge = as.double(generalStats$totalPledge)
perkStats$perkPriceOriginal = as.double(perkStats$perkPriceOriginal)

#Convert to USD function
convertCurrency = function(valuta, exchangeRate, column) {
  round(generalStats[generalStats$valuta == valuta, column] * exchangeRate, 2)
}

for(counter in 1:nrow(valutaMatrix)){
  valutaShort = valutaMatrix[counter, 3]
  valutaExchange = as.double(valutaMatrix[counter, 4])
  
  #Convert goal to USD
  generalStats$goal[generalStats$valuta == valutaShort] = convertCurrency(valutaShort, valutaExchange, "goal")
  
  #Convert totalPledge to USD
  generalStats$totalPledge[generalStats$valuta == valutaShort] = convertCurrency(valutaShort, valutaExchange, "totalPledge")
}

#========================================================
#   DETERMINE SUCCESS OF CAMPAIGNS
#========================================================
#Determine successfull and unsuccessful campaigns
generalStats$success = ifelse(generalStats$totalPledge >= generalStats$goal, 1, 0)
generalStats$success = factor(generalStats$success)

#Determine percentage of goal reached
generalStats$percentageGoal = generalStats$totalPledge / generalStats$goal * 100


#========================================================
#   DETERMINE CHEAPEST/MOST EXPENSIVE PERK (USD) + CHEAPEST PERK (ORIGINAL CURRENCY)
#========================================================
#Remove $-sign and comma(s)
perkStats[,"perkPriceDollar"] = sub("\\$","",perkStats$perkPriceDollar)
perkStats[,"perkPriceDollar"] = gsub(",","",perkStats$perkPriceDollar)
perkStats$perkPriceDollar = as.numeric(perkStats$perkPriceDollar)

#Determine cheapest perk function
lowestPerk = function(url){
  min(perkStats[perkStats$url == url, "perkPriceDollar"])
}

#Determine cheapest perk (original currency) function
lowestPerkPriceOriginal = function(url){
  min(perkStats[perkStats$url == url,"perkPriceOriginal"])
}

#Determine most expensive perk function
highestPerk = function(url){
  max(perkStats[perkStats$url == url, "perkPriceDollar"])
}

#Determine cheapest/most expensive perk for all campaigns (in USD)
for(url in generalStats$url){
  generalStats[generalStats$url == url, "lowestPerk"] = lowestPerk(url)
  generalStats[generalStats$url == url, "highestPerk"] = highestPerk(url)
  generalStats[generalStats$url == url, "lowestPerkPriceOriginal"] = lowestPerkPriceOriginal(url)
}

#Remove "permanently hidden because of privacy cases" campaigns 
generalStats = generalStats[generalStats$lowestPerk != "Inf",]


#========================================================
#   NUMBER OF PERKS 
#========================================================

#Determine frequency of url function
calculateLength = function(url, dataset, column){
  length(dataset[dataset$url == url, column])
}

#Determine number of perks for each campaign
for(url in generalStats$url){
  generalStats[generalStats$url == url, "numPerks"] = calculateLength(url, perkStats, "perkPriceOriginal")
}


#========================================================
#   NUMBER OF COLLABORATORS 
#========================================================

#Determine number of collaborators for each campaign
for(url in generalStats$urlCreator){
  generalStats[generalStats$urlCreator == url, "numCollaborators"] = calculateLength(url, collaborators,"collaborator")
}


#========================================================
#   NUMBER OF FACEBOOK FRIENDS
#========================================================
#Replace "Not connected" and "Connected to Facebook" by NA
generalStats[generalStats$facebookFriends == "Not connected" | generalStats$facebookFriends == "Connected to Facebook","facebookFriends"] = NA
generalStats$facebookFriends = gsub(" friends", "", generalStats$facebookFriends)
generalStats$facebookFriends = gsub(",", "", generalStats$facebookFriends)

#Replace first and last name by NA
generalStats$facebookFriends = gsub("[[:alpha:]]+", "", generalStats$facebookFriends)
generalStats$facebookFriends = sub("^\\s+", "", generalStats$facebookFriends)
generalStats$facebookFriends = as.integer(generalStats$facebookFriends)


#========================================================
#   PAST SUCCESS RATE
#========================================================
pastSuccessful$url = as.character(pastSuccessful$url)
generalStats$urlProfile = as.character(generalStats$urlProfile)

#Determine number of successful campaigns by creator
library(stringr)
pastSuccessful$numSuccessfulCampaigns = str_count(pastSuccessful$successfulCampaign, "https://")   

for(url in pastSuccessful$url){
  generalStats[generalStats$urlProfile == url, "numSuccessfulCampaigns"] = pastSuccessful[pastSuccessful$url == url, "numSuccessfulCampaigns"]
}
generalStats[is.na(generalStats$numSuccessfulCampaigns), "numSuccessfulCampaigns"] = 0

#Determine number of live campaigns by creator
for(url in liveCampaigns$url){
  generalStats$numLiveCampaigns[generalStats$urlProfile == url] = 1  
}
generalStats[is.na(generalStats$numLiveCampaigns), "numLiveCampaigns"] = 0

#Manually fix anomalies 
generalStats[generalStats$url == "https://www.kickstarter.com/projects/967512676/robots-drones-cybers-28mm-miniatures-sci-fi-alien?ref=ending_soon", "created"] = 6

#Past success rate = (total successful campaigns - current success (1 or 0)) / (total campaigns created - live campaigns - 1)
generalStats[generalStats$created > 1, "pastSuccessRate"] = (as.numeric(generalStats[generalStats$created > 1, "numSuccessfulCampaigns"]) - (as.numeric(generalStats[generalStats$created > 1, "success"])-1)) / (generalStats[generalStats$created > 1, "created"] - generalStats[generalStats$created > 1, "numLiveCampaigns"] - 1)

#Denominator is zero fix
generalStats[(generalStats$created - (as.numeric(generalStats$success)-1) - generalStats$numLiveCampaigns) == 0, "pastSuccessRate"] = 0

#Add hypothetical median past success rate 
generalStats[generalStats$created == 1, "pastSuccessRate"] = median(generalStats[!is.na(generalStats$pastSuccessRate) & generalStats$created > 1,"pastSuccessRate"])
mean(generalStats[!is.na(generalStats$pastSuccessRate),"pastSuccessRate"])


#========================================================
#   TOTAL WORD COUNT
#========================================================
library(stringi)

#Determine number of words in campaign body text
campaignQuality$textP5_w = stri_count(campaignQuality$textP5, regex="\\S+")

#Determine number of words in "Risk and Challenges" text
campaignQuality$textRiskChallenges_w = stri_count(campaignQuality$textRiskChallenges, regex="\\S+")

#Determine total number of words in campaign text
campaignQuality$totWordCount = campaignQuality$textP5_w + campaignQuality$textRiskChallenges_w

for(url in campaignQuality$url){
  generalStats[generalStats$url == url, "totWordCount"] = campaignQuality[campaignQuality$url == url, "totWordCount"]
}


#========================================================
#   MAIN AND ADDITIONAL VIDEO 
#========================================================

#Determine if the campaign includes a main campaign video
campaignQuality$mainVideo = ifelse(campaignQuality$video == "You'll need an HTML5 capable browser to see this content.", 1, 0)

#Determine the number of additional videos on the campaign page
campaignQuality$numAdditionalVideos = ifelse(campaignQuality$video2 != "", (str_count(campaignQuality$video2, ",") + 1), 0)  

for(url in campaignQuality$url){
  #Determine if the campaign includes a main campaign video
  generalStats[generalStats$url == url, "mainVideo"] = campaignQuality[campaignQuality$url == url, "mainVideo"]
  
  #Determine the number of additional videos on the campaign page
  generalStats[generalStats$url == url, "numAdditionalVideos"] = campaignQuality[campaignQuality$url == url, "numAdditionalVideos"]
}

#========================================================
#   NUMBER OF IMAGES 
#========================================================

#Determine the number of images the campaign contains
campaignQuality$numImages = str_count(campaignQuality$image2, ",") + 1  
for(url in campaignQuality$url){
  generalStats[generalStats$url == url, "numImages"] = campaignQuality[campaignQuality$url == url, "numImages"]
}

#========================================================
#   IMAGE GALLERY
#========================================================

#Determine whether the campaign includes a view gallery button
for(url in viewGallery$url){
  generalStats[generalStats$url == url, "viewGallery"] = 1
}

#Replace NA's by a 0 (campaign doesn't contain a gallery)
generalStats$viewGallery = ifelse(is.na(generalStats$viewGallery), 0, generalStats$viewGallery)

#========================================================
#   OUTLIER REMOVAL 
#========================================================
outlierRemoval = function(column, dataframe){
  outliersHypothesis = quantile(dataframe[,column])
  hypothesisLowerLimit = max(outliersHypothesis[2] - 1.5*(outliersHypothesis[4]-outliersHypothesis[2]),0)
  hypothesisUpperLimit = max(outliersHypothesis[4] + 1.5*(outliersHypothesis[4]-outliersHypothesis[2]),0)
  print(hypothesisLowerLimit) #lower limit
  print(hypothesisUpperLimit) #upper limit
  return(dataframe[dataframe[,column] >= hypothesisLowerLimit & dataframe[,column] <= hypothesisUpperLimit, ])
}









#=================================
#============= OLD ===============
#=================================

#import data
generalStats = read.csv('GeneralStats.csv')
perkStats = read.csv('PerkStats.csv')
states = read.csv("Abbreviation_State.csv")
perkStats1 = read.csv('PerkStats1.csv') #31-03-2017
perkStats2 =read.csv('PerkStats2.csv') #07-04-2017
#perkStats3 = read.csv('PerkStats3.csv') #14-04-2017
perkStats4 = read.csv('PerkStats4.csv') #21-04-2017
perkStats5 = read.csv('PerkStats5.csv') #28-04-2017
collaborators = read.csv("Collaborators.csv")
creatorBio = read.csv("CreatorBio.csv")
creatorProfileGeneral = read.csv("CreatorProfileGeneral.csv")
pastSuccessful = read.csv("SuccessfulCampaigns.csv")
campaignQuality = read.csv("CampaignQuality.csv")
viewGallery = read.csv("ViewGallery.csv")

#remove Canceled campaigns - MOGELIJK VERVANGEN DOOR EEN ATTRIBUUT DAT ZEGT OF DE CAMPAGNE GECANCELD IS
#generalStats = generalStats[!grepl("Canceled", generalStats$campaignTitle),]

#========================================================
#LOCATION CAMPAIGN
#========================================================
generalStats$location = as.character(generalStats$location)

#Determine location of comma for "Non-rojects we love"
for(url in generalStats$url){
  generalStats$commaLocation[generalStats$url == url] = gregexpr(pattern =",",generalStats$location[generalStats$url == url])[[1]]
}

#Update location info (due to "Projects we love" column)
missingLocationsURL = generalStats[generalStats$commaLocation == -1,"url"]
location = c("Brooklyn, NY", "Tokyo, Japan", "Phnom Penh, Cambodia","New York, NY", "Jersey City, NJ","Burbank, CA","Brooklyn, NY","Portland, OR", "Seoul, South Korea", "Los Angeles, CA", "Beaufort, SC", "San Antonio, TX","Rotterdam, Netherlands","San Mauro Pascoli, Italy", "Athens, Greece","London, UK","Nottinghamshire, UK","Townsville, AU","Brisbane, AU","Winnipeg, Canada","Ottawa, Canada","Hong Kong, Hong Kong","Tulum, Mexico","Auckland, NZ", "New York, NY", "New York, NY", "Concord, CA", "Portland, OR", "Oakland, CA","Boulder, CO","Rotterdam, Netherlands","Preston, UK", "London, UK", "Brighton and Hove City, UK", "Margaret River, AU", "Sydney, AU", "Ottawa, Canada", "Lafayette, LA", "Eugene, OR") #should be manually updated once more data is added
missingLocationsMatrix = cbind(url=as.character(missingLocationsURL), location=location)

#Determine location of comma for "Projects we love"
for(i in 1:nrow(missingLocationsMatrix)){
  generalStats$location[generalStats$url == missingLocationsMatrix[i,1]] = missingLocationsMatrix[i,2]
  generalStats$commaLocation[generalStats$url == missingLocationsMatrix[i,1]] = gregexpr(pattern =",",generalStats$location[generalStats$url == missingLocationsMatrix[i,1]])[[1]]
}

#Add "Projects we love" as a attribute
for(url in missingLocationsURL){
  generalStats$projectsWeLove[generalStats$url == url] = T  
}
generalStats[is.na(generalStats$projectsWeLove), "projectsWeLove"] = F  


#Determine country / state
for(url in generalStats$url){
  generalStats$countryState[generalStats$url == url] = substring(generalStats[generalStats$url == url, "location"],generalStats[generalStats$url == url, "commaLocation"][[1]]+2)
}

#Determine position of comma for fields with two commas
for(url in generalStats$url){
  generalStats$commaLocationMultipleCommas[generalStats$url == url] = gregexpr(pattern =",",generalStats$countryState[generalStats$url == url])[[1]]
}
missingLocationsMultipleCommas = generalStats[generalStats$commaLocationMultipleCommas != -1,"url"]

#Determine country / state for fields with two commas
for(url in missingLocationsMultipleCommas){
  generalStats$countryState[generalStats$url == url] = substring(generalStats[generalStats$url == url, "countryState"],generalStats[generalStats$url == url, "commaLocationMultipleCommas"][[1]]+2)
}

#Determine city
for(url in generalStats$url){
  generalStats$city[generalStats$url == url] = substring(generalStats[generalStats$url == url, "location"],first=1,last=generalStats[generalStats$url == url, "commaLocation"][[1]]-1)
}

#Determine city for field with two commas 
generalStats$secondComma  = generalStats$commaLocation + generalStats$commaLocationMultipleCommas
for(url in missingLocationsMultipleCommas){
  generalStats$city[generalStats$url == url] = substring(generalStats[generalStats$url == url, "location"],first=generalStats[generalStats$url == url, "commaLocation"][[1]]+2,last=generalStats[generalStats$url == url, "secondComma"][[1]])
}

#Convert abbreviations into full state names
states[,1] = as.character(states[,1])
states[,2] = as.character(states[,2])
states[,3] = NULL
for(i in 1:nrow(states)){
  generalStats$countryState[generalStats$countryState == states[i,1]] = states[i,2]
}  

#remove redundant/temporary comma list and X column
#generalStats$commaLocation = NULL
generalStats$X = NULL 

#========================================================
#CONVERT NUMBER OF BACKERS
#========================================================
generalStats$totalNumberBackers = gsub(",", "", generalStats$totalNumberBackers)
generalStats$totalNumberBackers = gsub(" backer", "", generalStats$totalNumberBackers)
generalStats$totalNumberBackers = gsub("s", "", generalStats$totalNumberBackers)
generalStats$totalNumberBackers = as.integer(generalStats$totalNumberBackers)
generalStats$numComments = as.integer(generalStats$numComments)

perkStats$numBackers = gsub(",", "", perkStats$numBackers)
perkStats$numBackers = gsub("Limited ", "", perkStats$numBackers)
perkStats$numBackers = gsub("Reward no longer available ", "", perkStats$numBackers)
perkStats$numBackers = gsub(" backer", "", perkStats$numBackers)
perkStats$numBackers = gsub("s", "", perkStats$numBackers)
perkStats$numBackers = as.integer(perkStats$numBackers)

#========================================================
#CONVERT CURRENCIES
#========================================================
#Exchange rates (13th of April 2017)
aud_exch = 0.7575
cad_exch = 0.7511
dkk_exch = 0.142657
eur_exch = 1.06122
hkd_exch = 0.128638
mxd_exch = 0.053804
nzd_exch = 0.7003
nok_exch = 0.116595
gbp_exch = 1.2506
sd_exch = 0.715052
sek_exch = 0.110624
chf_exch = 0.994036

generalStats$goal = as.character(generalStats$goal)
generalStats$totalPledge = as.character(generalStats$totalPledge)

#Create valuta matrix
valutaRaw = c("AU\\$", "CA\\$", "DKK", "â‚¬", "HK\\$", "MX\\$", "NZ\\$", "NOK", "Â£", "S\\$", "SEK", "CHF", "\\$") 
valutaRawSpacing = c("AU\\$ ", "CA\\$ ", "DKK ", "â‚¬", "HK\\$ ", "MX\\$ ", "NZ\\$ ", "NOK ", "Â£", "S\\$ ", "SEK ", "CHF ", "\\$") #to remove additional spaces
valutaShort = c("aud", "cad", "dkk", "eur", "hkd", "mxd", "nzd", "nok", "gbp", "sd", "sek", "chf", "usd")
valutaExchange = c(aud_exch, cad_exch, dkk_exch, eur_exch, hkd_exch, mxd_exch, nzd_exch, nok_exch, gbp_exch, sd_exch, sek_exch, chf_exch, 1)

valutaMatrix = cbind(valutaRaw, valutaRawSpacing, valutaShort, valutaExchange)

for(counter in 1:nrow(valutaMatrix)){
  valutaRaw = valutaMatrix[counter,1]
  valutaRawSpacing = valutaMatrix[counter,2]
  valutaShort = valutaMatrix[counter,3]

  generalStats[grepl(valutaRaw, generalStats$goal),"valuta"] = valutaShort
  generalStats[grepl(valutaRaw, generalStats$goal),"goal"] = sub(valutaRawSpacing, "", generalStats[grepl(valutaRaw, generalStats$goal),"goal"])
  generalStats[grepl(valutaRaw, generalStats$totalPledge),"totalPledge"] = sub(valutaRawSpacing, "", generalStats[grepl(valutaRaw, generalStats$totalPledge),"totalPledge"])
  
  perkStats[grepl(valutaRaw, perkStats$perkPriceOriginal),"perkPriceOriginal"] = sub(valutaRawSpacing, "", perkStats[grepl(valutaRaw, perkStats$perkPriceOriginal),"perkPriceOriginal"])
}

#remove commas and transform to numeric
generalStats[,"goal"] = gsub(",", "", generalStats$goal)
generalStats[,"totalPledge"] = gsub(",", "", generalStats$totalPledge)
perkStats[,"perkPriceOriginal"] = gsub(",","", perkStats$perkPriceOriginal)
generalStats$goal = as.double(generalStats$goal)
generalStats$totalPledge = as.double(generalStats$totalPledge)
perkStats$perkPriceOriginal = as.double(perkStats$perkPriceOriginal)

#Convert into usd
convertValutaGoal = function(valuta, exchangeRate) {
  round(generalStats$goal[generalStats$valuta == valuta] * exchangeRate,2)
}

convertValutaTotalPledge = function(valuta, exchangeRate) {
  round(generalStats$totalPledge[generalStats$valuta == valuta] * exchangeRate,2)
}

for(counter in 1:nrow(valutaMatrix)){
  valutaShort = valutaMatrix[counter,3]
  valutaExchange = as.double(valutaMatrix[counter,4])
  
  generalStats$goal[generalStats$valuta == valutaShort] = convertValutaGoal(valutaShort, valutaExchange)
  generalStats$totalPledge[generalStats$valuta == valutaShort] = convertValutaTotalPledge(valutaShort, valutaExchange)
}

#========================================================
#DETERMINE SUCCESSFUL CAMPAIGNS
#========================================================
generalStats$success = ifelse(generalStats$totalPledge >= generalStats$goal, 1, 0)
generalStats$success = factor(generalStats$success)

#========================================================
#LOWEST PERK
#========================================================
perkStats[,"perkPriceDollar"] = sub("\\$","",perkStats$perkPriceDollar)
perkStats[,"perkPriceDollar"] = gsub(",","",perkStats$perkPriceDollar)
perkStats$perkPriceDollar = as.numeric(perkStats$perkPriceDollar)

lowestPerk = function(url){
  min(perkStats[perkStats$url == url,"perkPriceDollar"])
}

for(url in generalStats$url){
  generalStats[generalStats$url == url, "lowestPerk"] = lowestPerk(url)
}

#Remove "permanently hidden because of privacy cases"
generalStats = generalStats[generalStats$lowestPerk != "Inf",]

#Remove lowestPerk = 0 cases
#generalStats = generalStats[generalStats$lowestPerk != 0, ]

#========================================================
# $25 OR LESS PERK AND $100 OR MORE PERK
#========================================================

highestPerk = function(url){
  max(perkStats[perkStats$url == url,"perkPriceDollar"])
}

for(url in generalStats$url){
  generalStats[generalStats$url == url, "highestPerk"] = highestPerk(url)
}

#Hypothesis 2: Kickstarter campaigns which include one or more pledge levels of $25 or less and one or more pledge levels of $100 or more are more likely to succeed (H2)
generalStats$hypothesisTwo = ifelse(generalStats$lowestPerk <= 25 & generalStats$highestPerk >= 100, T, F)

#calculate percentage of goal
generalStats$percentageGoal = generalStats$totalPledge / generalStats$goal * 100

#========================================================
# 1-perk (euro, usd, etc.)
#========================================================
perkStats$perkPriceOriginal = as.character(perkStats$perkPriceOriginal)

for(counter in 1:nrow(valutaMatrix)){
  valutaRaw = valutaMatrix[counter,1]
  valutaRawSpacing = valutaMatrix[counter,2]
  valutaShort = valutaMatrix[counter,3]
  
  perkStats[grepl(valutaRaw, perkStats$perkPriceOriginal),"perkPriceOriginal"] = sub(valutaRawSpacing, "", perkStats[grepl(valutaRaw, perkStats$perkPriceOriginal),"perkPriceOriginal"])
}

perkStats[,"perkPriceOriginal"] = gsub(",","",perkStats$perkPriceOriginal)
perkStats$perkPriceOriginal = as.double(perkStats$perkPriceOriginal)

lowestPerkPriceOriginal = function(url){
  min(perkStats[perkStats$url == url,"perkPriceOriginal"])
}

for(url in generalStats$url){
  generalStats[generalStats$url == url, "lowestPerkPriceOriginal"] = lowestPerkPriceOriginal(url)
}

#Hypothesis 5: it is hypothesised that Kickstarter campaigns which offer a $1 or €1 perk have relatively more backers (H5) 
generalStats$hypothesisFive = ifelse(generalStats$lowestPerkPriceOriginal <= 1, T, F)

#========================================================
# Number of perks
#========================================================

#OOK VOOR NUM COLLABORATORS
calculateLength = function(url, dataset, column){
  length(dataset[dataset$url == url, column])
}

for(url in generalStats$url){
  generalStats[generalStats$url == url, "numPerks"] = calculateLength(url, perkStats, "perkPriceOriginal")
}

#========================================================
# Median of perks
#========================================================

medianPerk = function(url, dataset){
  median(dataset[dataset$url == url,"perkPriceOriginal"])
}

for(url in generalStats$url){
  generalStats[generalStats$url == url, "medianPerk"] = medianPerk(url, perkStats)
}

#========================================================
#outlierRemoval
#========================================================
outlierRemoval = function(column,dataframe){
  outliersHypothesis = quantile(dataframe[,column])
  hypothesisLowerLimit = max(outliersHypothesis[2] - 1.5*(outliersHypothesis[4]-outliersHypothesis[2]),0)
  hypothesisUpperLimit = max(outliersHypothesis[4] + 1.5*(outliersHypothesis[4]-outliersHypothesis[2]),0)
  print(hypothesisLowerLimit)
  print(hypothesisUpperLimit)
  return(dataframe[dataframe[,column] >= hypothesisLowerLimit & dataframe[,column] <= hypothesisUpperLimit, ])
}

#========================================================
#MODERATORS
#========================================================

#Number of projects backed by author, number of comments by author, total number of campaigns created, registration date KS
generalStats = merge(generalStats, creatorProfileGeneral, by.x="urlProfile", by.y="urlCreated") 
generalStats$urlProjects = NULL
generalStats$lengthURL = NULL
generalStats$slashLocation = NULL
generalStats$slashLocation2 = NULL
generalStats$X_cached_page_id = NULL
generalStats$X_template = NULL
generalStats$X_type = NULL

generalStats = merge(generalStats, creatorBio, by.x="url", by.y="url") 
generalStats$X_cached_page_id = NULL
generalStats$X_template = NULL
generalStats$X_type = NULL
generalStats$urlBare = NULL
generalStats$slashLocation = NULL

#FACEBOOK FRIENDS (not related to totalPledge and success)
#facebookFriends = generalStats[generalStats$facebookFriends != "Not connected" & generalStats$facebookFriends != "Connected to Facebook",]
generalStats[,"facebookFriends"] = gsub(" friends", "", generalStats[,"facebookFriends"])
generalStats[,"facebookFriends"] = gsub(",", "", generalStats[,"facebookFriends"])
generalStats[generalStats$facebookFriends == "Not connected" | generalStats$facebookFriends == "Connected to Facebook","facebookFriends"] = ifelse(generalStats[generalStats$facebookFriends == "Not connected" | generalStats$facebookFriends == "Connected to Facebook","facebookFriends"], NA, generalStats[generalStats$facebookFriends,]) 

#Remove field with only the Facebook name (rather than the number of friends)
facebookFriends$facebookFriends = as.numeric(facebookFriends$facebookFriends) 
facebookFriends = facebookFriends[!is.na(facebookFriends$facebookFriends),]
facebookFriends = outlierRemoval("totalPledge", facebookFriends)

ggplot(data=facebookFriends, aes(x=facebookFriends, y=totalPledge)) + geom_point(aes(color=success)) + geom_smooth()
mean(facebookFriends[facebookFriends$success == 1,"facebookFriends"]) #637
mean(facebookFriends[facebookFriends$success == 0,"facebookFriends"]) #628

#CAMPAIGNS CREATED 
campaignsCreated = generalStats
campaignsCreated$success = as.numeric(generalStats$success)

mean(campaignsCreated[campaignsCreated$success == 2,"created"]) #2.32 campaigns
mean(campaignsCreated[campaignsCreated$success == 1,"created"]) #1.28 campaigns
mean(campaignsCreated[campaignsCreated$created > 1,"success"]) #62,27% campaigns
mean(campaignsCreated[campaignsCreated$created == 1,"success"]) #37,3% campaigns

#determine average success rate for number of campaigns created
binsCampaignExperience = factor(c("1", "2-3", "4-5","6-7","8-9","10 or more"), levels = c("1", "2-3", "4-5","6-7","8-9","10 or more"))
successCampaignExperience = data.frame(created=binsCampaignExperience, success=NA)
successCampaignExperience[1,2] = (mean(as.numeric(generalStats[generalStats$created == 1,"success"])) - 1) * 100  
successCampaignExperience[6,2] = (mean(as.numeric(generalStats[generalStats$created >= 10,"success"])) - 1) * 100  

for(i in 1:4){
  successCampaignExperience[i+1,2] = (mean(as.numeric(generalStats[generalStats$created > i & generalStats$created < i + 3,"success"])) - 1) * 100    
}

ggplot(successCampaignExperience, aes(x=created, y=success)) + 
  geom_bar(stat="identity", aes(fill=created)) + ylab("Average success rate (%)") + xlab("Number of campaigns created") + ggtitle("Average success rate related to the number of campaigns created") +
  theme(legend.position = "none")

#CAMPAIGNS BACKED BY CREATOR
campaignsBacked = outlierRemoval("backed", generalStats)

mean(campaignsBacked[campaignsBacked$success == 2,"backed"]) #1.00 campaigns backed
mean(campaignsBacked[campaignsBacked$success == 1,"backed"]) #0.35 campaigns backed
mean(as.numeric(campaignsBacked[campaignsBacked$backed >= 1,"success"])) #59,18% success
mean(as.numeric(campaignsBacked[campaignsBacked$backed == 0,"success"])) #27,04% success

binsCampaignsBacked = factor(c("0","1","2","3","4", "5 or more"))
successCampaignsBacked = data.frame(backed=binsCampaignsBacked, success=NA)

for(i in 0:4){
  successCampaignsBacked[i+1,2] = (mean(as.numeric(generalStats[generalStats$backed == i,"success"])) - 1) * 100    
}
successCampaignsBacked[6,2] = (mean(as.numeric(generalStats[generalStats$backed >= 5,"success"])) - 1) * 100

ggplot(successCampaignsBacked, aes(x=backed, y=success)) + 
  geom_bar(stat="identity", aes(fill=backed)) + ylab("Average success rate (%)") + xlab("Number of campaigns backed") + ggtitle("Average success rate related to the number of campaigns backed by the campaign creator") +
  theme(legend.position = "none")

#NUMBER OF COMMENTS BY CREATOR (data is too limited; 1 means data is not collected)

#COLLABORATORS
collaborators$X_cached_page_id = NULL
collaborators$X_index = NULL
collaborators$X_template = NULL
collaborators$X_type = NULL

numberCollaborators = function(url, dataset, column){
  length(dataset[dataset$url == url, column])
}

for(url in generalStats$urlCreator){
  generalStats[generalStats$urlCreator == url, "numCollaborators"] = calculateLength(url, collaborators,"collaborator")
}

#PAST SUCCESS RATE
#install.packages("stringr")
library(stringr)
pastSuccessful$numSuccessfulCampaigns = str_count(pastSuccessful$successfulURL, ",") + 1  
pastSuccessful$url = as.character(pastSuccessful$url)

for(url in pastSuccessful$url){
  generalStats[generalStats$urlProfile == url, "numSuccessfulCampaigns"] = pastSuccessful[pastSuccessful$url == url, "numSuccessfulCampaigns"]
}

#past success rate = (total successful campaigns - current success) / (total campaigns created - current campaign)
generalStats[generalStats$created > 1 & is.na(generalStats$numSuccessfulCampaigns), "numSuccessfulCampaigns"] = 0
generalStats[generalStats$created > 1, "pastSuccessRate"] = (generalStats[generalStats$created > 1, "numSuccessfulCampaigns"] - (as.numeric(generalStats[generalStats$created > 1, "success"] )-1)) / (generalStats[generalStats$created > 1, "created"]  - 1)
#generalStats$pastSuccessRate = ifelse(generalStats$pastSuccessRate < 0, 0, generalStats$pastSuccessRate) #to account for negative past success rates

#replace past success rate of campaigns that have only created 1 campaign with the mean success rate 
generalStats[is.na(generalStats$pastSuccessRate),"pastSuccessRate"] = mean(generalStats[!is.na(generalStats$pastSuccessRate),"pastSuccessRate"])

#TOTAL WORD COUNT (TITLES, BODY TEXT ETC.)
library(stringi)
campaignQuality$textP5_w = stri_count(campaignQuality$textP5,regex="\\S+")
campaignQuality$textRiskChallenges_w = stri_count(campaignQuality$textRiskChallenges,regex="\\S+")
campaignQuality$totWordCount = campaignQuality$textP5_w + campaignQuality$textRiskChallenges_w

for(url in campaignQuality$url){
  generalStats[generalStats$url == url, "totWordCount"] = campaignQuality[campaignQuality$url == url, "totWordCount"]
}

#MAIN VIDEO (promovideo at the top of a KS-page)
campaignQuality$mainVideo = ifelse(campaignQuality$video == "You'll need an HTML5 capable browser to see this content.", 1, 0)
for(url in campaignQuality$url){
  generalStats[generalStats$url == url, "mainVideo"] = campaignQuality[campaignQuality$url == url, "mainVideo"]
}

#NUMBER OF IMAGES 
campaignQuality$numImages = str_count(campaignQuality$image2, ",") + 1  
for(url in campaignQuality$url){
  generalStats[generalStats$url == url, "numImages"] = campaignQuality[campaignQuality$url == url, "numImages"]
}

#VIEW GALLERY BUTTON
for(url in viewGallery$url){
  generalStats[generalStats$url == url, "viewGallery"] = 1
}
generalStats$viewGallery = ifelse(is.na(generalStats$viewGallery), 0, generalStats$viewGallery)






#========================================================
#ANALYSES AND VISUALIZATION
#========================================================


#goal vs totalPledge
ggplot(data=generalStats, aes(x=goal, y=totalPledge)) + geom_point(aes(colour=success), alpha=.5) + coord_cartesian(ylim=c(0,50000), xlim=c(0,50000)) 

#============================#
#======= HYPOTHESIS 1 =======#
#============================#
generalStatsHypothesisOne = outlierRemoval("lowestPerk", generalStats)

#Average dollar value of lowest perk for unsuccessful and successful projects (outliers removed)
mean(generalStatsHypothesisOne[generalStatsHypothesisOne$success == 0,"lowestPerk"]) # $8.65
mean(generalStatsHypothesisOne[generalStatsHypothesisOne$success == 1,"lowestPerk"]) # $7.08

#Standard deviation dollar value of lowest perk for unsuccessful and successful projects
sd(generalStatsHypothesisOne[generalStatsHypothesisOne$success == 0,"lowestPerk"]) #7.73
sd(generalStatsHypothesisOne[generalStatsHypothesisOne$success == 1,"lowestPerk"]) #6.60

#Median dollar value of lowest perk for unsuccessful and successful projects
median(generalStatsHypothesisOne[generalStatsHypothesisOne$success == 0,"lowestPerk"]) #6
median(generalStatsHypothesisOne[generalStatsHypothesisOne$success == 1,"lowestPerk"]) #5

#Mode dollar value of lowest perk for unsuccessful and successful projects
Mode = function(x) {
  ux = unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

Mode(generalStatsHypothesisOne[generalStatsHypothesisOne$success == 0,"lowestPerk"]) #5
Mode(generalStatsHypothesisOne[generalStatsHypothesisOne$success == 1,"lowestPerk"]) #5

#ANOVA-test
anova_results_hypothesisOne = aov(lowestPerk ~ success, data=generalStatsHypothesisOne)
summary(anova_results_hypothesisOne) #p-value: 1.67e-06 (***)

#lowestPerk vs totalPledge (many datapoints in the top left hand side of the plot)
ggplot(data=generalStatsHypothesisOne, aes(x=lowestPerk, y=totalPledge)) + 
  geom_point(aes(colour=success), alpha=.5, size=2) + 
  coord_cartesian(xlim=c(0,32), ylim=c(0,13040)) + xlab("Cheapest Perk ($)") + ylab("Total pledged ($)") + ggtitle("Total amount pledged ($) related to the cheapest perk ($)") 

#Boxplot cheapest perk for unsuccessful and successful campaigns 
ggplot(generalStatsHypothesisOne, aes(x=factor(success), y=lowestPerk)) + 
  geom_jitter(alpha=0.5, aes(color=factor(success)), size=2) + geom_boxplot(aes(fill=success), alpha=0.1, outlier.color=NA) + 
  coord_cartesian(ylim=c(0,30)) + ggtitle("Dollar value of cheapest perk for unsuccessful (0) and successful campaigns (1)") +
  xlab("Campaign success") + ylab("Cheapest perk ($)") 

#Histogram cheapest perk for unsuccessful and successful campaigns
successfulCampaigns = generalStats[generalStats$success == 1, ]
unsuccessfulCampaigns = generalStats[generalStats$success == 0, ]
balancedCampaigns = successfulCampaigns
balancedCampaigns[972:1942,] = unsuccessfulCampaigns[1:971,] #create equal sample sizes

ggplot(data=balancedCampaigns, aes(x=lowestPerk)) +
  geom_bar(aes(y= 2*(..count..)/sum(..count..), fill=success), binwidth = 1, color="black") + 
  xlab("Cheapest perk ($)") + ylab("Percent of total within category(%)") + 
  ggtitle("Histogram of distribution of cheapest perk levels for unsuccessful (0) and successful (1) campaigns stacked") + 
  coord_cartesian(xlim=c(0,32)) + scale_y_continuous(labels=percent) 

#============================#
#======= HYPOTHESIS 2 =======#
#============================#
generalStatsHypothesisTwo = outlierRemoval("percentageGoal", generalStats)

#Boxplot percentage funded of goal for campaigns with and without availability of one or more <=$25 AND >=$100 perks 
ggplot(generalStatsHypothesisTwo, aes(x=hypothesisTwo, y=percentageGoal)) + 
  geom_jitter(alpha=0.5, aes(color=hypothesisTwo), size=2) + geom_boxplot(aes(fill=hypothesisTwo), alpha=0.1, outlier.color=NA) + 
  coord_cartesian(ylim=c(0,250)) + ggtitle("Percentage funded for campaigns with (TRUE) and without (FALSE) a <=$25 perk AND >=$100 perk") +
  xlab("Availability of a <=$25 and >=$100 perk") + ylab("Percentage of goal funded (%)") 
  # add for visualisation for slides
  #+
  # theme(text = element_text(family="Lato"),
  #       axis.text.x = element_text(size=14),
  #       axis.text.y = element_text(size=14),
  #       axis.title.x = element_text(size=22),
  #       axis.title.y = element_text(size=22),
  #       legend.title = element_text(size=20),
  #       legend.text = element_text(size=16),
  #       plot.title = element_text(size=27, face="bold"))


#Average percentage funded for campaigns with and without a <= $25 and >= $100 perk
mean(generalStatsHypothesisTwo[generalStatsHypothesisTwo$hypothesisTwo == T,"percentageGoal"]) # 66.51%
mean(generalStatsHypothesisTwo[generalStatsHypothesisTwo$hypothesisTwo == F,"percentageGoal"]) # 37.26%
mean(generalStatsHypothesisTwo[,"percentageGoal"]) # 55.98%

#Median percentage funded for campaigns with and without a <= $25 and >= $100 perk
median(generalStatsHypothesisTwo[generalStatsHypothesisTwo$hypothesisTwo == T,"percentageGoal"]) # 41.30%
median(generalStatsHypothesisTwo[generalStatsHypothesisTwo$hypothesisTwo == F,"percentageGoal"]) # 2.22%

#Chi-square test
hypothesisTwoTable = table("Hypothesis"=balancedCampaigns$hypothesisTwo, "Successful campaign"=balancedCampaigns$success)
barplot(t(hypothesisTwoTable), beside=T, legend=c("Unsuccessful campaign", "Successful campaign"), xlab="Availability of a <=$25 and >=$100 perk", ylab="Number of campaigns", main="Success rate for campaigns with (TRUE) and without (FALSE) a <=$25 perk AND >=$100 perk") #side by side frequencies
chisq.test(hypothesisTwoTable, correct=T) #p-value: 0.0009483

#============================#
#======= HYPOTHESIS 3 =======#
#============================#
generalStatsHypothesisThree = outlierRemoval("numPerks", generalStats)

#Average number of perks for unsuccessful and successful campaigns 
mean(generalStatsHypothesisThree[generalStatsHypothesisThree$success == 2,"numPerks"]) # 8.42 perks
mean(generalStatsHypothesisThree[generalStatsHypothesisThree$success == 1,"numPerks"]) # 5.55 perks

#Median number of perks for unsuccessful and successful campaigns 
median(generalStatsHypothesisThree[generalStatsHypothesisThree$success == 2,"numPerks"]) # 8 perks
median(generalStatsHypothesisThree[generalStatsHypothesisThree$success == 1,"numPerks"]) # 5 perks

#Median number of perks for unsuccessful and successful campaigns
Mode(generalStatsHypothesisThree[generalStatsHypothesisThree$success == 2,"numPerks"]) #6
Mode(generalStatsHypothesisThree[generalStatsHypothesisThree$success == 1,"numPerks"]) #1

#Most frequent
categoryFilter = generalStatsHypothesisThree$category %in% c("Product Design","Tabletop Games","Music","Children's Books","Art")

#Number of perks for unsuccessful and successful campaigns
ggplot(generalStatsHypothesisThree[categoryFilter,], aes(x=success, y=numPerks)) + 
  geom_jitter(alpha=0.5, aes(color=success), size=2) + geom_boxplot(aes(fill=success), alpha=0.1, outlier.color=NA) + facet_grid(.~category) + 
  coord_cartesian(ylim=c(0,20)) + ggtitle("Number of perks for unsuccessful (0) and successful campaigns (1) for top-5 categories") +
  xlab("Campaign success") + ylab("Number of perks") 

#ANOVA-test
anova_results_hypothesisThree = aov(numPerks ~ success, data=generalStatsHypothesisThree)
summary(anova_results_hypothesisThree) #p-value: <2e-16 (***)

#============================#
#======= HYPOTHESIS 5 =======#
#============================#
generalStatsHypothesisFive= outlierRemoval("percentageGoal", generalStats)

ggplot(generalStatsHypothesisFive, aes(x=hypothesisFive, y=percentageGoal)) + 
  geom_jitter(alpha=0.5, aes(color=hypothesisFive), size=2) + geom_boxplot(aes(fill=hypothesisFive), alpha=0.1, outlier.color=NA) + 
  coord_cartesian(ylim=c(0,200)) + ggtitle("Percentage funded for campaigns with (TRUE) and without (FALSE) a 1-value perk") +
  xlab("Availability of a 1-value perk") + ylab("Percentage of goal funded (%)") 

#Average percentage funded for campaigns with and without a 1-value perk
mean(generalStatsHypothesisFive[generalStatsHypothesisFive$hypothesisFive == T, "percentageGoal"]) # 72.16%
mean(generalStatsHypothesisFive[generalStatsHypothesisFive$hypothesisFive == F, "percentageGoal"]) # 51.66%

#Median percentage funded for campaigns with and without a 1-value perk
median(generalStatsHypothesisFive[generalStatsHypothesisFive$hypothesisFive == T,"percentageGoal"]) # 47.6%
median(generalStatsHypothesisFive[generalStatsHypothesisFive$hypothesisFive == F,"percentageGoal"]) # 12.5%

hypothesisFiveTable = table("1-perk available"=balancedCampaigns$hypothesisFive, "Successful campaign"=balancedCampaigns$success)
barplot(t(hypothesisFiveTable), beside=T, legend=c("Unsuccessful campaign", "Successful campaign"), xlab="Availability of a 1-value perk", ylab="Number of campaigns", main="Success rate for campaigns with and without 1-value perk") #side by side frequencies
chisq.test(hypothesisFiveTable, correct=T) #p-value: <2.2e-16

#============================#
#======= HYPOTHESIS 6 =======#
#============================#
generalStatsHypothesisSix= outlierRemoval("totalNumberBackers", generalStats)

ggplot(generalStatsHypothesisSix, aes(x=hypothesisFive, y=totalNumberBackers)) + 
  geom_jitter(alpha=0.5, aes(color=hypothesisFive), size=2) + geom_boxplot(aes(fill=hypothesisFive), alpha=0.1, outlier.color=NA) + 
  coord_cartesian(ylim=c(0,175)) + ggtitle("Number of backers for campaigns with (TRUE) and without (FALSE) a 1-value perk") +
  xlab("Availability of a 1-value perk") + ylab("Total number of backers")  

#Average total number of backers for campaigns with and without a 1-value perk
mean(generalStatsHypothesisSix[generalStatsHypothesisSix$hypothesisFive == T, "totalNumberBackers"]) # 36
mean(generalStatsHypothesisSix[generalStatsHypothesisSix$hypothesisFive == F, "totalNumberBackers"]) # 25

#Median total number of backers for campaigns with and without a 1-value perk
median(generalStats[generalStats$hypothesisFive == T,"totalNumberBackers"]) # 30
median(generalStats[generalStats$hypothesisFive == F,"totalNumberBackers"]) # 11

anova_results_hypothesis_six = aov(hypothesisFive ~ totalNumberBackers, data=generalStatsHypothesisSix)
summary(anova_results_hypothesis_six) #p-value: <1.21e-06 (***)

#============================#
#======= HYPOTHESIS 7 =======#
#============================#
numberPerksGeneric = function(url, dataset){
  print(length(dataset[dataset$url== url,"perkPriceOriginal"]))
  length(dataset[dataset$url== url,"perkPriceOriginal"])
}

#Number of perks at 31-03-2017 (1)
perkStats1$url = as.character(perkStats1$url)
for(url in generalStats$url){
  generalStats[generalStats$url == url, "numPerks1"] = numberPerksGeneric(url, perkStats1)
}

#number of added perks since 31-03-2017
generalStats$addedPerks = generalStats$numPerks - generalStats$numPerks1

#Number of perks at 21-04-2017 (4)
perkStats4$url = as.character(perkStats4$url)
for(url in generalStats$url){
  generalStats[generalStats$url == url, "numPerks4"] = numberPerksGeneric(url, perkStats4)
}

#Number of perks at 28-04-2017 (4)
perkStats5$url = as.character(perkStats5$url)
for(url in generalStats$url){
  generalStats[generalStats$url == url, "numPerks5"] = numberPerksGeneric(url, perkStats5)
}


generalStatsStartToEnd = generalStats[generalStats$numPerks4 > 0,]
generalStatsStartToEndMore = generalStatsStartToEnd[generalStatsStartToEnd$numPerks4 > generalStatsStartToEnd$numPerks1,]
generalStatsStartToEnd$success = as.numeric(generalStatsStartToEnd$success)
generalStatsStartToEndMore$success = as.numeric(generalStatsStartToEndMore$success)
mean(generalStatsStartToEndMore[!is.na(generalStatsStartToEndMore$success),"success"]) 
mean(generalStatsStartToEnd[!is.na(generalStatsStartToEnd$success),"success"]) #65.8%


#============================#
#======= HYPOTHESIS 8 =======#
#============================#
for(url in perkStats$url){
  perkStats$perkPriceOriginalLastDigit[perkStats$url == url] = substring(perkStats[perkStats$url == url, "perkPriceOriginal"],nchar(perkStats[perkStats$url == url, "perkPriceOriginal"]))
}

perkStatsHypothesisEight = outlierRemoval("numBackers", perkStats)

#Distribution of last digit of perk price
ggplot(data=perkStats, aes(x=perkPriceOriginalLastDigit)) +
  geom_bar(aes(y= (..count..)/sum(..count..), fill=perkPriceOriginalLastDigit), color="black") + 
  xlab("Last digit of perk price (original currency)") + ylab("Percent of all perks(%)") + 
  ggtitle("Histogram of distribution of last digit of perk price") + 
  scale_y_continuous(labels=percent) + theme(legend.position="none")

#Create matrix 
lastDigit = c(0:9)
numBackers = data.frame(lastDigit, averageNumberOfBackers=NA)
numBackers$lastDigit = factor(numBackers$lastDigit)

#mean number of backers for perk price that ends with 1:9
for(i in 0:9){
  numBackers[i+1,"averageNumberOfBackers"] = round(mean(perkStatsHypothesisEight[perkStatsHypothesisEight$perkPriceOriginalLastDigit == i & perkStatsHypothesisEight$perkPriceDollar<50, "numBackers"]),2)
  numBackers[i+1,"medianNumberOfBackers"] = median(perkStatsHypothesisEight[perkStatsHypothesisEight$perkPriceOriginalLastDigit == i& perkStatsHypothesisEight$perkPriceDollar<50, "numBackers"])
}

#Distribution of last digit of perk price
ggplot(data=numBackers, aes(x=lastDigit, y=averageNumberOfBackers)) +
  geom_bar(stat="identity", aes(fill=lastDigit), color="black") + theme(legend.position="none") +
  xlab("Last digit of perk price (original currency)") + ylab("Average number of backers for perk") + 
  ggtitle("Mean number of backers for perks with varying last digits (dollar value <50)") 

ggplot(data=perkStatsHypothesisEight, aes(x=perkPriceDollar, y=numBackers)) + 
  geom_point(aes(colour=perkPriceOriginalLastDigit), alpha=.5) + geom_smooth(method="loess") + 
  coord_cartesian(xlim=c(0,500), ylim=c(0,15)) + facet_grid(.~perkPriceOriginalLastDigit) +
  xlab("Perk price in USD ($)") + ylab("Number of backers") + ggtitle("Number of backers for 0-500$ perks categorised by the last digit of the original perk price") + 
  theme(text = element_text(family="Lato"),
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=12),
        axis.title.x = element_text(size=22),
        axis.title.y = element_text(size=22),
        legend.title = element_text(size=20),
        legend.text = element_text(size=16),
        plot.title = element_text(size=27, face="bold"))

#ANOVA-test
anova_results_hypothesis_eight = aov(perkPriceOriginalLastDigit ~ numBackers, data=perkStatsHypothesisEight)
summary(anova_results_hypothesis_eight) #p-value: <2e-16 (***)

#Number of backers for 0-100$ perks combined in one graph
ggplot(data=perkStatsHypothesisEight, aes(x=perkPriceDollar, y=numBackers)) + 
  geom_point(aes(colour=perkPriceOriginalLastDigit), alpha=0) +
  geom_smooth(data=perkStatsHypothesisEight[perkStatsHypothesisEight$perkPriceOriginalLastDigit == 0,], aes(colour=perkPriceOriginalLastDigit), fill=NA, method="loess") +
  #geom_smooth(data=perkStatsHypothesisEight[perkStatsHypothesisEight$perkPriceOriginalLastDigit == 1,], aes(colour=perkPriceOriginalLastDigit), fill=NA, method="loess") +
  geom_smooth(data=perkStatsHypothesisEight[perkStatsHypothesisEight$perkPriceOriginalLastDigit == 2,], aes(colour=perkPriceOriginalLastDigit), fill=NA) +
  geom_smooth(data=perkStatsHypothesisEight[perkStatsHypothesisEight$perkPriceOriginalLastDigit == 3,], aes(colour=perkPriceOriginalLastDigit), fill=NA) +
  geom_smooth(data=perkStatsHypothesisEight[perkStatsHypothesisEight$perkPriceOriginalLastDigit == 4,], aes(colour=perkPriceOriginalLastDigit), fill=NA) +
  geom_smooth(data=perkStatsHypothesisEight[perkStatsHypothesisEight$perkPriceOriginalLastDigit == 5,], aes(colour=perkPriceOriginalLastDigit), fill=NA, method="loess") +
  geom_smooth(data=perkStatsHypothesisEight[perkStatsHypothesisEight$perkPriceOriginalLastDigit == 6,], aes(colour=perkPriceOriginalLastDigit), fill=NA) +
  geom_smooth(data=perkStatsHypothesisEight[perkStatsHypothesisEight$perkPriceOriginalLastDigit == 7,], aes(colour=perkPriceOriginalLastDigit), fill=NA) +
  geom_smooth(data=perkStatsHypothesisEight[perkStatsHypothesisEight$perkPriceOriginalLastDigit == 8,], aes(colour=perkPriceOriginalLastDigit), fill=NA) +
  geom_smooth(data=perkStatsHypothesisEight[perkStatsHypothesisEight$perkPriceOriginalLastDigit == 9,], aes(colour=perkPriceOriginalLastDigit), fill=NA) +
  coord_cartesian(xlim=c(0,100), ylim=c(0,5)) +
  xlab("Perk price in USD ($)") + ylab("Number of backers") + ggtitle("Number of backers for 0-100$ perks categorised by the last digit of the original perk price") 

#Export to CSV for Tableau integration
write.csv(generalStats,"~/Desktop/generalStats2.csv")

