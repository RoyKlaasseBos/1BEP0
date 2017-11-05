#DATA PREPROCESSING
#Thesis: Perk Pricing Strategies for Reward Based Crowdfunding
#Author: R.J. Klaasse Bos 
#Educational Institution: Eindhoven University of Technology
#Email: r.j.klaasse.bos@student.tue.nl

#========================================================
#   IMPORT DATA 
#========================================================
generalStats = read.csv("GeneralStats.csv")
perkStats = read.csv("PerkStats.csv")
perkStats1 = read.csv("PerkStats1.csv")
states = read.csv("Abbreviation_State.csv")
collaborators = read.csv("Collaborators.csv")
creatorBio = read.csv("CreatorBio.csv")
creatorProfileGeneral = read.csv("CreatorProfileGeneral.csv")
pastSuccessful = read.csv("SuccessfulCampaigns.csv")
liveCampaigns = read.csv("LiveCampaigns.csv")
campaignQuality = read.csv("CampaignQuality.csv")
viewGallery = read.csv("ViewGallery.csv")
continent = read.csv("Continents.csv")
campaignText = read.csv("CampaignText.csv")
mainCategories = read.csv("MainCategories.csv")
optimalGoal = read.csv("OptimalGoal.csv")

#========================================================
#   MERGE DATA
#========================================================
#Number of projects backed by author, number of comments by author, total number of campaigns created, registration date KS
generalStats = merge(generalStats, creatorProfileGeneral, by.x="urlProfile", by.y="urlCreated") 
generalStats[,c("urlProjects","lengthURL","slashLocation","slashLocation2","X_cached_page_id","X_template","X_type", "username")] = NULL
generalStats$comments = as.numeric(generalStats$comments)

#Number of Facebook friends, websites
generalStats = merge(generalStats, creatorBio, by.x="url", by.y="url") 
generalStats[,c("X_cached_page_id", "generalStats$X_template", "slashLocation", "createdBacked","generalStats$X_type", "X_type", "X_template", "urlBare", "generalStats$urlBare", "generalStats$slashLocation", "length")] = NULL

#Main category
generalStats = merge(generalStats, mainCategories, by.x="url", by.y="url") 


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
location = c("London, UK",	"Winnipeg, Canada",	"Seoul, South Korea",	"New York, NY",	"Eugene, OR",	"Guanajuato, Mexico",	"New York, NY",	"Sydney, AU",	"Athens, Greece",	"Beaufort, SC",	"Santa Cruz, CA",	"San Antonio, TX",	"Brooklyn, NY",	"Rotterdam, Netherlands",	"Ottawa, Canada",	"Lafayette, LA",	"Milan, Italy",	"Estado de Mexcio, Mexico",	"Tokyo, Japan",	"Rotterdam, Netherlands",	"London, UK",	"New York, NY",	"London, UK",	"San Mauro Pascoli, Italy", "Concord, CA",	"Preston, UK",	"Burbank, CA",	"Portland, OR",	"Los Angeles, CA",	"Wilmington, DE",	"Ottawa, Canada",	"Vancouver, Canada",	"Brooklyn, NY",	"Boulder, CO",	"Tulum, Mexico",	"Nottinghamshire, UK",	"Auckland, NZ",	"Portland, OR",	"Brighton and Hove City, UK",	"Brisbane, AU",	"Phnom Penh, Cambodia",	"Manchester, UK",	"Margaret River, AU",	"Oakland, CA",	"Palo Alto, CA",	"Jersey City, NJ",	"Townsville, AU")
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
states[,3] = as.character(states[,3])
for(i in 1:nrow(states)){
  generalStats$countryState[generalStats$countryState == states[i,1]] = states[i,2]
  generalStats$continent[generalStats$countryState == states[i,2]] = states[i,3]
}  

#Determine remaining continents
continent[,1] = as.character(continent[,1])
for(i in 1:nrow(continent)){
  generalStats$continent[generalStats$countryState == continent[i,2]] = continent[i,1]
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

#Determine limited perks
perkStats[grepl("Limited", perkStats$numBackers), "limited"] = TRUE
perkStats[grepl("Reward no longer available" , perkStats$numBackers), "limited"] = TRUE
perkStats[is.na(perkStats$limited), "limited"] = FALSE

perkStats$numBackers = gsub(",", "", perkStats$numBackers)
perkStats$numBackers = gsub("Limited ", "", perkStats$numBackers)
perkStats$numBackers = gsub("Reward no longer available ", "", perkStats$numBackers)
perkStats$numBackers = gsub(" backer", "", perkStats$numBackers)
perkStats$numBackers = gsub("s", "", perkStats$numBackers)
perkStats$numBackers = as.integer(perkStats$numBackers)


#========================================================
#   CATEGORIES
#========================================================
generalStats$category = gsub("Project We Love", "", generalStats$category)
generalStats$category = gsub(",", "", generalStats$category)
generalStats[generalStats$category == "Learn more about accountability.","url"]

generalStats$category = sub("^\\s+", "", generalStats$category)
#Learn more about accountability..

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

#Determine median price perk function
medianPerk = function(url){
  median(perkStats[perkStats$url == url, "perkPriceDollar"])
}

#Determine cheapest/most expensive perk for all campaigns (in USD)
for(url in generalStats$url){
  generalStats[generalStats$url == url, "lowestPerk"] = lowestPerk(url)
  generalStats[generalStats$url == url, "highestPerk"] = highestPerk(url)
  generalStats[generalStats$url == url, "medianPerk"] = medianPerk(url)
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

#Replace NA by median number of Facebook friends
generalStats[is.na(generalStats$facebookFriends), "noFacebookFriends"]  = TRUE
generalStats[is.na(generalStats$noFacebookFriends), "noFacebookFriends"]  = FALSE
generalStats[is.na(generalStats$facebookFriends), "facebookFriends"] = median(generalStats[!is.na(generalStats$facebookFriends), "facebookFriends"])

#========================================================
#   NUMBER OF EXTERNAL WEBSITES ON BIO PROFILE
#========================================================
#Determine number of external websites mentioned on the creator page
generalStats$numWebsites = str_count(generalStats$websites, "http")   

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

#Fix negative success rate
generalStats[generalStats$pastSuccessRate < 0, "numSuccessfulCampaigns"] = 1

#Past success rate = (total successful campaigns - current success (1 or 0)) / (total campaigns created - live campaigns - 1)
generalStats[generalStats$created > 1, "pastSuccessRate"] = (as.numeric(generalStats[generalStats$created > 1, "numSuccessfulCampaigns"]) - (as.numeric(generalStats[generalStats$created > 1, "success"])-1)) / (generalStats[generalStats$created > 1, "created"] - generalStats[generalStats$created > 1, "numLiveCampaigns"] - 1)

#Denominator is zero fix
generalStats[(generalStats$created - (as.numeric(generalStats$success)-1) - generalStats$numLiveCampaigns) == 0, "pastSuccessRate"] = 0
generalStats[is.na(generalStats$pastSuccessRate), "pastSuccessRate"] = 0 

#Past success rate > 1 fix
generalStats[generalStats$pastSuccessRate > 1, "pastSuccessRate"] = 1

#Add hypothetical median past success rate 
generalStats[generalStats$created == 1, "pastSuccessRate"] = median(generalStats[!is.na(generalStats$pastSuccessRate) & generalStats$created > 1,"pastSuccessRate"])


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

#Manually fix missing data
generalStats[generalStats$url == "https://www.kickstarter.com/projects/860417349/its-about-time-for-new-body-electric-shows?ref=ending_soon", "totWordCount"] = 986



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
campaignQuality$numImages = ifelse(str_count(campaignQuality$image2, ",") >= 1, str_count(campaignQuality$image2, ",") + 1, 0)  
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
#   ADDED PERKS BETWEEN THE 31TH OF MARCH 2017 AND DEADLINE
#========================================================

#Number of perks at 31-03-2017 (1)
perkStats1$url = as.character(perkStats1$url)
for(url in generalStats$url){
  generalStats[generalStats$url == url, "numPerks1"] = calculateLength(url, perkStats1, "perkPriceOriginal")
}

#number of added perks since 31-03-2017
generalStats$addedPerks = generalStats$numPerks - generalStats$numPerks1
generalStats$numPerks1 = NULL


#========================================================
#   CAMPAIGN DURATION 
#========================================================

#Determine the duration of the campaign in days
for(url in campaignQuality$url){
  generalStats[generalStats$url == url, "duration"] = campaignQuality[as.character(campaignQuality$url) == url, "duration"]
}


#========================================================
#   ESTIMATED DELIVERY 
#========================================================
for(url in campaignQuality$url){
  generalStats[generalStats$url == url, "endDate"] = campaignQuality[campaignQuality$url == url, "endDate"]
  generalStats[generalStats$url == url, "startDate"] = campaignQuality[campaignQuality$url == url, "startDate"]
}

#Determine frequency of url function
medianData = function(url, dataset, column){
  median(dataset[dataset$url == url, column])
}

#Determine average of url function
meanData = function(url, dataset, column){
  mean(dataset[dataset$url == url, column])
}

#Determine number of perks for each campaign
generalStats$endDate = as.POSIXct(generalStats$endDate, format="%d/%m/%Y %H:%M")
generalStats$startDate = as.POSIXct(generalStats$startDate, format="%d/%m/%Y %H:%M")
perkStats$estimatedDelivery = as.POSIXct(perkStats$estimatedDelivery, format="%d/%m/%Y %H:%M")
for(url in generalStats$url){
  generalStats[generalStats$url == url, "estimatedDelivery"] = meanData(url, perkStats, "estimatedDelivery") - generalStats[generalStats$url == url,"endDate"]
}

#Replace negative values by 0 
generalStats[generalStats$estimatedDelivery < 0, "estimatedDelivery"] = 0

#Remove incomplete records
generalStats = generalStats[!is.na(generalStats$estimatedDelivery),]

#========================================================
#   CREATOR EXPERIENCE IN DAYS
#========================================================
#Determine time between registration and launch campaign (creator experience)
generalStats$joinKS = as.POSIXct(generalStats$joinKS, format="%d/%m/%Y %H:%M")
generalStats$experience = (generalStats$startDate - generalStats$joinKS)/60/24 #experience in days
generalStats$experience = gsub(" mins", "",generalStats$experience)
generalStats$experience = as.numeric(generalStats$experience)

#Manual fixes
generalStats = generalStats[complete.cases(generalStats$experience),]

#========================================================
#   CANCELLATION
#========================================================
Determine whether the campaign has been canceled
generalStats[grepl("(Canceled)", generalStats$campaignTitle),"canceled"] = TRUE
generalStats[is.na(generalStats$canceled),"canceled"] = FALSE


#========================================================
#   NUMBER OF COMMENTS
#========================================================
generalStats$numComments = as.character(generalStats$numComments)
generalStats$numComments = gsub(",", "", generalStats$numComments)
generalStats$numComments = as.integer(generalStats$numComments)


#========================================================
#   AVERAGE NUMBER OF BACKERS REQUIRED
#========================================================
for(url in generalStats$url){
  generalStats[generalStats$url == url, "averageBackersRequired"] = generalStats[generalStats$url == url, "goal"] / medianData(url, perkStats, "perkPriceDollar")
}

#========================================================
#   OVERPAYING / PLEDGERS WITHOUT REWARD
#========================================================
#Determine the aggregated perk value (= the minimum required total pledge)
perkStats$aggregatedPerkValue = perkStats$perkPriceDollar * perkStats$numBackers

overPaying = function(url){
  sum(perkStats[perkStats$url == url, "aggregatedPerkValue"])
}

for(url in generalStats$url){
  generalStats[generalStats$url == url, "aggregatedPerkValue"] = overPaying(url)
}

#Determine the difference between the actual total pledge and the minimum required total pledge 
generalStats$overPaying = generalStats$totalPledge - generalStats$aggregatedPerkValue

#========================================================
#   SHIPPING
#========================================================
#Determine whether perk is shipped worldwide
#It is assumed that perks without an explicit indication of shipping location will ship worldwide
perkStats[grepl(",", perkStats$shipping),"shipping"] = "Anywhere in the world"
perkStats[grepl("-", perkStats$shipping),"shipping"] = "Anywhere in the world" #for dates
perkStats[is.na(perkStats$shipping),"shipping"] = "Anywhere in the world"

#Determine whether perk is shipped worldwide
perkStats[!is.na(perkStats$shipping) & perkStats$shipping %in% unique(perkStats$shipping)[-1], "shippingWorld"] = as.numeric(0)
perkStats[!is.na(perkStats$shipping) & perkStats$shipping == "Anywhere in the world", "shippingWorld"] = as.numeric(1)

#Calculate mean number of perks shipped worldwide (relative to the total number of perks)
shippingWorld = function(url){
  mean(perkStats[perkStats$url == url & !is.na(perkStats$shippingWorld), "shippingWorld"])
}

for(url in generalStats$url){
  generalStats[generalStats$url == url, "shippingWorld"] = shippingWorld(url)
}

generalStats[is.na(generalStats$shippingWorld),"shippingWorld"] = mean(generalStats[!is.na(generalStats$shippingWorld),"shippingWorld"])

#========================================================
#   NUMBER OF COMPETITORS
#========================================================
uniqueCategories = unique(generalStats$category)
categoryPerkPrice = cbind(categoryName = uniqueCategories, meanPerkPrice = NA)
for(i in 1:length(uniqueCategories)){
  categoryPerkPrice[i,2] = mean(generalStats[generalStats$category == uniqueCategories[i], "medianPerk"])
}
categoryPerkPrice = as.data.frame(categoryPerkPrice)

#within sub category (within main category smaller effect)
for(category in uniqueCategories){
  generalStats$numCompetitors[generalStats$category == category] = length(generalStats[generalStats$category == category, "medianPerk"]) - 1
}

#========================================================
#   AVERAGE MONEY PER REWARD LEVEL
#========================================================
generalStats$averageMoneyReward = generalStats$goal / generalStats$numPerks

#========================================================
#   EXPORT
#========================================================
write.csv(generalStats,"~/Desktop/1BEP0/Kickstarter_preprocessed.csv")

#write.csv(generalStats[generalStats$valuta %in% c("usd","gbp", "aud", "cad"),c("campaignText","url","success")],"~/Desktop/1BEP0/Kickstarter_Natural_Language_Processing.csv")







#OLD

#========================================================
#   WORD COUNT PERK DESCRIPTION - INSIGNIFICANT
#========================================================
#Determine number of words in perk description body text
perkStats$descriptionWordLength = stri_count(perkStats$description, regex="\\S+")
median(perkStats$descriptionWordLength)
mean(perkStats[perkStats$descriptionWordLength <= 26, "numBackers"])
mean(perkStats[perkStats$descriptionWordLength > 26, "numBackers"])

#========================================================
#   DEVIATION PERK PRICE - INSIGNIFICANT
#========================================================
#Determine difference between optimal in-category goal and campaign goal


uniqueCategories = unique(generalStats$category)
categoryPerkPrice = cbind(categoryName = uniqueCategories, meanPerkPrice = NA)
for(i in 1:length(uniqueCategories)){
  categoryPerkPrice[i,2] = mean(generalStats[generalStats$category == uniqueCategories[i], "medianPerk"])
}
categoryPerkPrice = as.data.frame(categoryPerkPrice)

#within sub category (within main category smaller effect)
for(category in uniqueCategories){
  generalStats$numCompetitors[generalStats$category == category] = length(generalStats[generalStats$category == category, "medianPerk"]) - 1
}

#ANOVA-test
anova_results_hypothesisThree = aov(numCompetitors ~ success, data=generalStats)
summary(anova_results_hypothesisThree) #p-value: <2e-16 (***)


#========================================================
# 1-perk (euro, usd, etc.) - INSIGNIFICANT
#========================================================
lowestPerkPriceOriginal = function(url){
  min(perkStats[perkStats$url == url,"perkPriceOriginal"])
}

for(url in generalStats$url){
  generalStats[generalStats$url == url, "lowestPerkPriceOriginal"] = lowestPerkPriceOriginal(url)
}


generalStats$oneValuePerk = ifelse(generalStats$lowestPerkPriceOriginal <= 1, T, F)

#Determine whether both a <= 25-value and >= 100-value perk is present
generalStats$pricingVariation = ifelse(generalStats$lowestPerk <= 25 & generalStats$highestPerk >= 100, T, F)

generalStats$moneyPerPerk = generalStats$goal / generalStats$numPerks




