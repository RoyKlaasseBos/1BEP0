# Bachelor End Project - Eindhoven University of Technology

## Abstract
Reward based crowdfunding platforms such as Kickstarter allow founders to fund their project by selling perks to a relative large customer base. Drawing on a scraped dataset of 3652 campaigns with combined funding over $51M, this Bachelor End Project (BEP) discusses several underlying dynamics of successful crowdfunding. Empirical analyses show that the mean estimated delivery time of perks, creator’s past crowdfunding success rate, the creator’s adoption speed, the number of previous projects by the creator and the number of Facebook friends affect this success rate. Using these and other directly scrapeable attributes from Kickstarter, a binary classification model has been built to predict the success or failure of campaigns. Random Forests perform the best achieving an accuracy of 78.8% at campaign launch (20 percentage points above the baseline model). The practical implications of these findings for both Kickstarter and creators are discussed.

## Contents
This repository contains the following files: 
* Infographic (pdf)
* Final presentation slides (pdf)
* Bachelor Thesis (pdf)
* Data preprocessing and visualisation code (R) - see also [this](http://royklnl84.eightyfour.axc.nl/kickstarter.html) page.

## Infographic
<img src="https://preview.ibb.co/iWaJiG/2017_06_12_Infographic_BEP.jpg" />

## FAQ
* **Where can I find the feature definitions?**

For most features: Appendix III of my Bachelor Thesis. A few exceptions to this rule will be addressed below.

* **Sometimes a number for "FacebookFriends" shows up in a row even if "noFacebookFriends" = `TRUE` (meaning that they didn't connect their profiles and we would not know the number of FB friends)**

Missing values, in case of facebookFriends = `FALSE`, are replaced by the median of the same column. That is because you can’t do machine learning on a dataset with NA’s (missing values). You could also exclude these rows but since your data is limited I wouldn’t do that.

* **What do the following features in the dataset represent?** 

| Feature | Definition |
| ------- | ---------- |
| numLiveCampaigns | The number of live campaigns in addition to the current record. After all, a creator might have started multiple campaigns simultaneously. |
| addedPerks | The difference in the number of perks between my first and last scraping moment. Since many campaigns already started multiple days ago or even weeks prior to my first scraping moment I would not recommend using this feature (unless you only focus on campaigns which have been launched very close to the first scraping moment).
| experience | A synonym for adoption (speed) - the number of days since the registration on Kickstarter and their current campaign launch date.|
| aggregatedPerkValue | The perk value multiplied by the number of backers for a given perk. Note that this sum is often different than the total amount pledged. That is because some people pay more than is necessarily needed (e.g. a perk is $20 and you pay $21). | 
| overPaying | The difference between the expected total amount pledged and the `aggregatedPerkValue` |
| shippingWorld | A ratio that indicates the fraction of perks (at t=deadline) which are shipped worldwide (so if you have 3 perks and 2 of them are shipped worldwide it will be 2/3)| 
| numCompetitors | The number of campaigns in the same (sub)category as a measure to indicate how many similar projects are live at the moment of time (and thus competitors). | 
