
## The Goal
The goal of this analysis is to answer these questions:

* Which teams draft the best and which draft picks provided the best/worst value for their teams?

* When drafting a certain position, which round should you draft the player in to achieve the highest value per pick?

## The Data
* All data is from pro-football-reference.com (draft data and NFL player performance data)

## Methodology

To be able to determine the value of a draft pick, we must first determine how to evaluate a player’s performance or value to their team during their time in the NFL. To do this, a metric known as approximate value (AV) will be used. Approximate Value, or AV, is a method of evaluating players developed by Doug Drinen from pro-football-reference.com; if you are interested in the details of his methodology you can read about at the link at the bottom of the page. The metric is designed to show that all players with one rating, say an 8, are collectively better than all players rated a 7. However, there will be debate about how one of the players rated a 7 is better than another player rated an 8. The thing to keep in mind is that collectively, all of the 8’s have performed better than all of the 7’s.

  
We next need to look at what teams are trying to accomplish with most of their draft picks. Rookie contacts are the cheapest contract in the league and for a team to be successful they need their players to out-perform the value of their contracts. This is important because the NFL has a salary cap, and when players out-perform their contracts, teams can spend those additional resources on other positions. Additionally, drafting well is important to establish core pieces of a team. It is very difficult to find a superstar in free agency since they are rarely available, teams are usually able to retain their star players by signing them before they become free agents.

The actual AV calculated in this analysis weighs years 5-8 of a player’s career at 50% if they are still with their draft team, compared to 100% for years 1-4. The change in weight is because in years 5-8 the player is being paid near market value, which results in lesser value per dollar compared to years 1-4.

## Acronyms and Calculations
**AV** – Approximate Value

**xAV** - Expected Apporximate Value - The median AV for all players, of the same position, drafted in a certain position in the draft

**Actual AV** – Cumulative Approximate Value in the first four years in the NFL + AV of years 5-8 if the player is still on their draft team, years 5-8 are weighted at 50%

**dAV** – Delta Approximate Value, the difference between RookieAV and the expected approximate value for a player drafted in the same area of the draft and plays the same position

**Grade** – Percentile of a player’s dAV, categorized by draft position and position played

## Draft Grade Breakdown

Draft_Grade  |  Percentile 
------------- | -------------- 
Bust    |   Bottom 5%     
Poor Pick        |    6%-25%
Below Average Pick     |   26%-40%
Average Pick       |   41%-60%
Good Pick       |    61%-75%   
Great Pick       |    76%-95%
Steal     |   Top 5%
