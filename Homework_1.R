#---------------------------#
#--------- SDS 313 ---------#
#-------- Homework 1 -------#
#------- Sep. 2, 2024 ------#
#---------------------------#

#-------------1-------------#
bonds <- read.csv('Homework1_Bonds.csv')

#Number of bonds carried and defeated
table(bonds$Result)

#Pull out bonds that were carried
bonds_carr <- bonds[bonds$Result=='Carried',]

#Total number of bonds that sought approval by each government type
table(bonds$Type)

#Number of bonds carried by each government type
table(bonds_carr$Type)

#Gives the rates of approved bonds by each government type
sum(bonds_carr$Type == 'CITY') / sum(bonds$Type == 'CITY')
sum(bonds_carr$Type == 'COUNTY') / sum(bonds$Type == 'COUNTY')
sum(bonds_carr$Type == 'ISD') / sum(bonds$Type == 'ISD')
sum(bonds_carr$Type == 'WD') / sum(bonds$Type == 'WD')

#Another simpler way to find the rates of approved bonds by each government type
prop.table(table(bonds$Type,bonds$Result), 1)
#---------------------------#


#-------------2-------------#
#Adds a column for total number of votes
bonds$Votes_Total <- bonds$VotesFor + bonds$VotesAgainst

#Gives the details of the bond with the highest number of votes
bonds[which.max(bonds$Votes_Total),]
#---------------------------#


#-------------3-------------#
#Adds a column to bonds carried subset with votes total
bonds_carr$Votes_Total <- bonds_carr$VotesFor + bonds_carr$VotesAgainst

#Creates new subset with bonds that had at least 100 votes
votes100 <- bonds_carr[bonds_carr$Votes_Total>=100,]

#Adds a column that gives the proportion of votes for the bond
votes100$PropFor <- votes100$VotesFor / votes100$Votes_Total

#Gives histogram displaying the proportion of votes for of bonds that have at least 100 votes
hist(votes100$PropFor, main = 'Histogram of Bonds Carried with at Least 100 Votes', xlab = 'Proportion of Votes For', col = 'lightblue', xlim = c(.5,1))

#Gives the center or median of the proportion of votes for the bonds that have at least 100 votes
median(votes100$PropFor)

#Gives the IQR or spread of the proportion of votes for the bonds that have at least 100 votes
IQR(votes100$PropFor)
#---------------------------#


#-------------4-------------#
#Finds the correlation coefficient of whether the margin a bond was approved is related to the cost
cor(votes100$PropFor, votes100$Amount)

#Graph of the relationship between the margin a bond was approved to its cost
plot(votes100$PropFor, votes100$Amount,main='Proportion of Votes For and Cost',xlab='Proportion of Votes For',ylab='Cost')
#---------------------------#

print("Hello World")
