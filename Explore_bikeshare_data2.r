
ny = read.csv('new_york_city.csv')
wash = read.csv('washington.csv')
chi = read.csv('chicago.csv')

#libraries
library(ggplot2)


Na.Func <- function(N) {
    test.logic.null <- any(is.na(N))
    sum.null <- sum(is.na(N))
    colsums.null <- colSums(is.na(N))
      
    return(c(test.logic.null,sum.null,colsums.null))   

 
}
Na.Func(chi)
Na.Func(ny)
Na.Func(wash)

#Creating a mode function for the starting months.
Uniq.Func <- function(DS){
  U.F <- unique(DS)
  U.F[which.max(tabulate(match(DS, U.F)))]
}
#Calculating which month appeared the most.
Uniq.Func(chi)
Uniq.Func(ny)
Uniq.Func(wash)

Data.visualization <- function(DS) {
    
    # Convert DS Variable to d.t 
    d.t = DS
    
    # The as.Date methods accept character strings, factors, logical NA and objects 
     # Character strings are processed as far as necessary for the format specified: 
      # any trailing characters are ignored.
    
    d.t$Start.Time <- as.Date(d.t$Start.Time)
    
    ggplot(aes(format(Start.Time, "%Y-%m")), data = na.omit(d.t)) +
      geom_bar(width = 0.6, color= 'black', fill = 'blue') +
      ggtitle(' most common month Bar Charts ') +
      labs(x = 'first 6 months of 2017', y = 'trips count')
    
}
           
Data.visualization(chi)
Data.visualization(ny)
Data.visualization(wash)

" From the resulting plots we can see that the month with the most number
of trips is June (06/2017) and its the case for all the three cities
Which makes sense, because people drive bikes more in the summer than the
other seasons"

#Summary For All Datasets
summary(chi['Start.Time'])
print('----------------------------------------------------------------------------')
summary(ny['Start.Time'])
print('----------------------------------------------------------------------------')
summary(wash['Start.Time'])


head(ny, 3)
dim(ny['Trip.Duration'])

Total.T.D.Users <- function(DS){
    ggplot(aes("User.Type",`Trip.Duration`, fill = User.Type), data = subset(DS, User.Type != "")) + 
      geom_boxplot()+
      scale_y_discrete(limits = seq(0,3000,250))+
      ggtitle('total travel time for users in different cities')+
      labs(x = 'Users Type', y = 'Trip Duration')

}
Total.T.D.Users(ny)
Total.T.D.Users(chi)
Total.T.D.Users(wash)

"We notice that the customers numbers in trip duration in the 3 states 
are much more than the numbers of subscribers 
the customers seems to be very enthusiastic!!!"

by(ny$Trip.Duration, ny$User.Type, summary)
print("_________________________________________________________")
by(chi$Trip.Duration, chi$User.Type, summary)
print("_________________________________________________________")
by(wash$Trip.Duration, wash$User.Type, summary)

"We note that the descending order of the clients is in the 3 states respectively
1) Washonton
2) New York
3) Chicago
And in terms of subscribers
1) New York
2) Washonton
3) Chicago.
"

by(ny$Trip.Duration , ny$User.Type, mean )
print("_________________________________________________________")
by(chi$Trip.Duration , chi$User.Type, mean )
print("_________________________________________________________")
by(wash$Trip.Duration , wash$User.Type, mean )

User.Type <- function(DS) {
    ggplot(aes(x = User.Type), data=subset(DS, User.Type != "")) +
    geom_bar(color ='black', fill = '#ff9900') +
    ggtitle('counts of each user type')+
    labs(x = 'User Type', y = 'Count Of User Type')

}
User.Type(ny)
User.Type(chi)
User.Type(wash)

# We notice here that subscribers in the three states have more numbers than the customer 

summary(ny['User.Type'])
print('--------------')
summary(chi['User.Type'])
print('--------------')
summary(wash['User.Type'])

Count.Gender <- function(DS) {
    ggplot(aes(x = Gender), data = subset(DS, Gender != "")) +
    geom_bar(color ='black', fill = '#cc6600') +
    ggtitle('counts of Genders')+
    labs(x = 'Gender Type', y = 'Count Of Genders')
    
}
Count.Gender(ny)
Count.Gender(chi)

# We notice here that MAle in the New york And Chicago have more numbers than the Female

summary(ny['Gender'])
print('--------------')
summary(chi['Gender'])

E.M.Year.Of.Birth <- function(DS) {
    ggplot(aes(x = Birth.Year), data = na.omit(DS))+
    geom_bar(color = 'black', fill = '#00ffcc')+
    scale_x_continuous(breaks = seq(0,2000,3))+
    coord_cartesian(xlim=c(1940,2000)) +
    ggtitle(' the earliest And most common year of birth') +
    labs(x = 'The Birth By Years', y = 'Count')
}
E.M.Year.Of.Birth(ny)
E.M.Year.Of.Birth(chi)


"From the resulting plots we can see that, the earliest, most recent, most common year of birth Of
New York State (Between 1985 And 1990), But the biggest of them is (1989),
And in the state of Chicago most common year of birth (Between 1986 And 1992), But the biggest of them 
is the same age in New York (1989) 
"
#Summary For All Datasets
summary(chi['Birth.Year'])
print('-------------------')
summary(ny['Birth.Year'])

system('python -m nbconvert Explore_bikeshare_data.ipynb')
