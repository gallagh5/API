#install.packages("jsonlite")
library(jsonlite)
#install.packages("httpuv")
library (httpuv)
#install.packages("httr")
library (httr)
oauth_endpoints("github")
myapp<-oauth_app(appname="Coursera_Harvey_Gallagher",key="c5432fcd246819c1b6bc",
                 secret="1da5611f37186cc219dcff0e5853cdf900c72175")

github_token<-oauth2.0_token(oauth_endpoints("github"),myapp)
gtoken<-config(token=github_token)

req<-GET("https://api.github.com/users/gallagh5/repos",gtoken)

stop_for_status(req)

json1=content(req)

gitDF=jsonlite::fromJSON(jsonlite::toJSON(json1))

gitDF=[gitDF$full_name == "gallagh5/datasharing", "created_at"]




#Code used  from https://towardsdatascience.com/accessing-data-from-github-api-using-r-3633fb62cb08

#My Github data that I am retrieving
my_GitData = fromJSON("https://api.github.com/users/gallagh5")

#shows  number of Git followers
my_GitData$followers 

followers = fromJSON("https://api.github.com/users/gallagh5/followers")
followers$login
#shows user all my git followers

my_GitData$following 
#displays number of people I am following

following = fromJSON("https://api.github.com/users/gallagh5/following")
following$login 
#gives the names I am following on github

myData$public_repos #displays the number of repositories I have

repositeries = fromJSON("https://api.github.com/users/gallagh5/repos")
repositeries$name 
#Gives of the names of my public repositories
repositeries$created_at 
#Gives   dates the repositories were created 





#Interrogate the Github API to extract data from another account because I am only new to github.
#From browsing on github I noticed that Fabien Potencier is a very active developer
myData = GET("https://api.github.com/users/fabpot/followers?per_page=100;", my_token)
stop_for_status(myData)
extract = content(myData)
data_frame = jsonlite::fromJSON(jsonlite::toJSON(extract)) #converts into data frame
data_frame$login

# Retrieve usernames 
id = data_frame$login
user_ids = c(id)



# Create an empty vector and data.frame
gitusers = c()
gitusersDB = data.frame(
  username = integer(),
  following = integer(),
  followers = integer(),
  repositeries = integer(),
  date_Created = integer()
)

#loops through users and adds to list
for(i in 1:length(user_ids))
{
  
  followinglinkURL = paste("https://api.github.com/users/", user_ids[i], "/following", sep = "")
  followingReq = GET(followinglinkURL, gtoken)
  following_content = content(followingReq)
  
  #skips if the user doest folow the person we are extracting data
  if(length(following_content) == 0)
  {
    next
  }
  
  following_df = jsonlite::fromJSON(jsonlite::toJSON(following_content))
  following_login = following_df$login
  
  #Loop through users who follow the page
  for (j in 1:length(following_login))
  {
    # Check that the user is not in list already
    if (is.element(following_login[j], users) == FALSE)
    {
      #Add user to list of followers
      gitusers[length(gitusers) + 1] = following_login[j]
      
      #Retrieve data on each user
      following_newURL = paste("https://api.github.com/users/", following_login[j], sep = "")
      following2 = GET(following_newURL, gtoken)
      following_content2 = content(following2)
      following_df2 = jsonlite::fromJSON(jsonlite::toJSON(following_content2))
      
      #Retrieve who each user follows
      following_Number = following_df2$following
      
      #Retrieve each user's followers
      followers_Number = following_df2$followers
      
      #Retrieve each user's number of repositories
      repositeryNumber = following_df2$public_repos
      
      #Retrieve year which each user joined Github
      yearCreated = substr(following_df2$created_at, start = 1, stop = 4)
      
      #Add users data to a new row in dataframe
      gitusersDB[nrow(gitusersDB) + 1, ] = c(following_login[j], following_Number, followers_Number, repositoryNumber, yearCreated)
      
    }
    next
  }
  #Stop when there are more than 150
  if(length(users) > 150)
  {
    break
  }
  next
}

#Use plotly to graph

Sys.setenv("plotly_username"="gallagh5")
Sys.setenv("plotly_api_key"="l2hkDe0C8ZIFIH0VI7V3")

# plot repositories v followers coloured by year
plot1 = plot_ly(data = usersDB, x = ~repositories, y = ~followers, 
                text = ~paste("Followers: ", followers, "<br>Repositories: ", 
                              repos, "<br>Date Created:", dateCreated), color = ~dateCreated)
plot1

#send to plotly
api_create(plot1, filename = "Repositories vs Followers")

#plot 2 graphs following v followers coloured by year
plot2 = plot_ly(data = GitusersDB, x = ~following, y = ~followers, text = ~paste("Followers: ", followers, "<br>Following: ", following), color = ~dateCreated)
plot2

#send to plotly
api_create(plot2, filename = "Following vs Followers")

#now attempting to graph the 10 most popular languages used by the 250 users.
languages = c()

for (i in 1:length(Gitusers))
{
  Repositoriesurl = paste("https://api.github.com/users/", users[i], "/repos", sep = "")
  Repositories = GET(Repositoriesurl, gtoken)
  Repositories_content = content(Repositories)
  Repositories_df = jsonlite::fromJSON(jsonlite::toJSON(Repositories_Content))
  Repositories_names = Repositories_dF$name
  
  #Loop through all the repositories of an individual user
  for (j in 1: length(Repositories_names))
  {
    #Find all repositories and save in data frame
    Repositories_url2 = paste("https://api.github.com/repos/", users[i], "/", Repositories_names[j], sep = "")
    Repositories2 = GET(Repositoriesurl2, gtoken)
    Repositories_content2 = content(Repositories2)
    Repositories_df2 = jsonlite::fromJSON(jsonlite::toJSON(Repositories_content2))
    language = Repositories_df2$language
    
    #Removes repositories containing no specific languages
    if (length(language) != 0 && language != "<NA>")
    {
      languages[length(languages)+1] = language
    }
    next
  }
  next
}

#Puts 10 most popular languages in table 
allLanguages = sort(table(languages), increasing=TRUE)
top10Languages = allLanguages[(length(allLanguages)-9):length(allLanguages)]

#converts to dataframe
language_DF = as.data.frame(top10Languages)

#Plot the data frame of languages
plot3 = plot_ly(data = language_DF, x = language_DF$languages, y = language_DF$Freq, type = "bar")
plot3

Sys.setenv("plotly_username"="gallagh5")
Sys.setenv("plotly_api_key"="l2hkDe0C8ZIFIH0VI7V3")
api_create(plot3, filename = "10 Most Popular Languages")

