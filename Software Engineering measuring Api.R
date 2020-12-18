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
gitDF=[gitDF$full_name=="jtleek/datahsraing","created_at"]

