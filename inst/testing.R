# Testing script while in DEV.
# Delete after

strava <- StravR()
strava$GetAppCredentials()
strava$GetAccessToken()


load(file.path(path.package("StravR"), "accesstoken.rda"))

api.response.json <- 
  getURL(paste0("https://www.strava.com/api/v3/athlete -d access_token = ",
                                   access.token))

api.response.list <- fromJSON(api.response.json, method = 'C')  