#' Creates a skeleton shell for accessing the Strava (V3) API.
#' 
#' In order to utilise this package a user must create a STRAVA api application
# 'https://www.strava.com/settings/api
#' 
#' @export
#' @import rjson RCurl
#' @return
#'   Returns a list of methods, for accessing the Strava reporting API
#'       GetAppCredentials()
#'       RemoveToken()
#'       RemoveAppCredentials()
#'       ValidateToken()
#'       GenerateAccessToken()
#'       RefreshToAccessToken()
#' @examples
#' strava <- StravR()     
StravR <- function() {
  # We use oauth 2.0 API to authorize the user account 
  # and to get the access token to request to the Starva data API.
  
  query.uri <- NULL
  dataframe.param <- data.frame()
  
  # Set the CURL options for Windows    
  options(RCurlOptions = list(capath = system.file("CurlSSL",
                                                   "cacert.pem", 
                                                   package = "RCurl"),
                              ssl.verifypeer = FALSE))
  

  GetAppCredentials <- function(client.id, client.secret) {
    #' Save App Credentials (Client ID and Client Secret) locally to system
    #'  
    #' This function gets the Client ID and Client Secret of the Application from
    #' the user and saves it locally to a file for OAuth 2.0 Authorization flow. 
    #' 
    #' One must create an app in Strava https://www.strava.com/settings/api
    #' Recommended 
    #'  
    #' @export  
    #' @param client.id Client ID of the Application 
    #' @param client.secret Client Secret of the Application
    #' 
    #' @examples
    #' strava <- StravR()
    #' strava$GetAppCredentials('xxxxxxxxxxxx','xxxxxx-xxxxxxxxxxxxx')
    #' @return Saves the App Credentials to a file on the user's system     
    
    
    if(file.exists(file.path(system.file(package = "StravR"),
                             "app_credentials.rda"))) {
      stop(cat("Your Application Credentials are already saved to your system. 
               Please use the RemoveAppCredentials Function to delete the credentials\n"))
    }
    
    # Argument Validation
    if (missing(client.id)) {
      stop(cat("Please specify a Client ID in the function arguments"))
    }
    
    if (missing(client.secret)) {
      stop(cat("Please specify a Client Secret in the function arguments"))
    }
    client.id <- as.character(client.id)
    client.secret <- as.character(client.secret)
    
    save(client.id,
         client.secret,
         file = file.path(system.file(package = "StravR"),
                          "app_credentials.rda"))
    file.path <- as.character(file.path(system.file(package = "StravR"),
                                        "app_credentials.rda"))
    cat("Your App Credentials have been saved to", file.path, "\n")
    }
  
  SaveAccessToken <- function(access.token) {
    #' Saves an OAuth 2.0 Access Token from a user enable Strava App 
    #' 
    #' When evaluated for the first time this function takes the access token and saves
    #' them locally to a file on the user's system.
    #' If the user had authorized an account earlier nothing happens.
    #'
    #' @export  
    #' @param access_token the access token from strava for an enabled app
    #'    https://www.strava.com/settings/api 
    #' @returns None. The Tokens are saved to a file on the user's system.
    #'   by default the location is where the package is installed.
    
    # Check if the Access Token File already exists
    if(!file.exists(file.path(path.package("StravR"), "accesstoken.rda"))) {
      # File Does not exist
      # Check if API_Creds exists
      if(!file.exists(file.path(path.package("StravR"), "app_credentials.rda"))) {
        stop(cat("Application Credentials do not exist.\nPlease use the GetAppCredentials 
                 function to save the credentials to a local file"))
      } else {
                
        save(token.list, file = file.path(path.package("StravR"), "accesstoken.rda"))
        
        access.token.file.path <- 
          as.character(file.path(path.package("StravR"), "accesstoken.rda"))                 
        
        cat("Access token has been saved to", access.token.file.path, "\n")
        
        return(invisible())
      }
    
    } else {
      # Load the Access Token from the file saved to the system
      
      load(file.path(path.package("StravR"), "accesstoken.rda"))
      load(file.path(path.package("StravR"), "app_credentials.rda"))
      
      # Get new Access Token
      access.token <- RefreshToAccessToken(token.list$refresh_token, client.id, client.secret)
      
      #In case if a New Access Token is generated update it in the file as well
      token.list$access_token <- access.token
      
      #Save the updated credentials into the file
      save(token.list, file = file.path(path.package("StravR"), "accesstoken.rda"))
      
      cat("Access token has been regenerated\n")
      
      return(invisible())
    }
  }
    
  RemoveToken <- function() {
    #' Deletes the stored Access and Refresh Tokens from the local file 
    #'
    #' In case if the user wants to query a new profile, then the Authorization flow 
    #' has to repeated. This requires deleting the stored Access and Refresh Tokens
    #' from the system file
    #' 
    #' @export
    #' @param None
    #' @examples
    #' ga$RemoveToken()
    #' 
    #' Returns : 
    #'    Deletes the Access Token file from the system
    
    if(file.exists(file.path(path.package("StravR"), "accesstoken.rda"))) {
      unlink(file.path(path.package("StravR"),
                       "accesstoken.rda"),
             recursive = FALSE)
      cat("The Access token has been deleted from your system\n")    
    } else {
      stop(cat("The Access token file could not be found on your system\n"))
    }
    
  }
  
  RemoveAppCredentials <- function() {
    #' Deletes App Credentials(Client ID and Client Secret) from the system
    #'
    #' In case if the user creates a new project in the Google API Developer Console, then
    #' a fresh set of OAuth2.0 credentials (Client ID and Client Secret) are provided
    #' This requires deletion of old stored credentials
    #' 
    #' @export
    #' @param None
    #' @examples
    #' ga$RemoveAppCredentials()  
    #' Returns : 
    #'   Deletes the app_credentials.rda file from the user's system
    
    if(file.exists(file.path(path.package("StravR"), "app_credentials.rda"))) {
      unlink(file.path(path.package("StravR"),
                       "app_credentials.rda"),
             recursive = FALSE)
      cat("The Application Credentials have been deleted from your system\n")  
    } else {
      stop(cat("The Application Credentials file could not be found on your system\n"))
    }
    
  }
  
  ParseApiErrorMessage <- function(api.response.json) {
    #' To check whether the returned JSON response is error or not. 
    #' If it is error then it will  
    #' @keywords internal 
    #' Args :  
    #'   api.response.json: The json data as reposnse returned by the 
    #'   Google Data feed API or Google Management API
    #' Returns :
    #'   If there is error in JSON response then this function will return the 
    #'   related error code and message for that error.
    api.response.list <- fromJSON(api.response.json, method = 'C')  
    check.param <- regexpr("error", api.response.list)
    if (check.param[1] != -1) {
      return(list(code = api.response.list$error$code,
                  message = api.response.list$error$message))
    } else {
      code <- NULL
      return(api.response.list)
    }   
  }
  ##############################################################################
  
  return(list(GetAppCredentials    = GetAppCredentials,
              ParseApiErrorMessage = ParseApiErrorMessage,
              SaveAccessToken      = SaveAccessToken,
              RemoveToken          = RemoveToken,
              RemoveAppCredentials = RemoveAppCredentials)) 
}
