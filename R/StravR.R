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
  
  GenerateAccessToken <- function() {
    #' Gets an OAuth 2.0 Access Token by authorizing the user account to the 
    #' Strava API https://www.strava.com/oauth/authorize
    #' 
    #' When evaluated for the first time this function asks for User Consent
    #' for the Strava Account and retrieves the Access and Refresh Tokens
    #' for Authorization. These tokens are saved locally to a file on the user's system.
    #' If the user had authorized an account earlier and refresh token is already found
    #' on the user's system, then this function retrives a new Access Token and updates
    #' the Access Token File in user's memory.
    #'
    #' @export  
    #' @param None
    #' @examples
    #' 
    #' ga$GenerateAccessToken()
    #' 
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
        # API Credentials file exists  
        # Load the app_credentials file
        load(file.path(path.package("StravR"), "app_credentials.rda"))
        
        # Build the URL string
        client.id <- as.character(client.id) 
        client.secret <- as.character(client.secret)
        redirect.uri <- 'http://localhost'
        
      	# If a user wants to change read / write access to the api, use the scope option below
        # for saftey, this is set to 'public' to give viewability of user data. 
        url <- paste0('https://www.strava.com/oauth/authorize?',
                      'scope=public&',
                      'redirect_uri=', redirect.uri, '&',
                      'response_type=code&',
                      'client_id=', client.id, '&',
                      'approval_prompt=force')
        
    
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
  
  RefreshToAccessToken <- function(refresh.token, client.id, client.secret){
    #' This function takes the Refresh Token as an argument and retrives a new 
    #' Access Token based on it
    #' Reference : https://developers.google.com/accounts/docs/OAuth2#installed
    #' Args :
    #'   refresh.token : Refresh Token that was saved to the local file
    #'   client.id     : Client ID of the Application. This is a OAuth2.0 Credential
    #'   client.secret : Client Secret of the Application. Again this too is an
    #'                   OAuth2.0 Credential
    #' Returns :
    #'   Access Token  : New Access Token 
    
    refresh.token.list = fromJSON(postForm('https://accounts.google.com/o/oauth2/token',
                                           refresh_token = refresh.token,
                                           client_id = client.id,
                                           client_secret = client.secret,
                                           grant_type = "refresh_token",
                                           style = "POST" ))
    
    return(refresh.token.list$access_token)
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
  
  
  ValidateToken <- function() {
    #' This function checks whether the Access Token stored in the local file is 
    #' expired. If yes, it generates a new Access Token and updates the local file.
    #' If no, then it returns the stored Access Token
    #' @keywords internal 
    #' Args: 
    #'    None 
    #'  Returns: 
    #'    None
    #'    The old token is checked for expiry. If it is expired, a new access token is 
    #'    generated and updated in the local file
    
    load(file.path(path.package("StravR"),
                   "accesstoken.rda"))
    
    api.response.json <- getURL(paste0("https://www.googleapis.com/oauth2/v1/",
                                       "tokeninfo?access_token=",
                                       token.list$access_token
    ))
    api.response.list <- fromJSON(api.response.json, method = 'C')  
    check.token.param <- regexpr("error", api.response.json)
    
    if (check.token.param[1] != -1) {
      # If token has expired Generate a New Access token
      cat("Access Token had expired. Regenerating access token\n") 
      GenerateAccessToken()
      return(invisible())
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
              GenerateAccessToken  = GenerateAccessToken,
              RefreshToAccessToken = RefreshToAccessToken,
              RemoveToken          = RemoveToken,
              ValidateToken        = ValidateToken, 
              RemoveAppCredentials = RemoveAppCredentials)) 
}
