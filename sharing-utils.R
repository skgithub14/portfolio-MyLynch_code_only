# sharing urls for UI
app.url <- "https://MyLynch.org"
share.message <- "MyLynch is a website developed at Dana-Farber Cancer Institute to help people with Lynch syndrome and their families by providing a personalized, free, and private cancer risk assessment with options to lower their risks for cancer:"
twitter.url <- paste0("https://twitter.com/intent/tweet?text=", share.message," ", app.url)
facebook.url <- paste0("https://www.facebook.com/sharer/sharer.php?u=", app.url)
linkedin.url <- paste0("https://www.linkedin.com/sharing/share-offsite/?url=", app.url)
whatsapp.url <- paste0('whatsapp://send?text=', share.message," ", app.url)

#' Email the user recovery information
#' 
#' @param recipientEmail string, email address of the recipient
#' `emailType = 'shareWebsite'`. It let's the recipient know who shared the 
#' website with them.
#' @param emailType string, one of c("sendReport", "shareWebsite")
#' @param senderName optional string with the name of the sender which will be 
#' included in the sharing email. Only required when `emailType = 'shareWebsite'`
#' @param reportFile optional string with attachment file location. Only required 
#' when `emailType = 'sendReport'`
#' @return an email is sent to the requested email address containing either 
#' the user's personalized PDF report, or a link to the website.
emailUser <- function(recipientEmail, emailType, 
                      senderName = NULL, reportFile = NULL){
  
  # username email subject and body
  if(emailType == "sendReport"){
    if(is.null(reportFile)){
      warning("reportFile arguement missing. Cannot email the report.")
    }
    
    # PRIVACY NOTE: THE GMAIL ACCOUNT HAS A FILTER WHICH AUTOMATICALLY DELETES 
    # SENT ITEMS FROM THE ACCOUNT WITH THE SUBJECT BELOW. IF YOU EDIT THIS SUBJECT 
    # THEN YOU MUST ALSO EDIT THE GMAIL FILTER.
    subject <- "Your personalized cancer risk report from MyLynch"
    body <- paste0("Dear User,\n\n",
                   "Attached you will find your personalized cancer risk report from MyLynch.",
                   " You can return to the site using this address: ",
                   app.url,
                   "\n\nThank you, \n",
                   "The BayesMendel Lab at Dana-Farber Cancer Institute\n\n\n",
                   "Please do not reply to this email, this 'no reply' email address is not monitored.")
    
    # password email subject and body
  } else if(emailType == "shareWebsite"){
    
    # PRIVACY NOTE: THE GMAIL ACCOUNT HAS A FILTER WHICH AUTOMATICALLY DELETES 
    # SENT ITEMS FROM THE ACCOUNT WITH THE SUBJECT BELOW. IF YOU EDIT THIS SUBJECT 
    # THEN YOU MUST ALSO EDIT THE GMAIL FILTER.
    subject <- "MyLynch: A Cancer Risk Tool for People with Lynch Syndrome"
    senderName <- ifelse(senderName == "", "Someone you know", senderName)
    body <- paste0("Hi!, \n\n",
                   senderName," thought that you might be interested in MyLynch. ",
                   share.message," ",app.url,
                   "\n\nThanks, \n",
                   "The BayesMendel Lab at Dana-Farber Cancer Institute\n\n\n",
                   "Please do not reply to this email, this 'no reply' email address is not monitored.")
  }
  
  # credentials: get token using service account method
  token <- gargle::credentials_service_account(
    scopes = c("https://www.googleapis.com/auth/gmail.send"), 
    path = Sys.getenv("gmail.json.path"), 
    subject = Sys.getenv("gmail.email") # Allow service account to act on behalf of this subject
  )
  # Assign token to .auth internal object from gmailr package
  assign("cred", token, gmailr:::.auth)
  
  # compose and send
  email_message <- 
    gm_mime() %>%
    gm_to(recipientEmail) %>%
    gm_from(Sys.getenv("gmail.noreply.email")) %>%
    gm_subject(subject) %>%
    gm_text_body(body)
  
  # optionally attached report
  if(emailType == "sendReport"){
    email_message <- 
      email_message %>%
      gm_attach_file(reportFile)
  }
  
  gm_send_message(email_message)
}

#' Check new email meets requirements
#' 
#' @param emEntry string, new email, first entry
#' @return string, Success if all checks passed, otherwise a description of the 
#' problems with the new email address.
checkNewEmail <- function(emEntry){
  mssg <- "Fail"
  if(is.character(emEntry)){
    if(grepl(pattern = "[@]", x = emEntry) & grepl(pattern = "[.]", x = emEntry)){
      mssg <- "Success"
    } 
  }
  return(mssg)
}
