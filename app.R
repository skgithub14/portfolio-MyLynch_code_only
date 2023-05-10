#### Set-up ####

# shiny
library(shiny)
library(shinyBS) # shiny tool tips
library(shinyjs) # java script tools

# data manipulation
library(tidyverse)
library(rlang)

# html
library(htmltools)

# line and bar plots
library(ggplot2)
library(plotly)
library(ggplotify)
library(ggpattern)

# waffle/personograph graph
library(ggimage)

# arrange graphics
library(gridExtra)
library(grid)
library(gtable)

# image processing
library(png)
library(Cairo)

# email
library(gmailr)
library(httr)

# load data
load("./panelpro-lynch-data.RData")
other.cancers <- read.csv("other-cancers.csv")

# utils and variables
source("./global-utils.R")
source("./sharing-utils.R")
source("./data-utils.R")
source("./plot-utils.R")

# dana farber logo for reports
logo <- readPNG("./www/dana-farber-logo-small-2.PNG", native = TRUE)

# check for mobile device connection
mobileDetect <- function(inputId, value = 0) {
  tagList(
    singleton(tags$head(tags$script(src = "js/mobile.js"))),
    tags$input(id = inputId,
               class = "mobile-element",
               type = "hidden")
  )
}


#### UI ####

ui <- fixedPage( title = "MyLynch: A Cancer Risk Tool for People with Lynch Syndrome",
  
  # Google analytics
  tags$head(includeHTML(("google-analytics.html"))),
  
  # title banner
  fluidRow(div(style = "margin-top: 10px;",
  
    # dana-faber logo
    column(width = 5, img(src="dana-farber-logo-small-2.PNG") ),
    
    # Share buttons
    column(width = 7,
      div(style = "display:inline-block;float:right;margin-top:15px;",
        HTML("<font size=4>share this tool: </font size>"),
        actionButton("twitter_share",
                    label = "",
                    icon = icon("twitter"),
                    onclick = sprintf("window.open('%s')", twitter.url),
                    style = "color: #1DA1F2"),
        actionButton("facebook_share",
                     label = "",
                     icon = icon("facebook"),
                     onclick = sprintf("window.open('%s')", facebook.url),
                     style = "color: #4267B2"),
        actionButton("linkedin_share",
                     label = "",
                     icon = icon("linkedin"),
                     onclick = sprintf("window.open('%s')", linkedin.url),
                     style = "color: #0072B1"),
        actionButton("whatsapp_share",
                     label = "",
                     icon = icon("whatsapp"),
                     onclick = sprintf("window.open('%s')", whatsapp.url),
                     style = "color: #25D366"),
        actionButton("email_share",
                     label = "",
                     icon = icon("envelope"),
                     style = "color: grey"),
        
        # pop-up dialog box to enter email when share button is clicked
        bsModal(id = "emailShareModal",
                title = "Enter an Email Address",
                trigger = "email_share",
                textInput(inputId = "shareName",
                          label = "Your name (optional):"),
                textInput(inputId = "shareEmail",
                          label = "Recipient's email address:"),
                conditionalPanel(condition = "!output.goodShareEmail",
                                 h5("You must enter a valid email address.", style = "color: red") 
                ),
                actionButton(inputId = "shareEmailGo",
                             label = "Send Email"),
                conditionalPanel(condition = "output.sentShareEmail",
                                 h4("Email Sent!", style = "color: blue") 
                )
        )
      )
    )
  )),
    
  
  ##### Nav Bar ####
  navbarPage(id = "navbarTabs",
      
    tags$style( type = "text/css",
      ".navbar {background-color: #10699B}",
      ".navbar-default .navbar-nav li a {color: white;font-weight: bold;}",
      ".navbar-default .navbar-nav .active a {color: white;background-color: #F89823;}",
      ".navbar-default .navbar-nav .active a:focus {color: white;background-color: #F89823;}",
      ".navbar-default .navbar-nav li a:hover {color: white;background-color:#3CC5F1;"
    ),
    
    ##### Home Tab ####
    tabPanel("Home",
             
      # runs check for mobile device, nothing displayed
      mobileDetect('isMobile'),
      # for troubleshooting: check mobile device status
      # textOutput("check_mobile"),
             
      h1("MyLynch: A Cancer Risk Tool for People with Lynch Syndrome", style = "margin-bottom: 15px;"),
      
      fluidRow(
        column(width = 7,
               
          p("MyLynch was built by cancer researchers and statisticians from the 
            BayesMendel lab at Dana-Farber Cancer Institute and Harvard University 
            to help people with Lynch Syndrome (LS) understand how their LS can 
            increase the risk of different cancers and to show them what they can 
            do to lower their risks."),
          p(HTML("<i>MyLynch is new and we are seeking YOUR input 
            to help us improve this website; there will be a link to a user survey 
            at the end of the tool so please give us feedback.</i>"), style = "color:blue;"),
        
          h2("What is Lynch Syndrome?", style = "margin-bottom: 15px; margin-top: 30px;"),
          p("Lynch Syndrome (LS) is a condition passed down through families that 
            affects about 1 in 300 people in the United States. People with LS 
            have a significantly increased likelihood of developing one or more 
            cancers throughout their lifetime, with colorectal cancer being the 
            most common. As LS research has evolved, many other cancers have also 
            been linked to LS however, advances in medicine have also found 
            effective ways to prevent and treat these cancers."),
          p("LS is caused by a pathogenic mutation on one of five genes:", 
            HTML("<ul> <li>MLH1</li> <li>MSH2</li> <li>MSH6</li> <li>PMS2</li> <li>EPCAM</li> </ul>")),
          p("There are tests available for LS, both commercially and through your 
            doctor, to detect if you have a pathogenic mutation on one of the genes 
            above. If someone in your family has been diagnosed with LS, or your 
            family has a history of cancer, you may have LS and you should talk 
            with your doctor about getting tested."),
          p("People diagnosed with LS are often referred to a medical specialist 
            called a genetic counselor. Your genetic counselor will work with your 
            doctor to make a plan to manage your LS."),
          p(HTML("Dana-Farber has a dedicated site for LS where you can learn more:
                  <a href = 'https://www.dana-farber.org/cancer-genetics-and-prevention/syndromes-genes-and-programs/lynch-syndrome-center/' id = 'redirect' target='_blank'>
                    click here</a>")),
          
          h2("What Does MyLynch Do?", style = "margin-bottom: 15px; margin-top: 30px;"),
          p("This tool is based on a large body of medical research that links 
            specific LS genes to different cancer types. The research shows that 
            for people with LS, risk for these cancers varies widely from 
            person-to-person based on several factors such as which gene is 
            causing their LS, their sex, their age, and other factors. This tool 
            will lead your through a series of steps and in the end, you can get 
            a personalized report that tells you:"),
          p("1. Which cancers you are at risk for due to LS.", style = "text-indent: 1%"),
          p("2. What your chance of getting each cancer is.", style = "text-indent: 1%"),
          p("3. What you can do to lower those risks.", style = "margin-bottom: 15px; text-indent: 1%"),
          p("The visualization below are just some examples of what you'll see when using MyLynch.")
        ),
        column(width = 5,
               br(),
               br(),
               img(src = "lynch-infographic.png", align = "right", width = "100%")
        )
      ),
      
      fluidRow(
        column(width = 4,
               br(),
               br(),
               img(src = "line-bar-example.png", align = "center", width = "100%")
        ),
        column(width = 8,
          h3("Lowering Your Risk", style = "margin-bottom: 15px; margin-top: 30px;"),
          p("Based on your unique characteristics you may have options to lower 
            your risk for cancer such as losing weight, taking aspirin, colonoscopies, 
            or even electing to have preventative surgery. No two people are the 
            same, so the options you have for lowering your risk may be different 
            from someone else’s options."),
          
          h3("Your Personalized Report", style = "margin-bottom: 15px; margin-top: 30px;"),
          p("Near the end of the tool, you will build a personalized report which 
            you can download or email to your doctor or genetic counselor. If you 
            choose, you can also send it to a family member or trusted friend."),
          
          h3("Sharing the tool", style = "margin-bottom: 15px; margin-top: 30px;"),
          p("Because LS is an inherited condition, you may have family members who 
            have LS but are undiagnosed. You can share this tool with family members 
            to motivate them to get tested for LS and help them understand their 
            risks. There are share buttons in the top right corner of the website 
            and these buttons will appear again in the last step. Don't worry, these 
            buttons only share the link to this tool; they do not share any information 
            that you enter, nor do they share your personalized report."),
          
          h3("Your Privacy", style = "margin-bottom: 15px; margin-top: 30px;"),
          p("Because we are a hospital, we take patient privacy very seriously. 
            This website will ask you for basic information like your age, sex, 
            Lynch gene, and more, but rest assured that we do not share this 
            information with anyone, nor do we store this information anywhere. 
            No one can see your information except you due to this site’s security 
            features."),
          
          h3("Staying Up-to-date", style = "margin-bottom: 15px; margin-top: 30px;"),
          p("LS research is evolving rapidly, and as it does, we will continually 
            update and improve this site with the latest LS information, including 
            any new associated LS cancers and risk reducing options.")
        )
      ),
      fluidRow(column(width = 9,
          h3("What MyLynch Does Not Do", style = "margin-bottom: 15px; margin-top: 30px;"),
          p("MyLynch can aid in making informed decisions but, it does not replace 
            the advice of a medical doctor. Consult your doctor for all LS questions 
            and before making any LS related decisions."),
          
          h3("Getting Started", style = "margin-bottom: 15px; margin-top: 30px;"),
          p("First, you will need to know your Lynch gene to use the tool. If you 
            do not know your Lynch gene, ask your doctor first and then come back."),
          p("You can take as little or as much time as you want to go through the 
            tool, but for first time users it is recommended to set aside about 
            10 to 30 minutes."),
          p("When you are ready to begin, click the button below or go to the 
            'Get My Cancer Risks' tab."),
          actionButton(inputId = "getStarted", 
                       label = "Get Started",
                       style = "color: white; background-color: #10699B; border-color: #10699B")
      )),
      fluidRow(column(width = 9, 
                      br(),
                      br(),
                      img(src = "pgraph-example.png", align = "center", width = "100%")
      )),
      fluidRow(column(width = 9,
          h3("Assumptions and References", style = "margin-bottom: 15px; margin-top: 30px;"),
          p(HTML(paste0("For technical notes on the assumptions used to calculate the impact of risk reducing interventions used in this tool 
                           and for a list of references ",a(href = 'assumptions-and-references.pdf', "click here", target="_blank"),".")))
        )
      )
    ),
    
    
    ##### Get My Cancer Risks Tab ####
    tabPanel(title = "Get My Cancer Risks",
     h1("MyLynch: Your Personal Cancer Risk", style = "margin-bottom: 20px;"),
     
     # progress bar image
     imageOutput(outputId = "progress", inline = T),
     
     ###### Instructions ####
     tabsetPanel( id = "calculatorTabs", type = "hidden",
                  
       tabPanel("Instructions",
              
          h2("Navigating the Tool", style = "margin-top:25px;margin-bottom:15px;"),
          
          fluidRow(column(width = 9,
            p("This tool will take you through several screens which you can follow 
              with the progress bar at the top.",
              HTML("When you are done with a screen, <b>look for the Continue and 
                   Back buttons at the bottom</b> to move on or go back.")),
            p("You can take as little or as much time as you want to go through 
              the tool, but for first-time users, ",
              HTML("<b>we recommended to set aside about 10 to 30 minutes.</b>")),
            p(HTML("<b>Make sure you know which Lynch gene you have a mutation on</b>."), 
              " If you don't know this, ask your doctor and then come back.", 
              style = "margin-top: 10px"),
            
            h2("CLICK CONTINUE TO PROCEED", style = "margin-top:25px;margin-bottom:0px;"),
           actionButton(inputId = "goEnter", 
                        label = "Continue",
                        icon = icon('play'),
                        style = "color: white; background-color: #10699B; border-color: #10699B; margin-top: 20px")
          ))
        ),
       
        ###### User Information ####
        tabPanel("1. My Profile",
                
          h2("My Profile", style = "margin-bottom: 15px"),
          fluidRow(column(width = 9,
            p("Things like your sex, current age, Lynch gene, and cancer history 
               are crucial to calculating you cancer risks. 
               Based on your Lynch gene and sex, you may be asked to enter other 
               information like your height, weight, and some surgical history. 
              Things like your race and ethnicity will help us further personalize 
              your risk estimates."),
            p("We only ask for the bare minimum information needed to calculate your 
               personal risk. ", 
              HTML("<b>We do not share this information with anyone, 
               nor do we store this information anywhere.</b> 
               No one can see the information you enter except you.")),
            p(HTML("<b>Once you have answered all the questions, the continue button 
               will appear at the bottom of the screen.</b>"))
          )),
          
          # user's sex input
          selectInput(inputId = "gender", label = h4("My sex is:"),
                      choices = c(" "=" ","Female"="Female","Male"="Male"),
                      width = "150px"),
          
          # user's prophylactic surgery input, conditioned on sex being female
          conditionalPanel(condition = "input.gender == 'Female'",
            
            h4("Select if you have had either or both of these surgeries:"),
            checkboxInput(inputId = "oophorectomy",
                          label = h4("I had both ovaries removed (called an oophorectomy)", style = "margin-top: 3px;"),
                          FALSE, 
                          width = "50%"),
            bsTooltip(id = "oophorectomy",
                      title= "If you had both surgeries at the same time then check both.",
                      placement = "right"),
            
            checkboxInput(inputId = "hysterectomy",
                          label = h4("I had my uterus removed (called a hysterectomy)", style = "margin-top: 3px;"),
                          FALSE, 
                          width = "50%"),
            bsTooltip(id = "hysterectomy",
                      title= "If you had both surgeries at the same time then check both.",
                      placement = "right")
          ),
          
          # user's age input
          numericInput(inputId = "age", label = h4("My current age is:"),
                       min = 1, max = max.age-5, value = "",
                       width = "150px"),
          
          conditionalPanel(condition = "!output.goodAge",
            h5("You must enter a valid age from 1 to 80", style = "color: red") 
          ),
          
          # user's race and ethnicity
          selectInput(inputId = "race", label = h4("My race is:"),
                      choices = c("Other, Mixed Race, or Prefer Not to Specify" = "All_Races",
                                  "American Indian/Alaskan Native" = "AIAN",
                                  "Asian/Pacific Islander" = "Asian",
                                  "Black" = "Black",
                                  "White" = "White"),
                      width = "35%"),
          bsTooltip(id = "race",
                    title= "Risks vary between races so if you provide this information you will receive a more personalized risk assessment.",
                    placement = "right"),
          selectInput(inputId = "ethnicity", label = h4("My ethnicity is:"),
                      choices = c("Other, Mixed Ethnicity, or Prefer Not to Specify" = "Other_Ethnicity",
                                  "Hispanic" = "Hispanic",
                                  "Non-Hispanic" = "Non-Hispanic"),
                      width = "35%"),
          bsTooltip(id = "ethnicity",
                    title= "Risks vary between ethnicities so if you provide this information you will receive a more personalized risk assessment.",
                    placement = "right"),
                           
          # user's gene mutation input
          selectInput(inputId = "gene", label = h4("My Lynch gene is:"),
                      choices = c(" "=" ","MLH1"="MLH1","MSH2"="MSH2",
                                  "MSH6"="MSH6","PMS2"="PMS2","EPCAM"="EPCAM")),
          
          conditionalPanel(condition = "output.goodAge & input.gene != ' ' & input.gender != ' '",
              
            # BMI
            conditionalPanel(condition = "input.gene == 'MLH1' | 
                            (input.gender == 'Female' & input.gene != 'EPCAM' & !input.hysterectomy & !output.hadEndo)",             
              h4("My Body Mass Index (BMI):", style = "margin-top:25px;"),
              h5("Your height and weight determine your BMI which will help us customize your risk."),
               
              div(style = "display: inline-block; width: 150px",
                  selectInput(inputId = "heightFT",
                              label = "Height (feet):",
                              choices = seq(4,8),
                              selected = 5)
              ),
              div(style = "display: inline-block; width: 150px",
                  selectInput(inputId = "heightIN",
                              label = "Height (inches):",
                              choices = seq(0,11),
                              selected = 0)
              ),
               
              numericInput(inputId = "weight",
                           label = "Weight (pounds):",
                           min = 50,
                           max = 500,
                           value = ""),
              conditionalPanel(condition = "!output.goodWeight",
                h5("Enter a valid weight", style = "color: red")
              ),
               
              conditionalPanel(condition = "output.goodWeight",
                conditionalPanel(condition = "input.calcBMI==0",
                                 actionButton(inputId = "calcBMI",
                                              label = "Calculate my BMI",
                                              icon = icon('calculator'),
                                              style = "color: white; background-color: #10699B; border-color: #10699B; margin-bottom: 15px;")
                ),
                conditionalPanel(condition = "input.calcBMI==1",
                                 actionButton(inputId = "recalcBMI",
                                              label = "Re-calculate my BMI",
                                              icon = icon('calculator'),
                                              style = "color: white; background-color: #10699B; border-color: #10699B"),
                                 h4(HTML(paste0("<b>My BMI is: ",textOutput(outputId = "bmi", inline = TRUE), "</b>")),
                                    style = "text-indent: 2%; margin-bottom: 15px;")
                )
              )
            ),
             
            # previous cancers
            selectInput(inputId = "hadCancer",
                        label = h4("Have you had cancer before (or do you have it now)?"),
                        choices = c(" ","Yes","No"),
                        width = "40%"),
             
            conditionalPanel(condition = "input.hadCancer == 'Yes'",
              checkboxGroupInput(inputId = "previousCancers",
                                 label = h4("Check any cancers below that you have had (or have now):"),
                                 choices = cancer.choices,
                                 width = "75%"),
              fluidRow(column(width = 9,
                p("This tool provides estimates only for the first occurence of cancer therefore any cancers
                   selected above will not be analyzed. For questions about the risks of any of your previous
                   cancers returning, consult your doctor."),
                
                conditionalPanel(condition = "input.donePrevCancers==0",
                  actionButton(inputId = "donePrevCancers", 
                               label = "I am done selecting previous cancers",
                               style = "color: white; background-color: #10699B; border-color: #10699B")
                )
              ))
            )
          ),
        
          # messages for no risk displays available
          conditionalPanel(condition = "output.noEstimate & output.goodAge & input.gender == 'Male' & input.gene != ' '",
            fluidRow(column(width = 9,
                           p("Based on the information you entered, this tool cannot calculate cancer risks for you. 
                              You have had (or have) each of the possible cancers related to your Lynch Syndrome gene.")
            ))
          ),
       
          conditionalPanel(condition = "output.noEstimate & output.goodAge & input.gender == 'Female' & input.gene != ' '",
            fluidRow(column(width = 9,
              p("Based on the information you entered, this tool cannot calculate cancer risks for you. 
                 You have had (or have) each of the possible cancers related to your Lynch Syndrome gene already
                 and/or you have had a preventative surgery for some of these cancers already.")
            ))
           ),
          
          # tab switching
          fluidRow(column(width = 9,
            h2("CLICK CONTINUE TO PROCEED", style = "margin-top:25px;margin-bottom:15px;"),
            p("When you have entered all the necessary information, the continue button will appear below.")
          )),
            
          conditionalPanel(condition = "!output.goodAge | input.gene == ' ' | input.gender == ' ' | input.hadCancer == ' ' | (input.hadCancer == 'Yes' & input.donePrevCancers==0)",
             actionButton(inputId = "backInstructions", 
                          label = "Back",
                          style = "color: white; background-color: #10699B; border-color: #10699B; margin-top: 25px")
          ),
          
          conditionalPanel(condition = "output.goodAge & input.gene != ' ' & input.gender != ' ' & input.hadCancer != ' ' & !output.noEstimate & (input.hadCancer == 'No' | (input.hadCancer == 'Yes' & input.donePrevCancers==1))",
            actionButton(inputId = "backInstructions1", 
                         label = "Back",
                         style = "color: white; background-color: #10699B; border-color: #10699B"),
            actionButton(inputId = "goList", 
                         label = "Continue",
                         icon = icon('play'),
                         style = "color: white; background-color: #10699B; border-color: #10699B")
          )
       ), # end of tab
       
       ###### Cancer Table #### 
       tabPanel("2. Possible Cancers",
         h2("Possible Cancers", style = "margin-top: 10px; margin-bottom: 20px"),
         
         fluidRow(column(width = 9,              
             p(HTML("<b>You have a higher risk for the following cancers, compared to someone without Lynch Syndrome.</b>
                The list is ordered from your highest risk cancer to your lowest risk cancer."),
                style = "margin-bottom: 25px")
         )),
             
         # output of ranked list of cancers
         fluidRow(column(width = 10,
                         div(tableOutput(outputId = "cancerList"), style = "font-size: 125%"))),
         
         conditionalPanel(condition = "!output.hadColo",
                          fluidRow(column(width = 9,
                                          h6("Note: Consult your doctor to determine how frequently you should receive colonoscopies.")
                          ))),
         
         fluidRow(uiOutput(outputId = "extraCancers")),
         
         conditionalPanel(condition = "input.gene == 'EPCAM' | (input.gene == 'MSH2' & input.gender == 'Male') & output.age < 50",
                          fluidRow(column(width = 9,
                            h6("Although Colorectal Cancer risks without colonoscopies are very high in your case, note that 
                               the vast majority of people with Lynch Syndrome get regular colonoscopies at more
                               frequent rates than the average person. Your chances of getting cancer while following
                               your doctor's recommended colonoscopy schedule will be greatly reduced. Also, due to 
                               limited data, it is unclear how accurate estimates are
                               for those who do not follow regular colonoscopy schedules.")
                          ))
         ),
         
         p(HTML("You can learn more about any of these cancers on this page from Dana-Farber's Lynch Syndrome page:
                 <a href = 'https://www.dana-farber.org/cancer-genetics-and-prevention/syndromes-genes-and-programs/lynch-syndrome-center/'>
                 Click here</a>"),
            style = "margin-top: 0px"),
         
        # tab switching
        h2("CLICK CONTINUE TO PROCEED", style = "margin-top: 25px; margin-bottom: 15px"),
        p("When you are done reading, hit continue."),
        
        actionButton(inputId = "backEnter", 
                     label = "Back",
                     style = "color: white; background-color: #10699B; border-color: #10699B"),
        actionButton(inputId = "goDisplay", 
                     label = "Continue",
                     icon = icon('play'),
                     style = "color: white; background-color: #10699B; border-color: #10699B")
                
       ), # end of tab
       
       ###### Display Options ####
       tabPanel("3. My View",             
         h2("My View", style = "margin-top: 10px,margin-bottom: 25px"),
         
         fluidRow(column(width = 9, 
                         p("Before we look at each cancer in more detail, please tell us how you would like to view this information by 
                           picking your favorite visualization option below. Note, the data you see here are just examples, ",
                           HTML("<i>these are not your risks.</i> <b>Use the drop-down menu below to select option 1, 2, or 3 then 
                                click the continue button at the bottom of the screen.</b>"))
         )),
         
         # choose the display style
         selectInput(inputId = "displayOption", label = h3("Select Your Display Choice:"),
                     choices = c(" ", paste0("Option ",c(1,2,3)))),
          
         fluidRow(
           
           # line graph option
           column(width = 4, align = "center",
                  img(src="line-example-option.png", width = "100%")
           ),
           
           # bar graph option
           column(width = 4, align = "center",
                  img(src="bar-example-option.png", width = "100%")
           )
          ),
         br(),
         br(),
         
         # waffle option
         fluidRow(
           column(width = 8, align = "center",
                  img(src="pgraph-example-option.png", width = "90%")
           )
         ), 
         br(),
         br(),
         
        # tab switching
        h2("CLICK CONTINUE TO PROCEED", style = "margin-top: 25px; margin-bottom: 15px"),
        p("When you have selected a display option in the drop-down the continue button will appear."),
        
        conditionalPanel(condition = "input.displayOption == ' '",
          actionButton(inputId = "backList1", 
                       label = "Back",
                       style = "color: white; background-color: #10699B; border-color: #10699B")
        ),
        
        conditionalPanel(condition = "input.displayOption != ' '",
          actionButton(inputId = "backList", 
                       label = "Back",
                       style = "color: white; background-color: #10699B; border-color: #10699B"),
          actionButton(inputId = "goPlot1", 
                       label = "Continue",
                       icon = icon('play'),
                       style = "color: white; background-color: #10699B; border-color: #10699B")
        )
       ), # end of tab
       
      ###### Plots (no interventions) ####
      tabPanel("4. Visualize My Risk",
        h2("Visualize My Risk", style = "margin-top: 25px; margin-bottom: 20px"),
           
        fluidRow(column(width = 9,
          p(HTML("<b>Below you will see a visual with your cancer risks.</b> The data behind the visuals 
             you will see today came from a rigorous review of scientific data and 
             peer-reviewed articles on Lynch sydrome.")),
          conditionalPanel(condition = "!output.hadColo",
            p(HTML("First, let's start by taking a look at your colorectal cancer (CRC) risk in the 
               display below because it is almost always the highest risk cancer for someone with Lynch. 
               Here you can view your risk for CRC if you get or don't get regular colonscopies. 
               <i>As you can see, colonoscopies provide a great benefit!</i>"))
           ),
          p(HTML("<b>Comparing your risk to someone without LS:</b> 
                 Can you see how your risks compare to someone without Lynch Syndrome? 
             The 'Average Person' being compared in the visual is someone with the same sex 
             and age as you but who does not have Lynch syndrome.")),
        )),
           
           fluidRow(column(width = 9,
             p(HTML("<b>Using this graph:</b>")),
                           
             # instructions for line and bar graphs
             conditionalPanel(condition = "input.displayOption != 'Option 3'",
                              p("To add more cancers to the display, click them on using the checkboxes to the right.")
             ),

             # instructions for waffle
             conditionalPanel(condition = "input.displayOption == 'Option 3'",
                              p("1. Switch between different cancers by selecting them to the right.", 
                                 style = "text-indent: 1%"),
                              p("2. Below the cancer options, you can switch between viewing your lifetime 
                                 risk (by age 85) to viewing your risk in 5 years.", 
                                 style = "text-indent: 1%")
             ),
             
             p(HTML("<b>When you are done</b>, scroll to the bottom and click the Continue button."))
           )),

           # display
           sidebarLayout( position = "right",
                          
              sidebarPanel( width = 3,
                            
                  conditionalPanel(condition = "input.displayOption != 'Option 3'",
                    conditionalPanel(condition = "output.is_mobile_device",
                       radioButtons(inputId = "cancerViewMobile", 
                                    label = h4("Select a cancer:", style = "font-weight: bold;"),
                                    choices = cancer.choices, selected = textOutput("defaultCancer"))
                                     
                    ),
                    conditionalPanel(condition = "!output.is_mobile_device",
                       checkboxGroupInput(inputId = "cancerView", 
                                          label = h4("Select one or more cancers:", style = "font-weight: bold;"),
                                          choices = cancer.choices, selected = textOutput("defaultCancer"))
                                     
                    )
                  ),
                  
                  conditionalPanel(condition = "input.displayOption == 'Option 3'",
                                   
                     radioButtons(inputId = "cancerView2", 
                                  label = h4("Select a cancer:", style = "font-weight: bold;"),
                                  choices = cancer.choices, selected = textOutput("defaultCancer")),
                     radioButtons(inputId = "waffleTime",
                                  label = h4("Select a time horizon:", style = "font-weight: bold;"),
                                  choices = c("In My Lifetime","In the Next 5 Years"), selected = "In My Lifetime")
                  )
              ), # end of sidebarPanel
              
              mainPanel( width = 9,
                         
                 conditionalPanel(condition = "input.displayOption == 'Option 1'",
                                  
                    # show an interactive plotly graph if only one cancer selected
                    conditionalPanel(condition = "output.oneLineCondition", 
                                     uiOutput('oneLine')
                    ),
                    
                    # show a non-interactive grid of graphs if multiple cancers selected (allows for a nicer legend and layout)
                    conditionalPanel(condition = "output.multiLineCondition", 
                                     uiOutput('multiLine')
                    )
                 ),
                 
                 conditionalPanel(condition = "input.displayOption == 'Option 2'",
                    plotOutput(outputId = "barGraph", height = paste0(height,"px"))
                 ),
                 
                 conditionalPanel(condition = "input.displayOption == 'Option 3'",
                                  
                    # adjust size of figure depending on if reduced risk shows
                    conditionalPanel(condition = "output.bigWaffleCRCscreen",
                                     uiOutput(outputId = "wafflePlargeCRCscreen")
                    ),
                    
                    conditionalPanel(condition = "!output.bigWaffleCRCscreen",
                                     uiOutput(outputId = "wafflePsmallCRCscreen")
                    )
                 )
              ) # end of mainPanel
           ), # end of sidebarLayout
           
           conditionalPanel(condition = "!output.hadColo & output.CRCselected",
              fluidRow(column(width = 9,
                h6("Note: Consult your doctor to determine how frequently you should receive colonoscopies."),
              ))
           ),
          
          conditionalPanel(condition = "input.displayOption != 'Option 1'",
              fluidRow(column(width = 9,
                h6("Note: Lifetime risk is assumed to be until age 85."),
              ))
          ),  
           
           conditionalPanel(condition = "input.gene == 'EPCAM' | (input.gene == 'MSH2' & input.gender == 'Male') & output.age < 50",
                            fluidRow(column(width = 9,       
                              h6("Although Colorectal Cancer risks without colonoscopies are very high in your case, note that 
                                 the vast majority of people with Lynch Syndrome get regular colonoscopies at more
                                 frequent rates than the average person. Your chances of getting cancer while following
                                 your doctor's recommended colonoscopy schedule will be greatly reduced. Also, due to 
                                 limited data on people who do not do colonoscopies, it is unclear how accurate estimates are
                                 for those who do not follow regular colonoscopy schedules.")
                              ))
            ),
                
            # tab switching
            # interventions available  
            conditionalPanel(condition = "output.eligCRCint | output.eligENDOint | output.eligOVARint",
                             
                             fluidRow(column(width = 9,
                               h2("CLICK CONTINUE TO PROCEED", style = "margin-top: 25px; margin-bottom: 15px"),
                               p("When you are done on this page, hit continue."),
                               
                                actionButton(inputId = "backDisplay", 
                                             label = "Back",
                                             style = "color: white; background-color: #10699B; border-color: #10699B"),
                                actionButton(inputId = "goIntervention", 
                                             label = "Continue",
                                             icon = icon('play'),
                                             style = "color: white; background-color: #10699B; border-color: #10699B")
                             ))
            ),
        
            # no interventions available
            conditionalPanel(condition = "!output.eligCRCint & !output.eligENDOint & !output.eligOVARint",
                             
                             h2("CLICK CONTINUE TO PROCEED", style = "margin-top: 25px; margin-bottom: 15px"),
                             
                              p("Unfortunately, the current Lynch Syndrome research has not yet discovered a way for you to lower your cancer risks. 
                                You can still create a personalized report though, just click the continue button."),
                              actionButton(inputId = "backDisplay1", 
                                           label = "Back",
                                           style = "color: white; background-color: #10699B; border-color: #10699B"),
                              actionButton(inputId = "jumpReport", 
                                           label = "Continue",
                                           icon = icon('play'),
                                           style = "color: white; background-color: #10699B; border-color: #10699B")
            )
       ), # end of tab
       
       ###### Intervention Options ####
       tabPanel("5. My Risk Reduction Options",
        h2("My Risk Reduction Options", style = "margin-top: 25px; margin-bottom: 20px"),
        ### Cancers with Options
        fluidRow(column(width = 9, 
          p(HTML("<b>Depending on your Lynch gene, sex, and age your customized list of risk reduction options 
             is below.</b> Your options are based on the current Lynch syndrome research available for someone 
             with your profile. As new research becomes available, we will update the tool.")),
                        
          # females with option to lower colorectal and endo
          conditionalPanel(condition = "output.eligCRCint & output.eligENDOint & !output.eligOVARint",
                           p(HTML("You have options to lower your risk of <b>Colorectal Cancer and Endometrial Cancer</b>."))
          ),
          
          # females with option to lower colorectal and ovarian
          conditionalPanel(condition = "output.eligCRCint & !output.eligENDOint & output.eligOVARint",
                           p(HTML("You have options to lower your risk of <b>Colorectal Cancer and Ovarian Cancer</b>."))
          ),
          
          # females with option to lower colorectal, endo, and ovarian
          conditionalPanel(condition = "output.eligCRCint & output.eligENDOint & output.eligOVARint",
                           p(HTML("You have options to lower your risk of <b>Colorectal Cancer, Endometrial Cancer, and Ovarian Cancer</b>."))
          ),
          
          # females with option to lower endo only
          conditionalPanel(condition = "!output.eligCRCint & output.eligENDOint & !output.eligOVARint",
                           p(HTML("You have options to lower your risk of <b>Endometrial Cancer</b>."))
          ),
          
          # females with option to lower ovarian only
          conditionalPanel(condition = "!output.eligCRCint & !output.eligENDOint & output.eligOVARint",
                           p(HTML("You have options to lower your risk of <b>Ovarian Cancer</b>."))
          ),
          
          # females with option to lower endo and ovarian only
          conditionalPanel(condition = "!output.eligCRCint & output.eligENDOint & output.eligOVARint",
                           p(HTML("You have options to lower your risk of <b>Endometrial Cancer and Ovarian Cancer</b>."))
          ),
          
          # Summary sentence for people with only colorectal cancer as an option
          conditionalPanel(condition = "output.eligCRCint & !output.eligENDOint & !output.eligOVARint",
                           p(HTML("You have options to lower your risk of <b>Colorectal Cancer</b>.  Your options are:")))
          )),

        ## List options for Colorectal Cancer
        # start of list of colorectal cancer intervention options, for people with more than one cancer with intervention options
        conditionalPanel(condition = "output.eligCRCint & (output.eligENDOint | output.eligOVARint)",
                         p(HTML("<b>Colorectal Cancer options:</b>"))
        ),
      
        
        ### Specific Options Available 
        
        conditionalPanel(condition = "output.eligCRCint",
          fluidRow(column(width = 9,
                          
            # aspirin option
            conditionalPanel(condition = "output.eligAsp",
              p(HTML("<li><i>Start an aspirin regimen.</i> The risk estimates you will see on the next screen were generated assuming 
                      a 600mg daily dose, however you should consult your doctor on the appropriate 
                      dosage for you.</li>"), style = "margin-left:25px;"),
              conditionalPanel(condition = "output.age >= 83",
                               p(HTML("<li>Aspirin takes at least 3 years to take effect and 
                                       this tool only calculates risks out to age 85. 
                                       It is likely that taking aspirin is beneficial for you but,
                                       the change in risk cannot be shown on the following screens.",
                                       style = "margin-left:200px;")))
            ),
            
            # colonoscopy option
            conditionalPanel(condition = "output.eligCol",
              p(HTML("<li><i>Get regular colonoscopies.</i> Consult your doctor on how frequently you should receive colonoscopies. 
                      The risk estimates you will see on the next screen were generated assuming a colonoscopy is done every three years.</li>"), 
                 style = "margin-left:25px;")
            ),
            
            # lower BMI 
            conditionalPanel(condition = "output.eligBMIcrc",
                 p(HTML("<li><i>Lose weight.</i> Because your BMI is 25 or higher, losing weight will put you closer to the 
                         normal BMI range (less than 25) and will lower your risk of cancer.</li>"), 
                    style = "margin-left:25px;")
            )
          ))
        ),

        fluidRow(column(width = 9,
            # hysterectompy option
            conditionalPanel(condition = "output.eligENDOint",
                             p(HTML("<b>Endometrial Cancer options:</b>")),
                             
                             conditionalPanel(condition = "output.eligBMIendo",
                                              p(HTML("<li><i>Lose weight.</i> Because your BMI is 25 or higher, losing weight will put you closer to the 
                                                      normal BMI range (less than 25) and will lower your risk of cancer.</li>"), 
                                                 style = "margin-left:25px;")
                              ),
                             
                             conditionalPanel(condition = "output.eligHyst",
                                              p(HTML("<li><i>Surgically remove your uterus.</i> You can lower your risk by having your uterus removed before cancer develops.
                                                       This surgery is known as a hysterectomy.</li>"), 
                                                 style = "margin-left:25px;")
                             )
            )
        )),
        
        fluidRow(column(width = 9,
                        
            # oophorectomy option
            conditionalPanel(condition = "output.eligOVARint",
                             p(HTML("<b>Ovarian Cancer options:</b>")),
                             
                             conditionalPanel(condition = "output.eligOoph",
                               p(HTML("<li><i>Surgically remove both your ovaries.</i> You can lower your risk by having both your ovaries removed before cancer develops.
                                        This surgery is known as an oophorectomy."), 
                                  style = "margin-left:25px;"))
            )
        )),
        
        fluidRow(column(width = 9,
                        
          p(HTML("<b>What else can I do to lower my risk?</b>")),
                        
          p(HTML(paste0("There are additional ways to lower your risk for the cancers listed above
                  and more. For example, regular exercise and avoiding tobacco are generally recommended, but it is difficult to measure the exact benefit
                  for these on individuals with Lynch Syndrome due to limited data and research at this time. We are constantly looking for new
                  studies that provide risk reducing options for individuals with Lynch Syndrome to add to this tool. General recommendations for lowering your
                  risk for cancer can be found on the ", a(href = "https://www.cancer.org/latest-news/6-steps-to-help-lower-your-cancer-risk.html",
                                                            "American Cancer Society website",
                                                           target="_blank"),".")),
             style = "margin-left:25px;"),
                        
          p(HTML(paste0("For technical notes on the assumptions used to calculate the impact of risk reducing interventions used in this tool 
                         and for a list of references ",a(href = 'assumptions-and-references.pdf', "click here", target="_blank"),".")),
             style = "margin-top:25px;")
          
        )),
              
        # tab switching
        fluidRow(column(width = 9,
        h2("CLICK CONTINUE TO PROCEED", style = "margin-top: 25px; margin-bottom: 15px"),
        p("When you are done reading, hit continue.")
        )),
        
        actionButton(inputId = "backPlot1", 
                     label = "Back",
                     style = "color: white; background-color: #10699B; border-color: #10699B"),
        actionButton(inputId = "goPlot2", 
                     label = "Continue",
                     icon = icon('play'),
                     style = "color: white; background-color: #10699B; border-color: #10699B")
                
       ), # end of tab
       
       ###### Plots (Interventions) ####
       tabPanel("6. Visualize My Risk Reduction",
          h2("Visualize My Risk Reduction", style = "margin-top: 25px; margin-bottom: 15px"),
          
          fluidRow(column(width = 9,
            p(HTML("<b>Using this graph:</b>"), 
              " The display below will show you how your risks change based on your options."),

            conditionalPanel(condition = "input.displayOption == 'Option 1'",
                             p("1. Select which cancers your want displayed. You can change these at any time.", 
                                style = "text-indent: 1%")
            ),

            conditionalPanel(condition = "input.displayOption != 'Option 1'",
                             p("1. Pick the cancer your want displayed. You can change this at any time.", 
                                style = "text-indent: 1%")
            ),
            
            conditionalPanel(condition = "input.displayOption != 'Option 3'",
              p("2. Turn on the options under 'How can I reduce my risk?' to see how your risks will change.
                 Experiment with different combinations of settings by turning them on and off.",
                 style = "text-indent: 1%")
            ),
            
            conditionalPanel(condition = "input.displayOption == 'Option 3'",
                             p("2. Below the cancer options, you can switch between viewing your lifetime risk to viewing your risk in 5 years.",
                                style = "text-indent: 1%"),
                             p("3. Turn on the options under 'How can I reduce my risk?' to see how your risks will change.
                 Experiment with different combinations of settings by turning them on and off.",
                 style = "text-indent: 1%")
            ),
            
            p(HTML("<b>When you are done</b>, scroll to the bottom and click the Continue button."))
          )),
          
          sidebarLayout( position = "right",
                         
                         sidebarPanel( width = 3,
                                       
                                       conditionalPanel(condition = "input.displayOption != 'Option 3'",
                                          conditionalPanel(condition = "output.is_mobile_device",
                                                           radioButtons(inputId = "cancerViewMobile3", 
                                                                        label = h4("Select a cancer:", style = "font-weight: bold;"),
                                                                        choices = cancer.choices, selected = textOutput("defaultCancer"))
                                                           
                                          ),
                                          conditionalPanel(condition = "!output.is_mobile_device",
                                                           checkboxGroupInput(inputId = "cancerView3", 
                                                                              label = h4("Select one or more cancers:", style = "font-weight: bold;"),
                                                                              choices = cancer.choices, selected = textOutput("defaultCancer"))
                                                           
                                          )
                                       ),
                                       
                                       conditionalPanel(condition = "input.displayOption == 'Option 3'",
                                                        radioButtons(inputId = "cancerView4",
                                                                     label = h4("Select a cancer:", style = "font-weight: bold;"),
                                                                     choices = cancer.choices, selected = textOutput("defaultCancer")),
                                                        radioButtons(inputId = "waffleTime2",
                                                                     label = h4("Select a time horizon:", style = "font-weight: bold;"),
                                                                     choices = c("In My Lifetime","In the Next 5 Years"), selected = "In My Lifetime")
                                                        
                                       ),
                                       
                                       conditionalPanel(condition = "output.eligCRCint",
                                                        h4("How can I lower my risk of Colorectal Cancer?", style = "font-weight: bold;"),
                                                        
                                                         # colonoscopies
                                                        conditionalPanel(condition = "output.eligCol",
                                                           checkboxInput(inputId = "colonoscopy",
                                                                         label = "Regular colonoscopies",
                                                                         value = TRUE),
                                                           
                                                           bsTooltip(id = "colonoscopy",
                                                                     title = "Talk to your doctor to see how often you should get a colonoscopy. This data is based on one every three years.",
                                                                     placement = "bottom")
                                                         ),
                                                         
                                                         # aspirin regimen
                                                         conditionalPanel(condition = "output.eligAsp & output.age < 83",
                                                                          
                                                                          checkboxInput(inputId = "aspirin",
                                                                                        label = "Start aspirin regimen"),
                                                                          
                                                                          bsTooltip(id = "aspirin",
                                                                                    title = "Talk to your doctor to see what dosage, if any, is recommended for you. This data is based on 600MG per day.",
                                                                                    placement = "bottom")
                                                         ),

                                                        # BMI reduction
                                                        conditionalPanel(condition = "output.eligBMIcrc & !output.eligBMIendo",
                                                                         
                                                                         sliderInput(inputId = "lowerBMI.colo.only", 
                                                                                     label = h5("Select how many pounds you want to lose:"),
                                                                                     min = 0, max = 50, value = 0,
                                                                                     width = "100%")
                                                                         
                                                        )
                                       ),
                                       
                                        # endometrial cancer
                                        conditionalPanel(condition = "output.eligENDOint",
                                          
                                          h4("How can I lower my risk of Endometrial Cancer?", style = "font-weight: bold;"),
                                          
                                          # hysterectomy
                                          conditionalPanel(condition = "output.eligHyst",
                                                           
                                                           checkboxInput(inputId = "hysterectomy.2",
                                                                         label = "Remove uterus",
                                                                         FALSE)
                                          ),
                                          
                                          # BMI reduction
                                          conditionalPanel(condition = "!output.eligBMIcrc & output.eligBMIendo",
                                                           
                                                           sliderInput(inputId = "lowerBMI.endo.only", 
                                                                       label = h5("Select how many pounds you want to lose:"),
                                                                       min = 0, max = 50, value = 0,
                                                                       width = "100%")
                                                           
                                          )
                                        ),
                                        
                                        
                                        # BMI reduction for both CRC and Endo
                                        conditionalPanel(condition = "output.eligBMIcrc & output.eligBMIendo",
                                                         
                                                         h4("How can I lower my risk of Colorectal and Endometrial Cancer?", style = "font-weight: bold;"),
                                                         
                                                         sliderInput(inputId = "lowerBMI.endo.colo", 
                                                                     label = h5("Select how many pounds you want to lose:"),
                                                                     min = 0, max = 50, value = 0,
                                                                     width = "100%")
                                                         
                                        ),
                                          
                                        # ovarian cancer/oophorectomy
                                        conditionalPanel(condition = "output.eligOVARint",
                                                         h4("How can I lower my risk of Ovarian Cancer?", style = "font-weight: bold;"),
                                                         conditionalPanel(condition = "output.eligOoph",
                                                           checkboxInput(inputId = "oophorectomy.2",
                                                                         label = "Remove both ovaries",
                                                                         FALSE))
                                        )
                         ), # end of sidebarPanel
                         
                         mainPanel( width = 9,
                                    
                                    conditionalPanel(condition = "input.displayOption == 'Option 1'",
                                                     
                                                     # show an interactive plotly graph if only one cancer selected
                                                     conditionalPanel(condition = "output.RoneLineCondition",
                                                                      uiOutput('RoneLine')
                                                     ),
                                                     
                                                     # show a non-interactive grid of graphs if multiple cancers selected (allows for a nicer legend and layout)
                                                     conditionalPanel(condition = "output.RmultiLineCondition",
                                                                      uiOutput('RmultiLine')
                                                     )
                                    ),
                                    
                                    conditionalPanel(condition = "input.displayOption == 'Option 2'",
                                                     plotOutput(outputId = "RbarGraph", height = paste0(height,"px"))
                                    ),
                                    
                                    conditionalPanel(condition = "input.displayOption == 'Option 3'",
                                                     plotOutput(outputId = "waffleI", height = paste0(w.display.height.rr,"px"))
                                    )
                         ) # end of mainPanel
          ), # end of sidebarLayout
          
          conditionalPanel(condition = "!output.hadColo & output.CRCselected3",
           fluidRow(column(width = 9,
              conditionalPanel(condition = "input.colonoscopy",
                h6("Note: Consult your doctor to determine how frequently you should receive colonoscopies.")
              ),
              conditionalPanel(condition = "input.aspirin",
                h6("Note: Consult your doctor to determine if you should be on an aspirin regimen and the dosage.")
              )
           ))
          ),
          
          conditionalPanel(condition = "input.displayOption != 'Option 1'",
             fluidRow(column(width = 9,
                h6("Note: Lifetime risk is assumed to be until age 85."),
             ))
          ), 
          
          conditionalPanel(condition = "input.gene == 'EPCAM' | (input.gene == 'MSH2' & input.gender == 'Male') & output.age < 50",
                           fluidRow(column(width = 9,
                                   h6("Although Colorectal Cancer risks without colonoscopies are very high in your case, note that 
                           the vast majority of people with Lynch Syndrome get regular colonoscopies at more
                           frequent rates than the average person. Your chances of getting cancer while following
                           your doctor's recommended colonoscopy schedule will be greatly reduced. Also, due to 
                           limited data on people who do not do colonoscopies, it is unclear how accurate estimates are
                           for those who do not follow regular colonoscopy schedules.")
                           ))
          ),
        
        # tab switching
        h2("CLICK CONTINUE TO PROCEED", style = "margin-top: 25px; margin-bottom: 15px"),
        p("When you are ready to to move on, hit continue."),
        actionButton(inputId = "backIntervention", 
                     label = "Back",
                     style = "color: white; background-color: #10699B; border-color: #10699B"),
        actionButton(inputId = "goReport", 
                     label = "Continue",
                     icon = icon('play'),
                     style = "color: white; background-color: #10699B; border-color: #10699B")
                
       ), # end of tab
       
       
       ###### Preview Report ####
       tabPanel("7. My Report",
         h2("My Report", style = "margin-top: 25px; margin-bottom: 20px"),

        fluidRow(column(width = 9,
          p(HTML("This screen will allow you to <b>create a customized 
             cancer risk report</b> <i>with the cancers and risk reduction 
             options you want</i>. You will be able to <b>download the 
             report or email it to yourself, your doctor, or genetic counselor.</b>")),
          p(HTML("<b>Using this graph:</b>")),
          conditionalPanel(condition = "input.displayOption != 'Option 3'",
             p("1. Select the cancers and risk reduction options you want on your report.")
          ),
          conditionalPanel(condition = "input.displayOption == 'Option 3'",
             p("1. Select the cancers, future age, and risk reduction options you want on your report.")
          ),
          p("2. Click the download button at the bottom. The report will open automatically after it downloads.")
        )),
        
        ## Display
        sidebarLayout( position = "right",
                       sidebarPanel( width = 3,
                                     conditionalPanel(condition = "input.displayOption != 'Option 3'",
                                        conditionalPanel(condition = "output.is_mobile_device",
                                                         radioButtons(inputId = "cancerViewMobile5", 
                                                                      label = h4("Select a cancer:", style = "font-weight: bold;"),
                                                                      choices = cancer.choices, selected = textOutput("defaultCancer"))
                                                         
                                        ),
                                        conditionalPanel(condition = "!output.is_mobile_device",
                                                         checkboxGroupInput(inputId = "cancerView5", 
                                                                            label = h4("Select one or more cancers:", style = "font-weight: bold;"),
                                                                            choices = cancer.choices, selected = textOutput("defaultCancer"))
                                                         
                                        )
                                     ),
                                     
                                     conditionalPanel(condition = "input.displayOption == 'Option 3'",
                                                      radioButtons(inputId = "cancerView6",
                                                                   label = h4("Select a cancer:", style = "font-weight: bold;"),
                                                                   choices = cancer.choices, selected = textOutput("defaultCancer")),
                                                      radioButtons(inputId = "waffleTime3",
                                                                   label = h4("Select a time horizon:", style = "font-weight: bold;"),
                                                                   choices = c("In My Lifetime","In the Next 5 Years"), selected = "In My Lifetime")
                                                      
                                     ),
                                     
                                     conditionalPanel(condition = "output.eligCRCint",
                                                      h4("How can I lower my risk of Colorectal Cancer?", style = "font-weight: bold;"),
                                                      
                                                      # colonoscopies
                                                      conditionalPanel(condition = "output.eligCol",
                                                        checkboxInput(inputId = "colonoscopy2",
                                                                      label = "Regular colonoscopies",
                                                                      value = TRUE),
                                                        
                                                        bsTooltip(id = "colonoscopy2",
                                                                  title = "Talk to your doctor to see how often you should get a colonoscopy. This data is based on one every three years.",
                                                                  placement = "bottom")
                                                      ),
                                                      
                                                      # aspirin regimen
                                                      conditionalPanel(condition = "output.eligAsp & output.age < 83",
                                                                       
                                                                       checkboxInput(inputId = "aspirin2",
                                                                                     label = "Start aspirin regimen"),
                                                                       
                                                                       bsTooltip(id = "aspirin2",
                                                                                 title = "Talk to your doctor to see what dosage, if any, is recommended for you. This data is based on 600MG per day.",
                                                                                 placement = "bottom")
                                                      ),
                                                      
                                                      # BMI reduction
                                                      conditionalPanel(condition = "output.eligBMIcrc & !output.eligBMIendo",
                                                                       
                                                                       sliderInput(inputId = "lowerBMI2.colo.only", 
                                                                                   label = h5("Select how many pounds you want to lose:"),
                                                                                   min = 0, max = 50, value = 0,
                                                                                   width = "100%")
                                                                       
                                                      )
                                     ),
                                                      
                                    # endometrial cancer
                                    conditionalPanel(condition = "output.eligENDOint",
                                                     
                                                     h4("How can I lower my risk of Endometrial Cancer?", style = "font-weight: bold;"),
                                                     
                                                     # hysterectomy
                                                     conditionalPanel(condition = "output.eligHyst",
                                                                      
                                                                      checkboxInput(inputId = "hysterectomy.3",
                                                                                    label = "Remove uterus",
                                                                                    FALSE)
                                                     ),
                                                     
                                                     # BMI reduction
                                                     conditionalPanel(condition = "!output.eligBMIcrc & output.eligBMIendo",
                                                                      
                                                                      sliderInput(inputId = "lowerBMI2.endo.only", 
                                                                                  label = h5("Select how many pounds you want to lose:"),
                                                                                  min = 0, max = 50, value = 0,
                                                                                  width = "100%")
                                                                      
                                                     )
                                    ),
                                    
                                    # BMI reduction for both CRC and Endo
                                    conditionalPanel(condition = "output.eligBMIcrc & output.eligBMIendo",
                                                     
                                                     h4("How can I lower my risk of Colorectal and Endometrial Cancer?", style = "font-weight: bold;"),
                                                     
                                                     sliderInput(inputId = "lowerBMI2.endo.colo", 
                                                                 label = h5("Select how many pounds you want to lose:"),
                                                                 min = 0, max = 50, value = 0,
                                                                 width = "100%")
                                                     
                                    ),
                                    
                                    # oophorectomy
                                    conditionalPanel(condition = "output.eligOVARint",
                                                     h4("How can I lower my risk of Ovarian Cancer?", style = "font-weight: bold;"),
                                                     conditionalPanel(condition = "output.eligOoph",
                                                       checkboxInput(inputId = "oophorectomy.3",
                                                                     label = "Remove both ovaries",
                                                                     FALSE))
                                    )
                       ), # end of sidebarPanel
                       
                       mainPanel( width = 9,
                                  
                                  conditionalPanel(condition = "input.displayOption == 'Option 1'",
                                                   
                                                   # show an interactive plotly graph if only one cancer selected
                                                   conditionalPanel(condition = "output.DoneLineCondition",
                                                                    uiOutput('DoneLine')
                                                   ),
                                                   
                                                   # show a non-interactive grid of graphs if multiple cancers selected (allows for a nicer legend and layout)
                                                   conditionalPanel(condition = "output.DmultiLineCondition",
                                                                    uiOutput('DmultiLine')
                                                   )
                                  ),
                                  
                                  conditionalPanel(condition = "input.displayOption == 'Option 2'",
                                                   plotOutput(outputId = "DbarGraph", height = paste0(height,"px"))
                                  ),
                                  
                                  conditionalPanel(condition = "input.displayOption == 'Option 3'",
                                                   
                                                   # adjust size of figure depending on if reduced risk shows
                                                   conditionalPanel(condition = "output.bigWaffle",
                                                      uiOutput(outputId = "wafflePlarge")
                                                   ),
                                                   
                                                   conditionalPanel(condition = "!output.bigWaffle",
                                                      uiOutput(outputId = "wafflePsmall")
                                                   )
                                  )
                       ) # end of mainPanel
        ), # end of sidebarLayout
        
        conditionalPanel(condition = "!output.hadColo & output.CRCselected5",
         fluidRow(column(width = 9,
           conditionalPanel(condition = "input.colonoscopy2",
                            h6("Note: Consult your doctor to determine how frequently you should receive colonoscopies.")
           ),
           conditionalPanel(condition = "input.aspirin2",
                            h6("Note: Consult your doctor to determine if you should be on an aspirin regimen and the dosage.")
           )
         ))
        ),
        
        conditionalPanel(condition = "input.displayOption != 'Option 1'",
           fluidRow(column(width = 9,
              h6("Note: Lifetime risk is assumed to be until age 85."),
           ))
        ), 
        
        conditionalPanel(condition = "input.gene == 'EPCAM' | (input.gene == 'MSH2' & input.gender == 'Male') & output.age < 50",
                         fluidRow(column(width = 9,
                          h6("Although Colorectal Cancer risks without colonoscopies are very high in your case, note that 
                             the vast majority of people with Lynch Syndrome get regular colonoscopies at more
                             frequent rates than the average person. Your chances of getting cancer while following
                             your doctor's recommended colonoscopy schedule will be greatly reduced. Also, due to 
                             limited data on people who do not do colonoscopies, it is unclear how accurate estimates are
                             for those who do not follow regular colonoscopy schedules.")
                         ))
        ),
        
        ###### Download ####
        h2("Download and Email Your Report"),
        fluidRow(column(width = 9, 
          p("Use the buttons below to download and/or email your report."),
          p("We recommend that you share this report with your doctor and genetic counselor. 
                If you want, you can also share your report with family members or trusted friends.")
        )),
        
        # only show download options for line and bar graphs if at least one cancer view is selected
        conditionalPanel(condition = "output.multiLineCondition | output.oneLineCondition",
                         
                         # unique download for each display type
                         conditionalPanel(condition = "input.displayOption == 'Option 1'",
                                          downloadButton(outputId = "report_line",
                                                         label = "Download My Personal Report",
                                                         icon = icon("download"),
                                                         style = "color: white; background-color: #10699B; border-color: #10699B")
                         ),
                         conditionalPanel(condition = "input.displayOption == 'Option 2'",
                                          downloadButton("report_bar",
                                                         label = "Download My Personal Report",
                                                         icon = icon("download"),
                                                         style = "color: white; background-color: #10699B; border-color: #10699B")
                         )
        ),
        
        conditionalPanel(condition = "input.displayOption == 'Option 3'",
                         downloadButton("report_waffle",
                                        label = "Download My Personal Report",
                                        icon = icon("download"),
                                        style = "color: white; background-color: #10699B; border-color: #10699B")
        ),
        
        ###### Email Report ####
        br(),
        textInput(inputId = "reportEmail", 
                  label = h4("Send report to this e-mail address:")),
        conditionalPanel(condition = "!output.goodReportEmail",
                         h5("You must enter a valid email address.", style = "color: red") 
        ),
        actionButton(inputId = "emailReport",
                     label = "Email Report",
                     icon = icon("envelope"),
                     style = "color: white; background-color: #10699B; border-color: #10699B"),
        conditionalPanel(condition = "output.sentReport",
                         h4("Email sent!", style = "color: blue") 
        ),
        
        h2("CLICK CONTINUE TO PROCEED", style = "margin-top: 25px; margin-bottom: 15px"),
        p("When you are ready to to move on, hit continue."),
                
        # tab switching
        conditionalPanel(condition = "output.eligCRCint | output.eligENDOint | output.eligOVARint",
           actionButton(inputId = "backPlot2", 
                        label = "Back",
                        style = "color: white; background-color: #10699B; border-color: #10699B"),
           actionButton(inputId = "goShare1", 
                        label = "Continue",
                        icon = icon("play"),
                        style = "color: white; background-color: #10699B; border-color: #10699B")
        ),
        
        # no interventions available
        conditionalPanel(condition = "!output.eligCRCint & !output.eligENDOint & !output.eligOVARint",
           actionButton(inputId = "backPlot1_1", 
                        label = "Back",
                        style = "color: white; background-color: #10699B; border-color: #10699B"),
           actionButton(inputId = "goShare2", 
                        label = "Continue",
                        icon = icon("play"),
                        style = "color: white; background-color: #10699B; border-color: #10699B")
        )
       ), # end of tab
       
        ###### Share ####
        tabPanel(title = "8. Sharing",
          h2("Help your Family by Sharing", style = "margin-bottom: 15px; margin-top: 30px;"),
          fluidRow(column(width = 9,
            h3("Did you know:", style = "margin-bottom: 10px"),
            fluidRow(column(width = 10, offset = 1,
              p(HTML("1. Lynch syndrome cancers are preventable and can be caught early... 
                 <i>if the person knows they have Lynch syndrome.</i> 
                 Catching cancers early means saving lives.")),
              p(HTML("<b>2. Only 1 in 100 people with Lynch syndrome know they have it.</b>")),
              p("3. Your parents, siblings, and kids have a 50% chance of having Lynch; 
                 your grandparents, aunts, uncles, cousins, and grandchildren are also at risk.")
            )),
            p(HTML("<b>Sharing your Lynch diagnosis with your family can save their lives.</b> 
               Almost all people with Lynch syndrome share their diagnosis with their parents, siblings, and children."),
               style = "margin-top: 10px"),
            h3("But...", style = "margin-bottom: 20px"),
            p(HTML("<i>Some people never tell their other relatives.</i>"), 
               style = "margin-bottom: 20px"),
            h3("Save a Life", style = "margin-bottom: 20px"),
            p("You could save the life of a family member if you choose to share your diagnosis."),
            p(HTML("Your doctor can help you make a plan to tell your family or you can use a website like Kin Talk 
                    <a href = 'https://kintalk.org/' id = 'redirect' target = '_blank'>
                    (kintalk.org)</a>. ")),
            p(HTML("You can also <b>share the MyLynch website with your family using the buttons below</b>. 
               These buttons will only share the link to the website, not your personal information."),
               style = "margin-bottom: 25px")
          )),
          
          # large share buttons
          fluidRow(column(width = 12, offset = 1,
            div(style = "display:inline-block; margin-top:15px;",
              actionButton("twitter_share1",
                           label = "",
                           icon = icon("twitter"),
                           onclick = sprintf("window.open('%s')", twitter.url),
                           style = "font-size: 400%; color: #1DA1F2",
                           width = "100px"),
              actionButton("facebook_share1",
                           label = "",
                           icon = icon("facebook"),
                           onclick = sprintf("window.open('%s')", facebook.url),
                           style = "font-size: 400%; color: #4267B2",
                           width = "100px"),
              actionButton("linkedin_share1",
                           label = "",
                           icon = icon("linkedin"),
                           onclick = sprintf("window.open('%s')", linkedin.url),
                           style = "font-size: 400%; color: #0072B1",
                           width = "100px"),
              actionButton("whatsapp_share1",
                           label = "",
                           icon = icon("whatsapp"),
                           onclick = sprintf("window.open('%s')", whatsapp.url),
                           style = "font-size: 400%; color: #25D366",
                           width = "100px"),
              actionButton("email_share1",
                           label = "",
                           icon = icon("envelope"),
                           style = "font-size: 400%; color: grey",
                           width = "100px"),
              
              # pop-up dialog box to enter email when share button is clicked
              bsModal(id = "emailShareModal1",
                      title = "Enter an Email Address",
                      trigger = "email_share1",
                      textInput(inputId = "shareName1",
                                label = "Your name (optional):"),
                      textInput(inputId = "shareEmail1",
                                label = "Recipient's email address:"),
                      conditionalPanel(condition = "!output.goodShareEmail1",
                                       h5("You must enter a valid email address.", style = "color: red") 
                      ),
                      actionButton(inputId = "shareEmail1Go",
                                   label = "Send Email"),
                      conditionalPanel(condition = "output.sentShareEmail1",
                                       h4("Email Sent!", style = "color: blue") 
                      )
              )
            )
          )),
          
          # tab switching
          fluidRow(column(width = 9,
            h3("What did you think of MyLych?", style = "margin-top: 25px;"),
            p(HTML("Please give us your feedback to help us improve. <a href = 'https://redcap.partners.org/redcap/surveys/?s=XMFMNW9TJF8LLWR3' id = 'redirect' target='_blank'>
                    Click here to fill our a brief user survey. </a>")),
            h3("You have finished!"),
            p("Thank you for using this tool; we hope you found it helpful. 
               Remember to share your personalized report with your doctor and genetic counselor. 
               You can now return to any of the previous screens to review your information 
               and make updates.", style = "margin-bottom: 25px;"),
            actionButton(inputId = "backReport", 
                         label = "Back",
                         style = "color: white; background-color: #10699B; border-color: #10699B"),
            actionButton(inputId = "startOver", 
                         label = "Back to First Screen",
                         style = "color: white; background-color: #10699B; border-color: #10699B")
          ))
        ) # end of tab
     ), # end of hidden calculator tabs
     
     ##### Tags ####
     
     # automatically go to top of tab when switching with buttons
     tags$script(" $(document).ready(function () {
       $('#goEnter').on('click', function (e) {
        window.scrollTo(0, 0)
             });
             });"
     ),
     tags$script(" $(document).ready(function () {
       $('#backInstructions1').on('click', function (e) {
        window.scrollTo(0, 0)
             });
             });"
     ),
     tags$script(" $(document).ready(function () {
       $('#backInstructions').on('click', function (e) {
        window.scrollTo(0, 0)
             });
             });"
     ),
     tags$script(" $(document).ready(function () {
       $('#goList').on('click', function (e) {
        window.scrollTo(0, 0)
             });
             });"
     ),
     tags$script(" $(document).ready(function () {
       $('#backEnter').on('click', function (e) {
        window.scrollTo(0, 0)
             });
             });"
     ),
     tags$script(" $(document).ready(function () {
       $('#goDisplay').on('click', function (e) {
        window.scrollTo(0, 0)
             });
             });"
     ),
     tags$script(" $(document).ready(function () {
       $('#backList').on('click', function (e) {
        window.scrollTo(0, 0)
             });
             });"
     ),
     tags$script(" $(document).ready(function () {
       $('#backList1').on('click', function (e) {
        window.scrollTo(0, 0)
             });
             });"
     ),
     tags$script(" $(document).ready(function () {
       $('#goPlot1').on('click', function (e) {
        window.scrollTo(0, 0)
             });
             });"
     ),
     tags$script(" $(document).ready(function () {
       $('#backDisplay').on('click', function (e) {
        window.scrollTo(0, 0)
             });
             });"
     ),
     tags$script(" $(document).ready(function () {
       $('#backDisplay1').on('click', function (e) {
        window.scrollTo(0, 0)
             });
             });"
     ),
     tags$script(" $(document).ready(function () {
       $('#goIntervention').on('click', function (e) {
        window.scrollTo(0, 0)
             });
             });"
     ),
     tags$script(" $(document).ready(function () {
       $('#backPlot1').on('click', function (e) {
        window.scrollTo(0, 0)
             });
             });"
     ),
     tags$script(" $(document).ready(function () {
       $('#backPlot1_1').on('click', function (e) {
        window.scrollTo(0, 0)
             });
             });"
     ),
     tags$script(" $(document).ready(function () {
       $('#goPlot2').on('click', function (e) {
        window.scrollTo(0, 0)
             });
             });"
     ),
     tags$script(" $(document).ready(function () {
       $('#backIntervention').on('click', function (e) {
        window.scrollTo(0, 0)
             });
             });"
     ),
     tags$script(" $(document).ready(function () {
       $('#goReport').on('click', function (e) {
        window.scrollTo(0, 0)
             });
             });"
     ),
     tags$script(" $(document).ready(function () {
       $('#backPlot2').on('click', function (e) {
        window.scrollTo(0, 0)
             });
             });"
     ),
     tags$script(" $(document).ready(function () {
       $('#goShare1').on('click', function (e) {
        window.scrollTo(0, 0)
             });
             });"
     ),
     tags$script(" $(document).ready(function () {
       $('#goShare2').on('click', function (e) {
        window.scrollTo(0, 0)
             });
             });"
     ),
     tags$script(" $(document).ready(function () {
       $('#startOver').on('click', function (e) {
        window.scrollTo(0, 0)
             });
             });"
     ),
     tags$script(" $(document).ready(function () {
       $('#jumpReport').on('click', function (e) {
        window.scrollTo(0, 0)
             });
             });"
     )
    ) # end of My Cancer Risk tab
  ), # end of navbarPage
  
  # automatically go to top of tab when selecting a tab
  tags$script(" $(document).ready(function () {
         $('#navbarTabs a[data-toggle=\"tab\"]').on('click', function (e) {
          window.scrollTo(0, 0)
               });
               });"
  ),
  tags$script(" $(document).ready(function () {
         $('#getStarted').on('click', function (e) {
          window.scrollTo(0, 0)
               });
               });"
  ),
  
  ##### Footer ####
  # white space
  fluidRow(column(width = 12, plotOutput(outputId = "whitespace", height = "100px"))),
  fluidRow(column(width = 9,
    h4(HTML("<b>DISCLAIMER:</b>")),
    p("This website is provided for informational purposes only. The content is 
      not intended as a substitute for professional medical advice, diagnosis, or 
      treatment. Always seek the advice of your doctor or other qualified health 
      provider with any questions you may have regarding your personal health or 
      medical condition."),
    p("The content and design of this website are protected by U.S. and international 
      copyright laws. This website or any portion thereof may not be reproduced, 
      distributed, altered, or used in any manner, beyond your own personal and 
      noncommercial use, without prior written consent from Dana-Farber Cancer Institute."),
    p(a(href = 'terms-and-conditions.pdf', "Terms and Conditions", target="_blank")),
    p(a(href = 'privacy-policy.pdf', "Privacy Policy", target="_blank")),
    p(HTML(paste0("For technical notes on the assumptions used to calculate the 
                  impact of risk reducing interventions used in this tool and for 
                  a list of references ",
                  a(href = 'assumptions-and-references.pdf', "click here", target="_blank"),".")))
  )),
  
  img(src="dana-farber-website-footer.PNG")
  
) # end of UI


#### Server ####
server <- function(input, output, session) {
  
  #### Tab Switching ####
  output$progress <- renderImage({
    tabs <- c("Instructions", "1. My Profile", "2. Possible Cancers", "3. My View", 
              "4. Visualize My Risk", "5. My Risk Reduction Options", 
              "6. Visualize My Risk Reduction", "7. My Report", "8. Sharing")
    selectedTab <- which(tabs == input$calculatorTabs) - 1
    imgFile <- paste0("./www/progress-",selectedTab,".png")
    list(src = imgFile,
         contentType = 'image/png',
         width = 800)
  }, deleteFile = F)
  
  observeEvent(list(input$goShare1, input$goShare2),{
    updateTabsetPanel(session, "calculatorTabs",
                      selected = "8. Sharing")
  })
  
  observeEvent(list(input$goReport, input$backReport, input$jumpReport),{
    updateTabsetPanel(session, "calculatorTabs",
                      selected = "7. My Report")
  })
  
  observeEvent(list(input$goPlot2, input$backPlot2),{
    updateTabsetPanel(session, "calculatorTabs",
                      selected = "6. Visualize My Risk Reduction")
  })
  
  observeEvent(list(input$goIntervention, input$backIntervention),{
    updateTabsetPanel(session, "calculatorTabs",
                      selected = "5. My Risk Reduction Options")
  })
  
  observeEvent(list(input$goPlot1, input$backPlot1, input$backPlot1_1),{
    updateTabsetPanel(session, "calculatorTabs",
                      selected = "4. Visualize My Risk")
  })
  
  observeEvent(list(input$goDisplay,input$backDisplay, input$backDisplay1),{
    updateTabsetPanel(session, "calculatorTabs",
                      selected = "3. My View")
  })
  
  observeEvent(list(input$goList, input$backList, input$backList1),{
    updateTabsetPanel(session, "calculatorTabs",
                      selected = "2. Possible Cancers")
  })
  
  observeEvent(list(input$backEnter, input$goEnter),{
    updateTabsetPanel(session, "calculatorTabs",
                      selected = "1. My Profile")
  })
  
  observeEvent(list(input$backInstructions, input$backInstructions1, input$startOver),{
    updateTabsetPanel(session, "calculatorTabs",
                      selected = "Instructions")
  })
  
  observeEvent(input$getStarted,{
    updateTabsetPanel(session, "navbarTabs",
                      selected = "Get My Cancer Risks")
  })
  
  #### Share Tool via Email ####
  # for small share button in top right corner
  sentShareEmail <- reactiveVal(FALSE)
  observeEvent(input$email_share,{
    sentShareEmail(FALSE)
  })
  observeEvent(list(input$shareEmailGo),{
    if(checkNewEmail(input$shareEmail) == "Success"){
      emailUser(recipientEmail = input$shareEmail,
                senderName = input$shareName,
                emailType = "shareWebsite")
      sentShareEmail(TRUE)
      updateTextInput(session, "shareEmail", value = "")
      updateTextInput(session, "shareName", value = "")
    }
  })
  output$sentShareEmail <- reactive({ sentShareEmail() })
  outputOptions(output, 'sentShareEmail', suspendWhenHidden = FALSE)
  
  # for large share button on the dedicated share page
  sentShareEmail1 <- reactiveVal(FALSE)
  observeEvent(input$email_share1,{
    sentShareEmail1(FALSE)
  })
  observeEvent(list(input$shareEmail1Go),{
    if(checkNewEmail(input$shareEmail1) == "Success"){
      emailUser(recipientEmail = input$shareEmail1, 
                senderName = input$shareName1,
                emailType = "shareWebsite")
      sentShareEmail1(TRUE)
      updateTextInput(session, "shareEmail1", value = "")
      updateTextInput(session, "shareName1", value = "")
    }
  })
  output$sentShareEmail1 <- reactive({ sentShareEmail1() })
  outputOptions(output, 'sentShareEmail1', suspendWhenHidden = FALSE)
  
  #### Tool Tips and Messages ####
  addTooltip(session,
             id = "gender", 
             title = "Enter your sex assigned at birth. 
                   If these two categories do not adequately describe your sex at birth, 
                   consult your doctor on which option to select.",
             placement = "right")
  addTooltip(session,
             id = "gene", 
             title = "If you do not know your Lynch gene, ask your doctor and come back 
             to the tool once you know it.",
             placement = "right")
  addTooltip(session,
             id = "heightFT", 
             title = "Your height and weight will be used to calculate your Body Mass Index (BMI). 
             BMI helps determine your risks and what your options are for lowering those risks.",
             placement = "right")
  addTooltip(session,
             id = "heightIN", 
             title = "Your height and weight will be used to calculate your Body Mass Index (BMI). 
             BMI helps determine your risks and what your options are for lowering those risks.",
             placement = "right")
  addTooltip(session,
             id = "weight", 
             title = "Your height and weight will be used to calculate your Body Mass Index (BMI). 
             BMI helps determine your risks and what your options are for lowering those risks.",
             placement = "right")
  addTooltip(session,
             id = "hadCancer", 
             title = "This tool calculates your risk for new cancers only. Any cancers you enter here 
             will be removed from the analysis. Consult your doctor about the chances of any of your
             previous cancers returning.",
             placement = "right")
  addTooltip(session,
             id = "displayOption", 
             title = "You can come back and change this at any time.",
             placement = "right")
  addTooltip(session,
             id = "twitter_share", 
             title = "Share the link to the tool only (no personal information) on Twitter.",
             placement = "bottom")
  addTooltip(session,
             id = "facebook_share", 
             title = "Share the link to the tool only (no personal information) on Facebook.",
             placement = "bottom")
  addTooltip(session,
             id = "linkedin_share", 
             title = "Share the link to the tool only (no personal information) on LinkedIn.",
             placement = "bottom")
  addTooltip(session,
             id = "whatsapp_share", 
             title = "Share the link to the tool only (no personal information) via WhatsApp.",
             placement = "bottom")
  addTooltip(session,
             id = "email_share", 
             title = "Share the link to the tool only (no personal information) via email.",
             placement = "bottom")
  addTooltip(session,
             id = "shareName",
             title= "This is to let the recipient know who sent the link. 
             If you leave it blank, it will be anonymous.",
             placement = "right")
  addTooltip(session,
             id = "shareName1",
             title= "This is to let the recipient know who sent the link. 
             If you leave it blank, it will be anonymous.",
             placement = "right")
  
  # check if app cannot make an estimate because user had every possible cancer and/or surgery
  noEstimate <- reactiveVal(FALSE)
  observeEvent(list(input$gender, input$gene), {
    
    if(input$gender != " " & input$gene != " "){
      
      # get possible cancers
      if(input$gender == "Male"){
        cn.gn.mat <- pp.male.gc.assoc
      } else {
        cn.gn.mat <- pp.female.gc.assoc
      }
      cn.gn.vec <- cn.gn.mat[, grep(pattern = input$gene, colnames(cn.gn.mat), value = T)]
      names(cn.gn.vec) <- rownames(cn.gn.mat)
      all.cancers <- names(cn.gn.vec)[cn.gn.vec]
  
      # remove cancers if prophylactic surgery already occurred
      if(input$oophorectomy){ all.cancers <- all.cancers[which(all.cancers != "Ovarian")] }
      if(input$hysterectomy){ all.cancers <- all.cancers[which(all.cancers != "Endometrial")] }
  
      if(length(all.cancers) == length(input$previousCancers)){
        noEstimate(TRUE)
      } else {
        noEstimate(FALSE)
      }
    }
  })
  output$noEstimate <- reactive({ noEstimate() })
  outputOptions(output, 'noEstimate', suspendWhenHidden = FALSE)
  
  #### Pre-process user inputs #####
  
  # error proof current age
  goodAge <- reactiveVal(FALSE)
  cur.age <- reactiveVal(25)
  observeEvent(input$age, {
    if(!is.numeric(input$age)){
      goodAge(FALSE)
      cur.age(25)
    } else {
      goodAge(TRUE)
      cur.age(input$age)
    }
  })
  output$age <- renderText({ cur.age() })
  output$goodAge1 <- renderText({ goodAge() })
  output$age <- reactive({ cur.age() })
  output$goodAge <- reactive({ goodAge() })
  outputOptions(output, 'age', suspendWhenHidden = FALSE)
  outputOptions(output, 'goodAge', suspendWhenHidden = FALSE)
  
  # error proof weight
  goodWeight <- reactiveVal(FALSE)
  cur.weight <- reactiveVal(100)
  observeEvent(input$weight, {
    if(!is.numeric(input$weight)){
      goodWeight(FALSE)
      cur.weight(100)
    } else {
      goodWeight(TRUE)
      cur.weight(input$weight)
    }
  })
  output$goodWeight <- reactive({ goodWeight() })
  outputOptions(output, 'goodWeight', suspendWhenHidden = FALSE)
  
  # error proof report email
  goodReportEmail <- reactiveVal(FALSE)
  observeEvent(input$reportEmail, {
    checkReportEmail <- checkNewEmail(input$reportEmail)
    if(checkReportEmail == "Success"){
      goodReportEmail(TRUE)
    } else if(checkReportEmail == "Fail"){
      goodReportEmail(FALSE)
    }
  })
  output$goodReportEmail <- reactive({ goodReportEmail() })
  outputOptions(output, 'goodReportEmail', suspendWhenHidden = FALSE)
  
  # error proof share email (small buttons)
  goodShareEmail <- reactiveVal(FALSE)
  observeEvent(input$shareEmail, {
    checkShareEmail <- checkNewEmail(input$shareEmail)
    if(checkShareEmail == "Success"){
      goodShareEmail(TRUE)
    } else if(checkShareEmail == "Fail"){
      goodShareEmail(FALSE)
    }
  })
  output$goodShareEmail <- reactive({ goodShareEmail() })
  outputOptions(output, 'goodShareEmail', suspendWhenHidden = FALSE)
  
  # error proof share email (large buttons)
  goodShareEmail1 <- reactiveVal(FALSE)
  observeEvent(input$shareEmail1, {
    checkShareEmail <- checkNewEmail(input$shareEmail1)
    if(checkShareEmail == "Success"){
      goodShareEmail1(TRUE)
    } else if(checkShareEmail == "Fail"){
      goodShareEmail1(FALSE)
    }
  })
  output$goodShareEmail1 <- reactive({ goodShareEmail1() })
  outputOptions(output, 'goodShareEmail1', suspendWhenHidden = FALSE)
  
  # set age range for subsetting based on user input
  age.range <- reactive({ seq(cur.age(),max.age) })
  
  # oophorectomy: has the user had one? and pass back to US
  had.ooph <- reactive({ if(input$oophorectomy){"Yes"}else{"No"} })

  # oophorectomy: reduced risk?
  reduce.risk.ooph <- reactive({ if(input$oophorectomy | input$oophorectomy.2){"Yes"}else{"No"} })
  reduce.risk.ooph.3 <- reactive({ if(input$oophorectomy | input$oophorectomy.3){"Yes"}else{"No"} })

  # hysterectomy: has the user had one?
  had.hyst <- reactive({ if(input$hysterectomy){"Yes"}else{"No"} })

  # hysterectomy: reduced risk?
  reduce.risk.hyst <- reactive({ if(input$hysterectomy | input$hysterectomy.2){"Yes"}else{"No"} })
  reduce.risk.hyst.3 <- reactive({ if(input$hysterectomy | input$hysterectomy.3){"Yes"}else{"No"} })

  # aspirin regimen: reduced risk?
  aspirin.tmp <- reactive({ if(input$aspirin){"Yes"}else{"No"} })
  aspirin.tmp.2 <- reactive({ if(input$aspirin2){"Yes"}else{"No"} })

  # colonoscopy: reduced risk?
  colonoscopy.tmp <- reactive({ if(input$colonoscopy){"Yes"}else{"No"} })
  colonoscopy.tmp.2 <- reactive({ if(input$colonoscopy2){"Yes"}else{"No"} })
  
  # determine if cancers with interventions were already had by the user
  hadColo <- reactive({
    if(sum(input$previousCancers %in% "Colorectal Cancer") > 0){
      return(TRUE)
    } else { return(FALSE) }
  })
  output$hadColo <- reactive({ hadColo() })
  outputOptions(output, 'hadColo', suspendWhenHidden = FALSE)
  
  hadEndo <- reactive({
    if(sum(input$previousCancers %in% "Endometrial Cancer") > 0){
      return(TRUE)
    } else { return(FALSE) }
  })
  output$hadEndo <- reactive({ hadEndo() })
  outputOptions(output, 'hadEndo', suspendWhenHidden = FALSE)
  
  hadOvar <- reactive({
    if("Ovarian Cancer" %in% input$previousCancers){
      return(TRUE)
    } else { return(FALSE) }
  })
  output$hadOvar <- reactive({ hadOvar() })
  outputOptions(output, 'hadOvar', suspendWhenHidden = FALSE)
  
  # check if CRC is a selected cancer
  output$CRCselected <- reactive({ ifelse("Colorectal Cancer" %in% input$cancerView, TRUE, FALSE) })
  output$CRCselected3 <- reactive({ ifelse("Colorectal Cancer" %in% input$cancerView3, TRUE, FALSE) })
  output$CRCselected5 <- reactive({ ifelse("Colorectal Cancer" %in% input$cancerView5, TRUE, FALSE) })
  outputOptions(output, 'CRCselected', suspendWhenHidden = FALSE)
  outputOptions(output, 'CRCselected3', suspendWhenHidden = FALSE)
  outputOptions(output, 'CRCselected5', suspendWhenHidden = FALSE)

  ## BMI ##
  # calculate user's BMI
  BMI <- reactiveValues()
  
  observeEvent(list(input$calcBMI, input$recalcBMI), {
    BMI$bmi <- get.BMI(ht_FT = input$heightFT, ht_IN = input$heightIN, weight = cur.weight())
  })
  output$bmi <- renderText({ round(BMI$bmi,1) })

  # only show BMI reductions intervention options if above normal weight
  Hweight <- eventReactive(list(input$calcBMI, input$recalcBMI), {
    
    # calc bmi and compare
    tmp.bmi <- get.BMI(ht_FT = input$heightFT, ht_IN = input$heightIN, weight = cur.weight())
    tmp.bmi > 25
    
  })
  output$Hweight <- reactive({ Hweight() })
  outputOptions(output, 'Hweight', suspendWhenHidden = FALSE)

  # dynamically change the user input selection values for weight loss
  observeEvent(list(input$heightFT, input$heightIN, input$weight), {

    # check how many BMI points the user could lose to get into the normal range
    temp.bmi <- get.BMI(ht_FT = input$heightFT, ht_IN = input$heightIN, weight = cur.weight())
    new.max <- max.weight.loss(tmp.bmi = temp.bmi, ht_FT = input$heightFT, ht_IN = input$heightIN)
    
    updateSliderInput(session,
                      inputId = "lowerBMI.endo.only",
                      max = new.max)
    
    updateSliderInput(session,
                      inputId = "lowerBMI2.endo.only",
                      max = new.max)
    
    updateSliderInput(session,
                      inputId = "lowerBMI.colo.only",
                      max = new.max)
    
    updateSliderInput(session,
                      inputId = "lowerBMI2.colo.only",
                      max = new.max)
    
    updateSliderInput(session,
                      inputId = "lowerBMI.endo.colo",
                      max = new.max)

    updateSliderInput(session,
                      inputId = "lowerBMI2.endo.colo",
                      max = new.max)

  })

  # pre-process user inputs for weight loss so they can be used to filter the master data frame
  dropBMI.tmp.endo.only <- reactive({
    
    delta.bmi <- change.in.bmi(wght = cur.weight(), 
                               weight.loss = input$lowerBMI.endo.only,
                               htFT = input$heightFT, 
                               htIN = input$heightIN)
    delta.bmi

  })
  
  dropBMI.tmp.2.endo.only <- reactive({
    
    delta.bmi <- change.in.bmi(wght = cur.weight(), 
                               weight.loss = input$lowerBMI2.endo.only,
                               htFT = input$heightFT, 
                               htIN = input$heightIN)
    delta.bmi
    
  })
  
  # pre-process user inputs for weight loss so they can be used to filter the master data frame
  dropBMI.tmp.colo.only <- reactive({
    
    delta.bmi <- change.in.bmi(wght = cur.weight(), 
                               weight.loss = input$lowerBMI.colo.only,
                               htFT = input$heightFT, 
                               htIN = input$heightIN)
    delta.bmi
    
  })
  
  dropBMI.tmp.2.colo.only <- reactive({
    
    delta.bmi <- change.in.bmi(wght = cur.weight(), 
                               weight.loss = input$lowerBMI2.colo.only,
                               htFT = input$heightFT, 
                               htIN = input$heightIN)
    delta.bmi
    
  })
  
  # pre-process user inputs for weight loss so they can be used to filter the master data frame
  dropBMI.tmp.endo.colo <- reactive({
    
    delta.bmi <- change.in.bmi(wght = cur.weight(), 
                               weight.loss = input$lowerBMI.endo.colo,
                               htFT = input$heightFT, 
                               htIN = input$heightIN)
    delta.bmi
    
  })
  
  dropBMI.tmp.2.endo.colo <- reactive({
    
    delta.bmi <- change.in.bmi(wght = cur.weight(), 
                               weight.loss = input$lowerBMI2.endo.colo,
                               htFT = input$heightFT, 
                               htIN = input$heightIN)
    delta.bmi
    
  })
  
  ## Eligible Interventions ##
  
  # colonscopies
  eligCol <- reactive({
    ifelse(!hadColo(), TRUE, FALSE)
  })
  
  # aspirin
  eligAsp <- reactive({
    ifelse(!hadColo(), TRUE, FALSE)
  })
  
  ## BMI
  # BMI - CRC
  eligBMIcrc <- reactive({
    ifelse(!hadColo() & input$gene == "MLH1" & Hweight(), TRUE, FALSE)
  })
  
  # BMI - Endo
  eligBMIendo <- reactive({
    ifelse(!hadEndo() & 
             !input$hysterectomy & Hweight() & 
             !input$gene %in% c("PMS2","EPCAM") & 
             input$gender == "Female",
           TRUE, FALSE)
  })
  
  # Hysterectomy
  eligHyst <- reactive({
    ifelse(!input$hysterectomy & !hadEndo() & 
             !input$gene %in% c("PMS2","EPCAM") & 
             input$gender == "Female", 
           TRUE, FALSE)
  })
  
  # Oophorectomy
  eligOoph <- reactive({
    ifelse(!input$oophorectomy & 
             !hadOvar() & 
             input$gene != "EPCAM" &
             input$gender == "Female", TRUE, FALSE)
  })
  
  # Any CRC intervention available
  eligCRCint <- reactive({
    ifelse(eligCol() | eligAsp() | eligBMIcrc(), TRUE, FALSE)
  })
  
  # Any Endo intervention available
  eligENDOint <- reactive({
    ifelse(eligBMIendo() | eligHyst(), TRUE, FALSE)
  })
  
  # Any Ovarian intervention available
  eligOVARint <- reactive({
    ifelse(eligOoph(), TRUE, FALSE)
  })
  
  # Any intervention available
  eligAnyIntervention <- reactive({
    ifelse(eligCRCint() | eligENDOint() | eligOVARint(), TRUE, FALSE)
  })
  
  output$eligCol <- reactive({ eligCol() })
  output$eligAsp <- reactive({ eligAsp() })
  output$eligBMIcrc <- reactive({ eligBMIcrc() })
  output$eligBMIendo <- reactive({ eligBMIendo() })
  output$eligHyst <- reactive({ eligHyst() })
  output$eligOoph <- reactive({ eligOoph() })
  output$eligCRCint <- reactive({ eligCRCint() })
  output$eligENDOint <- reactive({ eligENDOint() })
  output$eligOVARint <- reactive({ eligOVARint() })
  
  outputOptions(output, 'eligCol', suspendWhenHidden = FALSE)
  outputOptions(output, 'eligAsp', suspendWhenHidden = FALSE)
  outputOptions(output, 'eligBMIcrc', suspendWhenHidden = FALSE)
  outputOptions(output, 'eligBMIendo', suspendWhenHidden = FALSE)
  outputOptions(output, 'eligHyst', suspendWhenHidden = FALSE)
  outputOptions(output, 'eligOoph', suspendWhenHidden = FALSE)
  outputOptions(output, 'eligCRCint', suspendWhenHidden = FALSE)
  outputOptions(output, 'eligENDOint', suspendWhenHidden = FALSE)
  outputOptions(output, 'eligOVARint', suspendWhenHidden = FALSE)
  
  # combine race and ethnicity into PanelPRO race categories
  pp.race <- reactiveVal("All_Races")
  eventReactive(list(input$race, input$ethnicity), {
    if(input$race == "All_Races"){
      if(input$ethnicity %in% c("Other_Ethnicity", "Non-Hispanic")){
        pp.race("All_Races")
      } else if(input$ethnicity == "Hispanic"){
        pp.race("Hispanic")
      }
    } else if(input$race %in% c("AIAN", "Asian", "Black")){
      if(input$ethnicity %in% c("Other_Ethnicity", "Non-Hispanic")){
        pp.race(input$race)
      } else if(input$ethnicity == "Hispanic"){
        pp.race("All_Races")
      }
    } else if(input$race == "White"){
      if(input$ethnicity == "Non-Hispanic"){
        pp.race("WNH")
      } else if(input$ethnicity == "Hispanic"){
        pp.race("WH")
      } else if(input$ethnicity == "Other_Ethnicity"){
        pp.race("All_Races")
      }
    }
  })

  #### Main Data Frames #####

  # subset the master data frame based on user inputs and wrangle into a proper ggplot format
  
  # for everything but reports and report previews
  carrier.df <- reactive({
    
    selected.drop <- select.weight.loss.input(gene = input$gene, 
                                              Hweight = Hweight(), 
                                              gender = input$gender,
                                              hadColo = hadColo(),
                                              hadEndo = hadEndo(), 
                                              hysterectomy = input$hysterectomy,
                                              colo.only = dropBMI.tmp.colo.only(), 
                                              endo.only = dropBMI.tmp.endo.only(), 
                                              endo.colo = dropBMI.tmp.endo.colo())
    
    df <- main.df(data = ppdb, 
                  gender1 = input$gender, 
                  age1 = cur.age(), 
                  race1 = pp.race(),
                  gene1 = input$gene, 
                  hyst.input = input$hysterectomy,
                  ooph.input = input$oophorectomy,
                  hyst.status = had.hyst(), 
                  ooph.status = had.ooph(), 
                  age.range = age.range(), 
                  prev.cancers = input$previousCancers,
                  new.hyst = reduce.risk.hyst(), 
                  new.ooph = reduce.risk.ooph(), 
                  aspirin.status = aspirin.tmp(), 
                  colonoscopy.status = colonoscopy.tmp(), 
                  bmi.status = selected.drop,
                  user.bmi = BMI$bmi,
                  weight1 = cur.weight(),
                  heightFT1 = input$heightFT,
                  heightIN1 = input$heightIN,
                  noEstimate = noEstimate())

    df

  })
  
  # for reports and report previews
  carrier.df.2 <- reactive({
    
    selected.drop <- select.weight.loss.input(gene = input$gene, 
                                              Hweight = Hweight(), 
                                              gender = input$gender, 
                                              hadColo = hadColo(),
                                              hadEndo = hadEndo(), 
                                              hysterectomy = input$hysterectomy,
                                              colo.only = dropBMI.tmp.2.colo.only(), 
                                              endo.only = dropBMI.tmp.2.endo.only(), 
                                              endo.colo = dropBMI.tmp.2.endo.colo())
    
    df <- main.df(data = ppdb, 
                  gender1 = input$gender, 
                  age1 = cur.age(),
                  race1 = pp.race(),
                  gene1 = input$gene, 
                  hyst.input = input$hysterectomy,
                  ooph.input = input$oophorectomy,
                  hyst.status = had.hyst(), 
                  ooph.status = had.ooph(), 
                  age.range = age.range(), 
                  prev.cancers = input$previousCancers,
                  new.hyst = reduce.risk.hyst.3(), 
                  new.ooph = reduce.risk.ooph.3(), 
                  aspirin.status = aspirin.tmp.2(), 
                  colonoscopy.status = colonoscopy.tmp.2(), 
                  bmi.status = selected.drop,
                  user.bmi = BMI$bmi,
                  weight1 = cur.weight(),
                  heightFT1 = input$heightFT,
                  heightIN1 = input$heightIN,
                  noEstimate = noEstimate())
    
    df
    
  })


  #### Cancer View and Options Changes #####
  
  # previous cancer options
  prevCancers <- reactiveValues(cancers = cancer.choices)
  toListenPC <- reactive({ list(input$gene, input$gender, input$oophorectomy, input$hysterectomy) })
  observeEvent(toListenPC(), {
    
    if(input$gender != " " & input$gene != " "){

      # get possible cancers
      if(input$gender == "Male"){
        cn.gn.mat <- pp.male.gc.assoc
      } else if(input$gender == "Female"){
        cn.gn.mat <- pp.female.gc.assoc
      }
      cn.gn.vec <- cn.gn.mat[, grep(pattern = input$gene, colnames(cn.gn.mat), value = T)]
      names(cn.gn.vec) <- rownames(cn.gn.mat)
      all.cancers <- names(cn.gn.vec)[cn.gn.vec]
  
      # remove cancers if prophylactic surgery already occurred
      if(input$oophorectomy){ all.cancers <- all.cancers[which(all.cancers != "Ovarian")] }
      if(input$hysterectomy){ all.cancers <- all.cancers[which(all.cancers != "Endometrial")] }
      
      # update prior cancers
      prevCancers$cancers <- paste0(all.cancers, " Cancer")
      updateCheckboxGroupInput(session,
                               inputId = "previousCancers",
                               choices = prevCancers$cancers)
    }
  })

  # find the vector of cancers that are actual possibilities based on inputs
  allCancers <- reactiveValues(cancers = cancer.choices)
  rrCancers <- reactiveValues(cancers = rr.cancer.choices)

  toListen <- reactive({ list(input$gene, input$gender, input$oophorectomy, input$hysterectomy, input$previousCancers) })
  observeEvent(toListen(), {
    
    if(input$gene != ' ' & input$gender != ' '){

      # for the bar and line graphs cancer views, update to show only the possible cancers
      allCancers$cancers <- unique(carrier.df()$Cancer)
      updateCheckboxGroupInput(session,
                               inputId = "cancerView",
                               choices = allCancers$cancers, 
                               selected = defaultCancer())
      updateRadioButtons(session,
                         inputId = "cancerViewMobile",
                         choices = allCancers$cancers, 
                         selected = defaultCancer())
      # interventions
      updateCheckboxGroupInput(session,
                               inputId = "cancerView5",
                               choices = allCancers$cancers,
                               selected = defaultCancer())
      updateRadioButtons(session,
                         inputId = "cancerViewMobile5",
                         choices = allCancers$cancers,
                         selected = defaultCancer())
      # report
      updateRadioButtons(session,
                         inputId = "cancerView6",
                         choices = allCancers$cancers,
                         selected = defaultCancer())
  
      # for the waffle cancer views, update to show only the possible cancers
      updateRadioButtons(session,
                         inputId = "cancerView2",
                         choices = allCancers$cancers,
                         selected = defaultCancer())
      
      # for the bar and line graphs with interventions
      # filter data by cancers with options to reduce risk
      
      tmp.cans <- "Colorectal Cancer"
      if(input$gender == 'Female' & input$gene != "EPCAM"){
        if((!input$hysterectomy | Hweight()) & input$gene != "PMS2"){ 
          tmp.cans <- c(tmp.cans,"Endometrial Cancer")
        }
        if(!input$oophorectomy){ tmp.cans <- c(tmp.cans,"Ovarian Cancer") }
      }
      if(length(input$previousCancers) > 0){
        tmp.cans <- tmp.cans[which(!tmp.cans %in% input$previousCancers)]
      }

      rrCancers$cancers <- tmp.cans
      
      updateCheckboxGroupInput(session,
                               inputId = "cancerView3",
                               choices = rrCancers$cancers,
                               selected = tmp.cans[1])
      updateRadioButtons(session,
                         inputId = "cancerViewMobile3",
                         choices = rrCancers$cancers,
                         selected = tmp.cans[1])
      updateRadioButtons(session,
                         inputId = "cancerView4",
                         choices = rrCancers$cancers,
                         selected = tmp.cans[1])
      
    }
  })
  
  # change default cancer displayed if colorectal not available
  
  defaultCancer <- reactive({
    
    if(hadColo()){
      # find next highest risk cancer besides colorectal
      window.risks <- 
        carrier.df() %>% 
        filter(Age == max.age & Who == "Someone Like Me") %>%
        arrange(desc(Percent))
      default.cancer <- window.risks$Cancer[1]
    } else {
      default.cancer <- "Colorectal Cancer"
    }
    default.cancer
  })
  
  output$defaultCancer <- renderText({
    defaultCancer()
  })
  
  #### Mobile Screen ####
  # is_mobile_device <- reactive(isTRUE(input$isMobile))
  is_mobile_device <- reactive(FALSE)
  output$is_mobile_device <- reactive({ is_mobile_device() })
  outputOptions(output, 'is_mobile_device', suspendWhenHidden = FALSE)
  output$check_mobile <- renderText({ paste0("Mobile Device: ", is_mobile_device()) })

  #### Visualizations ####
  
  ## get the plot dimension, depending on mobile device status
  plot.height <- reactive({ plotDims(is_mobile_device())$height })
  # plot.l.width <- reactive({ plotDims(is_mobile_device())$l.width })
  # plot.b.width <- reactive({ plotDims(is_mobile_device())$b.width })
  # 
  # output$plot.height <- reactive({ plot.height() })
  # output$plot.l.width <- reactive({ plot.l.width() })
  # output$plot.b.width <- reactive({ plot.b.width() })
  # 
  # outputOptions(output, 'plot.height', suspendWhenHidden = FALSE)
  # outputOptions(output, 'plot.l.width', suspendWhenHidden = FALSE)
  # outputOptions(output, 'plot.b.width', suspendWhenHidden = FALSE)

  ## find rows and columns in plot matrices
  # for graphs with no interventions
  grid.rows <- reactive({ 
    if(is_mobile_device()){
      return(1)
    } else {
      return(num.grid.rows(selected.cancers = input$cancerView))
    }
  })
  grid.cols <- reactive({ 
    if(is_mobile_device()){
      return(1)
    } else {
      return(num.grid.cols(selected.cancers = input$cancerView))
    }
  })
  
  # for graphs with interventions
  grid.rows3 <- reactive({ 
    if(is_mobile_device()){
      return(1)
    } else {
      return(num.grid.rows(selected.cancers = input$cancerView3))
    }
  })
  grid.cols3 <- reactive({ 
    if(is_mobile_device()){
      return(1)
    } else {
      return(num.grid.cols(selected.cancers = input$cancerView3))
    }
  })
  
  # for report preview and reports
  grid.rows5 <- reactive({ 
    if(is_mobile_device()){
      return(1)
    } else {
      return(num.grid.rows(selected.cancers = input$cancerView5))
    }
  })
  grid.cols5 <- reactive({ 
    if(is_mobile_device()){
      return(1)
    } else {
      return(num.grid.cols(selected.cancers = input$cancerView5))
    }
  })
  
  
  ###### Cancer Ranked List ####
  
  output$cancerList <- renderTable({
    
    # risk of CRC if colonoscopies
    CRC.colonoscopies <- 
      carrier.df() %>% 
      filter(Age == max.age & Who == "Reduced Risk" & Cancer == "Colorectal Cancer") %>%
      mutate(Percent = round(Percent,0)) %>%
      ungroup() %>%
      select(Percent) %>%
      as.matrix() %>%
      as.vector()
    
    # risk of CRC for gen pop
    nc.CRC.colonoscopies <- 
      carrier.df() %>% 
      filter(Age == max.age & Who == "Average Person" & Cancer == "Colorectal Cancer") %>%
      mutate(Percent = round(Percent,0)) %>%
      ungroup() %>%
      select(Percent) %>%
      as.matrix() %>%
      as.vector()
    
    # relative risk with colonoscopies
    RR.colonoscopies <- ceiling(CRC.colonoscopies / nc.CRC.colonoscopies)
    
    # get lifetime cumulative risks for the user
    window.risks <- 
      carrier.df() %>% 
      filter(Age == max.age & Who == "Someone Like Me") 
    
    # get lifetime cumulative risks for the average person
    nc.window.risks <- 
      carrier.df() %>% 
      filter(Age == max.age & Who == "Average Person") %>%
      rename("NC.Percent" = Percent)
    
    # find the relative risks and absolute risks
    window.risks <- cbind(window.risks, nc.window.risks$NC.Percent)
    colnames(window.risks)[length(colnames(window.risks))] <- "NC.Percent"
    window.risks <-
      window.risks %>%
      mutate(RR = ceiling(Percent / NC.Percent)) %>%
      arrange(desc(Percent)) %>%
      mutate(Cancer = ifelse(Cancer == "Colorectal Cancer",
                             paste0(Cancer," (without colonoscopies)"),
                             Cancer)) %>%
      mutate(rel.risk = paste0(RR," times more risk")) %>%
      mutate(abs.risk = paste0(round(Percent),"%")) %>%
      ungroup() %>%
      select(Cancer, abs.risk, rel.risk) %>%
      mutate(Cancer = paste0(row_number(), ". ", Cancer))
      
    # add row without colonoscopies
    if(sum(grepl("without colonoscopies", window.risks$Cancer)) == 1){
      row.num <- which(grepl(pattern = "without colonoscopies", window.risks$Cancer))
      tmp.row <- data.frame(Cancer = paste0("....Colorectal Cancer (with colonoscopies)"),
                            abs.risk = paste0(CRC.colonoscopies,"%"),
                            rel.risk = paste0(RR.colonoscopies," times more risk"))
      window.risks <- rbind(window.risks, tmp.row)
      if(nrow(window.risks) > 2){
        ord <- c(1:row.num,nrow(window.risks),(row.num+1):(nrow(window.risks)-1))
        window.risks <- window.risks[ord,]
      }
    }
    
    colnames(window.risks) <- c("Cancer","My Lifetime Risk","Me Compared to Someone Without Lynch")
    window.risks
    
  }, striped = T, hover = T, spacing = "l", align = c("lcc"))
  
  output$extraCancers <- renderUI({
    
    additional.cancers <- as.character(other.cancers[which(other.cancers$Gene == input$gene & 
                                                             other.cancers$Gender == input$gender),
                                                     "Statement"])
    
    statement1 <- paste0(input$gene,
                         " mutation carriers also have some increased risk of additional cancers although there is insufficient")
    statement2 <- "data, at this time, to produce a risk estimate. Those additional cancers are:"
    
    
    if(grepl(pattern = "and upper urinary tract", additional.cancers)){
      additional.cancers <- sub(pattern = "and upper urinary tract \\(kidney \\& uterer\\)", replacement = "", additional.cancers)
      statement3 <- additional.cancers
      statement4 <- "and upper urinary tract (kidney & uterer)."
    } else {
      statement3 <- paste0(additional.cancers,".")
      statement4 <- ""
    }
    
    out <- paste0(p(statement1,
                     style = "text-indent: 1.5%; margin-bottom: 0px; margin-top: 20px;"),
                  "</br>",
                  p(statement2,
                     style = "text-indent: 1.5%; margin-bottom: 0px; margin-top: -15px;"),
                  "</br></br>",
                  p(statement3,
                     style = "font-weight: bold; text-indent: 3%; margin-bottom: 0px; margin-top: -15px;"),
                  "</br>",
                  p(statement4,
                     style = "font-weight: bold; text-indent: 3%; margin-bottom: 0px; margin-top: -15px;"),
                  "</br>")
    
    HTML(out)
    
  })


  ###### Plotly (no interventions) ####

  # Interactive Line Graph if one cancer is selected (current age to age max.age)
  output$oneCancerYearlyLine <- renderPlotly({
    
    if(is_mobile_device()){
      this.cancer <- input$cancerViewMobile
    } else {
      this.cancer <- input$cancerView
    }

    # validate at least one cancer to view is selected, this suppressed split second error when published
    if(length(this.cancer) == 0){ validate("") }

    if(length(this.cancer) == 1){
      
      # plotly colors
      user.color <- cancer.colors[grep(pattern = this.cancer, names(cancer.colors))]
      carrier.color <- paste("rgb(",paste(as.vector(col2rgb(user.color)), collapse = ", "),")")
      non.carrier.color <- paste("rgb(",paste(as.vector(col2rgb(gen.pop.color)), collapse = ", "),")")
      
      # show colonoscopies if CRC selected
      if(this.cancer == "Colorectal Cancer"){
        
        # prepare the data
        tmp.carrier.df <- plotly.data(df = carrier.df(),
                                      useRR = TRUE,
                                      cancerView = this.cancer)
        
        # plot
        pp1 <- plot_ly(tmp.carrier.df,
                       x = ~Age,
                       hovertemplate = paste("<b>Age:</b> %{x}",
                                             "<br><b>Percent:</b> %{y:.0%}"))
        pp1 <- pp1 %>% add_trace(y = ~AveragePerson,
                                 type = 'scatter',
                                 mode = "lines+markers",
                                 line = list(color = non.carrier.color, width = 4),
                                 marker = list(color = non.carrier.color, size = 10),
                                 name = 'Average Person')
        pp1 <- pp1 %>% add_trace(y = ~ReducedRisk,
                                 type = 'scatter',
                                 mode = "lines",
                                 line = list(color = carrier.color, dash = 'dash', width = 4),
                                 name = 'Someone Like Me, with Colonoscopies')
        pp1 <- pp1 %>% add_trace(y = ~SomeoneLikeMe,
                                 type = 'scatter',
                                 mode = "lines",
                                 line = list(color = carrier.color, width = 4),
                                 name = 'Someone Like Me, without Colonoscopies')
        
        # format plot
        pp1 <- format.plotly(base.plotly = pp1, 
                             selected.cancer = this.cancer, 
                             age = cur.age(),
                             isMobile = is_mobile_device())
        
        # not CRC selected
      } else {
      
        # prepare the data
        tmp.carrier.df <- plotly.data(df = carrier.df(),
                                      useRR = FALSE,
                                      cancerView = this.cancer)
        
        # plot
        pp1 <- plot_ly(tmp.carrier.df,
                       x = ~Age,
                       hovertemplate = paste("<b>Age:</b> %{x}",
                                             "<br><b>Percent:</b> %{y:.0%}"))
        pp1 <- pp1 %>% add_trace(y = ~AveragePerson,
                                 type = 'scatter',
                                 mode = "lines+markers",
                                 line = list(color = non.carrier.color, width = 4),
                                 marker = list(color = non.carrier.color, size = 10),
                                 name = 'Average Person')
        pp1 <- pp1 %>% add_trace(y = ~SomeoneLikeMe,
                                 type = 'scatter',
                                 mode = "lines",
                                 line = list(color = carrier.color, width = 4),
                                 name = 'Someone Like Me')
        
        # format plot
        pp1 <- format.plotly(base.plotly = pp1, 
                             selected.cancer = this.cancer, 
                             age = cur.age(),
                             isMobile = is_mobile_device())
      }
      
      # blank plot to avoid errors if more than one cancer selected
    } else {
      
      pp1 <- plot_ly(data.frame(x = 1, y = 1), x = 1, y = 1, type = 'scatter', mode = 'lines')
      
    }

    pp1

  })
  
  ###### Plotly (interventions) ####
  
  # Interactive Line Graph if one cancer is selected (current age to age max.age)
  output$RoneCancerYearlyLine <- renderPlotly({
    
    if(is_mobile_device()){
      this.cancer <- input$cancerViewMobile3
    } else {
      this.cancer <- input$cancerView3
    }
    
    # validate at least one cancer to view is selected, this suppressed split second error when published
    if(length(this.cancer) == 0){ validate("") }

    # colors
    if(length(this.cancer) == 1){
      user.color <- cancer.colors[grep(pattern = this.cancer, names(cancer.colors))]
      carrier.color <- paste("rgb(",paste(as.vector(col2rgb(user.color)), collapse = ", "),")")
      non.carrier.color <- paste("rgb(",paste(as.vector(col2rgb(gen.pop.color)), collapse = ", "),")")
    }
    
    # which weight loss input to use
    selected.w.loss.input <- select.weight.loss.input(gene = input$gene, 
                                                      Hweight = Hweight(), 
                                                      gender = input$gender, 
                                                      hadColo = hadColo(),
                                                      hadEndo = hadEndo(), 
                                                      hysterectomy = input$hysterectomy,
                                                      colo.only = input$lowerBMI.colo.only, 
                                                      endo.only = input$lowerBMI.endo.only, 
                                                      endo.colo = input$lowerBMI.endo.colo)
    
    # only include intervention bars and legend item if intervention selected
    if( length(this.cancer) == 1 & sum(rrCancers$cancers %in% this.cancer) == 1 &
        ((this.cancer == "Colorectal Cancer"  & (input$aspirin | input$colonoscopy | selected.w.loss.input > 0)) |
         (this.cancer == "Endometrial Cancer" & (input$hysterectomy.2 | selected.w.loss.input > 0)) | 
         (this.cancer == "Ovarian Cancer"     & input$oophorectomy.2) )){
      
      # prepare the data
      tmp.carrier.df <- plotly.data(df = carrier.df(),
                                    useRR = TRUE,
                                    cancerView = this.cancer)
      
      # plot
      pp1 <- plot_ly(tmp.carrier.df,
                     x = ~Age,
                     hovertemplate = paste("<b>Age:</b> %{x}",
                                           "<br><b>Percent:</b> %{y:.0%}"))
      pp1 <- pp1 %>% add_trace(y = ~AveragePerson,
                               type = 'scatter',
                               mode = "lines+markers",
                               line = list(color = non.carrier.color, width = 4),
                               marker = list(color = non.carrier.color, size = 10),
                               name = 'Average Person')
      pp1 <- pp1 %>% add_trace(y = ~ReducedRisk,
                               type = 'scatter',
                               mode = "lines",
                               line = list(color = carrier.color, dash = 'dash', width = 4),
                               name = 'Someone Like Me, with Risk Reduction')
      pp1 <- pp1 %>% add_trace(y = ~SomeoneLikeMe,
                               type = 'scatter',
                               mode = "lines",
                               line = list(color = carrier.color,width = 4),
                               name = 'Someone Like Me, without Risk Reduction')
      
    } else if(length(this.cancer) == 1){
      
      # prepare the data
      tmp.carrier.df <- plotly.data(df = carrier.df(),
                                    useRR = FALSE,
                                    cancerView = this.cancer)
      
      # plot
      pp1 <- plot_ly(tmp.carrier.df,
                     x = ~Age,
                     hovertemplate = paste("<b>Age:</b> %{x}",
                                           "<br><b>Percent:</b> %{y:.0%}"))
      pp1 <- pp1 %>% add_trace(y = ~AveragePerson,
                               type = 'scatter',
                               mode = "lines+markers",
                               line = list(color = non.carrier.color, width = 4),
                               marker = list(color = non.carrier.color, size = 10),
                               name = 'Average Person')
      pp1 <- pp1 %>% add_trace(y = ~SomeoneLikeMe,
                               type = 'scatter',
                               mode = "lines",
                               line = list(color = carrier.color,width = 4),
                               name = 'Someone Like Me')
     
      # prevent error if multiple cancers selected 
    } else {
      
      pp1 <- plot_ly(data.frame(x = 1, y = 1), x = 1, y = 1, type = 'scatter', mode = 'lines')
      
    }
    
    # format plot
    if(length(this.cancer) == 1){
      pp1 <- format.plotly(base.plotly = pp1, 
                           selected.cancer = this.cancer, 
                           age = cur.age(),
                           isMobile = is_mobile_device())
    }
    
    pp1
    
  })
  
  
  ###### Plotly Report Preview (interventions) ####
  
  # Interactive Line Graph if one cancer is selected (current age to age max.age)
  output$DoneCancerYearlyLine <- renderPlotly({
    
    if(is_mobile_device()){
      this.cancer <- input$cancerViewMobile5
    } else {
      this.cancer <- input$cancerView5
    }
    
    # validate at least one cancer to view is selected, this suppressed split second error when published
    if(length(this.cancer) == 0){ validate("") }
    
    # colors
    if(length(this.cancer) == 1){
      user.color <- cancer.colors[grep(pattern = this.cancer, names(cancer.colors))]
      carrier.color <- paste("rgb(",paste(as.vector(col2rgb(user.color)), collapse = ", "),")")
      non.carrier.color <- paste("rgb(",paste(as.vector(col2rgb(gen.pop.color)), collapse = ", "),")")
    }
    
    # which weight loss input to use
    selected.w.loss.input <- select.weight.loss.input(gene = input$gene, 
                                              Hweight = Hweight(), 
                                              gender = input$gender, 
                                              hadColo = hadColo(),
                                              hadEndo = hadEndo(), 
                                              hysterectomy = input$hysterectomy,
                                              colo.only = input$lowerBMI2.colo.only, 
                                              endo.only = input$lowerBMI2.endo.only, 
                                              endo.colo = input$lowerBMI2.endo.colo)
    
    # only include intervention bars and legend item if intervention selected
    if( length(this.cancer) == 1 & sum(rrCancers$cancers %in% this.cancer) == 1 &
        ((this.cancer == "Colorectal Cancer"  & (input$aspirin2 | input$colonoscopy2 | selected.w.loss.input > 0)) |
         (this.cancer == "Endometrial Cancer" & (input$hysterectomy.3 | selected.w.loss.input > 0)) | 
         (this.cancer == "Ovarian Cancer"     & input$oophorectomy.3) )){
      
      # prepare the data
      tmp.carrier.df <- plotly.data(df = carrier.df.2(),
                                    useRR = TRUE,
                                    cancerView = this.cancer)
      
      # plot
      pp1 <- plot_ly(tmp.carrier.df,
                     x = ~Age,
                     hovertemplate = paste("<b>Age:</b> %{x}",
                                           "<br><b>Percent:</b> %{y:.0%}"))
      pp1 <- pp1 %>% add_trace(y = ~AveragePerson,
                               type = 'scatter',
                               mode = "lines+markers",
                               line = list(color = non.carrier.color, width = 4),
                               marker = list(color = non.carrier.color, size = 10),
                               name = 'Average Person')
      pp1 <- pp1 %>% add_trace(y = ~ReducedRisk,
                               type = 'scatter',
                               mode = "lines",
                               line = list(color = carrier.color, dash = 'dash', width = 4),
                               name = 'Someone Like Me, with Risk Reduction')
      pp1 <- pp1 %>% add_trace(y = ~SomeoneLikeMe,
                               type = 'scatter',
                               mode = "lines",
                               line = list(color = carrier.color, width = 4),
                               name = 'Someone Like Me, without Risk Reduction')
      
    } else if(length(this.cancer) == 1){
      
      # prepare the data
      tmp.carrier.df <- plotly.data(df = carrier.df.2(),
                                    useRR = FALSE,
                                    cancerView = this.cancer)
      
      # plot
      pp1 <- plot_ly(tmp.carrier.df,
                     x = ~Age,
                     hovertemplate = paste("<b>Age:</b> %{x}",
                                           "<br><b>Percent:</b> %{y:.0%}"))
      pp1 <- pp1 %>% add_trace(y = ~AveragePerson,
                               type = 'scatter',
                               mode = "lines+markers",
                               line = list(color = non.carrier.color, width = 4),
                               marker = list(color = non.carrier.color, size = 10),
                               name = 'Average Person')
      pp1 <- pp1 %>% add_trace(y = ~SomeoneLikeMe,
                               type = 'scatter',
                               mode = "lines",
                               line = list(color = carrier.color,width = 4),
                               name = 'Someone Like Me')
      
      # prevent error if multiple cancers selected 
    } else {
      
      pp1 <- plot_ly(data.frame(x = 1, y = 1), x = 1, y = 1, type = 'scatter', mode = 'lines')
      
    }
    
    # format the plot
    if(length(this.cancer) == 1){
      pp1 <- format.plotly(base.plotly = pp1, 
                           selected.cancer = this.cancer, 
                           age = cur.age(),
                           isMobile = is_mobile_device())
    }
    
    pp1
    
  })
  

  ####### Line Graph (no interventions) ####

  # Non-interactive Line Graph for comparing cancers (current age to age max.age)
  output$multiCancerYearlyLine <- renderPlot({
    
    if(is_mobile_device()){
      this.can <- input$cancerViewMobile5
    } else {
      this.can <- input$cancerView5
    }

    # validate at least one cancer to view is selected, suppressed glitch when published
    if(length(this.can) == 0){ validate("") }

    # empty grid of plots
    g <- list()

    # single cancer plot
    for(this.cancer in input$cancerView){
      
      # show colonoscopy affect if CRC
      if(this.cancer == "Colorectal Cancer"){
        
        tmp.RR <- TRUE
        tmp.screen <- TRUE

        # not CRC
      } else {
        
        tmp.RR <- FALSE
        tmp.screen <- FALSE
        
      }
      
      # prepare the data
      tmp.carrier.df <- line.data(df = carrier.df(), 
                                  useRR = tmp.RR,
                                  cancer = this.cancer)
      
      # LINE GRAPH for a carrier vs non-carrier
      this.plot <- make.ggline(data = tmp.carrier.df,
                               cancer = this.cancer,
                               useRR = tmp.RR,
                               colonoscopies.only = tmp.screen,
                               age = cur.age(), 
                               grows = grid.rows(),
                               gcols = grid.cols())

      cancer.num <- which(input$cancerView == this.cancer)
      g[[cancer.num]] <- this.plot

    }

    # plot the grid of individual cancer plots
    p0 <- grid.arrange(grobs = g,
                       top = grid.title.line,
                       left = grid.y.axis.title,
                       bottom = grid.x.axis.title.line,
                       nrow = grid.rows())

    p0

  }, height = height, width = width )
  
  
  ####### Line Graph (interventions) ####
  
  # Non-interactive Line Graph for comparing cancers (current age to age max.age)
  output$RmultiCancerYearlyLine <- renderPlot({
    
    if(is_mobile_device()){
      this.can <- input$cancerViewMobile5
    } else {
      this.can <- input$cancerView5
    }
    
    # validate at least one cancer to view is selected, suppressed glitch when published
    if(length(this.can) == 0){ validate("") }
    
    # empty grid of plots
    g <- list()
    
    # single cancer plot
    for(this.cancer in input$cancerView3){
      
      # which weight loss input to use
      selected.w.loss.input <- select.weight.loss.input(gene = input$gene, 
                                                Hweight = Hweight(), 
                                                gender = input$gender, 
                                                hadColo = hadColo(),
                                                hadEndo = hadEndo(), 
                                                hysterectomy = input$hysterectomy,
                                                colo.only = input$lowerBMI.colo.only, 
                                                endo.only = input$lowerBMI.endo.only, 
                                                endo.colo = input$lowerBMI.endo.colo)
      
      # only include intervention bars and legend item if intervention selected
      if( sum(rrCancers$cancers == this.cancer) == 1 &
          ((this.cancer == "Colorectal Cancer"  & (input$aspirin | input$colonoscopy | selected.w.loss.input > 0)) |
           (this.cancer == "Endometrial Cancer" & (input$hysterectomy.2 | selected.w.loss.input > 0)) | 
           (this.cancer == "Ovarian Cancer"     & input$oophorectomy.2) )){
        
        tmp.RR <- TRUE
        
        # for cancers without intervention options or no interventions selected for this cancer
      } else {
        
        tmp.RR <- FALSE
        
      }
      
      # prepare the data
      tmp.carrier.df <- line.data(df = carrier.df(), 
                                  useRR = tmp.RR,
                                  cancer = this.cancer)
      
      # LINE GRAPH for a carrier vs non-carrier
      this.plot <- make.ggline(data = tmp.carrier.df,
                                 cancer = this.cancer,
                                 useRR = tmp.RR,
                                 age = cur.age(), 
                                 grows = grid.rows3(),
                                 gcols = grid.cols3())
      
      cancer.num <- which(input$cancerView3 == this.cancer)
      g[[cancer.num]] <- as.grob(this.plot)
      
    }
    
    # plot the grid of individual cancer plots
    p0 <- grid.arrange(grobs = g,
                       top = grid.title.line,
                       left = grid.y.axis.title,
                       bottom = grid.x.axis.title.line,
                       nrow = grid.rows3()
    )
    
    p0
    
  }, height = height, width = width )
  
  
  ####### Line Graph Report Preview (interventions) ####
  
  # Non-interactive Line Graph for comparing cancers (current age to age max.age)
  output$DmultiCancerYearlyLine <- renderPlot({
    
    if(is_mobile_device()){
      this.can <- input$cancerViewMobile5
    } else {
      this.can <- input$cancerView5
    }
    
    # validate at least one cancer to view is selected, suppressed glitch when published
    if(length(this.can) == 0){ validate("") }
    
    # empty grid of plots
    g <- list()
    
    # single cancer plot
    for(this.cancer in input$cancerView5){
      
      # which weight loss input to use
      selected.w.loss.input <- select.weight.loss.input(gene = input$gene, 
                                                Hweight = Hweight(), 
                                                gender = input$gender, 
                                                hadColo = hadColo(),
                                                hadEndo = hadEndo(), 
                                                hysterectomy = input$hysterectomy,
                                                colo.only = input$lowerBMI2.colo.only, 
                                                endo.only = input$lowerBMI2.endo.only, 
                                                endo.colo = input$lowerBMI2.endo.colo)
      
      # only include intervention bars and legend item if intervention selected
      if( sum(rrCancers$cancers == this.cancer) == 1 &
          ((this.cancer == "Colorectal Cancer"  & (input$aspirin2 | input$colonoscopy2 | selected.w.loss.input > 0)) |
           (this.cancer == "Endometrial Cancer" & (input$hysterectomy.3 | selected.w.loss.input > 0)) | 
           (this.cancer == "Ovarian Cancer"     & input$oophorectomy.3) )){
        
        tmp.RR <- TRUE
        
      } else {
        
        tmp.RR <- FALSE
        
      }
      
      # prepare the data
      tmp.carrier.df <- line.data(df = carrier.df.2(), 
                                  useRR = tmp.RR,
                                  cancer = this.cancer)
      
      # LINE GRAPH for a carrier vs non-carrier
      this.plot <- make.ggline(data = tmp.carrier.df,
                                 cancer = this.cancer,
                                 useRR = tmp.RR,
                                 age = cur.age(), 
                                 grows = grid.rows5(),
                                 gcols = grid.cols5())
      
      cancer.num <- which(input$cancerView5 == this.cancer)
      g[[cancer.num]] <- as.grob(this.plot)
      
    }
    
    # plot the grid of individual cancer plots
    p0 <- grid.arrange(grobs = g,
                       top = grid.title.line,
                       left = grid.y.axis.title,
                       bottom = grid.x.axis.title.line,
                       nrow = grid.rows5()
    )
    
    p0
    
  }, height = height, width = width )
  

  ####### Report Line Graph ####

  # due to display conditioning for the line graphs conflicting with how the report is displayed
  # a separate reactive function had to set up to generate the line graphs for the report
  lineReport <- reactive({
    
    if(is_mobile_device()){
      this.can <- input$cancerViewMobile5
    } else {
      this.can <- input$cancerView5
    }

    # empty grid of plots
    g <- list()
    
    # single cancer plot
    for(this.cancer in this.can){
      
      # which weight loss input to use
      selected.w.loss.input <- select.weight.loss.input(gene = input$gene, 
                                                Hweight = Hweight(), 
                                                gender = input$gender, 
                                                hadColo = hadColo(),
                                                hadEndo = hadEndo(), 
                                                hysterectomy = input$hysterectomy,
                                                colo.only = input$lowerBMI2.colo.only, 
                                                endo.only = input$lowerBMI2.endo.only, 
                                                endo.colo = input$lowerBMI2.endo.colo)
      
      # only include intervention bars and legend item if intervention selected
      if( sum(rrCancers$cancers == this.cancer) == 1 &
          ((this.cancer == "Colorectal Cancer"  & (input$aspirin2 | input$colonoscopy2 | selected.w.loss.input > 0)) |
           (this.cancer == "Endometrial Cancer" & (input$hysterectomy.3 | selected.w.loss.input > 0)) | 
           (this.cancer == "Ovarian Cancer"     & input$oophorectomy.3) )){
        
        tmp.RR <- TRUE

      } else {
        
        tmp.RR <- FALSE
        
      }
      
      # prepare the data
      tmp.carrier.df <- line.data(df = carrier.df.2(), 
                                  useRR = tmp.RR,
                                  cancer = this.cancer)
      
      # LINE GRAPH for a carrier vs non-carrier
      this.plot <- make.ggline(data = tmp.carrier.df,
                                 cancer = this.cancer,
                                 report = TRUE,
                                 useRR = tmp.RR,
                                 age = cur.age(), 
                                 grows = grid.rows5(),
                                 gcols = grid.cols5())
        
      cancer.num <- which(this.can == this.cancer)
      g[[cancer.num]] <- this.plot

    }

    # grid of individual cancer plots (does not draw)
    p0 <- arrangeGrob(grobs = g,
                       left = grid.y.axis.title,
                       bottom = grid.x.axis.title.line,
                       nrow = grid.rows5()
    )

    # dana farber logo grob (does not draw)
    logo.grob <- rasterGrob(logo)

    # combine logo and settings table into a grob (does not draw)
    p1 <- arrangeGrob(grobs = list(logo.grob, interventionsTable(), settingsTable()),
                      ncol = 1,
                      heights = unit(c(logo.height, interventions.height, settings.height),"in")
    )

    # combine settings/logo grob with plot grob (does not draw)
    p2 <- arrangeGrob(grobs = list(p0, p1),
                      top = grid.report.title,
                      nrow = 1,
                      heights = unit(graph.height,"in"),
                      widths = unit(c(graph.width,settings.width),"in")
    )

    p2

  })
  
  
  ###### Line Graph Switching ####
  # switch between displaying the interactive line graph and the faceted line graph
  # based on the number of cancers selected
  
  ## No intervention graphs
  # boolean conditions to be passed to the UI conditionalPanels
  output$oneLineCondition <- reactive({
    if(is_mobile_device()){
      this.cancer <- input$cancerViewMobile
    } else {
      this.cancer <- input$cancerView
    }
    if(length(this.cancer) == 1){ return(TRUE) } else { return(FALSE) }
  })
  
  output$multiLineCondition <- reactive({
    if(is_mobile_device()){
      this.cancer <- input$cancerViewMobile
    } else {
      this.cancer <- input$cancerView
    }
    if(length(this.cancer) > 1){ return(TRUE) } else { return(FALSE) }
  })
  
  # wrap plots in a renderUI output so that they respond to conditions on when to be displayed
  output$oneLine = renderUI({
    plotlyOutput(outputId = "oneCancerYearlyLine", height = paste0(height = plot.height(),"px"))
  })
  
  output$multiLine = renderUI({
    plotOutput(outputId = "multiCancerYearlyLine", height = paste0(height = height,"px"))
  })
  
  outputOptions(output, "oneLineCondition"    , suspendWhenHidden = FALSE)
  outputOptions(output, "multiLineCondition"  , suspendWhenHidden = FALSE)
  
  ## Intervention graphs
  # boolean conditions to be passed to the UI conditionalPanels
  output$RoneLineCondition <- reactive({
    if(is_mobile_device()){
      this.cancer <- input$cancerViewMobile3
    } else {
      this.cancer <- input$cancerView3
    }
    if(length(this.cancer) == 1){ return(TRUE) } else { return(FALSE) }
  })

  output$RmultiLineCondition <- reactive({
    if(is_mobile_device()){
      this.cancer <- input$cancerViewMobile3
    } else {
      this.cancer <- input$cancerView3
    }
    if(length(this.cancer) > 1){ return(TRUE) } else { return(FALSE) }
  })
  
  # wrap plots in a renderUI output so that they respond to conditions on when to be displayed
  output$RoneLine = renderUI({
    plotlyOutput(outputId = "RoneCancerYearlyLine", height = paste0(height = height,"px"))
  })

  output$RmultiLine = renderUI({
    plotOutput(outputId = "RmultiCancerYearlyLine", height = paste0(height = height,"px"))
  })
  
  outputOptions(output, "RoneLineCondition"  , suspendWhenHidden = FALSE)
  outputOptions(output, "RmultiLineCondition", suspendWhenHidden = FALSE)
  
  ## Report preview graphs
  # boolean conditions to be passed to the UI conditionalPanels
  output$DoneLineCondition <- reactive({
    if(is_mobile_device()){
      this.cancer <- input$cancerViewMobile5
    } else {
      this.cancer <- input$cancerView5
    }
    if(length(this.cancer) == 1){ return(TRUE) } else { return(FALSE) }
  })
  
  output$DmultiLineCondition <- reactive({
    if(is_mobile_device()){
      this.cancer <- input$cancerViewMobile5
    } else {
      this.cancer <- input$cancerView5
    }
    if(length(this.cancer) > 1){ return(TRUE) } else { return(FALSE) }
  })
  
  # wrap plots in a renderUI output so that they respond to conditions on when to be displayed
  output$DoneLine = renderUI({
    plotlyOutput(outputId = "DoneCancerYearlyLine", height = paste0(height = height,"px"))
  })
  
  output$DmultiLine = renderUI({
    plotOutput(outputId = "DmultiCancerYearlyLine", height = paste0(height = height,"px"))
  })
  
  outputOptions(output, "DoneLineCondition"  , suspendWhenHidden = FALSE)
  outputOptions(output, "DmultiLineCondition", suspendWhenHidden = FALSE)


  ###### Bar Graph (no interventions) ####

  # BAR GRAPH of short and long term risks
  output$barGraph <- renderPlot({
    
    if(is_mobile_device()){
      this.can <- input$cancerViewMobile
    } else {
      this.can <- input$cancerView
    }

    # validate at least one cancer to view is selected
    if(length(this.can) == 0){ validate("") }

    # empty grid of plots
    g <- list()

    # single cancer plot
    for(this.cancer in this.can){
      
      # show colonoscopy affect if CRC
      if(this.cancer == "Colorectal Cancer"){
        
        tmp.RR <- TRUE
        tmp.screen <- TRUE
        
        # not CRC
      } else {
        
        tmp.RR <- FALSE
        tmp.screen <- FALSE
        
      }
      
      # prepare the data
      window.risks <- bar.data(df = carrier.df(),
                               useRR = tmp.RR,
                               cancer = this.cancer,
                               age = cur.age())
      
      # BAR GRAPH comparing carriers vs non-carriers
      this.plot <- make.ggbar(data = window.risks, 
                              cancer = this.cancer,
                              cancerView = this.can,
                              useRR = tmp.RR, 
                              colonoscopies.only = tmp.screen,
                              age = cur.age(),
                              grows = grid.rows(),
                              gcols = grid.cols(),
                              isMobile = is_mobile_device())

      # store the plot in a list
      cancer.num <- which(this.can == this.cancer)
      g[[cancer.num]] <- this.plot

    }

    # plot the grid of individual cancer plots
    p0 <- grid.arrange(grobs = g, 
                       top = grid.title.bar, 
                       left = grid.y.axis.title, 
                       bottom = grid.x.axis.title.bar, 
                       nrow = grid.rows())

    # return the grob of faceted plots
    p0

  }, height = height, width = width)
  
  
  ###### Bar Graph (interventions) ####
  
  # BAR GRAPH of short and long term risks
  output$RbarGraph <- renderPlot({
    
    if(is_mobile_device()){
      this.can <- input$cancerViewMobile3
    } else {
      this.can <- input$cancerView3
    }
    
    # validate at least one cancer to view is selected
    if(length(this.can) == 0){ validate("") }
    
    # empty grid of plots
    g <- list()
    
    # which weight loss input to use
    selected.w.loss.input <- select.weight.loss.input(gene = input$gene, 
                                                      Hweight = Hweight(), 
                                                      gender = input$gender, 
                                                      hadColo = hadColo(),
                                                      hadEndo = hadEndo(), 
                                                      hysterectomy = input$hysterectomy,
                                                      colo.only = input$lowerBMI.colo.only, 
                                                      endo.only = input$lowerBMI.endo.only, 
                                                      endo.colo = input$lowerBMI.endo.colo)
    
    # single cancer plot
    for(this.cancer in this.can){
      
      # only include intervention bars and legend item if intervention selected
      if( sum(rrCancers$cancers == this.cancer) == 1 &
          ((this.cancer == "Colorectal Cancer"  & (input$aspirin | input$colonoscopy | selected.w.loss.input > 0)) |
           (this.cancer == "Endometrial Cancer" & (input$hysterectomy.2 | selected.w.loss.input > 0)) | 
           (this.cancer == "Ovarian Cancer"     & input$oophorectomy.2) )){
        
        tmp.RR <- TRUE
        
        # if cancer did not have intervention option or no intervention selected for this cancer
      } else {
        
        tmp.RR <- FALSE
        
      }
      
      # prepare the data
      window.risks <- bar.data(df = carrier.df(),
                               useRR = tmp.RR,
                               cancer = this.cancer,
                               age = cur.age())
      
      # BAR GRAPH comparing carriers vs non-carriers
      this.plot <- make.ggbar(data = window.risks, 
                              cancer = this.cancer,
                              cancerView = this.can,
                              useRR = tmp.RR, 
                              age = cur.age(),
                              grows = grid.rows3(),
                              gcols = grid.cols3(),
                              isMobile = is_mobile_device())
      
      # store the plot in a list
      cancer.num <- which(this.can == this.cancer)
      g[[cancer.num]] <- as.grob(this.plot)
      
    }
    
    # plot the grid of individual cancer plots
    p0 <- grid.arrange(grobs = g, 
                       top = grid.title.bar, 
                       left = grid.y.axis.title, 
                       bottom = grid.x.axis.title.bar, 
                       nrow = grid.rows3())
    
    # return the grob of faceted plots
    p0
    
  }, height = height, width = width)
  
  
  ###### Bar Graph Report Preview (interventions) ####
  
  # BAR GRAPH of short and long term risks
  output$DbarGraph <- renderPlot({
    
    if(is_mobile_device()){
      this.can <- input$cancerViewMobile5
    } else {
      this.can <- input$cancerView5
    }
    
    # validate at least one cancer to view is selected
    if(length(this.can) == 0){ validate("") }
    
    # empty grid of plots
    g <- list()
    
    # which weight loss input to use
    selected.w.loss.input <- select.weight.loss.input(gene = input$gene, 
                                                      Hweight = Hweight(), 
                                                      gender = input$gender, 
                                                      hadColo = hadColo(),
                                                      hadEndo = hadEndo(), 
                                                      hysterectomy = input$hysterectomy,
                                                      colo.only = input$lowerBMI2.colo.only, 
                                                      endo.only = input$lowerBMI2.endo.only, 
                                                      endo.colo = input$lowerBMI2.endo.colo)
    
    # single cancer plot
    for(this.cancer in this.can){
      
      # only include reduced risk bars and legend item if an intervention is applied
      if( sum(rrCancers$cancers == this.cancer) == 1 &
         ((this.cancer == "Colorectal Cancer"  & (input$aspirin2 | input$colonoscopy2 | selected.w.loss.input > 0)) |
          (this.cancer == "Endometrial Cancer" & (input$hysterectomy.3 | selected.w.loss.input > 0)) | 
          (this.cancer == "Ovarian Cancer"     & input$oophorectomy.3) )){

        tmp.RR <- TRUE
        
        # cancer does not have intervention options or no intervention options selected for this cancer
      } else {

        tmp.RR <- FALSE
        
      }
      
      
      # prepare the data
      window.risks <- bar.data(df = carrier.df.2(),
                               useRR = tmp.RR,
                               cancer = this.cancer,
                               age = cur.age())
      
      # BAR GRAPH comparing carriers vs non-carriers
      this.plot <- make.ggbar(data = window.risks, 
                              cancer = this.cancer,
                              cancerView = this.can,
                              useRR = tmp.RR, 
                              age = cur.age(),
                              grows = grid.rows5(),
                              gcols = grid.cols5(),
                              isMobile = is_mobile_device())
      
      # store the plot in a list
      cancer.num <- which(this.can == this.cancer)
      g[[cancer.num]] <- as.grob(this.plot)
      
    }
    
    # plot the grid of individual cancer plots
    p0 <- grid.arrange(grobs = g, 
                       top = grid.title.bar, 
                       left = grid.y.axis.title, 
                       bottom = grid.x.axis.title.bar, 
                       nrow = grid.rows5())
    
    # return the grob of faceted plots
    p0
    
  }, height = height, width = width)


  ####### Report Bar Graph ####

  # BAR GRAPH for report
  barReport <- reactive({
    
    if(is_mobile_device()){
      this.can <- input$cancerViewMobile5
    } else {
      this.can <- input$cancerView5
    }

    # empty grid of plots
    g <- list()
    
    # which weight loss input to use
    selected.w.loss.input <- select.weight.loss.input(gene = input$gene, 
                                                      Hweight = Hweight(), 
                                                      gender = input$gender, 
                                                      hadColo = hadColo(),
                                                      hadEndo = hadEndo(), 
                                                      hysterectomy = input$hysterectomy,
                                                      colo.only = input$lowerBMI2.colo.only, 
                                                      endo.only = input$lowerBMI2.endo.only, 
                                                      endo.colo = input$lowerBMI2.endo.colo)

    # single cancer plot
    for(this.cancer in this.can){
      
      # only include reduced risk bars and legend item if an intervention is applied
      if( sum(rrCancers$cancers == this.cancer) == 1 &
          ((this.cancer == "Colorectal Cancer"  & (input$aspirin2 | input$colonoscopy2 | selected.w.loss.input > 0)) |
           (this.cancer == "Endometrial Cancer" & (input$hysterectomy.3 | selected.w.loss.input > 0)) | 
           (this.cancer == "Ovarian Cancer"     & input$oophorectomy.3) )){
        
        tmp.RR <- TRUE
        
        # cancer selected without intervention options or no intervention options selected for this cancer
      } else {
        
        tmp.RR <- FALSE
        
      }
      
      # prepare the data
      window.risks <- bar.data(df = carrier.df.2(),
                               useRR = tmp.RR,
                               cancer = this.cancer,
                               age = cur.age())
      
      # BAR GRAPH comparing carriers vs non-carriers
      this.plot <- make.ggbar(data = window.risks, 
                              cancer = this.cancer,
                              cancerView = this.can,
                              useRR = tmp.RR, 
                              age = cur.age(),
                              report = TRUE, 
                              grows = grid.rows5(),
                              gcols = grid.cols5(),
                              isMobile = is_mobile_device())

      # store the plot in a list
      cancer.num <- which(this.can == this.cancer)
      g[[cancer.num]] <- this.plot

    }

    # grid of individual cancer plots (does not draw)
    p0 <- arrangeGrob(grobs = g,
                      left = grid.y.axis.title,
                      bottom = grid.x.axis.title.bar,
                      nrow = grid.rows5()
    )

    # dana farber logo grob (does not draw)
    logo.grob <- rasterGrob(logo)

    # combine logo and settings table into a grob (does not draw)
    p1 <- arrangeGrob(grobs = list(logo.grob, interventionsTable(), settingsTable()),
                      ncol = 1,
                      heights = unit(c(logo.height, interventions.height, settings.height),"in")
    )

    # combine settings/logo grob with plot grob (does not draw)
    p2 <- arrangeGrob(grobs = list(p0, p1),
                      top = grid.report.title,
                      nrow = 1,
                      heights = unit(graph.height,"in"),
                      widths = unit(c(graph.width,settings.width),"in")
    )

    p2

  })
  
  
  ###### Waffle (no interventions) ####
  
  output$waffleNI <- renderPlot({
    
    # prepare the data
    window.risks <- waffle.data(df = carrier.df(), 
                                waffleTime = input$waffleTime, 
                                age = cur.age(),
                                cancerView = input$cancerView2)
    
    w <- create.waffle(cancerView = input$cancerView2, 
                       useRR = FALSE, 
                       cancersRR = rrCancers$cancers,
                       waffleTime = input$waffleTime, 
                       data = window.risks)
    
    w
    
  })
  
  ###### Waffle (CRC with colonoscopies only) ####
  
  output$waffleCRCscreen <- renderPlot({
    
    # prepare the data
    window.risks <- waffle.data(df = carrier.df(), 
                                waffleTime = input$waffleTime, 
                                age = cur.age(),
                                cancerView = input$cancerView2)
    
    w <- create.waffle(cancerView = input$cancerView2, 
                       useRR = TRUE, 
                       colonoscopies.only = TRUE,
                       cancersRR = rrCancers$cancers,
                       waffleTime = input$waffleTime, 
                       data = window.risks)
    
    w
    
  })
  
  
  ###### Waffle (interventions) ####
  
  output$waffleI <- renderPlot({
    
    # prepare the data
    window.risks <- waffle.data(df = carrier.df(), 
                                waffleTime = input$waffleTime2, 
                                age = cur.age(),
                                cancerView = input$cancerView4)
    
    w <- create.waffle(cancerView = input$cancerView4, 
                       useRR = TRUE, 
                       cancersRR = rrCancers$cancers,
                       waffleTime = input$waffleTime2, 
                       data = window.risks)
    
    w
    
  })
  
  
  ###### Waffle Report Preview (no interventions) ####
  
  output$wafflePreview <- renderPlot({
    
    # prepare the data
    window.risks <- waffle.data(df = carrier.df.2(), 
                                waffleTime = input$waffleTime3, 
                                age = cur.age(),
                                cancerView = input$cancerView6)
    
    w <- create.waffle(cancerView = input$cancerView6, 
                       useRR = TRUE, 
                       cancersRR = rrCancers$cancers,
                       waffleTime = input$waffleTime3, 
                       data = window.risks)
    
    w
    
  })
  

  ###### Waffle Report Preview (interventions) ####

  output$wafflePreviewRR <- renderPlot({

    # prepare the data
    window.risks <- waffle.data(df = carrier.df.2(),
                                waffleTime = input$waffleTime3,
                                age = cur.age(),
                                cancerView = input$cancerView6)

    w <- create.waffle(cancerView = input$cancerView6,
                       useRR = TRUE,
                       cancersRR = rrCancers$cancers,
                       waffleTime = input$waffleTime3, 
                       data = window.risks)

    w

  })
  
  
  ##### Waffle Switching (Report Preview) ####
  
  ## dynamically change the non-intervention plot just for CRC to show colonoscopies by default
  output$bigWaffleCRCscreen <- reactive({
    if(!is.null(input$cancerView2)){
      if(input$cancerView2 == "Colorectal Cancer"){ return(TRUE) } else { return(FALSE) }
    } else {
      return(FALSE)
    }
  })
  outputOptions(output, "bigWaffleCRCscreen", suspendWhenHidden = FALSE)
  
  # wrap plots in a renderUI output so that they respond to conditions on when to be displayed
  output$wafflePsmallCRCscreen = renderUI({
    plotOutput(outputId = "waffleNI", height = paste0(w.display.height,"px"))
  })
  
  output$wafflePlargeCRCscreen = renderUI({
    plotOutput(outputId = "waffleCRCscreen", height = paste0(w.display.height.rr,"px"))
  })
  
  ## dynamically change waffle display size based on if cancer has interventions or not
  output$bigWaffle <- reactive({
    if(!is.null(input$cancerView6)){
      if(input$cancerView6 %in% rrCancers$cancers){ return(TRUE) } else { return(FALSE) }
    } else {
      return(FALSE)
    }
  })
  outputOptions(output, "bigWaffle", suspendWhenHidden = FALSE)
  
  # wrap plots in a renderUI output so that they respond to conditions on when to be displayed
  output$wafflePsmall = renderUI({
    plotOutput(outputId = "wafflePreview", height = paste0(w.display.height,"px"))
  })
  
  output$wafflePlarge = renderUI({
    plotOutput(outputId = "wafflePreviewRR", height = paste0(w.display.height.rr,"px"))
  })
  
  
  ###### Waffle Report ####
  
  reportWaffle <- reactive({
    
    # prepare the data
    window.risks <- waffle.data(df = carrier.df.2(), 
                                waffleTime = input$waffleTime3, 
                                age = cur.age(),
                                cancerView = input$cancerView6)
    
    # create plot grob
    w <- create.waffle(cancerView = input$cancerView6, 
                       useRR = TRUE, 
                       cancersRR = rrCancers$cancers,
                       report = TRUE, 
                       waffleTime = input$waffleTime3, 
                       data = window.risks)
    
    # dana farber logo grob (does not draw)
    logo.grob <- rasterGrob(logo)
    
    # combine logo and settings table into a grob (does not draw)
    p1 <- arrangeGrob(grobs = list(logo.grob, interventionsTableWaffle(), settingsTableWaffle()),
                      ncol = 1,
                      heights = unit(mult * c(logo.height, interventions.height, settings.height),"in")
    )
    
    # combine settings/logo grob with plot grob (does not draw)
    grid.report.title.waffle <- textGrob(report.title, gp = gpar(fontsize = title.size+4))
    p2 <- arrangeGrob(grobs = list(w, p1),
                      top = grid.report.title.waffle,
                      nrow = 1,
                      heights = unit(mult * graph.height,"in"),
                      widths = unit(mult * c(graph.width,settings.width),"in")
    )
    
    p2
    
  })

  
  #### White Space Plot ####
  output$whitespace <- renderPlot({
    
    ggplot() + theme_void()
    
  })


  #### Download Report ####
  settingsTable <- reactive({

    table <- make.settings.table(ht_FT = input$heightFT, 
                                  ht_IN = input$heightIN, 
                                  weight = cur.weight(), 
                                  gender = input$gender, 
                                  age = cur.age(), 
                                  gene = input$gene, 
                                  ooph.status = had.ooph(), 
                                  hyst.status = had.hyst(),
                                  base.size = 8,
                                  hadEndo = hadEndo())
    
    table

  })
  
  settingsTableWaffle <- reactive({
    
    table <- make.settings.table(ht_FT = input$heightFT, 
                                  ht_IN = input$heightIN, 
                                  weight = cur.weight(), 
                                  gender = input$gender, 
                                  age = cur.age(), 
                                  gene = input$gene, 
                                  ooph.status = had.ooph(), 
                                  hyst.status = had.hyst(),
                                  base.size = 10,
                                  hadEndo = hadEndo())
    
    table
    
  })
  
  interventionsTable <- reactive({
    
    # which weight loss input to use
    selected.drop <- select.weight.loss.input(gene = input$gene, 
                                              Hweight = Hweight(), 
                                              gender = input$gender, 
                                              hadColo = hadColo(),
                                              hadEndo = hadEndo(), 
                                              hysterectomy = input$hysterectomy,
                                              colo.only = dropBMI.tmp.2.colo.only(), 
                                              endo.only = dropBMI.tmp.2.endo.only(), 
                                              endo.colo = dropBMI.tmp.2.endo.colo())
    
    table <- make.interventions.table(gene = input$gene, 
                                       gender = input$gender, 
                                       aspirin.status = aspirin.tmp.2(),
                                       colonoscopy.status = colonoscopy.tmp.2(),
                                       ht_FT = input$heightFT,
                                       ht_IN = input$heightIN,
                                       weight = cur.weight(),
                                       lowerBMI.selected = selected.drop,
                                       ooph.input = input$oophorectomy,
                                       hyst.input = input$hysterectomy,
                                       rr.ooph = reduce.risk.ooph.3(),
                                       rr.hyst = reduce.risk.hyst.3(),
                                       base.size = 8,
                                       hadColo = hadColo(),
                                       hadEndo = hadEndo(),
                                       hadOvar = hadOvar(),
                                       Hweight = Hweight(),
                                       any.int.avail = eligAnyIntervention())
    
    table
    
  })
  
  interventionsTableWaffle <- reactive({
    
    # which weight loss input to use
    selected.drop <- select.weight.loss.input(gene = input$gene, 
                                              Hweight = Hweight(), 
                                              gender = input$gender, 
                                              hadColo = hadColo(),
                                              hadEndo = hadEndo(), 
                                              hysterectomy = input$hysterectomy,
                                              colo.only = dropBMI.tmp.2.colo.only(), 
                                              endo.only = dropBMI.tmp.2.endo.only(), 
                                              endo.colo = dropBMI.tmp.2.endo.colo())
    
    table <- make.interventions.table(gene = input$gene, 
                                      gender = input$gender, 
                                      aspirin.status = aspirin.tmp.2(),
                                      colonoscopy.status = colonoscopy.tmp.2(),
                                      ht_FT = input$heightFT,
                                      ht_IN = input$heightIN,
                                      weight = cur.weight(),
                                      lowerBMI.selected = selected.drop,
                                      ooph.input = input$oophorectomy,
                                      hyst.input = input$hysterectomy,
                                      rr.ooph = reduce.risk.ooph.3(),
                                      rr.hyst = reduce.risk.hyst.3(),
                                      base.size = 10,
                                      hadColo = hadColo(),
                                      hadEndo = hadEndo(),
                                      hadOvar = hadOvar(),
                                      Hweight = Hweight(),
                                      any.int.avail = eligAnyIntervention())
    
    table
    
  })

  # reports for each visual tab
  output$report_line <- downloadHandler( filename = "My_Cancer_Risk_Line_Report.pdf",
                                         content = function(file) {
                                           pdf(file,
                                               onefile = TRUE,
                                               width = report.width,
                                               height = report.height)
                                           grid.draw(lineReport())
                                           dev.off()
                                         }
  )

  output$report_bar  <- downloadHandler( filename = "My_Cancer_Risk_Bar_Report.pdf",
                                         content = function(file) {
                                           pdf(file,
                                               onefile = TRUE,
                                               width = report.width,
                                               height = report.height)
                                           grid.draw(barReport())
                                           dev.off()
                                         }
  )
  
  output$report_waffle  <- downloadHandler( filename = "My_Cancer_Risk_Personograph_Report.PNG",
                                           content = function(file) {
                                             png(file,
                                                 width = report.width.waffle,
                                                 height = report.height.waffle)
                                             grid.draw(reportWaffle())
                                             dev.off()
                                           }
  )
  
  #### Email report ####
  sentReport <- reactiveVal(FALSE)
  observeEvent(input$emailReport,{
    if(input$displayOption != "Option 3"){
      if(input$displayOption == "Option 1"){
        fileName <- "My_Cancer_Risk_Line_Report.pdf"
        pdf(file = fileName,
            onefile = TRUE,
            width = report.width,
            height = report.height)
        grid.draw(lineReport())
      } else if(input$displayOption == "Option 2"){
        fileName <- "My_Cancer_Risk_Bar_Report.pdf"
        pdf(file = fileName,
            onefile = TRUE,
            width = report.width,
            height = report.height)
        grid.draw(barReport())
      }
    } else {
      fileName <- "My_Cancer_Risk_Personograph_Report.PNG"
      png(file = fileName,
          width = report.width.waffle,
          height = report.height.waffle)
      grid.draw(reportWaffle())
    }
    dev.off()
    emailUser(recipientEmail = input$reportEmail,
              emailType = "sendReport",
              reportFile = fileName)
    file.remove(fileName)
    sentReport(TRUE)
    updateTextInput(session, "reportEmail", value = "")
  })
  output$sentReport <- reactive({ sentReport() })
  outputOptions(output, 'sentReport', suspendWhenHidden = FALSE)
  
}

shinyApp(ui = ui, server = server)