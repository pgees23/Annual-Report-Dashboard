## SMSR Dashboard for Annual Report

#install.packages("rsconnect")

## Deploy App with password protection

rsconnect::setAccountInfo(name='scotland', 
                          token='258388766211AC8A2A1A158D0804CE0D', 
                          secret='LxKKe4ZudtR5DaQ813g39hO5kTLY9YI9Xlv0vuCX')


#### ADD "-2025" TO THE END OF LINK TO MAKE SURE IT IS PUBLISHED WITH A DATE!!! ----
#### update each year!!!! ----
#### ONLY NEED ONE LINK GOING FORWARD, DEPLOY FOR PRE RELEASE WITH PASSWORD AND 
#THEN GENERAL RELEASE WITHOUT SO TIME STAMP AND LINK REMAIN CONSISTENT ----

# Password protected app
# COMMENT OUT IF PUBLISHING LIVE WITH NO PASSWORD PROTECTION
rsconnect::deployApp('/conf/quality/msaudit/Active/Shiny', appName= "PHS-SMSR-2025")


# app link: https://scotland.shinyapps.io/phs-SMSR-Draft/

# If the above code fails try typing the following first:
# renv::settings$snapshot.type("explicit")
# options(rsconnect.packrat = TRUE)