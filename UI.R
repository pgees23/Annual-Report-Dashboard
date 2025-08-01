## Creating an R Shiny dashboard of SMSR Annual Report ##
## User Interface Script
## By George Addo Opoku-Pare
## and Rita Nogueira

# dashboard header
header <- dashboardHeader(title = "Scottish Mulitple Sclerosis Register: Annual National Report 2025",
                          titleWidth = 700,
                          tags$li(a(href = 'https://publichealthscotland.scot/',target="_blank",
                                    img(src = 'PHS logo, positive.png',
                                        title = "Click to visit the Public Health Scotland website", height = "50px"),
                                    style = "padding-top:10px; padding-bottom:10px;"),
                                  class = "dropdown")
)


# dashboard sidebar
sidebar <-   dashboardSidebar(
  tags$style(".left-side, .main-sidebar {padding-top: 70px}"),
  width=350,
  #tags$script(JS("document.getElementsByClassName('sidebar-toggle')[0].style.visibility = 'hidden';")), # Remove the sidebar toggle element
  sidebarMenu(id = "tabs", # Setting id makes input$tabs give the tabName of currently-selected tab
              #menuItem("Foreword", tabName = "foreword", icon = icon("list-alt")),
              #menuItem("Main Points", tabName = "mainpts", icon = icon("list-alt")),
              menuItem("Introduction", icon = icon("book"),
                       menuSubItem("Foreword", tabName = "foreword", icon = icon("pencil")),
                       menuSubItem("Introduction", tabName = "introduction", icon = icon("notes-medical")),
                       menuSubItem("Key points infographic", tabName = "infographic", icon = icon("image")),
                       menuSubItem("Background", tabName = "background", icon = icon("rectangle-list")),
                       menuSubItem("Incidence across Scotland", tabName = "Incidence-across-Scotland", icon = icon("arrow-trend-down"))
              ),
              menuItem("Governance", icon = icon("gavel"),
                       menuSubItem("SNAP Governance", tabName = "SNAP-Governance",icon = icon("scale-balanced")),
                       menuSubItem("National Clinical Standard", tabName = "National-Clinical-Standard",icon = icon("chart-line"))
                       # menuSubItem("Commentary", tabName = "Commentary",icon = icon("bullhorn")),
              ),
              menuItem("Context", icon = icon("folder-plus"),
                       menuSubItem("Incidence map", tabName = "Incidence-Map",icon = icon("map-location-dot")),
                       menuSubItem("Incidence rate by sex and age", tabName = "incidence-age-and-sex",icon = icon("venus-mars")),
                       menuSubItem("Deprivation", tabName = "Deprivation",icon = icon("house")),
                       menuSubItem("Investigations", tabName = "Investigations",icon = icon("magnifying-glass-chart")),
                       menuSubItem("Mortality - MS deaths", tabName = "Mortality",icon = icon("cross")),
                       menuSubItem("Mortality - causes of death", tabName = "MortalityCauses",icon = icon("cross")),
                       menuSubItem("Time to specialist nurse", tabName = "Weeks-from-confirmed-diagnosis-to-first-contact",icon = icon("calendar-days")),
                       menuSubItem("Time to Diagnosis", tabName = "Months-from-primary-referral-to-confirmed-diagnosis",icon = icon("calendar-week")),
                       menuSubItem("Disease modifying treatments (DMTs)", tabName = "results-DMT",icon = icon("prescription-bottle-medical"))
              ),
             
              menuItem("Quality Improvement - Research", tabName = "qi_research", icon = icon("th"),
                       menuSubItem("Anne Rowling Regenerative Neurology Clinic", tabName = "Anne-Rowling-Regenerative-Neurology-Clinic"),
                       menuSubItem("Revive MS Support", tabName = "Revive-MS-Support")
              ),
              menuItem("Conclusion",icon = icon("list-alt"),
                       menuSubItem("Acknowledgements", tabName = "Acknowledgements",icon = icon("handshake")),
                       menuSubItem("Contact", tabName = "Contact",icon = icon("envelope")),
                       menuSubItem("Glossary", tabName = "Glossary",icon = icon("book-open")),
                       menuSubItem("References", tabName = "References",icon = icon("book")),
                       menuSubItem("Data sources",tabName = "Data-sources", icon = icon("database")))
  )
)

# dashboard body
# source dashboards
dashboard_files <- list.files("Boards/",pattern=".R$")
sapply(paste0("Boards/",dashboard_files),source)

body <- dashboardBody(
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")),
  tags$script(HTML("$('body').addClass('fixed');")),
  tabItems(d_foreword,
           d_introduction,
           d_infographic,
           d_background,
           d_mainpts,
           d_Incidence_across_Scotland,
           d_SNAP_Governance,
           d_National_Clinical_Standard,
           d_incid_Map,
           d_incid_age_sex,
           d_deprivation,
           d_investigations,
           d_mortality,
           d_mortalityCause, 
           d_confirmedDiagnosis,
           d_primrefDiag,
           d_results_DMT,
           d_anne_rowling,
           d_revive_ms,
           d_acknowledgements,
           d_contact,
           d_glossary,
           d_references,
           d_dataSources
  )
)


rm(list=ls(pattern="d_"))


#RN - 26/03/2025 - added to deploy dashboard into a password protected site.
# Password Protection---------------------------------------
# KEEP IF PUBLISHING WITH PASSWORD PROTECTIONS
# COMMENT OUT IF PUBLISHING LIVE WITH NO PASSWORD PROTECTION
#ui <- secure_app(dashboardPage(header,sidebar,body)) #To be commented out when publishing live

# No Password Protection ------------------------------------------------------------------
# COMMENT OUT IF PUBLISHING WITH PASSWORD PROTECTIONS
# KEEP IF PUBLISHING LIVE WITH NO PASSWORD PROTECTION
ui <- dashboardPage(header,sidebar,body) 