## R Shiny dashboard of SMSR Annual Report ##
## Server Script
## By George Addo Opoku-Pare

# Define server logic----
server <- function(input, output, session) {
  
# Password protection (credentials) 
  credentials <- readRDS("credentials.rds")
 
  
# Password protection (secure server) 
  res_auth <- secure_server(
    check_credentials = check_credentials(credentials))
  
  output$auth_output <- renderPrint({reactiveValuesToList(res_auth)
  })
  ### Incidence across Scotland----
  # Filter DF based on drop-down menu selection
  incidence_numbers_select <- reactive({ 
    incidence_numbers %>%
      mutate(across(where(is.numeric), ~ round(., 2))) %>%
      dplyr::rename(Health_Board = Hblabel) %>% 
      subset(Health_Board == input$Incidence_across_Scotland_Dropdown)  # filter DF to the year selected by user
  })
  
  ### Chart Title----
  output$Incidence_across_Scotland_table_title <- renderText({ 
    ## Create reactive plot title
    paste0(input$Incidence_across_Scotland_Dropdown, " ")
  }) 
  
  ### Chart - Incidence across Scotland----
  Incidence_across_Scotland_chart <- reactive({ 
    ggplot(incidence_numbers_select (),
           aes(
             x = Year,
             y = Rate,
             label1 = Year,
             label2 = Rate
           )
    ) +
      geom_point(colour = phs_pu1, size = 4) +
      geom_line(colour = phs_pu1) +
      # geom_bar(stat = "identity", position = "dodge", fill = "#0078D4") +
      labs(title = " ", x = "Year", y = "Rate per 100,000",size = 3) + 
      scale_y_continuous(limits = c(0, NA)) +
      theme_bw() +
      theme(
        axis.text.x = element_text(angle = 0, hjust = 1, vjust = 1),
        panel.border = element_blank(),
        plot.background = element_blank(),
        legend.title = element_blank()
      )
  })
  
  ### Render Chart as Plotly----
  output$Incidence_across_Scotland_chart<- renderPlotly({
    ggplotly(Incidence_across_Scotland_chart(), tooltip = c("label1", "label2")) %>%
      layout(
        legend = list(
          orientation = "h",
          x = 0.5,
          xanchor = "center",
          y = -0.2,
          title = list(text = NULL)  
        ),
        plot_bgcolor = 'white',      
        paper_bgcolor = 'white'      
      )
  })
  
  ### Table Title ----
  output$Incidence_across_Scotland_table_title <- renderText({ 
    paste0("The table below contains the number of patients in the MS register and corresponding incidence rate by NHS board and calendar year")
  })
  ### Table Sub-title----
  output$Incidence_across_Scotland_table_subtitle <- renderText({ 
    paste0("Please use the search box in the right corner to filter by NHS board")
  }) 
  
  ### Create Table ----
  Incidence_across_Scot <- incidence_numbers %>% 
    dplyr::select(3,2,1,4) %>% 
    dplyr::rename("NHS Health Board"= Hblabel,"Incidence rate" = Rate, "Number of new MS registrations"= N_BREAK)
  
  ###Generate Table ----
  output$Incidence_across_Scotland_table <- DT::renderDataTable({ # Use reactive expression to generate data table
    DT::datatable(Incidence_across_Scot,
                  options = list(pageLength = 25, scrollX = TRUE), 
                  rownames = FALSE)
  })
  
  ###Download Button----
  output$download_Incidence_across_Scot <- downloadHandler(
    filename = function() {
      "Incidence across Scotland.csv"
    },
    content = function(file) {
      write.csv(Incidence_across_Scot, file, row.names = FALSE)
    }
  )  
  
##Create governance chart----
  SNAP_Governance_chart <- reactive({ 
    
### Apply random jitter to y-values for 100% compliance ##
    funnelRates$jittered_y <- funnelRates$COMPLIANCE + ifelse(funnelRates$COMPLIANCE == 1, 
                                                              runif(n = nrow(funnelRates), min = -0.09, max = 0.09), 
                                                              0) 
    
    ## Round COMPLIANCE to 2 decimal places ##
    funnelRates$COMPLIANCE <- round(funnelRates$COMPLIANCE, 2)
    
    ## Funnel Chart
    ggplot() + 
      geom_line(data = lkup, aes(x = N, y = P, col = Index, linetype = Index), show.legend = FALSE) +  
      
      scale_color_manual(values = c("Scotland" = "#0078D4",   # Blue for Scotland
                                    "+2SD" = "#3F3685",       
                                    "-2SD" = "#3F3685",       
                                    "+3SD" = "#3F3685",       
                                    "-3SD" = "#3F3685")) +    
      
      scale_linetype_manual(values = c("Scotland" = "solid",  # Solid line for Scotland
                                       "+2SD" = "dotted",     
                                       "-2SD" = "dotted",     
                                       "+3SD" = "solid",      
                                       "-3SD" = "solid")) +   
      
      geom_point(data = funnelRates, aes(x = N_TOTAL, y = COMPLIANCE, 
                                         text = paste("Health Board: ", HB, 
                                                      "<br>Patients: ", N_TOTAL, 
                                                      "<br>Compliance: ", scales::percent(COMPLIANCE))), size = 1.0) +
      
      geom_text(data = funnelRates, aes(x = N_TOTAL, y = jittered_y , label = HB), size = 3.0, check_overlap = TRUE) +
      
      geom_hline(aes(yintercept = COMPLIANCE_SCOT,
                     color = "Scotland", 
                     linetype = "Scotland")) +  # Keep Scotland in legend
      
      labs(x = "Number of Patients",
           y = "Percentage (%) meeting standard",
           colour = " ",
           linetype = " ") +
      
      xlim(0, 115) +
      scale_y_continuous(labels = scales::percent, limits = c(0.6, 1.2)) +
      theme_minimal()  
  })
  
  
  ### Generate Chart
  output$SNAP_Governance_chart <- renderPlotly({
    ggplotly(SNAP_Governance_chart(), tooltip = c("text")) %>%
      layout(
        legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.2),  # Ensures legend is centered at the bottom
        plot_bgcolor = 'white',  # Explicitly set the background to white
        paper_bgcolor = 'white'   # Ensures the plotly container has a white background
      )
  })
  
  # National Clinical Standard----
  ## Create Chart----
  National_Clinical_Standard_chart <- reactive({ # Create reactive plot
    Data_standard_p <- Data_standard %>% 
      mutate(STD_PC_round = round(STD_PC,1),
             STD_PC_round_char =paste0(as.character(STD_PC_round),"%"))
    
    ggplot(Data_standard_p,
           aes(x = year, y = STD_PC_round)) +
      geom_line(colour ="#3F3685" ) +
      geom_point(colour = "#3F3685") +
      geom_text(aes(label = STD_PC_round_char),nudge_x = 0.5) +
      scale_x_continuous(breaks = seq(2010,2024, by=1), labels = seq(2010,2024, by = 1)) +
      labs(x = " ",
           y = "Percentage(%) meeting standard") +
      theme_bw() +
      theme(
        axis.title.x = 
          element_text(
            size = 10,
            face = "bold",
            colour = "black"
          ),
        axis.title.y = 
          element_text(
            size = 10,
            face = "bold",
            angle  = 360,
            colour = "black"
          ),
        panel.border = element_blank(),    # Remove the panel border
        plot.background = element_blank(),
        legend.title = element_blank()) 
    
  })
  
  ## Generate Chart 
  output$National_Clinical_Standard_chart <- renderPlotly({
    ggplotly(National_Clinical_Standard_chart(), tooltip = c("label1","label2","label3"))
    
  })
  
  ## Table Title ----
  output$National_Clinical_Standard_table_title <- renderText({ # Create reactive table title
    paste0("Percentage of patients with a new diagnosis of MS contacted by MS specialist nurse within 10 working days of confirmed diagnosis")
  })
  
  
  ##Create Table ----
  # Use reactive expression to generate data table
  
  ## National_Clinical_Standard_table
  National_Clinical_Standard_table <- Data_standard_p %>%
    dplyr::select(2,5,6,8) %>% 
    dplyr::rename("Number of patients with a confirmed diagnosis"= N_TOTAL, "Number of patients contacted by MS specialist nurse within 10 working days of confirmed diagnosis"= N_BREAK,
                  "Percentage meeting stardard"= STD_PC_round_char)
  
  ##Generate National Clinical Standard Table
  output$National_Clinical_Standard_table <- DT::renderDataTable({ 
    DT::datatable(National_Clinical_Standard_table, 
                  options = list(pageLength = 27, scrollX = TRUE), 
                  rownames = FALSE)
  })
  
  ##Download button----
  output$download_Nat_Clin_Stdrd <- downloadHandler(
    filename = function() {
      "National Clinical Standard.csv"
    },
    content = function(file) {
      write.csv(National_Clinical_Standard_table, file, row.names = FALSE)
    }
  )  
  
  # Incidence Map----
  output$Incidence_Map_Chart <- renderLeaflet({
    
    # Renaming Health Boards for consistency 
    Intro_Shape <- Intro_Shape %>% 
      mutate(HBName = case_when(
        HBName == "Ayrshire and Arran" ~ "NHS A&A",
        HBName == "Borders" ~ "NHS Borders",
        HBName == "Dumfries and Galloway" ~ "NHS D&G",
        HBName == "Forth Valley" ~ "NHS Forth Valley",
        HBName == "Grampian" ~ "NHS Grampian",
        HBName == "Highland" ~ "NHS Highland",
        HBName == "Lothian" ~ "NHS Lothian",
        HBName == "Orkney" ~ "NHS Orkney",
        HBName == "Shetland" ~ "NHS Shetland",
        HBName == "Western Isles" ~ "NHS Western Isles",
        HBName == "Fife" ~ "NHS Fife",
        HBName == "Tayside" ~ "NHS Tayside",
        HBName == "Greater Glasgow and Clyde" ~ "NHS GG&C",
        HBName == "Lanarkshire" ~ "NHS Lanarkshire",
        TRUE ~ HBName
      ))
    
    # Merging Incident numbers with Hospital Coordinates  
    Incid_Map <- merge(incidence_numbers,Hospital_coordinates, by = "Hblabel", all = TRUE)
    
    # Sorting shape file
    Intro_Shape <- Intro_Shape%>%
      dplyr::rename(Hblabel = HBName) %>%
      left_join(Incid_Map, by="Hblabel")
    
    # Add x and y columns
    Intro_Shape <- Intro_Shape %>% mutate(X = Longitude, Y = Latitude)
    
    # Factor palette for coloring the health boards
    Factor_palette <- colorFactor("magma", unique(Intro_Shape$Hblabel))
    
    # Filter for the last 5 years and calculate the average incidence
    current_year <- max(Intro_Shape$Year)  # Find the latest year
    Intro_Shape <- Intro_Shape %>%
      filter(Year >= (current_year - 4)) # Filter data for the last 5 years
    
    Intro_Shape <- Intro_Shape %>%
      group_by(Hblabel) %>%  # Group by Health Board
      summarise(
        Average_Incidence = mean(Rate, na.rm = TRUE),  # Calculate average incidence
        X = first(Longitude),  # Keep the longitude
        Y = first(Latitude),  # Keep the latitude
        Year = paste0((current_year - 4), "-", current_year))  # Label Year as a range
    
    # Define the location pin icon using Font Awesome
    pin_icon <- awesomeIcons(
      icon = 'hospital',  # Font Awesome location pin icon
      markerColor = "darkpurple",  # Red color for the pin
      iconColor = "#FFFFFF",  # White color for the icon itself
      library = 'fa'  # Use Font Awesome library
    )
    
    ## Create the map----
    leaflet(Intro_Shape) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = -4.2026, lat = 56.4907, zoom = 6) %>%
      addPolylines(data = Intro_Shape,
                   color = Factor_palette(Intro_Shape$Hblabel),
                   weight = 2) %>%
      addAwesomeMarkers(., ~X, ~Y, icon = pin_icon,  # Use the location pin icon
                        popup = paste0("<strong>", 
                                       Intro_Shape$Hblabel,
                                       "</strong>",
                                       "<br><br><strong>Year: </strong>", Intro_Shape$Year,
                                       "<br><strong>Average annual incidence: </strong>", Intro_Shape$Average_Incidence
                        ))
  })
  
  ##Incidence (map) Table----
  #Calculate Scotland rate
  map_incidence_table <- incidence_numbers %>% 
    filter(Year >= (max(Year)  - 4)) %>% #select last 5 years
    group_by(Hblabel) %>%  # Group by Health Board
    summarise(Average_Incidence = mean(Rate, na.rm = TRUE)) %>% 
    mutate(across(where(is.numeric), ~ round(., 2))) %>% 
    dplyr::rename('NHS board' = Hblabel,
                  'Average incidence rate' = Average_Incidence)
  
  ##Render table
  output$map_incidence_table  <- DT::renderDataTable({
    DT::datatable(map_incidence_table, 
                  options = list(pageLength = 27, scrollX = TRUE), 
                  rownames = FALSE)
  })
  
  ## Download button----
  output$download_map_incidence_table <- downloadHandler(
    filename = function() {
      "Five year incidence rate average.csv"
    },
    content = function(file) {
      write.csv(map_incidence_table, file, row.names = FALSE)
    }
  ) 
  
  # Incidence Rate: Age and Gender ----
  
  ## Chart Title ----
  output$Incid_age_sex_chart_title <- renderText({ # Create reactive plot title
    
    if(input$Incid_age_sex_Dropdown == "Age" ) {
      paste0("Fig 2. Average annual age specific incidence of people diagnosed with MS in Scotland per 100,000 population, by sex, 2010-2024")}
    
    else if(input$Incid_age_sex_Dropdown == "Sex and year of birth" ) {
      paste0("Fig 2a. Number of people diagnosed with MS in Scotland, by sex and year of birth, 2010-2024")}
  })
  
  ## Create Chart ---- 
  Incid_age_sex_graph <- reactive({
    if (input$Incid_age_sex_Dropdown == "Age") {
      ggplot(
        agegenderrate %>%
          filter(!AgeGroup == "5-9", !is.na(SEX)) %>%  # Remove NA values
          mutate(across(where(is.numeric), ~ round(., 2))) %>%
          dplyr::rename(Age group = AgeGroup, Rate = RATE),
        aes(
          x = Age group, 
          colour = SEX, 
          group = SEX, 
          y = Rate, 
          shape = SEX, 
          label1 = Age group, 
          label2 = SEX, 
          label3 = Rate
        )
      ) +
        geom_point(size = 3) +
        geom_line() +
        labs(title = " ", x = "Age Group", y = "Rate per 100,000") +
        scale_color_manual(values = c(phs_bl1, phs_pu1, phs_ru1), na.translate = FALSE) +  # Remove NA legend entry
        scale_shape_manual(values = c(16, 17, 18), na.translate = FALSE) +  # Remove NA legend entry
        theme_bw() +
        theme(
          axis.text.x = element_text(angle = 0, hjust = 1, vjust = 1),
          panel.border = element_blank(),    
          plot.background = element_blank(),
          legend.title = element_blank()
        )
    }
    else if (input$Incid_age_sex_Dropdown == "Sex and year of birth") {
      ggplot(
        dobgender %>%
          dplyr::rename(Age_group = age_group, Number = N_BREAK),
        aes(
          x = Age_group, 
          y = Number, 
          fill = Gender, 
          label1 = Age_group, 
          label2 = Number, 
          label3 = Gender
        )
      ) +
        geom_bar(stat = "identity", position = "dodge") +
        labs(title = " ", x = "Year of Birth", y = "Number of People") +
        scale_fill_manual(values = c(phs_pu1, phs_ru1)) +
        theme_bw() +
        theme(
          axis.text.x = element_text(angle = 0, hjust = 1, vjust = 1),
          panel.border = element_blank(),
          plot.background = element_blank(),
          legend.title = element_blank()
        )
    }
  })
  
  ### Render the Chart as Plotly
  output$Incid_age_sex_chart <- renderPlotly({
    ggplotly(Incid_age_sex_graph(), tooltip = c("label1", "label2", "label3")) %>%
      layout(
        legend = list(
          orientation = "h",
          x = 0.5,
          xanchor = "center",
          y = -0.2,
          title = list(text = NULL)  
        ),
        # Set hovermode to 'x unified' for compare data on hover
        hovermode = "x unified", # Enables compare data on hover
        plot_bgcolor = 'white',      
        paper_bgcolor = 'white'      
      )
  })
  
  ## Table Title ----
  output$Incid_age_sex_table_title <- renderText({ # Create reactive table title
    
    if(input$Incid_age_sex_Dropdown == "Age") {
      paste0("Average annual age-specific incidence rate of people diagnosed with MS in Scotland per 100,000 population, by sex, 2010-2024")}
    
    else if(input$Incid_age_sex_Dropdown == "Sex and year of birth" ) {
      paste0("Number of people diagnosed with MS in Scotland, by sex and year of birth, 2010-2024")}
  })
  
  ## Create Table ----
  Age_table <- agegenderrate %>%
    dplyr::select(c(2,1,6)) %>% 
    filter(!AgeGroup == "5-9") %>% 
    dplyr::rename("Age Group" = AgeGroup)
  
  Age_table <- Age_table %>%
    pivot_wider(names_from = SEX, values_from = RATE)
  
  Age_table <- Age_table %>%
    mutate(across(where(is.numeric), ~ round(.,2)))
  
  ## Sex and Year of Birth Table
  
  Sex_Year_table <- dobgender %>%
    dplyr::select(c(1,2,3)) %>% 
    dplyr::rename("Year of birth" = age_group) %>% 
    arrange(1)
  
  Sex_Year_table  <- Sex_Year_table %>%
    pivot_wider(names_from = Gender, values_from = N_BREAK)
  
  
  ## Generate Table ----
  output$Incid_age_sex_table <- DT::renderDataTable({ # Use reactive expression to generate data table
    ## Age table
    if(input$Incid_age_sex_Dropdown == "Age"){
      DT::datatable(Age_table, 
                    options = list(pageLength = 27, scrollX = TRUE), 
                    rownames = FALSE)
    }
    # Sex and year of birth table
    else if(input$Incid_age_sex_Dropdown == "Sex and year of birth"){
      DT::datatable(Sex_Year_table, 
                    options = list(pageLength = 27, scrollX = TRUE), 
                    rownames = FALSE)
    }
  })
  
  ##Download button----
  output$download_age_sex_Table <- downloadHandler(
    filename = function() {
      if (input$Incid_age_sex_Dropdown == "Age") {
        "MS Incidence by age group.csv"
      } else if (input$Incid_age_sex_Dropdown == "Sex and year of birth") {
        "MS Incidence by sex and date of birth.csv"
      } else {
        "Age Gender rate.csv"
      }
    },
    content = function(file) {
      if (input$Incid_age_sex_Dropdown == "Age") {
        write.csv(Age_table, file, row.names = FALSE)
      } else if (input$Incid_age_sex_Dropdown == "Sex and year of birth") {
        write.csv(Sex_Year_table, file, row.names = FALSE)
      }
    }
  )
  
  #Deprivation----
  ## Deprivation Chart ---- 
  Deprivation_graph <- reactive({
    ggplot(
      deprivation %>% 
        filter(!is.na(simd_quintile)) %>% 
        filter(hb == input$Deprivation_HB_Dropdown & diag_year == input$Deprivation_year_Dropdown) %>% 
        dplyr::mutate(simd_quintile = as.character(simd_quintile),
                      simd_quintile = case_when(simd_quintile == "1" ~ "1 (most deprived)",
                                                simd_quintile == "2" ~ "2",
                                                simd_quintile == "3" ~ "3",
                                                simd_quintile == "4" ~ "4",
                                                simd_quintile == "5" ~ "5 (least deprived)")) %>% 
        mutate(across(where(is.numeric), ~ round(., 2))) %>% 
        dplyr::rename("Year" = diag_year,
                      "Quintile" = simd_quintile,
                      "NHS board" = hb, 
                      "Rate" = MSRate),
      aes(x = Quintile, y = Rate, 
          label1 = Quintile, 
          label2 = Rate
      )) +
      geom_bar(stat = "identity", position = "dodge",fill = "#3F3685", color = NA) +
      labs(title = " ", x = "SIMD quintile", y = "MS Rate per 100,000 pop") +
      scale_fill_manual(values = c(phs_pu1, phs_ru1)) +
      theme_bw() +
      theme(
        axis.text.x = element_text(angle = 0, hjust = 1, vjust = 1),
        panel.border = element_blank(),
        plot.background = element_blank(),
        legend.title = element_blank()
      )
  })
  
  ### Render the Chart as Plotly
  output$Deprivation_chart <- renderPlotly({
    ggplotly(Deprivation_graph(), tooltip = c("label1")) %>%
      layout(
        legend = list(
          orientation = "h",
          x = 0.5,
          xanchor = "center",
          y = -0.2,
          title = list(text = NULL)  
        ),
        plot_bgcolor = 'white',      
        paper_bgcolor = 'white'      
      )
  })  
  
  ##Create Table----
  deprivation_table <- deprivation %>% 
    dplyr::mutate(simd_quintile = as.character(simd_quintile),
                  simd_quintile = case_when(simd_quintile == "1" ~ "1 (most deprived)",
                                            simd_quintile == "2" ~ "2",
                                            simd_quintile == "3" ~ "3",
                                            simd_quintile == "4" ~ "4",
                                            simd_quintile == "5" ~ "5 (least deprived)")) %>% 
    dplyr::mutate(across(where(is.numeric), ~ round(.,2))) %>% 
    dplyr::rename(Calendar_Year = diag_year,
                  Health_Board_of_Residence = hb,
                  SIMD_quintile = simd_quintile,
                  MS_diagnosis_rate_per_100 = MSRate) %>% 
    dplyr::select(-4,-5)
  
  #render table
  output$deprivation_table <- DT::renderDataTable({ # Use reactive expression to generate data table
    DT::datatable(deprivation_table,
                  options = list(pageLength = 25, scrollX = TRUE), 
                  rownames = FALSE)
  })
  
  
  ##Download button----
  output$download_deprivation_table <- downloadHandler(
    filename = function() {
      "MS deprivation.csv"
    },
    content = function(file) {
      write.csv(deprivation_table, file, row.names = FALSE)
    }
  )  
  
  # Investigations ----
  # Filter DF based on drop-down menu selection
  Investigations_select <- reactive({
    investigations %>%
      filter(year == input$Investigations_Year_Dropdown, Ind == input$Investigations_Dropdown) %>%
      mutate(Ind = dplyr::recode(Ind,
                                 "LumPunc" = "New Label 1",
                                 "SPMRI" = "New Label 2",
                                 "OliBands" = "New Label 3")) %>%
      group_by(HB) %>%
      mutate(PERCENT = 100 * PERCENT / sum(PERCENT, na.rm = TRUE)) %>%  # Ensure bars sum to 100%
      ungroup() %>%
      mutate(across(where(is.numeric), ~ round(., 2))) %>%
      dplyr::rename(Health_Board = HB, Percentage = PERCENT, Response = value, Investigations = Ind) %>%
      mutate(Response = factor(Response, levels = c("Yes","No","Not Known")))
  })
  
  ## Chart Title ----
  output$Investigations_chart_title <- renderText({ 
    paste0(input$Investigations_Dropdown, " (", input_Dropdown, ")")
  })
  
  ## Generate Chart ----
  output$Investigations_chart <- renderPlotly({
    # Get the filtered data
    data <- Investigations_select()
    
    # Create the Plotly stacked bar chart
    p <- plot_ly(
      data = data,
      x = ~Percentage,
      y = ~Health Board,
      color = ~Response,
      colors = c("Yes" = "#3F3685", "No" = "#C73918", "Not Known" = "#0078D4"),
      type = "bar",
      orientation = "h",
      hoverinfo = "text",
      text = ~paste("Percentage: ", Percentage, "<br>Health Board: ", Health Board, "<br>Response: ", Response)
    ) %>%
      layout(
        title = "",
        xaxis = list(
          title = "Percentage",
          range = c(0, 100),  # Ensure the x-axis always goes from 0 to 100
          showgrid = TRUE,
          zeroline = FALSE
        ),
        yaxis = list(
          title = "",
          categoryorder = "array",
          categoryarray = rev(unique(sort(data$Health_Board))),  # Reverse order for proper stacking
          showgrid = TRUE,
          zeroline = FALSE
        ),
        legend = list(
          orientation = "h",
          x = 0.5,
          xanchor = "center",
          y = -0.2,
          title = list(text = NULL)
        ),
        barmode = "relative",  # Ensures bars stack to 100%
        plot_bgcolor = "white",
        paper_bgcolor = "white"
      )
    
    p
    
  })
  
  ## Table Titles ----
  output$Investigations_table_title <- renderText({ # Create reactive table title
    paste0("Investigations split by NHS board (of diagnosis) and year of diagnosis")
  })
  # Table Title b
  output$Investigations_table_title <- renderText({
    paste0("Investigations split by NHS board (of diagnosis) and year of diagnosis")
  })
  
  ## Create Tables ----
  # Investigation_year table
  Investigations_table <- investigations %>%
    dplyr::filter(!Ind == "99") %>% 
    dplyr::mutate(across(where(is.numeric), ~ round(.,2)),
                  value = case_when(value == "Not Known" ~ "Unknown",
                                    value == "Yes" ~ "Yes",
                                    value == "No" ~ "No")) %>% 
    dplyr::rename("NHS board" = HB,
                  "Calendar year" = year,
                  "Investigation carried out" = value,
                  "Investigation type" = Ind,
                  "Number of investigations" = N_BREAK,
                  "Number of MS registrations" = N_TOTAL,
                  "Percentage of investigations (%)" = PERCENT)
  
  #  Generate Investigations Tables ----
  output$Investigations_table <- DT::renderDataTable({
    DT::datatable(Investigations_table, 
                  options = list(pageLength = 27, scrollX = TRUE), 
                  rownames = FALSE)
  })
  
  ## Download button
  output$download_investigations_table <- downloadHandler(
    filename = function() {
      "Investigations.csv"
    },
    content = function(file) {
      write.csv(Investigations_table, file, row.names = FALSE)
    }
  )  
  
  # Mortality----
  ## Create MS Deaths Chart ----
  Mortality_chart <-ggplot(MS_Deaths %>%
                             dplyr::rename(Year = YEAR_OF_DEATH, Number = Number) %>%
                             filter(Year != "Grand Total"),
                           aes(x = Year , y = Number,
                               label1 = Year, label2 = Number)) +
    geom_bar(stat = "identity", size = 1, fill = "#3F3685", color = NA) + # Removed the outline border by setting color to NA
    labs(title = " ", x = "Year ", y = "Number of deaths") +
    theme_bw() +
    theme(axis.text.x.bottom = element_text(angle = 0, hjust = 1, vjust = 1),
          panel.border = element_blank(),    # Remove the panel border
          plot.background = element_blank(),
          legend.title = element_blank())
  
  # Generate Chart
  output$Mortality_chart <- renderPlotly({
    ggplotly(Mortality_chart,tooltip = c("label1","label2"))
  })
  
  ##create Table----
  output$Deaths_table <- DT::renderDataTable({ # Use reactive expression to generate data table
    DT::datatable(MS_Deaths %>% 
                    dplyr::rename(Year_of_death = YEAR_OF_DEATH,
                                  Number_of_deaths = Number),
                  options = list(pageLength = 25, scrollX = TRUE), 
                  rownames = FALSE)
  })
  
  ## Download Table-----
  # Button
  output$download_deaths_table <- downloadHandler(
    filename = function() {
      "MS Deaths.csv"
    },
    content = function(file) {
      write.csv(output$Deaths_table, file, row.names = FALSE)
    }
  )  
  
  
  ## Common Complications with MS Deaths Chart ----
  
  Mortality_cause_chart <- reactive({ 
    ggplot(
      Top_five_causes %>%
        dplyr::rename(Complication = Listed_cause,
                      'Number of deaths' = count)  %>%
        dplyr::filter(TimePeriod == input$MS_Deaths_Cause_Dropdown) %>% 
        dplyr::select(-1) %>%
        mutate(Complication = fct_reorder(Complication, Number_of_deaths, .desc = FALSE)),
      aes(x = Number_of_deaths, y = Complication,
          label1 = Number_of_deaths, label2 = Complication)
    ) +
      
      geom_bar(stat = "identity", size = 0.5, fill = "#3F3685", color = NA) +
      labs(x = "Number of deaths", y = "") +
      theme_bw() +
      theme(axis.text.x.bottom = element_text(angle = 0, hjust = 1, vjust = 1),
            panel.border = element_blank(),    # Remove the panel border
            plot.background = element_blank(),
            legend.title = element_blank())
  })
  
  ###Generate Chart----
  output$Mortality_cause_chart <- renderPlotly({
    ggplotly(Mortality_cause_chart(),tooltip = c("label1","label2"))
    
  })
  
  ###Generate Table----
  
  output$death_causes_table <- DT::renderDataTable({ # Use reactive expression to generate data table
    DT::datatable(Top_five_causes %>% 
                    dplyr::rename(Time_period = TimePeriod,
                                  Listed_cause_of_death = Listed_cause,
                                  Number_of_deaths = count) %>% 
                    dplyr::select(-2),
                  options = list(pageLength = 25, scrollX = TRUE), 
                  rownames = FALSE)
  })
  
  ### Download Table-----
  # Button
  output$download_death_causes_table <- downloadHandler(
    filename = function() {
      "Causes of deaths.csv"
    },
    content = function(file) {
      write.csv(death_causes_table, file, row.names = FALSE)
    }
  )  
  
  
  # Weeks from diagnosis to first contact ----
  # Filter DF based on drop-down menu selection
  Diag2cont_select <- reactive({ 
    diag2cont %>%
      mutate(across(where(is.numeric), ~ round(., 2))) %>%
      subset(year == input$Weeks_from_Diagnosis_Year_Dropdown) %>% # filter df to the year selected by user
      dplyr::rename(Percentage = STD_PC, Health_Board = HB)
  })
  
  
  ## Chart Title ----
  output$Weeks_from_Diagnosis_chart_title <- renderText({ 
    ## Create reactive plot title
    paste0(input$Weeks_from_Diagnosis_Year_Dropdown, " ")
  })
  ## Create Chart----
  Weeks_from_Diagnosis_chart <- reactive({ 
    ggplot(Diag2cont_select(),
           aes(x = Percentage, y = Health_Board, fill = forcats::fct_rev(Time),
               label1 = Percentage, label2 = Health_Board, label3 = Time)) + 
      geom_bar(stat = "identity", position = "stack", size = 1) +
      labs(title = " ", x = "Percentage", y = " ") +
      scale_fill_manual(values = c( "<=2 weeks" = "#0078D4",
                                    ">2 and <=4 weeks" = "#1E7F84",
                                    ">4 and <=6 weeks" = "#3F3685",
                                    ">6 weeks" =  "#C73918"),
                        limits = c("<=2 weeks",
                                   ">2 and <=4 weeks",
                                   ">4 and <=6 weeks",
                                   ">6 weeks")) + 
      scale_y_discrete(limits = c("Scotland","NHS Western Isles","NHS Tayside",
                                  "NHS Shetland","NHS Orkney","NHS Lothian",
                                  "NHS Lanarkshire","NHS Highland","NHS Grampian",
                                  "NHS GG&C","NHS Forth Valley","NHS Fife",
                                  "NHS D&G","NHS Borders","NHS A&A")) +
      scale_x_continuous()     
  })
  
  ## Generate Plotly chart
  output$Weeks_from_Diagnosis_chart <- renderPlotly({
    
    ## Convert ggplot to plotly
    p <- ggplotly(Weeks_from_Diagnosis_chart(), tooltip = c("label1", "label2", "label3"))
    
    ## Extract and reorder traces to match desired legend order
    trace_order <- c("<=2 weeks", ">2 and <=4 weeks", ">4 and <=6 weeks", ">6 weeks")
    
    ## Create a list to store reordered traces
    reordered_traces <- list()
    
    ## Loop through each desired order and find the matching trace
    for (trace_name in trace_order) {
      trace <- p$x$data[[which(sapply(p$x$data, function(x) x$name) == trace_name)]]
      reordered_traces <- c(reordered_traces, list(trace))
    }
    
    ## Replace the existing traces with the reordered traces
    p$x$data <- reordered_traces
    
    # Apply layout customizations
    p <- p %>%
      layout(
        title = "",
        xaxis = list(
          title = "Percentage",
          tickangle = 0,
          showgrid = TRUE,
          zeroline = FALSE
        ),
        yaxis = list(
          title = "",
          showgrid = TRUE,
          zeroline = FALSE
        ),
        legend = list(
          title = list(text = ""),
          orientation = "h",
          x = 0.5,
          xanchor = "center",
          y = -0.2
        ),
        plot_bgcolor = "#ffffff",
        paper_bgcolor = "#ffffff",
        margin = list(t = 40, b = 60, l = 50, r = 50)
      )
    
    p
    
  })
  
  ## Table Title ----
  output$Weeks_from_Diagnosis_table_title <- renderText({ # Create reactive table title
    paste0("Percentage of people newly diagnosed with MS in Scotland, by number of weeks from confirmed diagnosis to first contact with a MS specialist nurse")
  })
  
  
  ## Create Tables ----
  
  
  ### Weeks from diagnosis to first contact Table
  Weeks_from_Diagnosis_table <- diag2cont %>%
    #filter(year == input$Weeks_from_Diagnosis_Year_Dropdown) %>%
    pivot_wider(names_from = Time, values_from = N_BREAK, values_fill = list(N_BREAK = 0)) %>% 
    mutate(across(where(is.numeric), ~ round(.,2))) %>% 
    dplyr::rename("NHS board"= HB,
                  "Total"= N_TOTAL,
                  "Percentage (%)" = STD_PC)
  
  ### Weeks from diagnosis to first contact Table
  output$Weeks_from_Diagnosis_table <- DT::renderDataTable({ # Use reactive expression to generate data table
    DT::datatable(Weeks_from_Diagnosis_table, 
                  options = list(pageLength = 27, scrollX = TRUE), 
                  rownames = FALSE)
  })
  
  ### Download Table-----
  # Button
  output$download_weeksfromDiag_table <- downloadHandler(
    filename = function() {
      "Diagnosis to first contact.csv"
    },
    content = function(file) {
      write.csv(weeksfromDiag_table, file, row.names = FALSE)
    }
  )  
  # Months from primary referral to confirmed diagnosis ---- 
  
  # Filter DF based on drop down menu selection
  Primref2diag_select <- reactive({ 
    primref2diag %>%
      dplyr::rename(Percentage = Primref_PC, Health_Board = HB) %>% 
      filter(Time != "Missing") %>% # Remove rows where Time is "Missing"
      subset(year == input$primref_Diag_Year_Dropdown) %>% # Filter dataset to the selected year
      group_by(Health_Board) %>% # Group by Health Board
      mutate(Percentage = Percentage / sum(Percentage, na.rm = TRUE) * 100) %>% # Normalize percentages to sum to 100
      mutate(across(where(is.numeric), ~ round(., 2))) %>%
      mutate(Time = forcats::fct_relevel(Time, 
                                         "<=6 months", ">6 and <=12 months", ">12 and <=18 months", 
                                         ">18 and <=24 months", ">=24 months"))  %>%  # Order Time categories
      ungroup() %>%
      tidyr::complete(Health_Board, Time, fill = list(Percentage = 0)) %>% # Ensure all Time categories are present
      group_by(Health_Board,year) %>% 
      fill(N_TOTAL,.direction = "updown") %>% 
      ungroup()
    
  })
  
  
  
  ## Create Chart----
  primref_Diag_Chart <- reactive({ 
    ggplot(Primref2diag_select() %>% 
             filter(!Time == "Missing"),
           aes(x = Percentage, y = Health_Board, fill =  forcats::fct_rev(Time),
               label1 = Percentage, label2 = Health_Board, label3 = Time)) +
      geom_bar(stat = "identity", position = "stack", size = 1) +
      labs(title = " ", x = "Percentage", y = " ") +
      scale_fill_manual(values = c("<6 months" = "#0078D4", 
                                   ">6 and <=12 months" =  "#83BB26",
                                   ">12 and <=18 months" = "#1E7F84",
                                   ">18 and <=24 months" = "#3F3685",
                                   ">=24 months" =  "#C73918"),
                        limits = c("<=6 months",
                                   ">6 and <=12 months",
                                   ">12 and <=18 months",
                                   ">18 and <=24 months",
                                   ">=24 months")) + 
      scale_y_discrete(limits = c("Scotland","NHS Western Isles","NHS Tayside",
                                  "NHS Shetland","NHS Orkney","NHS Lothian",
                                  "NHS Lanarkshire","NHS Highland","NHS Grampian",
                                  "NHS GG&C","NHS Forth Valley","NHS Fife",
                                  "NHS D&G","NHS Borders","NHS A&A")) +
      scale_x_continuous()     
    
  })
  
  # Generate Plotly Chart
  output$primref_Diag_Chart <- renderPlotly({
    # Convert ggplot to plotly
    p <- ggplotly(primref_Diag_Chart(), tooltip = c("label1", "label2", "label3"))
    
    # Extract and reorder traces to match desired legend order
    trace_order <- c("<=6 months", ">6 and <=12 months", ">12 and <=18 months", ">18 and <=24 months", ">=24 months")
    
    # Create a list to store reordered traces
    reordered_traces <- list()
    
    # Loop through each desired order and find the matching trace
    for (trace_name in trace_order) {
      trace <- p$x$data[[which(sapply(p$x$data, function(x) x$name) == trace_name)]]
      reordered_traces <- c(reordered_traces, list(trace))
    }
    
    # Replace the existing traces with the reordered traces
    p$x$data <- reordered_traces
    
    # Apply layout customizations
    p <- p %>%
      layout(
        title = "",
        xaxis = list(
          title = "Percentage",
          tickangle = 0,
          showgrid = TRUE,
          zeroline = FALSE
        ),
        yaxis = list(
          title = "",
          showgrid = TRUE,
          zeroline = FALSE
        ),
        legend = list(
          title = list(text = ""),
          orientation = "h",
          x = 0.5,
          xanchor = "center",
          y = -0.2
        ),
        plot_bgcolor = "#ffffff",
        paper_bgcolor = "#ffffff",
        margin = list(t = 40, b = 60, l = 50, r = 50)
      )
    
    p
    
  })
  
  ## Table Title ----
  output$primref_Diag_table_title <- renderText({ # Create reactive table title
    paste0("Percentage of people newly diagnosed with MS in Scotland, by number of months from primary referral to confirmed diagnosis")
  })
  
  ## Create Table ----
  # Create Months from diagnosis to first contact Table ----
  primref_Diag_table <- primref2diag %>%
    #filter(year == input$primref_Diag_Year_Dropdown) %>%
    #pivot_wider(names_from = Time, values_from = N_BREAK, values_fill = list(N_BREAK = 0)) %>% 
    dplyr::mutate(across(where(is.numeric), ~ round(.,2))) %>% 
    dplyr::select(HB,year,Time,N_BREAK,N_TOTAL,Primref_PC) %>% 
    dplyr::rename("NHS board"= HB,
                  "Number of new MS registrations"= N_TOTAL,
                  "Year" = year,
                  "Time from primary referral to confirmed diagnosis" = Time,
                  "Number of patients" = N_BREAK,
                  "Percentage (%)" = Primref_PC) 
  
  ## Generate Table ----
  # Months from diagnosis to first contact 
  output$primref_Diag_table <- DT::renderDataTable({ # Use reactive expression to generate data table
    DT::datatable(primref_Diag_table, 
                  options = list(pageLength = 27, scrollX = TRUE), 
                  rownames = FALSE)
  })
  
  ### Download Table-----
  # Button
  output$download_primref_Diag_table <- downloadHandler(
    filename = function() {
      "Primary referral to confirmed diagnosis .csv"
    },
    content = function(file) {
      write.csv(primref_Diag_table, file, row.names = FALSE)
    }
  )  
  
  # DMT----
  
  
  ## Chart Title ----
  output$Chart_DMT_title <- renderText({ # Create reactive table title
    
    if(input$Results_DMT_Dropdown == "DMTs discussed at the time of diagnosis") {
      paste0("Percentage of patients where potential use of DMTs discussed at the time of diagnosis by NHS board, for calendar year 2024")}
    
    else if(input$Results_DMT_Dropdown == "DMTs by type of MS" ) {
      paste0("DMTs by type of MS split by NHS board for calendar year 2024")}
  }) 
  
  ##  Create Chart -----
  #Create a reactive expression for the DMTs chart
  
  Results_DMT_chart <- reactive({
    if(input$Results_DMT_Dropdown == "DMTs discussed at the time of diagnosis"){
      ggplot(DMTs %>% 
               filter(!HB == "NA") %>% 
               mutate(across(where(is.numeric), ~ round(.,2))) %>% 
               dplyr::rename(Percentage = PERCENT, Health_Board = HB, Response = Ind),
             aes(x =Percentage, y = Health_Board, fill = Response,
                 label1 = Percentage, label2 = Health_Board, label3 = Response)) + 
        geom_col(position = "stack", size = 1) +
        labs(title = " ", x = "Percentage", y = " ") +
        scale_fill_manual(
          values = c("Yes" = "#3F3685", "No" = "#C73918"))+
        scale_y_discrete(limits = rev(unique(sort(DMTs$HB)))) +
        theme_bw() +
        theme(
          axis.text.x.bottom = element_text(angle = 0, hjust = 1, vjust = 1),
          panel.border = element_blank(),    # Remove the panel border
          plot.background = element_blank(), # Set plot background to white
          legend.title = element_blank(),    # Remove the legend title
          legend.key = element_blank()       # Optional: Remove legend key background
        )
      
    }
    # DMTs by type of MS chart
    else if(input$Results_DMT_Dropdown == "DMTs by type of MS"){
      ggplot(DMTs_MSType %>% 
               filter(!HB == "NA") %>% 
               mutate(MSDiagForm = case_when(MSDiagForm =="Relapsing Remitting" ~ "Relapsing-Remitting",
                                             MSDiagForm == "Not known" ~ "Type unknown",
                                             TRUE ~ MSDiagForm),
                      MSDiagForm = factor(MSDiagForm,levels = c("Primary Progressive","Secondary Progressive","Relapsing-Remitting","Type unknown"))) %>% 
               dplyr::rename(MS_Type= MSDiagForm,
                             Percentage = PERCENT,
                             Health_Board = HB,
                             DMTs_Discussed = Ind) %>% 
               group_by(Health_Board) %>% 
               #mutate(Percentage = Percentage / sum(Percentage) * 100) %>% 
               mutate(across(where(is.numeric), ~ round(.,2))),
             aes(x = Percentage, y = Health_Board, fill = DMTs_Discussed,
                 label1 =DMTs_Discussed, label2 = MS_Type, label3 = Health_Board, label4 = Percentage )) + 
        geom_bar(stat = "identity", size = 1) +
        facet_grid(.~MS Type)+
        labs(title = " ", x = "Percentage (%)", y = " ") +
        scale_fill_manual(values = c("Yes" = "#0078D4",
                                     "No" = "#3F3685")) +
        scale_y_discrete(limits = rev(unique(sort(DMTs_MSType$HB)))) +
        scale_x_continuous(labels = scales::percent_format(scale = 1))+
        theme_bw() +
        theme(
          axis.text.x.bottom = element_text(angle = 0, hjust = 1, vjust = 1),
          panel.border = element_blank(),    # Remove the panel border
          plot.background = element_blank(), # Set plot background to white
          legend.title = element_blank(),    # Remove the legend title
          legend.key = element_blank()       # Optional: Remove legend key background
        )
    }
  })
  
  ## Generate Chart 
  output$Results_DMT_chart <- renderPlotly({
    p <- ggplotly(Results_DMT_chart(), tooltip = c("label1", "label2", "label3", "label4")) %>%
      layout(
        legend = list(
          orientation = "h",        # Horizontal legend
          x = 0.5,
          xanchor = "center",
          y = -0.2,
          title = list(text = "DMTs Discussed"), 
          traceorder = "reversed"    # Reverse the legend order
        ),
        plot_bgcolor = 'white',     # Ensure white background
        paper_bgcolor = 'white',
        xaxis = list(
          #title = "Percentage",
          showgrid = FALSE,         # Remove x-axis grid lines
          zeroline = FALSE
        ),
        yaxis = list(
          title = "",
          showgrid = FALSE,         # Remove y-axis grid lines
          zeroline = FALSE
        )
      )
    p
  })
  
  ## Table Title ----
  output$Results_DMT_title <- renderText({ # Create reactive table title
    
    if(input$Results_DMT_Dropdown == "DMTs discussed at the time of diagnosis") {
      paste0("Percentage of patients where potential use of DMTs discussed at the time of diagnosis by NHS board, for calendar year 2024")}
    
    else if(input$Results_DMT_Dropdown == "DMTs by type of MS" ) {
      paste0("DMTs by type of MS split by NHS board for calendar year 2024")}
  })
  
  ## Create Table ----
  
  ## DMTs at diagnosis Table
  DMT_Diag_table <- DMTs %>%
    dplyr::select(c(1,2,3,6)) %>% 
    dplyr::filter(!HB == "NA") %>% 
    dplyr::select(-year) %>% 
    dplyr::rename("NHS board"= HB,
                  "DMTs Discussed" = Ind) %>% 
    pivot_wider(names_from = "NHS board", values_from = PERCENT) %>%
    mutate(across(where(is.numeric), ~ round(.,2)))
  
  ## DMTs by type of MS Table
  MS_type_table <- DMTs_MSType %>%
    dplyr::select(c(1,3,4,5,6,7)) %>% 
    filter(!HB == "NA") %>% 
    #pivot_wider(names_from = "NHS board", values_from = PERCENT | N_BREAK) %>%
    mutate(MSDiagForm = dplyr::case_when(MSDiagForm == "Relapsing Remitting" ~ "Relapsing-Remitting",
                                         MSDiagForm == "Not known" ~ "Type unknown",
                                         TRUE ~ MSDiagForm)) %>% 
    dplyr::rename("NHS board"= HB,
                  "DMTs Discussed" = Ind,
                  "Type of MS" = MSDiagForm,
                  "Number of patients by MS Type and DMT discussion" = N_BREAK,
                  "Total number of patients with subtype" = N_TOTAL,
                  "Percentage(%)" = PERCENT) %>% 
    mutate(across(where(is.numeric), ~ round(.,2)))
  
  
  ## Generate Table ----
  output$Results_DMT_table <- DT::renderDataTable({ # Use reactive expression to generate data table
    # DMTs at diagnosis Table
    if(input$Results_DMT_Dropdown == "DMTs discussed at the time of diagnosis"){
      DT::datatable(DMT_Diag_table, 
                    options = list(pageLength = 27, scrollX = TRUE), 
                    rownames = FALSE)
    }
    # DMTs by type of MS Table
    else if(input$Results_DMT_Dropdown == "DMTs by type of MS"){
      DT::datatable(MS_type_table, 
                    options = list(pageLength = 27, scrollX = TRUE), 
                    rownames = FALSE)
    }
  })
  
  ## Download Table-----
  # Button
  output$download_DMT_Table <- downloadHandler(
    filename = function() {
      if (input$Results_DMT_Dropdown == "DMTs discussed at the time of diagnosis") {
        "DMTs_discussed_at_diagnosis.csv"
      } else if (input$Results_DMT_Dropdown == "DMTs by type of MS") {
        "DMTs_by_type_of_MS.csv"
      } else {
        "DMT_data.csv"
      }
    },
    content = function(file) {
      if (input$Results_DMT_Dropdown == "DMTs discussed at the time of diagnosis") {
        write.csv(DMT_Diag_table, file, row.names = FALSE)
      } else if (input$Results_DMT_Dropdown == "DMTs by type of MS") {
        write.csv(MS_type_table, file, row.names = FALSE)
      }
    }
  )
  ##Generate dynamic text ------
  # DMTs at diagnosis text
  output$DMT_Text <- renderUI({ 
    if(input$Results_DMT_Dropdown == "DMTs discussed at the time of diagnosis"){
      #Steve's commentary
      tagList(p("This chart shows the percentage of patients, by NHS board, where DMTS were
    discussed at the time of diagnosis."),
              p("Currently, patients with active disease are potentially eligible for DMTs
    and there is availability of DMTs for the different phenotypes of MS from RRMS,
    SPMS to PPMS."),
              #SMSR Writer's text:
              p("Multiple sclerosis is a rapid evolving field. In the last decade,
                a dozen new DMTs have been approved for relapsing multiple sclerosis
                including new B cell depleting agents, SP1 receptor modulators and
                new formulations for other classical DMTs. Early diagnosis and early
                initiation of DMTs are associated with better outcomes regarding
                function and long-term disability. Although most of DMTs are approved
                for relapsing MS, and there are many unmet needs and a necessity for
                new treatments for progressive MS including non-relapsing progressive
                MS and PPMS, there are already a few therapies also approved for
                progressive MS."),
              p("It is clinically relevant to offer information to people with MS
              regarding therapeutic options from the time of diagnosis, as potential
              treatments are available for both relapsing and progressive forms of
              the disease. The percentage of patients in each NHS board where
              DMTs were discussed at the time of diagnosis are summarised in the
              chart above.",
                p("Patients with evidence of active disease defined clinically or
                radiologically are potentially eligible to start a DMT. Factors
                influencing the discussion may relate to who made the diagnosis
                and if they were a DMT prescribing physician or not. Highly
                active therapies may be discussed first in a DMT meeting, as
                not all neurologists will prescribe DMTs and they may refer to
                MS specialists or a DMT multidisciplinary meeting to help to
                manage this aspect of patient care.")))
    }
    # DMTs by type of MS Text
    else if(input$Results_DMT_Dropdown == "DMTs by type of MS"){
      #Steve's text
      tagList(p("This chart shows the percentage of patients, by NHS board, where
              DMTs were discussed at the time of diagnosis, by subtype of MS."),
              #SMSR Writer's text
              p("The subtype of MS at the time of diagnosis and associated discussion
              regarding DMTs is outlined in this chart. In most NHS boards a
              discussion took place for patients with both relapses and those
              with PPMS who have never had a clinical relapse. Generally speaking,
              the potential use of DMTs was discussed with specific patients
              having either relapsing or progressing MS. Unfortunately, the
              accuracy of data is not good as a significant proportion of data
              was gathered as “unknown” in some NHS boards. More accurate,
              consistent data is needed to offer formal analysis. Additionally,
              some NHS boards held discussions within each group for patients 
              with progressing and relapsing MS, while others differentiated 
              between patients experiencing relapses at the time of diagnosis
              and those who were not. It may be possible that some patients may 
              have required a follow up appointment to discuss the pros and cons
              of starting a DMT, and also some time needed for screening and
              re-baseline MRI to check MS activity (some DMTs require an MRI
              to be completed within 3 months of starting these medications),
              before making the decision about starting a DMT."),
              p("It is also noted a trend towards a higher percentage of DMT
              discussions taking place at the time of diagnosis for relapsing
              MS compared to progressive MS. This reflects the higher
              availability of DMTs for the RRMS, when compared to SPMS to
              PPMS."))
    }
  })
  
  
  #Conclusion----
  ##Acknowlegements----
  output$acknowledgements_table <- renderTable(acknowledgements,
                                               striped = TRUE,
                                               hover = FALSE,
                                               bordered = FALSE,
                                               spacing = c("s"),
                                               width = "auto",
                                               align = c("l"),
                                               rownames = FALSE,
                                               colnames = TRUE,
                                               digits = NULL,
  )
  
  ##Glossary----
  ###Glossary Table-----
  output$Glossary_table <- DT::renderDataTable({
    DT::datatable(
      glossary, 
      options = list(pageLength = 25, scrollX = TRUE),
      rownames = FALSE
    )
  })
  
}