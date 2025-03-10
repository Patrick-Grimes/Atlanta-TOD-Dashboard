library(shiny)
library(tidyverse)
library(tidycensus)
library(dplyr)
library(sf)
library(tmap)
library(stringr)
library(readr)
library(DT)
library(bslib)

tmap_mode("view")

##################################
# Data Preparation (Run Once)    #
##################################

tod_data <- read_csv("tod_database_download copy.csv")

beltline_stations_MARTA <- tod_data %>%
  filter(Buffer == "Existing Transit")

beltline_stations <- tod_data %>%
  filter(Agency == "Beltline w Alignment 1")

beltline_MARTA_sf <- st_as_sf(beltline_stations_MARTA, coords = c("Longitude", "Latitude"), crs = 4326)
beltline_stations_sf <- st_as_sf(beltline_stations, coords = c("Longitude", "Latitude"), crs = 4326)

all_stations_sf <- rbind(beltline_MARTA_sf, beltline_stations_sf)

variables <- c(
  income_white = "B19013A_001",
  income_black = "B19013B_001",
  income_asian = "B19013D_001",
  income_hispanic = "B19013I_001",
  transit_white = "B08105A_004",
  transit_black = "B08105B_004",
  transit_asian = "B08105D_004",
  transit_hispanic = "B08105I_004",
  total_transport_white = "B08105A_001",
  total_transport_black = "B08105B_001",
  total_transport_asian = "B08105D_001",
  total_transport_hispanic = "B08105I_001"
)

get_acs_data <- function(year) {
  suppressMessages(
    get_acs(
      geography = "tract",
      variables = variables,
      state = "GA",
      county = "Fulton",
      year = year,
      survey = "acs5",
      geometry = TRUE
    ) %>% mutate(year = year)
  )
}

years <- c(2010, 2015, 2019)
acs_data <- suppressMessages(
  bind_rows(lapply(years, get_acs_data))
)

acs_data <- st_transform(acs_data, st_crs(beltline_MARTA_sf))

beltline_MARTA_sf <- st_join(beltline_MARTA_sf, acs_data, join = st_intersects)
beltline_stations_sf <- st_join(beltline_stations_sf, acs_data, join = st_intersects)
all_stations_sf <- rbind(beltline_MARTA_sf, beltline_stations_sf)

fulton_geom <- get_acs(
  geography = "tract",
  variables = "B19013_001",
  state = "GA",
  county = "Fulton",
  year = 2019,
  survey = "acs5",
  geometry = TRUE,
  cache_table = TRUE
)

# Transit Data Processing
transit_data <- acs_data %>%
  filter(variable %in% c(
    "transit_white", "transit_black", "transit_asian", "transit_hispanic",
    "total_transport_white", "total_transport_black", "total_transport_asian", "total_transport_hispanic"
  )) %>%
  group_by(GEOID, year, variable) %>% 
  summarize(estimate = sum(estimate, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = variable, values_from = estimate) %>%
  mutate(
    transit_white_rate = (transit_white / total_transport_white) * 100,
    transit_black_rate = (transit_black / total_transport_black) * 100,
    transit_asian_rate = (transit_asian / total_transport_asian) * 100,
    transit_hispanic_rate = (transit_hispanic / total_transport_hispanic) * 100
  )

county_transit_data <- transit_data %>%
  st_drop_geometry() %>%
  group_by(year) %>%
  filter(
    transit_white_rate > 0 | transit_white_rate < 100, 
    transit_black_rate > 0 | transit_black_rate < 100, 
    transit_asian_rate > 0 | transit_asian_rate < 100, 
    transit_hispanic_rate > 0 | transit_hispanic_rate < 100) %>%
  summarize(
    white_avg_rate = mean(transit_white_rate, na.rm = TRUE),
    black_avg_rate = mean(transit_black_rate, na.rm = TRUE),
    asian_avg_rate = mean(transit_asian_rate, na.rm = TRUE),
    hispanic_avg_rate = mean(transit_hispanic_rate, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = ends_with("_avg_rate"),
    names_to = "race",
    values_to = "rate"
  ) %>%
  mutate(
    race = recode(race,
                  "white_avg_rate" = "White",
                  "black_avg_rate" = "Black",
                  "asian_avg_rate" = "Asian",
                  "hispanic_avg_rate" = "Hispanic")
  )

selected_years <- c(2010, 2015, 2019)
county_transit_filtered <- county_transit_data %>%
  filter(year %in% selected_years)

# Income Data
income_race_aggregated <- all_stations_sf %>%
  filter(variable %in% c("income_white", 
                         "income_black", 
                         "income_asian", 
                         "income_hispanic")) %>%
  select(GEOID, year, variable, estimate) %>%
  mutate(variable = recode(variable,
                           "income_white" = "White",
                           "income_black" = "Black",
                           "income_asian" = "Asian",
                           "income_hispanic" = "Hispanic"))

income_race_filtered <- income_race_aggregated %>%
  filter(year %in% selected_years) %>% 
  mutate(estimate = estimate / 1000)

# Housing Data
housing_variables <- c(
  median_rent = "B25064_001",
  homeownership_white = "B25003A_002",
  homeownership_black = "B25003B_002",
  homeownership_asian = "B25003D_002",
  homeownership_hispanic = "B25003I_002",
  median_property_value = "B25077_001",
  total_households = "B25003_001"
)

get_housing_acs_data <- function(year) {
  suppressMessages(
    get_acs(
      geography = "tract",
      variables = housing_variables,
      state = "GA",
      county = "Fulton",
      year = year,
      survey = "acs5",
      geometry = TRUE
    ) %>% mutate(year = year)
  )
}

housing_years <- 2010:2020
housing_acs_data <- suppressMessages(
  bind_rows(lapply(housing_years, get_housing_acs_data))
)

homeownership_data <- housing_acs_data %>%
  filter(variable %in% c("homeownership_white", 
                         "homeownership_black", 
                         "homeownership_asian", 
                         "homeownership_hispanic",
                         "total_households")) %>%
  group_by(GEOID, year, variable) %>% 
  summarize(estimate = sum(estimate, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = variable, values_from = estimate) %>%
  mutate(
    homeownership_white_rate = homeownership_white / total_households * 100,
    homeownership_black_rate = homeownership_black / total_households * 100,
    homeownership_asian_rate = homeownership_asian / total_households * 100,
    homeownership_hispanic_rate = homeownership_hispanic / total_households * 100
  )

county_homeownership_data <- homeownership_data %>%
  st_drop_geometry() %>%
  group_by(year) %>%
  filter(homeownership_white_rate > 0,
         homeownership_black_rate > 0,
         homeownership_asian_rate > 0,
         homeownership_hispanic_rate > 0) %>%
  summarize(
    year = first(year),
    white_avg_rate = mean(homeownership_white_rate, na.rm = TRUE),
    black_avg_rate = mean(homeownership_black_rate, na.rm = TRUE),
    asian_avg_rate = mean(homeownership_asian_rate, na.rm = TRUE),
    hispanic_avg_rate = mean(homeownership_hispanic_rate, na.rm = TRUE),
    .groups = "drop"
  ) %>% 
  pivot_longer(
    cols = ends_with("_avg_rate"),
    names_to = "race",
    values_to = "rate"
  ) %>%
  mutate(
    race = recode(race,
                  "white_avg_rate" = "White",
                  "black_avg_rate" = "Black",
                  "asian_avg_rate" = "Asian",
                  "hispanic_avg_rate" = "Hispanic")
  )

county_homeownership_filtered <- county_homeownership_data %>%
  filter(year %in% c(2010, 2015, 2019))

median_rent_overall <- housing_acs_data %>%
  filter(variable == "median_rent") %>%
  group_by(year) %>%
  summarize(mean_rent = mean(estimate, na.rm = TRUE))

median_property_value_over_time <- housing_acs_data %>%
  filter(variable == "median_property_value") %>%
  group_by(year) %>%
  summarize(mean_value = mean(estimate, na.rm = TRUE)) 

ui <- fluidPage(
  titlePanel("Exploring Transit-Oriented Developments in Atlanta, GA"),
  tabsetPanel(
    # First tab: Income & Public Transportation
    tabPanel("Income & Public Transportation",
             sidebarLayout(
               sidebarPanel(
                 h4("Income and Public Transportation"),
                 h5("Description"),
                 p("The Atlanta Beltline project, initiated in the early 2000s, is one of the most ambitious urban redevelopment 
                   programs in the United States. It leverages a historic 22-mile rail corridor that circles Atlanta’s core to 
                   create a network of public parks, multi-use trails, and transit options. Over the years, it has spurred 
                   significant growth, connecting neighborhoods, attracting new businesses, and providing equitable transit-oriented 
                   development opportunities. As it continues to expand, the Beltline not only aims to increase accessibility 
                   and mobility throughout the city but also strives to bridge gaps in income disparity, improve public 
                   transportation usage, and enhance the overall quality of life for diverse communities across Atlanta."),
                 h5("Filters for Income Boxplot"),
                 checkboxGroupInput(
                   inputId = "boxplot_races",
                   label = "Select Races to Display:",
                   choices = c("White", "Black", "Asian", "Hispanic"),
                   selected = c("White", "Black", "Asian", "Hispanic")
                 ),
                 h5("Filters for Public Transportation Usage"),
                 checkboxGroupInput(
                   inputId = "bar_races",
                   label = "Select Races to Display:",
                   choices = c("White", "Black", "Asian", "Hispanic"),
                   selected = c("White", "Black", "Asian", "Hispanic")
                 )
               ),
               mainPanel(
                 fluidRow(
                   column(8,
                          tmapOutput("map", height = "400px"),
                          h5("Legend:"),
                          p("Blue circles = MARTA lines/stations"),
                          p("Red circles = BeltLine stations")
                   ),
                   column(4,
                          wellPanel(
                            p("As identified below, the Blue circles correspond with MARTA lines and stations, which is Atlanta's
                              combined bus, rail, and streetcar transit. These stations were first developed in the 70s and 
                              finished construction in the late 90s. The Red circles create a circular path that goes around the city.
                              The red circles consist of a mix of rail development plus trails. This portion is being worked on in
                              various sections, with the project beginning in 2005, with hopes of ending by 2030.")
                          )
                   )
                 ),
                 
                 fluidRow(
                   column(6,
                          plotOutput("income_race_plot", height="250px"),
                          wellPanel(
                            p("This boxplot showcases the median income distribution across ethnic groups for the years 
                              2010, 2015, and 2019. Asian households consistently have higher median incomes compared 
                              to other ethnicities, while Black households have the lowest median incomes over the same period. 
                              Overall, each ethnic group sees increases in median income across the years, except potentially
                              Blacks from 2015 to 2019 which appears to have a constant if not slightly lower median income.")
                          )
                   ),
                   column(6,
                          plotOutput("public_transport_plot", height="250px"),
                          wellPanel(
                            p("This bar chart represents the percentage of people from various ethnic groups who used public 
                              transit as their means of travel to work in 2010, 2015, and 2019. Blacks and Hispanics 
                              generally demonstrate higher public transportation usage compared to Asian and White across 
                              the years. Asians and Whites showcase increasing usage of public transit across the years while
                              Blacks and Hispanics see a downward trend. In 2015, 9% of Hispanics used public transit as 
                              their means to work, a high figure compared to 2010 and 2019.")
                          )
                   )
                 )
               )
             )
    ),
    
    # Second tab: Housing
    tabPanel("Housing",
             sidebarLayout(
               sidebarPanel(
                 h4("Housing"),
                 h5("Description"),
                 p("This section focuses on housing dynamics influenced by the Atlanta Beltline and related Transit-Oriented Developments (TODs). 
                   The Beltline’s transformative role is not limited to transportation; it has also influenced residential landscapes. 
                   By improving accessibility and proximity to amenities, the Beltline can shape rental prices, homeownership patterns, 
                   and property values. The following visualizations help understand how these initiatives might be affecting 
                   housing affordability, ownership rates, and long-term property value trends in Fulton County."),
                 
                 h5("Filter for Median Rent Over Time"),
                 sliderInput(
                   inputId = "rent_years",
                   label = "Select Year Range:",
                   min = 2010, max = 2020,
                   value = c(2010, 2019),
                   step = 1
                 ),
                 
                 h5("Filters for Homeownership Rates"),
                 checkboxGroupInput(
                   inputId = "homeownership_races",
                   label = "Select Races to Display:",
                   choices = c("White", "Black", "Asian", "Hispanic"),
                   selected = c("White", "Black", "Asian", "Hispanic")
                 ),
                 
                 h5("Filter for Property Values by Year"),
                 sliderInput(
                   inputId = "property_years",
                   label = "Select Year Range:",
                   min = 2010, max = 2020,
                   value = c(2010, 2019),
                   step = 1
                 )
               ),
               mainPanel(
                 fluidRow(
                   plotOutput("median_rent_plot", height = "250px")
                 ),
                 fluidRow(
                   column(12,
                          wellPanel(
                            p("The line chart shows the trend in average median rent across census tracts in Fulton County
                              from 2010 to 2019. There is a steady increase in median rent over time, with significant 
                              growth observed after 2015. From 2019 to 2020, there is a steeper increase in median 
                              rents, increasing from around $1275 to $1350 in a single year.")
                          )
                   )
                 ),
                 
                 fluidRow(
                   column(6,
                          plotOutput("homeownership_plot", height="300px"),
                          wellPanel(
                            p("This bar chart displays homeownership rates for different ethnic groups in 2010, 2015, and 2019. 
                              White households have the highest homeownership rates consistently across the years, with rates
                              above 40%, though a slight downward trend is present. On the other hand, Hispanic, Asian, and Black 
                              households have substantially lower rates. Asians and Hispanics appear to demonstrate increases
                              in home ownership across the years while Blacks see a downward trend.")
                          )
                   ),
                   column(6,
                          plotOutput("property_values_plot", height="300px"),
                          wellPanel(
                            p("The boxplot shows the distribution of median property values across census tracts 
                              in Fulton County for the years 2010 to 2019. While median property values increase 
                              slightly over time, there is significant variability within each year. There is a sharp 
                              jump in the range of property values from 2016 to 2017, with the upper end nearing $800k by 2020.")
                          )
                   )
                 )
               )
             )
    ),
    
    # Third tab: Data
    tabPanel("Data",
             fluidRow(
               column(12,
                      wellPanel(
                        h4("Data"),
                        h5("Description"),
                        p("This tab presents an overview of the underlying datasets. 
                          The all_stations_sf dataset includes station-level spatial data integrated with demographic and economic attributes, 
                          while the housing_acs_data dataset encompasses tract-level housing and property metrics across multiple years. 
                          Use the dropdown to select how many entries are displayed, and scroll through the entire dataset.")
                      )
               )
             ),
             fluidRow(
               column(12,
                      h3("All Stations SF Data"),
                      DTOutput("all_stations_table")
               )
             ),
             fluidRow(
               column(12,
                      h3("Housing ACS Data"),
                      DTOutput("housing_acs_table")
               )
             )
    ),
    
    # This code snippet replaces the "Conclusions" tab with a "User Guide" tab.
    # We have removed sidebars, so that the text spans the entire width of the page.
    # We have included headers and paragraphs as requested.
    # Copy and paste this where your "Conclusions" tab currently exists in the UI.
    
    tabPanel("User Guide",
             fluidPage(
               h2("1. Research and Question"),
               p("Transit-Oriented Developments, or TODs, are a contemporary concept, intended to promote high-density, 
                 mixed-use development near public transportation hubs. The concept was solidified by Peter Calthorpe in the 1980s 
                 when he published The New American Metropolis. Calthorpe saw this idea as an easy-to-understand method to grow communities. 
                 The goal of our research is to determine—forty years later—the growth these zones have cultivated."),
               p("The objective of TODs is to promote increased access to public transit in a place where one does not need a 
                 car for their basic necessities and work commute. This stands in stark opposition to the car-centric country 
                 we currently live in. At face value, one could assume that, without a need for a dedicated personal vehicle, 
                 personal expenses would drop, and people could exercise more economic freedom. On the flip side, however, we 
                 could potentially examine how a rise in property values could inadvertently reduce the availability of affordable housing. 
                 This could potentially displace low-income residents and exacerbate socioeconomic disparities. Our research question was
                 how TODs affected the lives of their residents and how well they succeeded at their goals through proxies such as homeownership,
                 public transportation usage, home values, and income. We also wanted to see how these TODs affected different racial groups to 
                 identify any disparities."),
               p("We chose the Atlanta Beltline as our area of interest because it is one of the most ambitious and well-known examples 
                 of a transit-oriented development in the United States. Originally envisioned in 1999, the Beltline is transforming 22 
                 miles of former railway corridors into a loop of trails, parks, and transit. Combined with the existing MARTA lines, the 
                 overall MARTA-Beltline system makes it an ideal case study to examine the impact of TODs."),
               
               h2("2. Sources"),
               p("What we needed to embark on this project was both a general set of data regarding socioeconomic conditions, but we 
                 also needed to filter our search to transit-oriented developments. For this purpose we combined the American Community Survey 
                 (ACS) and the TOD Database. From there we were able to create a map of Atlanta, specifically the BeltLine, and examine 
                 how census tracts there have changed over time via markers such as median rent, homeownership, as well as other variables 
                 all faceted by race."),
               
               h2("3. App Usage"),
               p("On our app, there are several pages to choose from near the top of the screen, each focusing on different aspects of
                 our research. The first two pages, “Income & Public Transportation” and “Housing”, display maps and data visualizations 
                 along with explanatory descriptions. To enhance interactivity, you can use the filters provided on the left to adjust what 
                 data is shown. On the “Income & Public Transportation” page, you can select which racial groups to include in both the
                 income boxplot and the public transportation usage bar chart by checking or unchecking the corresponding boxes. On the 
                 “Housing” page, you can use sliders to select specific year ranges for the median rent line chart and property value 
                 boxplot, as well as checkboxes to choose which races appear in the homeownership bar chart. The third page, “Data”, 
                 presents the underlying datasets used to create the visualizations. You can select how many entries to display with the 
                 dropdown menu and search for specific variables with the search bar. Finally, the “User Guide” page provides an overview
                 of our key findings and insights based on the research. In this way, our dashboard allows you to dynamically explore, 
                 filter, and interpret the data behind our analyses."),
               
               h2("4. Research Results"),
               p("While transit-oriented developments aim to enhance accessibility and community growth, their impacts are multifaceted. 
                 Income data reveals persistent disparities across ethnic groups, with Asian households maintaining the highest median 
                 incomes, while Black households lag behind, experiencing stagnant or declining income trends in recent years. Public 
                 transit usage shows diverging trends, with Asians and Whites gradually increasing their reliance on transit, whereas 
                 Blacks and Hispanics, historically the most reliant, show a decrease—a potential indicator of changing accessibility 
                 or displacement patterns - or perhaps gentrification as the TOD develops."),
               p("The upward trends in median rent and property values underscore the economic growth associated with TODs but also 
                 raise concerns about affordability. Rapid rent increases post-2015 and significant property value variability suggest 
                 growing economic pressures that may disproportionately affect low-income and minority residents. This is reflected 
                 in the homeownership data, where white people consistently have the highest ownership rates, while black homeownership 
                 has declined, potentially signaling exclusionary housing trends."),
               p("Overall, the data suggests that while TODs have succeeded in driving economic development and fostering increased 
                 public transit use among some groups, these benefits are not evenly distributed. Rising property values and rents
                 may exacerbate socioeconomic disparities, challenging TODs’ original goal of equitable, car-independent living. 
                 Future strategies must address these disparities to ensure TODs fulfill their promise of fostering inclusive, 
                 sustainable communities."),
               
               h2("5. Team"),
               p("Colin Decker: Design"),
               p("Sam Schorsch: QA Engineer"),
               p("Taehee Kim: Prompt Engineer"),
               p("Patrick Grimes: Project Manager")
             )
    )
  )
)

server <- function(input, output, session) {
  
  # Map remains unchanged
  output$map <- renderTmap({
    tm_shape(fulton_geom) +
      tm_polygons(
        id = "GEOID",
        popup.vars = c("GEOID" = "GEOID")
      ) +
      tm_shape(beltline_MARTA_sf) +
      tm_bubbles(
        size = 0.1, 
        col = "blue", 
        alpha = 0.6, 
        border.col = "black", 
        border.alpha = 0.4
      ) +
      tm_shape(beltline_stations_sf) +
      tm_bubbles(
        size = 0.1, 
        col = "red", 
        alpha = 0.6, 
        border.col = "black", 
        border.alpha = 0.4
      ) +
      tm_layout(title = "MARTA and BeltLine Stations")
  })
  
  # Reactive data for Income Boxplot
  filtered_income_data <- reactive({
    req(input$boxplot_races)
    income_race_filtered %>%
      filter(variable %in% input$boxplot_races)
  })
  
  # Income by Race plot (Boxplot)
  output$income_race_plot <- renderPlot({
    ggplot(filtered_income_data(), 
           aes(x = as.factor(year), y = estimate, fill = variable)) +
      geom_boxplot(outlier.shape = NA, alpha = 0.7) +
      labs(
        title = "Income by Race (2010, 2015, 2019)",
        x = "Year",
        y = "Median Income (in $1000s)",
        fill = "Race"
      ) +
      theme_minimal() +
      theme(
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5)
      )
  })
  
  # Reactive data for Public Transportation Usage
  filtered_transit_data <- reactive({
    req(input$bar_races)
    county_transit_filtered %>%
      filter(race %in% input$bar_races)
  })
  
  # Public Transportation Usage by Race plot
  output$public_transport_plot <- renderPlot({
    ggplot(filtered_transit_data(), aes(x = race, y = rate, fill = as.factor(year))) +
      geom_bar(stat = "identity", position = "dodge", alpha = 0.7) +
      labs(
        title = "Public Transportation Usage by Race (2010, 2015, 2019)",
        x = "Race",
        y = "Public Transportation Usage (%)",
        fill = "Year"
      ) +
      theme_minimal() +
      scale_fill_brewer(palette = "Set2")
  })
  
  # Reactive data for Median Rent Over Time (Housing tab)
  filtered_rent_data <- reactive({
    req(input$rent_years)
    median_rent_overall %>%
      filter(year >= input$rent_years[1], year <= input$rent_years[2])
  })
  
  # Median Rent Over Time plot
  output$median_rent_plot <- renderPlot({
    ggplot(filtered_rent_data(), aes(x = year, y = mean_rent)) +
      geom_line(size = 1, color = "lightblue") +
      geom_point(size = 2, color = "black") +
      labs(
        title = "Average Median Rent Over Time",
        x = "Year",
        y = "Average Median Rent ($)"
      ) +
      theme_minimal()
  })
  
  # Reactive data for Homeownership by Race
  filtered_homeownership_data <- reactive({
    req(input$homeownership_races)
    county_homeownership_filtered %>%
      filter(race %in% input$homeownership_races)
  })
  
  # Homeownership plot
  output$homeownership_plot <- renderPlot({
    ggplot(filtered_homeownership_data(), aes(x = race, y = rate, fill = as.factor(year))) +
      geom_bar(stat = "identity", position = "dodge", alpha = 0.7) +
      labs(
        title = "Homeownership Rates by Race (2010, 2015, 2019)",
        x = "Race",
        y = "Homeownership Rate (%)",
        fill = "Year"
      ) +
      theme_minimal() +
      scale_fill_brewer(palette = "Set2")
  })
  
  # Reactive data for Property Values Boxplot
  filtered_property_data <- reactive({
    req(input$property_years)
    property_value_data <- housing_acs_data %>%
      filter(variable == "median_property_value")
    property_value_data %>%
      filter(year >= input$property_years[1], year <= input$property_years[2])
  })
  
  # Property Values plot
  output$property_values_plot <- renderPlot({
    ggplot(filtered_property_data(), 
           aes(x = as.factor(year), y = estimate)) +
      geom_boxplot(outlier.shape = NA, alpha = 0.7, fill = "lightblue") +
      labs(
        title = "Distribution of Home Values by Year",
        x = "Year",
        y = "Median Property Value"
      ) +
      theme_minimal()
  })
  
  # Data tables
  output$all_stations_table <- renderDT({
    datatable(
      st_set_geometry(all_stations_sf, NULL),
      options = list(
        scrollX = TRUE,
        scrollY = "300px",
        lengthMenu = c(10, 25, 50, 100),
        pageLength = 10
      ), 
      rownames = FALSE
    )
  })
  
  output$housing_acs_table <- renderDT({
    datatable(
      st_set_geometry(housing_acs_data, NULL),
      options = list(
        scrollX = TRUE,
        scrollY = "300px",
        lengthMenu = c(10, 25, 50, 100),
        pageLength = 10
      ), 
      rownames = FALSE
    )
  })
}

shinyApp(ui, server)
