## descriptive text 


# # description of use ------------------------------------------------------
# fluidRow(class = "sectionTitle", 
#          h2("Understanding the EnviroScreen Tool")
# ),
# 
# tabsetPanel(
#   tabPanel(title = "Purpose and limitations"
#            ,br()
#            ,p("Colorado EnviroScreen is an environmental justice mapping tool that uses population and environmental factors to calculate an “EnviroScreen score.” A higher EnviroScreen score means the area is more likely to be affected by environmental health injustice. ")
#            ,p("The tool includes scores for each county, census tract, and census block group in Colorado. CDPHE will improve and update the tool in response to feedback and as new data becomes available. Please note that areas under Ute Mountain Ute and Southern Ute tribal jurisdiction are not currently represented on the map.")
#            ,p("Although EnviroScreen provides a robust measure of cumulative environmental burden, it is not a perfect tool. The tool uses limited environmental, health, and sociodemographic data to calculate the EnviroScreen score.")
#            ,p(
#              tags$strong("Colorado EnviroScreen does:")
#              ,tags$ul(
#                tags$li("Show which areas in Colorado are more likely to have higher environmental health injustices."),
#                tags$li("Identify areas in Colorado where government agencies can prioritize resources and work to reduce pollution and other sources of environmental injustice."),
#                tags$li("Provide information to empower communities to advocate to improve public health and the environment."),
#                tags$li("Identify areas that meet the definition of “Disproportionately Impacted Community” under the Colorado Environmental Justice Act (HB21-1266)."),
#              )
#            )
#            ,p(
#              tags$strong("Colorado EnviroScreen does not:"),
#              tags$ul(
#                tags$li("Define a healthy or unhealthy environment."),
#                tags$li("Establish causal associations between environmental risks and health."),
#                tags$li("Define all areas that may be affected by environmental injustice or specific environmental risks."),
#                tags$li("Provide information about an individual person’s health status or environment."),
#                tags$li("Take all environmental exposures into account."),
#                tags$li("Tell us about smaller areas within a census block group that may be more vulnerable to environmental exposures than other areas."),
#                tags$li("Provide information about non-human health or ecosystem risks."),
#              )
#            )
#   ),
#   tabPanel(title = "How to use the map",
#            br()
#            ,p("The default view of the map shows the state of Colorado. Individual counties, census tracts, or census block groups are color-coded based on their EnviroScreen score. The legend on the right side of the map shows what each color represents. The darker the color, the worse the EnviroScreen score. Users can zoom in and out, drag the map to a different location, and click on a location to learn more about its EnviroScreen score and how the score was calculated.")
#            ,tags$img(
#              id = "mapDesc", 
#              src="MapElements_3_crop.jpg",
#              title = "Map Elements",
#              height="auto"
#            )
#            ,tags$h3("To use the map, follow the steps below.")
#            ,tags$h4("Step 1: Select your map settings.")
#            ,p(
#              tags$strong("First, Select the “Geographic Scale” you want to visualize.")
#              ," he default layer displayed on the map is the overall EnviroScreen score, which combines all the indicators included in the tool in a single score. Click on the drop-down menu and scroll through the options to select another layer, such as a group component or individual indicators. Proceed to "
#              ,tags$em(" Select the “Indicator” for Map")
#            )
#            ,br()
#            ,p(
#              tags$strong("Next, Select the “Indicator” that you want to visualize.")
#              ," The default layer displayed on the map is the overall EnviroScreen Score, which is calculated from a variety of data sources. Click on the drop-down menu and scroll through the options to view the individual data sources that feed into the EnviroScreen score. Proceed to"
#              ,tags$em("“Measure or %”"))
#            ,br()
#            ,p(
#              tags$strong("Third, Select if you want to visualize the indicator “Measure or %.”")
#              ," Choose how to display the selected map layer. Measure displays the actual measurement of the data source (e.g., micrograms of pollutants, cases of disease, etc.). The measure is most meaningful for individual data layers like ozone or asthma hospitalizations. A percentile is like a ranking. The number represents the percentage of places in Colorado that are equal or have a lower ranking than the selected area. For example, a EnviroScreen percentile of 80 signifies that 80% of areas in Colorado are less likely to be affected by environmental health injustices than the area of interest and that 20% of areas in Colorado are more likely to be affected by environmental health injustices." )
#            ,br()
#            ,p(
#              "Once you have made your selections, click"
#              ,tags$strong(" Update Map.")
#              ," It may take a moment for the map to load.")
#            ,p(
#              tags$em("Note: If you do not click")
#              ,tags$strong(" Update Map")
#              ,tags$em(" on after selecting your map settings the selection will not be updated in the map, figure and table. The")
#              ,tags$strong(" Remove Highlighted Areas")
#              ,tags$em("button on the right side clears the highlighted areas. You can highlight areas using the data table at the bottom of the tool.")
#            ),
#            tags$h4("Step 2: Interact with the map."),
#            p(
#              "Individual counties, census tracts, or census block groups are color-coded based on the map layer you select. The legend on the right side of the map shows what these colors represent. The darker colors mean greater values, which represent a worse EnviroScreen score. Use the icons in the upper left of the map to zoom in and out, search for a specific address, center the map, customize the background map color, or add additional map layers."
#              ,br()
#              ,br()
#              ,tags$strong("Learn more about an area")
#              ,br()
#              ,"Click an area on the map to learn more. The pop-up window displays the value for the indicator you selected in the settings toolbar above the map. The graph and table below the map display more information about the area you clicked."
#              ,br()
#              ,br()
#              ,tags$strong("Search for an address")
#              ,br()
#              ,"Search for an address, city, ZIP code, or place name using the magnifying glass icon and “Search using OSM Geocoder” search bar."
#              ,br()
#              ,br()
#              ,tags$strong("Customize and add context (additional map layers)")
#              ,br()
#              ,"On the left side of the map, customize the display by selecting the base map or by adding additional map layers. "
#              ,br()
#              ,"The base map options provide different background maps (e.g., light, dark, or with streets and points of interest). The base map options do not influence the ranks or measures presented in the tool."
#              ,br()
#              ,"The additional map layers provide information about areas that produce oil and gas, have coal power plants, are rural communities, are areas designated by the federal government as Justice40 areas, or are areas that meet CDPHE’s definition of disproportionately impacted communities. The additional map layers options only provide additional context. The additional map layer options are not part of the EnviroScreen methods and do not influence the ranks or measures presented in the tool."
#            ),
#            tags$h4("Step 3: Explore the data in another way."),
#            p(
#              "Learn more about a selected area"
#              ,tags$b(" using the bar charts and data table.")
#              ,br()
#              ,br()
#              ,tags$strong("Bar Charts")
#              ,br()
#              ,"The bar charts show whether an area is more or less affected than other parts of the state for each score category. Read the “How to use the bar charts” tab to the right to learn more."
#              ,br()
#              ,"The data table presents the same data shown in the map and bar charts in a tabular, downloadable format. The table presents the data at the same geographical scale as the map. Read the “How to use the data table” tab to the right to learn more."
#            )
#   ),
#   tabPanel(title = "How to use the bar charts",
#            p(
#              "The bar chart on the right side of the map shows the overall EnviroScreen score. The bar charts below the map show the scores of each of the five components that make up the overall score. These charts show whether an area is more or less affected than other parts of the state for each category. "
#              ,br()
#              ,tags$strong("Bar height")
#              , " (y axis) represents the number of areas in Colorado that share the same range of burden as the selected area."
#              ,br()
#              ,br()
#              , "The" 
#              ,tags$strong(" horizontal location of the bar")
#              ," (x axis) represents the burden category (based on the percentile rank). If the bar is more towards the left, it represents a less burdened area compared to the rest of Colorado. If the bar is more towards the right, it represents a more burdened area compared to the rest of Colorado."
#              ,br()
#              ,br()
#              ,"When an area is selected in the map this is displayed in orange in the charts. Orange bars represent where the selected area is located in the distribution of the overall EnviroScreen score and individual components."
#              
#            )
#            #insert image 
#            ,tags$img(
#              id = "histoDesc", 
#              src="histoDesc.png",
#              title = "Bar Charts Elements",
#              height="auto"
#            )
#            ,br()
#            ,tags$strong("Data Table")
#            ,br()
#            ,"The area selected in the map also filters the data table below the charts. Explore the data by sorting the table. Select a row or rows to highlight the selection in the map. For example, a user could sort the table to find the areas with the highest climate vulnerability score, select rows in the table, and click “Highlight Selection on Map.” The areas selected in the table will be highlighted on the map."
#   ),
#   tabPanel(title = "How to use the data table",
#            br()
#            ,p(
#              "The data table presents the same data shown in the map and chart in a tabular format."
#              ,tags$strong(" Table columns")
#              ," describe the type of information displayed (e.g., county, component, indicator name)."
#              ,tags$strong(" Table rows")
#              ,"represent individual geographies. The table presents the data at the same geographical scale as the map. If the map is a county level, the data table will show county label data; if the map displays a census tract or census block group, the data table will show the corresponding geography."
#              ,br()
#              ,br()
#              ,"By default, the table shows ten rows. You can"
#              , tags$strong(" increase the number of rows")
#              , "visualized by clicking the 'show entries' box at the top left of the table (maximum 100 rows). To visualize the data from all geographies in the table, the number of available pages containing all the data is also presented at the bottom right of the table."
#              ,br()
#              ,br()
#              ,"A"
#              ,tags$strong(" 'search' box")
#              ,"is also available at the top right corner of the table. You can type the county name, census tract, or census block group number to easily identify the column with the corresponding data."
#              ,br()
#              ,br()
#              ,"The data table also includes"
#              ,tags$strong(" different tabs")
#              ,"  organized by score components."
#              ,br()
#              ,br()
#              ,"You can also explore the data by"
#              ,tags$strong(" sorting it by high or low values.")
#              ,br()
#              ,br()
#              ,"The area selected in the map also filters the data table below the charts. Select a row or rows to highlight the selection in the map. For example, you could sort the table to find the areas with the highest climate vulnerability score, select rows in the table, and click “Highlight Selection on Map.” The areas selected in the table will be highlighted on the map."
#              ,br()
#              ,br()
#              ,"Data presented in the table can also be downloaded by clicking the"
#              ,tags$strong(" 'Download Data from Current Geography'")
#              ," box."
#            )
#            #insert image 
#            ,tags$img(
#              id = "dataTable", 
#              src="dataTable.png",
#              title = "Data Table Elements",
#              height="auto"
#            )
#   ),
#   tabPanel(title = "Understanding the data",
#            br()
#            ,tags$strong("EnviroScreen score")
#            ,p(
#              "Colorado EnviroScreen maps the overlap of environmental exposures and effects, climate vulnerability, sensitive populations, and demographics to understand environmental injustice and environmental health risks in Colorado."
#              ,br()
#              ,br()
#              ,"The EnviroScreen score is calculated using an area’s relative ranking (percentile) of individual data indicators. The EnviroScreen score combines individual indicators into topic-based sub-components, like climate vulnerability or demographics. The EnviroScreen score then combines the sub-components further into the Pollution and Climate Burden and Health and Social Factors scores. These two scores are multiplied together to get the EnviroScreen score. The EnviroScreen score is the default data shown on the map. You can display individual indicators or sub-component scores."
#              ,br()
#              ,br()
#              ,"A higher EnviroScreen score means the area is more likely to be affected by environmental health injustices."
#            )
#            #insert image 
#            ,tags$img(
#              id = "scoreDesc", 
#              src="scoreDesc.png",
#              title = "Score Calculation",
#              height="auto")
#            ,tags$strong("Component scores")
#            ,p(
#              "You can view the components that make up the EnviroScreen score by selecting either of the Group Component Scores or any of the Individual Component Scores. Each of these scores is made up of multiple individual indicators, which you can also view on their own. Like the overall EnviroScreen score, component scores reflect relative rankings (percentiles). "
#            )
#            ,tags$strong("Individual indicators")
#            ,p(
#              "You can also view individual data indicators on the map. In the “Indicator” drop-down menu below, these individual indicators are organized under the component they feed into. For example, measures of air quality can be found under Environmental Exposures."
#              ,br()
#              ,br()
#              ,"Each individual data indicator represents something different and uses a different unit of measure. For example, some indicators (such as air quality,) are estimated concentrations while others (such as heart disease) are rates or percentages of certain health outcomes. For more information, please consult the technical document."
#            )
#            ,tags$strong("Data sources")
#            ,p(
#              "Indicators included in Colorado EnviroScreen come from a variety of data sources. Many of these sources are publicly available from state and federal agencies, like the U.S. Environmental Protection Agency (EPA), the Centers for Disease Control and Prevention (CDC), the Colorado Department of Public Health and Environment (CDPHE), and the Colorado Oil and Gas Conservation Commission (COGCC)."
#              ,br()
#              ,br()
#              ,"Although EnviroScreen contains many indicators, some of these indicators are from a few years ago or represent measurements at a larger geographic scale. There are also some environmental exposures, climate impacts, health outcomes, and demographic factors that we could not include in EnviroScreen because there are no reliable data sources available. "
#            )
#            ,tags$strong("Data limitations") 
#            ,p(
#              "The data included in EnviroScreen has several limitations due to the use of secondary data. Secondary data refers to data that is collected by someone other than the primary user. Moreover, the indicators used were often compiled from different years, making it difficult to compare the data sets. Furthermore, not all data was originally available at the same geographical scale. Some data were reported at the county level (2 indicators), others at the census tract level (9 indicators), and others at the census block group level or in a smaller geographical scale (20 indicators). Finally, although the tool aims to provide a comprehensive description of environmental injustices, indicators were not always available in a geographical information system format. Given this, Colorado EnviroScreen provides a limited representation of environmental injustices based on data availability. For more information, please consult the technical user guide."
#            )
#   ),
#   tabPanel(title = "Example use",
#            p(
#              "Here is one example of how a stakeholder could use Colorado EnviroScreen to meet their goals."
#              ,br()
#              ,br()
#              ,"A non-profit group is applying for a grant to install more air pollution monitoring in its community. The grant rules specify that applicants must show that their community needs the funds. The group knows science-based information will strengthen its application. The group uses Colorado EnviroScreen to get more information about their community."
#              ,br()
#              ,br()
#              ,"Here is how the community group could use Colorado EnviroScreen:"
#              ,tags$ol(
#                tags$li("The non-profit staff goes to the Colorado EnviroScreen webpage.")
#                ,tags$li("They use the “Search” feature in the map to find the census tract in which their community is located. ")
#                ,tags$li("They click the area on the map and visualize the results in the charts and table to view additional information. ")
#                ,tags$li(" They review how the overall EnviroScreen, component and indicator scores for their census tract compare with the rest of the state. ")
#                ,tags$li("They download the data for their community at the bottom of the page. ")
#                ,tags$li("They use this information to write a compelling grant application for community air monitors.")
#              )
#            )
#   ),
#   tabPanel(title =  "Definitions",
#            br()
#            ,tags$strong("EnviroScreen Score")
#            ,p(
#              "The EnviroScreen Score combines population characteristics and environmental burdens. The score goes from 0 to 100, with"
#              ,tags$strong(" the highest score representing the highest burden.")
#              ,br()
#              ,"The EnviroScreen score is a percentile, which is like a ranking. The number represents how many of the state’s counties, census tracts, or census block groups have a lower score than the area in question."
#              ,br()
#              ,tags$strong("Suppose a county has an EnviroScreen score of 70.")
#              ," This means its EnviroScreen score is higher than 70% of all counties in Colorado. In other words, 70% of counties in Colorado are less likely to be affected by environmental health injustices than the selected county."
#              ,br()
#              ,tags$strong("Suppose a census tract has an EnviroScreen score of 20.")
#              ," This means its EnviroScreen score is higher than 20% of all census tracts in Colorado. In other words, 20% of counties in Colorado are less likely to be affected by environmental health injustices than the selected census tract, or 80% of census tracts in Colorado are more likely to be affected by environmental health injustices than the selected census tracts."
#            )
#            ,tags$strong("Pollution and Climate Burden score")
#            ,p(
#              "The Pollution and Climate Burden score combines the scores from the following components: Environmental Exposures, Environmental Effects, and Climate Burden. The score ranges from 0 to 100, with the highest score representing the environmentally burdened populations."
#            )
#            ,tags$strong("Health and Social Factors score")
#            ,p(
#              "The Health and Social Factors score combines the Sensitive Populations and Demographics scores. The score ranges from 0 to 100, with the highest score representing the most susceptible and vulnerable populations."
#            )
#            ,tags$strong("Environmental Exposures")
#            ,p(
#              "The environmental exposures score represents a community’s exposure to certain environmental risks relative to the rest of the state. The score ranges from 0 to 100, with higher scores being worse. The environmental exposures score does not cover all pollutants; it is the average of data on diesel particulate matter, traffic proximity, ozone, PM 2.5, air toxics, other air pollutants, lead exposure risk, drinking water violations, and noise."
#            )
#            ,tags$strong("Environmental Effects")
#            ,p(
#              "The environmental effects score represents how many hazardous or toxic sites are in a community relative to the rest of the state. The score ranges from 0 to 100, with a higher score being worse. The score is the average of data on proximity to mining, oil and gas operations, impaired surface waters, wastewater discharge facilities, Superfund sites, facilities that use hazardous chemicals, and facilities that generate, treat, store, or dispose of hazardous wastes. As most people are not directly exposed to these sites, this score is weighted half as much as environmental exposures in the overall Pollution and Climate Burden score."
#            )
#            ,tags$strong("Climate Burden")
#            ,p(
#              "This climate burden score represents a community’s risk of drought, flood, extreme heat, and wildfire compared to the rest of the state. The score ranges from 0 to 100, the higher the score, the higher the burden."
#            )
#            ,tags$strong("Sensitive Populations")
#            ,p(
#              "The sensitive populations score captures how at risk a community is to environmental exposures and climate impacts as it relates to health. For example, air pollution has stronger impacts on older and younger people, and people with chronic conditions such as asthma. The score ranges from 0 to 100, with a higher score being worse. The score is calculated using data on asthma hospitalization rate, cancer prevalence, diabetes prevalence, heart disease prevalence, life expectancy, low birth weight rate, mental health, population over 65, and population under 5."
#            )
#            ,tags$strong("Demographics")
#            ,p(
#              
#              "The demographics score represents a community’s social and economic vulnerabilities. The score ranges from 0 to 100, with a higher number representing a higher vulnerability. It is calculated using data on people living with disabilities, housing cost burden, educational attainment, limited English proficiency, income, and race and ethnicity."
#            )
#            ,tags$strong("Disproportionately Impacted Community")
#            ,p(
#              "This term refers to areas that meet the definition of “Disproportionately Impacted Community” in the Colorado Environmental Justice Act (House Bill 21-1266). The definition includes census block groups where more than 40% of the population are low-income, housing cost-burdened, or people of color. “Low-income” means that median household income is at or below 200% of the federal poverty line. “Housing cost-burdened” means that a household spends more than 30% of its income on housing costs. “People of color” includes all people who do not identify as non-Hispanic white. This definition is not part of the EnviroScreen components or score, and does not influence the results presented in the map, charts or table."
#            )
#            ,tags$strong("Coal Community")
#            ,p(
#              "All census tracts and block groups within counties that have a coal-burning power plant are designated as coal communities. This data is not part of the EnviroScreen components or score, and does not influence the results presented in the map, charts or table. "
#            )
#            ,tags$strong("Oil and Gas Community")
#            ,p(
#              "All census tracts and block groups within counties that have active oil and gas operations are designated as oil and gas communities. Proximity to oil and gas is also included in EnviroScreen as a part of the environmental effect component."
#            )
#            ,tags$strong("Urban/Rural")
#            ,p(
#              "The U.S. Census Bureau's 'urban areas' are densely populated and include residential, commercial, and other properties. Counties that include these urban areas are considered urban. All counties not included within urban centers are considered rural counties. This data is not part of the EnviroScreen components or score, and does not influence the results presented in the map, charts or table. "
#            )
#            ,tags$strong("Justice40")
#            ,p(
#              "In early 2022, the White House launched the Justice40 Initiative. The goal of the Justice40 Initiative is to provide 40 percent of the overall benefits of Federal investments in seven key areas to disadvantaged communities. These seven key areas are: climate change, clean energy and energy efficiency, clean transit, affordable and sustainable housing, training and workforce development, the remediation and reduction of legacy pollution, and the development of critical clean water infrastructure. According to the definition of Justice40, a community qualifies as “disadvantaged,” if the census tract is above the threshold for one or more environmental or climate indicators and the tract is above the threshold for the socioeconomic indicators. This definition is not part of the EnviroScreen components or score, and does not influence the results presented in the map, charts or table. "
#            )
#            ,tags$strong("Story Maps")
#            ,p(
#              "A StoryMap is an immersive story that combines text, interactive maps, and other multimedia content. In Colorado EnviroScreen, the StoryMaps highlight life experiences that are complementary to the data included in the tool but importantly, they do not contribute to the EnviroScreen score."
#            )
#   ),
#   tabPanel("Tool development",
#            br()
#            ,tags$strong("Development through partnership")
#            ,p(
#              "Colorado EnviroScreen was created through a collaborative partnership between the Colorado Department of Public Health and Environment (CDPHE) and Colorado State University (CSU) as a result of a competitive bid process. "
#              ,br()
#              ,br()
#              ,"The team at CSU worked tirelessly to develop and build the EnviroScreen tool, with input from the community, stakeholders, and CDPHE. Moving forward, CDPHE will own and maintain the tool."
#              ,br()
#              ,br()
#              ,"Questions or comments about Colorado EnviroScreen should be sent to cdphe_ej@state.co.us."
#            )
#            ,br()
#            ,tags$strong("Community engagement")
#            ,p(
#              "Participation and feedback from the Colorado community was an essential factor in the development of Colorado EnviroScreen."
#              ,br()
#              ,br()
#              ,"From the start of developing Colorado EnviroScreen, the CSU and CDPHE team worked hard to engage the public. In the early stages of defining the tool, the team conducted individual interviews, hosted focus groups in English and Spanish, and facilitated a large bilingual public meeting. Through this outreach, the team learned how diverse Coloradans could use the tool, and gathered feedback about what to include in the tool."
#              ,br()
#              ,br()
#              ,"After the team created a basic version of Colorado EnviroScreen, they facilitated a closed-wave beta test in which more than 100 users tested the tool. In this beta, the team gathered feedback through a bilingual online questionnaire and interviews with individuals representing important groups of Colorado EnviroScreen users. After improving Colorado EnviroScreen based on feedback from the first round of beta testing, the team facilitated a public beta test. During the public beta test, many community members across Colorado shared their ideas through a bilingual online questionnaire and a public meeting. At every stage, the development team reviewed the feedback received and integrated it into the tool as much as possible."
#            )
#   ),
#   tabPanel("Additional Resources"
#            ,h4("Colorado EnviroScreen supporting materials")
#            ,p(
#              "Basic user guide (English |Spanish)"
#              ,br()
#              ,br()
#              ,"Technical user guide (currently only available in English)."
#              ,br()
#              ,br()
#              ,"Community engagement executive summary"
#            )
#            ,h4("CDPHE programs")
#            ,p(
#              "Environmental Justice Unit"
#              ,tags$a(
#                href = "https://cdphe.colorado.gov/environmental-justice"
#                ,tags$em("https://cdphe.colorado.gov/environmental-justice")
#                , target = "_blank"
#              )
#            )
#            ,p(
#              "Toxicology and Environmental Epidemiology Office"
#              ,tags$a(
#                href = "https://cdphe.colorado.gov/environment/toxicology-and-environmental-epidemiology "
#                ,tags$em("https://cdphe.colorado.gov/environment/toxicology-and-environmental-epidemiology ")
#                , target = "_blank"
#              )
#            )
#            ,p(
#              "Air Pollution Control Division"
#              ,tags$a(
#                href = "https://cdphe.colorado.gov/environment/air-pollution-control"
#                ,tags$em("https://cdphe.colorado.gov/environment/air-pollution-control")
#                , target = "_blank"
#              )
#            )
#            ,p(
#              "Water Quality Control Division"
#              ,tags$a(
#                href = "https://cdphe.colorado.gov/water-quality "
#                ,tags$em("https://cdphe.colorado.gov/water-quality")
#                , target = "_blank"
#              )
#            )
#            ,p(
#              "Hazardous Materials and Waste Management Division"
#              ,tags$a(
#                href = "https://cdphe.colorado.gov/hm"
#                ,tags$em("https://cdphe.colorado.gov/hm")
#                , target = "_blank"
#              )
#            )
#            ,p(
#              "Office of Health Equity"
#              ,tags$a(
#                href = "https://cdphe.colorado.gov/ohe"
#                ,tags$em("https://cdphe.colorado.gov/ohe")
#                , target = "_blank"
#              )
#            )
#            ,p(
#              "Prevention and Wellness"
#              ,tags$a(
#                href = "https://cdphe.colorado.gov/health/prevention-and-wellness"
#                ,tags$em("https://cdphe.colorado.gov/health/prevention-and-wellness")
#                , target = "_blank"
#              )
#            )
#            ,p(
#              "CDPHE Commerce City & North Denver Information"
#              ,tags$a(
#                href = "https://cdphe.colorado.gov/cc-nd"
#                ,tags$em("https://cdphe.colorado.gov/cc-nd")
#                , target = "_blank"
#              )
#            )
#            ,h4("Other state agencies")
#            ,p(
#              "Colorado Department of Natural Resources"
#              ,tags$a(
#                href = "https://dnr.colorado.gov/"
#                ,tags$em("https://dnr.colorado.gov/")
#                , target = "_blank"
#              )
#            )
#            ,p(
#              "Colorado Department of Transportation"
#              ,tags$a(
#                href = "https://www.codot.gov/"
#                ,tags$em("https://www.codot.gov/")
#                , target = "_blank"
#              )
#            )
#            ,p(
#              "Colorado Department of Human Services"
#              ,tags$a(
#                href = "https://cdhs.colorado.gov/"
#                ,tags$em("https://cdhs.colorado.gov/")
#                , target = "_blank"
#              )
#            )
#            ,p(
#              "Colorado Oil and Gas Conservation Commission"
#              ,tags$a(
#                href = "https://cogcc.state.co.us/#/home"
#                ,tags$em("https://cogcc.state.co.us/#/home")
#                , target = "_blank"
#              )
#            )
#            ,p(
#              "Colorado Department of Transportation"
#              ,tags$a(
#                href = "https://puc.colorado.gov/"
#                ,tags$em("https://puc.colorado.gov/")
#                , target = "_blank"
#              )
#            )
#            ,p(
#              "Colorado Hazard Mapping and Risk Map portal"
#              ,tags$a(
#                href = "https://www.codot.gov/"
#                ,tags$em("https://www.codot.gov/")
#                , target = "_blank"
#              )
#            )
#            ,p(
#              "CDPHE Commerce City & North Denver Information"
#              ,tags$a(
#                href = "https://coloradohazardmapping.com/"
#                ,tags$em("https://coloradohazardmapping.com/")
#                , target = "_blank"
#              )
#            )
#            ,h4("Environmental justice at federal programs and agencies")
#            ,p(
#              "U.S. Environmental Protection Agency"
#              ,tags$a(
#                href = "https://www.epa.gov/environmentaljustice"
#                ,tags$em("https://www.epa.gov/environmentaljustice")
#                , target = "_blank"
#              )
#            )
#            ,p(
#              "U.S. Department of Housing and Urban Development"
#              ,tags$a(
#                href = "https://www.hud.gov/climate/environmental_justice"
#                ,tags$em("https://www.hud.gov/climate/environmental_justice")
#                , target = "_blank"
#              )
#            )
#            ,p(
#              "Justice40 Initiative"
#              ,tags$a(
#                href = "https://www.whitehouse.gov/omb/briefing-room/2021/07/20/the-path-to-achieving-justice40/"
#                ,tags$em("https://www.whitehouse.gov/omb/briefing-room/2021/07/20/the-path-to-achieving-justice40/")
#                , target = "_blank"
#              )
#            )
#            ,p(
#              "Council on Environmental Quality Climate and Economic Justice Screening Tool - beta"
#              ,tags$a(
#                href = "https://screeningtool.geoplatform.gov/en/#3/33.47/-97.5"
#                ,tags$em("https://screeningtool.geoplatform.gov/en/#3/33.47/-97.5")
#                , target = "_blank"
#              )
#            )
#            ,p(
#              "Center for Disease Control & Prevention"
#              ,tags$a(
#                href = "https://www.cdc.gov/nceh/tracking/topics/EnvironmentalJustice.htm"
#                ,tags$em("https://www.cdc.gov/nceh/tracking/topics/EnvironmentalJustice.htm")
#                , target = "_blank"
#              )
#            )
#            ,h4("Real-time air monitoring")
#            ,p(
#              "Denver Love My Air Program"
#              ,tags$a(
#                href = "https://www.denvergov.org/Government/Agencies-Departments-Offices/Agencies-Departments-Offices-Directory/Public-Health-Environment/Environmental-Quality/Air-Quality/Love-My-Air"
#                ,tags$em("https://www.denvergov.org/Government/Agencies-Departments-Offices/Agencies-Departments-Offices-Directory/Public-Health-Environment/Environmental-Quality/Air-Quality/Love-My-Air")
#                , target = "_blank"
#              )
#            )
#            ,p(
#              "Tri-County Health Department Love My Air Program"
#              ,tags$a(
#                href = "https://www.tchd.org/868/Love-My-Air"
#                ,tags$em("https://www.tchd.org/868/Love-My-Air")
#                , target = "_blank"
#              )
#            )
#            ,p(
#              "Cultivando’s air monitoring program in Commerce City"
#              ,tags$a(
#                href = "https://www.bouldair.com/commerce_city.htm"
#                ,tags$em("https://www.bouldair.com/commerce_city.htm")
#                , target = "_blank"
#              )
#            )
#            ,p(
#              "AirNow.gov"
#              ,tags$a(
#                href = "https://www.airnow.gov/"
#                ,tags$em("https://www.airnow.gov/")
#                , target = "_blank"
#              )
#            )
#            ,p(
#              "Suncor Refinery Community Air Monitoring"
#              ,tags$a(
#                href = "https://www.ccnd-air.com/"
#                ,tags$em("https://www.ccnd-air.com/")
#                , target = "_blank"
#              )
#            )
#            ,p(
#              "Colorado Department of Transportation particulate matter (dust) monitoring for Central 70 project in North Denver"
#              ,tags$a(
#                href = "https://www.codot.gov/projects/i70east/resources/air-quality"
#                ,tags$em("https://www.codot.gov/projects/i70east/resources/air-quality")
#                , target = "_blank"
#              )
#            )
#   ),
#   tabPanel("FAQ",
#            br(),
#            p(
#              tags$a(
#                href = "https://docs.google.com/document/d/1_GEjGbOd3CmXwZu09QJ9oO4ZI8hqXtFwZAAeTsNV5lQ/edit?usp=sharing"
#                ,tags$em("Frequently Asked Questions")
#                , target = "_blank"
#              ) 
#            )
#   )
# ),