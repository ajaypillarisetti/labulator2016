dashboardPage(
  dashboardHeader(title = "Labulator"),
  dashboardSidebar(disable=TRUE),
	dashboardBody(
        tags$head(
            tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
            tags$script(type='text/javascript', src='dygraph-extra.js'),
            tags$script(type='text/javascript', src='scripties.js')
            ),

        fluidRow(
            box(width=8, collapsible = TRUE, status="info", solidHeader=TRUE, title="Introduction",
                h5('Welcome to Labulator!'),
                p("Labulator is a simple tool to visualize and quickly check the status of the RFS balance room. Begin by selecting the date range of interest.")
            ),
            column(width=4,
                box(width=NULL, collapsible = FALSE, status='info', solidHeader=T, title='Filter Data',
                    dateRangeInput("dateSelect", label='Select a Date', start = Sys.Date()-7, end=Sys.Date())
                )
            )
        ),
        fluidRow(
            box(
                width = 8, height='510px',
                status = "info", solidHeader = TRUE,
                title = textOutput("selectedMID"),
                dygraphOutput('plainPlot', height='225px'),
                dygraphOutput('plainPlot2', height='225px')
            ),
            box(textOutput("legendARY"), title = "Legend", collapsible = TRUE, status='info', solidHeader=TRUE, width=4),
            infoBoxOutput('maxTemp'),
            infoBoxOutput('minTemp'),
            infoBoxOutput('avgDailyRange')                           
    	)	
	)
)