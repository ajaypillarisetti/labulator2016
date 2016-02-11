### Ajay Pillarisetti, University of California, Berkeley, 2015
### V1.0G

Sys.setenv(TZ="GMT")

shinyServer(function(input, output) {

	#read in data
	datasetInput <- reactive({
		dta <- all[as.Date(datetime, tz="America/Los_Angeles")>=input$dateSelect[1] & as.Date(datetime, tz="America/Los_Angeles")<=input$dateSelect[2]]
	})

	datasetName <- reactive({
	    inFile <- input$files
    	if (is.null(inFile)){return(NULL)} 
		inFile$name[1]
	})

	data_cleaned <- reactive({
		if (is.null(datasetInput())) return(NULL)
		data_d <- datasetInput()[,with=F]
	})

	####################
	##### datasets ##### 
	####################
	dataXTS.plainplot.temp <- reactive({
		dta <- data_cleaned()[,c('datetime','degC_air'), with=F]
		cols <- colnames(dta)[colnames(dta)!='datetime']
		dta.wide <- dta[, lapply(.SD, as.numeric), by=datetime]
		cols <- colnames(dta.wide)[colnames(dta.wide)!='datetime']
		as.xts(dta.wide)
	})

	dataXTS.plainplot.rh <- reactive({
		dta <- data_cleaned()[,c('datetime','RH_air'), with=F]
		cols <- colnames(dta)[colnames(dta)!='datetime']
		dta.wide <- dta[, lapply(.SD, as.numeric), by=datetime]
		cols <- colnames(dta.wide)[colnames(dta.wide)!='datetime']
		as.xts(dta.wide)
	})

	####################
	##### dygraphs ##### interactivity - to subset data to user-selected range
	####################
	#threshold plots
	from <- reactive({
		if (!is.null(input$thresholdPlot_date_window))
		ymd_hms(strftime(gsub(".000Z","",gsub("T"," ",input$thresholdPlot_date_window[[1]])), "%Y-%m-%d %H:%M:%S"), tz="America/Los_Angeles")
	})
  
	to <- reactive({
		if (!is.null(input$thresholdPlot_date_window))
		ymd_hms(strftime(gsub(".000Z","",gsub("T"," ",input$thresholdPlot_date_window[[2]])), "%Y-%m-%d %H:%M:%S"), tz="America/Los_Angeles")
	})  
	output$from <- renderText({from()})
  	output$to <- renderText({to()})
	output$graphTitle <- renderText({paste("Time Series Graph:", paste(from(), to(), sep=" to ")," ")}) 

	#UI OUTPUTS
	fileMin <- reactive({data_cleaned()[,min(temp)]})

	fileMax <- reactive({data_cleaned()[,max(temp)]})

	fileSamplingInterval <- reactive({as.numeric(data_cleaned()[10,'datetime',with=F]-data_cleaned()[9,'datetime',with=F])})

	output$selectMID <- renderUI({
		selectInput('mid', "Maternal ID", all[!is.na(mid) & as.Date(datetime)>=input$dateSelect[1] & as.Date(datetime)<=input$dateSelect[2],sort(unique(mid))])
	})

	output$selectedMID <- renderText({paste("Time-series plot", sep="")})

	output$selectFiles <- renderUI({
    	if(is.null(input$mid)) return()
	    files <- all[mid==input$mid & as.Date(datetime)>=input$dateSelect[1] & as.Date(datetime)<=input$dateSelect[2], paste(unique(location),"_", unique(description),"_", unique(device_id), " (", unique(paste(as.Date(min(datetime)), as.Date(max(datetime)), sep=" to ")), ")", sep="")]
	    checkboxGroupInput("files", "Choose stoves", choices  = files, selected = files)
  	})

	####################
	###### Tables ###### 
	####################
	output$allDataTable<-renderPrint({
		orig <- options(width = 1000)
		options(orig)
	})

	diagnostics <- reactive({
		datasetInput()[, list(
			Start_Date =as.Date(min(datetime)),
			End_Date = as.Date(max(datetime)),
			Max_Temp = max(temp),
			Min_Temp = min(temp)
		), by='device_id,location']
	})

	output$diagnosticsOutput <- renderDataTable(diagnostics(), options=list(searchable = FALSE, searching = FALSE, pageLength = 20,rowCallback = I('function(row, data) {
			if (data[5] >= 85) 
				$("td", row).css({"background" : "red", "color" : "white"});
			else if (data[6] < 0) 
				$("td", row).css({"background" : "blue", "color" : "white"});
			;}')))

	####################
	####### PLOTS ###### 
	####################
	output$plainPlot<- 
	renderDygraph({
		dygraph(dataXTS.plainplot.temp(), group = "lab") %>% 
	    dyOptions(axisLineWidth = 1.5, fillGraph = F, drawGrid = FALSE, useDataTimezone=T, strokeWidth=1, connectSeparatedPoints=T) %>%
	    dyAxis("y", label = "Temp C") %>%
	    dyAxis("x", label = "Date & Time") %>%
        dyLegend(labelsDiv = "legendARY")
	})

	output$plainPlot2<- 
	renderDygraph({
		dygraph(dataXTS.plainplot.rh(), group = "lab") %>% 
	    dyOptions(axisLineWidth = 1.5, fillGraph = F, drawGrid = FALSE, useDataTimezone=T, strokeWidth=1, connectSeparatedPoints=T) %>%
	    dyAxis("y", label = "Temp C") %>%
	    dyAxis("x", label = "Date & Time") %>%
        dyLegend(labelsDiv = "legendARY")
	})

	##########################
	####### DL HANDLERS ###### 
	##########################
	output$downloadCSV <- downloadHandler(
		filename = function() {paste(datasetName(), '.cleaned.csv', sep='') },
		content = function(file) {
			write.csv(melt(data_cleaned(), id.var=c('datetime','device_id')), file, row.names=F)
		}
	)

	output$downloadThresholdCSV <- downloadHandler(
		filename = function() {paste(datasetName(), '.threshold.output.csv', sep='') },
		content = function(file) {
			write.csv(thresholdData(), file, row.names=F)
		}
	)	
})