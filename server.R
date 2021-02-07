source("PkgLoader.R")
source("GetDatasetList.R")
source("GetColumnsType.R")
source("DataSummary.R")
source("Preprocess.R")
source("StatisticsTest.R")
source("Clustering.R")
source("Visualize.R")
#source("Heirachical.R")

shinyServer(
	function(input, output) {

		###################################################################################################
		#Dataset

		dataset_result <- reactiveValues(dataset = NULL, numeric_list = NULL, non_numeric_list = NULL, summary_numeric = NULL, summary_non_numeric = NULL, plot = NULL, plot_non_numeric = NULL, plot_height = NULL)

		output$dataset_parameter_panel <- renderUI({
			switch(input$dataset_type,
				"Build-in Dataset" = list(
					fluidRow(
						column(4, uiOutput("dataset_list"))
					),
					fluidRow(
						column(11),
						column(1, actionButton("select_dataset", label = "Select"))
					)
				),
				"Upload CSV" = list(
					fluidRow(
						column(3, fileInput("csv", label = h4("Choose CSV File"), accept=c("text/csv", "text/comma-separated-values,text/plain", ".csv"))),
						column(3, radioButtons("header", label = h4("Header"), c(True = TRUE, False = FALSE))),
						column(3, radioButtons("sep", label = h4("Separator"), c(Comma=",", Semicolon=";", Tab="\t"), ",")),
						column(3, radioButtons("quote", label = h4("Quote"), c(None="", "Double Quote"="\"", "Single Quote"="'"), "\""))
					),
					fluidRow(
						column(11),
						column(1, actionButton("select_dataset", label = "Select"))
					)
				)
			)
		})

		output$dataset_list <- renderUI({
			withProgress(min=1, max=20, expr={
				for(i in 1:20) {
					setProgress(message = "Page loading.", detail = "This may take a while...", value=i)
					print(i)
					Sys.sleep(0.05)
				}
			})
			dataset_list <- getDatasetList()
			selectInput("dataset", label = h4("Choose Dataset"), dataset_list, selected = "iris")
		})

		observeEvent(input$select_dataset, {
			withProgress(min=1, max=20, expr={
				for(i in 1:20) {
					setProgress(message = "Processing.", detail = "This may take a while...", value=i)
					print(i)
					Sys.sleep(0.1)
				}
			})
			dataset_result$dataset <- switch(input$dataset_type,
				"Build-in Dataset" = get(input$dataset),
				"Upload CSV" = read.table(input$csv$datapath, header = if(input$header == "TRUE") TRUE else FALSE, sep = input$sep , quote = input$quote)
			)
			if(!is.null(dataset_result$dataset)) {
				dataset_result$numeric_list <- unlist(getColumnsType(dataset_result$dataset)[1])
				dataset_result$non_numeric_list <- unlist(getColumnsType(dataset_result$dataset)[2])
				dataset_result$summary_numeric <- dataSummary_numreic(dataset_result$dataset)
				dataset_result$summary_non_numeric <- dataSummary_non_numeric(dataset_result$dataset)
				if(!is.null(dataset_result$summary_numeric)) {
					dataset_result$plot_non_numeric <- if(input$dataset_type == "Upload CSV") FALSE else TRUE
					dataset_result$plot <- plotPairs(dataset_result$dataset, non_numeric = dataset_result$plot_non_numeric)
					dataset_result$plot_height <- if(length(names(dataset_result$dataset)) <= 8) paste(length(names(dataset_result$dataset)) * 200, "px", sep = "") else "1600px"
				}
			}
		})

		output$dataset_result_panel <- renderUI({
			if(!is.null(dataset_result$summary_numeric)) {
				tabsetPanel(
					tabPanel("Summary",
						fluidRow(
							column(6,
								fluidRow(column(12, h4("Numeric summary"))),
								fluidRow(column(12, tableOutput("dataset_summary_numeric")))
							),
							column(6,
								fluidRow(column(12, h4("Correlation Matrix"))),
								fluidRow(column(12, tableOutput("correlation_matrix")))
							)
						),
						fluidRow(
							column(6,
								fluidRow(column(12, h4("Non-Numeric Summary"))),
								if(length(dataset_result$summary_non_numeric) == 1) {
									fluidRow(column(12, tableOutput("dataset_summary_non_numeric")))
								}else {
									fluidRow(verbatimTextOutput("dataset_summary_non_numerics"))
								}
							)
						)
					),
					tabPanel("Plot",
						uiOutput("dataset_plot_parameter_panel"),
						fluidRow(column(12, plotOutput("dataset_plot", height = dataset_result$plot_height)))
					),
					tabPanel("Table",
						fluidRow(column(12, dataTableOutput("dataset_datatable")))
					)
				)
			}else if(!is.null(dataset_result$dataset)){
				tabsetPanel(
					tabPanel("Table",
						fluidRow(column(12, h4("This dataset may not be used to clustering and classification."))),
						fluidRow(column(12, tableOutput("dataset_table")))
					)
				)
			}
		})


		output$dataset_summary_numeric <- renderTable({
			if(!is.null(dataset_result$summary_numeric)) print(as.data.frame(dataset_result$summary_numeric[1]))
		})

		output$correlation_matrix <- renderTable({
			if(!is.null(dataset_result$summary_numeric)) print(as.data.frame(dataset_result$summary_numeric[2]))
		})

		output$dataset_summary_non_numeric <- renderTable({
			if(!is.null(dataset_result$summary_non_numeric)) print(as.data.frame(dataset_result$summary_non_numeric))
		})

		output$dataset_summary_non_numerics <- renderPrint({
			for( table in dataset_result$summary_non_numeric) {
				print(as.data.frame(table))
			}
		})

		output$dataset_plot_parameter_panel <- renderUI({
			if(!is.null(dataset_result$summary_non_numeric) && dataset_result$plot_non_numeric) {
				list(
					fluidRow(
						column(4, selectInput("dataset_plot_class", label = h4("Choose Class Attribute"), c("None", unlist(getColumnsType(dataset_result$dataset)[2]))))
					),
					fluidRow(
						column(11),
						column(1, actionButton("dataset_plot", label = "Plot"))
					)
				)
			}
		})

		observeEvent(input$dataset_plot, {
			if(!is.null(dataset_result$summary_numeric)) {
				class <- if(input$dataset_plot_class == "None") NULL else input$dataset_plot_class
				dataset_result$plot <- plotPairs(dataset_result$dataset, non_numeric = dataset_result$plot_non_numeric)
			}
		})

		output$dataset_plot <- renderPlot({
			if(!is.null(dataset_result$plot)) {
				withProgress(min=1, max=20, expr={
					for(i in 1:20) {
						setProgress(message = "Creating Plot.", detail = "This may take a while...", value=i)
						print(i)
						Sys.sleep(0.1)
					}
				})
				print(dataset_result$plot)
			}
		})

		output$dataset_datatable <- renderDataTable({
			withProgress(min=1, max=20, expr={
				for(i in 1:20) {
					setProgress(message = "Creating Table.", detail = "This may take a while...", value=i)
					print(i)
					Sys.sleep(0.1)
				}
			})
			dataset_result$dataset[, drop =FALSE]
		}, options = list(lengthMenu = c(10, 25, 50), pageLength = 10)
		)

		output$dataset_table <- renderTable({
			dataset_result$dataset
		})

		
		

		###################################################################################################
		#Clustering

		clustering_result <- reactiveValues(result = NULL, result_data_frame = NULL, plot = NULL, plot_height = NULL, manova = NULL)

		output$clustering_parameters_panel <- renderUI({
			if(!is.null(dataset_result$summary_numeric)) {
				withProgress(min=1, max=20, expr={
					for(i in 1:20) {
						setProgress(message = "Page Loading.", detail = "This may take a while...", value=i)
						print(i)
						Sys.sleep(0.05)
					}
				})
				switch(input$clustering_method,
					"K-Means" = list(
						fluidRow(
							column(4, sliderInput("kmeans_k", label = h4("Set K"), min = 2, max = 10, value = 2))
						),
						fluidRow(
							column(11),
							column(1, actionButton("start_clustering", label = "Clustering"))
						)
					),
					
			
					"DBSCAN" = list(
						fluidRow(
							column(4, sliderInput("dbscan_eps", label = h4("Set Eps"), min = 0.1, max = 1, value = 0.5)),
							column(4, sliderInput("dbscan_pts", label = h4("Set MinPts"), min = 2, max = 20, value = 10))
						),
						fluidRow(
							column(11),
							column(1, actionButton("start_clustering", label = "Clustering"))
						)
					),
					"Heirachical" = list(
		
					  fluidRow(
					    column(11),
					    column(1, actionButton("start_clustering", label = "Clustering"))
					  )
					),
					"Spectral" = list(
						fluidRow(
							column(4, sliderInput("spectral_centers", label = h4("Set Centers"), min = 0, max = 10, value = 0)),
							column(4, sliderInput("spectral_nn", label = h4("Set NN"), min = 2, max = 20, value = 7))	
						),
						fluidRow(
							column(11),
							column(1, actionButton("start_clustering", label = "Clustering"))
						)
					)
				)
				
			}
		})

		observeEvent(input$start_clustering, {
			clustering_result$result <- switch(input$clustering_method,
				"K-Means" = clusteringKmeans(dataset_result$dataset, input$kmeans_k),
				
				"DBSCAN" = clusteringDBSCAN(dataset_result$dataset, input$dbscan_eps, input$dbscan_pts),
				"Heirachical" = clusteringHeirachical(dataset_result$dataset),
				"Spectral" = clusteringSpectral(dataset_result$dataset, input$spectral_centers, input$spectral_nn),
				
				
					)
			clustering_result$result_data_frame <- as.data.frame(clustering_result$result[1][1])
			clustering_result$manova <- statisticsMANOVA(clustering_result$result_data_frame, names(clustering_result$result_data_frame)[length(names(clustering_result$result_data_frame))], names(clustering_result$result_data_frame)[1:length(names(clustering_result$result_data_frame)) - 1])
			clustering_result$plot <- plotClusteringResult(clustering_result$result_data_frame)
			clustering_result$plot_height <- if(length(names(clustering_result$result_data_frame)) <= 8) paste(length(names(clustering_result$result_data_frame)) * 200, "px", sep = "") else "1600px"
			
				})

		output$clustering_result_panel <-renderUI({
			if(!is.null(clustering_result$result)) {
				tabsetPanel(
					tabPanel("Detail",
						fluidRow(column(12, verbatimTextOutput("clustering_result")))
					),
					tabPanel("Plot",
						fluidRow(column(12, plotOutput("clustering_result_plot", height = clustering_result$plot_height)))
					),
					
					tabPanel("Table",
						fluidRow(column(12, dataTableOutput("clustering_result_table")))
					)
				)
			}
		})

		output$clustering_result <- renderPrint({
			if(!is.null(clustering_result$result)) {
				print(clustering_result$result[2])
			}
		}, width = 180
		)

		output$clustering_result_plot <- renderPlot({
			if(!is.null(clustering_result$result)) {
				withProgress(min=1, max=20, expr={
					for(i in 1:20) {
						setProgress(message = "Creating Plot.", detail = "This may take a while...", value=i)
						print(i)
						Sys.sleep(0.1)
					}
				})
				print(clustering_result$plot)
			}
		})

		output$clustering_manova <- renderPrint({
			if(!is.null(clustering_result$result)) {
				print(clustering_result$manova)
				print(summary(clustering_result$manova))
			}
		})

		output$clustering_result_table <- renderDataTable({
			if(!is.null(clustering_result$result)) {
				withProgress(min=1, max=20, expr={
					for(i in 1:20) {
						setProgress(message = "Creating Table.", detail = "This may take a while...", value=i)
						print(i)
						Sys.sleep(0.1)
					}
				})
				clustering_result$result_data_frame[, drop =FALSE]
			}
		}, options = list(lengthMenu = c(10, 25, 50), pageLength = 10)
		)

	

	}
)

