shinyUI(
    navbarPage(
        HTML(" Clustring Analysis"),
        inverse = TRUE,
        windowTitle = "Rising stars Shiny",
        header = fluidRow(
                column(1),
                column(9, HTML("
                <script>
                    (function(i,s,o,g,r,a,m){i['GoogleAnalyticsObject']=r;i[r]=i[r]||function(){
                    (i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();a=s.createElement(o),
                    m=s.getElementsByTagName(o)[0];a.async=1;a.src=g;m.parentNode.insertBefore(a,m)
                    })(window,document,'script','https://www.google-analytics.com/analytics.js','ga');

                    ga('create', 'UA-76756154-2', 'auto');
                    ga('send', 'pageview');

                </script>
                ")),
                column(2, HTML("<a href=\"https://chenjr-jacob.idv.tw/\"></a>"))
                ),
        ###################################################################################################
        #Dataset
        tabPanel("Dataset",
            fluidRow(
                column(1),
                column(10,
                    fluidRow(
                        column(4, selectInput("dataset_type", label = h4("Chooses Type of Datset"), c("Build-in Dataset", "Upload CSV")))
                    ),
                    fluidRow(
                        column(12, uiOutput("dataset_parameter_panel"))
                    ),
                    fluidRow(column(12, uiOutput("dataset_result_panel")))
                )
            )
        ),

       

        ###################################################################################################
        #Clustering
        tabPanel("Clustering",
            fluidRow(
                column(1),
                column(10,
                    fluidRow(
                        
                        column(4, selectInput("clustering_method", label = h4("Choose Clustering Method"), c("K-Means",  "DBSCAN", "Spectral","Heirachical"), selected = "K-Means"))
                    ),

                    uiOutput("clustering_parameters_panel"),
                    fluidRow(column(12, uiOutput("clustering_result_panel")))
                )
            )
        )
       
    )
)
