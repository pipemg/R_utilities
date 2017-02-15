list.of.packages <- c("shiny", "DT" ,"ggplot2") #List the libraries
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])] #Get the new packages
if(length(new.packages)) install.packages(new.packages, dependencies=TRUE, clean=TRUE, verbose=FALSE,  repos="https://cloud.r-project.org") #Install the packages

library(shiny) # load shiny
library(DT)# DT
library(ggplot2) # load ggplot


runApp(list(
  ui = fluidPage(
  titlePanel("Reaction Pattern Index Calculator - Single"),
	  sidebarLayout(
	    sidebarPanel(

#SideBar Input
	#File
	      strong(h3("Upload the file you whant to use")),      
	      fileInput('file1', 'Table of Importance Factors Reaction Pattern',accept=c('text/tsv','text/csv', 'text/comma-separated-values,text/plain', '.csv', '.tsv')),
	      checkboxInput('header', 'Header', TRUE),


	      column(6,radioButtons('sep', 'Separator', c(Tab='\t', Comma=',', Semicolon=';'), '\t')),
	      column(6,radioButtons('quote', 'Quote', c(None='', 'Double Quote'='"', 'Single Quote'="'"), '')),


	#Select the organs

	      checkboxInput('gills', 'Gills', FALSE),
	      checkboxInput('kidney', 'Kidney', FALSE),
	      checkboxInput('liver', 'Liver', FALSE),
	      checkboxInput('skin', 'Skin', FALSE),

	      column(12,  tags$hr()),
		
	      strong(h4("Select the Reaction Patterns to use")),     

	      fixedRow(
		column(6,br(),checkboxInput('rp1', 'Reaction Pattern 1', FALSE)),
		column(3,numericInput(inputId="min_rp1", label="MIN ROW", value=0, min=0))),


	      fixedRow(
		column(6,br(),checkboxInput('rp2', 'Reaction Pattern 2', FALSE)),
		column(3,numericInput(inputId="min_rp2", label="MIN ROW", value=0, min=0))),


	      fixedRow(
		column(6,br(),checkboxInput('rp3', 'Reaction Pattern 3', FALSE)),
		column(3,numericInput(inputId="min_rp3", label="MIN ROW", value=0, min=0))),


	      fixedRow(
		column(6,br(),checkboxInput('rp4', 'Reaction Pattern 4', FALSE)),
		column(3,numericInput(inputId="min_rp4", label="MIN ROW", value=0, min=0))),

	      fixedRow(
		column(6,br(),checkboxInput('rp5', 'Reaction Pattern 5', FALSE)),
		column(3,numericInput(inputId="min_rp5", label="MIN ROW", value=0, min=0))),


	     tags$hr(),	      

	      p(textAreaInput('desc', "Remarks:", value = "", cols = 300, rows = 3, placeholder = "Name", resize = "both")
	    )),



#Output Panel
	    mainPanel(
	      textOutput('intro'),
	      textOutput('names'),
	
     
	      strong(h3("Gills")),      
	      tabsetPanel(
			tabPanel('Reaction Pattern 1', DT::dataTableOutput("Trp1_g"), plotOutput("boxplot1_g")),
      			tabPanel('Reaction Pattern 2', DT::dataTableOutput("Trp2_g"), plotOutput("boxplot2_g")),
      			tabPanel('Reaction Pattern 3', DT::dataTableOutput("Trp3_g"), plotOutput("boxplot3_g")),
      			tabPanel('Reaction Pattern 4', DT::dataTableOutput("Trp4_g"), plotOutput("boxplot4_g")),
      			tabPanel('Reaction Pattern 5', DT::dataTableOutput("Trp5_g"), plotOutput("boxplot5_g")),
      			tabPanel('Results')
    	      ),

	      strong(h3("Kidney")),      
	      tabsetPanel(
			tabPanel('Reaction Pattern 1', DT::dataTableOutput("Trp1_k"), plotOutput("boxplot1_k")),
      			tabPanel('Reaction Pattern 2', DT::dataTableOutput("Trp2_k"), plotOutput("boxplot2_k")),
      			tabPanel('Reaction Pattern 3', DT::dataTableOutput("Trp3_k"), plotOutput("boxplot3_k")),
      			tabPanel('Reaction Pattern 4', DT::dataTableOutput("Trp4_k"), plotOutput("boxplot4_k")),
      			tabPanel('Reaction Pattern 5', DT::dataTableOutput("Trp5_k"), plotOutput("boxplot5_k")),
      			tabPanel('Results')
    	      ),

	      strong(h3("Liver")),      
	      tabsetPanel(
			tabPanel('Reaction Pattern 1', DT::dataTableOutput("Trp1_l"), plotOutput("boxplot1_l")),
      			tabPanel('Reaction Pattern 2', DT::dataTableOutput("Trp2_l"), plotOutput("boxplot2_l")),
      			tabPanel('Reaction Pattern 3', DT::dataTableOutput("Trp3_l"), plotOutput("boxplot3_l")),
      			tabPanel('Reaction Pattern 4', DT::dataTableOutput("Trp4_l"), plotOutput("boxplot4_l")),
      			tabPanel('Reaction Pattern 5', DT::dataTableOutput("Trp5_l"), plotOutput("boxplot5_l")),
      			tabPanel('Results')
    	      ),

	      strong(h3("Skin")),      
	      tabsetPanel(
			tabPanel('Reaction Pattern 1', DT::dataTableOutput("Trp1_s"), plotOutput("boxplot1_s")),
      			tabPanel('Reaction Pattern 2', DT::dataTableOutput("Trp2_s"), plotOutput("boxplot2_s")),
      			tabPanel('Reaction Pattern 3', DT::dataTableOutput("Trp3_s"), plotOutput("boxplot3_s")),
      			tabPanel('Reaction Pattern 4', DT::dataTableOutput("Trp4_s"), plotOutput("boxplot4_s")),
      			tabPanel('Reaction Pattern 5', DT::dataTableOutput("Trp5_s"), plotOutput("boxplot5_s")),
      			tabPanel('Results')
    	      ),
	      
	      textOutput('description')
	    )
	  )
	  ),








	  server = function(input, output) {


	    output$description <- renderText({input$desc})
	    output$intro <- renderText({"This is are the tables of reacction paterns"})
	    output$names <- renderText({
		inFile <- input$file1

		if (is.null(inFile))
		      return(NULL)
		    
		cn<-colnames(read.table(inFile$datapath, header=input$header, sep=input$sep, quote=input$quote,  row.names=1))
		paste0(cn)

	    })
	  }
))
