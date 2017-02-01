library(shiny)
library(DT)
runApp(list(
  ui = fluidPage(
  titlePanel("Reaction Pattern Index Calculator - Single"),
	  sidebarLayout(
	    sidebarPanel(
	      strong(h3("Upload the file you whant to use")),      
	      textAreaInput('org', "Organ:", value = "", cols = 300, rows = 1, placeholder = "Organ Ej. liver", resize = "both"),
	      fileInput('file1', 'Table of Importance Factors Reaction Pattern',accept=c('text/tsv','text/csv', 'text/comma-separated-values,text/plain', '.csv', '.tsv')),
	      checkboxInput('header', 'Header', TRUE),

	      column(6,radioButtons('sep', 'Separator', c(Tab='\t', Comma=',', Semicolon=';'), '\t')),
	      column(6,radioButtons('quote', 'Quote', c(None='', 'Double Quote'='"', 'Single Quote'="'"), '')),


	      column(12,  tags$hr()),
		
	      strong(h4("Select the Reaction Patterns to use")),     


	      fixedRow(
		column(6,br(),checkboxInput('rp1', 'Reaction Pattern 1', FALSE)),
		column(3,numericInput(inputId="min_rp1", label="MIN ROW", value=0, min=0)),
		column(3,numericInput(inputId="max_rp1", label="MAX ROW", value=0, min=0))),


	      fixedRow(
		column(6,br(),checkboxInput('rp2', 'Reaction Pattern 2', FALSE)),
		column(3,numericInput(inputId="min_rp2", label="MIN ROW", value=0, min=0)),
		column(3,numericInput(inputId="max_rp2", label="MAX ROW", value=0, min=0))),


	      fixedRow(
		column(6,br(),checkboxInput('rp3', 'Reaction Pattern 3', FALSE)),
		column(3,numericInput(inputId="min_rp3", label="MIN ROW", value=0, min=0)),
		column(3,numericInput(inputId="max_rp3", label="MAX ROW", value=0, min=0))),


	      fixedRow(
		column(6,br(),checkboxInput('rp4', 'Reaction Pattern 4', FALSE)),
		column(3,numericInput(inputId="min_rp4", label="MIN ROW", value=0, min=0)),
		column(3,numericInput(inputId="max_rp4", label="MAX ROW", value=0, min=0))),

	      fixedRow(
		column(6,br(),checkboxInput('rp5', 'Reaction Pattern 5', FALSE)),
		column(3,numericInput(inputId="min_rp5", label="MIN ROW", value=0, min=0)),
		column(3,numericInput(inputId="max_rp5", label="MAX ROW", value=0, min=0))),


	     tags$hr(),	      

	      p(textAreaInput('desc', "Description", value = "", cols = 300, rows = 3, placeholder = "Name", resize = "both")
	    )),
	    mainPanel(
	      textOutput('intro'),
	      textOutput('names'),
             
	      tabsetPanel(
			tabPanel('Reaction Pattern 1', DT::dataTableOutput("Trp1"), tableOutput('I_org_rp1')),
      			tabPanel('Reaction Pattern 2', DT::dataTableOutput("Trp2"), tableOutput('I_org_rp2')),
      			tabPanel('Reaction Pattern 3', DT::dataTableOutput("Trp3"), tableOutput('I_org_rp3')),
      			tabPanel('Reaction Pattern 4', DT::dataTableOutput("Trp4"), tableOutput('I_org_rp4')),
      			tabPanel('Reaction Pattern 5', DT::dataTableOutput("Trp5"), tableOutput('I_org_rp5')),
      			tabPanel('Results')
    	      ),
	      textOutput('description')
	    )
	  )
	  ),








	  server = function(input, output) {

	    output$Trp1 <- DT::renderDataTable({ 

		if(input$rp1 == FALSE){

			rt=NULL

		}else{

			inFile <- input$file1

			if (is.null(inFile))
			      return(NULL)
			    
			rt<-read.table(inFile$datapath, header=input$header, 
					sep=input$sep, quote=input$quote)
			rt<-rt[input$min_rp1:input$max_rp1,]
			rt$RowSums<-rowSums(rt)

		}
		rt
		 

	    }, options=list(  
		initComplete = JS(
    "function(settings, json) {",
    "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
    "}"		)  #close JS
	) #close options
 )


	    output$I_org_rp1 <- renderTable({

		inFile <- input$file1

		if (is.null(inFile))
		      return(NULL)
		    
		rt<-read.table(inFile$datapath, header=input$header, 
			sep=input$sep, quote=input$quote)
		rt<-t(colSums(rt[input$min_rp1:input$max_rp1,]))

		if(input$rp1 == FALSE){
			rt=NULL
		}
		rt
	    })

	    output$Trp2 <- DT::renderDataTable({

		inFile <- input$file1

		if (is.null(inFile))
		      return(NULL)
		    
		rt<-read.table(inFile$datapath, header=input$header, 
				sep=input$sep, quote=input$quote)
		rt<-rt[input$min_rp2:input$max_rp2,]

		rt$RowSums<-rowSums(rt)

		if(input$rp2 == FALSE){
			rt=NULL
		}
		rt

	    }, options=list(  
		initComplete = JS(
    "function(settings, json) {",
    "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
    "}"		)  #close JS
	) #close options
)

	    output$I_org_rp2 <- renderTable({

		inFile <- input$file1

		if (is.null(inFile))
		      return(NULL)
		    
		rt<-read.table(inFile$datapath, header=input$header, 
			sep=input$sep, quote=input$quote)
		rt<-t(colSums(rt[input$min_rp2:input$max_rp2,]))

		if(input$rp2 == FALSE){
			rt=NULL
		}
		rt

	    })
	     

	    output$Trp3 <- DT::renderDataTable({

		inFile <- input$file1

		if (is.null(inFile))
		      return(NULL)
		    
		rt<-read.table(inFile$datapath, header=input$header, 
				sep=input$sep, quote=input$quote)
		rt<-rt[input$min_rp3:input$max_rp3,]
		rt$RowSums<-rowSums(rt)
		if(input$rp3 == FALSE){
			rt=NULL
		}
		rt

	    }, options=list(  
		initComplete = JS(
    "function(settings, json) {",
    "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
    "}"		)  #close JS
	) #close options
)

	    output$I_org_rp3 <- renderTable({

		inFile <- input$file1

		if (is.null(inFile))
		      return(NULL)
		    
		rt<-read.table(inFile$datapath, header=input$header, 
			sep=input$sep, quote=input$quote)
		rt<-t(colSums(rt[input$min_rp3:input$max_rp3,]))

		if(input$rp3 == FALSE){
			rt=NULL
		}
		rt

	    })


 	    output$Trp4 <- DT::renderDataTable({

		inFile <- input$file1

		if (is.null(inFile))
		      return(NULL)
		    
		rt<-read.table(inFile$datapath, header=input$header, 
				sep=input$sep, quote=input$quote)
		rt<-rt[input$min_rp4:input$max_rp4,]
		rt$RowSums<-rowSums(rt)
		if(input$rp4 == FALSE){
			rt=NULL
		}
		rt

	    }, options=list(  
		initComplete = JS(
    "function(settings, json) {",
    "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
    "}"		)  #close JS
	) #close options
)

	    output$I_org_rp4 <- renderTable({

		inFile <- input$file1

		if (is.null(inFile))
		      return(NULL)
		    
		rt<-read.table(inFile$datapath, header=input$header, 
			sep=input$sep, quote=input$quote)
		rt<-t(colSums(rt[input$min_rp4:input$max_rp4,]))

		if(input$rp4 == FALSE){
			rt=NULL
		}
		rt

	    })



 	    output$Trp5 <- DT::renderDataTable({

		inFile <- input$file1

		if (is.null(inFile))
		      return(NULL)
		    
		rt<-read.table(inFile$datapath, header=input$header, 
				sep=input$sep, quote=input$quote)
		rt<-rt[input$min_rp5:input$max_rp5,]
		rt$RowSums<-rowSums(rt)

		if(input$rp5 == FALSE){
			rt=NULL
		}
		rt

	    }, options=list(  
		initComplete = JS(
    "function(settings, json) {",
    "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
    "}"		)  #close JS
	) #close options
)

	    output$I_org_rp5 <- renderTable({

		inFile <- input$file1

		if (is.null(inFile))
		      return(NULL)
		    
		rt<-read.table(inFile$datapath, header=input$header, 
			sep=input$sep, quote=input$quote)
		rt<-t(colSums(rt[input$min_rp5:input$max_rp5,]))

		if(input$rp5 == FALSE){
			rt=NULL
		}
		rt

	    })



	    output$description <- renderText({input$desc})
	    output$intro <- renderText({"This is are the tables of reacction paterns"})
	    output$names <- renderText({
		inFile <- input$file1

		if (is.null(inFile))
		      return(NULL)
		    
		cn<-colnames(read.table(inFile$datapath, header=input$header, sep=input$sep, quote=input$quote))
		paste0(cn)

	    })
	  }
))
