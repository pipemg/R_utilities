library(shiny)

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
		column(3,numericInput(inputId="min_rp1", label="MIN", value=0)),
		column(3,numericInput(inputId="max_rp1", label="MAX", value=0))),


	      fixedRow(
		column(6,br(),checkboxInput('rp2', 'Reaction Pattern 2', FALSE)),
		column(3,numericInput(inputId="min_rp2", label="MIN", value=0)),
		column(3,numericInput(inputId="max_rp2", label="MAX", value=0))),


	      fixedRow(
		column(6,br(),checkboxInput('rp3', 'Reaction Pattern 3', FALSE)),
		column(3,numericInput(inputId="min_rp3", label="MIN", value=0)),
		column(3,numericInput(inputId="max_rp3", label="MAX", value=0))),


	      fixedRow(
		column(6,br(),checkboxInput('rp4', 'Reaction Pattern 4', FALSE)),
		column(3,numericInput(inputId="min_rp4", label="MIN", value=0)),
		column(3,numericInput(inputId="max_rp4", label="MAX", value=0))),

	      fixedRow(
		column(6,br(),checkboxInput('rp5', 'Reaction Pattern 5', FALSE)),
		column(3,numericInput(inputId="min_rp5", label="MIN", value=0)),
		column(3,numericInput(inputId="max_rp5", label="MAX", value=0))),


	     tags$hr(),	      

	      p(textAreaInput('desc', "Description", value = "", cols = 300, rows = 3, placeholder = "Name", resize = "both")
	    )),
	    mainPanel(
	      textOutput('intro'),
	      textOutput('names'),
             
	      tabsetPanel(
			tabPanel('Reaction Pattern 1', dataTableOutput("Trp1"), tableOutput('I_org_rp1')),
      			tabPanel('Reaction Pattern 2', dataTableOutput("Trp2"), tableOutput('I_org_rp2')),
      			tabPanel('Reaction Pattern 3', dataTableOutput("Trp3"), tableOutput('I_org_rp3')),
      			tabPanel('Reaction Pattern 4', dataTableOutput("Trp4"), tableOutput('I_org_rp4')),
      			tabPanel('Reaction Pattern 5', dataTableOutput("Trp5"), tableOutput('I_org_rp5')),
      			tabPanel('Results')
    	      ),
	      textOutput('description')
	    )
	  )
	  ),




	  server = function(input, output) {

	    output$Trp1 = renderDataTable({

		if(input$rp1 == FALSE){

			rt=NULL

		}else{

			inFile <- input$file1

			if (is.null(inFile))
			      return(NULL)
			    
			rt<-read.table(inFile$datapath, header=input$header, 
					sep=input$sep, quote=input$quote, row.names=1)
			rt<-rt[1:5,]

		}
		rt

	    })


	    output$I_org_rp1 = renderTable({

		inFile <- input$file1

		if (is.null(inFile))
		      return(NULL)
		    
		rt<-read.table(inFile$datapath, header=input$header, 
			sep=input$sep, quote=input$quote, row.names=1)
		rt<-t(colSums(rt[1:5,]))

		if(input$rp1 == FALSE){
			rt=NULL
		}
		rt

	    })
	     

	    output$Trp2 = renderDataTable({

		inFile <- input$file1

		if (is.null(inFile))
		      return(NULL)
		    
		rt<-read.table(inFile$datapath, header=input$header, 
				sep=input$sep, quote=input$quote, row.names=1)
		rt<-rt[10:20,]

		if(input$rp2 == FALSE){
			rt=NULL
		}
		rt

	    })

	    output$I_org_rp2 = renderTable({

		inFile <- input$file1

		if (is.null(inFile))
		      return(NULL)
		    
		rt<-read.table(inFile$datapath, header=input$header, 
			sep=input$sep, quote=input$quote, row.names=1)
		rt<-t(colSums(rt[10:20,]))

		if(input$rp2 == FALSE){
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
		    
		cn<-colnames(read.table(inFile$datapath, header=input$header, sep=input$sep, quote=input$quote, row.names=1))
		paste0(cn)

	    })
	  }
))
