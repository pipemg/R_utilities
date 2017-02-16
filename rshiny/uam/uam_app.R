list.of.packages <- c("shiny", "DT" ,"ggplot2") #List the libraries
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])] #Get the new packages
if(length(new.packages)) install.packages(new.packages, dependencies=TRUE, clean=TRUE, verbose=FALSE,  repos="https://cloud.r-project.org") #Install the packages

library(shiny) # load shiny
library(DT)# DT
library(ggplot2) # load ggplot


get_summary<-function(table,org,rp,w){
	
	W<-table[,w]
	factors<-as.matrix(table[,1:2])
	alt<-table[,3]
	matrix<-table[,-c(1:4)]
	rownames(matrix)<-alt
	
	#Calculamos el Indice I_org_rp (Sumatoria de los pesos de un rp para un organo determinado
	matrix[paste("I",org,rp,sep="_"),]<-t(colSums(apply(matrix,2,function(x) x*W)))
			

	#we calculate the summary by rows
	rowsummary<-t(apply(matrix,1,summary))
	#Agregamos la suma por renglones 
	Total<-rowSums(matrix)
	#Agregamos la varianza por renglones 
	Var<-round(apply(matrix,1,var), digits = 2) 
	#Agregamos la desviación estandar por renglones 
	Sd<-round(apply(matrix,1,sd), digits = 2) 
	#Agregamos las columnas
	matrix<-cbind(matrix,rowsummary)
	matrix$TOTAL<-Total
	matrix$VAR<-Var
	matrix$SD<-Sd
	#Agregamos la columna de pesos
	factors<-rbind(factors,c(factors[1,1],factors[1,2]))
	alt<-c(as.vector(alt),paste("I",org,rp,sep="_"))
	W<-c(W,0)
 	matrix<-cbind(factors,alt,W,matrix)

}


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

	     tags$hr(),	      

	      p(textAreaInput('desc', "Remarks:", value = "", cols = 300, rows = 3, placeholder = "Name", resize = "both")
	    )),



#Output Panel
	    mainPanel(
	      textOutput('intro'),
	
     
	      strong(h3(textOutput("organ1"))),      
	      tabsetPanel(
			tabPanel('Reaction Pattern 1', DT::dataTableOutput("Org1_rp1"), plotOutput("boxplot1_g")),
      			tabPanel('Reaction Pattern 2', DT::dataTableOutput("Org1_rp2"), plotOutput("boxplot2_g")),
      			tabPanel('Reaction Pattern 3', DT::dataTableOutput("Org1_rp3"), plotOutput("boxplot3_g")),
      			tabPanel('Reaction Pattern 4', DT::dataTableOutput("Org1_rp4"), plotOutput("boxplot4_g")),
      			tabPanel('Reaction Pattern 5', DT::dataTableOutput("Org1_rp5"), plotOutput("boxplot5_g")),
      			tabPanel('Results')
    	      ),

	#      strong(h3("Kidney")),      
	#      tabsetPanel(
	#		tabPanel('Reaction Pattern 1', DT::dataTableOutput("Trp1_k"), plotOutput("boxplot1_k")),
      	#		tabPanel('Reaction Pattern 2', DT::dataTableOutput("Trp2_k"), plotOutput("boxplot2_k")),
      	#		tabPanel('Reaction Pattern 3', DT::dataTableOutput("Trp3_k"), plotOutput("boxplot3_k")),
      	#		tabPanel('Reaction Pattern 4', DT::dataTableOutput("Trp4_k"), plotOutput("boxplot4_k")),
      	#		tabPanel('Reaction Pattern 5', DT::dataTableOutput("Trp5_k"), plotOutput("boxplot5_k")),
      	#		tabPanel('Results')
    	#      ),

	#      strong(h3("Liver")),      
	#      tabsetPanel(
	#		tabPanel('Reaction Pattern 1', DT::dataTableOutput("Trp1_l"), plotOutput("boxplot1_l")),
      	#		tabPanel('Reaction Pattern 2', DT::dataTableOutput("Trp2_l"), plotOutput("boxplot2_l")),
      	#		tabPanel('Reaction Pattern 3', DT::dataTableOutput("Trp3_l"), plotOutput("boxplot3_l")),
      	#		tabPanel('Reaction Pattern 4', DT::dataTableOutput("Trp4_l"), plotOutput("boxplot4_l")),
      	#		tabPanel('Reaction Pattern 5', DT::dataTableOutput("Trp5_l"), plotOutput("boxplot5_l")),
      	#		tabPanel('Results')
    	    #  ),

	    #  strong(h3("Skin")),      
	   #   tabsetPanel(
		#	tabPanel('Reaction Pattern 1', DT::dataTableOutput("Trp1_s"), plotOutput("boxplot1_s")),
      		#	tabPanel('Reaction Pattern 2', DT::dataTableOutput("Trp2_s"), plotOutput("boxplot2_s")),
      		#	tabPanel('Reaction Pattern 3', DT::dataTableOutput("Trp3_s"), plotOutput("boxplot3_s")),
      		#	tabPanel('Reaction Pattern 4', DT::dataTableOutput("Trp4_s"), plotOutput("boxplot4_s")),
      		#	tabPanel('Reaction Pattern 5', DT::dataTableOutput("Trp5_s"), plotOutput("boxplot5_s")),
      		#	tabPanel('Results')
    	     # ),
	      
	      textOutput('description')
	    )
	  )
	  ),





	  server = function(input, output) {

		output$organ1 <- renderText({
			inFile <- input$file1

			if (is.null(inFile))
			      return(NULL)

			rt<-read.table(inFile$datapath, header=input$header,sep=input$sep, quote=input$quote)
			
			factor1<-unique(rt[,1]) #Organs
			
			return(factor1[1])
			
		})

		output$Org1_rp1 <- DT::renderDataTable({ 

			inFile <- input$file1

			if (is.null(inFile))
			      return(NULL)

			rt<-read.table(inFile$datapath, header=input$header,sep=input$sep, quote=input$quote)
			
			factor1<-unique(rt[,1]) #Organs
			factor2<-unique(rt[,2]) #RPs

			#create a submatrix of first value of factor1  and first of factor2
			rt<-rt[which(rt[,1]==factor1[1] & rt[,2]==factor2[1]),]
		
			rt<-get_summary(rt,factor1[1],factor2[1],4)

		
		}, options=list(initComplete = JS( "function(settings, json) {", "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});","}")) #close options
		)



	    output$description <- renderText({input$desc})
	    output$intro <- renderText({"This is are the tables of reacction paterns"})

	  }
))
