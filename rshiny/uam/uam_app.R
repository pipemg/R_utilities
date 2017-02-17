#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

list.of.packages <- c("shiny", "DT" ,"ggplot2") #List the libraries
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])] #Get the new packages
if(length(new.packages)) install.packages(new.packages, dependencies=TRUE, clean=TRUE, verbose=FALSE,  repos="https://cloud.r-project.org") #Install the packages

library(shiny) # load shiny
library(DT)# DT
library(ggplot2) # load ggplot


get_plot<-function(table){
    alt<-table[,3]
    samp<-colnames(table[,-c(1:4)])
    vector<-as.vector(unlist(c(table[,-c(1:4)]))) #convert to a vector
    Alt<-rep(alt,length(samp))
    df<-data.frame(value=vector,Alt)
    p <- ggplot(df, aes(as.factor(Alt), value,  fill=Alt)) + 
      geom_boxplot(outlier.size = 1.5) + xlab(colnames(rt))  +
      stat_summary(fun.y="mean", colour="darkred", geom="point", size=2, show.legend = F)  
    return(p)
}


get_all<-function(iFile,h,s,q){

	

}





# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Reaction Pattern Index Calculator - Single"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      
      strong(h3("Upload the file you whant to use")),      
      fileInput('file1', 'Table of Importance Factors Reaction Pattern',accept=c('text/tsv','text/csv', 'text/comma-separated-values,text/plain', '.csv', '.tsv')),
      checkboxInput('header', 'Header', TRUE),
      column(6,radioButtons('sep', 'Separator', c(Tab='\t', Comma=',', Semicolon=';'), '\t')),
      column(6,radioButtons('quote', 'Quote', c(None='', 'Double Quote'='"', 'Single Quote'="'"), '')),
      tags$hr(),	      
      p(textAreaInput('desc', "Remarks:", value = "", cols = 300, rows = 3, placeholder = "Name", resize = "both"))
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      textOutput('intro'),
      strong(h3(textOutput("organ1"))),      
      tabsetPanel(
        tabPanel('Reaction Pattern 1',DT::dataTableOutput("Org1_rp1_table"),DT::dataTableOutput("Org1_rp1_results"),plotOutput("boxplot_Org1_rp1", width = "600px")),
        tabPanel('Reaction Pattern 2',DT::dataTableOutput("Org1_rp2_table"),DT::dataTableOutput("Org1_rp2_results"),plotOutput("boxplot_Org1_rp2", width = "600px")),
        tabPanel('Reaction Pattern 3',DT::dataTableOutput("Org1_rp3_table"),DT::dataTableOutput("Org1_rp3_results"),plotOutput("boxplot_Org1_rp3", width = "600px")),
        tabPanel('Reaction Pattern 4',DT::dataTableOutput("Org1_rp4_table"),DT::dataTableOutput("Org1_rp4_results"),plotOutput("boxplot_Org1_rp4", width = "600px")),
        tabPanel('Reaction Pattern 5',DT::dataTableOutput("Org1_rp5_table"),DT::dataTableOutput("Org1_rp5_results"),plotOutput("boxplot_Org1_rp5", width = "600px"))
      ),#End of tabsetPanel
      
      textOutput('description')
      
    ) #End of mainPanel
  ) # End of sidebarLayout
) #End of fluid page

# Define server logic required to draw a histogram






server <- function(input, output) {

	iMatrix<-reactive({
		inFile <- input$file1
		if (is.null(inFile))
			return(NULL)  

		iTable<-read.table(inFile$datapath, header=input$header,sep=input$sep, quote=input$quote)
		return(iTable)

	})

		
	
	get_matrix<-function(f1,f2){
		iTable<-iMatrix()
		factor1<-unique(iTable[,1]) #Organs
		factor2<-unique(iTable[,2]) #RPs
		matrix<-iTable[which(iTable[,1]==factor1[f1] & iTable[,2]==factor2[f2]),]
		matrix
	}


	get_organ<-function(f1){
		iTable<-iMatrix()
		organs<-unique(iTable[,1])
		return(as.character(organs[f1]))    
	}

	### FUNCTION GET SUMMARY
	get_summary<-function(org,rp,w){

		iTable<-iMatrix()

		factor1<-unique(iTable[,1]) #Organs
		factor2<-unique(iTable[,2]) #RPs
		mat<-iTable[which(iTable[,1]==factor1[org] & iTable[,2]==factor2[rp]),]
		W<-mat[,w]
		alt<-mat[,3]
		mat<-mat[,-c(1:4)]
		mat<-mat[1:10,]

		#rownames(matrix)<-alt
		
		#Calculamos el Indice I_org_rp (Sumatoria de los pesos de un rp para un organo determinado
		#matrix[paste("I",org,rp,sep="_"),]<-t(colSums(apply(matrix,2,function(x){x*W})))
		#matrix["mean",]<-t(apply(X=matrix,MARGIN=2,FUN=mean))
#
		#matrix[,"dim"]=dim(matrix)
		
		
		#we calculate the summary by rows
		#rowsummary<-as.data.frame(t(apply(matrix,1,summary)))
		#Agregamos la suma por renglones 
		#rowsummary$TOTAL<-rowSums(matrix)
		#Agregamos la varianza por renglones 
		#rowsummary$VAR<-round(apply(matrix,1,var), digits = 2)
		#Agregamos la desviación estandar por renglones 
		#rowsummary$SD<-round(apply(matrix,1,sd), digits = 2)
		#ALT<-c(as.vector(alt),paste("I",org,rp,sep="_"))
		#W<-c(W,0)
		#matrix<-cbind(W,rowsummary)
		
		return(mat)
		
	}


#ORGAN 1 TABLES
	output$Org1_rp1_table<-DT::renderDataTable({get_matrix(1,1)		
		},options=list(initComplete = JS( "function(settings, json) {", "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});","}"))) #PRINT THE TABLE 

	output$Org1_rp2_table<-DT::renderDataTable({get_matrix(1,2)		
		},options=list(initComplete = JS( "function(settings, json) {", "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});","}"))) #PRINT THE TABLE 

	output$Org1_rp3_table<-DT::renderDataTable({get_matrix(1,3)		
		},options=list(initComplete = JS( "function(settings, json) {", "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});","}"))) #PRINT THE TABLE 

	output$Org1_rp4_table<-DT::renderDataTable({get_matrix(1,4)		
		},options=list(initComplete = JS( "function(settings, json) {", "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});","}"))) #PRINT THE TABLE 

	output$Org1_rp5_table<-DT::renderDataTable({get_matrix(1,5)		
		},options=list(initComplete = JS( "function(settings, json) {", "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});","}"))) #PRINT THE TABLE 


#ORGAN 1 SUMMARY
	#output$Org1_rp1_results<-DT::renderDataTable({get_summary(1,1,4)},
	#	options=list(initComplete = JS( "function(settings, json) {", "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});","}"))) #PRINT THE SUMMARY 

	#output$Org1_rp2_results<-DT::renderDataTable({get_summary(1,2,4)		
	#	},options=list(initComplete = JS( "function(settings, json) {", "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});","}"))) #PRINT THE SUMMARY 

	#output$Org1_rp3_results<-DT::renderDataTable({get_summary(1,3,4)		
	#	},options=list(initComplete = JS( "function(settings, json) {", "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});","}"))) #PRINT THE SUMMARY 

	#output$Org1_rp4_results<-DT::renderDataTable({get_summary(1,4,4)		
	#	},options=list(initComplete = JS( "function(settings, json) {", "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});","}"))) #PRINT THE SUMMARY 

	#output$Org1_rp5_results<-DT::renderDataTable({get_summary(1,5,4)		
	#	},options=list(initComplete = JS( "function(settings, json) {", "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});","}"))) #PRINT THE SUMMARY   

     
	output$description <- renderText({get_summary(1,1,4)})
	#output$description <- renderText({input$desc})
	output$intro <- renderText({"This is are the tables of reacction paterns"})

  
}

# Run the application 
shinyApp(ui = ui, server = server)


