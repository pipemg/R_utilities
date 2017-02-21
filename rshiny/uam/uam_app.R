#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
options(shiny.deprecation.messages=FALSE)
list.of.packages <- c("shiny", "DT" ,"ggplot2") #List the libraries
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])] #Get the new packages
if(length(new.packages)) install.packages(new.packages, dependencies=TRUE, clean=TRUE, verbose=FALSE,  repos="https://cloud.r-project.org") #Install the packages

library(shiny) # load shiny
library(DT)# DT
library(ggplot2) # load ggplot



#get_all<-function(iFile,h,s,q){

	

#}





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
      p(textAreaInput('description', "Remarks:", value = "", cols = 300, rows = 3, placeholder = "Name", resize = "both"))
      
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

      strong(h3(textOutput("organ2"))),      
      tabsetPanel(
        tabPanel('Reaction Pattern 1',DT::dataTableOutput("Org2_rp1_table"),DT::dataTableOutput("Org2_rp1_results"),plotOutput("boxplot_Org2_rp1", width = "600px")),
        tabPanel('Reaction Pattern 2',DT::dataTableOutput("Org2_rp2_table"),DT::dataTableOutput("Org2_rp2_results"),plotOutput("boxplot_Org2_rp2", width = "600px")),
        tabPanel('Reaction Pattern 3',DT::dataTableOutput("Org2_rp3_table"),DT::dataTableOutput("Org2_rp3_results"),plotOutput("boxplot_Org2_rp3", width = "600px")),
        tabPanel('Reaction Pattern 4',DT::dataTableOutput("Org2_rp4_table"),DT::dataTableOutput("Org2_rp4_results"),plotOutput("boxplot_Org2_rp4", width = "600px")),
        tabPanel('Reaction Pattern 5',DT::dataTableOutput("Org2_rp5_table"),DT::dataTableOutput("Org2_rp5_results"),plotOutput("boxplot_Org2_rp5", width = "600px"))
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

		
	
	get_matrix<-function(f1,f2,w){
		iTable<-iMatrix()
		factor1<-unique(iTable[,1]) #Organs
		factor2<-unique(iTable[,2]) #RPs
		matrix<-iTable[which(iTable[,1]==factor1[f1] & iTable[,2]==factor2[f2]),]
		W<-matrix[,w]
		#matrix[paste("I",org,rp,sep="_"),]<-
		#Ident<-colSums( apply( matrix,2,function(x){x*W} ) )
		#Ident<-colSums( apply( matrix,2,function(x) x*W ) )
		#c(factor1[f1],factor2[f2],0,)
		#rbind(matrix,)
		#Ident
		matrix
	}



	get_plot<-function(org,rp){

		iTable<-iMatrix()

		factor1<-unique(iTable[,1]) #Organs
		factor2<-unique(iTable[,2]) #RPs
		mat<-iTable[which(iTable[,1]==factor1[org] & iTable[,2]==factor2[rp]),]

		vector<-as.vector(unlist(c(mat[,-c(1:4)]))) #convert to a vector
		Alt<-rep(mat[,3],ncol(mat[,-c(1:4)]))
		df<-data.frame(vector,Alt)

		p <- ggplot(df, aes(as.factor(Alt), vector,  fill=Alt)) + 
		geom_boxplot(outlier.size = 1.5) + xlab(colnames(rt))  +
		stat_summary(fun.y="mean", colour="darkred", geom="point", size=2, show.legend = F)  
		return(p)
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

		rowsummary<-apply(as.data.frame(mat),1,summary)
				
		mat<-data.frame(as.vector(mat))
		TOTAL<-as.vector(round(apply(mat,1,sum), digits = 3))
		VAR<-as.vector(round(apply(mat,1,var), digits = 3))
		SD<-as.vector(round(apply(mat,1,sd), digits = 3))


		matrix<-cbind(W,t(rowsummary),TOTAL,VAR,SD)
		#rownames(matrix)<-paste(factor1[org],factor2[rp],alt,sep="_")
		matrix
		
	}


#ORGAN 1
	output$organ1<-renderText({get_organ(1)})


#ORGAN 1 TABLES
	output$Org1_rp1_table<-DT::renderDataTable({get_matrix(1,1,4)		
		},options=list(initComplete = JS( "function(settings, json) {", "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});","}"))) #PRINT THE TABLE 

	output$Org1_rp2_table<-DT::renderDataTable({get_matrix(1,2,4)		
		},options=list(initComplete = JS( "function(settings, json) {", "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});","}"))) #PRINT THE TABLE 

	output$Org1_rp3_table<-DT::renderDataTable({get_matrix(1,3,4)		
		},options=list(initComplete = JS( "function(settings, json) {", "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});","}"))) #PRINT THE TABLE 

	output$Org1_rp4_table<-DT::renderDataTable({get_matrix(1,4,4)		
		},options=list(initComplete = JS( "function(settings, json) {", "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});","}"))) #PRINT THE TABLE 

	output$Org1_rp5_table<-DT::renderDataTable({get_matrix(1,5,4)		
		},options=list(initComplete = JS( "function(settings, json) {", "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});","}"))) #PRINT THE TABLE 


#ORGAN 1 SUMMARY
	output$Org1_rp1_results<-DT::renderDataTable({get_summary(1,1,4)},
		options=list(initComplete = JS( "function(settings, json) {", "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});","}"))) #PRINT THE SUMMARY 

	output$Org1_rp2_results<-DT::renderDataTable({get_summary(1,2,4)		
		},options=list(initComplete = JS( "function(settings, json) {", "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});","}"))) #PRINT THE SUMMARY 

	output$Org1_rp3_results<-DT::renderDataTable({get_summary(1,3,4)		
		},options=list(initComplete = JS( "function(settings, json) {", "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});","}"))) #PRINT THE SUMMARY 

	output$Org1_rp4_results<-DT::renderDataTable({get_summary(1,4,4)		
		},options=list(initComplete = JS( "function(settings, json) {", "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});","}"))) #PRINT THE SUMMARY 

	output$Org1_rp5_results<-DT::renderDataTable({get_summary(1,5,4)		
		},options=list(initComplete = JS( "function(settings, json) {", "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});","}"))) #PRINT THE SUMMARY   

#ORGAN 1 BOXPLOTS
     	output$boxplot_Org1_rp1 <- reactivePlot(function() { get_plot(1,1) })
     	output$boxplot_Org1_rp2 <- reactivePlot(function() { get_plot(1,2) })
     	output$boxplot_Org1_rp3 <- reactivePlot(function() { get_plot(1,3) })
     	output$boxplot_Org1_rp4 <- reactivePlot(function() { get_plot(1,4) })
     	output$boxplot_Org1_rp5 <- reactivePlot(function() { get_plot(1,5) })


#ORGAN 2
	output$organ2<-renderText({get_organ(2)})


#ORGAN 2 TABLES
	output$Org2_rp1_table<-DT::renderDataTable({get_matrix(2,1,4)		
		},options=list(initComplete = JS( "function(settings, json) {", "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});","}"))) #PRINT THE TABLE 

	output$Org2_rp2_table<-DT::renderDataTable({get_matrix(2,2,4)		
		},options=list(initComplete = JS( "function(settings, json) {", "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});","}"))) #PRINT THE TABLE 

	output$Org2_rp3_table<-DT::renderDataTable({get_matrix(2,3,4)		
		},options=list(initComplete = JS( "function(settings, json) {", "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});","}"))) #PRINT THE TABLE 

	output$Org2_rp4_table<-DT::renderDataTable({get_matrix(2,4,4)		
		},options=list(initComplete = JS( "function(settings, json) {", "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});","}"))) #PRINT THE TABLE 

	output$Org2_rp5_table<-DT::renderDataTable({get_matrix(2,5,4)		
		},options=list(initComplete = JS( "function(settings, json) {", "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});","}"))) #PRINT THE TABLE 


#ORGAN 2 SUMMARY
	output$Org2_rp1_results<-DT::renderDataTable({get_summary(2,1,4)},
		options=list(initComplete = JS( "function(settings, json) {", "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});","}"))) #PRINT THE SUMMARY 

	output$Org2_rp2_results<-DT::renderDataTable({get_summary(2,2,4)		
		},options=list(initComplete = JS( "function(settings, json) {", "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});","}"))) #PRINT THE SUMMARY 

	output$Org2_rp3_results<-DT::renderDataTable({get_summary(2,3,4)		
		},options=list(initComplete = JS( "function(settings, json) {", "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});","}"))) #PRINT THE SUMMARY 

	output$Org2_rp4_results<-DT::renderDataTable({get_summary(2,4,4)		
		},options=list(initComplete = JS( "function(settings, json) {", "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});","}"))) #PRINT THE SUMMARY 

	output$Org2_rp5_results<-DT::renderDataTable({get_summary(2,5,4)		
		},options=list(initComplete = JS( "function(settings, json) {", "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});","}"))) #PRINT THE SUMMARY   

	output$organ3<-renderText({get_organ(3)})
	output$organ4<-renderText({get_organ(4)})
	output$organ5<-renderText({get_organ(5)})



	#output$description <- renderText({get_summary(1,1,4)})
	output$description <- renderText({input$desc})
	output$intro <- renderText({"This is are the tables of reacction paterns"})

  
}

# Run the application 
shinyApp(ui = ui, server = server)


