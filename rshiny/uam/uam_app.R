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


### FUNCTION GET SUMMARY
get_summary<-function(table,org,rp,w){
  
  W<-table[,w]
  factors<-as.matrix(table[,1:2])
  alt<-table[,3]
  matrix<-table[,-c(1:4)]
  rownames(matrix)<-alt
  
  #Calculamos el Indice I_org_rp (Sumatoria de los pesos de un rp para un organo determinado
  matrix[paste("I",org,rp,sep="_"),]<-t(colSums(apply(matrix,2,function(x) x*W)))
  
  
  #we calculate the summary by rows
  rowsummary<-as.data.frame(t(apply(matrix,1,summary)))
  #Agregamos la suma por renglones 
  rowsummary$TOTAL<-rowSums(matrix)
  #Agregamos la varianza por renglones 
  rowsummary$VAR<-round(apply(matrix,1,var), digits = 2)
  #Agregamos la desviación estandar por renglones 
  rowsummary$SD<-round(apply(matrix,1,sd), digits = 2)
  ALT<-c(as.vector(alt),paste("I",org,rp,sep="_"))
  W<-c(W,0)
  matrix<-cbind(W,rowsummary)
  
  return(matrix)
  
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
        tabPanel('Reaction Pattern 1',DT::dataTableOutput("Org1_rp1_table"),DT::dataTableOutput("Org1_rp1_results"),plotOutput("boxplot_Org1_rp1", width = "600px"))  #,
        #tabPanel('Reaction Pattern 2',DT::dataTableOutput("Org1_rp2_table"),DT::dataTableOutput("Org1_rp2_results"),plotOutput("boxplot_Org1_rp2", width = "600px"))
      ),#End of tabsetPanel
      
      textOutput('description')
      
    ) #End of mainPanel
  ) # End of sidebarLayout
) #End of fluid page

# Define server logic required to draw a histogram






server <- function(input, output) {

	complete_list<-reactive({

		inFile <- input$file1
	    
		if (is.null(inFile))
			return(NULL)  


		iTable<-read.table(inFile$datapath, header=input$header,sep=input$sep, quote=input$quote)

		factor1<-unique(iTable[,1]) #Organs
		factor2<-unique(iTable[,2]) #RPs
	
		listM<-as.list();
		listR<-as.list();
		listP<-as.list();

		for(i in 1:length(factor1)){
			organ<-as.character(factor1[i]) #GET THE ORGAN NAME
			for(j in 1:length(factor2)){
	    			matrix<-iTable[which(iTable[,1]==factor1[i] & iTable[,2]==factor2[j]),] #GET THE SUB MATRIX
				results<-DT::renderDataTable({get_summary(matrix,factor1[i],factor2[j],4)},
					options=list(initComplete = JS( "function(settings, json) {", "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});","}"))) #PRINT THE RESULTS
				boxplot<- reactivePlot(function() { get_plot(matrix)}) #PRINT THE BOXPLOT
			}
	
			listM<-c(listM,matrix)
			listR<-c(listR,results)
			listP<-c(listP,boxplot)
		
		}

		return(listM[1])

	})



	output$Org1_rp1_table<-DT::renderDataTable({
			complete_list
		},options=list(initComplete = JS( "function(settings, json) {", "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});","}"))) #PRINT THE TABLE 
  
 	#output$Org1_rp1_results<-DT::renderDataTable({complete_list[2]},
	#		options=list(initComplete = JS( "function(settings, json) {", "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});","}"))) #PRINT THE TABLE 
  
	#output$boxplot_Org1_rp1<-reactivePlot(function() {complete_list[3]}) #PRINT THE TABLE 
     
  
	output$description <- renderText({input$desc})
	output$intro <- renderText({"This is are the tables of reacction paterns"})

  
}

# Run the application 
shinyApp(ui = ui, server = server)


