# setwd("C:/Users/bradiej/Documents/BWRA_Tool//App")

library(shiny)
# library(geosphere)
enableBookmarking("disable")

earth.dist <- function (long1, lat1, long2, lat2)
{
  rad <- pi/180
  a1 <- lat1 * rad
  a2 <- long1 * rad
  b1 <- lat2 * rad
  b2 <- long2 * rad
  dlon <- b2 - a2
  dlat <- b1 - a1
  a <- (sin(dlat/2))^2 + cos(a1) * cos(b1) * (sin(dlon/2))^2
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  R <- 6378.145
  d <- R * c
  return(d)
}

textInputRow<-function (inputId, label, value = "") 
{
  div(style="display:inline-block",
      tags$label(label, `for` = inputId), 
      tags$input(id = inputId, type = "text", value = value,class="input-small"))
}

dateInputRow<-function (inputId, label, value=NULL) 
{
  div(style="display:inline-block",
      tags$label(label, `for` = inputId), 
      tags$input(id = inputId, type = "date", value=NULL,class="input-small"))
}

alldata=read.csv("port_data.csv")
# shipdata=read.csv("master_ship_list.csv")
alldata$PortName=as.character(alldata$PortName)
# shipdata$ShipName=as.character(shipdata$ShipName)
data=alldata[which(!is.na(alldata$PortName)),]
cols_interest=c(5,6,7,8)
alldata[cols_interest]=apply(alldata[cols_interest],2,scale)


ui <- fluidPage(
  
  tabsetPanel(
    tabPanel("Step 1: Basic Data",
             titlePanel("BWIP Tool"),
             dateInput("date", "Date", value = NULL, min = NULL, max = NULL, format = "yyyy-mm-dd", 
                       startview = "month", weekstart = 0, language = "en", width = NULL),
             # selectInput("arrival_port", "Choose port of arrival:", 
             # choices = c("",data$PortName[which(data$Country=="CAN")]),selectize=TRUE),
             # tags$style(type='text/css', ".selectize-dropdown-content {max-height: 400px; }"),
             sliderInput("n_arrivals","How many ships are arriving?",
                         min=1,max=25,value=2),
             
             wellPanel(tags$b(paste("Enter the ship names:")),
                       uiOutput("nships")),
             wellPanel(tags$b(paste("Enter the expected arrival date:")),
                       uiOutput("dates")),
             tags$b("Note: After filling in information above, proceed to Step 2a: Tank Data")),
    
    
    tabPanel("Step 2a: Tank Data",
             tags$h2("Enter tank data"),
             wellPanel(tags$b(paste("Enter the number of unique ballast source/ ballast discharge combinations for each ship:")),
                       uiOutput("ntanks")),
             
             tags$b("Note: After filling in information above for each ship, proceed to Step 2b: Data type")),
    
    tabPanel("Step 2b: Data type",
             tags$h2("Enter ship data"),
             uiOutput("mytabs"),
             
             tags$b("Note: After filling in information above for each ship, proceed to Step 3: Ballast information")),
    
    tabPanel("Step 3: Ballast information",
             tags$body("Note: Form may take several minutes to populate"),
             tags$body(""),
             uiOutput("tanklocale"),
             tags$b("Note: After filling in information above for each ship, proceed to Step 4: Results")),
    
    tabPanel("Step 4: Results",
             tags$h2("Risk Ratings"),
             tags$body("Note: Results may take several minutes to populate.  Wait until results display before downloading."),
             tags$body(""),
             # htmlOutput("text"),
             tableOutput("restable"),
             tags$body(""),
             downloadButton('downloadtable', 'Download results'))
    # tags$body("Note: Wait until results display above before downlaoding")
  )
)

server <- function(input, output) {
  
  output$nships<-renderUI({
    n_ships <- input$n_arrivals
    lapply(1:n_ships,function(s) {
      textInputRow(paste0("shipname",s),
                   paste("Ship ",s,sep=""),
                   value="")
    })
    
    # output$nships<-renderUI({
    # n_ships <- input$n_arrivals
    # lapply(1:n_ships,function(s) {
    # selectInput(paste0("shipname",s),paste("Ship ",s,sep=""),choices=c("",sort(shipdata$ShipName)),selected=NULL,selectize=TRUE)
    # })
    
  })
  output$dates<-renderUI({
    n_ships <- input$n_arrivals
    lapply(1:n_ships,function(s) {
      dateInputRow(paste0("date",s),paste("Ship ",s,sep=""), value = NULL)
    })
  })
  
  output$ntanks<-renderUI({
    n_arrivals <- input$n_arrivals
    
    lapply(1:n_arrivals,function(s) {
      numericInput(paste0("tanks",s),
                   input[[paste0("shipname", s)]],
                   value=1,min=1,max=20)
    })
  })
  
  output$mytabs = renderUI({
    nTabs=input$n_arrivals
    myTabs <<-NULL
    myTabs=lapply(1:nTabs, function (i) {
      
      tabPanel(input[[paste0("shipname", i)]],
               
               splitLayout(
                 # wellPanel(
                 # lapply(1:input[[paste0("tanks", i)]],function (n) {
                 # dateInput(paste("filldate",i,"_",n,sep=""),
                 #           paste("Tank ",n,": Enter fill date.",sep=""),
                 #           value=0)})),
                 
                 wellPanel(lapply(1:input[[paste0("tanks", i)]],function (n) {
                   
                   textInput(paste("tanknames",i,"_",n,sep=""),
                             paste("Route ",n,": Enter tank names",sep=""),value="")})),
                   
                 # wellPanel(lapply(1:input[[paste0("tanks", i)]],function (n) {
                 #   numericInput(paste("vol",i,"_",n,sep=""),
                 #                paste("Tank ",n,": Enter volume",sep=""),
                 #                value=0)}),
                 #   
                 #   selectInput("volume_units","Enter units for volume",choices=c("m3","MT","LT","ST","gal"))),
                 # 
                 wellPanel(lapply(1:input[[paste0("tanks", i)]],function (n) {
                   
                   radioButtons(paste("input_type",i,"_",n,sep=""),
                                paste("Source ",n, ": Is source a port or Lat & Long value?",sep=""),
                                choices=c("Port","Lat & Long"),
                                selected="Port",
                                inline=T)
                 })
                 ),
                 
                 wellPanel(lapply(1:input[[paste0("tanks", i)]],function (n) {
                   
                   radioButtons(paste("discharge_type",i,"_",n,sep=""),
                                paste("Discharge location ",n, ": Is location a port or Lat & Long value?",sep=""),
                                choices=c("Port","Lat & Long"),
                                selected="Port",
                                inline=T)
                   
                 }))
                 
               )
               # )
      )
    })
    
    do.call(tabsetPanel, myTabs)
  })
  
  output$tanklocale = renderUI({
    nTabs=input$n_arrivals
    myTabs <<-NULL
    myTabs=lapply(1:nTabs, function (i) {
      
      tabPanel(input[[paste0("shipname", i)]],
               
               lapply(1:input[[paste0("tanks", i)]],function (n) {
                 if(input[[paste0("input_type",i,"_",n)]]=="Port"&input[[paste0("discharge_type",i,"_",n)]]=="Port") {
                   tempcount=0
                   wellPanel(lapply(1:2,function (a) {
                     tempcount=tempcount+1
                     name=NULL
                     name[1]=paste("Route ",n,": Type ballast source",sep="")
                     name[2]=paste("Route ",n,": Type discharge location",sep="")
                     # name[3]=paste("Route ",n,": Type tank names",sep="")
                     in_name=NULL
                     in_name[1]=paste("source",i,"_",n,sep="")
                     in_name[2]=paste("discharge",i,"_",n,sep="")
                     # in_name[3]=paste("tanknames",i,"_",n,sep="")
                     # selections=NULL
                     # selections[1]=list(sort(data$PortName))
                     # selections[2]=list(sort(data$PortName[which(data$Country=="CAN")]))
                     selectInput(in_name[a],name[a],choices=c("",sort(data$PortName)),selected=NULL)
                     # if (a!=3) {selectInput(in_name[a],name[a],choices=c("",sort(data$PortName)),selected=NULL)}
                     # if (a==3) {textInput(in_name[a],"Enter tank names",value="")}                                  
                   }))
                   
                 } else if (input[[paste0("input_type",i,"_",n)]]=="Port"&input[[paste0("discharge_type",i,"_",n)]]=="Lat & Long") {
                   
                   wellPanel(lapply(1:3,function (l) {
                     name=NULL
                     name[1]=paste("Route ",n,": Type ballast source",sep="")
                     name[2]=paste("Route ",n,": Enter latitude for ballast discharge (e.g. -45.65, +20.40)",sep="")
                     name[3]=paste("Route ",n,": Enter longitude for ballast discharge",sep="")
                     in_name=NULL
                     in_name[1]=paste("source",i,"_",n,sep="")
                     in_name[2]=paste("discharge",i,"_",n,sep="")
                     in_name[3]=paste("discharge",i,"_",n,"long",sep="")
                     function_type=NULL
                     function_type[1]="selectInput"
                     function_type[2]="numericInput"
                     function_type[3]="numericInput"
                     arg_list=list()
                     arg_list[[1]]=list(in_name[1],name[1],choices=c("",sort(data$PortName)),selected=NULL)
                     arg_list[[2]]=list(in_name[2],name[2],value=0)
                     arg_list[[3]]=list(in_name[3],name[3],value=0)
                     do.call(function_type[l],arg_list[[l]])
                   }))
                   
                 } else if (input[[paste0("input_type",i,"_",n)]]=="Lat & Long"&input[[paste0("discharge_type",i,"_",n)]]=="Port") {
                   
                   wellPanel(lapply(1:3,function (l) {
                     name=NULL
                     name[1]=paste("Route ",n,": Enter latitude for ballast source (e.g. -45.65, +20.40)",sep="")
                     name[2]=paste("Route ",n,": Enter longitude for ballast source",sep="")
                     name[3]=paste("Route ",n,": Type ballast discharge location",sep="")
                     in_name=NULL
                     in_name[1]=paste("source",i,"_",n,sep="")
                     in_name[2]=paste("source",i,"_",n,"long",sep="")
                     in_name[3]=paste("discharge",i,"_",n,sep="")
                     function_type=NULL
                     function_type[1]="numericInput"
                     function_type[2]="numericInput"
                     function_type[3]="selectInput"
                     arg_list=list()
                     arg_list[[1]]=list(in_name[1],name[1],value=0)
                     arg_list[[2]]=list(in_name[2],name[2],value=0)
                     # arg_list[[3]]=list(in_name[3],name[3],choices=c("",sort(data$PortName[which(data$Country=="CAN")])),selected=NULL)
                     arg_list[[3]]=list(in_name[3],name[3],choices=c("",sort(data$PortName)),selected=NULL)
                     do.call(function_type[l],arg_list[[l]])
                   }))
                   
                 } else if (input[[paste0("input_type",i,"_",n)]]=="Lat & Long"&input[[paste0("discharge_type",i,"_",n)]]=="Lat & Long") {
                   
                   wellPanel(lapply(1:4,function (l) {
                     name=NULL
                     name[1]=paste("Route ",n,": Enter latitude for ballast source (e.g. -45.65, +20.40)",sep="")
                     name[2]=paste("Route ",n,": Enter longitude for ballast source",sep="")
                     name[3]=paste("Route ",n,": Enter latitude for ballast discharge (e.g. -45.65, +20.40)",sep="")
                     name[4]=paste("Route ",n,": Enter longitude for ballast discharge",sep="")
                     in_name=NULL
                     in_name[1]=paste("source",i,"_",n,sep="")
                     in_name[2]=paste("source",i,"_",n,"long",sep="")
                     in_name[3]=paste("discharge",i,"_",n,sep="")
                     in_name[4]=paste("discharge",i,"_",n,"long",sep="")
                     
                     arg_list=list()
                     arg_list[[1]]=list(in_name[1],name[1],value=0)
                     arg_list[[2]]=list(in_name[2],name[2],value=0)
                     arg_list[[3]]=list(in_name[3],name[3],value=0)
                     arg_list[[4]]=list(in_name[4],name[4],value=0)
                     do.call(numericInput,arg_list[[l]])
                   })) } 
                 
               })
      )
    })
    
    do.call(tabsetPanel, myTabs)
  })
  
  
  dtableInput<- reactive({
    risk<<-NULL
    risklist<<-NULL
    temps<<-NULL
    location_interest=alldata[which(alldata$PortName==input$arrival_port),cols_interest]
    count<<-0
    lapply(1:input$n_arrivals,function(i) {
      lapply(1:input[[paste0("tanks", i)]],function (n) {
        count<<-count+1
        if(is.character(input[[paste0("source", i,"_",n)]])&is.character(input[[paste0("discharge", i,"_",n)]])) {
          env_data1<<-alldata[which(alldata$PortName==input[[paste0("source", i,"_",n)]]),cols_interest]
          env_data2<<-alldata[which(alldata$PortName==input[[paste0("discharge", i,"_",n)]]),cols_interest]
          
        } else if(is.character(input[[paste0("source", i,"_",n)]])&is.numeric(input[[paste0("discharge", i,"_",n)]])) {
          env_data1<<-alldata[which(alldata$PortName==input[[paste0("source", i,"_",n)]]),cols_interest]
          
          dist_list<<-lapply(1:length(alldata[,1]), function (l) {
            earth.dist(input[[paste0("discharge", i,"_",n)]],input[[paste0("discharge", i,"_",n,"long")]],
                       as.numeric(alldata$Latitude[l]),as.numeric(alldata$Longitude[l])) })
          row_vals<<-which(dist_list==min(unlist(dist_list)))
          
          if(length(row_vals)>1) {
            tempdata=alldata[row_vals,cols_interest]
            env_data2<<-apply(tempdata,2,mean)}
          if(length(row_vals)==1) {
            env_data2<<-alldata[row_vals,cols_interest]}
          
        } else if(is.numeric(input[[paste0("source", i,"_",n)]])&is.character(input[[paste0("discharge", i,"_",n)]])) {
          
          env_data2<<-alldata[which(alldata$PortName==input[[paste0("discharge", i,"_",n)]]),cols_interest]
          
          dist_list<<-lapply(1:length(alldata[,1]), function (l) {
            earth.dist(input[[paste0("source", i,"_",n)]],input[[paste0("source", i,"_",n,"long")]],
                       as.numeric(alldata$Latitude[l]),as.numeric(alldata$Longitude[l])) })
          row_vals<<-which(dist_list==min(unlist(dist_list)))
          if(length(row_vals)>1) {
            tempdata=alldata[row_vals,cols_interest]
            env_data1<<-apply(tempdata,2,mean)}
          if(length(row_vals)==1) {
            env_data1<<-alldata[row_vals,cols_interest]}
          
        } else if(is.numeric(input[[paste0("source", i,"_",n)]])&is.numeric(input[[paste0("discharge", i,"_",n)]])) {
          
          dist_list<<-lapply(1:length(alldata[,1]), function (l) {
            earth.dist(input[[paste0("source", i,"_",n)]],input[[paste0("source", i,"_",n,"long")]],
                       as.numeric(alldata$Latitude[l]),as.numeric(alldata$Longitude[l])) })
          row_vals<<-which(dist_list==min(unlist(dist_list)))
          if(length(row_vals)>1) {
            tempdata=alldata[row_vals,cols_interest]
            env_data1<<-apply(tempdata,2,mean)}
          if(length(row_vals)==1) {
            env_data1<<-alldata[row_vals,cols_interest]}
          
          dist_list<<-lapply(1:length(alldata[,1]), function (l) {
            earth.dist(input[[paste0("discharge", i,"_",n)]],input[[paste0("discharge", i,"_",n,"long")]],
                       as.numeric(alldata$Latitude[l]),as.numeric(alldata$Longitude[l])) })
          row_vals<<-which(dist_list==min(unlist(dist_list)))
          
          if(length(row_vals)>1) {
            tempdata=alldata[row_vals,cols_interest]
            env_data2<<-apply(tempdata,2,mean)}
          if(length(row_vals)==1) {
            env_data2<<-alldata[row_vals,cols_interest]}
          
        } 
        
        risk[count]<<-round(sum((env_data1-env_data2)^2)^.5,digits=3) 
        risklist[count]<<-paste("Ship ",i,"Tank",n,": Environmental distance is",risk[count],sep=" ")
        
        print(count)
        # print(mem_used())
      })
    })
    
    temp=matrix(data=NA,nrow=count,ncol=7,dimnames=list(NULL,c("Tank info","Ballast source","Ballast destination","Environmental Distance","Risk level","Priority","Model Run Date")))
    today <- Sys.Date()
    
    new_counter<<-0
    lapply(1:input$n_arrivals,function(i) {
      lapply(1:input[[paste0("tanks", i)]],function (n) {
        # print("arrivals")
        # print(input$n_arrivals)
        # print("tanks")
        # print(input[[paste0("tanks", i)]])
        # print(count)
        # print(new_counter)
        # a=mem_used()
        print('a')
        # print(dim(temp))
        new_counter<<-new_counter+1
        temp[new_counter,1]<<-paste0(input[[paste0("shipname", i)]], ", Tanks ",input[[paste0("tanknames",i,"_",n)]],sep="")
        # temp[new_counter,2]<<-input[[paste0("date", i)]]  #Need to figure this out because number of tanks is not equal to number of dates...
        # print("This is dates")
        # print(output$dates)
        # print(count)
        # print(mem_used())
        
        # print(new_counter)
        # print(dim(temp))
        if(!is.numeric(input[[paste0("source", i,"_",n)]])) {
          temp[new_counter,2]<<-input[[paste0("source", i,"_",n)]]
        } else {temp[new_counter,2]<<-paste(input[[paste0("source", i,"_",n)]],",",input[[paste0("source", i,"_",n,"long")]])}
        if(!is.numeric(input[[paste0("discharge", i,"_",n)]])) {
          temp[new_counter,3]<<-input[[paste0("discharge", i,"_",n)]]
        } else {temp[new_counter,3]<<-paste(input[[paste0("discharge", i,"_",n)]],",",input[[paste0("discharge", i,"_",n,"long")]])}
        temp[new_counter,4]<<-risk[new_counter]
        if(temp[new_counter,4]<=0.787) {temp[new_counter,5]<<-"Very high risk"} #Top 10 percentile
        # print("here")
        if(temp[new_counter,4]>0.787&temp[new_counter,4]<=1.5) {temp[new_counter,5]<<-"High risk"}  #10 to 25th percentile
        if(temp[new_counter,4]>1.5&temp[new_counter,4]<=2.778) {temp[new_counter,5]<<-"Moderate risk"}  # 25 to 50 percentile
        if(temp[new_counter,4]>2.778&temp[new_counter,4]<=4.020) {temp[new_counter,5]<<-"Low risk"}  #50-70 percentile
        if(temp[new_counter,4]>4.020) {temp[new_counter,5]<<-"Very low risk"}  #less than 70th percentile
        # print(order(risk))
        # print(new_counter)
        # print('fog')
        # print(ls())
        # print(input[[paste0("shipname", i)]])
        # print(input[[date1]])
        # print(input[[paste0("date",i)]])
              
        # print(order(risk)[new_counter])
        temp[new_counter,6]<<-rank(risk,ties.method="min")[new_counter]
        temp[new_counter,7]<<-format(today, format="%B %d %Y")
        # temp[new_counter,8]<<-input[[paste0("dates",i)]]
      })      
      return(temp)
    })
    return(temp)
  })
  
  output$restable<-renderTable(dtableInput())
  
  
  output$downloadtable <- downloadHandler(
    filename = function() { paste('risk',Sys.Date(), '.csv', sep='') },
    content = function(file) {
      write.csv(dtableInput(), file)
    }
  )
  
  
}


shinyApp(ui = ui, server = server)
