library(shiny)

# Define UI for data upload app ----
ui <- fluidPage(tabsetPanel(
  tabPanel("Construction of dataset",
           # App title ----
           titlePanel("Uploading File"),
           
           # Sidebar layout with input and output definitions ----
           sidebarLayout(
             
             # Sidebar panel for inputs ----
             sidebarPanel(
               
               # Input: Select a file ----
               fileInput("file1", "Choose CSV File",
                         multiple = TRUE,
                         accept = c("text/csv",
                                    "text/comma-separated-values,text/plain",
                                    ".csv")),
               
               # Horizontal line ----
               tags$hr(),
               
               # Input: Checkbox if file has header ----
               checkboxInput("header", "Header", TRUE),
               
               # Input: Select separator ----
               radioButtons("sep", "Separator",
                            choices = c(Comma = ",",
                                        Semicolon = ";",
                                        Tab = "\t"),
                            selected = ","),
               
               # Input: Select quotes ----
               radioButtons("quote", "Quote",
                            choices = c(None = "",
                                        "Double Quote" = '"',
                                        "Single Quote" = "'"),
                            selected = '"'),
               
               # Horizontal line ----
               tags$hr(),
               
               # Input: Select number of rows to display ----
               radioButtons("disp", "Display",
                            choices = c(Head = "head",
                                        All = "all"),
                            selected = "head")
               
             ),
             
             # Main panel for displaying outputs ----
             mainPanel(checkboxGroupInput("variables","Choose the variables you want to enrich your dataset",
                                          choiceNames = list("Terms(Go.Db)","Orthology(Kegg.db)","Motif(Kegg.db)","Pathway(Kegg.db)","Band(Biomart)","Transcript count(Biomart)","Gene biotype(Biomart)"),
                                          choiceValues = list("terms","orthology","motif","pathway","band","transcriptcount","genebiotype")
             ),
             textOutput("checkbox"),
             
             
             # Output: Data file ----
             textOutput("data1"),
             tableOutput("clusters"),
             tags$hr(),
             
             textOutput("data2"),
             
             tableOutput("refinal"),
             tags$hr(),
             
             textOutput("data3"),
             
             tableOutput("re2")
             )
             
             
           )
  ),
  tabPanel("Parameters",
           titlePanel("Select the parametres of your analysis"),
           sidebarLayout(
           sidebarPanel( 
          radioButtons(inputId = "algorithmcenter", label = "Choose if you want an optimal number of clusters or you want to decide the number of clusters", choices = c("optimal","mynumber"),selected =  "optimal") ,
           sliderInput("numalgorithm", "Select the number of custers", min=2, max=15,value = 2, step=1) ,
           radioButtons(inputId = "algorithm",label = "Select the algorithm you want to use(note that this choice continues to all the further analysis)",choices = c("kmeans","kmodes","kproto"), selected = "kmeans")
           #textOutput("checkmaxnumber")
           ),
          mainPanel(
                    tags$hr(),
                    textOutput("kmeanss"),
                    tableOutput("clust1"),   
                    tags$hr(),
                    textOutput("kmodess"),
                    tableOutput("clust2"),
                    tags$hr(),
                    textOutput("kprotoss"),
                    tableOutput("clust3")
                    
            
          )
  )
  ),
  tabPanel("Categorical data analysis",selectInput("barplotvar", label = "Select the variable you want to have the Barplot of a scecific cluster",choices = c("terms","orthology","motif","pathway1","pathway2","pathway3","pathway4","pathway5","pathwaynum","band","transcript_count","gene_biotype"), selected = NULL) ,
           selectInput("numcharact", label = "Choose the number of the most frequent characteristics for the variable you chose",choices = c(2:10)),
           selectInput("numclust", label = "Choose the number of the cluster you want to see the analysis ",choices = c(1:30),selected = 1),
           
           plotOutput("barplotclust1"),
           selectInput("columnbox", label = "Choose the column of arthmetic variable you want to analyze by reference to the categorical variable",choices = c(2:140),selected = 2),
           
           plotOutput("boxplot1"),
           textOutput("namesofvar"),
           tableOutput("refrequent"),
           
          # plotOutput("barplotclust2"),
          # plotOutput("barplotclust3"),
          # plotOutput("barplotclust4"),
          # plotOutput("barplotclust5"),
          # plotOutput("barplotclust6"),
          # plotOutput("barplotclust7"),
          # plotOutput("barplotclust8"),
          # plotOutput("barplotclust9"),
          
          # plotOutput("barplotclust10"),
          # plotOutput("barplotclust11"),
           #plotOutput("barplotclust12"),
           #plotOutput("barplotclust13"),
           #plotOutput("barplotclust14"),
          # plotOutput("barplotclust15"),
           tableOutput("recateg")
           
           ),

  tabPanel("Arithmetical data analysis",selectInput("columnchoice", label = "Select a column of numericaldata 1(select only numerical variables)",choices = c(2:130)) ,
           selectInput("columnchoice1", label = "Select a column of numericaldata 2(select only numerical variables)",choices = c(3:130)) ,
           
           plotOutput("clustofnumer"),
           plotOutput("boxplot2"),
           plotOutput("colourofclust")
          # tableOutput("check")
  ),
  
  tabPanel("Upset diagram",titlePanel("The most frequent values of two variables that are common for the genes of the cluster(you chosed in categorical data analysis) tade of the clustering") ,
           selectInput("barplotvar123", label = "Select the first categorical variable for the upset diagram",choices = c("terms","orthology","motif","pathway1","pathway2","pathway3","pathway4","pathway5","pathwaynum","band","transcript_count","gene_biotype"),selected = "terms"),
                       selectInput("barplotvar1234", label = "Select the second categorical variable for the upset diagram",choices = c("terms","orthology","motif","pathway1","pathway2","pathway3","pathway4","pathway5","pathwaynum","band","transcript_count","gene_biotype"),selected = NULL),
           plotOutput("upset"),
           textOutput("colnames1"),
           textOutput("colnames2"),
           textOutput("colnames3"),
           textOutput("colnames4"),
           textOutput("colnames5"),
           textOutput("colnames6")
           
  ),
  
  
  tabPanel("Sankey Diagram",titlePanel("The amount of the genes that belong in the same cluster for every clustering"),
           #textInput("ensemblid", "Put an ansembl id", "value"),
           sankeyNetworkOutput("sankeydiagram"),
           tableOutput("resankeysou"),
           tableOutput("resankeynames")),
           
  tabPanel("Entropy", 
           selectInput("barplotvar12", label = "Select the variable you want to see the barpolt for the entropy of every cluster",choices = c("terms","orthology","motif","pathway1","pathway2","pathway3","pathway4","pathway5","pathwaynum","band","transcript_count","gene_biotype"), selected = NULL) ,
           plotOutput("entropybarplot"),
           textOutput("entropytext"),
           textOutput("refrequentnames")
           
  ),
  tabPanel("Cohesion", 
           #tableOutput("clust4"),
           plotOutput("plotgower"),
           textOutput("colorss")
           
  )
  
)
)

# Define server logic to read selected file ----
server <- function(input, output) {
  library(networkD3)
  library(UpSetR)
  library(cluster)
  library(dplyr)
  library(ggplot2)
  library(readr)
  library(Rtsne)
  library(GO.db)
  library(hugene20sttranscriptcluster.db)
  library(KEGGREST)
  
  
  #=======#Dhmiourgia dataset
  
  output$data1<-renderText({
    "The data you asked"
  })
  output$data2<-renderText({
    "Final data"
  })
  output$data3<-renderText({
    "Dara of arithmetical counts"
  })
  output$kmeanss<-renderText({
    "KMEANS CLUSTERING RESULTS"
  })
  output$kmodess<-renderText({
    "KMODES CLUSTERING RESULTS"
  })
  output$kprotoss<-renderText({
    "KPROTO CLUSTERING RESULTS"
  })
  
  #akatergasta countdata
  re1<-reactive({
    req(input$file1)
    df2 <- read.csv(input$file1$datapath,
                    header = input$header,
                    sep = input$sep,
                    quote = input$quote)})
  
  #katergasemna countdata
  re2<-reactive({thresh <- re1()[,-1] > 0
  keep <- rowSums(thresh) >= 1
  #the final counts that we keep
  countdataa<-re1()[keep,]
  colnames(countdataa)[1]<-"ensemblid"
  return(countdataa)
  })
  
  #pinakas ensemblids
  reensembl<-reactive({
    ensemblids<- re2()[,1]
    ensemblids<-as.character(ensemblids)
    return(ensemblids)
  })
  
  #katergasemna categoricaldata
  refinal<-reactive({
    categoricaldataa<- data.frame( name=reensembl())
    colnames(categoricaldataa)[1]<-"ensemblid"
    
    if(isTRUE(any(input$variables=="terms")))
    {library(GO.db)
      library(hugene20sttranscriptcluster.db)
      Xgo<-c()
      XXgo<-c()
      d<-data.frame( name=reensembl())
      Xgo<-select(hugene20sttranscriptcluster.db, reensembl(),"GO", "ENSEMBL")
      XXgo<-select( GO.db::GO.db ,Xgo$GO,"TERM","GOID")
      XXgo$ensemblid<-Xgo$ENSEMBL
      d <- data.frame( name=reensembl())
      for ( i in seq_along(reensembl())){gos_for_a_gene<-XXgo[which(XXgo$ensembl==reensembl()[i]),"TERM"]
      d$terms[i]<-gos_for_a_gene[length(gos_for_a_gene)]}
      colnames(d)[1]<-"ensemblid"
      categoricaldataa<-merge(categoricaldataa, d, 
                              by.categoricaldataa = categoricaldataa$ensemblid, by.d = d$ensemblid, all = FALSE, all.categoricaldataa = all, all.d = all,
                              sort = TRUE, suffixes = c(".x",".y"), no.dups = TRUE)
      categoricaldataa[is.na(categoricaldataa)]="nothing"
      categoricaldataa$terms<-factor(categoricaldataa$terms)
      
    }
    if(isTRUE(any(input$variables=="orthology"))){
      library(KEGGREST)
      entrezgenenames<-select(hugene20sttranscriptcluster.db::hugene20sttranscriptcluster.db,reensembl(),"ENTREZID", "ENSEMBL")
      dd1<- data.frame( name=reensembl())
      colnames(dd1)[1]<-"ensemblid"
      n<-0
      #loop for accessing kegg as a list
      for (n in seq_along(entrezgenenames[,2]))
      {
        entr<-paste("hsa", entrezgenenames[n,2], sep=":")
        keggquery1<-tryCatch(KEGGREST::keggGet(entr), error=function(e) NULL)
        if(!(is.null(keggquery1[[1]]$ORTHOLOGY)))
        {dd1$orthology[n]<-keggquery1[[1]]$ORTHOLOGY}
        else{dd1$orthology[n]<-NA} }
      
      categoricaldataa<-merge(categoricaldataa, dd1, 
                              by.categoricaldataa = categoricaldataa$ensemblid, by.dd1 = dd1$ensemblid, all = FALSE, all.categoricaldataa = all, all.dd1 = all,
                              sort = TRUE, suffixes = c(".x",".y"), no.dups = TRUE)
      categoricaldataa[is.na(categoricaldataa)]="nothing"
      categoricaldataa$orthology<-factor(categoricaldataa$orthology)
    }
    
    if(isTRUE(any(input$variables=="motif"))){
      library(KEGGREST)
      entrezgenenames<-select(hugene20sttranscriptcluster.db,reensembl(),"ENTREZID", "ENSEMBL")
      dd2<- data.frame( name=reensembl())
      colnames(dd2)[1]<-"ensemblid"
      n<-0
      #loop for accessing kegg as a list
      for (n in seq_along(entrezgenenames[,2]))
      {
        entr<-paste("hsa", entrezgenenames[n,2], sep=":")
        keggquery1<-tryCatch(KEGGREST::keggGet(entr), error=function(e) NULL)
        if(!(is.null(keggquery1[[1]]$MOTIF)))
        {dd2$motif[n]<-keggquery1[[1]]$MOTIF}
        else{dd2$motif[n]<-NA} }
      
      categoricaldataa<-merge(categoricaldataa, dd2, 
                              by.categoricaldataa = categoricaldataa$ensemblid, by.dd2 = dd2$ensemblid, all = FALSE, all.categoricaldataa = all, all.dd2 = all,
                              sort = TRUE, suffixes = c(".x",".y"), no.dups = TRUE)
      categoricaldataa[is.na(categoricaldataa)]="nothing"
      categoricaldataa$motif<-factor(categoricaldataa$motif)
    }
    if(isTRUE(any(input$variables=="pathway"))){
      dd3<- data.frame( name=reensembl())
      colnames(dd3)[1]<-"ensemblid"
      n<-0
      library(KEGGREST)
      entrezgenenames<-select(hugene20sttranscriptcluster.db,reensembl(),"ENTREZID", "ENSEMBL")
      for (n in seq_along(entrezgenenames[,2]))
      {
        entr<-paste("hsa", entrezgenenames[n,2], sep=":")
        keggquery1<-tryCatch(KEGGREST::keggGet(entr), error=function(e) NULL)
        if(!(is.null(keggquery1[[1]]$PATHWAY)))
        {dd3$pathway1[n]<-keggquery1[[1]]$PATHWAY[1]
        dd3$pathway2[n]<-keggquery1[[1]]$PATHWAY[2]
        dd3$pathway3[n]<-keggquery1[[1]]$PATHWAY[3]
        dd3$pathway4[n]<-keggquery1[[1]]$PATHWAY[4]
        dd3$pathway5[n]<-keggquery1[[1]]$PATHWAY[5]
        dd3$pathwaynum[n]<-length(keggquery1[[1]]$PATHWAY)}
        else{dd3$pathway1[n]<-NA
        dd3$pathway2[n]<-NA
        dd3$pathway3[n]<-NA
        dd3$pathway4[n]<-NA
        dd3$pathway5[n]<-NA
        dd3$pathwaynum[n]<-0}
        
      }
      categoricaldataa<-merge(categoricaldataa, dd3, 
                              by.categoricaldataa = categoricaldataa$ensemblid, by.dd3 = dd3$ensemblid, all = FALSE, all.categoricaldataa = all, all.dd3 = all,
                              sort = TRUE, suffixes = c(".x",".y"), no.dups = TRUE)
      categoricaldataa[is.na(categoricaldataa)]="nothing"
      categoricaldataa$pathway1<-factor(categoricaldataa$pathway1)  
      categoricaldataa$pathway2<-factor(categoricaldataa$pathway2)  
      categoricaldataa$pathway3<-factor(categoricaldataa$pathway3)  
      categoricaldataa$pathway4<-factor(categoricaldataa$pathway4)  
      categoricaldataa$pathway5<-factor(categoricaldataa$pathway5)  
      categoricaldataa$pathwaynum<-factor(categoricaldataa$pathwaynum)  
      
    }
    if(isTRUE(any(input$variables=="band"))){
      dd4<- data.frame( name=reensembl())
      colnames(dd4)[1]<-"ensemblid"
      library("biomaRt")
      #ensembl = useMart(biomart= "ensembl",dataset="hsapiens_gene_ensembl")
      mart <- useMart("ENSEMBL_MART_ENSEMBL")
      mart <- useMart("ENSEMBL_MART_ENSEMBL","hsapiens_gene_ensembl")
      dd4<-getBM(c("ensembl_gene_id",listAttributes(mart)[13,1]), c("ensembl_gene_id"), reensembl(), mart)
      colnames(dd4)[1]<-"ensemblid"
      categoricaldataa<-merge(categoricaldataa, dd4, 
                              by.categoricaldataa = categoricaldataa$ensemblid, by.dd4 = dd4$ensemblid, all = FALSE, all.categoricaldataa = all, all.dd4 = all,
                              sort = TRUE, suffixes = c(".x",".y"), no.dups = TRUE)    
      categoricaldataa[is.na(categoricaldataa)]="nothing"
      categoricaldataa$band<-factor(categoricaldataa$band)
    }
    
    if(isTRUE(any(input$variables=="transcriptcount"))){
      dd5<- data.frame( name=reensembl())
      colnames(dd5)[1]<-"ensemblid"
      library("biomaRt")
      #ensembl = useMart(biomart= "ensembl",dataset="hsapiens_gene_ensembl")
      mart <- useMart("ENSEMBL_MART_ENSEMBL")
      mart <- useMart("ENSEMBL_MART_ENSEMBL","hsapiens_gene_ensembl")
      dd5<-getBM(c("ensembl_gene_id",listAttributes(mart)[26,1]), c("ensembl_gene_id"), reensembl(), mart)
      colnames(dd5)[1]<-"ensemblid"
      categoricaldataa<-merge(categoricaldataa, dd5, 
                              by.categoricaldataa = categoricaldataa$ensemblid, by.dd5 = dd5$ensemblid, all = FALSE, all.categoricaldataa = all, all.dd5 = all,
                              sort = TRUE, suffixes = c(".x",".y"), no.dups = TRUE)
      categoricaldataa[is.na(categoricaldataa)]="nothing"
      categoricaldataa$transcript_count<-factor(categoricaldataa$transcript_count)
    }
    
    if(isTRUE(any(input$variables=="genebiotype"))){
      dd6<- data.frame( name=reensembl())
      colnames(dd6)[1]<-"ensemblid"
      library("biomaRt")
      #ensembl = useMart(biomart= "ensembl",dataset="hsapiens_gene_ensembl")
      mart <- useMart("ENSEMBL_MART_ENSEMBL")
      mart <- useMart("ENSEMBL_MART_ENSEMBL","hsapiens_gene_ensembl")
      dd6<-getBM(c("ensembl_gene_id",listAttributes(mart)[28,1]), c("ensembl_gene_id"), reensembl(), mart)
      colnames(dd6)[1]<-"ensemblid"
      categoricaldataa<-merge(categoricaldataa, dd6, 
                              by.categoricaldataa = categoricaldataa$ensemblid, by.dd6 = dd6$ensemblid, all = FALSE, all.categoricaldataa = all, all.dd6 = all,
                              sort = TRUE, suffixes = c(".x",".y"), no.dups = TRUE)
      categoricaldataa[is.na(categoricaldataa)]="nothing"
      categoricaldataa$gene_biotype<-factor(categoricaldataa$gene_biotype)
    }
    
    
    
    
    return(categoricaldataa)
    
  })
  
  remixed<-reactive({
    categoric<-refinal()
    numeric<-re2()
    colnames(categoric)[1]<-"ensemblid"
    colnames(numeric)[1]<-"ensemblid"
    kprotodata<-merge(numeric, categoric, 
                      by.numeric = numeric$ensemblid, by.categoric = categoric$ensemblid, all = FALSE, all.numeric = all, all.categoric = all,
                      sort = TRUE, suffixes = c(".x",".y"), no.dups = TRUE)
    
    return(kprotodata)
  })
  
  
  #==============Efarmogh algorithmwn
  
  
  
  #KMEANS
  remeans<-reactive({
    if(input$algorithmcenter=="optimal"){
      #silhouette to find the k centers
      library(cluster)
      for (i in 2:30){km10<-kmeans(re2()[,-1], centers = i, nstart=25)
      ss <- silhouette(km10$cluster, dist(re2()[,-1]))
      meansilh[k]<-mean(ss[, 3])
      
      if(abs(meansilh[i]-meansilh[i-1])<0.008) {return(km10)}
      }
    }
    if(input$algorithmcenter=="mynumber"){
      km2<-kmeans(re2()[,-1],centers = input$numalgorithm,nstart = 25)
      return(km2)
    }
  })
  
  
  #KMODEs
  remodes<-reactive({
    if(input$algorithmcenter=="optimal"){
      
      #silhouette to find the k centers
      library(klaR)
      wss<-vector()
      wss[1]=0
      for (j in 2:30){ kmodesresult11<-kmodes(refinal()[,-1], modes = j)
      wss[j] <- sum(kmodesresult11$withindiff)
      if(abs(wss[j]-wss[j-1])<=1) {return(kmodesresult11)
      }
      }
    }
    
    if(input$algorithmcenter=="mynumber"){
      library(klaR)
      kmodesresult12<-kmodes(refinal()[,-1], modes = input$numalgorithm)
      return(kmodesresult12)
    }
  })
  
  #KPROTO
  reproto<-reactive({
    if(input$algorithmcenter=="optimal"){
      #silhouette to find the k centers
      library(clustMixType)
      wss1<-vector()
      wss1[1]=0
      for (j in 2:30){ kprotoresult11<-kproto(remixed()[,-1], j , iter.max = 20)
      wss1[j] <- kprotoresult11$tot.withinss
      if(abs(wss1[j]-wss1[j-1])<=mean(wss[2:30])*0.7) {return(kprotoresult11)
      }
      }
    }
    
    if(input$algorithmcenter=="mynumber"){
      library(klaR)
      kprotoresult12<-kproto(remixed()[,-1], input$numalgorithm ,iter.max = 20)
      return(kprotoresult12)
    }
  })
  
  #Apotelesmata algorithmwn
  
  reusealgorithm<-reactive({
    
    if(input$algorithm=="kmeans"){
      return(remeans())
    }
    if(input$algorithm=="kmodes"){
      return(remodes())
    }
    if(input$algorithm=="kproto"){
      return(reproto())
    }
  })
  
  re<-reactive({
    
    modelmeans<-kmeans(re2()[,-1],3)
    clusterstable<-table(modelmeans$cluster)
    clusterss<-modelmeans$cluster
    return(clusterss)})
  
  
  output$clusters <- renderTable({
    
    if(input$disp == "head") {
      return(head(refinal()))
    }
    else {
      return(head(refinal()))
    }
    
  })
  output$refinal<-renderTable({
    return(head(remixed()))
  })
  output$re2<-renderTable({
    return(head(re2()))
  })
  
  
  # input$file1 will be NULL initially. After the user selects
  # and uploads a file, head of that data file by default,
  # or all rows if selected, will be shown.
  
  output$clust1<-renderTable({
    table(remeans()$cluster)
  })
  output$clust2<-renderTable({
    table(remodes()$cluster)
  })
  output$clust3<-renderTable({
    table(reproto()$cluster)
  })
  output$clust4<-renderTable({
    table(reusealgorithm()$cluster)
  })
  
  
  
  #======Plot diasporas sto xwro
  
  
  renumer<-reactive({
    if(input$algorithm=="kmeans"){
    countss=re2()
    countss$color<-reusealgorithm()$cluster
  }
    if(input$algorithm=="kmodes"){
    countss=re2()[refinal()$ensemblid,]
  countss$color<-reusealgorithm()$cluster}
    
    if(input$algorithm=="kproto"){
      countss=re2()[refinal()$ensemblid,]
      countss$color<-reusealgorithm()$cluster}
    
    return(countss)
  })
  output$clustofnumer<-renderPlot({ 
    plot(renumer()[,as.numeric(input$columnchoice)],renumer()[,as.numeric(input$columnchoice1)],
         col = renumer()$color,pch = 20,
         xlab=colnames(renumer())[as.numeric(input$columnchoice)],
         main=paste("visualizing data with axis",colnames(renumer())[as.numeric(input$columnchoice)],"and",colnames(renumer())[as.numeric(input$columnchoice)],sep=" "),
    ylab=colnames(renumer())[as.numeric(input$columnchoice)])

    })
  
  output$boxplot2<-renderPlot({
    boxplot(renumer()[,as.numeric(input$columnchoice)] ~ renumer()$color,
            main=paste("Statistical analysis of every cluster by reference to ",colnames(renumer())[as.numeric(input$columnchoice)],"(first choice)",sep=" "),
            xlab="Clusters",
            ylab= colnames(renumer())[as.numeric(input$columnchoice)]
            )
  })
  
  output$colourofclust<-renderPlot({
    plot(table(reusealgorithm()$cluster),col = c(1:length(table(reusealgorithm()$cluster))),
         xlab="Colors of the clusters",
         ylab="Size of the cluster"
         )
  
    })
  
  
  
  #=====Barplots gia kathgorika xarakthrhtika
  
  
  recateg<-reactive({
    if(input$algorithm=="kmeans"){
      countss1=re2()
      countss2=refinal()
      countss1$clusters<-reusealgorithm()$cluster
      mixeddata<-merge(countss1, countss2, 
                        by.countss1 = countss1$ensemblid, by.countss2 = countss2$ensemblid, all = FALSE, all.countss1 = all, all.countss2 = all,
                        sort = TRUE, suffixes = c(".x",".y"), no.dups = TRUE)
     categ<-mixeddata[c(as.character(input$barplotvar),"clusters")]
     return(categ)
     }
    if(input$algorithm=="kmodes"){
      countss3<-refinal()
      countss3$clusters<-reusealgorithm()$cluster
      categ<-countss3[c(as.character(input$barplotvar),"clusters")]
      return(categ)
    }
    
    if(input$algorithm=="kproto"){
      countss4<-refinal()
      countss4$clusters<-reusealgorithm()$cluster
      categ<-countss4[c(as.character(input$barplotvar),"clusters")]
      return(categ)
      }
    return(categ)
  })
  
  #Data me epilegmenh sthlh metrhsewn kai epilegmenh sthlh xarakthrhstikwn
  
  recateg1<-reactive({
    if(input$algorithm=="kmeans"){
      countss1=re2()
      countss2=refinal()
      countss1$clusters<-reusealgorithm()$cluster
      mixeddata<-merge(countss1, countss2, 
                       by.countss1 = countss1$ensemblid, by.countss2 = countss2$ensemblid, all = FALSE, all.countss1 = all, all.countss2 = all,
                       sort = TRUE, suffixes = c(".x",".y"), no.dups = TRUE)
      categ1<-mixeddata[c(names(countss1)[as.numeric(input$columnbox)],as.character(input$barplotvar),"clusters")]
    }
    if(input$algorithm=="kmodes"){
      countss3<-remixed()
      countss3$clusters<-reusealgorithm()$cluster
      categ1<-countss3[c(names(countss3)[as.numeric(input$columnbox)],as.character(input$barplotvar),"clusters")]
      return(categ1)
    }
    if(input$algorithm=="kproto"){
      countss4<-remixed()
      countss4$clusters<-reusealgorithm()$cluster
      categ1<-countss4[c(names(countss4)[as.numeric(input$columnbox)],as.character(input$barplotvar),"clusters")]
      return(categ1)
    }
    return(categ1)
  })
  
  output$recateg<- renderTable({
    recateg()
  })
  
  refrequent<-reactive({
    table(recateg1()[(recateg1()$clusters==as.numeric(input$numclust)),as.character(input$barplotvar)])[head(order(table(recateg1()[(recateg1()$clusters==as.numeric(input$numclust)),as.character(input$barplotvar)]),decreasing = TRUE),as.numeric(input$numcharact))]
  
    })
  refrequentnames<-reactive({
    namesss<-names(table(recateg1()[(recateg1()$clusters==as.numeric(input$numclust)),as.character(input$barplotvar)])[head(order(table(recateg1()[(recateg1()$clusters==as.numeric(input$numclust)),as.character(input$barplotvar)]),decreasing = TRUE),as.numeric(input$numcharact))])
    vvv<-c(1:(as.numeric(input$numcharact)))
    for(t in 1:(as.numeric(input$numcharact))){
      vvv[t]<-as.character(namesss[t])
    }
      return(vvv)
  })
  refrequentmatrix2<-reactive({
    if(input$numcharact == 2){
    matr<-recateg1()[(recateg1()[,as.character(input$barplotvar)]==as.character(refrequentnames()[1]))|(recateg1()[,as.character(input$barplotvar)]==as.character(refrequentnames()[2])),] 
    return(matr)}
    if(input$numcharact == 3){
      matr<-recateg1()[(recateg1()[,as.character(input$barplotvar)]==as.character(refrequentnames()[1]))|(recateg1()[,as.character(input$barplotvar)]==as.character(refrequentnames()[2]))|(recateg1()[,as.character(input$barplotvar)]==as.character(refrequentnames()[3])),] 
      return(matr)}
    if(input$numcharact == 4){
      matr<-recateg1()[(recateg1()[,as.character(input$barplotvar)]==as.character(refrequentnames()[1]))|(recateg1()[,as.character(input$barplotvar)]==as.character(refrequentnames()[2]))|(recateg1()[,as.character(input$barplotvar)]==as.character(refrequentnames()[3]))|(recateg1()[,as.character(input$barplotvar)]==as.character(refrequentnames()[4])),] 
      return(matr)}
    if(input$numcharact == 5){
      matr<-recateg1()[(recateg1()[,as.character(input$barplotvar)]==as.character(refrequentnames()[1]))|(recateg1()[,as.character(input$barplotvar)]==as.character(refrequentnames()[2]))|(recateg1()[,as.character(input$barplotvar)]==as.character(refrequentnames()[3]))|(recateg1()[,as.character(input$barplotvar)]==as.character(refrequentnames()[4]))|(recateg1()[,as.character(input$barplotvar)]==as.character(refrequentnames()[5])),] 
      return(matr)}
    if(input$numcharact == 6){
      matr<-recateg1()[(recateg1()[,as.character(input$barplotvar)]==as.character(refrequentnames()[1]))|(recateg1()[,as.character(input$barplotvar)]==as.character(refrequentnames()[2]))|(recateg1()[,as.character(input$barplotvar)]==as.character(refrequentnames()[3]))|(recateg1()[,as.character(input$barplotvar)]==as.character(refrequentnames()[4]))|(recateg1()[,as.character(input$barplotvar)]==as.character(refrequentnames()[5]))|(recateg1()[,as.character(input$barplotvar)]==as.character(refrequentnames()[6])),] 
      return(matr)}
    if(input$numcharact == 7){
      matr<-recateg1()[(recateg1()[,as.character(input$barplotvar)]==as.character(refrequentnames()[1]))|(recateg1()[,as.character(input$barplotvar)]==as.character(refrequentnames()[2]))|(recateg1()[,as.character(input$barplotvar)]==as.character(refrequentnames()[3]))|(recateg1()[,as.character(input$barplotvar)]==as.character(refrequentnames()[4]))|(recateg1()[,as.character(input$barplotvar)]==as.character(refrequentnames()[5]))|(recateg1()[,as.character(input$barplotvar)]==as.character(refrequentnames()[6]))|(recateg1()[,as.character(input$barplotvar)]==as.character(refrequentnames()[7])),] 
      return(matr)}
    if(input$numcharact == 8){
      matr<-recateg1()[(recateg1()[,as.character(input$barplotvar)]==as.character(refrequentnames()[1]))|(recateg1()[,as.character(input$barplotvar)]==as.character(refrequentnames()[2]))|(recateg1()[,as.character(input$barplotvar)]==as.character(refrequentnames()[3]))|(recateg1()[,as.character(input$barplotvar)]==as.character(refrequentnames()[4]))|(recateg1()[,as.character(input$barplotvar)]==as.character(refrequentnames()[5]))|(recateg1()[,as.character(input$barplotvar)]==as.character(refrequentnames()[6]))|(recateg1()[,as.character(input$barplotvar)]==as.character(refrequentnames()[7]))|(recateg1()[,as.character(input$barplotvar)]==as.character(refrequentnames()[8])),] 
      return(matr)}
    if(input$numcharact == 9){
      matr<-recateg1()[(recateg1()[,as.character(input$barplotvar)]==as.character(refrequentnames()[1]))|(recateg1()[,as.character(input$barplotvar)]==as.character(refrequentnames()[2]))|(recateg1()[,as.character(input$barplotvar)]==as.character(refrequentnames()[3]))|(recateg1()[,as.character(input$barplotvar)]==as.character(refrequentnames()[4]))|(recateg1()[,as.character(input$barplotvar)]==as.character(refrequentnames()[5]))|(recateg1()[,as.character(input$barplotvar)]==as.character(refrequentnames()[6]))|(recateg1()[,as.character(input$barplotvar)]==as.character(refrequentnames()[7]))|(recateg1()[,as.character(input$barplotvar)]==as.character(refrequentnames()[8]))|(recateg1()[,as.character(input$barplotvar)]==as.character(refrequentnames()[9])),] 
      return(matr)}
    if(input$numcharact == 10){
      matr<-recateg1()[(recateg1()[,as.character(input$barplotvar)]==as.character(refrequentnames()[1]))|(recateg1()[,as.character(input$barplotvar)]==as.character(refrequentnames()[2]))|(recateg1()[,as.character(input$barplotvar)]==as.character(refrequentnames()[3]))|(recateg1()[,as.character(input$barplotvar)]==as.character(refrequentnames()[4]))|(recateg1()[,as.character(input$barplotvar)]==as.character(refrequentnames()[5]))|(recateg1()[,as.character(input$barplotvar)]==as.character(refrequentnames()[6]))|(recateg1()[,as.character(input$barplotvar)]==as.character(refrequentnames()[7]))|(recateg1()[,as.character(input$barplotvar)]==as.character(refrequentnames()[8]))|(recateg1()[,as.character(input$barplotvar)]==as.character(refrequentnames()[9]))|(recateg1()[,as.character(input$barplotvar)]==as.character(refrequentnames()[10])),] 
      return(matr)}
    return(matr)
    
  })
  #sthles tou table pou apoteloun ta pio 
  #head(order(table(recateg1()[(recateg1()$clusters==1),as.character(input$barplotvar)]),decreasing = TRUE),as.numeric(input$numcharact))
  #table(recateg1()[(recateg1()$clusters==1),as.character(input$barplotvar)])[head(order(table(recateg1()[(recateg1()$clusters==1),as.character(input$barplotvar)]),decreasing = TRUE),as.numeric(input$numcharact))
  #]
  #names(table(recateg1()[(recateg1()$clusters==1),as.character(input$barplotvar)])[head(order(table(recateg1()[(recateg1()$clusters==1),as.character(input$barplotvar)]),decreasing = TRUE),as.numeric(input$numcharact))])
  #boxplot(recateg1[(recateg1()$clusters==1)&(recateg1()[,as.character(input$barplotvar)==names(table(recateg1()[(recateg1()$clusters==1),as.character(input$barplotvar)])[head(order(table(recateg1()[(recateg1()$clusters==1),as.character(input$barplotvar)]),decreasing = TRUE),as.numeric(input$numcharact))])]),colnames(recateg1())[input$columnchoice]]  ~ recateg1[(recateg1()$clusters==1)&(recateg1()[,as.character(input$barplotvar)==names(table(recateg1()[(recateg1()$clusters==1),as.character(input$barplotvar)])[head(order(table(recateg1()[(recateg1()$clusters==1),as.character(input$barplotvar)]),decreasing = TRUE),as.numeric(input$numcharact))])]), as.character(input$barplotvar)  ])
  
  barplottable<-reactive({
   table<- rbind( table(recateg()[(recateg()$clusters==as.numeric(input$numclust)),as.character(input$barplotvar)])[head(order(table(recateg()[(recateg()$clusters==as.numeric(input$numclust)),as.character(input$barplotvar)]),decreasing = TRUE),as.numeric(input$numcharact))],
                  table(recateg()[(recateg()$clusters!=as.numeric(input$numclust)),as.character(input$barplotvar)])[head(order(table(recateg()[(recateg()$clusters==as.numeric(input$numclust)),as.character(input$barplotvar)]),decreasing = TRUE),as.numeric(input$numcharact))])
                 row.names(table)<-c("The selected cluster","The other clusters") 
                  
   return(table)
    })
  
  output$barplotclust1<-renderPlot({
    barplot(barplottable(),
      main = paste("The",as.character(input$numcharact),"more frequent values of", as.character(input$barplotvar),"in cluster",as.character(input$numclust),"in comparison with these who are in other clusters", sep = " " ),
      
            #table(recateg()[(recateg()$clusters==as.numeric(input$numclust)),as.character(input$barplotvar)])[head(order(table(recateg()[(recateg()$clusters==as.numeric(input$numclust)),as.character(input$barplotvar)]),decreasing = TRUE),as.numeric(input$numcharact))],
      
            xlab=as.character(input$barplotvar),
    col=c("red","green")
    )
    legend("topright",
           c("our cluster","other clusters"),
           fill = c("red","green")
      
    )
    })
  #output$boxplot1<-renderPlot({
   # boxplot(recateg1()[(recateg1()$clusters==1)&((recateg1()[,as.character(input$barplotvar)]==as.character(refrequentnames()[1]))|(recateg1()[,as.character(input$barplotvar)]==as.character(refrequentnames()[2]))),names(recateg1())[as.numeric(input$columnchoice)]]  ~ recateg1()[(recateg1()$clusters==1)&((recateg1()[,as.character(input$barplotvar)]==as.character(refrequentnames()[1]))|(recateg1()[,as.character(input$barplotvar)]==as.character(refrequentnames()[2]))), as.character(input$barplotvar)  ])
    #
  #})
  refrequentmatrix22<-reactive({
    countsssssss<-refrequentmatrix2()
    return(countsssssss)
  })
  output$boxplot1<-renderPlot({
    boxplot(      refrequentmatrix22()[refrequentmatrix22()$clusters==as.numeric(input$numclust),1] ~ refrequentmatrix22()[refrequentmatrix22()$clusters==as.numeric(input$numclust),as.character(input$barplotvar)],

      main= paste("The arithmetic analysis of the values of",as.character(input$barplotvar),"we saw above by reference to",as.character(colnames(renumer())[as.numeric(input$columnbox)]),sep=" "),
     xlab=as.character(input$barplotvar),
    ylab= as.character(colnames(renumer())[as.numeric(input$columnbox)])

         )
  })
  output$namesofvar<-renderText({ 
    paste("The row of characteristics of the boxplot is",toupper(refrequentnames()[order(as.character(refrequentnames()))]),sep=" ")
  })
  output$check<-renderTable({
    barplottable()
  })
  output$refrequent<-renderTable({
    recateg1()[(recateg1()[,as.character(input$barplotvar)]==as.character(refrequentnames()[1]))|(recateg1()[,as.character(input$barplotvar)]==as.character(refrequentnames()[2])),] 
    #refrequentmatrix2()
    })
   #boxplot(final_table3$NA06985[final_table3$terms==c("metal ion binding","extracellular exosome")] ~ final_table3$terms[final_table3$terms==c("metal ion binding","extracellular exosome")] )
  
  #output$barplotclust1<-renderPlot({
   # barplot( summary(recateg()[(recateg()$clusters==1),as.character(input$barplotvar)],input$numcharact))
  #})
  output$barplotclust2<-renderPlot({
    barplot(table(recateg()[(recateg()$clusters==2),as.character(input$barplotvar)])[head(order(table(recateg()[(recateg()$clusters==2),as.character(input$barplotvar)]),decreasing = TRUE),as.numeric(input$numcharact))])
  })
  #output$barplotclust2<-renderPlot({
   # barplot( summary(recateg()[(recateg()$clusters==2),as.character(input$barplotvar)],input$numcharact))
  #})
  output$barplotclust3<-renderPlot({
    barplot(table(recateg()[(recateg()$clusters==3),as.character(input$barplotvar)])[head(order(table(recateg()[(recateg()$clusters==3),as.character(input$barplotvar)]),decreasing = TRUE),as.numeric(input$numcharact))])
  })
  #output$barplotclust3<-renderPlot({
   # barplot( summary(recateg()[(recateg()$clusters==3),as.character(input$barplotvar)],input$numcharact))
  #})
  output$barplotclust4<-renderPlot({
    barplot(table(recateg()[(recateg()$clusters==4),as.character(input$barplotvar)])[head(order(table(recateg()[(recateg()$clusters==4),as.character(input$barplotvar)]),decreasing = TRUE),as.numeric(input$numcharact))])
  })
  #output$barplotclust4<-renderPlot({
   # barplot( summary(recateg()[(recateg()$clusters==4),as.character(input$barplotvar)],input$numcharact))
  #})
  output$barplotclust5<-renderPlot({
    barplot( summary(recateg()[(recateg()$clusters==5),as.character(input$barplotvar)],input$numcharact))
  })
  output$barplotclust6<-renderPlot({
    barplot( summary(recateg()[(recateg()$clusters==6),as.character(input$barplotvar)],input$numcharact))
  })
  output$barplotclust7<-renderPlot({
    barplot( summary(recateg()[(recateg()$clusters==7),as.character(input$barplotvar)],input$numcharact))
  })
  output$barplotclust8<-renderPlot({
    barplot( summary(recateg()[(recateg()$clusters==8),as.character(input$barplotvar)],input$numcharact))
  })
  output$barplotclust9<-renderPlot({
    barplot( summary(recateg()[(recateg()$clusters==9),as.character(input$barplotvar)],input$numcharact))
  })
  output$barplotclust10<-renderPlot({
    barplot( summary(recateg()[(recateg()$clusters==10),as.character(input$barplotvar)],input$numcharact))
  })
  output$barplotclust11<-renderPlot({
    barplot( summary(recateg()[(recateg()$clusters==11),as.character(input$barplotvar)],input$numcharact))
  })
  output$barplotclust12<-renderPlot({
    barplot( summary(recateg()[(recateg()$clusters==12),as.character(input$barplotvar)],input$numcharact))
  })
  output$barplotclust13<-renderPlot({
    barplot( summary(recateg()[(recateg()$clusters==13),as.character(input$barplotvar)],input$numcharact))
  })
  output$barplotclust14<-renderPlot({
    barplot( summary(recateg()[(recateg()$clusters==14),as.character(input$barplotvar)],input$numcharact))
  })
  output$barplotclust15<-renderPlot({
    barplot( summary(recateg()[(recateg()$clusters==15),as.character(input$barplotvar)],input$numcharact))
  })
  
 # output$ensemblrow<-renderTable({(df[df$Varietal==input$ensemblid,])})
  
  output$checkbox <- renderText({
    
    if(isTRUE(any(input$variables=="terms"))){
      return(input$variables)
    }
  })
  
  
  
  #=====UPSET
  
  
  #===================Edw douleia gia to upseeeeeet
  recateg3<-reactive({
    if(input$algorithm=="kmeans"){
      countss1=re2()
      countss2=refinal()
      countss1$clusters<-reusealgorithm()$cluster
      mixeddata<-merge(countss1, countss2, 
                       by.countss1 = countss1$ensemblid, by.countss2 = countss2$ensemblid, all = FALSE, all.countss1 = all, all.countss2 = all,
                       sort = TRUE, suffixes = c(".x",".y"), no.dups = TRUE)
      categ1<-mixeddata[c(as.character(input$barplotvar1234),as.character(input$barplotvar123),"clusters")]
    }
    if(input$algorithm=="kmodes"){
      countss3<-remixed()
      countss3$clusters<-reusealgorithm()$cluster
      categ1<-countss3[c(as.character(input$barplotvar1234),as.character(input$barplotvar123),"clusters")]
      return(categ1)
    }
    if(input$algorithm=="kproto"){
      countss4<-remixed()
      countss4$clusters<-reusealgorithm()$cluster
      categ1<-countss4[c(as.character(input$barplotvar1234),as.character(input$barplotvar123),"clusters")]
      return(categ1)
    }
    return(categ1)
  })
  
  refrequentnames1<-reactive({
    namesss<-names(table(recateg3()[(recateg3()$clusters==as.numeric(input$numclust)),as.character(input$barplotvar123)])[head(order(table(recateg3()[(recateg3()$clusters==as.numeric(input$numclust)),as.character(input$barplotvar123)]),decreasing = TRUE),3)])
    vvv<-c(1:3)
    for(t in 1:3){
      vvv[t]<-as.character(namesss[t])
    }
    return(vvv)
  })
  refrequentnames2<-reactive({
    namesss<-names(table(recateg3()[(recateg3()$clusters==as.numeric(input$numclust)),as.character(input$barplotvar1234)])[head(order(table(recateg3()[(recateg3()$clusters==as.numeric(input$numclust)),as.character(input$barplotvar1234)]),decreasing = TRUE),3)])
    vvv<-c(1:3)
    for(t in 1:3){
      vvv[t]<-as.character(namesss[t])
    }
    return(vvv)
  })
  
  retableforupset<-reactive({
    
    countssss<-refinal()
    countssss$col1<-c(0)
    #how many terms in cluster make them 1
    countssss$col1[which(countssss[,as.character(input$barplotvar123)]==as.character(refrequentnames1()[1]))]=1
    countssss$col2<-c(0)
    #how many terms in cluster make them 1
    countssss$col2[which(countssss[,as.character(input$barplotvar123)]==as.character(refrequentnames1()[2]))]=1
    countssss$col3<-c(0)
    #how many terms in cluster make them 1
    countssss$col3[which(countssss[,as.character(input$barplotvar123)]==as.character(refrequentnames1()[3]))]=1
    countssss$col4<-c(0)
    #how many terms in cluster make them 1
    countssss$col4[which(countssss[,as.character(input$barplotvar1234)]==as.character(refrequentnames2()[1]))]=1
    countssss$col5<-c(0)
    #how many terms in cluster make them 1
    countssss$col5[which(countssss[,as.character(input$barplotvar1234)]==as.character(refrequentnames2()[2]))]=1
    countssss$col6<-c(0)
    #how many terms in cluster make them 1
    countssss$col6[which(countssss[,as.character(input$barplotvar1234)]==as.character(refrequentnames2()[3]))]=1
    
    return(countssss)
    
  })
  output$upset<-renderPlot({
    upset(retableforupset(), sets = c("col1","col2","col3","col4","col5","col6"),order.by="degree",matrix.color="blue",point.size=5,sets.bar.color=c(1:6))
    
  })
    
    output$colnames1<-renderText({
    paste("col1=",as.character(refrequentnames1()[1]),sep = " ")
      })
    output$colnames2<-renderText({
      paste("col2=",as.character(refrequentnames1()[2]),sep = " ")
    })
    output$colnames3<-renderText({
      paste("col3=",as.character(refrequentnames1()[3]),sep = " ")
    })
    output$colnames4<-renderText({
      paste("col4=",as.character(refrequentnames2()[1]),sep = " ")
    })
    output$colnames5<-renderText({
      paste("col5=",as.character(refrequentnames2()[2]),sep = " ")
    })
    output$colnames6<-renderText({
      paste("col6=",as.character(refrequentnames2()[3]),sep = " ")
    })
  
  
  
  
  
    #=========== Entropy
    
    reentropyforeveryclusting<-reactive({
      entropyall<-c(1:3)
      #===kmeans
      countsss1<-re2()
      countsss2<-remixed()
      countsss1$clusters<-remeans()$cluster
      mixeddata<-merge(countsss1, countsss2, 
                       by.countsss1 = countsss1$ensemblid, by.countsss2 = countsss2$ensemblid, all = FALSE, all.countsss1 = all, all.countsss2 = all,
                       sort = TRUE, suffixes = c(".x",".y"), no.dups = TRUE)
      #EDW MPOROUN NA EPILEGONTAI OI STHLES GIA TIS OPOIES THELOUME NA GINEI I AXIOLOGHGH
     # mixeddata1<-mixeddata[,-c(1:(as.numeric(length(countsss1[1,]))-1))] 
    #  for(i in 1:length(rownames(t(mixeddata1[,-1])))){
       # mixx[i]=rownames(t(mixeddata1[,-1]))[i]
      #}
      entropytermsall=0
      #thecolumns we need for loop
      #entropyofeverycluster<-c(1:30)
      #entropyofeverycluster[1:30]=0
      clusterss<-as.data.frame(table(remeans()$cluster))
      for(y in 1:length(table(remeans()$cluster))){
        
        termsofcluster1<-table(mixeddata[mixeddata$clusters==y,as.character(input$barplotvar12)])
        
        termsofcluster1<-termsofcluster1[termsofcluster1>=1]
        propabiliteisstable<-termsofcluster1/clusterss$Freq[y]
        propabiliteisstable<-as.data.frame(propabiliteisstable)
        propabiliteisstable<-propabiliteisstable[,-1]
        propabiliteisstable<-propabiliteisstable[!is.na(propabiliteisstable)]
        entropytermsofcluste1=0
        for(i in seq_along(propabiliteisstable)){entropytermsofcluste1<-entropytermsofcluste1+(propabiliteisstable[i]*log2(propabiliteisstable[i]))}
        
        entropytermsall= entropytermsall + entropytermsofcluste1*dim(as.data.frame(mixeddata[mixeddata$clusters==y,as.character(input$barplotvar12)]))[1]/length(mixeddata[,as.character(input$barplotvar12)])
      }
      entropyall[1]<-abs(entropytermsall)
     
      
      
      #======Kmodes
      entropytermsall=0
      countsss3<-refinal()
      countsss3$clusters<-remodes()$cluster
      mixeddata<-countsss3
      clusterss<-as.data.frame(table(remodes()$cluster))
      
      for(y in 1:length(table(remodes()$cluster))){
        termsofcluster1<-table(mixeddata[mixeddata$clusters==y,as.character(input$barplotvar12)])
        
        termsofcluster1<-termsofcluster1[termsofcluster1>=1]
        propabiliteisstable<-termsofcluster1/clusterss$Freq[y]
        propabiliteisstable<-as.data.frame(propabiliteisstable)
        propabiliteisstable<-propabiliteisstable[,-1]
        propabiliteisstable<-propabiliteisstable[!is.na(propabiliteisstable)]
        entropytermsofcluste1=0
        for(i in seq_along(propabiliteisstable)){entropytermsofcluste1<-entropytermsofcluste1+(propabiliteisstable[i]*log2(propabiliteisstable[i]))}
        
        entropytermsall= entropytermsall + entropytermsofcluste1*dim(as.data.frame(mixeddata[mixeddata$clusters==y,as.character(input$barplotvar12)]))[1]/length(mixeddata[,as.character(input$barplotvar12)])
        
        
        }
      entropyall[2]<-abs(entropytermsall)
      
      #==========kproto
      
      entropytermsall=0
      countsss3<-refinal()
      countsss3$clusters<-reproto()$cluster
      mixeddata<-countsss3
      clusterss<-as.data.frame(table(reproto()$cluster))
      
      for(y in 1:length(table(reproto()$cluster))){
        termsofcluster1<-table(mixeddata[mixeddata$clusters==y,as.character(input$barplotvar12)])
        
        termsofcluster1<-termsofcluster1[termsofcluster1>=1]
        propabiliteisstable<-termsofcluster1/clusterss$Freq[y]
        propabiliteisstable<-as.data.frame(propabiliteisstable)
        propabiliteisstable<-propabiliteisstable[,-1]
        propabiliteisstable<-propabiliteisstable[!is.na(propabiliteisstable)]
        entropytermsofcluste1=0
        for(i in seq_along(propabiliteisstable)){entropytermsofcluste1<-entropytermsofcluste1+(propabiliteisstable[i]*log2(propabiliteisstable[i]))}
        
        entropytermsall= entropytermsall + entropytermsofcluste1*dim(as.data.frame(mixeddata[mixeddata$clusters==y,as.character(input$barplotvar12)]))[1]/length(mixeddata[,as.character(input$barplotvar12)])
        
        
      }
      entropyall[3]<-abs(entropytermsall)

      
      
      
      
      
      
      
      return(entropyall)
      
    })
    output$entropybarplot<-renderPlot({
      barplot(reentropyforeveryclusting(),main = "The entropy of every clustering",names.arg = c("kmeans","kmodes","kproto"))
    })
    output$entropytext<-renderText({
      "Remember a good clustering has a small entropy
      Where is 1<-kmeans,2<-kmodes,3<-kproto  "
    })
    
    output$refrequentnames<-renderText({
      paste("The specific values are:",as.character(reentropyforeveryclusting()),sep=" ")
    })
    
    
    
    #====cohesion
    
    regower1<-reactive({
      gowerofeveryclust<-c(1:length(table(remeans()$cluster)))
      countsss1<-re2()
      countsss2<-remixed()
      countsss1$clusters<-remeans()$cluster
      mixeddata<-merge(countsss1, countsss2, 
                       by.countsss1 = countsss1$ensemblid, by.countsss2 = countsss2$ensemblid, all = FALSE, all.countsss1 = all, all.countsss2 = all,
                       sort = TRUE, suffixes = c(".x",".y"), no.dups = TRUE)
      for(i in 1:length(table(remeans()$cluster))){
        gowerdist<-daisy(mixeddata[mixeddata$clusters==i,-1], metric = "gower")
        gower_mat <- as.matrix(gowerdist)
        gowerofeveryclust[i]<-sum(gower_mat)/(length(mixeddata$ensemblid[mixeddata$clusters==i])*length(mixeddata$ensemblid[mixeddata$clusters==i]))
      }
      return(gowerofeveryclust)
    })
    
    regower2<-reactive({
      gowerofeveryclust<-c(1:length(table(remodes()$cluster)))
      countss1<-remixed()
      countss1$clusters<-remodes()$cluster
      for(i in 1:length(table(remodes()$cluster))){
        gowerdist<-daisy(countss1[countss1$clusters==i,-1], metric = "gower")
        gower_mat <- as.matrix(gowerdist)
        gowerofeveryclust[i]<-sum(gower_mat)/(length(countss1$ensemblid[countss1$clusters==i])*length(countss1$ensemblid[countss1$clusters==i]))
      }
      return(gowerofeveryclust)
    })
    
    regower3<-reactive({
      gowerofeveryclust<-c(1:length(table(reproto()$cluster)))
      countss1<-remixed()
      countss1$clusters<-reproto()$cluster
      for(i in 1:length(table(reproto()$cluster))){
        gowerdist<-daisy(countss1[countss1$clusters==i,-1], metric = "gower")
        gower_mat <- as.matrix(gowerdist)
        gowerofeveryclust[i]<-sum(gower_mat)/(length(countss1$ensemblid[countss1$clusters==i])*length(countss1$ensemblid[countss1$clusters==i]))
      }
      return(gowerofeveryclust)
    })
    
    output$plotgower<-renderPlot({
      plot(regower1(),main="The internal dissimilarity(cohesion) between the points of every cluster of the three clusterings",type="b",ylim=c(0,1),xlim=c(1,20))
      lines(regower2() , type="b" , col="red")
      lines(regower3(),type="b",col="blue")
      legend("topright",
             c("Kmeans","Kmodes","Kproto"),
             fill = c("black","red","blue"))
      
    })
    output$colorss<-renderText({
      "kmeans<-black   kmodes<-red   kproto<-blue"
    })
    
    
    
    
    
    #==========Sankey diagram
    
    
    
    #NAMES FOR SANKEY
    resankeynames<-reactive({
      
      namesofnodes<-c(1:15)
      k=0
      #arithmoi twn omadwn me tis perissoteres metrhseis
      mostfreq<-head(order(table(reusealgorithm()$cluster),decreasing = TRUE),5)
      #perissoteres metrhseis
      table(reusealgorithm()$cluster)[head(order(table(reusealgorithm()$cluster),decreasing = TRUE),5)]
      #onomata twn nodes
      for(k in 1:5){
        namesofnodes[k]=paste(as.character(input$algorithm),head(order(table(reusealgorithm()$cluster),decreasing = TRUE),5)[k],sep="")
      }
      
      if(input$algorithm !="kmeans"){
        head(order(table(remeans()$cluster),decreasing = TRUE),5)
        table(remeans()$cluster)[head(order(table(remeans()$cluster),decreasing = TRUE),5)]
        for(k in 1:5){
          namesofnodes[k+5]=paste("kmeans",head(order(table(reusealgorithm()$cluster),decreasing = TRUE),5)[k],sep="")
        }
      }
      
      if(input$algorithm !="kmodes"){
        head(order(table(remodes()$cluster),decreasing = TRUE),5)
        table(remodes()$cluster)[head(order(table(remodes()$cluster),decreasing = TRUE),5)]
        if(input$algorithm =="kmeans"){
        for(k in 1:5){
          namesofnodes[k+5]=paste("kmodes",head(order(table(remodes()$cluster),decreasing = TRUE),5)[k],sep="")
        }
        }
        if(input$algorithm =="kproto"){
          for(k in 1:5){
            namesofnodes[k+10]=paste("kmodes",head(order(table(remodes()$cluster),decreasing = TRUE),5)[k],sep="")
          }
        }
      }
      if(input$algorithm !="kproto"){
        head(order(table(reproto()$cluster),decreasing = TRUE),5)
        table(reproto()$cluster)[head(order(table(reproto()$cluster),decreasing = TRUE),5)]
        
          for(k in 1:5){
            namesofnodes[k+10]=paste("kproto",head(order(table(reproto()$cluster),decreasing = TRUE),5)[k],sep="")
          }
         }
      names<-namesofnodes
      namesofnodes11<-data.frame(names,c(1:15))
       return(namesofnodes11)
      })
    
    #source target value sankey diagram
    resankeysou<-reactive({
      i=0
      j=0
      n=5
      source=c(1:(25))
      target=c(1:(25))
      for(i in 1:10)
      {u=i-1
      if(u<5)
      {source[(1+5*u):(5*i)]=u
      for(j in 1:5)
      {target[(1+5*u):(5*i)]= c(5:9)
      }
      }
      if(u>=5)
      {source[(1+5*u):(5*i)]=u
      for(j in 1:n)
      {target[(1+5*u):(5*i)]= c(10:14)
      }
      }
      }
      r=0
      l=0
      m=0
      value=c(1:50)
      #VALUES
      countss1=re2()
      countss1$clusters<-remeans()$cluster
      countss2=refinal()
      mixeddata<-merge(countss1, countss2, 
                       by.countss1 = countss1$ensemblid, by.countss2 = countss2$ensemblid, all = FALSE, all.countss1 = all, all.countss2 = all,
                       sort = TRUE, suffixes = c(".x",".y"), no.dups = TRUE)
      categ<-mixeddata[,c("ensemblid","clusters")]
      countss2$clusters<-remodes()$cluster
      countss3=remixed()
      countss3$clusters<-reproto()$cluster
      
      
        if(input$algorithm =="kmodes"){
          #fernoume ta apotelesmata means stis idies diastaseis
          
         #emploutismos values kmodes-kmeans
          r=1
          for(l in 1:5)
          {
            column1<-as.data.frame(countss2$ensemblid[countss2$clusters==(head(order(table(remodes()$cluster),decreasing = TRUE),5)[r])])
            column2<-as.data.frame(categ$ensemblid[categ$clusters==(head(order(table(remeans()$cluster),decreasing = TRUE),5)[l])])
            colnames(column1)[1]="namess"
            colnames(column2)[1]="namess"
            value[l]<-table(duplicated(rbind(column1,column2)))["TRUE"]
            }
          r=2
          for(l in 1:5)
          {
            column1<-as.data.frame(countss2$ensemblid[countss2$clusters==(head(order(table(remodes()$cluster),decreasing = TRUE),5)[r])])
            column2<-as.data.frame(categ$ensemblid[categ$clusters==(head(order(table(remeans()$cluster),decreasing = TRUE),5)[l])])
            colnames(column1)[1]="namess"
            colnames(column2)[1]="namess"
            value[l+5]<-table(duplicated(rbind(column1,column2)))["TRUE"]
          }
          r=3
          for(l in 1:5)
          {
            column1<-as.data.frame(countss2$ensemblid[countss2$clusters==(head(order(table(remodes()$cluster),decreasing = TRUE),5)[r])])
            column2<-as.data.frame(categ$ensemblid[categ$clusters==(head(order(table(remeans()$cluster),decreasing = TRUE),5)[l])])
            colnames(column1)[1]="namess"
            colnames(column2)[1]="namess"
            value[l+10]<-table(duplicated(rbind(column1,column2)))["TRUE"]
          }
          r=4
          for(l in 1:5)
          {
            column1<-as.data.frame(countss2$ensemblid[countss2$clusters==(head(order(table(remodes()$cluster),decreasing = TRUE),5)[r])])
            column2<-as.data.frame(categ$ensemblid[categ$clusters==(head(order(table(remeans()$cluster),decreasing = TRUE),5)[l])])
            colnames(column1)[1]="namess"
            colnames(column2)[1]="namess"
            value[l+15]<-table(duplicated(rbind(column1,column2)))["TRUE"]  

            }
          r=5
          for(l in 1:5)
          {
            column1<-as.data.frame(countss2$ensemblid[countss2$clusters==(head(order(table(remodes()$cluster),decreasing = TRUE),5)[r])])
            column2<-as.data.frame(categ$ensemblid[categ$clusters==(head(order(table(remeans()$cluster),decreasing = TRUE),5)[l])])
            colnames(column1)[1]="namess"
            colnames(column2)[1]="namess"
            value[l+20]<-table(duplicated(rbind(column1,column2)))["TRUE"]
          }
          
          #emploutismos value kmeans-proto
          
          r=1
          for(l in 1:5)
          {
            column1<-as.data.frame(categ$ensemblid[categ$clusters==(head(order(table(remeans()$cluster),decreasing = TRUE),5)[r])])
            column2<-as.data.frame(countss3$ensemblid[countss3$clusters==(head(order(table(reproto()$cluster),decreasing = TRUE),5)[l])])
            colnames(column1)[1]="namess"
            colnames(column2)[1]="namess"
            value[l+25]<-table(duplicated(rbind(column1,column2)))["TRUE"]
          }
          r=2
          for(l in 1:5)
          {
            column1<-as.data.frame(countss2$ensemblid[countss2$clusters==(head(order(table(remodes()$cluster),decreasing = TRUE),5)[r])])
            column2<-as.data.frame(categ$ensemblid[categ$clusters==(head(order(table(remeans()$cluster),decreasing = TRUE),5)[l])])
            colnames(column1)[1]="namess"
            colnames(column2)[1]="namess"
            value[l+30]<-table(duplicated(rbind(column1,column2)))["TRUE"]
          }
          r=3
          for(l in 1:5)
          {
            column1<-as.data.frame(countss2$ensemblid[countss2$clusters==(head(order(table(remodes()$cluster),decreasing = TRUE),5)[r])])
            column2<-as.data.frame(categ$ensemblid[categ$clusters==(head(order(table(remeans()$cluster),decreasing = TRUE),5)[l])])
            colnames(column1)[1]="namess"
            colnames(column2)[1]="namess"
            value[l+35]<-table(duplicated(rbind(column1,column2)))["TRUE"]
          }
          r=4
          for(l in 1:5)
          {
            column1<-as.data.frame(countss2$ensemblid[countss2$clusters==(head(order(table(remodes()$cluster),decreasing = TRUE),5)[r])])
            column2<-as.data.frame(categ$ensemblid[categ$clusters==(head(order(table(remeans()$cluster),decreasing = TRUE),5)[l])])
            colnames(column1)[1]="namess"
            colnames(column2)[1]="namess"
            value[l+40]<-table(duplicated(rbind(column1,column2)))["TRUE"]
          }
          r=5
          for(l in 1:5)
          {
            column1<-as.data.frame(countss2$ensemblid[countss2$clusters==(head(order(table(remodes()$cluster),decreasing = TRUE),5)[r])])
            column2<-as.data.frame(categ$ensemblid[categ$clusters==(head(order(table(remeans()$cluster),decreasing = TRUE),5)[l])])
            colnames(column1)[1]="namess"
            colnames(column2)[1]="namess"
            value[l+45]<-table(duplicated(rbind(column1,column2)))["TRUE"]
          }
          
        }
          
        if(input$algorithm =="kproto"){
          #fernoume ta apotelesmata means stis idies diastaseis
          
          #emploutismos values
          r=1
          for(l in 1:5)
          {
            column1<-as.data.frame(countss3$ensemblid[countss3$clusters==(head(order(table(reproto()$cluster),decreasing = TRUE),5)[r])])
            column2<-as.data.frame(categ$ensemblid[categ$clusters==(head(order(table(remeans()$cluster),decreasing = TRUE),5)[l])])
            colnames(column1)[1]="namess"
            colnames(column2)[1]="namess"
            value[l]<-table(duplicated(rbind(column1,column2)))["TRUE"]
          }
          r=2
          for(l in 1:5)
          {
            column1<-as.data.frame(countss3$ensemblid[countss3$clusters==(head(order(table(reproto()$cluster),decreasing = TRUE),5)[r])])
            column2<-as.data.frame(categ$ensemblid[categ$clusters==(head(order(table(remeans()$cluster),decreasing = TRUE),5)[l])])
            colnames(column1)[1]="namess"
            colnames(column2)[1]="namess"
            value[l+5]<-table(duplicated(rbind(column1,column2)))["TRUE"]
          }
          r=3
          for(l in 1:5)
          {
            column1<-as.data.frame(countss3$ensemblid[countss3$clusters==(head(order(table(reproto()$cluster),decreasing = TRUE),5)[r])])
            column2<-as.data.frame(categ$ensemblid[categ$clusters==(head(order(table(remeans()$cluster),decreasing = TRUE),5)[l])])
            colnames(column1)[1]="namess"
            colnames(column2)[1]="namess"
            value[l+10]<-table(duplicated(rbind(column1,column2)))["TRUE"]
          }
          r=4
          for(l in 1:5)
          {
            column1<-as.data.frame(countss3$ensemblid[countss3$clusters==(head(order(table(reproto()$cluster),decreasing = TRUE),5)[r])])
            column2<-as.data.frame(categ$ensemblid[categ$clusters==(head(order(table(remeans()$cluster),decreasing = TRUE),5)[l])])
            colnames(column1)[1]="namess"
            colnames(column2)[1]="namess"
            value[l+15]<-table(duplicated(rbind(column1,column2)))["TRUE"]

          }
          r=5
          for(l in 1:5)
          {
            column1<-as.data.frame(countss3$ensemblid[countss3$clusters==(head(order(table(reproto()$cluster),decreasing = TRUE),5)[r])])
            column2<-as.data.frame(categ$ensemblid[categ$clusters==(head(order(table(remeans()$cluster),decreasing = TRUE),5)[l])])
            colnames(column1)[1]="namess"
            colnames(column2)[1]="namess"
            value[l+20]<-table(duplicated(rbind(column1,column2)))["TRUE"]
          }
          r=1
          for(l in 1:5)
          {
            column1<-as.data.frame(categ$ensemblid[categ$clusters==(head(order(table(remeans()$cluster),decreasing = TRUE),5)[r])])
            column2<-as.data.frame(countss2$ensemblid[countss2$clusters==(head(order(table(remodes()$cluster),decreasing = TRUE),5)[l])])
            colnames(column1)[1]="namess"
            colnames(column2)[1]="namess"
            value[25+l]<-table(duplicated(rbind(column1,column2)))["TRUE"]
          }
          r=2
          for(l in 1:5)
          {
            column1<-as.data.frame(categ$ensemblid[categ$clusters==(head(order(table(remeans()$cluster),decreasing = TRUE),5)[r])])
            column2<-as.data.frame(countss2$ensemblid[countss2$clusters==(head(order(table(remodes()$cluster),decreasing = TRUE),5)[l])])
            colnames(column1)[1]="namess"
            colnames(column2)[1]="namess"
            value[30+l]<-table(duplicated(rbind(column1,column2)))["TRUE"]
          }
          r=3
          for(l in 1:5)
          {
            column1<-as.data.frame(categ$ensemblid[categ$clusters==(head(order(table(remeans()$cluster),decreasing = TRUE),5)[r])])
            column2<-as.data.frame(countss2$ensemblid[countss2$clusters==(head(order(table(remodes()$cluster),decreasing = TRUE),5)[l])])
            colnames(column1)[1]="namess"
            colnames(column2)[1]="namess"
            value[35+l]<-table(duplicated(rbind(column1,column2)))["TRUE"]
          }
          r=4
          for(l in 1:5)
          {
            column1<-as.data.frame(categ$ensemblid[categ$clusters==(head(order(table(remeans()$cluster),decreasing = TRUE),5)[r])])
            column2<-as.data.frame(countss2$ensemblid[countss2$clusters==(head(order(table(remodes()$cluster),decreasing = TRUE),5)[l])])
            colnames(column1)[1]="namess"
            colnames(column2)[1]="namess"
            value[40+l]<-table(duplicated(rbind(column1,column2)))["TRUE"]

          }
          r=5
          for(l in 1:5)
          {
            column1<-as.data.frame(categ$ensemblid[categ$clusters==(head(order(table(remeans()$cluster),decreasing = TRUE),5)[r])])
            column2<-as.data.frame(countss2$ensemblid[countss2$clusters==(head(order(table(remodes()$cluster),decreasing = TRUE),5)[l])])
            colnames(column1)[1]="namess"
            colnames(column2)[1]="namess"
            value[45+l]<-table(duplicated(rbind(column1,column2)))["TRUE"]
          }
        }
      
      
        if(input$algorithm =="kmeans"){
          #fernoume ta apotelesmata means stis idies diastaseis
          
          #emploutismos values
          r=1
          for(l in 1:5)
          {
            column1<-as.data.frame(categ$ensemblid[categ$clusters==(head(order(table(remeans()$cluster),decreasing = TRUE),5)[r])])
            column2<-as.data.frame(countss2$ensemblid[countss2$clusters==(head(order(table(remodes()$cluster),decreasing = TRUE),5)[l])])
            colnames(column1)[1]="namess"
            colnames(column2)[1]="namess"
            value[l]<-table(duplicated(rbind(column1,column2)))["TRUE"]
          }
          r=2
          for(l in 1:5)
          {
            column1<-as.data.frame(categ$ensemblid[categ$clusters==(head(order(table(remeans()$cluster),decreasing = TRUE),5)[r])])
            column2<-as.data.frame(countss2$ensemblid[countss2$clusters==(head(order(table(remodes()$cluster),decreasing = TRUE),5)[l])])
            colnames(column1)[1]="namess"
            colnames(column2)[1]="namess"
            value[l+5]<-table(duplicated(rbind(column1,column2)))["TRUE"]
          }
          r=3
          for(l in 1:5)
          {
            column1<-as.data.frame(categ$ensemblid[categ$clusters==(head(order(table(remeans()$cluster),decreasing = TRUE),5)[r])])
            column2<-as.data.frame(countss2$ensemblid[countss2$clusters==(head(order(table(remodes()$cluster),decreasing = TRUE),5)[l])])
            colnames(column1)[1]="namess"
            colnames(column2)[1]="namess"
            value[l+10]<-table(duplicated(rbind(column1,column2)))["TRUE"]
          }
          r=4
          for(l in 1:5)
          {
            column1<-as.data.frame(categ$ensemblid[categ$clusters==(head(order(table(remeans()$cluster),decreasing = TRUE),5)[r])])
            column2<-as.data.frame(countss2$ensemblid[countss2$clusters==(head(order(table(remodes()$cluster),decreasing = TRUE),5)[l])])
            colnames(column1)[1]="namess"
            colnames(column2)[1]="namess"
            value[l+15]<-table(duplicated(rbind(column1,column2)))["TRUE"] 

          }
          r=5
          for(l in 1:5)
          {
            column1<-as.data.frame(categ$ensemblid[categ$clusters==(head(order(table(remeans()$cluster),decreasing = TRUE),5)[r])])
            column2<-as.data.frame(countss2$ensemblid[countss2$clusters==(head(order(table(remodes()$cluster),decreasing = TRUE),5)[l])])
            colnames(column1)[1]="namess"
            colnames(column2)[1]="namess"
            value[l+20]<-table(duplicated(rbind(column1,column2)))["TRUE"] 
          }
         
          r=1
          for(l in 1:5)
          {
            column1<-as.data.frame(countss2$ensemblid[countss2$clusters==(head(order(table(remodes()$cluster),decreasing = TRUE),5)[r])])
            column2<-as.data.frame(countss3$ensemblid[countss3$clusters==(head(order(table(reproto()$cluster),decreasing = TRUE),5)[l])])
            colnames(column1)[1]="namess"
            colnames(column2)[1]="namess"
            value[25+l]<-table(duplicated(rbind(column1,column2)))["TRUE"]
          }
          r=2
          for(l in 1:5)
          {
            column1<-as.data.frame(countss2$ensemblid[countss2$clusters==(head(order(table(remodes()$cluster),decreasing = TRUE),5)[r])])
            column2<-as.data.frame(countss3$ensemblid[countss3$clusters==(head(order(table(reproto()$cluster),decreasing = TRUE),5)[l])])
            colnames(column1)[1]="namess"
            colnames(column2)[1]="namess"
            value[30+l]<-table(duplicated(rbind(column1,column2)))["TRUE"]
          }
          r=3
          for(l in 1:5)
          {
            column1<-as.data.frame(countss2$ensemblid[countss2$clusters==(head(order(table(remodes()$cluster),decreasing = TRUE),5)[r])])
            column2<-as.data.frame(countss3$ensemblid[countss3$clusters==(head(order(table(reproto()$cluster),decreasing = TRUE),5)[l])])
            colnames(column1)[1]="namess"
            colnames(column2)[1]="namess"
            value[35+l]<-table(duplicated(rbind(column1,column2)))["TRUE"]
          }
          r=4
          for(l in 1:5)
          {
            column1<-as.data.frame(countss2$ensemblid[countss2$clusters==(head(order(table(remodes()$cluster),decreasing = TRUE),5)[r])])
            column2<-as.data.frame(countss3$ensemblid[countss3$clusters==(head(order(table(reproto()$cluster),decreasing = TRUE),5)[l])])
            colnames(column1)[1]="namess"
            colnames(column2)[1]="namess"
            value[40+l]<-table(duplicated(rbind(column1,column2)))["TRUE"]  

          }
          r=5
          for(l in 1:5)
          {
            column1<-as.data.frame(countss2$ensemblid[countss2$clusters==(head(order(table(remodes()$cluster),decreasing = TRUE),5)[r])])
            column2<-as.data.frame(countss3$ensemblid[countss3$clusters==(head(order(table(reproto()$cluster),decreasing = TRUE),5)[l])])
            colnames(column1)[1]="namess"
            colnames(column2)[1]="namess"
            value[45+l]<-table(duplicated(rbind(column1,column2)))["TRUE"]
          }
        }
        
      
      #if(input$algorithm =="kproto"){
       # for(kr in 1:5){
        #  namesofnodes[k+10]=paste("kmodes",head(order(table(remodes()$cluster),decreasing = TRUE),5)[k],sep="")
        #}
      #}
    #}
      value[is.na(value)]=0
      
      #----------------/////---------------problem
      #gia tis omadopoihseis pou den exoun 5 omades
      
      value[value>=(length(countss3$cluster)-2)]=0
      nodesforclusters<-data.frame(source,target,value)
      return(nodesforclusters)
      
    })
    
  output$sankeydiagram<-renderSankeyNetwork({
     library(networkD3)
     sankeyNetwork(Links = resankeysou(), Nodes = resankeynames(), Source = "source",Target = "target", Value = "value",NodeID = "names")
     
      })
  
  output$checkmaxnumber<-renderText({length(remodes()$cluster)})
   
   output$resankeysou<-renderTable({
      resankeysou()
    })
    output$resankeynames<-renderTable({
      resankeynames()
    })
  
}
# Run the app ----
shinyApp(ui, server)
