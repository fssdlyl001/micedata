library(shiny)
library(shinysky)
library(shinyBS)
library(ggplot2)
library(grid)
library(reshape2)
library(rJava)
library(xlsx)
options(shiny.maxRequestSize=30*1024^2)
options(java.parameters = "-Xmx8000m")

shinyServer(function(input, output, session) {
#Basic Analysis#
  Data = reactive({
    input$goButton
    if (input$goButton == 0) {return(NULL)}
    isolate({
      inFile = input$file1
      if (is.null(inFile))
        return(NULL)
      if (identical(input$format,'.xlsx')) {micedata = read.xlsx2(inFile$datapath, sheetIndex=input$sheet, header=F)
                                            if (is.data.frame(micedata)==F) {mice = read.xlsx(inFile$datapath, sheetIndex=input$sheet, header=F)} else {mice=micedata}
                                            mice[mice==""]="NA"; mice[mice==" "]="NA"} else{
        if (identical(input$format,'.csv'))  {mice = read.csv(inFile$datapath, header=F, na.strings=c("NA","NaN", ""," "), fileEncoding="latin1")} else{
          if (identical(input$format,'.txt'))  {mice = read.table(inFile$datapath, header=F, na.strings=c("NA","NaN", ""," "), fileEncoding="latin1")}}}
    
      if (any(colSums(is.na(mice))>=nrow(mice)-3)) {
        blankcol=which(colSums(is.na(mice))>=nrow(mice)-3)
        mice=mice[,-blankcol]}
      if (any(rowSums(is.na(mice))>=length(mice))) {
        blankrow=which(rowSums(is.na(mice))>=length(mice))
        mice=mice[-blankrow,]}
      headrow=1
      for (i in 1:(which(mice[,1] %in% c("C","c","T","t"))-1)){
      row1=lapply(mice[i,],as.character)
      if(length(which(!is.na(mice[i,])))>=(length(mice)-2)/2 & length(which(grepl("W",row1)))>=(length(mice)-2)/2){
        headrow=i
        headrownames=c(row1)
        break}}
      if (headrow==1){mice=mice[-1,]} else{
        mice=mice[-1:-headrow,]}
      colnames(mice)=headrownames
      
      test=which(mice[,1] %in% c("C","c","T","t"))-1
      control=nrow(mice)-test-1
      testnumber=test
      if (mice[test+1,1] %in% c("C","c")){label1=c("test","control")} else {label1=c("control","test")}
      mice=mice[-(test+1),]
      if (any(rowSums(is.na(mice))>length(mice)-2)) {
        blankrow=which(rowSums(is.na(mice))>length(mice)-2)
        mice=mice[-blankrow,]
        if (blankrow<=testnumber) {test=test-1} else {control=control-1}
      }
      
      GROUP=na.omit(unique(as.vector(mice[,1])))
      if (any(is.na(mice[,1]))) {
        bl=which(is.na(mice[,1]))
        g=mice[,1]
        if (bl[1] == 1) {return(NULL)}
        g[2] = g[1]
        g[3] = g[2]
        for (i in 3:(length(bl)-1)) {
          if (bl[i+1]-bl[i] == 1){
            g[bl[i]]=g[bl[i]-1]
            g[bl[i+1]]=g[bl[i]]}}
        if (is.na(g[length(g)])==T) {g[length(g)]=g[length(g)-1]}
        GROUP = g
        mice = data.frame(GROUP,mice[,-1])
      } else{GROUP=mice[,1]}
      
      if (is.na(as.numeric(substr(mice[2,2],1,1)))) {
        NUMBER = paste(GROUP,mice[,2],sep="")
        NUMBER=gsub(" ","",NUMBER)
      } else {NUMBER = as.character(mice[,2])}
      mice = data.frame(GROUP,NUMBER,mice[,-1:-2])
      
      if (any(is.na(mice)) == T){
        na.row=which(rowSums(is.na(mice))>0)
        na=length(na.row)
        if (na>0){
          for (i in 1:na){
            if (na.row[i]<=testnumber) {test=test-1} else {control=control-1}}}
        mice=na.omit(mice)
      }
      
      name1=c(0,0,names(mice)[-1:-2])
      orderw=which(grepl("W",name1))
      orderb=which(!grepl("W",name1))[-1:-2]
      mice=mice[,c(1,2,orderw,orderb)]
      n=length(orderw)
      rawmice = mice
      exactdate = NULL
      lean=0
      name=names(mice)
      if (nchar(name[-1:-2])>4 && grepl(".20",name[-1:-2])==T) {
        wonly=name[3:(length(orderw)+2)]
        position=regexpr("20",wonly)
        exactdate=substr(name[3:(length(orderw)+2)],position,nchar(wonly))
      }
      if (is.null(exactdate)) {labx = as.character(1:n); fontsize=12} else {labx = as.character(exactdate);lean=30; if (n<=9) {fontsize=13} else {fontsize=120/n}}
      zero = NULL
      if (n != length(orderb)){mice=mice[,-3]
                               if (labx[1]==1) {zero="W0";labx=labx[-n]} else {zero=labx[1];labx=labx[-1]}
                               n=n-1}
      name=names(mice)
      wname=paste("W",labx,sep="")
      bname=paste("BG",labx,sep="")
      colnames(mice)=c("GROUP","NUMBER",wname,bname)
      if (is.null(zero)) {colnames(rawmice)=colnames(mice)} else {colnames(rawmice)=c("GROUP","NUMBER",zero,wname,bname)}
      
      firstl=substr(names(mice),1,1)
      order1=which(firstl=="W")
      order2=which(firstl=="B")
      mice1=mice[, -order2]
      colnames(mice1)[3:(n+2)]=as.character(c(1:n))
      mice2=mice[, -order1]
      colnames(mice2)[3:(n+2)]=as.character(c(1:n))
      mice1=melt(mice1,ids=1:2,measure=c(3:length(mice1)))
      mice2=melt(mice2,ids=1:2,measure=c(3:length(mice2)))
      micetotal=data.frame(mice1,mice2[,c(-1:-3)])
      colnames(micetotal) = c("GROUP","NUMBER","DATE","W","BG")
      micetotal$W=as.numeric(as.character(micetotal$W))
      micetotal$BG=as.numeric(as.character(micetotal$BG))
      TYPE=factor(rep(c(rep(0,times=test),rep(1,times=control)),times=n),levels=c(0,1),labels=c("test","control"))
      micetotal=data.frame(TYPE,micetotal)
      
      lastw=wname[n]
      lastbg=bname[n]
      TG=subset(micetotal, TYPE=="test",select=-TYPE)
      CG=subset(micetotal, TYPE=="control",select=-TYPE)
      MBG=na.omit(c(tapply(as.numeric(TG$BG),TG$NUMBER,mean),tapply(as.numeric(CG$BG),CG$NUMBER,mean)))
      type.n=factor(c(rep(0,times=test),rep(1,times=control)),levels=c(0,1),labels=c("test","control"))
      relation=data.frame(as.numeric(as.character(mice[,lastw])),as.numeric(as.character(mice[,lastbg])),type.n)
      relation2=data.frame(as.numeric(as.character(mice[,lastw]))-as.numeric(as.character(mice[,order1[1]])),MBG,type.n)
      colnames(relation)=c("lastw","lastbg","type.n")
      colnames(relation2)=c("wgain","bgmean","type.n")
      
      wsd=data.frame(t(tapply(micetotal$W,data.frame(micetotal$TYPE,micetotal$DATE),sd)))
      wse=c(wsd$test/sqrt(test),wsd$control/sqrt(control))
      bsd=data.frame(t(tapply(micetotal$BG,data.frame(micetotal$TYPE,micetotal$DATE),sd)))
      bse=c(bsd$test/sqrt(test),bsd$control/sqrt(control))
      wmean=data.frame(t(tapply(micetotal$W,data.frame(micetotal$TYPE,micetotal$DATE),mean)))
      wmean=c(wmean$test,wmean$control)
      bmean=data.frame(t(tapply(micetotal$BG,data.frame(micetotal$TYPE,micetotal$DATE),mean)))
      bmean=c(bmean$test,bmean$control)
      date=c(1:n,1:n)
      type=factor(rep(c(0,1),each=n),levels=c(0,1),labels=c("test","control"))
      MEAN=data.frame(type,date,wmean,bmean,wse,bse)
      pd=position_dodge(.3)
      MEAN=as.data.frame(MEAN)
      mice=as.data.frame(mice)
      micetotal=as.data.frame(micetotal)
      if (mean(bmean) > 40) {unit="Plasma Glucose (mg/dl)"; scale=10; digit=-1} else {unit="Plasma Glucose (mmol/L)"; scale=1; digit=0}
      
      info = list(MEAN=MEAN, mice=mice, rawmice=rawmice, micetotal=micetotal, wmean=wmean, bmean=bmean, wse=wse, bse=bse, pd=pd, n=n, TYPE=micetotal$TYPE, DATE=micetotal$DATE, GROUP=GROUP, W=micetotal$W, BG=micetotal$BG, NUMBER=micetotal$NUMBER, relation=relation, relation2=relation2, lastw=relation$lastw, lastbg=relation$lastbg, type.n=type.n, labx=labx, fontsize=fontsize, unit=unit, label1=label1, scale=scale, digit=digit, lean=lean)
      return(info)
      })
  })
  
  Value = reactive({
    total=Data()$micetotal
    TYPE=Data()$TYPE
    DATE=Data()$DATE
    GROUP=Data()$GROUP
    W=Data()$W
    BG=Data()$BG
    NUMBER=Data()$NUMBER
    MEAN=Data()$MEAN
    pd=Data()$pd
    n=Data()$n
    wmean=Data()$wmean
    bmean=Data()$bmean
    wse=Data()$wse
    bse=Data()$bse
    relation=Data()$relation
    relation2=Data()$relation2
    lastw=Data()$lastw
    lastbg=Data()$lastbg
    type.n=Data()$type.n
    labx=Data()$labx
    fontsize=Data()$fontsize
    unit=Data()$unit
    label1=Data()$label1
    scale=Data()$scale
    digit=Data()$digit
    lean=Data()$lean
    
    isolate({
    aline=ggplot(MEAN,aes(x=date,y=wmean,colour=type,shape=type))+geom_errorbar(aes(ymin=wmean-wse,ymax=wmean+wse),width=.3,size=0.75,position=pd)+geom_line(position=pd,size=0.75)+geom_point(position=pd,size=4)+ggtitle("Body Weight Gain")+labs(x="Test Time",y="Body Weight (g)",colour="Group",shape="Group")+scale_x_continuous(breaks=c(seq(1,n-1,1),n+0.2),labels=labx)+scale_y_continuous(breaks=seq(round(min(wmean-wse)-5,-1),round(max(wmean+wse)+5,-1),5))+coord_cartesian(xlim=c(0.8,n+0.2),ylim=c(min(wmean-wse)-5,round(round(max(wmean+wse)+5,-1))))+scale_color_manual(values=c("#CC0000","#000000"),labels=label1)+scale_shape_manual(values=c(16,17),labels=label1)+theme(plot.title = element_text(face="bold",size=20),legend.title=element_text(face="bold",size=14),legend.text=element_text(face="bold",size=12),legend.key.height=unit(1,"cm"),legend.key=element_rect(fill="white"),axis.title.x = element_text(face="bold", size=16),axis.title.y = element_text(face="bold", size=15),axis.text.x=element_text(colour="black",face="bold",angle=lean,hjust=1,vjust=1,size=fontsize),axis.text.y=element_text(colour="black",face="bold",size=12),panel.background=element_blank(),panel.background=element_blank(),panel.grid.minor=element_blank(),axis.line=element_line(colour="black"))
    bline=ggplot(MEAN,aes(x=date,y=bmean,colour=type,shape=type))+geom_errorbar(aes(ymin=bmean-bse,ymax=bmean+bse),width=.3,size=0.75,position=pd)+geom_line(position=pd,size=0.75)+geom_point(position=pd,size=4)+ggtitle("Plasma Glucose Change")+labs(x="Test Time",y=unit,colour="Group",shape="Group")+scale_x_continuous(breaks=c(seq(1,n-1,1),n+0.2),labels=labx)+scale_y_continuous(breaks=seq(round(min(bmean-bse)-scale,digit),round(max(bmean+bse)+scale,digit),scale))+coord_cartesian(xlim=c(0.8,n+0.2),ylim=c(round(min(bmean-bse)-scale,digit),round(max(bmean+bse)+scale,digit)))+scale_color_manual(values=c("#CC0000","#000000"),labels=label1)+scale_shape_manual(values=c(16,17),labels=label1)+theme(plot.title = element_text(face="bold",size=20),legend.title=element_text(face="bold",size=14),legend.text=element_text(face="bold",size=12),legend.key.height=unit(1,"cm"),legend.key=element_rect(fill="white"),axis.title.x = element_text(face="bold", size=16),axis.title.y = element_text(face="bold", size=15),axis.text.x=element_text(colour="black",face="bold",angle=lean,hjust=1,vjust=1,size=fontsize),axis.text.y=element_text(colour="black",face="bold",size=12),panel.background=element_blank(),panel.background=element_blank(),panel.grid.minor=element_blank(),axis.line=element_line(colour="black"))
    abar=ggplot(MEAN,aes(x=date,y=wmean))+geom_bar(stat="identity",position="dodge",aes(fill=type),colour="black")+geom_errorbar(aes(ymin=wmean,ymax=wmean+wse,group=type),width=.3,size=0.5,colour="black",position=position_dodge(.9))+scale_fill_grey(start=1,end=0.5,labels=label1)+ggtitle("Body Weight Gain")+labs(x="Date",y="Body Weight (g)",fill="Group")+scale_x_continuous(breaks=seq(1,n,1),labels=labx)+scale_y_continuous(expand=c(0,0),breaks=seq(round(min(wmean-wse)-5,-1),round(max(wmean+wse)+5,-1),5))+coord_cartesian(ylim=c(min(wmean-wse)-5,round(round(max(wmean+wse)+5,-1))))+theme(plot.title = element_text(face="bold",size=20),legend.title=element_text(face="bold",size=14),legend.text=element_text(face="bold",size=12),legend.key=element_rect(fill="white"),axis.title.x = element_text(face="bold", size=16),axis.title.y = element_text(face="bold", size=15),axis.text.x=element_text(colour="black",face="bold",angle=lean,hjust=1,vjust=1,size=fontsize),axis.text.y=element_text(colour="black",face="bold",size=12),panel.background=element_blank(),panel.grid.minor=element_blank(),axis.line=element_line(colour="black"))
    bbar=ggplot(MEAN,aes(x=date,y=bmean))+geom_bar(stat="identity",position="dodge",aes(fill=type),colour="black")+geom_errorbar(aes(ymin=bmean,ymax=bmean+bse,group=type),width=.3,size=0.5,colour="black",position=position_dodge(.9))+scale_fill_grey(start=1,end=0.5,labels=label1)+ggtitle("Plasma Glucose Change")+labs(x="Date",y=unit,fill="Group")+scale_x_continuous(breaks=seq(1,n,1),labels=labx)+scale_y_continuous(breaks=seq(round(min(bmean-bse)-scale,digit),round(max(bmean+bse)+scale,digit),scale))+coord_cartesian(ylim=c(round(min(bmean-bse)-scale,digit),round(max(bmean+bse)+scale,digit)))+theme(plot.title = element_text(face="bold",size=20),legend.title=element_text(face="bold",size=14),legend.text=element_text(face="bold",size=12),legend.text=element_text(face="bold",size=10),legend.key=element_rect(fill="white"),axis.title.x = element_text(face="bold", size=16),axis.title.y = element_text(face="bold", size=15),axis.text.x=element_text(colour="black",face="bold",angle=lean,hjust=1,vjust=1,size=fontsize),axis.text.y=element_text(colour="black",face="bold",size=12),panel.background=element_blank(),panel.grid.minor=element_blank(),axis.line=element_line(colour="black"))
    t1=ggplot(total,aes(x=interaction(TYPE,DATE),y=W,fill=TYPE),main="Body Weight")+geom_boxplot()+geom_line(aes(colour=factor(GROUP),group=NUMBER),show_guide = FALSE)+geom_text(aes(label=NUMBER),size=3)+labs(x="Date",y="Body Weight (g)",fill="Treatment")+scale_x_discrete(breaks=seq(1,n,1))+scale_fill_brewer(palette="Set2",labels=label1)+theme(plot.title = element_text(face="bold",size=20),legend.title=element_text(face="bold",size=14),legend.text=element_text(face="bold",size=12),legend.key.height=unit(1,"cm"),legend.key=element_rect(fill="white"),axis.title.x = element_text(face="bold", size=16),axis.title.y = element_text(face="bold", size=16),axis.text.x=element_text(colour="black",face="bold",angle=lean,hjust=1,vjust=1, size=14),axis.text.y=element_text(colour="black",face="bold"),panel.background=element_blank(),panel.background=element_blank(),panel.grid.minor=element_blank(),axis.line=element_line(colour="black"))
    t2=ggplot(total,aes(x=interaction(TYPE,DATE),y=BG,fill=TYPE),main="Blood Glucose")+geom_boxplot()+geom_line(aes(colour=factor(GROUP),group=NUMBER),show_guide = FALSE)+geom_text(aes(label=NUMBER),size=3)+labs(x="Date",y=unit,fill="Treatment")+scale_x_discrete(breaks=seq(1,n,1))+scale_fill_brewer(palette="Set2",labels=label1)+theme(plot.title = element_text(face="bold",size=20),legend.title=element_text(face="bold",size=14),legend.text=element_text(face="bold",size=12),legend.key.height=unit(1,"cm"),legend.key=element_rect(fill="white"),axis.title.x = element_text(face="bold", size=16),axis.title.y = element_text(face="bold", size=16),axis.text.x=element_text(colour="black",face="bold",angle=lean,hjust=1,vjust=1, size=14),axis.text.y=element_text(colour="black",face="bold", size=14),panel.background=element_blank(),panel.background=element_blank(),panel.grid.minor=element_blank(),axis.line=element_line(colour="black"))
    inter=ggplot(relation,aes(x=lastw,y=lastbg,colour=type.n,shape=type.n,group=type.n))+geom_point()+geom_smooth(method=lm)+ggtitle("Weight & Plasma Glucose")+labs(x="Last Time Weight",y="Last Time Plasma Glucose",colour="Group",shape="Group")+scale_color_manual(values=c("#0033FF","#FF9900"),labels=label1)+scale_shape_manual(values=c(16,17),labels=label1)+theme(plot.title = element_text(face="bold",size=20),legend.title=element_text(face="bold",size=14),legend.text=element_text(face="bold",size=12),legend.key.height=unit(1,"cm"),legend.key=element_rect(fill="white"),axis.title.x = element_text(face="bold", size=16),axis.title.y = element_text(face="bold", size=16),axis.text.x=element_text(colour="black",face="bold", size=14),axis.text.y=element_text(colour="black",face="bold", size=14),panel.background=element_blank(),panel.background=element_blank(),panel.grid.minor=element_blank(),axis.line=element_line(colour="black"))
    inter2=ggplot(relation2,aes(x=wgain,y=bgmean,colour=type.n,shape=type.n,group=type.n))+geom_point()+geom_smooth(method=lm)+ggtitle("Weight Gain & Plasma Glucose (Whole Time)")+labs(x="Weight Gain",y="Mean Plasma Glucose",colour="Group",shape="Group")+scale_color_manual(values=c("#0033FF","#FF9900"),labels=label1)+scale_shape_manual(values=c(16,17),labels=label1)+theme(plot.title = element_text(face="bold",size=20),legend.title=element_text(face="bold",size=14),legend.text=element_text(face="bold",size=12),legend.key.height=unit(1,"cm"),legend.key=element_rect(fill="white"),axis.title.x = element_text(face="bold", size=16),axis.title.y = element_text(face="bold", size=16),axis.text.x=element_text(colour="black",face="bold", size=14),axis.text.y=element_text(colour="black",face="bold", size=14),panel.background=element_blank(),panel.background=element_blank(),panel.grid.minor=element_blank(),axis.line=element_line(colour="black"))
    
    info2 = list(MEAN=MEAN,total=total,relation=relation,relation2=relation2,n=n,aline=aline,bline=bline,abar=abar,bbar=bbar,t1=t1,t2=t2,inter=inter,inter2=inter2)
    return(info2)
    })
  })
  
  
  output$example = renderTable({
    w0=c(33,28,33,42.5,40.5,41,"......","NA",20,22,19,"......")
    w1=c(44,33.3,33.5,45.4,41.4,44.4,"......","NA",25.2,22.2,19.2,"......")
    w2=c("52.0",44.4,44.8,55,54.4,55.5,"......","NA",33.3,32.2,31.1,"......")
    bg1=c(13,13.3,11.1,10.0,11.5,9.0,"......","NA",9.9,10.0,8.8,"......")
    bg2=c(14,9.9,10.0,11.0,1,9.0,"......","NA",9.0,10.0,11.0,"......")
    gp=c(1,"1 or NA","1 or NA",2,"2 or NA","2 or NA","......","C or T",3,"3 or NA","3 or NA","......")
    nb=c("1L","1R","1N","2L","2R","2N","......","NA","3L","3R","3N","......")
    example = data.frame(gp,nb,w0,w1,w2,bg1,bg2)
    colnames(example)=c("GROUP \ (flexible)","NUMBER (flexible)","W0","W1","W2","BG1","BG2")
    print(example)
  })

  output$raw = renderTable({
    if (input$goButton == 0) {return(NULL)} 
    if (is.null(input$file1))
      return(NULL)
    rawmice=Data()$rawmice
    rawmice
  })
  
  output$MEAN = renderTable({
    if (input$goButton == 0) {return(NULL)} 
    if (is.null(input$file1))
      return(NULL)
    MEAN=Data()$MEAN
    colnames(MEAN)=c("Treatment","Date","Average Weight","Average Plasma Glucose","Weight se","Plasma Glucose se")
    MEAN
  })
  
  output$graph1 = renderPlot({
    if (input$goButton == 0) {return(NULL)} 
    if (is.null(input$file1))
      return(NULL)
    p1=Value()$aline
    print(p1)},width=650,height=400)
  
  output$graph2 = renderPlot({
    if (input$goButton == 0) {return(NULL)} 
    if (is.null(input$file1)) 
      return(NULL)
    p2=Value()$bline
    print(p2)},width=650,height=400)
  
  output$graph3 = renderPlot({
    if (input$goButton == 0) {return(NULL)} 
    if (is.null(input$file1))
      return(NULL)
    p3=Value()$abar
    print(p3)},width=650,height=400)
  
  output$graph4 = renderPlot({
    if (input$goButton == 0) {return(NULL)} 
    if (is.null(input$file1)) 
      return(NULL)
    p4=Value()$bbar
    print(p4)},width=650,height=400)
  
  output$graph5 = renderPlot({
    if (input$goButton == 0) {return(NULL)} 
    if (is.null(input$file1)) 
      return(NULL)
    t1=Value()$t1
    print(t1)},width=750,height=400)
  
  output$graph6 = renderPlot({
    if (input$goButton == 0) {return(NULL)} 
    if (is.null(input$file1)) 
      return(NULL)
    t2=Value()$t2
    print(t2)},width=750,height=400)
  
  output$graph7 = renderPlot({
    if (input$goButton == 0) {return(NULL)} 
    if (is.null(input$file1)) 
      return(NULL)
    inter=Value()$inter
    print(inter)},width=700,height=400)
  
  output$graph8 = renderPlot({
    if (input$goButton == 0) {return(NULL)} 
    if (is.null(input$file1)) 
      return(NULL)
    inter2=Value()$inter2
    print(inter2)},width=700,height=400)

  
  
  
  
#Metabolic Cage#
  Metainfo = reactive({
    inFile2 = input$file2
    if (is.null(inFile2))
      return(NULL)
    judge = read.csv(inFile2$datapath, header=F, na.strings=c("NA","NaN", ""," "), nrows=18, fileEncoding="latin1", row.names=NULL)
    start2=which(judge[,1]=="Date")
    if (is.na(as.numeric(judge[start2-1,1]))) {end1=start2-5} else {end1=start2-4}
    information = read.csv(inFile2$datapath, header=T, na.strings=c("NA","NaN", ""," "), skip=2, nrows=end1, fileEncoding="latin1", row.names=NULL)
    infoheader=colnames(information)
    
    if ((!any(grepl("Text1",infoheader)))|(!any(grepl("Box",infoheader))))
      return(NULL)
    box = as.vector(information[,grep("Box",infoheader)[1]])
    text1 = as.character(information[,grep("Text1",infoheader)[1]])
    
    if (any(grepl("Text2",infoheader))) {
      text2 = as.character(information[,grep("Text2",infoheader)[1]])
    } else {
      text2 = as.character(box)
    }
    
    text2.1 = text2
    if (any(duplicated(text2.1))) {
      dup=which(duplicated(text2.1))
      text2.1[dup]=paste(text2.1[dup],".",length(dup),sep="")
    }
    
    
    cage = read.csv(inFile2$datapath, header=T, na.strings=c("NA","NaN", ""," "), skip=start2-1, fileEncoding="latin1", row.names=NULL)[-1,]
    if (any(rowSums(!is.na(cage))<2)){
      emptyr=which(rowSums(!is.na(cage))<2)
      cage=cage[-emptyr,]
    }
    if (any(colSums(!is.na(cage))<2)){
      emptyc=which(colSums(!is.na(cage))<2)
      cage=cage[,-emptyc]
    }
    
    Treatment = as.numeric(as.character(cage$Box))
    Group = as.numeric(as.character(cage$Box))
    for (i in 1:length(box)) {
      j=box[i]
      Treatment[Treatment==j]=text1[i]
      Group[Group==j]=text2.1[i]
    }
    cage=data.frame(cage,Treatment,Group)
    nchar=nchar(as.vector(cage$Date))
    cage$Date=as.character(as.Date(paste(substring(cage$Date,1,nchar-4),substring(cage$Date,nchar-1,nchar),sep=""),"%m/%d/%y"))
    cage$Time=substring(strptime(cage$Time,"%H:%M"),12,16)
    
    errordata=0
    if (!any(colnames(cage)=="RER")) {
      RER=rep(0,nrow(cage))
      cage=data.frame(cage,RER)
      errordata=1
    }
    if (!any(colnames(cage)=="XT.YT")) {
      XT.YT=rep(0,nrow(cage))
      cage=data.frame(cage,XT.YT)
      errordata=1
    }
    if (!any(colnames(cage)=="Feed")) {
      Feed=rep(0,nrow(cage))
      cage=data.frame(cage,Feed)
      errordata=1
    }
    
    feedchange=c()
    for (i in 1:length(box)) {
      fc=subset(cage, Box==box[i],select=Feed)
      fc=as.numeric(as.matrix(fc))
      fc=c(0,fc[-1]-fc[-length(fc)])
      feedchange=c(feedchange,fc)
    }
    cage$feedchange=feedchange
    
    metainfo=list(inFile2=inFile2, box=box, text1=text1, text2=text2, cage=cage, errordata=errordata)
    return(metainfo)
  })
  
  Data2 = reactive({
    if (input$goButton2 == 0)
      return(NULL)       
    isolate({
      inFile2=Metainfo()$inFile2
      box=Metainfo()$box
      text1=Metainfo()$text1
      text2=Metainfo()$text2
      cage=Metainfo()$cage
      errordata=Metainfo()$errordata
      
      if (input$ifchoosebox == TRUE) {
        if (!is.null(input$boxchoose)) {
          boxchoose=as.numeric(input$boxchoose)
          text1=text1[boxchoose]
          text2=text2[boxchoose]
          box=box[boxchoose]
          cage=subset(cage,Box %in% box)
          
          if (length(input$boxchoose) == 0)
            return(NULL)
        }
      }
      
      time=subset(cage,Box==box[1],select=Time)
      time1=as.character(as.matrix(time))
      Time1=paste(cage$Date,cage$Time)
      c1=data.frame(Time1,cage[,-(1:2)])
      n=nrow(time)
      groupnum=as.character(unique(text1))
      
      merrorformat=input$merrorformat
      if (is.null(merrorformat)) {return(NULL)} else {
        if (merrorformat == 1) {sdfunction=se} else {sdfunction=sd}
      }
      
      if (any(duplicated(text1))) {
        if (!any(!(text1 %in% unique(text1[duplicated(text1)])))) {
          if (length(groupnum) >= 2) {
            MRER=with(c1,tapply(as.numeric(as.character(RER)),data.frame(Time1,Treatment),mean))
            typeorder=colnames(MRER)
            MRER=as.vector(MRER)
            MPA=with(c1,tapply(as.numeric(as.character(XT.YT)),data.frame(Time1,Treatment),mean))
            MPA=as.vector(MPA)
            MF=with(c1,tapply(as.numeric(as.character(Feed)),data.frame(Time1,Treatment),mean))
            MF=as.vector(MF)
            MFC=with(c1,tapply(as.numeric(as.character(feedchange)),data.frame(Time1,Treatment),mean))
            MFC=as.vector(MFC)
            
            SD1=with(c1,tapply(as.numeric(as.character(RER)),data.frame(Time1,Treatment),sdfunction))
            SD1=as.vector(SD1)
            SD2=with(c1,tapply(as.numeric(as.character(XT.YT)),data.frame(Time1,Treatment),sdfunction))
            SD2=as.vector(SD2)
            SD3=with(c1,tapply(as.numeric(as.character(Feed)),data.frame(Time1,Treatment),sdfunction))
            SD3=as.vector(SD3)
            SD4=with(c1,tapply(as.numeric(as.character(feedchange)),data.frame(Time1,Treatment),sdfunction))
            SD4=as.vector(SD4)
            type1=rep(1:length(groupnum),each=n)
            num=rep(1:n,length(groupnum))
            Time=rep(Time1[1:n],length(groupnum))
            Mean1=data.frame(num,Time,MRER,MPA,MF,MFC,SD1,SD2,SD3,SD4,type1)
          } else {
            MRER=with(c1,tapply(as.numeric(as.character(RER)),Time1,mean))
            typeorder=unique(text1)
            MRER=as.vector(MRER)
            MPA=with(c1,tapply(as.numeric(as.character(XT.YT)),Time1,mean))
            MPA=as.vector(MPA)
            MF=with(c1,tapply(as.numeric(as.character(Feed)),Time1,mean))
            MF=as.vector(MF)
            MFC=with(c1,tapply(as.numeric(as.character(feedchange)),Time1,mean))
            MFC=as.vector(MFC)
            
            SD1=with(c1,tapply(as.numeric(as.character(RER)),Time1,sdfunction))
            SD1=as.vector(SD1)
            SD2=with(c1,tapply(as.numeric(as.character(XT.YT)),Time1,sdfunction))
            SD2=as.vector(SD2)
            SD3=with(c1,tapply(as.numeric(as.character(Feed)),Time1,sdfunction))
            SD3=as.vector(SD3)
            SD4=with(c1,tapply(as.numeric(as.character(feedchange)),Time1,sdfunction))
            SD4=as.vector(SD4)
            type1=rep(1,each=n)
            num=rep(1:n,length(groupnum))
            Time=rep(Time1[1:n],length(groupnum))
            Mean1=data.frame(num,Time,MRER,MPA,MF,MFC,SD1,SD2,SD3,SD4,type1)
          }
        } else {
          isdup=which(text1 %in% unique(text1[duplicated(text1)]))
          nodup=which(!(text1 %in% unique(text1[duplicated(text1)])))
          c2=subset(c1, Treatment %in% text1[isdup])
          c3=subset(c1, Treatment %in% text1[nodup])
          MRER=with(c2,tapply(as.numeric(as.character(RER)),data.frame(Time1,Treatment),mean))
          MRER=MRER[,colSums(is.na(MRER)) != nrow(MRER)]
          if (length(groupnum)==length(nodup)+1) {typeorder=text1[isdup][1]} else {typeorder=colnames(MRER)}
          MRER=c(as.vector(MRER),c3$RER)
          MPA=with(c2,tapply(as.numeric(as.character(XT.YT)),data.frame(Time1,Treatment),mean))
          MPA=MPA[,-which(colSums(is.na(MPA))>0)]
          MPA=c(as.vector(MPA),c3$XT.YT)
          MF=with(c2,tapply(as.numeric(as.character(Feed)),data.frame(Time1,Treatment),mean))
          MF=MF[,-which(colSums(is.na(MF))>0)]
          MF=c(as.vector(MF),as.numeric(as.character(c3$Feed)))
          MFC=with(c2,tapply(as.numeric(as.character(feedchange)),data.frame(Time1,Treatment),mean))
          MFC=MFC[,-which(colSums(is.na(MFC))>0)]
          MFC=c(as.vector(MFC),as.numeric(as.character(c3$feedchange)))
          SD1=with(c2,tapply(as.numeric(as.character(RER)),data.frame(Time1,Treatment),sdfunction))
          SD1=SD1[,-which(colSums(is.na(SD1))>0)]
          SD1=c(as.vector(SD1),rep(0,nrow(c3)))
          SD2=with(c2,tapply(as.numeric(as.character(XT.YT)),data.frame(Time1,Treatment),sdfunction))
          SD2=SD2[,-which(colSums(is.na(SD2))>0)]
          SD2=c(as.vector(SD2),rep(0,nrow(c3)))
          SD3=with(c2,tapply(as.numeric(as.character(Feed)),data.frame(Time1,Treatment),sdfunction))
          SD3=SD3[,-which(colSums(is.na(SD3))>0)]
          SD3=c(as.vector(SD3),rep(0,nrow(c3)))
          SD4=with(c2,tapply(as.numeric(as.character(feedchange)),data.frame(Time1,Treatment),sdfunction))
          SD4=SD4[,-which(colSums(is.na(SD4))>0)]
          SD4=c(as.vector(SD4),rep(0,nrow(c3)))
          typeorder=c(typeorder,text1[nodup])
          type1=rep(1:length(groupnum),each=n)
          num=rep(1:n,length(groupnum))
          Time=rep(Time1[1:n],length(groupnum))
          Mean1=data.frame(num,Time,MRER,MPA,MF,MFC,SD1,SD2,SD3,SD4,type1)
        }
      } else {
        MRER=with(c1,tapply(as.numeric(as.character(RER)),data.frame(Time1,Treatment),mean))
        typeorder=colnames(MRER)
        MRER=as.vector(MRER)
        MPA=with(c1,tapply(as.numeric(as.character(XT.YT)),data.frame(Time1,Treatment),mean))
        MPA=as.vector(MPA)
        MF=with(c1,tapply(as.numeric(as.character(Feed)),data.frame(Time1,Treatment),mean))
        MF=as.vector(MF)
        MFC=with(c1,tapply(as.numeric(as.character(feedchange)),data.frame(Time1,Treatment),mean))
        MFC=as.vector(MFC)
        type1=rep(1:length(text1),each=n)
        num=rep(1:n,length(text1))
        Time=rep(Time1[1:n],length(text1))
        Mean1=data.frame(num,Time,MRER,MPA,MF,MFC,type1)
        
        if (length(text1) == 1) {
          Mean1=na.omit(Mean1)
          typeorder=unique(text1)
        }
      }
      
      typefac=factor(type1,levels=unique(type1),labels=typeorder)
      
      d1=gsub("-",".",unique(cage$Date))
      date1=paste(d1[1],d1[length(d1)],sep="-")
      date1=paste("(",date1,")",sep="")
      
      if (errordata==1|length(groupnum)<2) {P1="";P2="";P3=""} else {
        
        reraov=aov(MRER~type1*num,data=Mean1)
        p=summary(reraov)
        Fv=p[[1]][1,5]
        if (Fv<0.0001){P1="P<0.0001"}
        if (Fv>=0.0001&Fv<0.1){value=round(Fv,4)
        P1=paste("P =",value)}
        if (Fv>=0.1){P1="    NS"}
        
        paaov=aov(MPA~type1*num,data=Mean1)
        p2=summary(paaov)
        Fv=p2[[1]][1,5]
        if (Fv<0.0001){P2="P<0.0001"}
        if (Fv>=0.0001&Fv<0.1){value=round(Fv,4)
        P2=paste("P =",value)}
        if (Fv>=0.1){P2="    NS"}
        
        faov=aov(MF~type1*num,data=Mean1)
        p3=summary(faov)
        Fv=p3[[1]][1,5]
        if (Fv<0.0001){P3="P<0.0001"}
        if (Fv>=0.0001&Fv<0.1){value=round(Fv,4)
        P3=paste("P =",value)}
        if (Fv>=0.1){P3="    NS"}
      }
      
      info3=list(cage=cage, Mean1=Mean1, num=num, time1=time1, P1=P1, P2=P2, P3=P3, n=n, date1=date1, groupnum=groupnum, text1=text1, text2=text2, typefac=typefac)
      return(info3)
    })
  })
  
  Value2 = reactive({
    cage=Data2()$cage
    Mean1=Data2()$Mean1
    n=Data2()$n
    num=Data2()$num
    time1=Data2()$time1
    P1=Data2()$P1
    P2=Data2()$P2
    P3=Data2()$P3
    date1=Data2()$date1
    groupnum=Data2()$groupnum
    text1=Data2()$text1
    text2=Data2()$text2
    typefac=Data2()$typefac
    dawntime=input$daytime[1]
    dusktime=input$daytime[2]
    feedchange=cage$feedchange
    
    isolate({
      if (input$ifchoosebox == TRUE) {
        if (length(input$boxchoose) == 0)
          return(NULL)
      }
      
      pd2=position_dodge(.9)
      breaks=length(time1)+1
      
      addzero1="0"
      addzero2="0"
      addzero3=""
      addzero4=""
      if (dusktime<10) {
        addzero3="0"
        addzero4="0"
      } else {
        if(dusktime==10) {
          addzero4="0"
        } else {
          if (dawntime>=10) {
            addzero1=""
            if (dawntime>10) {
              addzero2=""
            }
          }
        }
      }
      
      dawntime1=paste(addzero1,floor(dawntime),":",sep="")
      dawntime2=paste(addzero2,floor(dawntime)-1,":",sep="")
      dusktime1=paste(addzero3,floor(dusktime),":",sep="")
      dusktime2=paste(addzero4,floor(dusktime)-1,":",sep="")
      
      if (!any(substr(time1,1,3)==dawntime1)|!any(substr(time1,1,3)==dusktime1)) {rect_right=0; rect_left=0; br=c(0,breaks); timepoints=c("","")} else {
        dawn=grep(dawntime1,substr(time1,1,3))
        dawn1=grep(dawntime2,substr(time1,1,3))
        dusk=grep(dusktime1,substr(time1,1,3))
        dusk1=grep(dusktime2,substr(time1,1,3))
        if (any(dawn[-1] %in% (dawn1+1))) {
          rect_right=c(dawn[1],intersect(dawn[-1],dawn1+1))
        } else {rect_right=dawn[1]}
        if (any(dusk[-1] %in% (dusk1+1))) {
          rect_left=c(dusk[1],intersect(dusk[-1],dusk1+1))
        } else {rect_left=dusk[1]}
        
        br=sort(c(rect_left,rect_right))    
        if (rect_right[1]<rect_left[1]) {timepoints=c(paste(dawntime1,"00",sep=""),paste(dusktime1,"00",sep="")); rect_left=c(0,rect_left)} else {timepoints=c(paste(dusktime1,"00",sep=""),paste(dawntime1,"00",sep=""))}
        if (rect_right[length(rect_right)]<rect_left[length(rect_left)]) {rect_right=c(rect_right,breaks)}
        if ((length(br) %% 2)==0) {timepoints=rep(timepoints,length(br)/2)} else {timepoints=rep(timepoints,length(br)/2+1);timepoints=timepoints[-length(timepoints)]}
        if (br[1] != 0) {br=c(0,br); timepoints=c("",timepoints)}
        br=c(br,breaks)
        timepoints=c(timepoints,"")
      }
      
      Mean1=data.frame(Mean1,typefac)
      if (any(duplicated(text1))) {
        if (length(groupnum)==2) {
          if (length(text1)==8 & length(unique(text1[c(1,2,5,6)])==1 & length(unique(text1[c(3,4,7,8)])==1))) {
            rer=ggplot(cage,aes(x=Time,y=RER,colour=factor(Group,levels=unique(Group)),group=Group),environment=environment())+geom_line()+ggtitle(paste("Total RER",date1))+labs(x="Time",y="RER(VCO2/O2)")+theme(axis.text.x=element_text(angle=90,hjust=1,size=6))+guides(colour=guide_legend("Group"))
            rer=rer+facet_grid(.~Date,scales="free",space="free")+scale_color_manual(values=c("#000000","#333333","#00FFFF","#3399CC","#666666","#999999","#0000FF","#000099"))+theme(panel.background=element_blank(),panel.grid.minor=element_blank(),axis.line=element_line())
            rer=rer+theme(plot.title = element_text(face="bold",size=18),strip.text=element_text(face="bold",size=rel(0.8)),strip.background=element_rect(fill="lightblue",colour="black",size=1),legend.key.height=unit(1,"cm"),legend.key=element_rect(fill="white"),legend.text=element_text(face="bold",colour="black"),axis.title.x = element_text(face="bold", size=18),axis.title.y = element_text(face="bold", size=17),legend.title=element_text(face="bold",size=14),axis.text.x=element_text(colour="black",face="bold",size=5.5),axis.text.y=element_text(colour="black",face="bold",size=12),axis.line=element_line(colour="black"))
            
            pa=ggplot(cage,aes(x=Time,y=as.numeric(as.character(XT.YT)),colour=factor(Group,levels=unique(Group)),group=Group),environment=environment())+geom_line()+ggtitle(paste("Total Physical Activity",date1))+labs(x="Time",y="Physical Activity (Cnts)")+theme(axis.text.x=element_text(angle=90,hjust=1,size=6))+guides(colour=guide_legend("Group"))
            pa=pa+facet_grid(.~Date,scales="free",space="free")+scale_color_manual(values=c("#000000","#333333","#00FFFF","#3399CC","#666666","#999999","#0000FF","#000099"))+theme(panel.background=element_blank(),panel.grid.minor=element_blank(),axis.line=element_line())
            pa=pa+theme(plot.title = element_text(face="bold",size=18),strip.text=element_text(face="bold",size=rel(0.8)),strip.background=element_rect(fill="lightblue",colour="black",size=1),legend.key.height=unit(1,"cm"),legend.key=element_rect(fill="white"),legend.text=element_text(face="bold",colour="black"),axis.title.x = element_text(face="bold", size=18),axis.title.y = element_text(face="bold", size=17),legend.title=element_text(face="bold",size=14),axis.text.x=element_text(colour="black",face="bold",size=5.5),axis.text.y=element_text(colour="black",face="bold",size=12),axis.line=element_line(colour="black"))
            
            feed=ggplot(cage,aes(x=Time,y=as.numeric(as.character(Feed)),colour=factor(Group,levels=unique(Group)),group=Group),environment=environment())+geom_line()+ggtitle(paste("Total Cumulative feeding",date1))+labs(x="Time",y="Feed(g)")+theme(axis.text.x=element_text(angle=90,hjust=1,size=6))+guides(colour=guide_legend("Group"))
            feed=feed+facet_grid(.~Date,scales="free",space="free")+scale_color_manual(values=c("#000000","#333333","#00FFFF","#3399CC","#666666","#999999","#0000FF","#000099"))+theme(panel.background=element_blank(),panel.grid.minor=element_blank(),axis.line=element_line())
            feed=feed+theme(plot.title = element_text(face="bold",size=18),strip.text=element_text(face="bold",size=rel(0.8)),strip.background=element_rect(fill="lightblue",colour="black",size=1),legend.key.height=unit(1,"cm"),legend.key=element_rect(fill="white"),legend.text=element_text(face="bold",colour="black"),axis.title.x = element_text(face="bold", size=18),axis.title.y = element_text(face="bold", size=17),legend.title=element_text(face="bold",size=14),axis.text.x=element_text(colour="black",face="bold",size=5.5),axis.text.y=element_text(colour="black",face="bold",size=12),axis.line=element_line(colour="black"))
            
            feed2=ggplot(cage,aes(x=Time,y=as.numeric(as.character(feedchange)),colour=factor(Group,levels=unique(Group)),group=Group),environment=environment())+geom_line()+ggtitle(paste("Total change of food-intake",date1))+labs(x="Time",y="Feed(g)")+theme(axis.text.x=element_text(angle=90,hjust=1,size=6))+guides(colour=guide_legend("Group"))
            feed2=feed2+facet_grid(.~Date,scales="free",space="free")+scale_color_manual(values=c("#000000","#333333","#00FFFF","#3399CC","#666666","#999999","#0000FF","#000099"))+theme(panel.background=element_blank(),panel.grid.minor=element_blank(),axis.line=element_line())+coord_cartesian(ylim=c(0,round(max(feedchange)+0.5)))+scale_y_continuous(breaks=seq(0,round(max(feedchange)+0.5),0.5))
            feed2=feed2+theme(plot.title = element_text(face="bold",size=18),strip.text=element_text(face="bold",size=rel(0.8)),strip.background=element_rect(fill="lightblue",colour="black",size=1),legend.key.height=unit(1,"cm"),legend.key=element_rect(fill="white"),legend.text=element_text(face="bold",colour="black"),axis.title.x = element_text(face="bold", size=18),axis.title.y = element_text(face="bold", size=17),legend.title=element_text(face="bold",size=14),axis.text.x=element_text(colour="black",face="bold",size=5.5),axis.text.y=element_text(colour="black",face="bold",size=12),axis.line=element_line(colour="black"))
            
          } else {
            rer=ggplot(cage,aes(x=Time,y=RER,colour=factor(Group,levels=unique(Group)),group=Group),environment=environment())+geom_line()+ggtitle(paste("Total RER",date1))+labs(x="Time",y="RER(VCO2/O2)")+theme(axis.text.x=element_text(angle=90,hjust=1,size=6))+guides(colour=guide_legend("Group"))
            rer=rer+facet_grid(.~Date,scales="free",space="free")+scale_color_hue()+theme(panel.background=element_blank(),panel.grid.minor=element_blank(),axis.line=element_line())
            rer=rer+theme(plot.title = element_text(face="bold",size=18),strip.text=element_text(face="bold",size=rel(0.8)),strip.background=element_rect(fill="lightblue",colour="black",size=1),legend.key.height=unit(1,"cm"),legend.key=element_rect(fill="white"),legend.text=element_text(face="bold",colour="black"),axis.title.x = element_text(face="bold", size=18),axis.title.y = element_text(face="bold", size=17),legend.title=element_text(face="bold",size=14),axis.text.x=element_text(colour="black",face="bold",size=5.5),axis.text.y=element_text(colour="black",face="bold",size=12),axis.line=element_line(colour="black"))
            
            pa=ggplot(cage,aes(x=Time,y=as.numeric(as.character(XT.YT)),colour=factor(Group,levels=unique(Group)),group=Group),environment=environment())+geom_line()+ggtitle(paste("Total Physical Activity",date1))+labs(x="Time",y="Physical Activity (Cnts)")+theme(axis.text.x=element_text(angle=90,hjust=1,size=6))+guides(colour=guide_legend("Group"))
            pa=pa+facet_grid(.~Date,scales="free",space="free")+scale_color_hue()+theme(panel.background=element_blank(),panel.grid.minor=element_blank(),axis.line=element_line())
            pa=pa+theme(plot.title = element_text(face="bold",size=18),strip.text=element_text(face="bold",size=rel(0.8)),strip.background=element_rect(fill="lightblue",colour="black",size=1),legend.key.height=unit(1,"cm"),legend.key=element_rect(fill="white"),legend.text=element_text(face="bold",colour="black"),axis.title.x = element_text(face="bold", size=18),axis.title.y = element_text(face="bold", size=17),legend.title=element_text(face="bold",size=14),axis.text.x=element_text(colour="black",face="bold",size=5.5),axis.text.y=element_text(colour="black",face="bold",size=12),axis.line=element_line(colour="black"))
            
            feed=ggplot(cage,aes(x=Time,y=as.numeric(as.character(Feed)),colour=factor(Group,levels=unique(Group)),group=Group),environment=environment())+geom_line()+ggtitle(paste("Total Cumulative feeding",date1))+labs(x="Time",y="Feed(g)")+theme(axis.text.x=element_text(angle=90,hjust=1,size=6))+guides(colour=guide_legend("Group"))
            feed=feed+facet_grid(.~Date,scales="free",space="free")+scale_color_hue()+theme(panel.background=element_blank(),panel.grid.minor=element_blank(),axis.line=element_line())
            feed=feed+theme(plot.title = element_text(face="bold",size=18),strip.text=element_text(face="bold",size=rel(0.8)),strip.background=element_rect(fill="lightblue",colour="black",size=1),legend.key.height=unit(1,"cm"),legend.key=element_rect(fill="white"),legend.text=element_text(face="bold",colour="black"),axis.title.x = element_text(face="bold", size=18),axis.title.y = element_text(face="bold", size=17),legend.title=element_text(face="bold",size=14),axis.text.x=element_text(colour="black",face="bold",size=5.5),axis.text.y=element_text(colour="black",face="bold",size=12),axis.line=element_line(colour="black"))
            
            feed2=ggplot(cage,aes(x=Time,y=as.numeric(as.character(feedchange)),colour=factor(Group,levels=unique(Group)),group=Group),environment=environment())+geom_line()+ggtitle(paste("Total change of food-intake",date1))+labs(x="Time",y="Feed(g)")+theme(axis.text.x=element_text(angle=90,hjust=1,size=6))+guides(colour=guide_legend("Group"))
            feed2=feed2+facet_grid(.~Date,scales="free",space="free")+scale_color_hue()+theme(panel.background=element_blank(),panel.grid.minor=element_blank(),axis.line=element_line())
            feed2=feed2+theme(plot.title = element_text(face="bold",size=18),strip.text=element_text(face="bold",size=rel(0.8)),strip.background=element_rect(fill="lightblue",colour="black",size=1),legend.key.height=unit(1,"cm"),legend.key=element_rect(fill="white"),legend.text=element_text(face="bold",colour="black"),axis.title.x = element_text(face="bold", size=18),axis.title.y = element_text(face="bold", size=17),legend.title=element_text(face="bold",size=14),axis.text.x=element_text(colour="black",face="bold",size=5.5),axis.text.y=element_text(colour="black",face="bold",size=12),axis.line=element_line(colour="black"))
          }
          
          rermin=floor(min(as.vector(Mean1$MRER-Mean1$SD1))*10)/10
          minlimit=rermin-0.02
          if (max(as.vector(Mean1$MRER+Mean1$SD1))>1) {if (max(as.vector(Mean1$MRER+Mean1$SD1))<=1.1) {rermax=1.1} else{rermax=round(max(as.vector(Mean1$MRER+Mean1$SD1)),2); if (rermax<=1.25) {rermax=1.25}; if (rermax>1.25 & rermax<=1.5) {rermax=1.5} }} else {rermax=1}
          if (max(as.vector(Mean1$MPA+Mean1$SD2))<=5000) {pamax=5000}
          if (max(as.vector(Mean1$MPA+Mean1$SD2))>5000 & max(as.vector(Mean1$MPA+Mean1$SD2))<=10000) {pamax=10000}
          if (max(as.vector(Mean1$MPA+Mean1$SD2))>10000 & max(as.vector(Mean1$MPA+Mean1$SD2))<=15000) {pamax=15000}
          if (max(as.vector(Mean1$MPA+Mean1$SD2))>15000 & max(as.vector(Mean1$MPA+Mean1$SD2))<=20000) {pamax=20000}
          if (max(as.vector(Mean1$MPA+Mean1$SD2))>20000 & max(as.vector(Mean1$MPA+Mean1$SD2))<=25000) {pamax=25000}
          if (max(as.vector(Mean1$MPA+Mean1$SD2))>25000 & max(as.vector(Mean1$MPA+Mean1$SD2))<=30000) {pamax=30000}
          if (max(as.vector(Mean1$MPA+Mean1$SD2))>30000) {pamax=round(max(as.vector(Mean1$MPA+Mean1$SD2))+5000,-4)}
          if (max(as.vector(Mean1$MF+Mean1$SD3))<=1) {fmax=1}
          if (max(as.vector(Mean1$MF+Mean1$SD3))>1 & max(as.vector(Mean1$MF+Mean1$SD3))<=2) {fmax=2}
          if (max(as.vector(Mean1$MF+Mean1$SD3))>2 & max(as.vector(Mean1$MF+Mean1$SD3))<=5) {fmax=5}
          if (max(as.vector(Mean1$MF+Mean1$SD3))>5 & max(as.vector(Mean1$MF+Mean1$SD3))<=10) {fmax=10}
          if (max(as.vector(Mean1$MF+Mean1$SD3))>10 & max(as.vector(Mean1$MF+Mean1$SD3))<=15) {fmax=15} 
          if (max(as.vector(Mean1$MF+Mean1$SD3))>15 & max(as.vector(Mean1$MF+Mean1$SD3))<=20) {fmax=20}
          if (max(as.vector(Mean1$MF+Mean1$SD3))>20) {fmax=round(max(as.vector(Mean1$MF+Mean1$SD3))+5,-1)}
          
          rectangles = data.frame(xmin = rect_left,xmax = rect_right,ymin = minlimit,ymax = rermax)
          rer1=ggplot(data=Mean1,aes(x=num,y=MRER,colour=typefac,group=type1),environment=environment())+geom_line(position=pd2,size=0.5)+geom_point(position=pd2,size=2)+geom_errorbar(aes(ymin=MRER-SD1,ymax=MRER+SD1),width=1.5,size=0.6,position=pd2)+ggtitle(paste("RER",date1))+labs(x="Time",y="RER(VCO2/O2)")+guides(colour=guide_legend("Treatment"))
          rer1=rer1+scale_colour_manual(values=c("#FF0000","#000000"))+coord_cartesian(xlim=c(0,max(num)+1),ylim=c(minlimit,rermax))+scale_x_discrete(breaks=c(num[1:max(num)],max(num)+1),labels=c(time1,""))+annotate("text",face="bold",size=7,label=P1,x=min(num)+length(time1)/12,y=rermax-0.025)+theme(plot.title = element_text(face="bold",size=20),legend.title=element_text(face="bold",size=14),legend.text=element_text(face="bold",size=12),legend.key.height=unit(1,"cm"),legend.key=element_rect(fill="white"),axis.title.x = element_text(face="bold", size=18),axis.title.y = element_text(face="bold", size=17),axis.text.x=element_text(angle=90,hjust=1,colour="black",face="bold",size=5.5),axis.text.y=element_text(colour="black",face="bold",size=12),panel.background=element_blank(),panel.background=element_blank(),panel.grid.minor=element_blank(),axis.line=element_line(),axis.line=element_line(colour="black"))
          rer2=ggplot(environment=environment())+geom_rect(data=rectangles, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),fill='gray80', alpha=0.8)+geom_line(data=Mean1,aes(x=num,y=MRER,colour=typefac,group=type1),position=pd2,size=0.6)+ggtitle(paste("RER",date1))+labs(x="Time",y="RER(VCO2/O2)")+guides(colour=guide_legend("Treatment"))
          rer2=rer2+scale_colour_manual(values=c("#FF0000","#000000"))+coord_cartesian(xlim=c(0,max(br)),ylim=c(minlimit,rermax))+scale_x_discrete(breaks=br,labels=timepoints)+annotate("text",face="bold",size=5.5,label=P1,x=min(num)+length(time1)/12,y=rermax-0.05)+theme(plot.title = element_text(face="bold",size=20),legend.title=element_text(face="bold",size=14),legend.text=element_text(face="bold",size=12),legend.key.height=unit(1,"cm"),legend.key=element_rect(fill="white"),axis.title.x = element_text(face="bold", size=18),axis.title.y = element_text(face="bold", size=17),axis.text.x=element_text(colour="black",face="bold",size=14),axis.text.y=element_text(colour="black",face="bold",size=14),panel.background=element_blank(),panel.background=element_blank(),panel.grid.minor=element_blank(),axis.line=element_line(colour="black"))
          rer3=ggplot(environment=environment())+geom_rect(data=rectangles, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),alpha=0.2)+geom_line(data=Mean1,aes(x=num,y=MRER,colour=typefac,group=type1),position=pd2,size=0.5)+geom_point(data=Mean1,aes(x=num,y=MRER,colour=typefac,group=type1),position=pd2,size=2)+geom_errorbar(data=Mean1,aes(x=num,y=MRER,colour=typefac,group=type1,ymin=MRER-SD1,ymax=MRER+SD1),width=1.5,size=0.6,position=pd2)+ggtitle(paste("RER",date1))+labs(x="Time",y="RER(VCO2/O2)")+guides(colour=guide_legend("Treatment"))
          rer3=rer3+scale_colour_manual(values=c("#FF0000","#000000"))+coord_cartesian(xlim=c(0,max(br)),ylim=c(minlimit,rermax))+scale_x_discrete(breaks=br,labels=timepoints)+annotate("text",face="bold",size=7,label=P1,x=min(num)+length(time1)/12,y=rermax-0.025)+theme(plot.title = element_text(face="bold",size=20),legend.title=element_text(face="bold",size=14),legend.text=element_text(face="bold",size=12),legend.key.height=unit(1,"cm"),legend.key=element_rect(fill="white"),axis.title.x = element_text(face="bold", size=18),axis.title.y = element_text(face="bold", size=17),axis.text.x=element_text(colour="black",face="bold",size=14),axis.text.y=element_text(colour="black",face="bold",size=14),panel.background=element_blank(),panel.background=element_blank(),panel.grid.minor=element_blank(),axis.line=element_line(colour="black"))
          
          
          rectangles2 = data.frame(xmin = rect_left,xmax = rect_right,ymin = 0,ymax = pamax)
          pa1=ggplot(data=Mean1,aes(x=num,y=MPA,colour=typefac,group=type1),environment=environment())+geom_line(position=pd2,size=0.5)+geom_point(position=pd2,size=2)+geom_errorbar(aes(ymin=MPA-SD2,ymax=MPA+SD2),width=1.5,size=0.6,position=pd2)+ggtitle(paste("Physical Activity",date1))+labs(x="Time",y="Physical Activity (Cnts)")+guides(colour=guide_legend("Treatment"))
          pa1=pa1+scale_colour_manual(values=c("#FF0000","#000000"))+coord_cartesian(xlim=c(0,max(num)+1),ylim=c(0,pamax))+scale_x_discrete(breaks=c(num[1:max(num)],max(num)+1),labels=c(time1,""))+annotate("text",face="bold",size=7,label=P2,x=min(num)+length(time1)/12,y=pamax-1000)+theme(plot.title = element_text(face="bold",size=20),legend.title=element_text(face="bold",size=14),legend.text=element_text(face="bold",size=12),legend.key.height=unit(1,"cm"),legend.key=element_rect(fill="white"),axis.title.x = element_text(face="bold", size=18),axis.title.y = element_text(face="bold", size=17),axis.text.x=element_text(angle=90,hjust=1,colour="black",face="bold",size=5.5),axis.text.y=element_text(colour="black",face="bold",size=14),panel.background=element_blank(),panel.background=element_blank(),panel.grid.minor=element_blank(),axis.line=element_line(colour="black"))
          pa2=ggplot(environment=environment())+geom_rect(data=rectangles2, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),fill='gray80', alpha=0.8)+geom_line(data=Mean1,aes(x=num,y=MPA,colour=typefac,group=type1),position=pd2,size=0.6)+ggtitle(paste("Physical Activity",date1))+labs(x="Time",y="Physical Activity (Cnts)")+guides(colour=guide_legend("Treatment"))
          pa2=pa2+scale_colour_manual(values=c("#FF0000","#000000"))+coord_cartesian(xlim=c(0,max(br)),ylim=c(0,pamax))+scale_x_discrete(breaks=br,labels=timepoints)+annotate("text",face="bold",size=5.5,label=P2,x=min(num)+length(time1)/12,y=pamax-1000)+theme(plot.title = element_text(face="bold",size=20),legend.title=element_text(face="bold",size=14),legend.text=element_text(face="bold",size=12),legend.key.height=unit(1,"cm"),legend.key=element_rect(fill="white"),axis.title.x = element_text(face="bold", size=18),axis.title.y = element_text(face="bold", size=17),axis.text.x=element_text(colour="black",face="bold",size=14),axis.text.y=element_text(colour="black",face="bold",size=14),panel.background=element_blank(),panel.background=element_blank(),panel.grid.minor=element_blank(),axis.line=element_line(colour="black"))
          pa3=ggplot(environment=environment())+geom_rect(data=rectangles2, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),alpha=0.2)+geom_line(data=Mean1,aes(x=num,y=MPA,colour=typefac,group=type1),position=pd2,size=0.5)+geom_point(data=Mean1,aes(x=num,y=MPA,colour=typefac,group=type1),position=pd2,size=2)+geom_errorbar(data=Mean1,aes(x=num,y=MPA,colour=typefac,group=type1,ymin=MPA-SD2,ymax=MPA+SD2),width=1.5,size=0.6,position=pd2)+ggtitle(paste("Physical Activity",date1))+labs(x="Time",y="Physical Activity (Cnts)")+guides(colour=guide_legend("Treatment"))
          pa3=pa3+scale_colour_manual(values=c("#FF0000","#000000"))+coord_cartesian(xlim=c(0,max(br)),ylim=c(0,pamax))+scale_x_discrete(breaks=br,labels=timepoints)+annotate("text",face="bold",size=7,label=P2,x=min(num)+length(time1)/12,y=pamax-1000)+theme(plot.title = element_text(face="bold",size=20),legend.title=element_text(face="bold",size=14),legend.text=element_text(face="bold",size=12),legend.key.height=unit(1,"cm"),legend.key=element_rect(fill="white"),axis.title.x = element_text(face="bold", size=18),axis.title.y = element_text(face="bold", size=17),axis.text.x=element_text(colour="black",face="bold",size=14),axis.text.y=element_text(colour="black",face="bold",size=14),panel.background=element_blank(),panel.background=element_blank(),panel.grid.minor=element_blank(),axis.line=element_line(colour="black"))
          
          
          rectangles3 = data.frame(xmin = rect_left,xmax = rect_right,ymin = 0,ymax = fmax)
          f1=ggplot(environment=environment())+geom_line(data=Mean1,aes(x=num,y=MF,colour=typefac,group=type1),position=pd2,size=0.5)+geom_point(data=Mean1,aes(x=num,y=MF,colour=typefac,group=type1),position=pd2,size=2)+geom_errorbar(data=Mean1,aes(x=num,y=MF,colour=typefac,group=type1,ymin=MF-SD3,ymax=MF+SD3),width=1.5,size=0.6,position=pd2)+ggtitle(paste("Cumulative feeding",date1))+labs(x="Time",y="Grams consumed")+guides(colour=guide_legend("Treatment"))
          f1=f1+scale_colour_manual(values=c("#FF0000","#000000"))+coord_cartesian(xlim=c(0,max(num)+1),ylim=c(0,fmax))+scale_x_discrete(breaks=c(num[1:max(num)],max(num)+1),labels=c(time1,""))+annotate("text",face="bold",size=7,label=P3,x=min(num)+length(time1)/12,y=fmax-1)+theme(plot.title = element_text(face="bold",size=20),legend.title=element_text(face="bold",size=14),legend.text=element_text(face="bold",size=12),legend.key.height=unit(1,"cm"),legend.key=element_rect(fill="white"),axis.title.x = element_text(face="bold", size=18),axis.title.y = element_text(face="bold", size=17),axis.text.x=element_text(angle=90,hjust=1,colour="black",face="bold",size=5.5),axis.text.y=element_text(colour="black",face="bold",size=12),panel.background=element_blank(),panel.background=element_blank(),panel.grid.minor=element_blank(),axis.line=element_line(colour="black"))
          f2=ggplot(environment=environment())+geom_rect(data=rectangles3, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),fill='gray80', alpha=0.8)+geom_line(data=Mean1,aes(x=num,y=MF,colour=typefac,group=type1),position=pd2,size=0.5)+geom_point(data=Mean1,aes(x=num,y=MF,colour=typefac,group=type1),position=pd2,size=2)+geom_errorbar(data=Mean1,aes(x=num,y=MF,colour=typefac,group=type1,ymin=MF-SD3,ymax=MF+SD3),width=1.5,size=0.6,position=pd2)+ggtitle(paste("Cumulative feeding",date1))+labs(x="Time",y="Grams consumed")+guides(colour=guide_legend("Treatment"))
          f2=f2+scale_colour_manual(values=c("#FF0000","#000000"))+coord_cartesian(xlim=c(0,max(num)+1),ylim=c(0,fmax))+scale_x_discrete(breaks=br,labels=timepoints)+annotate("text",face="bold",size=7,label=P3,x=min(num)+length(time1)/12,y=fmax-1)+theme(plot.title = element_text(face="bold",size=20),legend.title=element_text(face="bold",size=14),legend.text=element_text(face="bold",size=12),legend.key.height=unit(1,"cm"),legend.key=element_rect(fill="white"),axis.title.x = element_text(face="bold", size=18),axis.title.y = element_text(face="bold", size=17),axis.text.x=element_text(colour="black",face="bold",size=14),axis.text.y=element_text(colour="black",face="bold",size=14),panel.background=element_blank(),panel.background=element_blank(),panel.grid.minor=element_blank(),axis.line=element_line(colour="black"))
          
          f3=ggplot(environment=environment())+geom_rect(data=rectangles2, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),fill='gray80', alpha=0.8)+geom_line(data=Mean1,aes(x=num,y=MFC,colour=typefac,group=type1),position=pd2,size=0.6)+ggtitle(paste("Change of food-intake",date1))+labs(x="Time",y="Food-intake (g)")+guides(colour=guide_legend("Treatment"))
          f3=f3+scale_colour_manual(values=c("#FF0000","#000000"))+coord_cartesian(xlim=c(0,max(num)+1),ylim=c(0,round(max(Mean1$MFC)+0.1,1)))+scale_x_discrete(breaks=br,labels=timepoints)+scale_y_continuous(breaks=seq(0,round(max(Mean1$MFC)+0.1,1),0.1))+theme(plot.title = element_text(face="bold",size=20),legend.title=element_text(face="bold",size=14),legend.text=element_text(face="bold",size=12),legend.key.height=unit(1,"cm"),legend.key=element_rect(fill="white"),axis.title.x = element_text(face="bold", size=18),axis.title.y = element_text(face="bold", size=17),axis.text.x=element_text(colour="black",face="bold",size=14),axis.text.y=element_text(colour="black",face="bold",size=14),panel.background=element_blank(),panel.background=element_blank(),panel.grid.minor=element_blank(),axis.line=element_line(colour="black"))
          
        } else {
          rer=ggplot(cage,aes(x=Time,y=RER,colour=factor(Group,levels=unique(Group)),group=Group),environment=environment())+geom_line()+ggtitle(paste("Total RER",date1))+labs(x="Time",y="RER(VCO2/O2)")+theme(axis.text.x=element_text(angle=90,hjust=1,size=6))+guides(colour=guide_legend("Group"))
          rer=rer+facet_grid(.~Date,scales="free",space="free")+scale_color_hue()+theme(panel.background=element_blank(),panel.grid.minor=element_blank(),axis.line=element_line())
          rer=rer+theme(plot.title = element_text(face="bold",size=18),strip.text=element_text(face="bold",size=rel(0.8)),strip.background=element_rect(fill="lightblue",colour="black",size=1),legend.key.height=unit(1,"cm"),legend.key=element_rect(fill="white"),legend.text=element_text(face="bold",colour="black"),axis.title.x = element_text(face="bold", size=18),axis.title.y = element_text(face="bold", size=17),legend.title=element_text(face="bold",size=14),axis.text.x=element_text(colour="black",face="bold",size=5.5),axis.text.y=element_text(colour="black",face="bold",size=12),axis.line=element_line(colour="black"))
          
          pa=ggplot(cage,aes(x=Time,y=as.numeric(as.character(XT.YT)),colour=factor(Group,levels=unique(Group)),group=Group),environment=environment())+geom_line()+ggtitle(paste("Total Physical Activity",date1))+labs(x="Time",y="Physical Activity (Cnts)")+theme(axis.text.x=element_text(angle=90,hjust=1,size=6))+guides(colour=guide_legend("Group"))
          pa=pa+facet_grid(.~Date,scales="free",space="free")+scale_color_hue()+theme(panel.background=element_blank(),panel.grid.minor=element_blank(),axis.line=element_line())
          pa=pa+theme(plot.title = element_text(face="bold",size=18),strip.text=element_text(face="bold",size=rel(0.8)),strip.background=element_rect(fill="lightblue",colour="black",size=1),legend.key.height=unit(1,"cm"),legend.key=element_rect(fill="white"),legend.text=element_text(face="bold",colour="black"),axis.title.x = element_text(face="bold", size=18),axis.title.y = element_text(face="bold", size=17),legend.title=element_text(face="bold",size=14),axis.text.x=element_text(colour="black",face="bold",size=5.5),axis.text.y=element_text(colour="black",face="bold",size=12),axis.line=element_line(colour="black"))
          
          feed=ggplot(cage,aes(x=Time,y=as.numeric(as.character(Feed)),colour=factor(Group,levels=unique(Group)),group=Group),environment=environment())+geom_line()+ggtitle(paste("Total Cumulative feeding",date1))+labs(x="Time",y="Feed(g)")+theme(axis.text.x=element_text(angle=90,hjust=1,size=6))+guides(colour=guide_legend("Group"))
          feed=feed+facet_grid(.~Date,scales="free",space="free")+scale_color_hue()+theme(panel.background=element_blank(),panel.grid.minor=element_blank(),axis.line=element_line())
          feed=feed+theme(plot.title = element_text(face="bold",size=18),strip.text=element_text(face="bold",size=rel(0.8)),strip.background=element_rect(fill="lightblue",colour="black",size=1),legend.key.height=unit(1,"cm"),legend.key=element_rect(fill="white"),legend.text=element_text(face="bold",colour="black"),axis.title.x = element_text(face="bold", size=18),axis.title.y = element_text(face="bold", size=17),legend.title=element_text(face="bold",size=14),axis.text.x=element_text(colour="black",face="bold",size=5.5),axis.text.y=element_text(colour="black",face="bold",size=12),axis.line=element_line(colour="black"))
          
          feed2=ggplot(cage,aes(x=Time,y=as.numeric(as.character(feedchange)),colour=factor(Group,levels=unique(Group)),group=Group),environment=environment())+geom_line()+ggtitle(paste("Total Cumulative feeding",date1))+labs(x="Time",y="Feed(g)")+theme(axis.text.x=element_text(angle=90,hjust=1,size=6))+guides(colour=guide_legend("Group"))
          feed2=feed2+facet_grid(.~Date,scales="free",space="free")+scale_color_hue()+theme(panel.background=element_blank(),panel.grid.minor=element_blank(),axis.line=element_line())
          feed2=feed2+theme(plot.title = element_text(face="bold",size=18),strip.text=element_text(face="bold",size=rel(0.8)),strip.background=element_rect(fill="lightblue",colour="black",size=1),legend.key.height=unit(1,"cm"),legend.key=element_rect(fill="white"),legend.text=element_text(face="bold",colour="black"),axis.title.x = element_text(face="bold", size=18),axis.title.y = element_text(face="bold", size=17),legend.title=element_text(face="bold",size=14),axis.text.x=element_text(colour="black",face="bold",size=5.5),axis.text.y=element_text(colour="black",face="bold",size=12),axis.line=element_line(colour="black"))
          
          rermin=floor(min(as.vector(Mean1$MRER-Mean1$SD1))*10)/10
          minlimit=rermin-0.02
          if (max(as.vector(Mean1$MRER+Mean1$SD1))>1) {if (max(as.vector(Mean1$MRER+Mean1$SD1))<=1.1) {rermax=1.1} else{rermax=round(max(as.vector(Mean1$MRER+Mean1$SD1)),2); if (rermax<=1.25) {rermax=1.25}; if (rermax>1.25 & rermax<=1.5) {rermax=1.5} }} else {rermax=1}
          if (max(as.vector(Mean1$MPA+Mean1$SD2))<=5000) {pamax=5000}
          if (max(as.vector(Mean1$MPA+Mean1$SD2))>5000 & max(as.vector(Mean1$MPA+Mean1$SD2))<=10000) {pamax=10000}
          if (max(as.vector(Mean1$MPA+Mean1$SD2))>10000 & max(as.vector(Mean1$MPA+Mean1$SD2))<=15000) {pamax=15000}
          if (max(as.vector(Mean1$MPA+Mean1$SD2))>15000 & max(as.vector(Mean1$MPA+Mean1$SD2))<=20000) {pamax=20000}
          if (max(as.vector(Mean1$MPA+Mean1$SD2))>20000 & max(as.vector(Mean1$MPA+Mean1$SD2))<=25000) {pamax=25000}
          if (max(as.vector(Mean1$MPA+Mean1$SD2))>25000 & max(as.vector(Mean1$MPA+Mean1$SD2))<=30000) {pamax=30000}
          if (max(as.vector(Mean1$MPA+Mean1$SD2))>30000) {pamax=round(max(as.vector(Mean1$MPA+Mean1$SD2))+5000,-4)}
          if (max(as.vector(Mean1$MF+Mean1$SD3))<=1) {fmax=1}
          if (max(as.vector(Mean1$MF+Mean1$SD3))>1 & max(as.vector(Mean1$MF+Mean1$SD3))<=2) {fmax=2}
          if (max(as.vector(Mean1$MF+Mean1$SD3))>2 & max(as.vector(Mean1$MF+Mean1$SD3))<=5) {fmax=5}
          if (max(as.vector(Mean1$MF+Mean1$SD3))>5 & max(as.vector(Mean1$MF+Mean1$SD3))<=10) {fmax=10} 
          if (max(as.vector(Mean1$MF+Mean1$SD3))>10 & max(as.vector(Mean1$MF+Mean1$SD3))<=15) {fmax=15} 
          if (max(as.vector(Mean1$MF+Mean1$SD3))>15 & max(as.vector(Mean1$MF+Mean1$SD3))<=20) {fmax=20}
          if (max(as.vector(Mean1$MF+Mean1$SD3))>20) {fmax=round(max(as.vector(Mean1$MF+Mean1$SD3))+5,-1)}
          
          rectangles = data.frame(xmin = rect_left,xmax = rect_right,ymin = minlimit,ymax = rermax)
          rer1=ggplot(data=Mean1,aes(x=num,y=MRER,colour=typefac,group=type1),environment=environment())+geom_line(position=pd2,size=0.5)+geom_point(position=pd2,size=2)+geom_errorbar(aes(ymin=MRER-SD1,ymax=MRER+SD1),width=1.5,size=0.6,position=pd2)+ggtitle(paste("RER",date1))+labs(x="Time",y="RER(VCO2/O2)")+guides(colour=guide_legend("Treatment"))
          rer1=rer1+scale_color_hue()+coord_cartesian(xlim=c(0,max(num)+1),ylim=c(minlimit,rermax))+scale_x_discrete(breaks=c(num[1:max(num)],max(num)+1),labels=c(time1,""))+annotate("text",face="bold",size=7,label=P1,x=min(num)+length(time1)/12,y=rermax-0.025)+theme(plot.title = element_text(face="bold",size=20),legend.title=element_text(face="bold",size=14),legend.text=element_text(face="bold",size=12),legend.key.height=unit(1,"cm"),legend.key=element_rect(fill="white"),axis.title.x = element_text(face="bold", size=18),axis.title.y = element_text(face="bold", size=17),axis.text.x=element_text(angle=90,hjust=1,colour="black",face="bold",size=5.5),axis.text.y=element_text(colour="black",face="bold",size=12),panel.background=element_blank(),panel.background=element_blank(),panel.grid.minor=element_blank(),axis.line=element_line(),axis.line=element_line(colour="black"))
          rer2=ggplot(environment=environment())+geom_rect(data=rectangles, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),fill='gray80', alpha=0.8)+geom_line(data=Mean1,aes(x=num,y=MRER,colour=typefac,group=type1),position=pd2,size=0.6)+ggtitle(paste("RER",date1))+labs(x="Time",y="RER(VCO2/O2)")+guides(colour=guide_legend("Treatment"))
          rer2=rer2+scale_color_hue()+coord_cartesian(xlim=c(0,max(br)),ylim=c(minlimit,rermax))+scale_x_discrete(breaks=br,labels=timepoints)+annotate("text",face="bold",size=5.5,label=P1,x=min(num)+length(time1)/12,y=rermax-0.05)+theme(plot.title = element_text(face="bold",size=20),legend.title=element_text(face="bold",size=14),legend.text=element_text(face="bold",size=12),legend.key.height=unit(1,"cm"),legend.key=element_rect(fill="white"),axis.title.x = element_text(face="bold", size=18),axis.title.y = element_text(face="bold", size=17),axis.text.x=element_text(colour="black",face="bold",size=14),axis.text.y=element_text(colour="black",face="bold",size=14),panel.background=element_blank(),panel.background=element_blank(),panel.grid.minor=element_blank(),axis.line=element_line(colour="black"))
          rer3=ggplot(environment=environment())+geom_rect(data=rectangles, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),alpha=0.2)+geom_line(data=Mean1,aes(x=num,y=MRER,colour=typefac,group=type1),position=pd2,size=0.5)+geom_point(data=Mean1,aes(x=num,y=MRER,colour=typefac,group=type1),position=pd2,size=2)+geom_errorbar(data=Mean1,aes(x=num,y=MRER,colour=typefac,group=type1,ymin=MRER-SD1,ymax=MRER+SD1),width=1.5,size=0.6,position=pd2)+ggtitle(paste("RER",date1))+labs(x="Time",y="RER(VCO2/O2)")+guides(colour=guide_legend("Treatment"))
          rer3=rer3+scale_color_hue()+coord_cartesian(xlim=c(0,max(br)),ylim=c(minlimit,rermax))+scale_x_discrete(breaks=br,labels=timepoints)+annotate("text",face="bold",size=7,label=P1,x=min(num)+length(time1)/12,y=rermax-0.025)+theme(plot.title = element_text(face="bold",size=20),legend.title=element_text(face="bold",size=14),legend.text=element_text(face="bold",size=12),legend.key.height=unit(1,"cm"),legend.key=element_rect(fill="white"),axis.title.x = element_text(face="bold", size=18),axis.title.y = element_text(face="bold", size=17),axis.text.x=element_text(colour="black",face="bold",size=14),axis.text.y=element_text(colour="black",face="bold",size=14),panel.background=element_blank(),panel.background=element_blank(),panel.grid.minor=element_blank(),axis.line=element_line(colour="black"))
          
          
          rectangles2 = data.frame(xmin = rect_left,xmax = rect_right,ymin = 0,ymax = pamax)
          pa1=ggplot(data=Mean1,aes(x=num,y=MPA,colour=typefac,group=type1),environment=environment())+geom_line(position=pd2,size=0.5)+geom_point(position=pd2,size=2)+geom_errorbar(aes(ymin=MPA-SD2,ymax=MPA+SD2),width=1.5,size=0.6,position=pd2)+ggtitle(paste("Physical Activity",date1))+labs(x="Time",y="Physical Activity (Cnts)")+guides(colour=guide_legend("Treatment"))
          pa1=pa1+scale_color_hue()+coord_cartesian(xlim=c(0,max(num)+1),ylim=c(0,pamax))+scale_x_discrete(breaks=c(num[1:max(num)],max(num)+1),labels=c(time1,""))+annotate("text",face="bold",size=7,label=P2,x=min(num)+length(time1)/12,y=pamax-1000)+theme(plot.title = element_text(face="bold",size=20),legend.title=element_text(face="bold",size=14),legend.text=element_text(face="bold",size=12),legend.key.height=unit(1,"cm"),legend.key=element_rect(fill="white"),axis.title.x = element_text(face="bold", size=18),axis.title.y = element_text(face="bold", size=17),axis.text.x=element_text(angle=90,hjust=1,colour="black",face="bold",size=5.5),axis.text.y=element_text(colour="black",face="bold",size=14),panel.background=element_blank(),panel.background=element_blank(),panel.grid.minor=element_blank(),axis.line=element_line(colour="black"))
          pa2=ggplot(environment=environment())+geom_rect(data=rectangles2, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),fill='gray80', alpha=0.8)+geom_line(data=Mean1,aes(x=num,y=MPA,colour=typefac,group=type1),position=pd2,size=0.6)+ggtitle(paste("Physical Activity",date1))+labs(x="Time",y="Physical Activity (Cnts)")+guides(colour=guide_legend("Treatment"))
          pa2=pa2+scale_color_hue()+coord_cartesian(xlim=c(0,max(br)),ylim=c(0,pamax))+scale_x_discrete(breaks=br,labels=timepoints)+annotate("text",face="bold",size=5.5,label=P2,x=min(num)+length(time1)/12,y=pamax-1000)+theme(plot.title = element_text(face="bold",size=20),legend.title=element_text(face="bold",size=14),legend.text=element_text(face="bold",size=12),legend.key.height=unit(1,"cm"),legend.key=element_rect(fill="white"),axis.title.x = element_text(face="bold", size=18),axis.title.y = element_text(face="bold", size=17),axis.text.x=element_text(colour="black",face="bold",size=14),axis.text.y=element_text(colour="black",face="bold",size=14),panel.background=element_blank(),panel.background=element_blank(),panel.grid.minor=element_blank(),axis.line=element_line(colour="black"))
          pa3=ggplot(environment=environment())+geom_rect(data=rectangles2, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),alpha=0.2)+geom_line(data=Mean1,aes(x=num,y=MPA,colour=typefac,group=type1),position=pd2,size=0.5)+geom_point(data=Mean1,aes(x=num,y=MPA,colour=typefac,group=type1),position=pd2,size=2)+geom_errorbar(data=Mean1,aes(x=num,y=MPA,colour=typefac,group=type1,ymin=MPA-SD2,ymax=MPA+SD2),width=1.5,size=0.6,position=pd2)+ggtitle(paste("Physical Activity",date1))+labs(x="Time",y="Physical Activity (Cnts)")+guides(colour=guide_legend("Treatment"))
          pa3=pa3+scale_color_hue()+coord_cartesian(xlim=c(0,max(br)),ylim=c(0,pamax))+scale_x_discrete(breaks=br,labels=timepoints)+annotate("text",face="bold",size=7,label=P2,x=min(num)+length(time1)/12,y=pamax-1000)+theme(plot.title = element_text(face="bold",size=20),legend.title=element_text(face="bold",size=14),legend.text=element_text(face="bold",size=12),legend.key.height=unit(1,"cm"),legend.key=element_rect(fill="white"),axis.title.x = element_text(face="bold", size=18),axis.title.y = element_text(face="bold", size=17),axis.text.x=element_text(colour="black",face="bold",size=14),axis.text.y=element_text(colour="black",face="bold",size=14),panel.background=element_blank(),panel.background=element_blank(),panel.grid.minor=element_blank(),axis.line=element_line(colour="black"))
          
          
          rectangles3 = data.frame(xmin = rect_left,xmax = rect_right,ymin = 0,ymax = fmax)
          f1=ggplot(environment=environment())+geom_line(data=Mean1,aes(x=num,y=MF,colour=typefac,group=type1),position=pd2,size=0.5)+geom_point(data=Mean1,aes(x=num,y=MF,colour=typefac,group=type1),position=pd2,size=2)+geom_errorbar(data=Mean1,aes(x=num,y=MF,colour=typefac,group=type1,ymin=MF-SD3,ymax=MF+SD3),width=1.5,size=0.6,position=pd2)+ggtitle(paste("Cumulative feeding",date1))+labs(x="Time",y="Grams consumed")+guides(colour=guide_legend("Treatment"))
          f1=f1+scale_color_hue()+coord_cartesian(xlim=c(0,max(num)+1),ylim=c(0,fmax))+scale_x_discrete(breaks=c(num[1:max(num)],max(num)+1),labels=c(time1,""))+annotate("text",face="bold",size=7,label=P3,x=min(num)+length(time1)/12,y=fmax-1)+theme(plot.title = element_text(face="bold",size=20),legend.title=element_text(face="bold",size=14),legend.text=element_text(face="bold",size=12),legend.key.height=unit(1,"cm"),legend.key=element_rect(fill="white"),axis.title.x = element_text(face="bold", size=18),axis.title.y = element_text(face="bold", size=17),axis.text.x=element_text(angle=90,hjust=1,colour="black",face="bold",size=5.5),axis.text.y=element_text(colour="black",face="bold",size=12),panel.background=element_blank(),panel.background=element_blank(),panel.grid.minor=element_blank(),axis.line=element_line(colour="black"))
          f2=ggplot(environment=environment())+geom_rect(data=rectangles3, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),fill='gray80', alpha=0.8)+geom_line(data=Mean1,aes(x=num,y=MF,colour=typefac,group=type1),position=pd2,size=0.5)+geom_point(data=Mean1,aes(x=num,y=MF,colour=typefac,group=type1),position=pd2,size=2)+geom_errorbar(data=Mean1,aes(x=num,y=MF,colour=typefac,group=type1,ymin=MF-SD3,ymax=MF+SD3),width=1.5,size=0.6,position=pd2)+ggtitle(paste("Cumulative feeding",date1))+labs(x="Time",y="Grams consumed")+guides(colour=guide_legend("Treatment"))
          f2=f2+scale_color_hue()+coord_cartesian(xlim=c(0,max(num)+1),ylim=c(0,fmax))+scale_x_discrete(breaks=br,labels=timepoints)+annotate("text",face="bold",size=7,label=P3,x=min(num)+length(time1)/12,y=fmax-1)+theme(plot.title = element_text(face="bold",size=20),legend.title=element_text(face="bold",size=14),legend.text=element_text(face="bold",size=12),legend.key.height=unit(1,"cm"),legend.key=element_rect(fill="white"),axis.title.x = element_text(face="bold", size=18),axis.title.y = element_text(face="bold", size=17),axis.text.x=element_text(colour="black",face="bold",size=14),axis.text.y=element_text(colour="black",face="bold",size=14),panel.background=element_blank(),panel.background=element_blank(),panel.grid.minor=element_blank(),axis.line=element_line(colour="black"))
          
          f3=ggplot(environment=environment())+geom_rect(data=rectangles2, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),fill='gray80', alpha=0.8)+geom_line(data=Mean1,aes(x=num,y=MFC,colour=typefac,group=type1),position=pd2,size=0.6)+ggtitle(paste("Change of food-intake",date1))+labs(x="Time",y="Food-intake (g)")+guides(colour=guide_legend("Treatment"))
          f3=f3+scale_color_hue()+coord_cartesian(xlim=c(0,max(num)+1),ylim=c(0,round(max(Mean1$MFC)+0.1,1)))+scale_x_discrete(breaks=br,labels=timepoints)+scale_y_continuous(breaks=seq(0,round(max(Mean1$MFC)+0.1,1),0.1))+theme(plot.title = element_text(face="bold",size=20),legend.title=element_text(face="bold",size=14),legend.text=element_text(face="bold",size=12),legend.key.height=unit(1,"cm"),legend.key=element_rect(fill="white"),axis.title.x = element_text(face="bold", size=18),axis.title.y = element_text(face="bold", size=17),axis.text.x=element_text(colour="black",face="bold",size=14),axis.text.y=element_text(colour="black",face="bold",size=14),panel.background=element_blank(),panel.background=element_blank(),panel.grid.minor=element_blank(),axis.line=element_line(colour="black"))
          
        }} else {
          rer=ggplot(cage,aes(x=Time,y=RER,colour=factor(Group,levels=unique(Group)),group=Group),environment=environment())+geom_line()+ggtitle(paste("Total RER",date1))+labs(x="Time",y="RER(VCO2/O2)")+theme(axis.text.x=element_text(angle=90,hjust=1,size=6))+guides(colour=guide_legend("Group"))
          rer=rer+facet_grid(.~Date,scales="free",space="free")+scale_color_hue()+theme(panel.background=element_blank(),panel.grid.minor=element_blank(),axis.line=element_line())
          rer=rer+theme(plot.title = element_text(face="bold",size=18),strip.text=element_text(face="bold",size=rel(0.8)),strip.background=element_rect(fill="lightblue",colour="black",size=1),legend.key.height=unit(1,"cm"),legend.key=element_rect(fill="white"),legend.text=element_text(face="bold",colour="black"),axis.title.x = element_text(face="bold", size=18),axis.title.y = element_text(face="bold", size=17),legend.title=element_text(face="bold",size=14),axis.text.x=element_text(colour="black",face="bold",size=5.5),axis.text.y=element_text(colour="black",face="bold",size=12),axis.line=element_line(colour="black"))
          
          pa=ggplot(cage,aes(x=Time,y=as.numeric(as.character(XT.YT)),colour=factor(Group,levels=unique(Group)),group=Group),environment=environment())+geom_line()+ggtitle(paste("Total Physical Activity",date1))+labs(x="Time",y="Physical Activity (Cnts)")+theme(axis.text.x=element_text(angle=90,hjust=1,size=6))+guides(colour=guide_legend("Group"))
          pa=pa+facet_grid(.~Date,scales="free",space="free")+scale_color_hue()+theme(panel.background=element_blank(),panel.grid.minor=element_blank(),axis.line=element_line())
          pa=pa+theme(plot.title = element_text(face="bold",size=18),strip.text=element_text(face="bold",size=rel(0.8)),strip.background=element_rect(fill="lightblue",colour="black",size=1),legend.key.height=unit(1,"cm"),legend.key=element_rect(fill="white"),legend.text=element_text(face="bold",colour="black"),axis.title.x = element_text(face="bold", size=18),axis.title.y = element_text(face="bold", size=17),legend.title=element_text(face="bold",size=14),axis.text.x=element_text(colour="black",face="bold",size=5.5),axis.text.y=element_text(colour="black",face="bold",size=12),axis.line=element_line(colour="black"))
          
          feed=ggplot(cage,aes(x=Time,y=as.numeric(as.character(Feed)),colour=factor(Group,levels=unique(Group)),group=Group),environment=environment())+geom_line()+ggtitle(paste("Total Cumulative feeding",date1))+labs(x="Time",y="Feed(g)")+theme(axis.text.x=element_text(angle=90,hjust=1,size=6))+guides(colour=guide_legend("Group"))
          feed=feed+facet_grid(.~Date,scales="free",space="free")+scale_color_hue()+theme(panel.background=element_blank(),panel.grid.minor=element_blank(),axis.line=element_line())
          feed=feed+theme(plot.title = element_text(face="bold",size=18),strip.text=element_text(face="bold",size=rel(0.8)),strip.background=element_rect(fill="lightblue",colour="black",size=1),legend.key.height=unit(1,"cm"),legend.key=element_rect(fill="white"),legend.text=element_text(face="bold",colour="black"),axis.title.x = element_text(face="bold", size=18),axis.title.y = element_text(face="bold", size=17),legend.title=element_text(face="bold",size=14),axis.text.x=element_text(colour="black",face="bold",size=5.5),axis.text.y=element_text(colour="black",face="bold",size=12),axis.line=element_line(colour="black"))
          
          feed2=ggplot(cage,aes(x=Time,y=as.numeric(as.character(feedchange)),colour=factor(Group,levels=unique(Group)),group=Group),environment=environment())+geom_line()+ggtitle(paste("Total Cumulative feeding",date1))+labs(x="Time",y="Feed(g)")+theme(axis.text.x=element_text(angle=90,hjust=1,size=6))+guides(colour=guide_legend("Group"))
          feed2=feed2+facet_grid(.~Date,scales="free",space="free")+scale_color_hue()+theme(panel.background=element_blank(),panel.grid.minor=element_blank(),axis.line=element_line())
          feed2=feed2+theme(plot.title = element_text(face="bold",size=18),strip.text=element_text(face="bold",size=rel(0.8)),strip.background=element_rect(fill="lightblue",colour="black",size=1),legend.key.height=unit(1,"cm"),legend.key=element_rect(fill="white"),legend.text=element_text(face="bold",colour="black"),axis.title.x = element_text(face="bold", size=18),axis.title.y = element_text(face="bold", size=17),legend.title=element_text(face="bold",size=14),axis.text.x=element_text(colour="black",face="bold",size=5.5),axis.text.y=element_text(colour="black",face="bold",size=12),axis.line=element_line(colour="black"))
          
          
          rermin=floor(min(as.vector(Mean1$MRER))*10)/10
          minlimit=rermin-0.02
          if (max(as.vector(Mean1$MRER))>1) {if (max(as.vector(Mean1$MRER))<=1.1) {rermax=1.1} else{rermax=round(max(as.vector(Mean1$MRER)),2); if (rermax<=1.25) {rermax=1.25}; if (rermax>1.25 & rermax<=1.5) {rermax=1.5} }} else {rermax=1}
          if (max(as.vector(Mean1$MPA))<=5000) {pamax=5000}
          if (max(as.vector(Mean1$MPA))>5000 & max(as.vector(Mean1$MPA))<=10000) {pamax=10000}
          if (max(as.vector(Mean1$MPA))>10000 & max(as.vector(Mean1$MPA))<=15000) {pamax=15000}
          if (max(as.vector(Mean1$MPA))>15000 & max(as.vector(Mean1$MPA))<=20000) {pamax=20000}
          if (max(as.vector(Mean1$MPA))>20000 & max(as.vector(Mean1$MPA))<=25000) {pamax=25000}
          if (max(as.vector(Mean1$MPA))>25000 & max(as.vector(Mean1$MPA))<=30000) {pamax=30000}
          if (max(as.vector(Mean1$MPA))>30000) {pamax=round(max(as.vector(Mean1$MPA))+5000,-4)}
          if (max(as.vector(Mean1$MF))<=1) {fmax=1}
          if (max(as.vector(Mean1$MF))>1 & max(as.vector(Mean1$MF))<=2) {fmax=2}
          if (max(as.vector(Mean1$MF))>2 & max(as.vector(Mean1$MF))<=5) {fmax=5}
          if (max(as.vector(Mean1$MF))>5 & max(as.vector(Mean1$MF))<=10) {fmax=10}
          if (max(as.vector(Mean1$MF))>10 & max(as.vector(Mean1$MF))<=15) {fmax=15} 
          if (max(as.vector(Mean1$MF))>15 & max(as.vector(Mean1$MF))<=20) {fmax=20}
          if (max(as.vector(Mean1$MF))>20) {fmax=round(max(as.vector(Mean1$MF))+5,-1)}
          
          rectangles = data.frame(xmin = rect_left,xmax = rect_right,ymin = minlimit,ymax = rermax)
          rer1=ggplot(data=Mean1,aes(x=num,y=MRER,colour=typefac,group=type1),environment=environment())+geom_line(position=pd2,size=0.5)+geom_point(position=pd2,size=2)+ggtitle(paste("RER",date1))+labs(x="Time",y="RER(VCO2/O2)")+guides(colour=guide_legend("Treatment"))
          rer1=rer1+coord_cartesian(xlim=c(0,max(num)+1),ylim=c(minlimit,rermax))+scale_x_discrete(breaks=c(num[1:max(num)],max(num)+1),labels=c(time1,""))+theme(plot.title = element_text(face="bold",size=20),legend.title=element_text(face="bold",size=14),legend.text=element_text(face="bold",size=12),legend.key.height=unit(1,"cm"),legend.key=element_rect(fill="white"),axis.title.x = element_text(face="bold", size=18),axis.title.y = element_text(face="bold", size=17),axis.text.x=element_text(angle=90,hjust=1,colour="black",face="bold",size=5.5),axis.text.y=element_text(colour="black",face="bold",size=12),panel.background=element_blank(),panel.background=element_blank(),panel.grid.minor=element_blank(),axis.line=element_line(),axis.line=element_line(colour="black"))
          rer2=ggplot()+geom_rect(data=rectangles, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),fill='gray80', alpha=0.8)+geom_line(data=Mean1,aes(x=num,y=MRER,colour=typefac,group=type1),position=pd2,size=0.6)+ggtitle(paste("RER",date1))+labs(x="Time",y="RER(VCO2/O2)")+guides(colour=guide_legend("Treatment"))
          rer2=rer2+coord_cartesian(xlim=c(0,max(br)),ylim=c(minlimit,rermax))+scale_x_discrete(breaks=br,labels=timepoints)+theme(plot.title = element_text(face="bold",size=20),legend.title=element_text(face="bold",size=14),legend.text=element_text(face="bold",size=12),legend.key.height=unit(1,"cm"),legend.key=element_rect(fill="white"),axis.title.x = element_text(face="bold", size=18),axis.title.y = element_text(face="bold", size=17),axis.text.x=element_text(colour="black",face="bold",size=14),axis.text.y=element_text(colour="black",face="bold",size=14),panel.background=element_blank(),panel.background=element_blank(),panel.grid.minor=element_blank(),axis.line=element_line(colour="black"))
          rer3=ggplot(environment=environment())+geom_rect(data=rectangles, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),alpha=0.2)+geom_line(data=Mean1,aes(x=num,y=MRER,colour=typefac,group=type1),position=pd2,size=0.5)+geom_point(data=Mean1,aes(x=num,y=MRER,colour=typefac,group=type1),position=pd2,size=2)+ggtitle(paste("RER",date1))+labs(x="Time",y="RER(VCO2/O2)")+guides(colour=guide_legend("Treatment"))
          rer3=rer3+coord_cartesian(xlim=c(0,max(br)),ylim=c(minlimit,rermax))+scale_x_discrete(breaks=br,labels=timepoints)+theme(plot.title = element_text(face="bold",size=20),legend.title=element_text(face="bold",size=14),legend.text=element_text(face="bold",size=12),legend.key.height=unit(1,"cm"),legend.key=element_rect(fill="white"),axis.title.x = element_text(face="bold", size=18),axis.title.y = element_text(face="bold", size=17),axis.text.x=element_text(colour="black",face="bold",size=14),axis.text.y=element_text(colour="black",face="bold",size=14),panel.background=element_blank(),panel.background=element_blank(),panel.grid.minor=element_blank(),axis.line=element_line(colour="black"))
          
          
          rectangles2 = data.frame(xmin = rect_left,xmax = rect_right,ymin = 0,ymax = pamax)
          pa1=ggplot(data=Mean1,aes(x=num,y=MPA,colour=typefac,group=type1),environment=environment())+geom_line(position=pd2,size=0.5)+geom_point(position=pd2,size=2)+ggtitle(paste("Physical Activity",date1))+labs(x="Time",y="Physical Activity (Cnts)")+guides(colour=guide_legend("Treatment"))
          pa1=pa1+coord_cartesian(xlim=c(0,max(num)+1),ylim=c(0,pamax))+scale_x_discrete(breaks=c(num[1:max(num)],max(num)+1),labels=c(time1,""))+theme(plot.title = element_text(face="bold",size=20),legend.title=element_text(face="bold",size=14),legend.text=element_text(face="bold",size=12),legend.key.height=unit(1,"cm"),legend.key=element_rect(fill="white"),axis.title.x = element_text(face="bold", size=18),axis.title.y = element_text(face="bold", size=17),axis.text.x=element_text(angle=90,hjust=1,colour="black",face="bold",size=5.5),axis.text.y=element_text(colour="black",face="bold",size=14),panel.background=element_blank(),panel.background=element_blank(),panel.grid.minor=element_blank(),axis.line=element_line(colour="black"))
          pa2=ggplot(environment=environment())+geom_rect(data=rectangles2, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),fill='gray80', alpha=0.8)+geom_line(data=Mean1,aes(x=num,y=MPA,colour=typefac,group=type1),position=pd2,size=0.6)+ggtitle(paste("Physical Activity",date1))+labs(x="Time",y="Physical Activity (Cnts)")+guides(colour=guide_legend("Treatment"))
          pa2=pa2+coord_cartesian(xlim=c(0,max(br)),ylim=c(0,pamax))+scale_x_discrete(breaks=br,labels=timepoints)+theme(plot.title = element_text(face="bold",size=20),legend.title=element_text(face="bold",size=14),legend.text=element_text(face="bold",size=12),legend.key.height=unit(1,"cm"),legend.key=element_rect(fill="white"),axis.title.x = element_text(face="bold", size=18),axis.title.y = element_text(face="bold", size=17),axis.text.x=element_text(colour="black",face="bold",size=14),axis.text.y=element_text(colour="black",face="bold",size=14),panel.background=element_blank(),panel.background=element_blank(),panel.grid.minor=element_blank(),axis.line=element_line(colour="black"))
          pa3=ggplot(environment=environment())+geom_rect(data=rectangles2, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),alpha=0.2)+geom_line(data=Mean1,aes(x=num,y=MPA,colour=typefac,group=type1),position=pd2,size=0.5)+geom_point(data=Mean1,aes(x=num,y=MPA,colour=typefac,group=type1),position=pd2,size=2)+ggtitle(paste("Physical Activity",date1))+labs(x="Time",y="Physical Activity (Cnts)")+guides(colour=guide_legend("Treatment"))
          pa3=pa3+coord_cartesian(xlim=c(0,max(br)),ylim=c(0,pamax))+scale_x_discrete(breaks=br,labels=timepoints)+theme(plot.title = element_text(face="bold",size=20),legend.title=element_text(face="bold",size=14),legend.text=element_text(face="bold",size=12),legend.key.height=unit(1,"cm"),legend.key=element_rect(fill="white"),axis.title.x = element_text(face="bold", size=18),axis.title.y = element_text(face="bold", size=17),axis.text.x=element_text(colour="black",face="bold",size=14),axis.text.y=element_text(colour="black",face="bold",size=14),panel.background=element_blank(),panel.background=element_blank(),panel.grid.minor=element_blank(),axis.line=element_line(colour="black"))
          
          
          rectangles3 = data.frame(xmin = rect_left,xmax = rect_right,ymin = 0,ymax = fmax)
          f1=ggplot(environment=environment())+geom_line(data=Mean1,aes(x=num,y=MF,colour=typefac,group=type1),position=pd2,size=0.5)+geom_point(data=Mean1,aes(x=num,y=MF,colour=typefac,group=type1),position=pd2,size=2)+ggtitle(paste("Cumulative feeding",date1))+labs(x="Time",y="Grams consumed")+guides(colour=guide_legend("Treatment"))
          f1=f1+coord_cartesian(xlim=c(0,max(num)+1),ylim=c(0,fmax))+scale_x_discrete(breaks=c(num[1:max(num)],max(num)+1),labels=c(time1,""))+theme(plot.title = element_text(face="bold",size=20),legend.title=element_text(face="bold",size=14),legend.text=element_text(face="bold",size=12),legend.key.height=unit(1,"cm"),legend.key=element_rect(fill="white"),axis.title.x = element_text(face="bold", size=18),axis.title.y = element_text(face="bold", size=17),axis.text.x=element_text(angle=90,hjust=1,colour="black",face="bold",size=5.5),axis.text.y=element_text(colour="black",face="bold",size=12),panel.background=element_blank(),panel.background=element_blank(),panel.grid.minor=element_blank(),axis.line=element_line(colour="black"))
          f2=ggplot(environment=environment())+geom_rect(data=rectangles3, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),fill='gray80', alpha=0.8)+geom_line(data=Mean1,aes(x=num,y=MF,colour=typefac,group=type1),position=pd2,size=0.5)+geom_point(data=Mean1,aes(x=num,y=MF,colour=typefac,group=type1),position=pd2,size=2)+ggtitle(paste("Cumulative feeding",date1))+labs(x="Time",y="Grams consumed")+guides(colour=guide_legend("Treatment"))
          f2=f2+coord_cartesian(xlim=c(0,max(num)+1),ylim=c(0,fmax))+scale_x_discrete(breaks=br,labels=timepoints)+theme(plot.title = element_text(face="bold",size=20),legend.title=element_text(face="bold",size=14),legend.text=element_text(face="bold",size=12),legend.key.height=unit(1,"cm"),legend.key=element_rect(fill="white"),axis.title.x = element_text(face="bold", size=18),axis.title.y = element_text(face="bold", size=17),axis.text.x=element_text(colour="black",face="bold",size=14),axis.text.y=element_text(colour="black",face="bold",size=14),panel.background=element_blank(),panel.background=element_blank(),panel.grid.minor=element_blank(),axis.line=element_line(colour="black"))
          
          f3=ggplot(environment=environment())+geom_rect(data=rectangles2, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),fill='gray80', alpha=0.8)+geom_line(data=Mean1,aes(x=num,y=MFC,colour=typefac,group=type1),position=pd2,size=0.6)+ggtitle(paste("Change of food-intake",date1))+labs(x="Time",y="Food-intake (g)")+guides(colour=guide_legend("Treatment"))
          f3=f3+coord_cartesian(xlim=c(0,max(num)+1),ylim=c(0,round(max(Mean1$MFC)+0.1,1)))+scale_x_discrete(breaks=br,labels=timepoints)+scale_y_continuous(breaks=seq(0,round(max(Mean1$MFC)+0.1,1),0.1))+theme(plot.title = element_text(face="bold",size=20),legend.title=element_text(face="bold",size=14),legend.text=element_text(face="bold",size=12),legend.key.height=unit(1,"cm"),legend.key=element_rect(fill="white"),axis.title.x = element_text(face="bold", size=18),axis.title.y = element_text(face="bold", size=17),axis.text.x=element_text(colour="black",face="bold",size=14),axis.text.y=element_text(colour="black",face="bold",size=14),panel.background=element_blank(),panel.background=element_blank(),panel.grid.minor=element_blank(),axis.line=element_line(colour="black"))
          
        }
      
      info4 = list(cage=cage,Mean1=Mean1,rectangles=rectangles,rectangles2=rectangles2,rectangles3=rectangles3,rect_left=rect_left,pd2=pd2,breaks=breaks,date1=date1,time1=time1,br=br,timepoints=timepoints,rer=rer,pa=pa,feed=feed,rer1=rer1,pa1=pa1,rer2=rer2,pa2=pa2,f1=f1,rer3=rer3,pa3=pa3,f2=f2,typefac=typefac,feed2=feed2,f3=f3)
      return(info4)
    })
  })
  
  output$Boxchoose = renderUI({
    text2=Metainfo()$text2
    if (is.null(text2))
      return(NULL)
    if (input$ifchoosebox == FALSE)
      return(NULL)
    text2list=as.list(1:length(text2))
    names(text2list)=text2
    checkboxGroupInput("boxchoose", "", text2list, selected=1:length(text2), inline=TRUE)
  })
  
  output$cageraw = renderTable({
    if (input$goButton2 == 0) {return(NULL)} 
    if (is.null(input$file2))
      return(NULL)
    cageraw=data.frame(Data2()$cage)
    rownames(cageraw)=1:nrow(cageraw)
    cageraw
  })
  
  output$Mean1 = renderTable({
    if (input$goButton2 == 0) {return(NULL)} 
    if (is.null(input$file2))
      return(NULL)
    Mean=Value2()$Mean1
    if (is.null(Mean))
      return(NULL)
    text1=Data2()$text1
    Mean=Mean[,c(-1,-(length(Mean)-1))]
    metaindice=c("RER","Activity","Feed","Feed Change")
    merrorname=errorname[as.numeric(input$merrorformat)]
    if (any(duplicated(text1))) {
      colnames(Mean)=c("Date & Time",paste("Average",metaindice),paste("Average",metaindice,merrorname),"Type")
    } else {
      colnames(Mean)=c("Date & Time",metaindice,"Type")
    }
    Mean
  })
  
  output$plot1 = renderPlot({
    if (input$goButton2 == 0) {return(NULL)}
    if (is.null(input$file2))
      return(NULL)
    Mean1=Value2()$Mean1
    if (is.null(Mean1))
      return(NULL)
    rsmooth1=NULL
    rsmooth3=NULL
    if (input$ifsmooth==T) {
      rsmooth1=geom_smooth(method="loess",size=1.2,span=0.2,show_guide=F)
      rsmooth3=geom_smooth(data=Mean1,aes(x=num,y=MRER,colour=typefac,group=type1),size=1.2,span=0.2,show_guide=F)
    }
    rer1=Value2()$rer1+rsmooth1
    rer3=Value2()$rer3+rsmooth3
    if (input$mstyle==2) {print(rer1)} else {print(rer3)}
    },width=800,height=400)
  
  output$plot2 = renderPlot({
    if (input$goButton2 == 0) {return(NULL)}
    if (is.null(input$file2)) 
      return(NULL)
    rer2=Value2()$rer2
    print(rer2)},width=800,height=400)
  
  output$plot3 = renderPlot({
    if (input$goButton2 == 0) {return(NULL)}
    if (is.null(input$file2))
      return(NULL)
    rer=Value2()$rer
    print(rer)},width=820,height=400)
  
  output$plot4 = renderPlot({
    if (input$goButton2 == 0) {return(NULL)}
    if (is.null(input$file2))
      return(NULL)
    Mean1=Value2()$Mean1
    if (is.null(Mean1))
      return(NULL)
    pasmooth1=NULL
    pasmooth3=NULL
    if (input$ifsmooth==T) {
      pasmooth1=geom_smooth(method="loess",size=1.2,span=0.2,alpha=0.1,show_guide=F)
      pasmooth3=geom_smooth(data=Mean1,aes(x=num,y=MPA,colour=typefac,group=type1),method="loess",size=1.2,span=0.2,alpha=0.15,show_guide=F)
    }
    pa1=Value2()$pa1+pasmooth1
    pa3=Value2()$pa3+pasmooth3
    if (input$mstyle==2) {print(pa1)} else {print(pa3)}
    },width=800,height=400)
  
  output$plot5 = renderPlot({
    if (input$goButton2 == 0) {return(NULL)}
    if (is.null(input$file2))
      return(NULL)
    pa2=Value2()$pa2
    print(pa2)},width=800,height=400)
  
  output$plot6 = renderPlot({
    if (input$goButton2 == 0) {return(NULL)}
    if (is.null(input$file2))
      return(NULL)
    pa=Value2()$pa
    print(pa)},width=820,height=400)
  
  output$plot7 = renderPlot({
    if (input$goButton2 == 0) {return(NULL)}
    if (is.null(input$file2))
      return(NULL)
    Mean1=Value2()$Mean1
    if (is.null(Mean1))
      return(NULL)
    fsmooth1=NULL
    fsmooth3=NULL
    if (input$ifsmooth==T) {
      fsmooth1=geom_smooth(data=Mean1,aes(x=num,y=MF,colour=typefac,group=type1),method="loess",size=1.2,span=0.05,show_guide=F)
      fsmooth3=geom_smooth(data=Mean1,aes(x=num,y=MF,colour=typefac,group=type1),method="loess",size=1.2,span=0.05,show_guide=F)
    }
    f1=Value2()$f1+fsmooth1
    f2=Value2()$f2+fsmooth3
    if (input$mstyle==2) {print(f1)} else {print(f2)}
    },width=800,height=400)
  
  output$plot8 = renderPlot({
    if (input$goButton2 == 0) {return(NULL)}
    if (is.null(input$file2))
      return(NULL)
    feed=Value2()$feed
    print(feed)},width=820,height=400)

  output$plot9 = renderPlot({
    if (input$goButton2 == 0) {return(NULL)}
    if (is.null(input$file2))
      return(NULL)
    Mean1=Value2()$Mean1
    if (is.null(Mean1))
      return(NULL)
    fsmooth=NULL
    if (input$ifsmooth==T) {
      fsmooth=geom_smooth(data=Mean1,aes(x=num,y=MFC,colour=typefac,group=type1),method="loess",size=1.2,span=0.2,alpha=0.4,show_guide=F)
    }
    f3=Value2()$f3+fsmooth
    print(f3)},width=820,height=400)

  output$plot10 = renderPlot({
    if (input$goButton2 == 0) {return(NULL)}
    if (is.null(input$file2))
      return(NULL)
    feed2=Value2()$feed2
    print(feed2)},width=820,height=400)


#GTT & ITT#
Data3 = reactive({
  if (is.null(input$goButton3)) {return(NULL)} else {
    if (input$goButton3 == 0)
      return(NULL)
  }
  isolate({
    inFile3 = input$file3
    if (is.null(inFile3))
      return(NULL)
    
    if (identical(input$format2,'.xlsx')) {
      judge=read.xlsx(inFile3$datapath, sheetIndex=input$sheet2, header=F)
      firstcol=as.character(judge[,1])
      if (!any(grepl("TT",firstcol))) {assay=NULL} else {assay=firstcol[grep("TT",firstcol)][1]}
      if (any(grepl("No.",judge[,1]))) {
        firstrow=grep("No.",judge[,1])[1]
      } else {return(NULL)}
      TT = read.xlsx(inFile3$datapath, sheetIndex=input$sheet2, header=T, startRow=firstrow); TT[TT==""]="NA"; TT[TT==" "]="NA"; TT[TT=="<NA>"]="NA"
    } else{
      if (identical(input$format2,'.csv'))  {TT = read.csv(inFile3$datapath, header=T, na.strings=c("NA","NaN", ""," "), fileEncoding="latin1")} else{
        if (identical(input$format2,'.txt'))  {TT = read.table(inFile3$datapath, header=T, na.strings=c("NA","NaN", ""," "), fileEncoding="latin1")} else {return(NULL)}
      }
      firstcol=as.character(TT[,1])
      if (!any(grepl("TT",firstcol))) {assay=NULL} else {assay=firstcol[grep("TT",firstcol)][1]}
    }
    
    TTcolname=colnames(TT)
    if (any(!is.na(match("X120",TTcolname)))) {
      Tend=match("X120",TTcolname)[1]+1
    } else {
      if (any(!is.na(match("120",TTcolname)))) {
        Tend=match("120",TTcolname)[1]+1
      } else {
        if (any(!is.na(grepl("120",TTcolname)))) {
          Tend=grep("120",TTcolname)+1
        } else {return(NULL)}
      }
    }
    
    if (any(!is.na(match(c("Group","group","GROUP"),TTcolname)))) {
      groupcol=match(c("Group","group","GROUP"),TTcolname)[1]
      if (groupcol > Tend) {Tend=groupcol}
    } else {
      if (any(is.na(match("Prot.No.",TTcolname))))
        return(NULL)
      groupcol=match("Prot.No.",TTcolname)[1]
    }
    
    TT=TT[,1:Tend]
    TTcolname=colnames(TT)
    
    if (all(!is.na(match(c(0,15,30,60,90,120),TTcolname)))) {
      timecol=match(c(0,15,30,60,90,120),TTcolname)
    } else {
      timecol=c(grep("X0",TTcolname),grep("X15",TTcolname),grep("X30",TTcolname),grep("X60",TTcolname),grep("X90",TTcolname),grep("X120",TTcolname))
      if (length(timecol) == 0) {
        timecol=grep("PBS",TTcolname)[length(grep("PBS",TTcolname))]+seq(1,11,2)
        if (length(timecol) == 0) {return(NULL)}
      }
    }
    
    
    colnames(TT)[grep("Cage",TTcolname)]="Cage"
    colnames(TT)[grep("ID",TTcolname)]="ID"
    colnames(TT)[grep("BW",TTcolname)]="BW (g)"
    colnames(TT)[grep("PBS",TTcolname)]="PBS (uL)"
    colnames(TT)[timecol]=c("0","15","30","60","90","120")
    colnames(TT)[timecol+1]="Plasma Glucose"
    
    TTGroup=TT[,groupcol]
    endrow=length(TTGroup)
    if (any(is.na(TTGroup))) {
      navalue=which(is.na(TTGroup))
      endrow=navalue[1]-1
      if (length(navalue) == 2) {
        if (navalue[1] == length(TTGroup)-1) {
          if (navalue[2] == length(TTGroup)) {endrow=navalue[2]-1}
        }
      }
      if (length(navalue) >= 3) {
        if (any(duplicated(navalue-length(navalue)))) {
          if (length(which(duplicated(navalue-length(navalue)))) == 1) {
            if (which(duplicated(navalue-length(navalue))) < length(navalue)) {endrow=navalue[length(navalue)]-1}
          }
          if (length(which(duplicated(navalue-length(navalue)))) >= 2) {
            endrow=navalue[which(duplicated(navalue-length(navalue)))[1]]-1
          }
        }
      }
    }
    
    T1=timecol[1]
    Zerotime=TT[,T1]
    endrow1=endrow
    if (any(is.na(Zerotime))) {
      navalue1=which(is.na(Zerotime))
      endrow1=navalue1[1]-1
      if (length(navalue1) == 2) {
        if (navalue1[1] == length(Zerotime)-1) {
          if (navalue1[2] == length(Zerotime)) {endrow1=navalue1[2]-1}
        }
      }
      if (length(navalue1) >= 3) {
        if (any(duplicated(navalue1-length(navalue1)))) {
          if (length(which(duplicated(navalue1-length(navalue1)))) == 1) {
            if (which(duplicated(navalue1-length(navalue1))) < length(navalue1)) {endrow1=navalue1[length(navalue1)]-1}
          }
          if (length(which(duplicated(navalue1-length(navalue1)))) >= 2) {
            endrow1=navalue1[which(duplicated(navalue1-length(navalue1)))[1]]-1
          }
        }
      }
    }
    
    endrow=max(endrow,endrow1)
    TT=TT[1:endrow,]
    if (any(rowSums(!is.na(TT))<10)){
      blankr=which(rowSums(!is.na(TT))<10)
      TT=TT[-blankr,]
    }
    TT=data.frame(lapply(TT,as.character),stringsAsFactors=FALSE)
    
    PG=as.vector(as.matrix(TT[,timecol+1]))
    PG[PG=="LO"]="10"
    PG[PG=="HI"]="600"
    PG=as.numeric(PG)
    
    if (all(!is.na(match(c("Cage","ID"),colnames(TT))))) {
      micenumber=paste(TT$Cage,TT$ID,sep="")
    } else {
      if (groupcol == 2|groupcol > 4) {
        micenumber=paste(TT[,3],TT[,4],sep="")
      } else {
        if (groupcol == 3) {micenumber=paste(TT[,4],TT[,5],sep="")} else {micenumber=paste(TT[,3],TT[,5],sep="")}
      }
    }
    Num=length(micenumber)
    ttnum=rep(1:Num,6)
    No=as.character(rep(micenumber,6))
    Timepoints=as.numeric(rep(c(0,15,30,60,90,120),each=Num))
    Type2=rep(TT[,groupcol],6)
    
    tt=data.frame(ttnum, No, Timepoints, Type2, PG)
    realttgroup=unique(Type2)
    ttgroup=realttgroup
    
    
    if (any(is.na(TT[,timecol+1]))) {
      NArow=which(rowSums(is.na(TT[,timecol+1]))>0)
      TT.1=TT[-NArow,]
      
      micenumber1=micenumber[-NArow]
      Num1=length(micenumber1)
      ttnum1=rep(1:Num1,6)
      No1=as.character(rep(micenumber1,6))
      Timepoints.1=as.numeric(rep(c(0,15,30,60,90,120),each=Num1))
      Type2.1=rep(TT.1[,groupcol],6)
      
      PG1=as.vector(as.matrix(TT.1[,timecol+1]))
      PG1[PG1=="LO"]="10"
      PG1[PG1=="HI"]="600"
      PG1=as.numeric(PG1)
      finaltt=data.frame(ttnum1, No1, Timepoints.1, Type2.1, PG1)
      ttgroup=unique(Type2.1)
    } else {
      finaltt=tt
    }
    
    if (any(is.na(finaltt))) {
      finaltt=na.omit(finaltt)
    }
    colnames(finaltt)=c("ttnum", "No", "Timepoints", "Type2", "PG")
    
    duplicates=table(TTGroup)
    meantt=with(finaltt,tapply(PG,data.frame(Timepoints,Type2),mean))
    meantsd=with(finaltt,tapply(PG,data.frame(Timepoints,Type2),sd))
    meantse=with(finaltt,tapply(PG,data.frame(Timepoints,Type2),se))
    if (length(colnames(meantt))==length(ttgroup) & all(colnames(meantt) %in% ttgroup)) {} else {meantt=t(meantt)}
    if (length(colnames(meantsd))==length(ttgroup) & all(colnames(meantsd) %in% ttgroup)) {} else {meantsd=t(meantsd)}
    ttorder=colnames(meantt)
    mtt=c(meantt)
    tsd=c(meantsd)
    tse=c(meantse)
    n1=length(meantt)/length(ttgroup)
    timepoints2=rep(as.numeric(as.character(unique(Timepoints))),length(ttgroup))
    treat2=rep(ttorder,each=n1)
    MTT=data.frame(timepoints2,treat2,mtt,tsd,tse)
          
    if (mean(as.matrix(subset(MTT,timepoints2==0,mtt))) > mean(as.matrix(subset(MTT,timepoints2==15,mtt)))) {drop=1} else {drop=0}
    if (is.null(assay)) {
      if (drop == 0) {assay="GTT"} else {assay="ITT"}
    }
    if (input$ifttdate == T) {ttdate=paste(" (",input$ttdate,")",sep="")} else {ttdate=NULL}
    ttname=paste(assay,ttdate,sep="")
    ttname1=paste(assay, " Total Graph",ttdate,sep="")
    
    info5=list(TT=TT,tt=tt,finaltt=finaltt,MTT=MTT,treat2=treat2,mtt=mtt,tse=tse,timepoints2=timepoints2,realttgroup=realttgroup,ttgroup=ttgroup,ttname=ttname,ttname1=ttname1)
    return(info5)
  })
})


TTAOV = reactive({
  if (is.null(input$goButton3)) {return(NULL)} else {
    if (input$goButton3 == 0)
      return(NULL)
  }
  isolate({
    finaltt=Data3()$finaltt
    timepoints2=Data3()$timepoints2
    MTT=Data3()$MTT
    doanova=input$doanova
    anova1=NULL
    anova2=NULL
    
    if (doanova == 1)
      return(NULL)
    
    if (doanova == 2|doanova == 4) {
      minute=unique(finaltt$Timepoints)
      anova1=c()
      for(i in minute){
        A=subset(finaltt,Timepoints==i)
        aov=aov(as.numeric(PG)~Type2,data=A)
        p=summary(aov)
        Pv=p[[1]][1,5]
        if (Pv<=0.001){star="***"}
        if (Pv<=0.01&Pv>0.001){star="**"}
        if (Pv<=0.05&Pv>0.01){star="*"}
        if (Pv<=0.1&Pv>0.05){pvalue=round(Pv,4)
                             star=paste("P =",pvalue)}
        if (Pv>0.1){star=" "}
        anova1=append(anova1,star)
      }
    }
    
    if (doanova == 3|doanova == 4) {
      taov=aov(as.numeric(PG)~Type2*Timepoints,data=finaltt)
      tp=summary(taov)
      Pv2=tp[[1]][1,5]
      if (Pv2<0.001){Pvalue="P < 0.001"}
      if (Pv2<=0.1&Pv2>=0.001){pvalue2=round(Pv2,4)
                               Pvalue=paste("P =",pvalue2)}
      if (Pv2>0.1){Pvalue="NS"}
      anova2=Pvalue
    }
    
    if (is.null(anova1)) {AOV=NULL} else {
      xp=timepoints2
      asterisk=c(anova1,rep("",length(timepoints2)-length(anova1)))
      middle=c(with(MTT,tapply(mtt,timepoints2,mean)))
      AOV=data.frame(xp,asterisk)
      AOV$middle=middle
    }
    
    ttaovinfo=list(anova1=anova1,anova2=anova2,AOV=AOV)
  })
})


Value3 = reactive({
  if (is.null(input$goButton3)) {return(NULL)} else {
    if (input$goButton3 == 0)
      return(NULL)
  }
  isolate({
  TT=Data3()$TT
  if (is.null(TT))
    return(NULL)
  tt=Data3()$tt
  MTT=Data3()$MTT
  unit2=Data3()$unit2
  scale2=Data3()$scale2
  digit2=Data3()$digit2  
  
  if (mean(tt$PG,na.rm = T) > 40) {unit2="Plasma Glucose (mg/dl)"; ttscale=100; ttdigit=-2} else {unit2="Plasma Glucose (mmol/L)"; ttscale=1; ttdigit=0}
  max1=max(tt$PG,na.rm = T)
  max2=max(MTT$mtt+MTT$tse,na.rm = T)
  if (max1 <= 3*ttscale) {ttscale1=ttscale/2} else {ttscale1=ttscale}
  if (max2 <= 3*ttscale) {ttscale2=ttscale/2} else {ttscale2=ttscale}
  
  tt1=ggplot(tt,aes(x=Timepoints,y=PG,colour=factor(Type2),group=ttnum),environment=environment())+geom_line(aes(linetype=factor(Type2),size=Type2))+geom_point(size=3.5,aes(fill=factor(Type2),shape=factor(Type2)))+labs(x="Time (min)",y=unit2,colour="Treatment",shape="Treatment",linetype="Treatment",fill="Treatment")
  tt1=tt1+scale_x_continuous(breaks=c(0,15,30,60,90,122),labels=c("0","15","30","60","90","120"))+scale_y_continuous(breaks=seq(0,round(max1+ttscale1,ttdigit),ttscale1))+coord_cartesian(xlim=c(-2,122),ylim=c(0,round(max1+ttscale1,ttdigit)))+theme(plot.title = element_text(face="bold",size=22),legend.title=element_text(face="bold",size=17),legend.text=element_text(face="bold",size=14),legend.key.height=unit(1,"cm"),legend.key=element_rect(fill="white"),axis.title.x = element_text(face="bold", vjust=-0.4, size=18),axis.title.y = element_text(face="bold", vjust=1.2, size=18),axis.text=element_text(colour="black",face="bold",size=14),axis.ticks = element_line(colour="black"),panel.background=element_blank(),panel.grid.minor=element_blank(),axis.line=element_line(colour="black"))
  
  tt2=ggplot(MTT,aes(x=timepoints2,y=mtt,colour=factor(treat2)),environment=environment(),position=pd3)+geom_line(aes(linetype=factor(treat2),size=treat2))+labs(x="Time (min)",y=unit2,colour="Treatment",shape="Treatment",linetype="Treatment",fill="Treatment")
  tt2=tt2+scale_x_continuous(breaks=c(0,15,30,60,90,122),labels=c("0","15","30","60","90","120"))+scale_y_continuous(breaks=seq(0,round(max2+ttscale2,ttdigit),ttscale2))+coord_cartesian(xlim=c(-2.5,122),ylim=c(0,round(max2+ttscale2,ttdigit)))+theme(plot.title = element_text(face="bold",size=22),legend.title=element_text(face="bold",size=17),legend.text=element_text(face="bold",size=14),legend.key.height=unit(1,"cm"),legend.key=element_rect(fill="white"),axis.title.x = element_text(face="bold", vjust=-0.4, size=18),axis.title.y = element_text(face="bold", vjust=1.2, size=18),axis.text=element_text(colour="black",face="bold",size=14),axis.ticks = element_line(colour="black"),panel.background=element_blank(),panel.grid.minor=element_blank(),axis.line=element_line(colour="black"))
  
  info6=list(TT=TT, tt=tt, tt1=tt1, tt2=tt2, MTT=MTT, max2=max2)
  return(info6)
  })
})


TTcustom = reactive({
  MTT=Data3()$MTT
  AOV=TTAOV()$AOV
  anova1=TTAOV()$anova1
  anova2=TTAOV()$anova2
  ttgroup=Data3()$ttgroup
  realttgroup=Data3()$realttgroup
  ttname=Data3()$ttname
  ttname1=Data3()$ttname1
  
  ttcolor=1
  reverse1=F
  ttlinetype=1
  reverse2=F
  ttshape1=21
  ttshape2=24
  ttfill=1
  tterrorformat=1
  tterrorbar1=1
  tterrorstyle=1
  ttlineweight=0.75
  ttpointsize=4
  tterrorwidth=3
  aov1height=rep(0,6)
  aov2height=0
  aov2size=6
  
  if (!is.null(input$apply)) {
    if (input$apply != 0) {
      isolate({
        motitle=input$motitle
        if (!is.null(motitle)) {ttname=motitle}
        ttcolor=input$ttcolor
        reverse1=input$reverse1
        ttlinetype=input$ttlinetype
        reverse2=input$reverse2
        ttshape1=as.numeric(input$ttshape1)
        ttshape2=as.numeric(input$ttshape2)
        ttfill=input$ttfill
        tterrorbar1=input$tterrorbar1
        tterrorformat=input$tterrorformat
        tterrorstyle=input$tterrorstyle
        ttlineweight=input$ttlineweight
        ttpointsize=input$ttpointsize
        tterrorwidth=input$tterrorwidth
        aov1height=vapply(1:6, function(i) {eval(parse(text=paste("input$aov1height",i,sep="")))},numeric(1))
        aov2height=input$aov2height
        aov2size=input$aov2size
        
        if (is.null(tterrorwidth)) {
          tterrorformat=1
          tterrorbar1=1
          tterrorstyle=1
          ttlineweight=0.75
          ttpointsize=4
          tterrorwidth=3
        }
        if (is.null(ttfill)) {
          ttcolor=1
          reverse1=F
          ttlinetype=1
          reverse2=F
          ttshape1=21
          ttshape2=24
          ttfill=1
        }
        if (is.null(aov1height)) {
          aov1height=rep(0,6)
        }
        if (is.null(aov2height)) {
          aov2height=0
        }
        if (is.null(aov2size)) {
          aov2size=6
        }
      })
    }
  }
  
  if (is.null(AOV)) {addanova1=NULL} else {
    AOV$aov1height=aov1height
    addanova1=geom_text(data=AOV,aes(label=asterisk,x=xp,y=middle+aov1height),size=5.5,colour="black",fontface="bold")
  }
  if (is.null(anova2)) {addanova2=NULL} else {addanova2=annotate("text",label=anova2,x=105,y=max(MTT$mtt+MTT$tse),size=aov2size,colour="black",fontface="bold",vjust=-aov2height)}
  addanova=list(addanova1,addanova2)
  
  if (tterrorformat == 1) {
    tterror=aes(ymin=mtt-tse,ymax=mtt+tse)
  } else {
    tterror=aes(ymin=mtt-tsd,ymax=mtt+tsd)
  }
  if (tterrorstyle == 2) {tterrorwidth=0}
  if (tterrorbar1 == 1) {
    errortype=geom_errorbar(tterror,width=tterrorwidth,size=0.72)
  } else {
    errortype=geom_errorbar(tterror,colour="black",width=tterrorwidth,size=0.72)
  }
  
  pointfill=NULL
  linetype3=NULL 
  
  if (length(ttgroup)==2) {
    if (ttcolor == 1) {coltype=c("#FF0000","#000000")}
    if (ttcolor == 2.1) {coltype=c("#E41A1C","#377EB8")}
    if (ttcolor == 2.2) {coltype=c("red", "blue")}
    if (ttcolor == 2.3) {coltype=c("brown3", "deepskyblue4")}
    if (ttcolor == 3) {coltype=c("#0033FF","#FF9900")}
    if (ttcolor == 4) {coltype=c("black","#666666")}
    if (ttcolor == 5) {coltype=c("black","black")}
    if (reverse1 == T) {coltype=rev(coltype)}
    
    ttshape=c(ttshape1,ttshape2)
    
    if (ttfill == 1) {pointfill=scale_fill_manual(values=coltype)}
    if (ttfill == 2) {pointfill=scale_fill_manual(values=rep("white",2))}
    if (ttfill == 3) {pointfill=scale_fill_manual(values=rep("black",2))}
    if (ttfill == 4) {pointfill=scale_fill_manual(values=c("black","white"))}
    if (ttfill == 5) {pointfill=scale_fill_manual(values=c("white","black"))}
    
    if (ttlinetype == 1) {linetype3=rep("solid",length(ttgroup))} 
    if (ttlinetype == 2.1) {linetype3=c("solid","longdash")}
    if (ttlinetype == 2.2) {linetype3=c("solid","dotted")}
    if (ttlinetype == 2.3) {linetype3=c("solid","dotdash")}
    if (ttlinetype == 2.4) {linetype3=c("longdash","dotted")}
    if (reverse2 == T) {linetype3=rev(linetype3)}
    
    ttcustom1=list(addanova,errortype,geom_point(size=ttpointsize,aes(fill=factor(treat2),shape=factor(treat2))),scale_colour_manual(values=coltype),scale_shape_manual(values=ttshape),pointfill,scale_linetype_manual(values=linetype3),scale_size_discrete(range=c(ttlineweight,ttlineweight),guide=FALSE))
  } else {
    if (length(ttgroup)<=8) {
      if (ttlinetype == 1) {linetype3=scale_linetype_manual(values=rep("solid",length(ttgroup)))}
      if (ttlinetype == 2) {linetype3=NULL}
      if (ttfill == 2) {pointfill=scale_fill_manual(values=rep("white",length(ttgroup)))}
      if (ttfill == 3) {pointfill=scale_fill_manual(values=rep("black",length(ttgroup)))}
      ttcustom1=list(addanova,errortype,geom_point(size=ttpointsize,aes(fill=factor(treat2),shape=factor(treat2))),scale_colour_brewer(palette="Set1"),pointfill,linetype3,scale_size_discrete(range=c(ttlineweight,ttlineweight),guide=FALSE))
    }
  }
  
  
  if (identical(realttgroup,ttgroup)) {ttcustom2=ttcustom1[-1:-3]} else {
    pointfill.1=NULL
    linetype3.1=NULL
    if (length(realttgroup)==2) {
      if (ttcolor == 1) {coltype.1=c("#FF0000","#000000")}
      if (ttcolor == 2.1) {coltype.1=c("#E41A1C","#377EB8")}
      if (ttcolor == 2.2) {coltype.1=c("red", "blue")}
      if (ttcolor == 2.3) {coltype.1=c("brown3", "deepskyblue4")}
      if (ttcolor == 3) {coltype.1=c("#0033FF","#FF9900")}
      if (ttcolor == 4) {coltype.1=c("black","#666666")}
      if (ttcolor == 5) {coltype.1=c("black","black")}
      if (reverse1 == T) {coltype.1=rev(coltype.1)}
      
      ttshape=c(ttshape1,ttshape2)
      
      if (ttfill == 1) {pointfill.1=scale_fill_manual(values=coltype.1)}
      if (ttfill == 2) {pointfill.1=scale_fill_manual(values=rep("white",2))}
      if (ttfill == 3) {pointfill.1=scale_fill_manual(values=rep("black",2))}
      if (ttfill == 4) {pointfill.1=scale_fill_manual(values=c("black","white"))}
      if (ttfill == 5) {pointfill=scale_fill_manual(values=c("white","black"))}
      
      if (ttlinetype == 1) {linetype3.1=rep("solid",length(realttgroup))} 
      if (ttlinetype == 2.1) {linetype3.1=c("solid","longdash")}
      if (ttlinetype == 2.2) {linetype3.1=c("solid","dotted")}
      if (ttlinetype == 2.3) {linetype3.1=c("solid","dotdash")}
      if (ttlinetype == 2.4) {linetype3.1=c("longdash","dotted")}
      if (reverse2 == T) {linetype3.1=rev(linetype3.1)}
      
      ttcustom2=list(ggtitle(ttname1),scale_colour_manual(values=coltype.1),scale_shape_manual(values=ttshape),pointfill.1,scale_linetype_manual(values=linetype3.1),scale_size_discrete(range=c(ttlineweight,ttlineweight),guide=FALSE))
    } else {
      if (length(realttgroup)<=8) {
        if (ttlinetype == 1) {linetype3.1=scale_linetype_manual(values=rep("solid",length(realttgroup)))}
        if (ttfill == 2) {pointfill.1=scale_fill_manual(values=rep("white",length(realttgroup)))}
        if (ttfill == 3) {pointfill.1=scale_fill_manual(values=rep("black",length(realttgroup)))}
        ttcustom2=list(ggtitle(ttname1),scale_colour_brewer(palette="Set1"),pointfill.1,linetype3.1,scale_size_discrete(range=c(ttlineweight,ttlineweight),guide=FALSE))
      }
    }
  }
  
  ttcustom1[[length(ttcustom1)+1]]=ggtitle(ttname)
  
  customlist=list(ttcustom1=ttcustom1,ttcustom2=ttcustom2)
  return(customlist)
})

output$chart1 = renderPlot({
  tt2=Value3()$tt2
  if (is.null(tt2))
    return(NULL)
  ttcustom1=TTcustom()$ttcustom1
  tt2=tt2+ttcustom1
  print(tt2)},width=650,height=400)

output$chart2 = renderPlot({
  tt1=Value3()$tt1
  if (is.null(tt1))
    return(NULL)
  ttcustom2=TTcustom()$ttcustom2
  tt1=tt1+ttcustom2
  print(tt1)},width=650,height=400)


output$TT = renderTable({
  if (is.null(input$goButton3)) {return(NULL)} else {
    if (input$goButton3 == 0) {return(NULL)}
  }
  isolate({
    TT=Data3()$TT
    if (is.null(TT))
      return(NULL)
    TT=as.data.frame(TT)
  })
})


output$MTT = renderTable({
  if (is.null(input$goButton3)) {return(NULL)} else {
    if (input$goButton3 == 0) {return(NULL)}
  }
  isolate({
    MTT=Value3()$MTT
    if (is.null(MTT))
      return(NULL)
    MTT$mtt=round(MTT$mtt,2)
    MTT$tse=round(MTT$tse,2)
    MTT=data.frame(lapply(MTT,as.character))
    colnames(MTT)=c("Time (min)","Treatment","Average Plasma Glucose","SD","SEM")
    MTT
  })
})

output$ttbuttons = renderUI({
  div(class="row",div(class="span1", ''),div(class="span5",div(class="span6",actionButton("goButton3", "Analyze", style="info")),div(class="span6",uiOutput("DIYButton"),uiOutput("DIYbutton"))))
})

output$DIYButton = renderUI({  
  if (is.null(input$goButton3)) {return(NULL)} else {
    if (input$goButton3 == 0) {return(NULL)}
  }
  isolate({
    if (is.null(Value3()$tt2))
      return(NULL)
    bsButton("DIYbutton", "Customize", style="success")
  })
})

output$AOV1position = renderUI({
  max2=Value3()$max2
  if (is.null(max2)) {max2=300}
  max2=ceiling(max2/10)*10
  select_list = lapply(1:6, function(i) {
    timeposition=c(0,15,30,60,90,120)
    div(style="display:inline-block",sliderInput(paste("aov1height",i,sep=""),timeposition[i],min=-max2,max=max2,value=0,step=1,width="140px"))
  })
  do.call(tagList, select_list)
})

output$flexchart = renderPlot({
  if (is.null(input$goButton3)) {return(NULL)} else {
    if (input$goButton3 == 0) {return(NULL)}
  }
  MTT=Value3()$MTT
  AOV=TTAOV()$AOV
  anova1=TTAOV()$anova1
  anova2=TTAOV()$anova2
  tt2=Value3()$tt2
  if (is.null(tt2))
    return(NULL)
  ttgroup=Data3()$ttgroup
  ttname=Data3()$ttname
  motitle=input$motitle
  if (!is.null(motitle)) {ttname=motitle}
  ttcolor=input$ttcolor
  reverse1=input$reverse1
  ttlinetype=input$ttlinetype
  reverse2=input$reverse2
  ttshape1=as.numeric(input$ttshape1)
  ttshape2=as.numeric(input$ttshape2)
  ttfill=input$ttfill
  tterrorformat=input$tterrorformat
  tterrorbar1=input$tterrorbar1
  tterrorstyle=input$tterrorstyle
  ttlineweight=input$ttlineweight
  ttpointsize=input$ttpointsize
  tterrorwidth=input$tterrorwidth
  aov1height=rep(0,6)
  if (!is.null(input$aov1height1)) {
    aov1height=vapply(1:6, function(i) {eval(parse(text=paste("input$aov1height",i,sep="")))},numeric(1))
  }
  aov2height=input$aov2height
  aov2size=input$aov2size
  
  if (is.null(tterrorwidth)) {
    tterrorformat=1
    tterrorbar1=1
    tterrorstyle=1
    ttlineweight=0.75
    ttpointsize=4
    tterrorwidth=3
  }
  if (is.null(ttfill)) {
    ttcolor=1
    reverse1=F
    ttlinetype=1
    reverse2=F
    ttshape1=21
    ttshape2=24
    ttfill=1
  }
  if (is.null(aov2height)) {
    aov2height=0
  }
  if (is.null(aov2size)) {
    aov2size=6
  }
  
  if (is.null(AOV)) {addanova1=NULL} else {
    AOV$aov1height=aov1height
    addanova1=geom_text(data=AOV,aes(label=asterisk,x=xp,y=middle+aov1height),size=5.5,colour="black",fontface="bold")
  }
  if (is.null(anova2)) {addanova2=NULL} else {addanova2=annotate("text",label=anova2,x=105,y=max(MTT$mtt+MTT$tse),size=aov2size,colour="black",fontface="bold",vjust=-aov2height)}
  addanova=list(addanova1,addanova2)
  
  if (tterrorformat == 1) {
    tterror=aes(ymin=mtt-tse,ymax=mtt+tse)
  } else {
    tterror=aes(ymin=mtt-tsd,ymax=mtt+tsd)
  }
  if (tterrorstyle == 2) {tterrorwidth=0}
  if (tterrorbar1 == 1) {
    errortype=geom_errorbar(tterror,width=tterrorwidth,size=ttlineweight)
  } else {
    errortype=geom_errorbar(tterror,colour="black",width=tterrorwidth,size=ttlineweight)
  }
  
  pointfill=NULL
  
  if (length(ttgroup)==2) {
    if (ttcolor == 1) {coltype=c("#FF0000","#000000")}
    if (ttcolor == 2.1) {coltype=c("#E41A1C","#377EB8")}
    if (ttcolor == 2.2) {coltype=c("red", "blue")}
    if (ttcolor == 2.3) {coltype=c("brown3", "deepskyblue4")}
    if (ttcolor == 3) {coltype=c("#0033FF","#FF9900")}
    if (ttcolor == 4) {coltype=c("black","#666666")}
    if (ttcolor == 5) {coltype=c("black","black")}
    if (reverse1 == T) {coltype=rev(coltype)}
    
    ttshape=c(ttshape1,ttshape2)
    
    if (ttfill == 1) {pointfill=scale_fill_manual(values=coltype)}
    if (ttfill == 2) {pointfill=scale_fill_manual(values=rep("white",2))}
    if (ttfill == 3) {pointfill=scale_fill_manual(values=rep("black",2))}
    if (ttfill == 4) {pointfill=scale_fill_manual(values=c("black","white"))}
    if (ttfill == 5) {pointfill=scale_fill_manual(values=c("white","black"))}
    
    if (ttlinetype == 1) {linetype3=rep("solid",length(ttgroup))} 
    if (ttlinetype == 2.1) {linetype3=c("solid","longdash")}
    if (ttlinetype == 2.2) {linetype3=c("solid","dotted")}
    if (ttlinetype == 2.3) {linetype3=c("solid","dotdash")}
    if (ttlinetype == 2.4) {linetype3=c("longdash","dotted")}
    if (reverse2 == T) {linetype3=rev(linetype3)}
    
    flextt=tt2+addanova+ggtitle(ttname)+errortype+geom_point(size=ttpointsize,aes(fill=factor(treat2),shape=factor(treat2)))+scale_colour_manual(values=coltype)+scale_shape_manual(values=ttshape)+pointfill+scale_linetype_manual(values=linetype3)+scale_size_discrete(range=c(ttlineweight,ttlineweight),guide=FALSE)
  } else {
    if (length(ttgroup)<=8) {
      if (ttlinetype == 1) {linetype3=scale_linetype_manual(values=rep("solid",length(ttgroup)))}
      if (ttlinetype == 2) {linetype3=NULL}
      if (ttfill == 2) {pointfill=scale_fill_manual(values=rep("white",length(ttgroup)))}
      if (ttfill == 3) {pointfill=scale_fill_manual(values=rep("black",length(ttgroup)))}
      flextt=tt2+addanova+ggtitle(ttname)+errortype+geom_point(size=ttpointsize,aes(fill=factor(treat2),shape=factor(treat2)))+scale_colour_brewer(palette="Set1")+pointfill+linetype3+scale_size_discrete(range=c(ttlineweight,ttlineweight),guide=FALSE)
    }
  }
  
  print(flextt)
  },width=680,height=420)


output$tt2group = renderUI({
  ttgroup=Data3()$ttgroup
  if (is.null(ttgroup))
    return(NULL)
  if (length(ttgroup) != 2)
    return(NULL)
  list(
    div(h5("Color collocation:")),
    div(class="row-fluid",
        div(class="span6",selectInput("ttcolor", "",choices=list("Red & Black" = "1", "Red & Blue" = c("R&B (Default)" = "2.1", "R&B (Standard)" = "2.2", "R&B (Dark)" = "2.3"), "Blue & Yellow" = "3", "Black & Grey"="4", "Black" = "5"))),
        div(class="span5",list(helpText("\n"),bsToggleButton("reverse1", label = "Reverse color",style="warning")))
        ),
    div(h5("Line type:")),
    div(class="row-fluid",
        div(class="span6",selectInput("ttlinetype", "",choices=list("Solid" = "1", "Dinstinct" = c("Solid & Dash" = "2.1","Solid & Dot" = "2.2","Solid & Dotdash" = "2.3","Dash & Dot" = "2.4")))),
        div(class="span6",list(helpText("\n"),bsToggleButton("reverse2", label = "Reverse line type",style="primary")))
    ),
    bsButtonGroup("ttshape1", label = h5("Point shape of each group:"), toggle = "radio", value = "21",
                  bsButton("Dot", label = "Dot", value = "21"),
                  bsButton("Triangle", label = "Triangle", value = "24"),
                  bsButton("Square", label = "Square", value = "22"),
                  bsButton("Diamond", label = "Diamond", value = "23"),
                  bsButton("None", label = "None", value = "NA")),
    bsButtonGroup("ttshape2", label = "", toggle = "radio", value = "24",
                  bsButton("Dot", label = "Dot", value = "21"),
                  bsButton("Triangle", label = "Triangle", value = "24"),
                  bsButton("Square", label = "Square", value = "22"),
                  bsButton("Diamond", label = "Diamond", value = "23"),
                  bsButton("None", label = "None", value = "NA")),
    helpText(" \n"),
    div(class="row-fluid",div(class="span6",selectInput("ttfill", h5("Points fill color:"),choices=c("Default" = "1", "White" = "2", "Black" = "3", "Black & White"="4", "White & Black"="5"))))
  )
})


observe({
  if (is.null(Value3()$tt2))
    return(NULL)
  ttname=Data3()$ttname
  local({
    output$DIYModal = renderUI({
      mybsModal("DIYwindow", "Graph Customization", trigger = "DIYbutton",
              tags$div(class = "row-fluid",
                       tags$div(class = "span4 well control-panel",
                                textInput("motitle", h5("Graph Title:"), ttname),
                                uiOutput("tt2group"),
                                div(h5("Error bar:")),
                                div(class="row-fluid",
                                    div(class="span4",selectInput("tterrorformat", "Error format:",choices=c("SEM" = "1", "SD" = "2"))),
                                    div(class="span4",selectInput("tterrorbar1", "Color:",choices=c("Default" = "1", "Black" = "2"))),
                                    div(class="span4",selectInput("tterrorstyle", "Style:",choices=c("Default" = "1", "Segment" = "2")))
                                ),
                                sliderInput("ttlineweight",h5("Line weight:"),min=0,max=1.5,value=0.75,step=0.05,ticks=FALSE),
                                sliderInput("ttpointsize",h5("Point size:"),min=2,max=6,value=4,step=0.05,ticks=FALSE),
                                sliderInput("tterrorwidth",h5("Error bar width:"),min=1,max=5,value=3,step=0.05,ticks=FALSE),
                                helpText(h5("ANOVA results height:")),
                                bsCollapse(multiple = TRUE, id = "collapse1",
                                           bsCollapsePanel("One-way ANOVA results", id="aov1h", helpText("Height:"),
                                                           uiOutput("AOV1position")),
                                           bsCollapsePanel("Two-way ANOVA result", id="aov2h", sliderInput("aov2height","Height:",min=-10,max=10,value=0,step=0.1), sliderInput("aov2size","Size:",min=0,max=12,value=6,step=0.5,ticks=FALSE))),
                                busyIndicator(text = "Plotting...",wait = 0)
                       ),
                       tags$div(class = "span8",
                                plotOutput("flexchart")
                       )
              )
      )
    })
  })
})

toggleModal(session, "DIYwindow",data=NULL)


output$basic = renderUI({
  sidebarLayout(
    sidebarPanel(fileInput('file1',label=h4("Select general mice data file:"),accept=c('text/csv/xlsx','text/comma-separated-values','text/plain','.xlsx')),
                 radioButtons('format', label=h4("Choose file format:"), c('.csv', '.txt', '.xlsx')),
                 conditionalPanel(condition="input.format == '.xlsx'",
                                  numericInput('sheet',label=h5('Choose sheet:'),1)),
                 actionButton("goButton", "Analyze"),
                 h4("Note:"),
                 helpText("In this module, 'csv', 'txt' or 'xlsx' files with neat format are available. The uploaded table should have a header row containing necessary column names: the first 2 columns are mice group and number, while subsequent columns are body weight and plasma glucose level. Weight columns names must contain letter 'W', in contrast, we should confirm there are no letter 'W' in glucose columns names. Mice data must be divided into upper and lower parts by a blank row with a letter 'C' or 'T' in the first cell (corresponding to group column), referring to 'control' or 'test' group below. Click 'Analyze' button and all the other process will be automatically completed by app. There is no need to worry about blank rows and columns, or any format problems after that. App would have a delay during graphs generation or renewal, please be patient."),
                 helpText("Attention: File format must be correctly selected. If the excel file has multiple sheets, we should choose which sheet is needed for analysis. In addition, any rows with missing value will be removed when plotting. In case of program crash or errors, we just need to refresh it."),
                 helpText("."),
                 h4("Example Table Format:"),
                 h5(tableOutput("example")),
                 img(src="note.png",width=800,height=800)
    ),
    mainPanel(
      tabsetPanel(
        tabPanel(h4("Average Graphs"),
                 plotOutput("graph1"),
                 plotOutput("graph2"),
                 plotOutput("graph3"),
                 plotOutput("graph4")),
        tabPanel(h4("Total Plots & Correlation"),
                 plotOutput("graph5"),
                 plotOutput("graph6"),
                 plotOutput("graph7"),
                 plotOutput("graph8")),
        tabPanel(h4("Summary Table"),
                 h5(tableOutput("MEAN"))),
        tabPanel(h4("Raw Data"),
                 h5(tableOutput("raw")))
      )))
 })


output$metabolic1 = renderUI({
  sidebarLayout(
  sidebarPanel(fileInput('file2',label=h4("Upload metabolic cage data file:"),accept=c('csv','comma-separated-values')),
               selectInput("mstyle",h5("First graph style:"),choices=c("Distinguish day and night"="1","Time sequential format"="2")),
               conditionalPanel(condition="input.mstyle == '1'",
                                sliderInput("daytime",h5("Set daytime (o'clock):"),min=0,max=24,value=c(6,18))),
               selectInput("merrorformat", h5("Error format:"),choices=c("SEM" = "1", "SD" = "2")),
               checkboxInput("ifsmooth",h5("Add smooth fitting curve"),value=T),
               checkboxInput("ifchoosebox",h5("Select sample")),
               uiOutput("Boxchoose"),
               actionButton("goButton2", "Analyze",style="primary"),
               h4("Note:"),
               helpText("Please upload standard data files (csv format) created by metabolic cage device. Experimental data with different timeseries or treatment numbers are supported now. App would have a delay during graphs generation or renewal, please be patient."),
               helpText("Attention: Importing abnormal format data may lead to program errors. In case of program crash or errors, we just need to refresh it."),
               helpText("."),
               img(src="note2.png",width=800,height=800)),
  mainPanel(
  tabsetPanel(
    tabPanel(h4("Respiratory Exchange Ratio"),
             plotOutput("plot1"),
             plotOutput("plot2"),
             plotOutput("plot3")),
    tabPanel(h4("Physical Activity"),
             plotOutput("plot4"),
             plotOutput("plot5"),
             plotOutput("plot6")),
    tabPanel(h4("Feed"),
             plotOutput("plot7"),
             plotOutput("plot8"),
             plotOutput("plot9"),
             plotOutput("plot10")),
    tabPanel(h4("Summary Table"),
             h5(tableOutput("Mean1"))),
    tabPanel(h4("Raw Data"),
             h5(tableOutput("cageraw")))
  )))
 })


output$TT1 = renderUI({
  sidebarLayout(
   sidebarPanel(fileInput('file3',label=h4("Import GTT/ITT data file:"),accept=c('text/csv/xlsx','text/comma-separated-values','text/plain','.xlsx')),
                radioButtons('format2', label=h4("Choose file format:"), c('.xlsx', '.csv', '.txt')),
                conditionalPanel(condition="input.format2 == '.xlsx'",
                        numericInput('sheet2',label=h5('Choose sheet:'),1)),
                selectInput("doanova",h4("ANOVA:"),choices=c("None"="1","One-Way ANOVA"="2","Two-Way ANOVA"="3","Both"="4")),
                checkboxInput("ifttdate",h4("Select Date")),
                conditionalPanel(condition="input.ifttdate",
                                 dateInput("ttdate","")),
                helpText(" \n "),
                uiOutput("ttbuttons"),
                uiOutput("DIYModal"),
                h4("Note:"),
                helpText("The 'csv', 'txt' or 'xlsx' files are also available here. We can import data of glucose tolerance test (GTT) or insulin tolerance test (ITT) for analysis. App would have a delay during graphs generation or renewal, please be patient."),
                helpText("Attention: Care about the selection of file format, date and experiment type (GTT or ITT). If the excel file has multiple sheets, we should choose which sheet is needed for analysis. In addition, any rows with missing value will be removed when plotting. In case of program crash or errors, we just need to refresh it."),
                helpText("."),
                img(src="note3.png",width=800,height=800)),
  mainPanel(
  tabsetPanel(
    tabPanel(h4("Graphs"),
             plotOutput("chart1"),
             plotOutput("chart2")),
    tabPanel(h4("Summary Table"),
             h5(tableOutput("MTT"))),
    tabPanel(h4("Raw Data"),
             tableOutput("TT"))
  )))
 })

se = function(x, na.rm = FALSE) {
  sqrt(var(if (is.vector(x)) x else as.double(x), na.rm = na.rm)/length(x))
}

mymean=function(x,na.rm=TRUE){
  mean(x, na.rm = na.rm)
}
mysd=function(x,na.rm=TRUE){
  sd(x, na.rm = na.rm)
}
myse=function(x,na.rm=TRUE){
  se(x, na.rm = na.rm)
}

errorname=c("SEM","SD")


mybsModal <- function(id, title, trigger, ..., href) {
  mo <- tags$div(class = "modal sbs-modal hide fade", id = id, style = "width: 1200px; height :500px; left: 350px; top: 30px",
                 "data-trigger" = trigger,
                 tags$div(class = "modal-header",
                          tags$button(Type = "button", class = "close",
                                      "data-dismiss" = "modal", HTML("&times;")),
                          tags$h3(title)),
                 body <- tags$div(class = "modal-body"),
                 tags$div(class = "modal-footer",
                          tags$div(class="row-fluid",
                                   tags$div(class="span6","   "),
                                   tags$div(class="span6",
                                            tags$div(class="span4",""),
                                            tags$div(class="span2",actionButton("apply", "Apply",style="info")),
                                            tags$div(class="span4",actionButton("getimage", "Get High Quality Graph", style="success")),
                                            tags$div(class="span2",tags$a(href = "#", class = "btn", "data-dismiss" = "modal", "Close"))
                                            )
                                   )
                 )
  )
  
  if(!missing(href)) {
    mo <- addAttribs(mo, "data-remote" = href)
  } else {
    mo$children[[2]] <- tagAppendChildren(mo$children[[2]], list = list(...))
  }
  
  return(mo)
}
})
