library(shiny)

# Some plotting functions used below
plotMu <- function(mu){
	      	arrows(mu, 1, mu, 0,length=0.0,lwd=5,col="red")
      		mtext(expression(mu),side=3,at=mu,cex=3,col="red")
      		}
plot1SD <- function(mu,sigma){
      		arrows(mu+0.05, 0, mu+sigma, 0,length=0.15,lwd=5,col="black",code=3)
      		arrows(mu-sigma, 0, mu-0.05, 0,length=0.15,lwd=5,col="black",code=3)
      		mtext(expression(sigma),side=1,at=mu+sigma/2,cex=3,col="black",padj=1)
      		mtext(expression(sigma),side=1,at=mu-sigma/2,cex=3,col="black",padj=1)
      		}
plot2SD <- function(mu,sigma){
      		arrows(mu+0.05, 0, mu+2*sigma, 0,length=0.15,lwd=3,col="blue",code=3)
      		arrows(mu-2*sigma, 0, mu-0.05, 0,length=0.15,lwd=3,col="blue",code=3)
      		mtext(expression(2*sigma),side=1,at=mu+1.2*sigma,cex=3,col="blue",padj=1)
      		mtext(expression(2*sigma),side=1,at=mu-1.2*sigma,cex=3,col="blue",padj=1)
      		}

# Generate the plots and pass them back to ui.R
shinyServer(function(input,output){

# Probability Histogram Plots	
  output$ph_plot <- renderPlot({
  	if (input$typeDist == "bin") {	
      x <- 0:round(input$num_bin)
      y <- dbinom(x,input$num_bin,input$prob_bin)
      plot(x,y,type="n",ylim=c(0,max(y)),main="",xlab=expression(y),ylab=expression(P(Y==y)))
      lines(c(-2,input$num_bin+2),c(0,0))
      for(i in 1:(input$num_bin+1)){rect(x[i]-0.5,y[i],x[i]+0.5,0,col="gray")}
      
      mu <- input$num_bin*input$prob_bin
      sigma <- sqrt(input$num_bin*input$prob_bin*(1-input$prob_bin))
      if(input$show1SD==TRUE) plot1SD(mu,sigma)
      if(input$show2SD==TRUE) plot2SD(mu,sigma)
      if(input$showMean==TRUE) plotMu(mu)  
      
      mtext(bquote(Bin(n==.(input$num_bin),p==.(input$prob_bin))),,side=3,padj=0,cex=2,at=round(input$num_bin)/2)            
      }  
      
    if (input$typeDist == "geo") {	  
      x <- 0:round(input$max_x_geo)
      y <- dgeom(x-1,input$prob_geo)
      plot(x,y,type="n",ylim=c(0,max(y)),main="",xlab=expression(y),ylab=expression(P(Y==y)))
      lines(c(-2,input$max_x_geo+2),c(0,0))
      for(i in 1:(input$max_x_geo+1)){rect(x[i]-0.5,y[i],x[i]+0.5,0,col="gray")}
      
      mu <- 1/input$prob_geo
      sigma <- sqrt((1-input$prob_geo)/input$prob_geo^2)    
      if(input$show1SD==TRUE) plot1SD(mu,sigma)
      if(input$show2SD==TRUE) plot2SD(mu,sigma)
      if(input$showMean==TRUE) plotMu(mu)     
      
      mtext(bquote(Geo(p==.(input$prob_geo))),,side=3,padj=0,cex=2,at=round(input$max_x_geo)/2)    
      }        
      
    if (input$typeDist == "nbin") {	    
      x <- 0:round(input$num_trials)
      y <- dnbinom(x-input$num_suc_trials,input$num_suc_trials, input$prob_nbin)
      plot(x,y,type="n",ylim=c(0,max(y)),main="",xlab=expression(y),ylab=expression(P(Y==y)))
      lines(c(-2,input$num_trials+2),c(0,0))
      for(i in 1:(input$num_trials+1)){rect(x[i]-0.5,y[i],x[i]+0.5,0,col="gray")}   
            
      mu <- input$num_suc_trials/input$prob_nbin
      sigma <- sqrt(input$num_suc_trials*(1-input$prob_nbin)/input$prob_nbin^2)    
      if(input$show1SD==TRUE) plot1SD(mu,sigma)
      if(input$show2SD==TRUE) plot2SD(mu,sigma)
      if(input$showMean==TRUE) plotMu(mu)   
      
      mtext(bquote(NB(r==.(input$num_suc_trials),p==.(input$prob_nbin))),,side=3,padj=0,cex=2,at=round(input$num_trials)/2)   
      } 
           
    if (input$typeDist == "hyp") {	    
      x <- 0:round(input$sample_hyp)
      y <- dhyper(x,input$red_hyp,input$pop_hyp-input$red_hyp,input$sample_hyp)
      plot(x,y,type="n",ylim=c(0,max(y)),main="",xlab=expression(y),ylab=expression(P(Y==y)))
      lines(c(-2,input$sample_hyp+2),c(0,0))
      for(i in 1:(input$sample_hyp+1)){rect(x[i]-0.5,y[i],x[i]+0.5,0,col="gray")}  
             
      mu <- input$sample_hyp*input$red_hyp/input$pop_hyp
      sigma <- sqrt((input$sample_hyp*input$red_hyp/input$pop_hyp)*((input$pop_hyp-input$red_hyp)/input$pop_hyp)*((input$pop_hyp-input$sample_hyp)/(input$pop_hyp-1)))    
      if(input$show1SD==TRUE) plot1SD(mu,sigma)
      if(input$show2SD==TRUE) plot2SD(mu,sigma)
      if(input$showMean==TRUE) plotMu(mu)    

      mtext(bquote(Hyper(N==.(input$pop_hyp),r==.(input$red_hyp),n==.(input$sample_hyp))),,side=3,padj=0,cex=2,at=round(input$sample_hyp)/2)   
      }   
             
    if (input$typeDist == "pois") {	
      x <- 0:round(input$max_x_pois)
      y <- dpois(x,input$lambda)
      plot(x,y,type="n",ylim=c(0,max(y)),main="",xlab=expression(y),ylab=expression(P(Y==y)))
      lines(c(-2,input$max_x_pois+2),c(0,0))
      for(i in 1:(input$max_x_pois+1)){rect(x[i]-0.5,y[i],x[i]+0.5,0,col="gray")} 
            
      mu <- input$lambda
      sigma <- sqrt(input$lambda)    
      if(input$show1SD==TRUE) plot1SD(mu,sigma)
      if(input$show2SD==TRUE) plot2SD(mu,sigma)
      if(input$showMean==TRUE) plotMu(mu)        
      
      mtext(bquote(Pois(lambda==.(input$lambda))),,side=3,padj=0,cex=2,at=round(input$max_x_pois)/2)         
      }    
  })
  
# Probability Mass Function Plots	
  output$pmf_plot <- renderPlot({
  	if (input$typeDist == "bin") {	
  	  x <- 0:round(input$num_bin)
      y <- dbinom(x,input$num_bin,input$prob_bin)
      plot(x,y,type="h",main="",xlab=expression(y),ylab=expression(P(Y==y)),lwd=2,col="blue",ylim=c(0,max(y)))
      points(x,y,pch=19,col="blue")
      lines(c(-2,input$num_bin+2),c(0,0))
      mtext(bquote(Bin(n==.(input$num_bin),p==.(input$prob_bin))),,side=3,padj=0,cex=2,at=round(input$num_bin)/2)    
      }  
    if (input$typeDist == "geo") {	  
      x <- 0:round(input$max_x_geo)
      y <- dgeom(x-1,input$prob_geo)
      plot(x,y,type="h",main="",xlab=expression(y),ylab=expression(P(Y==y)),lwd=2,col="blue",ylim=c(0,max(y)))
      points(x,y,pch=19,col="blue")
      lines(c(-2,input$max_x_geo),c(0,0))       
      mtext(bquote(Geo(p==.(input$prob_geo))),,side=3,padj=0,cex=2,at=round(input$max_x_geo)/2)   
      }   
    if (input$typeDist == "nbin") {	
      x <- 0:round(input$num_trials)
      y <- dnbinom(x-input$num_suc_trials,input$num_suc_trials, input$prob_nbin)
      plot(x,y,type="h",main="",xlab=expression(y),ylab=expression(P(Y==y)),lwd=2,col="blue",ylim=c(0,max(y)))
      points(x,y,pch=19,col="blue")
      lines(c(-2,input$num_trials),c(0,0)) 
      mtext(bquote(NB(r==.(input$num_suc_trials),p==.(input$prob_nbin))),,side=3,padj=0,cex=2,at=round(input$num_trials)/2)   
      }      
    if (input$typeDist == "hyp") {	
      x <- 0:round(input$sample_hyp)
      y <- dhyper(x,input$red_hyp,input$pop_hyp-input$red_hyp,input$sample_hyp)
      plot(x,y,type="h",main="",xlab=expression(y),ylab=expression(P(Y==y)),lwd=2,col="blue",ylim=c(0,max(y)))
      points(x,y,pch=19,col="blue")
      lines(c(-2,input$sample_hyp),c(0,0))       
      mtext(bquote(Hyper(N==.(input$pop_hyp),r==.(input$red_hyp),n==.(input$sample_hyp))),,side=3,padj=0,cex=2,at=round(input$sample_hyp)/2)         
      }          
    if (input$typeDist == "pois") {	  
      x <- 0:round(input$max_x_pois)
      y <- dpois(x,input$lambda)
      plot(x,y,type="h",main="",xlab=expression(y),ylab=expression(P(Y==y)),lwd=2,col="blue",ylim=c(0,max(y)))
      points(x,y,pch=19,col="blue")
      lines(c(-2,input$max_x_pois),c(0,0)) 
      mtext(bquote(Pois(lambda==.(input$lambda))),,side=3,padj=0,cex=2,at=round(input$max_x_pois)/2)             
      }   
  })
  
# Cumultative Distribution Function Plots	
  output$cdf_plot <- renderPlot({
    if (input$typeDist == "bin") {	
      x <- 0:round(input$num_bin)
      y <- pbinom(x,input$num_bin,input$prob_bin)
      plot(x,y,type="p",main="",xlab=expression(y),ylab=expression(P(Y<=y)),pch=19,ylim=c(0,1),col="blue")
      for(i in 1:(input$num_bin+1)){lines(c(x[i],x[i]+1),c(y[i],y[i]),lwd=2,col="blue")}
      lines(c(-2,0),c(0,0),lwd=2,col="blue")
      mtext(bquote(Bin(n==.(input$num_bin),p==.(input$prob_bin))),,side=3,padj=0,cex=2,at=round(input$num_bin)/2)    
      }   
    if (input$typeDist == "geo") {	  
      x <- 0:round(input$max_x_geo)
      y <- pgeom(x-1,input$prob_geo)
      plot(x,y,type="p",main="",xlab=expression(y),ylab=expression(P(Y<=y)),pch=19,ylim=c(0,1),col="blue")
      for(i in 1:(input$max_x_geo+1)){lines(c(x[i],x[i]+1),c(y[i],y[i]),lwd=2,col="blue")}
      lines(c(-2,0),c(0,0),lwd=2,col="blue")      
      mtext(bquote(Geo(p==.(input$prob_geo))),,side=3,padj=0,cex=2,at=round(input$max_x_geo)/2)         }   
    if (input$typeDist == "nbin") {	  
      x <- 0:round(input$num_trials)
      y <- pnbinom(x-input$num_suc_trials,input$num_suc_trials, input$prob_nbin)
      plot(x,y,type="p",main="",xlab=expression(y),ylab=expression(P(Y<=y)),pch=19,ylim=c(0,1),col="blue")
      for(i in 1:(input$num_trials+1)){lines(c(x[i],x[i]+1),c(y[i],y[i]),lwd=2,col="blue")}
      lines(c(-2,0),c(0,0),lwd=2,col="blue")
      mtext(bquote(NB(r==.(input$num_suc_trials),p==.(input$prob_nbin))),,side=3,padj=0,cex=2,at=round(input$num_trials)/2)         
      }      
    if (input$typeDist == "hyp") {	 
      x <- 0:round(input$sample_hyp)
      y <- phyper(x,input$red_hyp,input$pop_hyp-input$red_hyp,input$sample_hyp)
      plot(x,y,type="p",main="",xlab=expression(y),ylab=expression(P(Y<=y)),pch=19,ylim=c(0,1),col="blue")
      for(i in 1:(input$sample_hyp+1)){lines(c(x[i],x[i]+1),c(y[i],y[i]),lwd=2,col="blue")}
      lines(c(-2,0),c(0,0),lwd=2,col="blue")     
      mtext(bquote(Hyper(N==.(input$pop_hyp),r==.(input$red_hyp),n==.(input$sample_hyp))),,side=3,padj=0,cex=2,at=round(input$sample_hyp)/2)         
      }          
    if (input$typeDist == "pois") {	  
      x <- 0:round(input$max_x_pois)
      y <- ppois(x,input$lambda)
      plot(x,y,type="p",main="",xlab=expression(y),ylab=expression(P(Y<=y)),pch=19,ylim=c(0,1),col="blue")
      for(i in 1:(input$max_x_pois+1)){lines(c(x[i],x[i]+1),c(y[i],y[i]),lwd=2,col="blue")}
      lines(c(-2,0),c(0,0),lwd=2,col="blue")
      mtext(bquote(Pois(lambda==.(input$lambda))),,side=3,padj=0,cex=2,at=round(input$max_x_pois)/2)             
      }    
  })
  
})


