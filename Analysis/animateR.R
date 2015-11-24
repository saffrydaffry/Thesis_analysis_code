## Brownian Motion: the 'parallel' version
library(animation)
library(jpeg)
library(png)
library(extrafont)
library(extrafontdb)
library(scales)

setwd("C:/Users/Mac")
file_list_1 <- list.files(pattern = ".csv")

saveLatex({
  for (i in unique(bm.data$step)) {
   print(ggplot(subset(bm.data, step <=i), aes(x=x, y = y, color = id), alpha = 0.1) +
            labs(title=paste("step",i)) +
            xlim(range(bm.data$x)) + ylim(range(bm.data$x)) + 
            geom_point(aes(size = rep(3, n * grp)), 
                       data = subset(bm.data, step == i)) + 
              facet_wrap(~group)+
            theme_bw() + theme(legend.position = "none"))
    
    #grp2<-ggplot(subset(bm.data, step <=i & group ==2), aes(x=x, y = y), alpha = 0.1) +
     # labs(title=paste("step",i)) +
      #xlim(range(bm.data$x)) + ylim(range(bm.data$x)) + 
      #geom_point(aes(size = rep(3, n * grp)), 
       #          data = subset(bm.data, step == i& group ==2, color = id)) + 
      #  facet_wrap(~grp)+
      #theme_bw() + theme(legend.position = "none")
    
#    multiplot(grp1,grp2, ncol = 2)
  }
}, ani.basename = "BM", img.name = "R2", ani.opts = "controls,loop,width=0.8\\textwidth",
interval = 0.2, img.name="R2", movie.name = "ggplot2.mpg", ani.width = 600, ani.height = 600)


## Attempt with own data
#latex
oopt = ani.options(interval = 0.5, nmax = 5, 
                   ani.dev = 'cairoPDF',ani.type = "pdf", 
                   ani.width =6, ani.height=5, outdir = "C:/Users/Mac/Documents")

#gif
oopt = ani.options(interval = 0.5, nmax = 5, outdir ="C:/Users/Mac/Documents" )
saveGIF({
  for (i in unique(pca_tv$yr)) {
    sizes = c(3.5,2.9,2.4,1.9,1.5)
   print(ggplot(subset(pca_tv, yr ==i),aes(x =rm_score, y  = tv_score, show_guide =FALSE))+
           geom_point(shape = 21, aes(fill = years, color = years, size = factor(yr))) +
           #geom_smooth()+
           scale_size_manual(values = sizes[1:i])+
           scale_fill_gradient(limits = c(0.28, 4.69), low="navy",high = "goldenrod1")+
           scale_color_gradient(low="purple4", high = "gold2")+
           geom_hline(y = 0, color = "grey")+ geom_vline(x=0, color = "grey")+
           facet_grid(mut_rate~Virulence, as.table = FALSE) +
           theme(legend.position="top")+theme_tufte(base_family = "Gill Sans MT", base_size = 14)
         )
  }
}, img.name = "S",ani.basename = "BM", movie.name = "Trans_Vir.mp4", ani.width = 800, ani.height = 600, clean = TRUE)
#latex# ani.basename = "BM", img.name = "R3", ani.opts = "controls,width=\\textwidth", latex.filename = "testggplot.tex")

ani.options(oopt)



## RNA Animation
## ggplot rhabdo_log_kaks from Rhabdo_Lfold_KaKs.Rnw
setwd("~/ScRVPNG/JPEG")


img <-readJPEG("15_ss.jpg")
grid.newpage(recording=FALSE)
grid(nx=1,ny=2)
rhabdo_log_kaks ##this just puts the png on top of the ggplot graph
grid.raster(img)

dev.off()

rhabdo_log_kaks+annotation_raster(img, xmin=120, xmax=130, ymin=-37, ymax=-44) 
##test 2
ggplot(data=ORF_poly_shift, aes(x = x,y=y)) + 
  geom_polygon(aes(fill = Group), color = "white", alpha = 0.4,size = 0.25) +
  annotation_raster(img, xmin= i*(15), xmax= i*15 + 5000*(dim(img)[2]/600), ymin=1.25,ymax = 8*(dim(img)[1]/600)) + #only works if data in ggplot()
  ylim(-1.5,9)
  
  geom_hline(yintercept = 0, linetype = "dotted", color = gray)


file_list <- list.files(pattern = ".jpg") #343 files

oopt = ani.options(interval = 0.1, nmax = 342, ani.width =550, ani.height = 450, outdir = "C:/Users/Mac/Documents")
saveGIF({
  for (i in 1:length(file_list)){
    img<-readJPEG(file_list[i])
    #grid.newpage(recording=FALSE) #grid.raster(img)
    
    print(ggplot(data=ORF_poly_shift, aes(x = x,y=y)) + 
      geom_polygon(aes(fill = Group), color = "white", alpha = 0.4,size = 0.25) +
         annotation_raster(img, xmin= i*(15), xmax= i*15 + 5000*(dim(img)[2]/600), ymin=1.25,ymax = 8*(dim(img)[1]/600)) + #only works if data in ggplot()
      ylim(-1,9) +
        geom_vline(data = saved_dat_cut[i,], aes(xintercept= start))+
      geom_hline(yintercept = 0, linetype = "dotted", color = gray) +
      geom_point(data = infKaKs_log, aes(x = start, y = Ka.Ks), shape = 23, size = 2, color = "gray75", alpha =0.9, fill = "gray75",show_guide = FALSE) +
      
      geom_point(data=betweens_log,aes(x = start, y = Ka.Ks), color = "black", alpha=0.8, size =2, show_guide = FALSE) +
      geom_point(data=everyoneElse_log, aes(x = start, y = Ka.Ks, color = Group), size = 2.25, alpha = 1,               show_guide = FALSE) + 
      geom_text(data= infKaKs_log, aes(x = start, y = Ka.Ks, label=labels), size=3, fontface="bold", color="navyblue", vjust=0, hjust=-0.1) +
      geom_text(data= zeros_log,  aes(x = start, y = Ka.Ks, label=labels), size=4, fontface="italic", color="black", vjust=-0.5, hjust=0) +
      geom_point(data=zeros_log,  aes(x = start, y = Ka.Ks),color = "black", shape = 1, alpha=0.8, size =2.5,
                 show_guide = FALSE) +
      ylab("log(Ka/Ks)") + xlab("Nucleotide Position") +
      scale_color_manual(values = muted( c("#F8766D", "#64B200", "#DB8E00", "#00BD5C", "#00C1A7", "#00BADE","#EF67EB","#00A6FF","#B385FF"))) +
      scale_fill_manual(values = c("#F8766D", "#64B200", "#DB8E00", "#00BD5C", "#AEA200", "#00C1A7", "#FF63B6", "#00BADE","#EF67EB","#00A6FF","#B385FF")) +
      theme_tufte(base_family = "Gill Sans MT", base_size = 10) + 
      theme(legend.title = element_blank())  
    )
  }  
}, ani.basename = "BM",  img.name = "S",clean = TRUE, movie.name = "ScRVfast.mp4")
#save latex: ani.opts = "controls, width=\\textwidth",
ani.options(oopt)

\end{document}
