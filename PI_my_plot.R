
# source("myggPlot.R")

plot_angle=ggplot()
plot_velocity=ggplot()
plot_input=ggplot()
plot_all=ggplot()


Trials <- c('ADP control','Initial control')
States <- c('x_1','x_2')
col_vec <- c('red','blue')



Trials <- c('1','2','3','4','5')
label <- expression(paste(x[1], ', ADP control'), 
                    paste(x[1], ', No control'), 
                    paste(x[2],', ADP control'),
                    paste(x[2],', No control'),
                    'Control input')

col_vec <- c('blue','red', 'green', 'orange', 'brown')

label <- expression(hat(w)[1], 
                    hat(w)[2], 
                    hat(w)[3])

wt <- t(w_list)
s <- seq(1, TimeTrain,length=TimeTrain)
wtDf <- data.frame(s, wt)
wtDfm = melt(wtDf, "s")
all_w <- ggplot(wtDfm, aes(x=s, y=value, color=variable)) + geom_line(size=.5) + 
  ylab("Weights") + xlab("Trials") + 
  scale_color_manual("", values = c('blue', 'green', 'red'), labels = label) +
  theme_bw() + theme(legend.position=c(.9 ,.5))

pdf("PI_w_Plot.pdf", paper="special",height=2, width=8)
print(all_w)
dev.off()



wt <- t(w_list)
s <- seq(1, TimeTrain,length=TimeTrain)
wtDf <- data.frame(s, wt)
wtDfm = melt(wtDf, "s")
g3 <- ggplot(wtDfm, aes(x=s, y=value, color=variable), show.legend = FALSE) + geom_line(size=.5, show.legend = FALSE) + 
  ylab(expression(hat(w))) + xlab("Trials") + theme_bw() 

ct <- t(c_list)
s <- seq(1, TimeTrain,length=TimeTrain)
ctDf <- data.frame(s, ct)
ctDfm = melt(ctDf, "s")
g4 <- ggplot(ctDfm, aes(x=s, y=value, color=variable), show.legend = FALSE) + geom_line(size=.5, show.legend = FALSE) + 
  ylab(expression(hat(c))) + xlab("Trials") + theme_bw()  







pdf("PI_weights_plot_exp2.pdf", paper="special",height=4, width=8)
par(bty = "n")
grid.arrange(g3, g4, nrow = 2)
dev.off()



# postscript("sim_Plot.eps", paper="special",height=4, width=8, horizontal=FALSE )    
# grid.arrange(plot_all,
#              all_w,
#              nrow = 2)
# dev.off()