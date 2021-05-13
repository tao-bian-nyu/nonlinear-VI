
# source("myggPlot.R")

plot_angle=ggplot()
plot_velocity=ggplot()
plot_input=ggplot()
plot_all=ggplot()

text_size = 16

Trials <- c('ADP control','Initial control')
States <- c('x_1','x_2')
col_vec <- c('red','blue')

names(p_samp1) <- c("time", "x1")
names(p_samp2) <- c("time", "x1")
plot_angle <- plot_angle + 
  geom_line(data=p_samp1, aes(x=time, y=x1, color=Trials[1], linetype=Trials[1]), size=.5) + 
  geom_line(data=p_samp2, aes(x=time, y=x1, color=Trials[2], linetype=Trials[2]), size=.5) + 
  scale_linetype_manual("", values=c(1,2)) + scale_color_manual("", values=col_vec) +
  ylab("x1") + xlab(expression(t)) + 
  theme_bw()



names(v_samp1) <- c("time", "x2")
names(v_samp2) <- c("time", "x2")
plot_velocity <- plot_velocity + 
  geom_line(data=v_samp1, aes(x=time, y=x2, color=Trials[1], linetype=Trials[1]), size=.5) + 
  geom_line(data=v_samp2, aes(x=time, y=x2, color=Trials[2], linetype=Trials[2]), size=.5) + 
  scale_linetype_manual("", values=c(1,2)) + scale_color_manual("", values=col_vec) +
  ylab("x2") + xlab(expression(t)) + theme_bw()


names(input_samp) <- c("time", "Input")
plot_input <- plot_input + 
  geom_line(data=input_samp, aes(x=time, y=Input, colour='Control input', linetype='Control input'), size=.5) + 
  scale_linetype_manual("", values=1) + scale_color_manual("", values='red') +
  ylab("Input") + xlab(expression(t)) + theme_bw()


Trials <- c('1','2','3','4','5')
label <- expression(paste(x[1], ', ADP control'), 
                    paste(x[1], ', No control'), 
                    paste(x[2],', ADP control'),
                    paste(x[2],', No control'),
                    'Control input')

col_vec <- c('blue','red', 'green', 'orange', 'brown')
names(p_samp1) <- c("time", "x1")
names(p_samp2) <- c("time", "x1")
names(v_samp1) <- c("time", "x2")
names(v_samp2) <- c("time", "x2")
names(input_samp) <- c("time", "x3")

plot_all <- plot_all + 
  geom_line(data=p_samp2, aes(x=time, y=x1, color=Trials[2], linetype=Trials[2]), size=.5) +
  geom_line(data=p_samp1, aes(x=time, y=x1, color=Trials[1], linetype=Trials[1]), size=.5) + 
  
  geom_line(data=v_samp2, aes(x=time, y=x2, color=Trials[4], linetype=Trials[4]), size=.5) + 
  geom_line(data=v_samp1, aes(x=time, y=x2, color=Trials[3], linetype=Trials[3]), size=.5) + 
  
  geom_line(data=input_samp, aes(x=time, y=x3, color=Trials[5], linetype=Trials[5]), size=.5) + 
  annotate("rect", xmin = 0, xmax = 1.99, ymin = -8, ymax = 3, alpha = .3) +
  annotate("text", x = 1, y = -7, label = 'Training phase') +
  annotate("text", x = 7, y = -7, label = 'After Training') +
  scale_linetype_manual("", values=c(1,2,1,2,1), labels = label) + 
  scale_color_manual("", values=col_vec, labels = label) +
  ylab(expression(x)) + xlab(expression(t)) + 
  scale_x_continuous(limits = c(0,10), expand = c(0.0015, 0)) +
  scale_y_continuous(limits = c(-8,3), expand = c(0, 0)) +
  theme_bw() + 
  theme(legend.text.align = 0)

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
  theme_bw() + theme(legend.position=c(.9 ,.5),axis.text.x = element_text(size=text_size),
                     axis.text.y = element_text(size=text_size),
                     text = element_text(size=text_size))

pdf("w_Plot.pdf", paper="special",height=2, width=8)
print(all_w)
dev.off()



wt <- t(w_list)
s <- seq(1, TimeTrain,length=TimeTrain)
wtDf <- data.frame(s, wt)
wtDfm = melt(wtDf, "s")
g3 <- ggplot(wtDfm, aes(x=s, y=value, color=variable), show.legend = FALSE) + geom_line(size=.5, show.legend = FALSE) + 
  ylab(expression(hat(w))) + xlab("Trials") + theme_bw() + theme(legend.position=c(.9 ,.5),axis.text.x = element_text(size=text_size),
                                                                 axis.text.y = element_text(size=text_size),
                                                                 text = element_text(size=text_size))

ct <- t(c_list)
s <- seq(1, TimeTrain,length=TimeTrain)
ctDf <- data.frame(s, ct)
ctDfm = melt(ctDf, "s")
g4 <- ggplot(ctDfm, aes(x=s, y=value, color=variable), show.legend = FALSE) + geom_line(size=.5, show.legend = FALSE) + 
  ylab(expression(hat(c))) + xlab("Trials") + theme_bw()  + theme(legend.position=c(.9 ,.5),axis.text.x = element_text(size=text_size),
                                                                  axis.text.y = element_text(size=text_size),
                                                                  text = element_text(size=text_size))

pdf("weights_plot_exp2.pdf", paper="special",height=4, width=8)
par(bty = "n")
grid.arrange(g3, g4, nrow = 2)
dev.off()







pdf("all_plot.pdf", paper="special",height=2, width=8)    
print(plot_all)
dev.off()


pdf("sim_Plot.pdf", paper="special",height=4, width=8)    
grid.arrange(plot_all,
             all_w,
             nrow = 2)
dev.off()

# postscript("sim_Plot.eps", paper="special",height=4, width=8, horizontal=FALSE )    
# grid.arrange(plot_all,
#              all_w,
#              nrow = 2)
# dev.off()