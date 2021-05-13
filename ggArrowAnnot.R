ggArrowAnnot <- function(plot_name, locat, label, direction, dist, offset){
	# plot_name: name of the plot
	# direction: 1-left; 2-right; 3-bottom; 4-top
	# dist: arrow length
	# locat: data point
	# label: name of labels
	# offset: labels and arrow offset
	
	n <- length(label)

	if (direction==1){
			for (i in 1:n){
				plot_name <- plot_name +
					annotate("text", label=label[i], x=locat[1,i]-dist-offset, y=locat[2,i], size=5, 
					      fontface="bold.italic") + # manually place the label for trial 1
					   annotate("segment", x=locat[1,i]-dist, y=locat[2,i], xend=locat[1,i], yend=locat[2,i], size=0.5, 
					      arrow=arrow(length=unit(.2, "cm")))
			}
		}else if (direction==2){
			for (i in 1:n){
				plot_name <- plot_name +
					annotate("text", label=label[i], x=locat[1,i]+dist+offset, y=locat[2,i], size=5, 
					      fontface="bold.italic") + # manually place the label for trial 1
					   annotate("segment", x=locat[1,i]+dist, y=locat[2,i], xend=locat[1,i], yend=locat[2,i], size=0.5, 
					      arrow=arrow(length=unit(.2, "cm")))
			}
		}else if (direction==3){
			for (i in 1:n){
				plot_name <- plot_name +
					annotate("text", label=label[i], x=locat[1,i], y=locat[2,i]-dist-offset, size=5, 
					      fontface="bold.italic") + # manually place the label for trial 1
					   annotate("segment", x=locat[1,i], y=locat[2,i]-dist, xend=locat[1,i], yend=locat[2,i], size=0.5, 
					      arrow=arrow(length=unit(.2, "cm")))
			}
	    }else{
				for (i in 1:n){
					plot_name <- plot_name +
						annotate("text", label=label[i], x=locat[1,i], y=locat[2,i]+dist+offset, size=5, 
						      fontface="bold.italic") + # manually place the label for trial 1
						annotate("segment", x=locat[1,i], y=locat[2,i]+dist, xend=locat[1,i], yend=locat[2,i], size=0.5, 
					      arrow=arrow(length=unit(.2, "cm")))
		}
	}
return(plot_name)
}
	
