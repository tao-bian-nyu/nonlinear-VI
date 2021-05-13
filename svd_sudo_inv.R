svdSudoInv <- function(Theta){
	# ss <- dim(Theta)
	 a.svd <- svd(Theta)
	 ds <- diag(1/a.svd$d)
	 us <- a.svd$u
	 vs <- a.svd$v
	 out <- vs %*% ds %*% t(us)
}