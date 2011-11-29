##
#	what does this function: writing a EexprSet into a database. a unique experiment name has to be submitted, the arrays from this experiment are
#	added, then the signal channels, 
#
setMethod("publishToDB","MadbSet",
	function(object=NULL,connection=NULL,exp.name=NULL,exp.date=NULL,samples=NULL,signal.channels=NULL,preprocessing=NULL,pubmed.id=NULL,global.transaction=TRUE,v=TRUE){
		require(pgUtils)
		if(is.null(object)){
			stop("please submit a array experiment\n\n")
		}
		if(is.null(connection)){
			stop("please open first a connection to your database.\n")
		}
		if(is.null(exp.date)){
			exp.date="18011977"
		}
		Con <- connection
		if(is.null(exp.name)){
			if(!is.null(description(object)@title)){
				exp.name <- description(object)@title
			}
			else{
				stop("please specify the experiment name!\n\n")
			}
		}
		
		##
		# if an error occurs, rollback the transaction!
		#on.exit(.resetAfterError())
		# have to be shure that a connection object exists also after an error...
		#assign("ConnectionForErrors",Con,env=globalenv())
		#options(error=expression(.rollItBack()))
		
		
		#if(is.null(samples)){
		#	stop("please specify the samples that are on the chips\n\n")
		#}
		#library(tkfiDB)
		#Con <- dbConnect(PgSQL(),dbname=dbname,host=host,user=user)
		#show(Con)
		
		if(global.transaction){
			r <- dbSendQuery(Con,"BEGIN TRANSACTION;")
		}
		
		tryCatch(.publishExperimentToDB(object,Con,exp.name,exp.date,samples,signal.channels,preprocessing=preprocessing,pubmed.id=pubmed.id,global.transaction,v),error=function(error){
			.rollItBack(Con)
			cat("\n",as.character(error),"\n","To get more information read the log file:",log.file,"\n")
		},interrupt=function(interrupt){
			.rollItBack(Con)
			cat("The transaction has been rolled back.\n")
		})
		

		#if(global.transaction){
		#	on.exit(dbSendQuery(Con, "ROLLBACK TRANSACTION"))
		#}
	}
)



.publishExperimentToDB <- function(object=NULL,Con=NULL,exp.name=NULL,exp.date=NULL,samples=NULL,signal.channels=NULL,preprocessing="",pubmed.id="",global.transaction=TRUE,v=TRUE){
		
	chips <- colnames(exprs(object))
	table.list <- dbListTables(Con)
	if(is.null(preprocessing)){
		preprocessing=""
	}
	if(is.null(pubmed.id)){
		pubmed.id=""
	}

	if( !("experiments" %in% table.list$relname)){
		if(v){
			cat("creating new experiments table...\n")
		}
		createDBTable(Con,name="experiments",attributes=c("title","description","exp_date","preprocessing","pubmed_id","publish_date"),options=c("","","","","",""),no.transaction=global.transaction,silent=TRUE)
	}
	if( !("arrays" %in% table.list$relname)){
		if(v){
			cat("creating new arrays table...\n")
		}
		createDBTable(Con,name="arrays",attributes=c("arrayname","type","chip"),references="experiments",no.transaction=global.transaction,silent=TRUE)
	}
	
	if( !("samples" %in% table.list$relname)){
		if(v){
			cat("creating new samples table...\n")
		}
		#colname <- replaceAll(slotNames(new("Sample")),old=".",new="_")
		#createDBTable(Con,name="samples",attributes=colname,no.transaction=global.transaction,silent=TRUE)
		createNewDBTable(new("Sample"),Con,no.transaction=global.transaction)
	}

	
	if(!("signal_channels" %in% table.list$relname)){
		if(v){
			cat("creating new signal_channels table...\n")
		}
		createNewDBTable(new("SignalChannel"),Con,no.transaction=global.transaction)
	}
	
			
	if( !("exp_values" %in% table.list$relname)){
		if(v){
			cat("creating new values table...\n")
		}
		createDBTable(Con,name="exp_values",attributes=c("id","value","log_value"),data.types=c("TEXT","float8","BOOL"),references="signal_channels",no.transaction=global.transaction,silent=TRUE)
	}

	if(v){
		cat("finished creating tables...\n")
	}
	if(global.transaction){
		r <- dbSendQuery(Con,"COMMIT TRANSACTION;")
		r <- dbSendQuery(Con,"BEGIN TRANSACTION;")
	}
#		if(global.transaction){
#			dbSendQuery(Con,"COMMIT TRANSACTION;")
#			dbSendQuery(Con,"BEGIN TRANSACTION;")
#			cat("committed...\n")
#		}
			
	if(!is.null(samples)){
		# let's start...
#		sample.pks <- matrix(ncol=length(samples),nrow=1)
		sample.pks <- character(length(samples))
		for(i in 1:length(samples)){
			sample.pks[i] <- getPK(samples[[i]],Con,no.transaction=global.transaction)
		}
	}
	else{
#		sample.pks <- matrix(ncol=1,nrow=1)
		sample.pks <- getPK(new("Sample",name="Unknown"),Con,no.transaction=global.transaction)
	}

	if(is.null(signal.channels)){
		signal.channels <- getSignalChannels(object)
		if(is.null(samples)){
			for(i in 1:length(signal.channels)){
				signal.channels[[i]]@sample.index <- 1
			}
		}
	}
		
	array.names <- getArrays(object)
	##
	# have to check if the SignalChannels match to the samples...
	for(i in 1:length(signal.channels)){
		if(signal.channels[[i]]@sample.index>length(sample.pks)){
			dbSendQuery(Con,"ROLLBACK TRANSACTION")
			stop("The sample.index attribute in the submitted SignalChannel exceeds the number of samples submitted!!\n")
		}
		if(signal.channels[[i]]@array.index>length(array.names)){
			dbSendQuery(Con,"ROLLBACK TRANSACTION")
			stop("The array.index attribute in the submitted SignalChannel exceeds the number of arrays submitted!!\n")
		}
	}
	
	title.db = dbGetResult(dbSendQuery(Con, "SELECT title FROM experiments"))
	
	if (title.db[,1] != exp.name) {
		insertIntoTable(Con,name="experiments",data=c(exp.name,exp.date,date(),description(object)@abstract,preprocessing,pubmed.id),attributes=c("title","exp_date","publish_date","description","preprocessing","pubmed_id"),no.transaction=global.transaction)
	}	else {cat("Experiment added to the experiments table was skipped, it already existed...\n")}
	
	#cat("Experiment added to the experiments table...\n")
	
	#for(i in 1:length(samples)){
	#	current <- samples[[i]]
	#	data <- getContentMatrix(current)
	#	insertIntoTable(Con,name="samples",data=c(data[1,],exp.name),attributes=c(colnames(data),"exp_name"))
	#	
	#}
	##
	# inserting arrays:
	if(v){
		cat("Adding the arrays to the arrays_table\n")
		l.a <- length(array.names)
		p.counter <- 0
		progress(0,steps=40)
	}
	for(i in 1:length(array.names)){
		#cat("array name",array.names[i],"\n")
		insertIntoTable(Con,name="arrays",data=c(array.names[i],object@type,annotation(object),exp.name),attributes=c("arrayname","type","chip","exp_name"),references=new("AutoReference",source.table="arrays",ref.table="experiments",source.table.column="exp_name",ref.table.column="title"),no.transaction=global.transaction)
		if(v){
			p.counter <- p.counter + 1
			progress(p.counter*100/l.a,steps=40)
		}
	}
	if(v){
		progress(100,steps=40)
		cat("\nfinished\n")
		cat("Adding the signal channels to the signal_channels table\n")
		l.c <- length(chips)
		progress(0,steps=40)
		p.counter <- 0
	}

	sig.channel.pks <- matrix(ncol=length(chips),nrow=1)
	array.ref <- new("AutoReference",source.table="signal_channels",ref.table="arrays",source.table.column="array_index",ref.table.column="arrays_pk")
	sample.ref <- new("AutoReference",source.table="signal_channels",ref.table="samples",source.table.column="sample_index",ref.table.column="samples_pk")
	for(i in 1:length(chips)){
		subquery <- paste("( SELECT experiments_pk FROM experiments WHERE title=",rpgsql.format.values(exp.name),")")
		res <- dbSendQuery(Con,"SELECT arrays_pk FROM arrays WHERE arrayname=",rpgsql.format.values(array.names[signal.channels[[i]]@array.index]),"AND experiments_fk=",subquery)
			
		#array.pks[i] <- as.character(dbGetResult(res)[[1]])
		##
		# inserting the signal channel...
		sig.channel <- getContentMatrix(signal.channels[[i]])
		sig.channel[1,"array_index"] <- as.character(dbGetResult(res)[[1]])
		
		sig.channel[1,"sample_index"] <- as.character(sample.pks[as.numeric(sig.channel[1,"sample_index"])])
		insertIntoTable(Con,name="signal_channels",data=sig.channel,references=list(array.ref,sample.ref),no.transaction=global.transaction)
		sig.channel.pks[i] <- as.character(dbGetResult(dbSendQuery(Con,"SELECT signal_channels_pk FROM signal_channels WHERE arrays_fk=",rpgsql.format.values(sig.channel[1,"array_index"]),"AND color=",rpgsql.format.values(sig.channel[1,"color"])))[[1]])
		if(v){
			p.counter <- p.counter + 1
			progress(p.counter*100/l.c,steps=40)
		}
	}
		
	if(v){
		progress(100,steps=40)
		cat("\nfinished\n")
		cat("Inserting the expression values in the exp_values table\n")
		l.c <- length(chips)
		progress(0,steps=40)
		p.counter <- 0
	}
		#cat(paste("please wait for",length(chips),"dots:"))
	log.values <- TRUE #.is.log(exprs(object))
	
	##
	# if object@weights -> ALTER TABLE exp_values ADD COLUMN weights float4
	##
	for(i in 1:length(chips)){
		exp <- exprs(object)[,i]
		data <- data.frame(id=rownames(exprs(object)),value=exp,log_value=rep(log.values,length(exp)),channel=rep(sig.channel.pks[i],length(exp)))
		insertIntoTable(Con,name="exp_values",data=data,references=new("AutoReference",source.table="exp_values",ref.table="signal_channels",source.table.column="channel",ref.table.column="signal_channels_pk"),attributes=c("id","value","log_value","channel"),no.transaction=global.transaction)
		if(v){	
			p.counter <- p.counter+1
			progress(p.counter*100/l.c,steps=40)
		}
	}
	if(v){
		progress(100,steps=40)
		cat("\nfinished\n\n")
	}
	if(global.transaction){
		r <- dbSendQuery(Con,"COMMIT TRANSACTION")
	}
}
