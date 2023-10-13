
#This is a bash script that must be installed on the system that this Rscript will call to process the command names.
#==============
# #!/bin/bash
#
#cmd=$1
#
## Check /usr/local/bin no subdirs
#ulbin=$(find /usr/local/bin -maxdepth 1 -name ${cmd} -type f -executable | head -1)
#if [ -z "$ulbin" ]; then
##If it's not in /usr/local/bin, search its subdirectories
#appbin=$(find /usr/local/bin -name ${cmd} -type f -executable | head -1)
#if [ -z "$appbin" ]; then
## If it's not in an app bin, search freesurfer
#fsbin=$(find /usr/local/freesurfer/*/bin -name ${cmd} -type f -executable | head -1)
#if [ -z "$fsbin" ]; then
## It's nowhere I can find, return it as is.
#orig=$cmd
#else
#  # It's in freesurfer
#  orig='freesurfer'
#fi
#else
#  # It's in an app in /usr/local/bin
#  orig=$(echo ${appbin}/ | sed 's:/usr/local/bin/\([^/]*\)/.*:\1:')
#fi
#else
#  # It's in /usr/local/bin
#  orig=$(echo ${ulbin}/ | sed 's:/usr/local/bin/\([^/]*\)/.*:\1:')
#fi
#
#echo $orig
#==============


fixCommand <- function(line){
  cols <- strsplit(trimws(line), "\\s+")[[1]] # split by whitespace
  cols[12] <- paste(cols[12:length(cols)], collapse = "_") # concatenate columns 12 and onwards
  cols <- cols[1:12] # keep only the first 12 columns
  return(paste(cols, collapse = ' '))
}

yest <- Sys.Date()-1

yesterday <- format(yest , "%Y%m%d")


synapse_processes <- readLines(paste0("/home/sa-j186b025/logs/synapse_processes.", yesterday))
epoch_ix <- grep("^EPOCH", synapse_processes)

allapp <- list()
alluser <- list()
for(index in 1:length(epoch_ix)){


ix <- epoch_ix[index]
ep <- synapse_processes[ix]
if(index == length(epoch_ix)){
proc <- synapse_processes[(ix+3):length(synapse_processes)]
} else {
ix_next <- epoch_ix[index+1]
proc <- synapse_processes[(ix+3):(ix_next-1)]
}

cleanproc <- unlist(lapply(proc, fixCommand))
procdf <- read.table(text = cleanproc, header = FALSE)
colnames(procdf) <- c('pid','user','pr','ni','virt','res','shr','s','cpu','mem','time','command')


apptotal <- aggregate(cbind(cpu, mem) ~ command, data = procdf, 'sum')
usertotal <- aggregate(cbind(cpu, mem) ~ user, data = procdf, 'sum')

apptotal$cpu <- apptotal$cpu/100 #num cpus out of the max 24
apptotal$mem <- apptotal$mem/100*64 #num gbs out of the max 64
usertotal$cpu <- usertotal$cpu/100 #num cpus out of the max 24
usertotal$mem <- usertotal$mem/100*64 #num gbs out of the max 64

cpupct <- as.numeric(strsplit(ep, ' ')[[1]][4])
cpunum <- cpupct/100*24
mempct <- as.numeric(strsplit(ep, ' ')[[1]][6])
memnum <- mempct/100*64
apptotal[dim(apptotal)[1]+1,] <- c('OTHER', cpunum - sum(apptotal$cpu), memnum - sum(apptotal$mem))
usertotal[dim(usertotal)[1]+1,] <- c('OTHER', cpunum - sum(usertotal$cpu), memnum - sum(usertotal$mem))

apptotal$timeindex <- index
usertotal$timeindex <- index

allapp[[index]] <- apptotal
alluser[[index]] <- usertotal
}
aa <- do.call('rbind',allapp)
au <- do.call('rbind',alluser)
aa$cpu <- as.numeric(aa$cpu)
aa$mem <- as.numeric(aa$mem)
au$cpu <- as.numeric(au$cpu)
au$mem <- as.numeric(au$mem)

#We want to group the commands by seeing what app they're from. If there's a + in the command, replace it with * before finding.
#First, search /usr/local/bin maxdepth 1.
#If no match, then search /usr/local/bin (any depth).
#If no match, then search /usr/local/freesurfer/*/bin
#If still no match. return itself. Otherwise return the processed match.
cmdlist <- data.frame(cmd = unique(aa$command), orig = NA, stringsAsFactors = FALSE)
cmdlist$orig <- unlist(lapply(cmdlist$cmd, function(x){
  system2('expandCommand', c(gsub('[+]','*',x)), stdout = TRUE, stderr = FALSE)
  }))
cmdlist$orig[grepl('x2go',cmdlist$cmd)] = 'x2go-lxqt'
cmdlist$orig[grepl('lxqt',cmdlist$cmd)] = 'x2go-lxqt'

aa$cmdgroup <- unlist(lapply(aa$command, function(x){cmdlist$orig[cmdlist$cmd == x]}))

agapp <- aggregate(cbind(cpu,mem) ~ cmdgroup, aa, sum)
appcpu <- sum(agapp$cpu)
appmem <- sum(agapp$mem)
agapp$cpupct <- agapp$cpu/appcpu*100
agapp$mempct <- agapp$mem/appmem*100

plotcpu <- agapp[agapp$cpupct >=1,c('cmdgroup','cpupct')]
plotcpu$cpupct <- as.numeric(plotcpu$cpupct)
less1 <- sum(agapp$cpupct[agapp$cpupct < 1])
less1_label <- sprintf('%.1f%%: %s',less1, paste(agapp$cmdgroup[agapp$cpupct < 1], collapse = ', '))
plotcpu$lab <- unlist(lapply(1:dim(plotcpu)[1], function(x){sprintf('%.1f%%: %s',plotcpu$cpupct[x], plotcpu$cmdgroup[x])}))
plotcpu[dim(plotcpu)[1]+1,] <- c('less1',less1,stringr::str_wrap(less1_label, width = 30))
plotcpu$cpupct <- as.numeric(plotcpu$cpupct)
plotcpu$labF <- factor(plotcpu$lab, levels = plotcpu$lab[rev(order(plotcpu$cpupct))])
plotcpu$cpupct <- as.numeric(plotcpu$cpupct)

ggplot2::ggplot(plotcpu, ggplot2::aes(x = 1, y = cpupct, fill = labF)) +
  ggplot2::theme_bw() +
  ggplot2::geom_bar(stat = 'identity', position = 'stack') +
  ggplot2::labs(x = '', y = '% CPU Usage by Application', title = sprintf('%s Synapse CPU Usage by App', format(yest, '%Y-%m-%d'))) +
  ggplot2::scale_fill_brewer(type = 'qual', palette = 'Set1', name = 'Application') +
  ggplot2::theme(axis.ticks.x = ggplot2::element_blank(), axis.text.x = ggplot2::element_blank()) +
  ggplot2::scale_x_continuous(expand = c(0,0)) +
  ggplot2::scale_y_continuous(expand = c(0,.1))
ggplot2::ggsave(sprintf('/home/sa-j186b025/logfigs/synapse_proc_appcpu.%s.png',yesterday), width = 4, height = 4)



plotmem <- agapp[agapp$mempct >=1,c('cmdgroup','mempct')]
plotmem$mempct <- as.numeric(plotmem$mempct)
less1 <- sum(agapp$mempct[agapp$mempct < 1])
less1_label <- sprintf('%.1f%%: %s',less1, paste(agapp$cmdgroup[agapp$mempct < 1], collapse = ', '))
plotmem$lab <- unlist(lapply(1:dim(plotmem)[1], function(x){sprintf('%.1f%%: %s',plotmem$mempct[x], plotmem$cmdgroup[x])}))
plotmem[dim(plotmem)[1]+1,] <- c('less1',less1,stringr::str_wrap(less1_label, width = 30))
plotmem$mempct <- as.numeric(plotmem$mempct)
plotmem$labF <- factor(plotmem$lab, levels = plotmem$lab[rev(order(plotmem$mempct))])
plotmem$mempct <- as.numeric(plotmem$mempct)

ggplot2::ggplot(plotmem, ggplot2::aes(x = 1, y = mempct, fill = labF)) +
  ggplot2::theme_bw() +
  ggplot2::geom_bar(stat = 'identity', position = 'stack') +
  ggplot2::labs(x = '', y = '% RAM Usage by Application', title = sprintf('%s Synapse RAM Usage by App', format(yest, '%Y-%m-%d'))) +
  ggplot2::scale_fill_brewer(type = 'qual', palette = 'Set1', name = 'Application') +
  ggplot2::theme(axis.ticks.x = ggplot2::element_blank(), axis.text.x = ggplot2::element_blank()) +
  ggplot2::scale_x_continuous(expand = c(0,0)) +
  ggplot2::scale_y_continuous(expand = c(0,.1))
ggplot2::ggsave(sprintf('/home/sa-j186b025/logfigs/synapse_proc_appram.%s.png',yesterday), width = 4, height = 4)




