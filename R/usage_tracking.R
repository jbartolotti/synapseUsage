
#' @export
synapseFigs <- function(logdir = '~', savedir = '~/R-Drive/Bartolotti_J/sysadmin/usagelogs', 
                        usage_filename = 'usage.txt', processes_filename = 'processes.txt', daysback = 7
){
  timenow <- Sys.time()
  dir.create(file.path(savedir,'usage_figs'), showWarnings = FALSE)
  datestring <- format(timenow, "%y%m%d_%H%M")
  imported <- importData(logdir, savedir, usage_filename, processes_filename, datestring)
  firstday <- '22-03-31'
  markers <- getMarkers(firstday, timenow, daysback)
  returndat <- analyzeProcesses(imported$prc)
  generateFigs(returndat$proc_grid_cpu, returndat$proc_cpu, imported$usg, markers, savedir)
}


importData <- function(logdir, savedir, usage_filename, processes_filename, datestring){
  imported <- list()
  
  #copy files to target
  usage_filename_split <- strsplit(usage_filename,'[.]')[[1]]
  processes_filename_split <- strsplit(processes_filename,'[.]')[[1]]
  
  usage_target_filepath <- file.path(savedir,sprintf('%s_%s.%s',usage_filename_split[1],datestring,usage_filename_split[2]))
  processes_target_filepath <- file.path(savedir,sprintf('%s_%s.%s',processes_filename_split[1],datestring,processes_filename_split[2]))
  system(sprintf('cp %s %s',file.path(logdir,usage_filename), usage_target_filepath))
  system(sprintf('cp %s %s',file.path(logdir,processes_filename), processes_target_filepath))
  
  #import CPU usage
  usg <- read.delim(usage_target_filepath,header = FALSE,stringsAsFactors = FALSE, sep = '')
  usg <- usg[,c(2,4,6)]
  names(usg) <- c('epoch','cpu_pct','mem_pct')
  usg$cpu <- unlist(lapply(usg$cpu_pct, function(x){as.numeric(gsub('%','',x))}))
  usg$mem <- unlist(lapply(usg$mem_pct, function(x){as.numeric(gsub('%','',x))}))
  
  #import process log
  prc <- readLines(processes_target_filepath)
  
  #return
  imported$usg <- usg
  imported$prc <- prc
  return(imported)
}

getMarkers <- function(firstday, timenow, daysback){
  m <- list()
  m$first_day_epoch <- as.numeric(as.POSIXct(paste(firstday,'00:00:00',sep = '_'),format = '%y-%m-%d_%H:%M:%S'))
  m$end_time <- timenow
  m$end_time_epoch = as.numeric(as.POSIXct(timenow,format = '%y-%m-%d %H:%M:%S'))
  m$hour_markers = seq(m$first_day_epoch,m$end_time_epoch,by=60*60)
  m$four_hour_markers = seq(m$first_day_epoch,m$end_time_epoch,by=60*60*4)
  m$day_markers = seq(m$first_day_epoch,m$end_time_epoch,by = 60*60*24)
  
  #cpu labels for figures
  m$cpu_breaks = seq(0,100,100/24)
  m$cpu_labels = 0:24
  m$cpu_labels[seq(2,24,2)] = ''
  
  m$xstart <- max(m$end_time_epoch - 60*60*24*daysback, max(m$day_markers))
  m$xend <- m$end_time_epoch
  
  return(m)
}

analyzeProcesses <- function(prc){
  linetops <- which(unlist(lapply(prc,function(x){grepl('top -',x)})))
  all_secs = as.numeric()
  all_dfs = list()
  for(i in 1:(length(linetops)))
  {
    firstline <- linetops[i]+2
    if (i == length(linetops)){
      lastline <- length(prc)
      }else{
    lastline <- linetops[i+1]-2
    }
    epochline <- linetops[i]-1
    #only read if there are some non-root processes for this reading. Otherwise make an empty df
    if(lastline - firstline > 0)
    {
      thisepoch <- strsplit(prc[epochline],' ')[[1]][2]
      thisdat <- prc[firstline:lastline]
      thisdf <- read.delim(textConnection(thisdat),sep = '', stringsAsFactors = FALSE,header = FALSE)
      thisdf <- thisdf[,1:12]
      thisdf[,13] = as.numeric(thisepoch)
      names(thisdf) = c('PID','USER','PR','NI','VIRT','RES','SHR','S','CPU','MEM','TIME','COMMAND','epoch')
      all_dfs[[i]] = thisdf
    } else {
      all_dfs[[i]] = data.frame(
        PID = as.numeric(), USER = as.character(), PR = as.numeric(), NI = as.numeric(), 
        VIRT = as.character(), RES = as.character(), SHR = as.numeric(), S = as.character(), 
        CPU = as.numeric(), MEM = as.numeric(), TIME = as.character(), COMMAND = as.character(), 
        epoch = as.numeric(), stringsAsFactors = FALSE )
    }
    
  }
  
  alldat <- do.call('rbind',all_dfs)
  
  proc_cpu = alldat %>% group_by(USER,epoch) %>% summarize(sum(CPU))
  names(proc_cpu)[3] = 'CPU'
  proc_cpu_command = alldat %>% group_by(USER,epoch,COMMAND) %>% summarize(sum(CPU))
  names(proc_cpu_command)[4] = 'CPU'
  proc_cpu_command$USER_COMMAND = paste(proc_cpu_command$USER, proc_cpu_command$COMMAND,sep = '_')
  
  #include NAs in the process data for unpresent combinations
  proc_cpu_grid <- merge(proc_cpu,
                  expand.grid(USER=unique(proc_cpu$USER),
                              epoch=unique(proc_cpu$epoch),
                              stringsAsFactors=F),
                  all.y=T)
  proc_cpu_grid$CPU[is.na(proc_cpu_grid$CPU)] <- 0
  
  returndat <- list()
  returndat$alldat
  returndat$proc_cpu
  returndat$proc_cpu_command
  returndat$proc_cpu_grid
  
  return(returndat)
}

generateFigs <- function(proc_grid_cpu, proc_cpu, usg, m, savedir){

  #Process Figures
  ggplot(proc_grid_cpu, aes(x = epoch, y = CPU/2400*100,fill = USER)) +
    theme_bw() +
    geom_hline(yintercept = 100, color = 'black') +
    geom_vline(xintercept = m$hour_markers,color = '#CCCCCC') +
    geom_vline(xintercept = m$four_hour_markers,color = '#888888') +
    geom_area(position = 'stack',alpha = 1) +
    geom_vline(xintercept = m$day_markers,color = 'black', size = 1) +
    #  geom_point(size = 1 ) +
    #  geom_line(size = 1 ) +
    scale_x_continuous(breaks = m$day_markers, labels = as.character(as.POSIXct(m$day_markers,origin="1970-01-01"))) + 
    scale_y_continuous(breaks = m$cpu_breaks, labels = m$cpu_labels) +
    labs(x = '', y = 'Number of CPUs',title = 'Synapse CPU use by user') +
    coord_cartesian(xlim = c(m$xstart,m$xend), ylim = c(0,100)) +
    theme(legend.position = 'none')
  ggsave(file.path(savedir,'usage_figs',sprintf('Synapse_processes_user_%s.png',gsub(':','-',gsub(' ','_',as.character(as.POSIXct(m$end_time,origin="1970-01-01")))))),height = 4, width=7)
  
  
  ggplot(proc_cpu, aes(x = epoch, y = CPU/2400*100,color = USER)) +
    theme_bw() +
    geom_hline(yintercept = 100, color = 'black') +
    geom_vline(xintercept = m$hour_markers,color = '#CCCCCC') +
    geom_vline(xintercept = m$four_hour_markers,color = '#888888') +
    geom_vline(xintercept = m$day_markers,color = 'black', size = 1) +
    geom_point(size = 1, alpha = .7 ) +
    #  geom_line(size = 1 ) +
    scale_x_continuous(breaks = m$day_markers, labels = as.character(as.POSIXct(m$day_markers,origin="1970-01-01"))) + 
    scale_y_continuous(breaks = m$cpu_breaks, labels = m$cpu_labels) +
    labs(x = '', y = 'Number of CPUs',title = 'Synapse CPU use by user') +
    coord_cartesian(xlim = c(m$xstart,m$xend), ylim = c(0,100)) +
    theme(legend.position = 'none')
  ggsave(file.path(savedir,'usage_figs',sprintf('Synapse_processes_user_%s_line.png',gsub(':','-',gsub(' ','_',as.character(as.POSIXct(m$end_time,origin="1970-01-01")))))),height = 4, width=7)
  
  #Usage Figure
  ggplot(usg, aes(x = epoch, y = cpu)) +
    theme_bw() +
    geom_vline(xintercept = m$hour_markers,color = '#CCCCCC') +
    geom_vline(xintercept = m$four_hour_markers,color = '#888888') +
    geom_vline(xintercept = m$day_markers,color = 'black', size = 1) +
    geom_point(color = 'red', size = .5) +
    geom_point(aes(y = mem), color = 'blue',size = .5)+
    scale_x_continuous(breaks = m$day_markers, labels = as.character(as.POSIXct(m$day_markers,origin="1970-01-01"))) + 
    coord_cartesian(xlim = c(m$xstart,m$xend)) +
    labs(x = '', y = 'Percent Usage', title = 'Synapse CPU (red) and RAM (blue) Usage')
  ggsave(file.path(savedir,'usage_figs',sprintf('Synapse_Usage_%s.png',gsub(':','-',gsub(' ','_',as.character(as.POSIXct(m$end_time,origin="1970-01-01")))))),height = 4, width=7)

}

