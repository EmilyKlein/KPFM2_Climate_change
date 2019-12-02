load.funcs<-function(path.string="C:/Users/emily/Desktop/SWFSC/Proposals, paperwork, etc/Klein_et_al_ClimateChg_PLOS/GGP input and code/Code and wrappers"){
  tt.list<-list.files(path.string)
  for(i in 1:length(tt.list)){
    source(paste(path.string,"/",tt.list[i],sep=""))
  }
}