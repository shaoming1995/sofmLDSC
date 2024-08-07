#' @title 用于计算两个性状之间的遗传度以及遗传关联性
#' @param keyssh 学号
#' @param GWAS1summary 预处理好的GWAS summay1
#' @param GWAS2summary 预处理好的GWAS summay2
#' @param GWAS1name GWAS summay1名称
#' @param GWAS2name GWAS summay2名称
#' @param pop 参考人群EUR欧洲 EAS亚洲
#' @param ready_data 是否有准备好的GWASname-sumstats-munged.txt.gz文件
#' @export

sofmldsc_gc<-function(keyssh,GWAS1summary,GWAS2summary,GWAS1name,GWAS2name,pop,ready_data=F){
  RegistID_dat <- RegistID_dat
    RegistID_u <- subset(RegistID_dat, IK == keyssh)
    tempid <- paste0(keyssh, "_", Sys.info()["nodename"], "_", 
        RegistID_u$RegistID)
    if (RegistID_u$FINN %in% tempid) {
      if(ready_data==F){
      message("开始准备",GWAS1name,"的数据...")
    test1<-try(GWAS1<- GWAS1summary[,c("other_allele.exposure","effect_allele.exposure","beta.exposure","se.exposure","samplesize.exposure","SNP")])
    if(class(test1)=="try-error"){
      GWAS1<- GWAS1summary[,c("other_allele.outcome","effect_allele.outcome","beta.outcome","se.outcome","samplesize.outcome","SNP")]
    }else{
      colnames(GWAS1)<-c("A2","A1","beta","se","N","SNP")
      GWAS1$Z<-GWAS1$beta/GWAS1$se
    }
    colnames(GWAS1)<-c("A2","A1","beta","se","N","SNP")
    GWAS1$Z<-GWAS1$beta/GWAS1$se
    GWAS1name1<-paste0(GWAS1name,"-sumstats-munged.txt.gz")
    write.table(GWAS1, gzfile(GWAS1name1),
                sep = "\t", row.names = FALSE)
    cat("已经完成",GWAS1name,"的数据准备!")
    test2<-try(GWAS2<- GWAS2summary[,c("other_allele.exposure","effect_allele.exposure","beta.exposure","se.exposure","samplesize.exposure","SNP")])
    if(class(test2)=="try-error"){
      
      GWAS2<- GWAS2summary[,c("other_allele.outcome","effect_allele.outcome","beta.outcome","se.outcome","samplesize.outcome","SNP")]
      message("第一次尝试准备",GWAS2name,"的数据失败，正在进行第二次数据准备...") 
      
    }else{
      colnames(GWAS2)<-c("A2","A1","beta","se","N","SNP")
      GWAS2$Z<-GWAS2$beta/GWAS2$se
    }
    colnames(GWAS2)<-c("A2","A1","beta","se","N","SNP")
    GWAS2$Z<-GWAS2$beta/GWAS2$se
    GWAS2name1<-paste0(GWAS2name,"-sumstats-munged.txt.gz")
    write.table(GWAS2, gzfile(GWAS2name1),
                sep = "\t", row.names = FALSE)
      
  cat("已经完成",GWAS2name,"的数据准备!")}else{
     GWAS1name1<-paste0(GWAS1name,"-sumstats-munged.txt.gz")
      GWAS2name1<-paste0(GWAS2name,"-sumstats-munged.txt.gz")
    rg_res <- ldscr::ldsc_rg(
      munged_sumstats = list(
        GWAS1name1,
        GWAS2name1
      ),ancestry = pop)
    rg<-cbind(rg_res$h2,rg_res$rg)
    rownames(rg)[1]<-GWAS1name
    rownames(rg)[2]<-GWAS2name
    filename<-paste0(GWAS1name,"-",GWAS2name,"遗传度与遗传关联性.csv")
    dir.create("遗传学分析结果")
    filenamepath<-paste0("遗传学分析结果","\\",filename)
    write.csv(rg,filenamepath,quote = F,row.names = T)
    cat("您的遗传度以及遗传关联性分析已经完成请前往文件夹中查看")}}else{
      cat("请联系微信联系SFM19950928获取密钥")
    }
}

