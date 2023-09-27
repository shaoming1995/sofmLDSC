#' @title 用于计算两个性状之间的遗传度以及遗传关联性
#' @param name 学号
#' @param key 密码
#' @param GWAS1summary 预处理好的GWAS summay
#' @param GWAS1name GWAS summay名称
#' @param pop 参考人群EUR欧洲 EAS亚洲
#' @export
sofmldsc_h2<-function(name,key,GWAS1summary,GWAS1name,pop){
  A<-name
  A<-as.numeric(gsub("DK","00",A))
  C<-A+key
  C<-C/10000
  if(C==231){
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
    h2_res <- ldscr::ldsc_h2(munged_sumstats = GWAS1name1, ancestry = pop)
    h2_res}else{
      cat("请联系微信联系SFM19950928获取密钥")
    }
}
