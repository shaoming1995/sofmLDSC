#' @title 激活ldsc函数
#' @param name 学号
#' @param path 激活包的存放路径
#' @param key 密码
#' @export
Activate_ldsc<-function(name,key,path){
  A<-name
  A<-as.numeric(gsub("DK","00",A))
  C<-A+key
  C<-C/10000
  if(C==231){
    pkg_path1<-paste0(path,"/ActivateLDSC.zip")
    devtools::install_local(pkg_path1)
    cat("环境已经配置完成ldsc所有函数已被激活")}else{
      cat("环境配置失败ldsc函数未被激活请联系微信联系SFM19950928获取")
    }}
