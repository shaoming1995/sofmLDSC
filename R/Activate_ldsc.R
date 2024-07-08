#' @title 激活ldsc函数
#' @param keyssh 学号
#' @param path 激活包的存放路径
#' @export
Activate_ldsc<-function(keyssh,path){
 RegistID_dat <- RegistID_dat
  RegistID_u <- subset(RegistID_dat, IK == keyssh)
  tempid <- paste0(keyssh, "_", Sys.info()["nodename"], "_",
                   RegistID_u$RegistID)
  if (RegistID_u$FINN %in% tempid) {
    pkg_path1<-paste0(path,"/ActivateLDSC.zip")
    devtools::install_local(pkg_path1)
    cat("环境已经配置完成ldsc所有函数已被激活")}else{
      cat("环境配置失败ldsc函数未被激活请联系微信联系SFM19950928获取")
    }}
