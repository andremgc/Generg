#' @export

ftheta<-function(Beta_gama,theta.z,gama.s){
  cos.theta<-cos(theta.z*pi/180)*cos(Beta_gama[1]*pi/180)+sin(theta.z*pi/180)*sin(Beta_gama[1]*pi/180)*cos(gama.s*pi/180-Beta_gama[2]*pi/180)
  #gama era 0 virou 180?
  cos.theta[cos.theta<=0]<-0

  theta<-acos(cos.theta)*180/pi
}