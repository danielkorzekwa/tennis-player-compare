#Example for Parameter Estimation for Linear Dynamical Systems by Zoubin Ghahramani
#Geoffrey E. Hinton, 1996
#
# author Daniel Korzekwa
#
function lds_learn()
 
  n = 10000000; # number of data points
  u = rand(n,1); #parent node
  x = u*2 + normrnd(0,3,n,1); #child node, linear gaussian model
  
  #Estimating Q (p(x|u) = N(mu,Q) following Daphne Koller, Probabilistic Graphical Models book, 
  #Chapter 17.2.4 Gaussian Bayesian Networks
  A_Koller = mean(x)/mean(u);
  Q_Koller = cov(x,x,1) - A_Koller*A_Koller*cov(u,u,1);
  
  #Estimating Q following Zoubin Ghahramani and Geoffrey E. Hinton, 1996
  P_t_xx = var(x,1) + mean(x)^2;
  P_t_uu = var(u,1) + mean(u)^2;
  P_t_xu = cov(u,x,1) + mean(x)*mean(u);
  A_Zoubin = P_t_xu/P_t_uu;
  Q_Zoubin = P_t_xx - A_Zoubin*P_t_xu;
  
  #Estimating Q following Christopher M. Bishop, Pattern Recognition and Machine Learning,2009
  #Chapter  13.3.2 Learning in LDS
  E_xx = var(x,1) + mean(x)^2;
  E_uu = var(u,1) + mean(u)^2;
  E_xu = cov(u,x,1) + mean(x)*mean(u);
  A_Bishop = E_xu/E_uu;
  Q_Bishop = E_xx - E_xu*A_Bishop - E_xu*A_Bishop + A_Bishop*A_Bishop*E_uu;
  
  sprintf("A_Koller=%f,A_Zoubin=%f, A_Bishop=%f",A_Koller, A_Zoubin, A_Bishop)
  sprintf("Q_Koller=%f, Q_Zoubin=%f, Q_Bishop=%f",Q_Koller, Q_Zoubin, Q_Bishop)
 
endfunction