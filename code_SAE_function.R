MM<-function(T,p,ti,tf,k,m,liste)
  #fonction qui crée une matrice de moyenne mobile par apport a une liste
  #entrée:
  #T->temps total d'analyse (integer)
  #P->periode(integer)
  #ti-> temps initial(integer)
  #tf->temps finale(integer)
  #k-> k(integer)
  #m-> ordre de la moyenne(integer)
  #liste-> la liste des donnée temporelles (data)
  #sortie:
  #matrice M
{
  f<-rep(0,length = T)
  #initialisation de la premiere moyenne mobile
  f[1]<-0
  for(i in (ti-k):(ti+k))
  {
    f[1]<-f[1]+liste[i]/m
  }
  #implentation de la première moyenne mobile
  M<-matrix(nrow = p,ncol = T/p)
  M[1,7]<-f[1]
  #création de la matrice de moyenne mobile
  for (i in 8:(tf-ti+2))
  {
    f[i]<-f[i-1]-(liste[ti+i-k-2]/m) +(liste[ti+i]/m)
    M[i]<-f[i]
  }
  return(M)
}