f_kmeans <- function(data, nclass, p=2){
  
  # On vérifie que le nombre de classes soit
  # un entier (R à tendance à tout définir comme
  # des flotants) et on caste le cas échéant
  if (!is.integer(nclass)){
    nclass <- as.integer(nclass)
  }
  
  # On veut que data soit une matrice
  if (!is.matrix(data)){
    # Exception, la fonction se stope
    # et renvoie un message d'erreur
    stop("data must be a matrix")
  }
  
  # Identique au premier test mais pour p
  if (!is.integer(p)){
    p <- as.integer(p)
  }
  
  # Calcul des minimumns et maximums
  # Comme data est de type *matrix* on peut
  # calculer directement le minimum et le maximum
  # de toute la matrice
  absolute_min = min(data)
  absolute_max = max(data)
  
  # Calcul d'epsilon
  epsilon = 10**(-10.0) * (absolute_max - absolute_min)
  
  # Appel de la fonction python
  out <- m_kmeans$kmeans(data, nclass, p, epsilon)
  
  return(out)
}