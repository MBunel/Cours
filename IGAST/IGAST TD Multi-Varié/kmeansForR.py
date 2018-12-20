# coding: utf-8

import math
import random

def distanceMinkowski(v1, v2, p):
    if len(v1) != len(v2):
        print ("Les tableaux sont de longueurs differentes")
        return
    dist = 0.0
    for i in range(len(v1)):
        dist += abs(v1[i] - v2[i])**p
        
    return dist**(1.0/p)

def ppv (FEATURES, V0, p):    
    if FEATURES == None or len(FEATURES) <= 0:
        print ("Le tableau de vecteur est vide")
        return
    distance_min = distanceMinkowski(FEATURES[0], V0, p)
    ppv = 0
    for i in range(len(FEATURES)):
        V = FEATURES[i]
        dist = distanceMinkowski(V, V0, p)
        if dist <= distance_min:
            distance_min = dist
            ppv = i
            
    return (ppv, distance_min)
	
def tirage(n, MIN, MAX):
	m = len(MIN)
	POINTS = []
	for i in range(n):
		point = []
		for j in range(m):
			point.append(random.uniform(MIN[j], MAX[j]))
		POINTS.append(point)
        
	return POINTS

def moyenneVecteur(FEATURES):
    
    moyenne = []
    n = len(FEATURES[0])
    for i in range(n):
        moyenne.append(0.0)
    
    for i in range (len(FEATURES)):
        vecteurI = FEATURES[i]
        for j in range (n):
            moyenne[j]  = moyenne[j] + vecteurI[j]
            
    # On divise par le nombre de vecteurs
    for i in range(n):
        if moyenne[i] != 0:
            moyenne[i] = moyenne[i] / len(FEATURES)
        # sinon on laisse tel quel
		
    return moyenne

def kmeans(FEATURES, K, p, epsilon):
	N = len(FEATURES)
	indices = random.sample(range(0, N),K)
	CENTRES = []
	
	for i in indices:
		centre = []
		for j in range(len(FEATURES[0])):
			centre.append(FEATURES[i][j])
		CENTRES.append(centre)
	stopAlgo = False
	
	rep = 0
	max = 100
		
	while not stopAlgo and rep < max:
		AFFECTATIONS = []
		for i in range(0, N):
			(index, dist) = ppv(CENTRES, FEATURES[i], p)
			AFFECTATIONS.append(index)
		nbChgt = 0
		for j in range(0, K):
			FEATURES_CENTRE = []
			for i in range(0, N):
				if AFFECTATIONS[i] == j:
					FEATURES_CENTRE.append(FEATURES[i])
			if len (FEATURES_CENTRE) > 0:
				moyenne = moyenneVecteur(FEATURES_CENTRE)
			else:
				moyenne = CENTRES[j]        
			if math.fabs(moyenne[0] - CENTRES[j][0]) > epsilon:
				nbChgt += 1
			CENTRES[j] = moyenne
		if nbChgt <= 0:
			stopAlgo = True
		rep += 1

	out = {}
	
	out['AFFECTATIONS'] = AFFECTATIONS
	out['CENTRES'] = CENTRES
		
	return out