# -*- coding: utf-8 -*- 
#benytter unicode så jeg har æ ø og å

import math

#############################
##          API            ##  
#############################

def convgntsReal(x, n):
    """public function that prints all reals to the console"""
    __convgnts_real(x, n, 0, 1, 1, 0)

#############################
## Internal Implementation ##  
#############################

# Dette er en privat function. Den er navngivet ud fra konventionen givet fra
# pythons offentlige documentation http://docs.python.org/2/tutorial/classes.html
# afsnit 9.6

def __convgnts_real(x0, n, a0, a1, b0, b1): 
   """private recursive function""" 
    if n == 0:
        print "done n = 0"
        return "done"

    if b0 != 0 and x0 == a0/b0: # checker om x = K, og undgår at dividere med nul
        print "K = x"           # ved hjælp af pythons dovne udregning
        return "stopped"      

    q = int (math.floor(x0))
    try:
        x1 = 1/(x0 - q) 
    except ZeroDivisionError as detail: #Fanger divition med 0
        print "runtime error:", detail
        return "error" # Begge returns ville være en break hvis jeg benyttede
                       # en lykke

    a2 = q * a1 + a0
    b2 = q * b1 + b0

    print (str(a2)+'/'+str(b2))

    __convgnts_real(x1, n-1, a1, a2, b1, b2) # Jeg har benyttet halerecursion da
                                             # jeg mener det giver en mere elegant 
                                             # kode end en whilelykke ville
