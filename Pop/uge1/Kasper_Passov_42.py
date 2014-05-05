"""
Copyright 2013 Kasper Passov, The one code to rule them all.
"""

import math 
import operator
import numpy

#############################
##          API            ##  
#############################

def test():
    (x1,x2) = findRootNumpy(10,-41347528,119)
    return {x1,x2}

def findRootNumpy(a,b,c):
    return numpy.roots([a,b,c])

def findRoot(a,b,c):
    return __calcRoot__(a,b,c)

#############################
## Internal Implementation ##  
#############################

def __calcRoot__(a,b,c):
    if b <= 0:
        x1 = __hfunc__(a,b,c,operator.add) #Parser add samt parametrene til hfunc 
    else:
        x1 = __hfunc__(a,b,c,operator.sub) 
    if x1 != 0:
        x2 = (c/a)/x1 
    else:
        x2 = (-b)/a
    return (x1,x2)

############################
##    Helper Functions    ##  
############################

def __hfunc__(a,b,c,op):                     #Jeg har lavet denne her function, ikke ndvendigvis 
    try:                                     #fordi den er ndvendig, men fordi jeg gerne ville
        bb4ac = math.sqrt(b * b - 4 * a * c) #prve at parse en operator til en funktion
        bbb4ac = op((-1 * b), bb4ac)
        return bbb4ac/(2 * a)
    except error:
        print "How! kan ikke tage roden af et negativt tal"
    return 0
