# -*- coding: cp1252 -*-

from kursusuge5modul import *

# global variables 
HEIGHT = 32
WIDTH = 32
LEVER = []
# jeg har valgt at skrive globale variabler i store bogstaver for
# nemmere at kunne adskilde dem fra lokale variabler og funktioner

########################
##        API         ##
########################

def foerste():
    """Starter modelen"""
    return (0,HEIGHT-1,0,WIDTH-1)

def naeste():
    """itterere modelen en tidsenhed"""
    global LEVER 

    lever_n = []
    for i in range(0,HEIGHT): 
        for j in range(0, WIDTH):
            if __check_om_skal_leve(i,j): 
                lever_n.append((i,j))

    LEVER = lever_n
    return (0,HEIGHT-1,0,WIDTH-1)

def levende(i,j):
    """returnere true hvis (i,j) er et levende element"""
    return (i,j) in LEVER

def klik(i,j):
    """hvis (i,j) er levende bliver den dræbt, hvis den er død bliver den
       opliver"""
    if levende(i,j):
        LEVER.remove((i,j))
    else:
        LEVER.append((i,j))

############################
##    Hjælpefunktioner    ##
############################

def __levende_omkring(levende_naboer,(i,j)):
    """returnere levende_naboer + 1 hvis (i,j) er et levende element"""
    if levende(i,j): 
        levende_naboer += 1
    return levende_naboer


def __alle_naboer(i,j):
    """returnere en liste med elementets 8 naboer"""
    return [(i-1,j+1), (i  ,j+1), (i+1,j+1), 
            (i-1,j  ),            (i+1,j  ),
            (i-1,j-1), (i  ,j-1), (i+1,j-1)]

def __check_om_skal_leve(i,j):
    """checker om det pågældene element burde leve, og gemmer elementet i
       listen lever_n hvis dette er tilfældet"""
    antal_levende = reduce(__levende_omkring, __alle_naboer(i,j), 0)
    if not levende(i,j):
        if antal_levende == 3: # hvis den er død og 3 levende omkring
           return True 
    else:
        if antal_levende in [2,3]: # hvis den lever og 2-3 levende omkring
           return True
    return False # hvis den skal være død

def gentager(f, n):
    def rfun(p):
        return reduce(lambda x, _: f(x), xrange(n), p)
    return rfun

################
##    Test    ##
################
def test():
    """Tester programmet"""
    #####################
    ##   Intern Test   ##
    #####################
    LEVER = [(1,1),(1,2),(1,3)]
    #test for tjek alle naboer
    print '__alle_naboer', (3,4), __alle_naboer(3,4) == [(2,5),(3,5),(4,5),(2,4),(4,4),(2,3),(3,3),(4,3)]
    print '__alle_naboer', (0,0), __alle_naboer(0,0) == [(-1,1),(0,1),(1,1),(-1,0),(1,0),(-1,-1),(0,-1),(1,-1)]
    #tester __check_om_skal_leve
    print '__check_om_skal_levë́', (0,2), __check_om_skal_leve(0,2) # er død skal leve
    print '__check_om_skal_levë́', (2,3), not __check_om_skal_leve(2,3) # er død skal dø
    print '__check_om_skal_levë́', (1,2), __check_om_skal_leve(1,2) # er levende skal leve
    print '__check_om_skal_levë́', (1,3), not __check_om_skal_leve(1,3) # er levende skal dø
    # Tester levende omkring
    print '__levende_omkring', (0,(1,1)), __levende_omkring(0,(1,1)) == 1 # skal addere en hvis (1,1) er levende 
    print '__levende_omkring', (0,(2,1)), __levende_omkring(0,(2,1)) == 0 # skal ikke addere en da (2,1) er død 
    # tester levende
    print 'levende', (1,1), levende(1,1) # tester levende
    print 'levende', (3,3), not levende(3,3) # tester død
    # tester klik
    klik(1,1)
    print 'klik', (1,1), not levende(1,1) # starter levende skal dø
    klik(1,1)
    print 'klik', (1,1), levende(1,1) # starter død skal leve
    # tester foerste
    print 'foerste', (), foerste() == (0,31,0,31)
    #tester naeste
    naeste()
    print 'naeste', (), LEVER == [(0,2),(1,2),(2,2)]
    ######################
    ##   Ekstern Test   ##
    ######################
    LEVER = []
    klik(0,0)
    klik(1,0)
    klik(2,0)
    naeste()
    print 'klik søjle i siden 1 naeste', LEVER == [(1,0),(1,1)]
    naeste()
    print 'klik søjle i siden 2 naeste', LEVER == []
    klik(10,10)
    klik(11,11)
    klik(10,11) # statisk figur
    klik(11,10)
    gentager(naeste,10)
    print 'statisk firkant 10 naeste', LEVER == [(10,10),(11,11),(10,11),(11,10)]


visLife(foerste, naeste, levende, klik)




