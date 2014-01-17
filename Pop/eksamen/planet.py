# -*- coding: cp1252 -*-

import math
import numpy as np 

class Planet(object):

########################
##        API         ##
########################

    def find_acceleration_n_tmp_pos(self, sos, tmp_pos):
        """kalder find_acceleration_n med en midlertidig position tmp_pos"""
        # Gemmer den nuværende position og sætter planeten til den midlertige
        # position.
        save_pos = self.position 
        self.position = tmp_pos

        # Udregner accelerationen af planeten på den nye position
        ret = self.find_acceleration_n(sos)

        # Henter den gamle position og returnere accelerationen
        self.position = save_pos
        return ret

    def find_acceleration_2(self, planet):
        """finder object planetens acceleration i forhold til en anden planet
           so"""
        rvector = self.position - planet.position
        r = np.sqrt(rvector[0] ** 2 + rvector[1] ** 2 + rvector[2] ** 2)
        return -(planet.GM / r ** 3.0 * rvector)

    def find_acceleration_n(self, planets):
        """finder object planetens acceleration i forhold til listen
           af planeter 'planets'"""
        acc = 0.0 
        for planet in planets: # Udregner og summere accelerationen af object 
            if self != planet: # planeten ud fra alle andre planeter 'planets'
                acc += self.find_acceleration_2(planet) 
        return acc

#######################
## Buildin Functions ##
#######################

    def __init__(self, *args):
        """initiere planeten med den givne position, hastighed og GM"""
        self.position = np.array(args[0], dtype=float) # position vector as list (r)
        self.velocity = np.array(args[1], dtype=float) # velocity vector as list (v)
        self.GM = args[2]            # gravitational constant for planet as float (GM)

################
##    Test    ##
################
if __debug__:
   """tester planet klassen"""
   solen = Planet([10.0,0.0,10.0],[0.0,0.0,0.0],2.959122082322128 * 10 ** -4, True) 
   planetX = Planet([10,-100.0,10.0],[10.0,10.0,1.0], 1.945211846204988 * 10 ** -12, False)
   planetX2 = Planet([0,-100.0,10.0],[100.1,10.0,1.0], 1.945211846204988 * 10 ** -12, False)
   print planetX.find_acceleration_n([solen, planetX, planetX2])

    

