# -*- coding: cp1252 -*-
import numpy as np
import matplotlib.pyplot as plt
import math


# global variables

M = 1
FTyngde = np.float64([0,-9.82])
Speed = np.float64([100,100])
C = 0.00027778 


########################
##        API         ##
########################

def plot():
    i = 0.0
    values = np.array([])
    while 1:
       (f,newvalue) = P(i)
       if newvalue < 0:
           break
       values = np.append(newvalue, values)
       i += 1 
    plt.plot(values, 'purple')    

    for (dt,color) in [(0.25,'blue'),(1,'red'),(4,'green')]: 
        i = 0.0
        values = np.array([])
        while 1: #uden luftmodstand
            (x, y) = Pdelta(i, dt, 0)
            if y < 0:
                break
            values = np.append(y, values)
            i += 1
        plt.plot(values, color, linestyle='--')

        values = np.array([])
        i = 0
        (xprev,yprev) = (0,0)

        while 1:
            vl = math.sqrt(xprev ** 2 + yprev ** 2) 
            print vl
            (x,y) = Pdelta(i, dt, vl) 
            if y < 0:
                break
            values = np.append(y, values)
            (xprev, yprev) = (x,y)
            i += 1
        plt.plot(values, color, linestyle=':')
    plt.show()

############################
##    Hjælpefunktioner    ##
############################

def P(t):
    return 0.5 * FTyngde * (t ** 2) + Speed * t  

def V(t):
    return FTyngde * t + Speed

def A(t, vl):
    FLuft = - C * vl * V(t) 
    A = (FTyngde + FLuft)/M
    return V(t) + A
    
def Vdelta(t, dt, vl):
    Vnow = V(t)
    return Vnow + A(t, vl) * dt

def Pdelta(t, dt, vl):
    Vt = Vdelta(t, dt, vl) 
    return P(t) + Vt 

################
##    Test    ##
################

    # TODO
    #####################
    ##   Intern Test   ##
    #####################

    ######################
    ##   Ekstern Test   ##
    ######################



