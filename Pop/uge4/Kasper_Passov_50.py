# -*- coding: cp1252 -*-
#benytter unicode så jeg har æ ø og å
"""Skrevet af Kasper Passov"""

import matplotlib.pyplot as plt

#############################
##          API            ##  
#############################

def plot_egg():
    """plots the file flueaeg.txt"""
    try:
        ht = dan_hashtabel('flueaeg.txt')
        plt.plot(* linRegrAn(ht))
        plt.title("flue" + u"æ" + "g.txt")
        plt.xlabel('tid')
        plt.ylabel("antal " + u"æ" + "g")
        plt.show()
    except SyntaxError as detail:
        print "Could not parse file", detail
    except NameError as detail:
        print "Could not parse file", detail

def linRegrAn(d):
    """finds f(xmin) and f(xmax) of a dictonary d"""
    xtotal = 0; ytotal = 0; xmin = float('+inf'); xmax = float('-inf')

    for x,y in d.items():
        xtotal += x
        ytotal += y
        xmax = max(xmax,x)
        xmin = min(xmin,x)

    xmean = xtotal/len(d)
    ymean = ytotal/len(d)

    SAK = 0;  SAP = 0
    for x,y in d.items():
        SAK += (x - xmean) ** 2
        SAP += (x - xmean) * y 
    try: 
        a = SAP/SAK
    except ZeroDivisionError as detail: #Fanger divition med 0
        print "runtime error:", detail
        raise 
   
    fmin = a * (xmin - xmean) + ymean
    fmax = a * (xmax - xmean) + ymean
   
    return ([xmin, xmax],[fmin, fmax])

#############################
## Internal Implementation ##  
#############################

def dan_hashtabel(path):
    """parses file on path into a dictonary"""
    file = open(path)

    lines = file.readlines()
    ret = {}
    for i in range(0, len(lines)):
        line = lines[i]
        try:
            (x,y) = eval(line)
        except:
            print "bad syntax in:", path
            raise
        try:
            ret[x] = y 
        except:
            print "bad content in:", path, x
            raise
    file.close()
    return ret
