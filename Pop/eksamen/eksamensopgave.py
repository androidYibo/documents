# -*- coding: cp1252 -*-

import planet as pl 
import matplotlib.pyplot as plt 
from mpl_toolkits.mplot3d import Axes3D
import numpy as np
import math

# global variables 
GM_Sun = 2.959122082322128 * 10 ** -4
GM_Mercury = 4.912549571831092 * 10 ** -11
GM_Venus = 7.243453179939512 * 10 ** -10
GM_Earth = 8.887692546888129 * 10 ** -10
GM_Mars = 9.549531924899252 * 10 ** -11
GM_Jupiter = 2.824760453365182 * 10 ** -7
GM_Saturn = 8.457615171185583 * 10 ** -8
GM_Uranus = 1.291894922020739 * 10 ** -8
GM_Neptune = 1.524040704548216 * 10 ** -8
GM_Pluto = 1.945211846204988 * 10 ** -12

########################
##        API         ##
########################

def plot(t, dt, planets, method):
    """laver og fylder et 3d plot med informationen givet ud fra calculatePath"""
    fig = plt.figure()
    ax = fig.add_subplot(111, projection='3d')
    planetpath = calculate_path(t, dt, planets, method)
    for j in range(0,len(planets)):
        ax.plot(planetpath[j, :, 0], planetpath[j, :, 1], planetpath[j, :, 2]
                ,linestyle=':') 
    plt.show()         

def calculate_path(t, dt, planets, method):
    """udregned planeternes 'planets' bevægelse og returnere numpy listen
       af positioner på ruten. Til udregning benyttes metoden 'method'""" 

    # Laver et numpy array med størelsen: 
    #   antal planeter * ceil(t/dt) * 3
    # dette array bliver benyttet til at gemme al position dataen
    planetpath = np.zeros((len(planets),math.ceil(t/dt),3),dtype=float)
    i = 0
    while i < (t / dt): # for hver tidsenhed
        j = 0
        for planet in planets: # for hver planet
            method(t, dt, planets, planet, planetpath, i, j)
            j += 1
        i += 1
    return planetpath

def euler_metoden(t, dt, planets, planet, planetpath, i, j):
    """Bruger Euler metoden til at udregne planeternes bane"""
    # Min implementation har planet.position som r(t) og planet.velocity som
    # v(t)

    # Euler udrening og gem informationen i planetpath
    planetpath[j][i] = planet.position + planet.velocity * dt # 1 = delta t  

    # flyt planeten til sin nye position
    planet.position  = planetpath[j][i]
    planet.velocity  = planet.velocity + planet.find_acceleration_n(planets) * dt

def midtpunkts_metoden(t, dt, planets, planet, planetpath, i, j):
    """Bruger midpunktsmetoden til at udregne planeternes bane"""

    # Euler estimation
    r_est = planet.position + planet.velocity * dt
    a_est = planet.find_acceleration_n_tmp_pos(planets, r_est)
                    
    # Midpunkts udreninger
    a     = planet.find_acceleration_n(planets)
    new_velocity = planet.velocity + 0.5 * (a + a_est) * dt 
    planet.position = planet.position + 0.5 * (planet.velocity + new_velocity) * dt

    # Gem ny data 
    planet.velocity = new_velocity
    planetpath[j][i] = planet.position

def runge_kutta_metoden(t, dt, planets, planet, planetpath, i, j): 
    """Bruger runge_kutta_metoden til at udregne planeternes bane"""
    # Første estimat benytter den nuværende position
    h1 = planet.velocity 
    k1 = planet.find_acceleration_n(planets)

    # Anden estimat ændrer først positionen og laver udregning
    # ud fra den nye position. a(tn, r2) bliver udregnet af 
    # en funktion i planeten som uddybes i Planet klassen
    r2 = planet.position + dt/2.0 * h1 
    h2 = planet.velocity + dt/2.0 * k1
    k2 = planet.find_acceleration_n_tmp_pos(planets, r2) 

    # Tredje estimat
    r3 = planet.position + dt/2.0 * h2
    h3 = planet.velocity + dt/2.0 * k2
    k3 = planet.find_acceleration_n_tmp_pos(planets, r3)

    # Fjerde estimat
    r4 = planet.position + dt * h3
    h4 = planet.velocity + dt * k3
    k4 = planet.find_acceleration_n_tmp_pos(planets, r4)

    # Udregner ny position og gemmer den nye hastighed i planeten 
    planet.velocity = planet.velocity + 1.0/6.0 * (k1 + 2.0 * k2 + 2.0 * k3 + k4) * dt
    planetpath[j][i] = planet.position + 1.0/6.0 * (h1 + 2.0 * h2 + 2.0 * h3 + h4) * dt

    # Flytter planeten til dens nye position
    planet.position = planetpath[j][i]

##############
##  Parser  ##
##############

def parse_path(*args):
    """parser og plotter planetdataen gemt i filenerne givet
       igennem *args. viser IKKE plottet"""
    fig = plt.figure()
    ax = fig.add_subplot(111, projection='3d')
    planetpath = np.zeros((len(args),13,3), dtype=float)
    j = 0
    for filename in args: #for hver fil
        try:
            infile = open(filename)
        except:
            raise IOError 
        words = infile.readline()
        instance = words.split('')
        i = 0
        for data in instance:
            information = data.split(',')
            try:
                position = information[2:5]
            except:
                raise IndexError("input data er af forkert form")
            if position != []:
                planetpath[j][i] = position
            i += 1
        ax.plot(planetpath[j, :, 0], planetpath[j, :, 1], planetpath[j, :, 2])
        j += 1
    plt.show()
    return planetpath 

def parse_planet(filename, GM):
    """parser det første stykke data fra filen 'filename' og laver en planet
       ud fra informationen"""
    try:
        infile = open(filename)
    except:
        raise IOError
    words = infile.readline()
    instance = words.split('')
    information = instance[0].split(',')
    try:
        position = information[2:5]
        vel      = information[5:8]
    except:
        raise IndexError("input data er af forkert længde")
    return pl.Planet(position, vel, GM)

################
##    Test    ##
################

def plot_all():
    """plotter de forskelige metoder. ADVARSEL: DENNE FUNKTION LAVER
       MANGE PLOTS"""
    sun     = parse_planet('data/Sun.txt',     GM_Sun)  
    mercury = parse_planet('data/Mercury.txt', GM_Mercury)  
    venus   = parse_planet('data/Venus.txt', GM_Venus)  
    earth   = parse_planet('data/Earth.txt', GM_Earth)  
    mars    = parse_planet('data/Mars.txt', GM_Mars)  
    jupiter = parse_planet('data/Jupiter.txt', GM_Jupiter)  
    saturn  = parse_planet('data/Saturn.txt', GM_Saturn)  
    uranus  = parse_planet('data/Uranus.txt', GM_Uranus)  
    neptune = parse_planet('data/Neptune.txt', GM_Neptune)  
    pluto   = parse_planet('data/Pluto.txt', GM_Pluto)  

    def sammenlign_pos(planetpath1, planetpath2, names, method):
        """testefunktion brugt til at plotte planeters bane i forhold til
           hinanden i 3d"""
        fig = plt.figure()
        for i in range(0, len(planetpath1)):
            fig, ax = plt.subplots(3,4)
            ax = plt.subplot(111, projection='3d')
            ax.plot(planetpath1[i, :, 0], planetpath1[i, :, 1], planetpath1[i, :, 2]
                    ,linestyle='-', color='green', label=method) 
            ax.plot(planetpath2[i, :, 0], planetpath2[i, :, 1], planetpath2[i, :, 2]
                    ,linestyle='--', color='red', label="Data") 
            plt.title(names[i] + " 3d")
            ax.legend()
        plt.show()         
             
    def sammenlign_pos2d(planetpath1, planetpath2, names, method):
        """testefunktion brugt til at plotte planeters bane i forhold til
           hinanden i 2d"""
        for i in range(0, len(planetpath1)):
            plt.plot(planetpath1[i, :, 0], planetpath1[i, :, 1]
                    ,linestyle='-', color='green', label=method) 
            plt.plot(planetpath2[i, :, 0], planetpath2[i, :, 1]
                    ,linestyle='--', color='red', label="Data") 
            plt.title(names[i] + " 2d")
            plt.legend()
            plt.show()

    def accum_fejl(planetpath1, planetpath2, names, method):
        """testefunktion brugt til at plotte forskellen imellem 2 baner"""
        firsts = [0,31,59,90,120,151,181,212,243,273,304,334,364]
        for i in range(0, len(planetpath1)):
            fig, ax = plt.subplots(3,4)
            ax = plt.subplot(111, projection='3d')
            ax.plot(planetpath1[i, firsts, 0] - planetpath2[i, : , 0],
                    planetpath1[i, firsts, 1] - planetpath2[i, : , 1],
                    planetpath1[i, firsts, 2] - planetpath2[i, : , 2],
                    label=method)
            plt.title(names[i] + " dif")
            ax.legend()
            plt.show
        return sum(planetpath1[:, firsts, :] - planetpath2[i, :, :])


    plot(365, 0.1, [sun, mercury, venus, earth, mars, jupiter, saturn, uranus,
                   neptune, pluto], euler_metoden) 

    plot(365, 0.1, [sun, mercury, venus, earth, mars, jupiter, saturn, uranus,
                   neptune, pluto], midtpunkts_metoden) 

    plot(365, 0.1, [sun, mercury, venus, earth, mars, jupiter, saturn, uranus,
                   neptune, pluto], runge_kutta__metoden) 

    rkm = calculate_path(365, 1, [sun, mercury, venus, earth, mars, jupiter, saturn, 
                  uranus, neptune, pluto], runge_kutta_metoden) 
    mpm = calculate_path(365, 1, [sun, mercury, venus, earth, mars, jupiter, saturn, 
                  uranus, neptune, pluto], midtpunkts_metoden) 
    eul = calculate_path(365, 1, [sun, mercury, venus, earth, mars, jupiter, saturn, 
                  uranus, neptune, pluto], euler_metoden) 

    path = parse_path('data/Sun.txt', 'data/Mercury.txt', 'data/Venus.txt',
                      'data/Earth.txt', 'data/Mars.txt', 'data/Jupiter.txt',
                      'data/Saturn.txt', 'data/Uranus.txt', 'data/Neptune.txt',
                      'data/Pluto.txt') 

    names = ["Solen","Merkur", 'Venus', 'Jorden', 'Mars', 'Jupiter', 'Saturn',
            'Uranus', 'Neptun', 'Pluto']

    sammenlign_pos(eul, path, names, "Euler") 
    sammenlign_pos(mpm, path, names, "Midtpunkt") 
    sammenlign_pos(rkm, path, names, "Runge_Kutta") 

    sammenlign_pos2d(eul, path, names, "Euler") 
    sammenlign_pos2d(mpm, path, names, "Midtpunkt") 
    sammenlign_pos2d(rkm, path, names, "Runge_Kutta") 

    accum_fejl(eul, path, names, "Euler"), "Euler" 
    accum_fejl(mpm, path, names, "Midtpunkt"), "Midtpunkt" 
    accum_fejl(rkm, path, names, "Runge_Kutta"), "Runge_Kutta" 



