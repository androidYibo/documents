# -*- coding: latin1 -*-
# kursusuge5modul.py

import turtle as tt
# http://docs.python.org/2/library/turtle.html
from math import floor

def visLife(foerste,naeste,levende,klik):
    cs = 10 # celleside, global konstant
    tt.Screen()
    tt.speed('fastest')
    tt.hideturtle()
    tt.penup()
    kodefarve = ['white','blue','red','green','black']
    # 0 = død, lige klikantal; 1 = levende, lige klikantal
    # 2 = levende, ulige klikantal; 3 = død, ulige klikantal; 4 = gitter

    # Lærredet bruger koordinater (x,y) og spillet koordinater (i,j)
    # Globale (x0,y0) er placering af tænkt celle (0,0)
    def toXY(i,j):
        return i*cs+x0,j*cs+y0
    def fromXY(x,y):
        return int(floor(float(x-x0)/cs)),int(floor(float(y-y0)/cs))
    
    def tegnGitter(i0,i1,j0,j1):
        """Gitteret har søjler fra i0 til og med i1 og rækker fra
j0 til og med j1.  Først blankstilles lærredet"""
        xmin,ymin = toXY(i0,j0)
        xlen,ylen = (i1-i0+2)*cs,(j1-j0+2)*cs
        tt.clear()
        tt.penup()
        tt.color(kodefarve[4])
        # vandrette linjer
        x,y = xmin-cs/2,ymin
        tt.setheading(0) # øst
        for j in range(j0,j1+2):
            tt.goto(x,y)
            tt.pendown()
            tt.forward(xlen)
            tt.penup()
            y += cs
        # lodrette linjer
        x,y = xmin,ymin-cs/2
        tt.setheading(90) # nord
        for i in range(i0,i1+2):
            tt.goto(x,y)
            tt.pendown()
            tt.forward(ylen)
            tt.penup()
            x += cs
            
    def tegnCelle(i,j,farve):
        x,y = toXY(i,j)
        tt.goto(x+1,y+1)
        tt.setheading(0) # øst
        tt.color(farve)
        tt.fill(True)
        tt.pendown()
        for k in range(4):
            tt.forward(cs-2)
            tt.left(90)
        tt.fill(False)
        tt.penup()

    def tegnGen(i0,i1,j0,j1):
        """Gitteret har søjler fra i0 til og med i1 og rækker fra
j0 til og med j1"""
        for i in range(i0,i1+1):
            for j in range(j0,j1+1):
                skyggegen[(i,j)] = 0 # død
                if levende(i,j):
                    tegnCelle(i,j,kodefarve[1])
                    skyggegen[(i,j)] = 1 # levende

    def skift(x,y):
        """Knyttes til museklik"""
        i,j = fromXY(x,y)
        if (i,j) in skyggegen:
            farvekode = 3 - skyggegen[(i,j)] # vend farve
            skyggegen[(i,j)] = farvekode
            tegnCelle(i,j,kodefarve[farvekode])
            klik(i,j)

    def tast():
        """Knyttes til tast af mellemrum"""
        i0,i1,j0,j1 = naeste()
        skyggegen = {}
        tegnGitter(i0,i1,j0,j1)
        tegnGen(i0,i1,j0,j1)

    def stop():
        """Knyttes til tast af Escape"""
        tt.bye()

    # første generation
    i0,i1,j0,j1 = foerste()
    # beregn basis, dvs. placering af tænkt celle (0,0)
    x0,y0 = -(i0+i1+1)*cs/2,-(j0+j1+1)*cs/2
    skyggegen = {}
    tegnGitter(i0,i1,j0,j1)
    tegnGen(i0,i1,j0,j1)

    # begivendhedsstyring:
    tt.onscreenclick(skift)
    tt.onkey(tast,'space')
    tt.onkey(stop,'Escape')
    tt.listen()
    tt.mainloop()
