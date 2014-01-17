# -*- coding: cp1252 -*-
import operator
import copy
class simplematrix(object):


    # class variables 

    ########################
    ##        API         ##
    ########################

    def read(self, filename):
        infile = open(filename)
        matrix = [] 
        for line in infile.readlines():
            words = line.rstrip('\n').split(' ') 
            newline = []
            for word in words:
                newline.append(int(word))
            matrix.append(newline)
        return matrix 

    def write(self, filename): # Rækkerne er adskilt af mellemrum og coloner med \n 
        matrix = self.parse_to_string() # Dette har jeg valgt da det betyder jeg kan genbruge kode                 
        try:  
            outfile = open(filename, 'w')   # mens det stadig kan se lave matricer der ikke kan misforstås     
        except:
            raise IOError
        outfile.writelines(matrix) 
        outfile.close()

    ########################
    ## Buildin Functions  ##
    ########################

    def __init__(self, *args):
        numArgs = len(args)
        v = 0
        self.m = []
        if numArgs == 0: # Hvis der ingen argumenter er sætter vi m og n til 0
            self.m = [0]
        elif numArgs == 2 or numArgs == 3: # Sætter m og n til de givne argumenter
            for _ in range(0, args[1]): # Det er forfærdelig kode. Har det dårligt over det
                self.m.append([0] * args[0]) 
            if numArgs == 3: # Hvis der er et tredje argument gem værdierne i m matricen
                self.m = self.save_matrix(args[2])
        else:
            raise TypeError("Wrong number of arguments")

    def __str__(self):
        return self.parse_to_string()

    def __mul__(self, other):
        """multiply 2 matrix, returning the value of the multiplication"""
        if not len(self.m[0]) == len(other.m): 
           raise ValueError("The matrix sizes cannot be multiplied") 
        
        s = 0 #denne kode er blevet taget fra http://code.wikia.com/wiki/Matrix_multiplication og derefter redigeret til at passe med problemet 
        sum = [[0 for _ in range(len(other.m[0]))] for _ in range(len(self.m))] # laver en ny sum matrice
        for i in range(len(self.m)):
            for c in range(len(other.m[0])):
                s = 0
                for j in range(len(other.m)):
                    s += self.m[i][j] * other.m[j][c]
                sum[i][c] = s
                s = 0
        self.m = sum
        return self 

    def testmul(self):
        print self.__mul__(self)

    def __add__(self, other):
        """adds 2 matrix, returning the value of the addition"""
        if not self.check_size(other):
           raise ValueError("The matrix are not of equal size") 
        self.m = map(lambda l1, l2: map(operator.add,l1,l2), self.m, other.m) 
        return self 

    def __cmp__(self, other):
        """compares 2 matrix returning 0 if they are equal, otherwise returns
        -1"""
        if not self.check_size(other):
           raise ValueError("The matrix are not of equal size") 
        if reduce(lambda b1,b2: b1 and b2, map(operator.eq, self.m, other.m), True): 
            return 0
        return -1

    ############################
    ##    Hjælpefunktioner    ##
    ############################

    def check_size(self, other):
        """returns true if the 2 matrix are of equal size"""
        if len(self.m) == len(other.m) and len(self.m[0]) == len(other.m[0]):
            return True 
        return False

    def parse_to_string(self):
        return reduce(lambda o,l: o + ' '.join(str(e) for e in l) + '\n', self.m, "")

    def save_matrix(self, values):
        """saves the values list into the m matrix"""
        i = k = 0
        for n in self.m:
            for j in range(len(n)):
                n[j] = values[i]
                i = i + 1
        return self.m 

