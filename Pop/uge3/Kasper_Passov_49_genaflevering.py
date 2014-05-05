# Skrevet af Kasper Passov
# -*- coding: utf-8 -*-

def is_BS(lists):
   """Public funktion der returnere True hvis lists er et bloksystem"""
   return __check_list_length(lists, len(lists[0])) and __check_all_elements(lists)

########################
##  Helper Functions  ##
########################

def __check_list_length(lists, listlength):
    """Privat funktion der returnere True hvis alle lister i listen 'lists' 
       er lige lange, returnere False hvis dette ikke er tilfældet"""
    if lists == []:
        return True
    if len(lists[0]) != listlength:
        return False
    return __check_list_length(lists[1:], listlength)

def __check_all_elements(lists):
    """Private funktion der returnere True hvis alle elementer i listen møder 
       hinanden præcist en gang i bloksystemet"""
    all_elements = []
    for l in lists: 
        if not __check_for_repeats(l):
            return False
        all_elements = __union(all_elements, l)
    all_pairs = __create_all_pairs(all_elements, [])
    for pair in all_pairs:
        has_found = False
        for l in lists:
            if pair[0] in l and pair[1] in l:
               if has_found == True:
                  return False #hvis det samme par er flere steder i koden
               has_found = True 
        if has_found == False:
            return False #den kommer hertil hvis paret ikke findes nogen liste 
    return True

def __create_all_pairs(all_elements, all_pairs):
    """Lave alle mulige par imellem elementerne"""
    if len(all_elements) == 1:
        return all_pairs
    current_element = all_elements[0]
    for element in all_elements[1:]:
        all_pairs.append([current_element, element])
    return __create_all_pairs(all_elements[1:], all_pairs) 

def __union(l1, l2):
    """Privat funktion der returnere en liste med alle unike elementer imellem de to lister""" 
    lt = __union_helper(l1, [])
    return __union_helper(l2, lt)

def __union_helper(le, lr):
    """Private hjælpefunktion der tager alle elementer i list_element(le) og
       lægger dem i list_return(lr) hvis elementet ikke alerede findes"""
    for e in le:
        if not e in lr: 
            lr.append(e)
    return lr

def __check_for_repeats(l):
    """Checks if there are multiple of the same element in the list"""
    if l == []:
        return True
    for e in l[1:]:
        if l[0] == e:
            return False
    return __check_for_repeats(l[1:])