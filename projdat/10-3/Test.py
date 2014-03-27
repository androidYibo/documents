from ARENA import *

def test():
    l1 = League("FPS")
    t1 = Tournament(l1, "duke1")
    t2 = Tournament(l1, "duke2")
    t3 = Tournament(l1, "quake1")

    p1 = Player("Zelda")
    t1.add_player(p1)
    t2.add_player(p1)
    t3.add_player(p1)

    p2 = Player("Mario")
    t1.add_player(p2)
    t2.add_player(p2)

    p3 = Player("Leoric")
    t2.add_player(p3)
    t3.add_player(p3)
    
    p4 = Player("Niels")
    t3.add_player(p4)

    print l1.tournaments_to_string()


