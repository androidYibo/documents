class Player(object):
    """Player Class"""

    def __init__(self, name):
        """Initialise players"""
        self.tournaments = []
        self.name = name

    def add_tournament(self, tournament):
        """Adds a tournament to this player"""
        if not tournament in self.tournaments:
            self.tournaments.append(tournament)
            tournament.add_player(self)

    def remove_tournament(self, tournament):
        """Removes a tournament from this player"""
        self.tournaments.remove(tournament)

    def get_name(self):
        ret = self.name
        return ret

class Tournament(object):
    """Tournament Class"""

    def __init__(self, league, name):
        """Initialises a tournament under the parameter league"""
        self.players = []
        self.rounds = []
        self.league = league
        self.name = name
        league.add_tournament(self)

    def add_player(self, player):
        """Adds a player to the tournament"""
        if not player in self.players:
            self.players.append(player)
            player.add_tournament(self)

    def remove_player(self, player):
        """Removes a player from the tournament"""
        self.players.remove(player)

    def get_players_string(self):
        s = "        Players:\n"
        i = 1
        for player in self.players:
            s += "            Player " + str(i) + ": " + player.get_name() + "\n"  
            i += 1
        return s + "\n"

    def add_round(self, round):
        if not round in self.rounds:
            self.rounds.append(round)

    def get_name(self):
        ret = self.name # to avoid giving a mutable name
        return ret 

class League(object):
    """Class for a league in a tournament"""

    def __init__(self, name):
        """Initialises the league"""
        self.tournaments = []
        self.name = name

    def add_tournament(self, tournament):
        """Adds a tournament to the league"""
        if not tournament in self.tournaments:
            self.tournaments.append(tournament)

    def tournaments_to_string(self):
        s = "In the League: " + self.name + "\nWe have the following tournaments: \n" 
        i = 1
        for tournament in self.tournaments:
            s += "    Tournament " + str(i) + ": " + tournament.get_name() + "\n"
            s += tournament.get_players_string()
            i += 1
        return s

class Round(object):
    """Class for a round in a tournament"""

    def __init__(tournament):
        """Initializes a round in the parameter tournament"""
        self.tournament = tournament
        tournament.add_round(self)

