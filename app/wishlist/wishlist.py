import cgi

from google.appengine.api import users
from google.appengine.ext import ndb

import webapp2


MAIN_PAGE_HTML = """\
<html>
    <body>
        <form action="/list" method="post">
            <div><textarea name="wishbox" rows="1" cols="80"></textarea></div>
            <div><input type="submit" value="Submit wish"></div>
        </form>
    </body>
</html>
"""

BUTTON_REMOVE = """\
        <html>
            <body> 
                <button type="button" onclick="alert('Hello world!')">
                        Remove
                </button>
            </body>
        </html> 
        """

class List(ndb.Model):
    """Models a list with an author, wishes and date"""
    owner  = ndb.UserProperty(required = True)
    wishes = ndb.StringProperty(repeated = True) 
    date = ndb.DateTimeProperty(auto_now_add=True)

class WishLists(ndb.Model):
    lists = ndb.KeyProperty(kind = List, repeated = True)

wishlist = WishLists()
wishlist.put()

class MainPage(webapp2.RequestHandler):

    def get(self):

        if users.get_current_user():
            url = users.create_logout_url(self.request.uri)
            url_linktext = 'Logout'
        else:
            self.redirect(users.create_login_url(self.request.uri))

        self.response.write(MAIN_PAGE_HTML)

    def post(self): # removes wish
        


class Wishlist(webapp2.RequestHandler):

    def get(self):
        s = '<html><body>'
        user = users.get_current_user()
        user_list = user_get_list(user)

        s += '<ol><b>%s</b> wishes:' % user_list.owner
        for wish in user_list.wishes:
            s+= '<li>%s</li>' % wish
        s += '</ol></body></html>'
        self.response.out.write(s)

    def post(self):
        user = users.get_current_user()
        user_list = user_get_list(user)
        wishes_str = cgi.escape(self.request.get('wishbox'))

        self.response.write('<html><body>You are ' + user.nickname() + ' and you wish for:<pre>')
        self.response.write(wishes_str)
        self.response.write('</pre></body></html>')

        wish_list = wishes_str.split('\n')
        for wish in wish_list:
            user_list.wishes.append(wish)
        user_list.put()

# class Remove(webapp2.RequestHandler):

    # def get(self):
        # s = '<html><body>' 
         

application = webapp2.WSGIApplication([
        ('/', MainPage),
        ('/list', Wishlist),
        ('/list/del', Remove),
        ],
    debug=True)

def user_get_list(cur_user):
    user_list = None
    for list in List.query(): 
        if list.owner == cur_user:
            user_list = list 
    if not user_list: 
        list = List(owner = cur_user)
        list.put() 
        wishlist.lists.append(list)
        wishlist.put()
    return user_list

