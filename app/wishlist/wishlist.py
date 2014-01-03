import cgi

from google.appengine.api import users
from google.appengine.ext import ndb

import webapp2


MAIN_PAGE_HTML = """\
<html>
    <body>
        <form action="/sign" method="post">
            <div><textarea name="content" rows="3" cols="60"></textarea></div>
            <div><input type="submit" value="Sign Guestbook"></div>
        </form>
    </body>
</html>
"""


class MainPage(webapp2.RequestHandler):

    def get(self):
        self.response.write(MAIN_PAGE_HTML)

class List(ndb.Model):
    """Models a list with an author, wishes and date"""
    author = ndb.UserProperty()
    wishes = ndb.StringProperty() 
    date   = ndb.DateTimeProperty(auto_now_add=True)

class Wishes(ndb.Model):
    wish = ndb. 


class Wishlist(webapp2.RequestHandler):

    def post(self):
        self.response.write('<html><body>You wrote:<pre>')
        self.response.write(cgi.escape(self.request.get('content')))
        self.response.write('</pre></body></html>')


        if users.get_current_user():
            url = users.create_logout_url(self.request.uri)
            url_linktext = 'Logout'
        else:
            url = users.create_login_url(self.request.uri)
            url_linktext = 'Login'


application = webapp2.WSGIApplication([
        ('/', MainPage),
        ('/list', Wishlist),
        ],
    debug=True)

