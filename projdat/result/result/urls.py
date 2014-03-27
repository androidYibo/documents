from django.conf.urls import patterns, include, url

# Uncomment the next two lines to enable the admin:
from django.contrib import admin
admin.autodiscover()

urlpatterns = patterns('',
    # Examples:
    url(r'^$', 'result.todolist.views.home', name='home'),
    url(r'^add_item/$', 'result.todolist.views.add_item', name='add_item'),
    url(r'^delete_item/(?P<item_id>\d+)/$', 'result.todolist.views.delete_item', name='delete_item'),
    url(r'^toggle_item_pressed/(?P<item_id>\d+)/$', 'result.todolist.views.toggle_item_pressed', name='toggle_item_pressed'),
    
    # Uncomment the next line to enable the admin:
    url(r'^admin/', include(admin.site.urls)),
)
