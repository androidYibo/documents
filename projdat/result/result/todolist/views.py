from django.shortcuts import render, HttpResponseRedirect
from django.core.urlresolvers import reverse
from result.todolist.models import TodoItem

def home(request):
    content = {}
    content["todo_items"] = TodoItem.objects.all()
    return render (request, "index.html", content)

def add_item(request):
    print "Hello World"
    if request.method == "POST":
        new_item = TodoItem()
        new_item.task = request.POST["new_task"]
        new_item.save()
    return HttpResponseRedirect(reverse("home"))

def delete_item(request, item_id):
    # if request.method == "POST":
    TodoItem.objects.get(id=item_id).delete()
        # ToDoItem().delete
    return HttpResponseRedirect(reverse("home"))

def toggle_item_pressed(request, item_id):
    item = TodoItem.objects.get(id=item_id)
    item.pressed = not item.pressed
    item.save()
    return HttpResponseRedirect(reverse("home"))
