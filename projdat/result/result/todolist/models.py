from django.db import models
from datetime import datetime

class TodoItem(models.Model):
	created_date = models.DateTimeField(default=datetime.now())
	task = models.CharField(max_length=50)
	pressed = models.BooleanField(default=False)

def __unicode__(self):
    pressed_string = ""
    if self.pressed:
        pressed_string = "(pressed)"
    return " ".join([self.task, pressed_string])
