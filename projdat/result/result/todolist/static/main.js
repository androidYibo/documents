//function markItemDone() {
//
//}
//
//function deleteItem(){
//	$(this).remove();
//}
//
//// jQuery starts here...
//jQuery(window).load(function() {
//	// here is our add new list item function
//	$("#button_todo").click(function(){
//		 console.log("test");
//		var text_to_item = $("#input_todo").val();
//		if (text_to_item != "") {
//			 var list_item = $('<li class="todo_item">'+text_to_item+'</li>');
//			 list_item.click(markItemDone);
//			 var ul_list = $("#list_todo");
//			 $(ul_list).append(list_item);
//			 $("#input_todo").val('');
//		}
//		$("#input_todo").focus();
//	});
//
//	$(".todo_item").each(function(){
//		$(this).click(markItemDone);
//	});
//});
//
