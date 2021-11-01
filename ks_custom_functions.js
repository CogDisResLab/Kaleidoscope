
function myFunction(check_id, item_id) {
  // Get the checkbox
  var checkBox = document.getElementById(check_id);
  // Get the output text
  var item = document.getElementById(item_id);

  // If the checkbox is checked, display the output text
  if (checkBox.checked == true){
    item.style.opacity = 0.3;
  } else {
    item.style.opacity = 0;
  }
}