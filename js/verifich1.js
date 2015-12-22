 
// Execute function body when the HTML document is ready
$(document).ready(function() {
// javascript code to send data to shiny server


Shiny.addCustomMessageHandler("verif1",
function(mes1) {
  
 alert(mes1)

}




);


});