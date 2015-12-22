 
// Execute function body when the HTML document is ready
$(document).ready(function() {
// javascript code to send data to shiny server


Shiny.addCustomMessageHandler("verif2",
function(mes2) {
  
 alert(mes2)

}


);


});