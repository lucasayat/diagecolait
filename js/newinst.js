 
// Execute function body when the HTML document is ready
$(document).ready(function() {
// javascript code to send data to shiny server
   var valenew ;
   var text
  document.getElementById("newinst").onclick = function() 
   {
          
         text= prompt("Nom"); 
         Shiny.onInputChange("nomnewi", text);
        valenew="on"
        Shiny.onInputChange("newi", valenew);
       
   };


Shiny.addCustomMessageHandler("oknew",
function(finew) {
  valenew=finew
  Shiny.onInputChange("newi", valenew);
}  
);


});