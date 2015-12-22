 
// Execute function body when the HTML document is ready
$(document).ready(function() {
// javascript code to send data to shiny server
var go;

  document.getElementById("exemple").onclick = function() 
   {
      go = "on";
      Shiny.onInputChange("ex", go); 
      go="stop";
      setTimeout(function(){Shiny.onInputChange("ex",go)}, 50);

   };
   
   

});