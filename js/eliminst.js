 
// Execute function body when the HTML document is ready
$(document).ready(function() {

   var valelim ;
   
  document.getElementById("eliminst").onclick = function() 
   {
       numelim=prompt("Numero instantane a supprimer?:");
       if (numelim==null) {
        valelim="off";
       }else{
       sup = confirm("Supprimer Instant_"+numelim+" ?");
       if( sup== true) {
          valelim=numelim; 
       }else{
         valelim="off";   
       }
        Shiny.onInputChange("supelim", valelim)
       }};
  
  
Shiny.addCustomMessageHandler("okelim",
function(mes) {  
fin="off";
Shiny.onInputChange("supelim", fin);
//alert(mes);
}  
);



});