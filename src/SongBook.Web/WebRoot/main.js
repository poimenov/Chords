  document.addEventListener('DOMContentLoaded', function() {
  const baseNotes = document.getElementById("baseNotes");
  if(baseNotes){
    baseNotes.onchange = function() {
        location.href = "/chords/" + this.value;
    };   
  }
});
