$(document).on('shiny:inputchanged', function(event) {

  // 1. Get the ID of the input that changed
  var changedId = event.name;
  var value = event.value;

  // 2. Find any toggle panel that is listening to this ID
  // Note: We handle namespaces by checking if the panel's data-trigger matches
  var panel = document.querySelector('.glass-toggle-panel[data-trigger="' + changedId + '"]');

  if (panel) {
    // 3. Logic: If value is Odd -> Show. If Even -> Hide.
    // This assumes the trigger is an action button (counter).
    if (value % 2 !== 0) {
      panel.classList.add('show');
    } else {
      panel.classList.remove('show');
    }
  }
});

// Optional: Initialize on load (in case value is already set, e.g. after reconnect)
$(document).on('shiny:sessioninitialized', function() {
  var panels = document.querySelectorAll('.glass-toggle-panel');
  panels.forEach(function(panel) {
    var triggerId = panel.getAttribute('data-trigger');
    // Check if Shiny has a value for this trigger
    if (Shiny.shinyapp.inputValues && Shiny.shinyapp.inputValues.hasOwnProperty(triggerId)) {
      var val = Shiny.shinyapp.inputsValues[triggerId];
      if (val % 2 !== 0) {
        panel.classList.add('show');
      }
    }
  });
});
