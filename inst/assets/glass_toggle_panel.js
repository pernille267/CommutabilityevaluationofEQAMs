$(document).on('shiny:inputchanged', function(event) {

  // 1. Get the ID of the input that changed
  var changedId = event.name;
  var value = event.value;

  // 2. Find ALL panels listening to this ID
  var panels = document.querySelectorAll('.glass-toggle-panel[data-trigger="' + changedId + '"]');

  panels.forEach(function(panel) {
    var showWhen = panel.getAttribute('data-show-when');

    // 3A. Logic: Match Specific Value (if data-show-when exists)
    if (showWhen !== null) {
      // Loose comparison (==) allows matching string "5" to number 5
      if (value == showWhen) {
        panel.classList.add('show');
      } else {
        panel.classList.remove('show');
      }
    }
    // 3B. Logic: Counter (Odd = Show) (Default/Original behavior)
    else {
      if (value % 2 !== 0) {
        panel.classList.add('show');
      } else {
        panel.classList.remove('show');
      }
    }
  });
});

// Initialize on load (to handle default selections/reconnects)
$(document).on('shiny:sessioninitialized', function() {
  var panels = document.querySelectorAll('.glass-toggle-panel');

  panels.forEach(function(panel) {
    var triggerId = panel.getAttribute('data-trigger');
    var showWhen = panel.getAttribute('data-show-when');

    // Check if Shiny has a value for this trigger
    if (Shiny.shinyapp.inputValues && Shiny.shinyapp.inputValues.hasOwnProperty(triggerId)) {
      var val = Shiny.shinyapp.inputValues[triggerId];

      if (showWhen !== null) {
        if (val == showWhen) panel.classList.add('show');
      } else {
        if (val % 2 !== 0) panel.classList.add('show');
      }
    }
  });
});
