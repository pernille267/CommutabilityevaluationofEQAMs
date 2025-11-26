// 1. Initialize on load
$(document).on('shiny:connected', function() {
  initGlassTabs();
});

$(document).on('shiny:sessioninitialized', function() {
  initGlassTabs();
});

function initGlassTabs() {
  // Find all tabsets that haven't been initialized
  var tabsets = document.querySelectorAll('.glass-tabset-container:not(.initialized)');

  tabsets.forEach(function(container) {
    var id = container.id;
    // Find the initially active tab (if any)
    var activeBtn = container.querySelector('.glass-tab-btn.active');

    if (activeBtn) {
      var value = activeBtn.dataset.value;
      Shiny.setInputValue(id, value);
    }

    container.classList.add('initialized');
  });
}

// 2. Click Handler
window.switchGlassTab = function(tabsetId, value, element) {
  var container = document.getElementById(tabsetId);
  if (!container) return;

  // A. Update Navigation Buttons
  var navBtns = container.querySelectorAll('.glass-tab-btn');
  navBtns.forEach(function(btn) {
    btn.classList.remove('active');
  });
  element.classList.add('active');

  // B. Switch Content Panes
  // We look for the content wrapper inside this specific tabset
  var contentWrapper = container.querySelector('.glass-tab-content-wrapper');
  var panes = contentWrapper.querySelectorAll('.glass-tab-pane');

  panes.forEach(function(pane) {
    pane.classList.remove('active');
    if (pane.dataset.value === value) {
      pane.classList.add('active');
    }
  });

  // C. Update Shiny Input
  Shiny.setInputValue(tabsetId, value);

  // Optional: Trigger a window resize event to force plots (like plotly/ggplot) to redraw correctly
  // This fixes the common issue where plots are wrong size in hidden tabs.
  setTimeout(function() {
    window.dispatchEvent(new Event('resize'));
  }, 50);
};
