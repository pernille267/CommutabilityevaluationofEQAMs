// ==========================================================================
// GLASS ROUTER ENGINE
// Replaces standard Shiny Tabs with custom DIV switching
// ==========================================================================

$(document).on('shiny:connected', function() {
  initGlassRouter();
});

function initGlassRouter() {
  // 1. Set initial state based on "active" class in HTML
  var activeNav = $('.glass-nav-item.active');
  if (activeNav.length > 0) {
    var target = activeNav.data('target');
    switchPage(target);
  }
}

// Global Click Delegation for Navigation
$(document).on('click', '.glass-nav-item', function() {
  var $el = $(this);
  var target = $el.data('target');

  // Visual Update (Sidebar)
  $('.glass-nav-item').removeClass('active');
  $el.addClass('active');

  // Router Logic
  switchPage(target);

  // Notify Shiny (Optional: if server needs to know current tab)
  // We assume the sidebar has an ID
  var sidebarId = $el.closest('.glass-sidebar').data('input-id');
  if (sidebarId) {
    Shiny.setInputValue(sidebarId, target);
  }
});

function switchPage(targetId) {
  // 1. Hide all pages
  $('.glass-page').removeClass('active');

  // 2. Show target page
  // We use [data-value] selector to match standard Shiny patterns or ID
  var $page = $('.glass-page[data-value="' + targetId + '"]');
  $page.addClass('active');

  // 3. Trigger resize event to fix Plots/DT tables rendering in hidden divs
  $(window).trigger('resize');
}
