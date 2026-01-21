// ==========================================================================
// GLASS ROUTER ENGINE
// Replaces standard Shiny Tabs with custom DIV switching
// ==========================================================================

$(document).on('shiny:connected', function() {
  // 1. Initialize Router
  initGlassRouter();
  
  // 1b. Helper for opening URLs in new tabs (e.g. documentation)
  Shiny.addCustomMessageHandler('open_url_new_tab', function(url) {
    window.open(url, '_blank');
  });

  // 2. Register Custom Message Handler
  Shiny.addCustomMessageHandler('glass-sidebar-highlight', function(message) {
    // message: { tabName: "dins", enable: true }

    // 1. Finn selve menyknappen (Forelderen)
    var $navItem = $('.glass-nav-item[data-target="' + message.tabName + '"]');

    // 2. Finn selve varsel-ikonet INNI knappen (Barnet)
    // Dette er trinnet som manglet:
    var $notificationIcon = $navItem.find('.glass-nav-notification');

    if (message.enable) {
      // Nå legges klassen på riktig element:
      // .glass-nav-notification blir til .glass-nav-notification.has-notification
      $notificationIcon.addClass('has-notification');
    } else {
      $notificationIcon.removeClass('has-notification');
    }
  });
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

  // Remove Pulse Icon if there
  $el.find('.glass-nav-notification').removeClass('has-notification');

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
