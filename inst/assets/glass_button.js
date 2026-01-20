var glassButtonBinding = new Shiny.InputBinding();

$.extend(glassButtonBinding, {

  find: function(scope) {
    return $(scope).find('.glass-btn, .glass-download-btn');
  },

  initialize: function(el) {
    $(el).data('val', 0);
  },

  getValue: function(el) {
    return $(el).data('val');
  },

  subscribe: function(el, callback) {
    $(el).on('click.glassButtonBinding', function(e) {
      if ($(el).hasClass('disabled')) return;
      var val = $(el).data('val') || 0;
      $(el).data('val', val + 1);
      callback();
    });
  },

  unsubscribe: function(el) {
    $(el).off('.glassButtonBinding');
  },

  receiveMessage: function(el, data) {
    var $el = $(el);

    // Update Label (Target the label span)
    if (data.hasOwnProperty('label')) {
      $el.find('.glass-btn-label').text(data.label);
    }

    // Update Icon (Target the icon span)
    if (data.hasOwnProperty('icon')) {
      $el.find('.glass-btn-icon').html(data.icon);
    }

    // Update Disabled State
    if (data.hasOwnProperty('disabled')) {
      if (data.disabled) {
        $el.addClass('disabled');
      } else {
        $el.removeClass('disabled');
      }
    }

    // Update Urgent State
    // The urgent indicator is a sibling within the wrapper, immediately preceding the button
    if (data.hasOwnProperty('urgent')) {
      var $indicator = $el.parent().find('.urgent-indicator');
      if (data.urgent) {
        $indicator.addClass('visible');
      } else {
        $indicator.removeClass('visible');
      }
    }

    // Update Urgent Text (if changed dynamically)
    if (data.hasOwnProperty('urgent_text')) {
       var $indicatorText = $el.parent().find('.urgent-text');
       $indicatorText.text(data.urgent_text);
    }
  }
});

Shiny.inputBindings.register(glassButtonBinding, 'shiny.glassButton');
