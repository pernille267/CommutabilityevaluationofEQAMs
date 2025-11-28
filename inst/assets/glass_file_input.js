// GLASS FILE INPUT - CUSTOM BINDING v5.0
var glassFileBinding = new Shiny.InputBinding();

$.extend(glassFileBinding, {

  find: function(scope) {
    return $(scope).find('.glass-file-binding');
  },

  initialize: function(el) {
    var $el = $(el);
    var $input = $el.find('.glass-native-input');
    var $btn = $el.find('.glass-file-btn');
    var self = this;

    // Trigger click on hidden input
    $btn.on('click', function(e) {
      $input.click();
    });

    // Handle File Selection
    $input.on('change', function(e) {
      var file = e.target.files[0];

      if (!file) {
        self.resetUI($el);
        return;
      }

      // 1. Update UI Text & Tooltip State
      var $textZone = $el.find('.glass-file-text-zone');
      var $filename = $el.find('.glass-file-filename');

      $filename.text(file.name);

      // THIS IS CRITICAL FOR TOOLTIPS:
      $textZone.addClass('has-file');
      $textZone.attr('data-state', 'filled');

      // 2. Start Read Process
      self.readFile(file, $el);
    });
  },

  readFile: function(file, $el) {
    var self = this;
    var reader = new FileReader();
    var $track = $el.find('.glass-progress-track');
    var $fill = $el.find('.glass-bar-fill');

    // Reset Bar
    $track.removeClass('finished').addClass('active');
    $fill.css('width', '0%');

    reader.onprogress = function(e) {
      if (e.lengthComputable) {
        var percent = Math.round((e.loaded / e.total) * 100);
        $fill.css('width', percent + '%');
      }
    };

    reader.onload = function(e) {
      $fill.css('width', '100%');
      $track.removeClass('active').addClass('finished');

      $el.data('fileData', {
        name: file.name,
        size: file.size,
        type: file.type,
        content: e.target.result
      });

      self.callback(true);
    };

    reader.onerror = function() {
      alert("Error reading file");
      self.resetUI($el);
    };

    reader.readAsDataURL(file);
  },

  resetUI: function($el) {
    var $textZone = $el.find('.glass-file-text-zone');
    var $filename = $el.find('.glass-file-filename');
    var placeholder = $filename.attr('data-placeholder');
    var $track = $el.find('.glass-progress-track');

    $filename.text(placeholder);

    // RESET STATE FOR TOOLTIPS
    $textZone.removeClass('has-file');
    $textZone.attr('data-state', 'empty');

    $track.removeClass('active finished');
    $el.removeData('fileData');
  },

  getValue: function(el) {
    return $(el).data('fileData') || null;
  },

  subscribe: function(el, callback) {
    this.callback = callback;
  },

  unsubscribe: function(el) {
    $(el).off('.glassFileBinding');
  }
});

Shiny.inputBindings.register(glassFileBinding, 'glass.fileInput');
