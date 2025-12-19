// --- Glass Loader JavaScript ---

$(document).on('shiny:connected', function(event) {

  var $overlay = $('#glass-loader-overlay');
  var $text = $overlay.find('.glass-loader-text');

  // Timers
  var busyTimer = null;
  var longWaitTimer = null;

  // Settings
  var delayMs = 1000;      // Viser loader etter 1 sekund (Best Practice)
  var longWaitMs = 10000;   // Bytter melding etter 10 sekunder

  // Texts (Husk at default må matche R-filen din hvis du vil unngå "flash" ved reset)
  var defaultText = "Performing statistical magic...";
  var longText = "Still crunching numbers... Heavy calculations require a little extra patience. We're getting there!";

  // 1. Når Shiny begynner å jobbe
  $(document).on('shiny:busy', function() {

    // Start "Show Loader" Timer (hvis den ikke allerede kjører)
    if (!busyTimer) {
      busyTimer = setTimeout(function() {
        $overlay.addClass('active');
      }, delayMs);
    }

    // Start "Long Wait Message" Timer
    if (!longWaitTimer) {
      longWaitTimer = setTimeout(function() {
        // Bytt tekst KUN hvis loaderen faktisk er synlig
        if ($overlay.hasClass('active')) {
          // En liten fade-effekt for tekstbyttet
          $text.fadeOut(200, function() {
            $(this).text(longText).fadeIn(200);
          });
        }
      }, longWaitMs);
    }

  });

  // 2. Når Shiny er ferdig
  $(document).on('shiny:idle', function() {

    // Rydd opp timere umiddelbart
    if (busyTimer) { clearTimeout(busyTimer); busyTimer = null; }
    if (longWaitTimer) { clearTimeout(longWaitTimer); longWaitTimer = null; }

    // Skjul overlay
    $overlay.removeClass('active');

    // Reset teksten tilbake til default (litt forsinket så brukeren ikke ser byttet mens den fader ut)
    setTimeout(function() {
      $text.text(defaultText);
      $text.show(); // Sikre at den er synlig (i tilfelle fadeOut hang igjen)
    }, 500);
  });

  // 3. Custom Message Handler for manuelle oppdateringer
  Shiny.addCustomMessageHandler('glass-loader-update-text', function(message) {
    // Oppdaterer defaultText også, slik at den ikke overskrives ved neste reset hvis man ønsker det
    // Men her holder vi det enkelt og oppdaterer bare visningen der og da.
    $text.text(message);
  });

  // 4. Custom Message Handler for å tvinge visning
  Shiny.addCustomMessageHandler('glass-loader-show', function(state) {
    if (state === true) {
      $overlay.addClass('active');
    } else {
      $overlay.removeClass('active');
    }
  });

});
