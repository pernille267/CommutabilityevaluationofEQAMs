// This listens for the 'shiny:connected' event.
// This implies the code is running inside an R Shiny app.
// It waits until the app is fully connected to the server before running our setup function.
$(document).on('shiny:connected', function() {
  initGlassTable();
});

// This listens for 'shiny:sessioninitialized'.
// Sometimes elements represent re-rendering, so we ensure the table
// is initialized again once the session is fully established.
$(document).on('shiny:sessioninitialized', function() {
  initGlassTable();
});

// --- MAIN SETUP FUNCTION ---
function initGlassTable() {

  // 1. CLEAN UP (Best Practice)
  // Before we tell the browser to listen for clicks, we turn OFF
  // any existing listeners on these elements.
  // Why? If 'initGlassTable' runs twice, we don't want the sort logic
  // to run twice for a single click (which would break things).
  $(document).off('click', '.glass-table-header-cell.sortable');

  // 2. LISTEN FOR CLICKS
  // We attach a 'click' listener to the document.
  // It watches for clicks specifically on elements with the classes '.glass-table-header-cell' AND '.sortable'.
  $(document).on('click', '.glass-table-header-cell.sortable', function() {

    // '$(this)' refers to the specific header the user just clicked.
    var $header = $(this);

    // We look up the tree to find the main container holding this specific table.
    var $tableContainer = $header.closest('.glass-table-container');

    // Inside that table, we find the container that holds the actual data rows.
    var $rowsContainer = $tableContainer.find('.glass-table-rows-container');

    // We find out which column number this is (0 for the first column, 1 for the second, etc.).
    //var idx = $header.index();
    var idx = parseInt($header.attr('data-col-index'));


    // 3. DETERMINE SORT DIRECTION
    // We check if we have already sorted this column by looking at stored data ('data-sort-dir').
    // If there is no data, we default to 'none'.
    var currentOrder = $header.data('sort-dir') || 'none';

    // If it was 'asc' (ascending), we switch to 'desc' (descending). Otherwise, we go 'asc'.
    var newOrder = (currentOrder === 'asc') ? 'desc' : 'asc';

    // 4. RESET VISUALS ON OTHER HEADERS
    // We find all OTHER headers in this table and reset them.
    $tableContainer.find('.glass-table-header-cell')
      .data('sort-dir', 'none')          // Forget their sort order
      .removeClass('asc desc')           // Remove CSS coloring for active sort
      .find('.glass-sort-icon')          // Find the little arrow icon
        .removeClass('fa-sort-up fa-sort-down') // Remove up/down arrows
        .addClass('fa-sort');            // Set it back to the neutral double-arrow

    // 5. UPDATE THE ACTIVE HEADER VISUALS
    // Save the new order ('asc' or 'desc') into the clicked header's memory.
    $header.data('sort-dir', newOrder).addClass(newOrder);

    // Decide which icon to show (Up arrow for Ascending, Down for Descending).
    var iconClass = (newOrder === 'asc') ? 'fa-sort-up' : 'fa-sort-down';

    // Remove the neutral icon and add the correct directional icon.
    $header.find('.glass-sort-icon').removeClass('fa-sort').addClass(iconClass);

    // 6. PREPARE TO SORT ROWS
    // We grab all the row elements and turn them into a JavaScript Array.
    // We need an Array so we can use the built-in '.sort()' function.
    var rows = $rowsContainer.children('.glass-table-row').toArray();

    // 7. EXECUTE SORTING LOGIC
    rows.sort(function(a, b) {
      // This function compares two rows (Row A and Row B) at a time.

      // Get the specific cell within Row A and Row B that matches the clicked column index.
      var $cellA = $(a).children('.glass-table-cell').eq(idx);
      var $cellB = $(b).children('.glass-table-cell').eq(idx);

      // Get the actual text inside those cells and trim whitespace.
      var valA = $cellA.text().trim();
      var valB = $cellB.text().trim();

      // --- Numeric Handling ---
      // We try to turn the text into numbers.
      // .replace(/,/g, '') removes commas (e.g., changes "1,000" to "1000") so the computer can read it.
      var numA = parseFloat(valA.replace(/,/g, ''));
      var numB = parseFloat(valB.replace(/,/g, ''));

      // We check if the conversion worked.
      // !isNaN means "Is Not a Number" is False (aka: It IS a number).
      var isNumA = !isNaN(numA) && valA !== "";
      var isNumB = !isNaN(numB) && valB !== "";

      // IF both values are valid numbers, we do math to compare them.
      if (isNumA && isNumB) {
        // If Ascending: A - B (Negative result puts A first).
        // If Descending: B - A (Positive result puts B first).
        return (newOrder === 'asc') ? numA - numB : numB - numA;
      }
      // ELSE (if they are words/strings), we use alphabetical comparison.
      else {
        // localeCompare is a fancy way to sort strings alphabetically.
        return (newOrder === 'asc') ? valA.localeCompare(valB) : valB.localeCompare(valA);
      }
    });

    // 8. UPDATE THE PAGE
    // Now that the 'rows' array is sorted correctly in memory, we append it back to the container.
    // In JavaScript, appending an element that is *already* on the page moves it to the new spot.
    // This physically reorders the rows on your screen.
    $rowsContainer.append(rows);
  });
}

initGlassTable();
