$(document).on('shiny:connected', function() {
  initGlassTable();
});

$(document).on('shiny:sessioninitialized', function() {
  initGlassTable();
});

function initGlassTable() {
  // Delegate click for sorting
  $(document).off('click', '.glass-table-header-cell.sortable');
  $(document).on('click', '.glass-table-header-cell.sortable', function() {
    var $header = $(this);
    var $tableContainer = $header.closest('.glass-table-container');
    // Find the rows container specifically (Right Side)
    var $rowsContainer = $tableContainer.find('.glass-table-rows-container');
    var idx = $header.index();

    // Determine Sort Direction
    var currentOrder = $header.data('sort-dir') || 'none';
    var newOrder = (currentOrder === 'asc') ? 'desc' : 'asc';

    // Reset other headers
    $tableContainer.find('.glass-table-header-cell')
      .data('sort-dir', 'none')
      .removeClass('asc desc')
      .find('.glass-sort-icon')
        .removeClass('fa-sort-up fa-sort-down')
        .addClass('fa-sort');

    // Update active header
    $header.data('sort-dir', newOrder).addClass(newOrder);
    var iconClass = (newOrder === 'asc') ? 'fa-sort-up' : 'fa-sort-down';
    $header.find('.glass-sort-icon').removeClass('fa-sort').addClass(iconClass);

    // Get Rows
    var rows = $rowsContainer.children('.glass-table-row').toArray();

    rows.sort(function(a, b) {
      var $cellA = $(a).children('.glass-table-cell').eq(idx);
      var $cellB = $(b).children('.glass-table-cell').eq(idx);

      var valA = $cellA.text().trim();
      var valB = $cellB.text().trim();

      // Numeric Parsing
      var numA = parseFloat(valA.replace(/,/g, ''));
      var numB = parseFloat(valB.replace(/,/g, ''));

      var isNumA = !isNaN(numA) && valA !== "";
      var isNumB = !isNaN(numB) && valB !== "";

      if (isNumA && isNumB) {
        return (newOrder === 'asc') ? numA - numB : numB - numA;
      } else {
        return (newOrder === 'asc') ? valA.localeCompare(valB) : valB.localeCompare(valA);
      }
    });

    // Re-append sorted rows
    $rowsContainer.append(rows);
  });
}
