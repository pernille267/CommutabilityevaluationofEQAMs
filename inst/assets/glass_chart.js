// ==========================================================================
// GLASS CHART JS v2 - Robust Interaction Fixes
// ==========================================================================

$(document).on('shiny:connected', function() {
  Shiny.addCustomMessageHandler('update_glass_chart', function(msg) {
    renderGlassChart(msg.id, msg.data);
  });
});

function renderGlassChart(containerId, payload) {
  const container = document.getElementById(containerId);
  if (!container) return;

  // 1. Cleanup
  d3.select(container).selectAll("*").remove();

  // 2. Data Unpacking
  const csData = payload.cs;
  const pbData = payload.pb;
  const ceData = payload.ce;
  const params = payload.params;

  // 3. Layout Setup
  const width = container.clientWidth || 800;
  const height = container.clientHeight || 600;
  const margin = { top: 40, right: 20, bottom: 40, left: 50 };
  const gap = 50; // Gap between facets

  // Facet Calculation
  const comparisons = [...new Set(ceData.map(d => d.comparison))];
  const nFacets = comparisons.length;
  const nCols = Math.ceil(Math.sqrt(nFacets));
  const nRows = Math.ceil(nFacets / nCols);

  const facetWidth = (width - margin.left - margin.right - (gap * (nCols - 1))) / nCols;
  const facetHeight = (height - margin.top - margin.bottom - (gap * (nRows - 1))) / nRows;

  // 4. Create Main SVG
  const svg = d3.select(container)
    .append("svg")
    .attr("width", width)
    .attr("height", height)
    .attr("viewBox", [0, 0, width, height])
    .style("max-width", "100%")
    .style("overflow", "visible"); // Allow tooltips to pop out if needed

  // Tooltip Div (Hidden by default)
  const tooltip = d3.select(container)
    .append("div")
    .attr("class", "glass-chart-tooltip")
    .style("opacity", 0);

  // 5. Render Facets
  comparisons.forEach((comp, i) => {

    // Position
    const row = Math.floor(i / nCols);
    const col = i % nCols;
    const tx = margin.left + col * (facetWidth + gap);
    const ty = margin.top + row * (facetHeight + gap);

    // Data Filtering
    const f_cs = csData.filter(d => d.comparison === comp);
    const f_pb = pbData.filter(d => d.comparison === comp);
    const f_ce = ceData.filter(d => d.comparison === comp);

    // --- SCALE CALCULATION (Independent per facet) ---
    // Y Domain: Include CS points, CE points, and Prediction Bands
    const allY = [
      ...f_cs.map(d => d.MP_A),
      ...f_ce.map(d => d.MP_A),
      ...f_ce.map(d => d.pi_lwr),
      ...f_ce.map(d => d.pi_upr),
      ...f_pb.map(d => d.pi_lwr),
      ...f_pb.map(d => d.pi_upr)
    ].filter(v => v != null && !isNaN(v));

    // X Domain
    const allX = [
      ...f_cs.map(d => d.MP_B),
      ...f_ce.map(d => d.MP_B),
      ...f_pb.map(d => d.predictor)
    ].filter(v => v != null && !isNaN(v));

    const x = d3.scaleLinear()
      .domain(d3.extent(allX)).nice()
      .range([0, facetWidth]);

    const y = d3.scaleLinear()
      .domain(d3.extent(allY)).nice()
      .range([facetHeight, 0]);

    // --- FACET GROUP ---
    // We create the group for this specific plot
    const g = svg.append("g")
      .attr("transform", `translate(${tx},${ty})`)
      .attr("class", "facet");

    // 1. Zoom Capture Layer (Background Rect)
    // Placed FIRST so it sits behind data. Capture drags for panning.
    // 'pointer-events: all' ensures it catches clicks even if transparent.
    const zoomRect = g.append("rect")
      .attr("class", "zoom-capture")
      .attr("width", facetWidth)
      .attr("height", facetHeight)
      .attr("fill", "transparent")
      .style("cursor", "crosshair");

    // 2. Axes Groups (So they sit behind data potentially, or above background)
    const xAxisG = g.append("g")
      .attr("class", "x-axis")
      .attr("transform", `translate(0,${facetHeight})`)
      .call(d3.axisBottom(x).ticks(5));

    const yAxisG = g.append("g")
      .attr("class", "y-axis")
      .call(d3.axisLeft(y).ticks(5));

    // Facet Title (Strip)
    g.append("rect")
      .attr("x", 0).attr("y", -24).attr("width", facetWidth).attr("height", 24)
      .attr("fill", params.comparison_fill || "#eee").attr("stroke", "#333");
    g.append("text")
      .attr("x", facetWidth/2).attr("y", -8)
      .attr("text-anchor", "middle")
      .style("font-weight", "bold").style("font-size", "11px")
      .text(comp);

    // 3. Clipping Defs (To prevent data spilling during zoom)
    const clipId = `clip-${containerId}-${i}`;
    g.append("defs").append("clipPath")
      .attr("id", clipId)
      .append("rect")
      .attr("width", facetWidth)
      .attr("height", facetHeight);

    // 4. Data Layer Group (Clipped)
    const plotArea = g.append("g")
      .attr("class", "plot-area")
      .attr("clip-path", `url(#${clipId})`);

    // --- DRAWING FUNCTIONS ---

    // Ribbon Generator
    const area = d3.area()
      .x(d => x(d.predictor))
      .y0(d => y(d.pi_lwr))
      .y1(d => y(d.pi_upr));

    // A. Prediction Bands
    const ribbon = plotArea.append("path")
      .datum(f_pb)
      .attr("class", "ribbon")
      .attr("fill", params.pb_fill || "#4CAF50")
      .attr("fill-opacity", 0.25)
      .attr("stroke", params.pb_border || "none")
      .attr("d", area);

    // B. Clinical Samples (Points)
    let csPoints = plotArea.selectAll(".cs-point");
    if (!params.exclude_cs) {
      csPoints = plotArea.selectAll(".cs-point")
        .data(f_cs)
        .enter().append("circle")
        .attr("class", "cs-point")
        .attr("cx", d => x(d.MP_B))
        .attr("cy", d => y(d.MP_A))
        .attr("r", 2.5)
        .attr("fill", "#999")
        .attr("stroke", "#333")
        .attr("stroke-width", 0.5)
        .style("cursor", "pointer");

      // Attach Tooltip events
      csPoints
        .on("mouseover", function(event, d) {
          d3.select(this).attr("r", 5).attr("fill", "orange");
          showTooltip(event, `
            <strong>Clinical Sample</strong><br>
            ID: ${d.SampleID}<br>
            x: ${d.MP_B.toFixed(2)}<br>
            y: ${d.MP_A.toFixed(2)}
          `);
        })
        .on("mouseout", function() {
          d3.select(this).attr("r", 2.5).attr("fill", "#999");
          hideTooltip();
        });
    }

    // C. Evaluated Materials (Lines + Points)
    // We group these so we can select them easily for zooming
    const ceGroup = plotArea.append("g").attr("class", "ce-objects");

    // 1. Error Bars (Vertical Line)
    const errorLines = ceGroup.selectAll(".err-line")
      .data(f_ce).enter().append("line")
      .attr("class", "err-line")
      .attr("x1", d => x(d.MP_B)).attr("x2", d => x(d.MP_B))
      .attr("y1", d => y(d.pi_lwr)).attr("y2", d => y(d.pi_upr))
      .attr("stroke", d => d.pi_inside === 1 ? "#1994DC" : "#DC1932")
      .attr("stroke-width", 1.5);

    // 2. Caps (Top and Bottom)
    const capWidth = 4; // visual width in pixels (static)
    // We need to store data to update position dynamically
    const topCaps = ceGroup.selectAll(".cap-top")
      .data(f_ce).enter().append("line").attr("class", "cap-top")
      .attr("stroke", d => d.pi_inside === 1 ? "#1994DC" : "#DC1932").attr("stroke-width", 1.5)
      .attr("x1", d => x(d.MP_B) - capWidth).attr("x2", d => x(d.MP_B) + capWidth)
      .attr("y1", d => y(d.pi_upr)).attr("y2", d => y(d.pi_upr));

    const botCaps = ceGroup.selectAll(".cap-bot")
      .data(f_ce).enter().append("line").attr("class", "cap-bot")
      .attr("stroke", d => d.pi_inside === 1 ? "#1994DC" : "#DC1932").attr("stroke-width", 1.5)
      .attr("x1", d => x(d.MP_B) - capWidth).attr("x2", d => x(d.MP_B) + capWidth)
      .attr("y1", d => y(d.pi_lwr)).attr("y2", d => y(d.pi_lwr));

    // 3. Points
    const cePoints = ceGroup.selectAll(".ce-point")
      .data(f_ce).enter().append("circle").attr("class", "ce-point")
      .attr("cx", d => x(d.MP_B)).attr("cy", d => y(d.MP_A))
      .attr("r", 5)
      .attr("fill", d => d.pi_inside === 1 ? "#1994DC" : "#DC1932")
      .attr("stroke", "white").attr("stroke-width", 1)
      .style("cursor", "pointer")
      .on("mouseover", function(event, d) {
        d3.select(this).attr("stroke", "black").attr("stroke-width", 2);
        const status = d.pi_inside === 1 ? "Inside PI" : "Outside PI";
        showTooltip(event, `
          <strong>${d.SampleID}</strong><br>
          <span style="color:${d.pi_inside===1?'#1994DC':'#DC1932'}">${status}</span><br>
          x: ${d.MP_B.toFixed(2)}<br>
          y: ${d.MP_A.toFixed(2)}
        `);
      })
      .on("mouseout", function() {
        d3.select(this).attr("stroke", "white").attr("stroke-width", 1);
        hideTooltip();
      });

    // --- ZOOM BEHAVIOR ---
    const zoom = d3.zoom()
      .scaleExtent([0.8, 10]) // Don't zoom out too far
      .extent([[0, 0], [facetWidth, facetHeight]])
      .on("zoom", zoomed);

    // Apply zoom to the background rect
    // IMPORTANT: "filter" prevents accidental wheel scroll.
    // We require Ctrl key for wheel zoom, OR standard drag panning.
    zoom.filter(function(event) {
       // Allow dragging (mousedown)
       if (event.type === 'mousedown') return true;
       // Allow wheel ONLY if ctrl key is pressed
       if (event.type === 'wheel' && event.ctrlKey) return true;
       // Allow double click
       if (event.type === 'dblclick') return true;
       return false;
    });

    zoomRect.call(zoom).on("dblclick.zoom", () => zoomRect.call(zoom.transform, d3.zoomIdentity));


    // --- ZOOM UPDATE FUNCTION ---
    function zoomed(event) {
      const newX = event.transform.rescaleX(x);
      const newY = event.transform.rescaleY(y);

      // 1. Update Axes
      xAxisG.call(d3.axisBottom(newX).ticks(5));
      yAxisG.call(d3.axisLeft(newY).ticks(5));

      // 2. Update Ribbon
      // We redefine the area generator with new scales
      const newArea = d3.area()
        .x(d => newX(d.predictor))
        .y0(d => newY(d.pi_lwr))
        .y1(d => newY(d.pi_upr));
      ribbon.attr("d", newArea);

      // 3. Update CS Points
      if (!params.exclude_cs) {
        plotArea.selectAll(".cs-point")
          .attr("cx", d => newX(d.MP_B))
          .attr("cy", d => newY(d.MP_A));
      }

      // 4. Update Error Lines
      plotArea.selectAll(".err-line")
        .attr("x1", d => newX(d.MP_B))
        .attr("x2", d => newX(d.MP_B))
        .attr("y1", d => newY(d.pi_lwr))
        .attr("y2", d => newY(d.pi_upr));

      // 5. Update Caps
      // Note: Cap width remains static in pixels (e.g., 4px), so we use newX center +/- constant
      plotArea.selectAll(".cap-top")
        .attr("x1", d => newX(d.MP_B) - capWidth).attr("x2", d => newX(d.MP_B) + capWidth)
        .attr("y1", d => newY(d.pi_upr)).attr("y2", d => newY(d.pi_upr));

      plotArea.selectAll(".cap-bot")
        .attr("x1", d => newX(d.MP_B) - capWidth).attr("x2", d => newX(d.MP_B) + capWidth)
        .attr("y1", d => newY(d.pi_lwr)).attr("y2", d => newY(d.pi_lwr));

      // 6. Update CE Points
      plotArea.selectAll(".ce-point")
        .attr("cx", d => newX(d.MP_B))
        .attr("cy", d => newY(d.MP_A));
    }

  }); // End Facet Loop

  // --- TOOLTIP HELPERS ---
  function showTooltip(event, html) {
    // Get mouse position relative to container
    // We use d3.pointer to get coordinates
    const [mx, my] = d3.pointer(event, container);

    tooltip
      .style("opacity", 1)
      .html(html)
      .style("left", (mx + 15) + "px")
      .style("top", (my - 10) + "px");
  }

  function hideTooltip() {
    tooltip.style("opacity", 0);
  }

}
