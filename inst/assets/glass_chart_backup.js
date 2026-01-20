// ==========================================================================
// GLASS CHART JS v15.2 - Centered Titles, Spacing Fix, Updated Hints
// ==========================================================================

(function () {
  $(document).on("shiny:connected", function () {
    Shiny.addCustomMessageHandler("update_glass_chart", function (msg) {
      GlassChart.render(msg.id, msg.data);
    });
  });

  $(document).on("shiny:disconnected", function () {
    GlassChart.destroyAll();
  });

  const charts = new Map();

  const GlassChart = {
    render(containerId, payload) {
      let chart = charts.get(containerId);
      const container = document.getElementById(containerId);

      // Fix for Stale DOM
      if (chart && container && container.querySelectorAll("svg").length === 0) {
        GlassChart.destroy(containerId);
        chart = null;
      }

      if (!chart) {
        chart = createChart(containerId);
        charts.set(containerId, chart);
      }
      chart.update(payload);
    },
    destroy(containerId) {
      const chart = charts.get(containerId);
      if (chart) {
        chart.destroy();
        charts.delete(containerId);
      }
    },
    destroyAll() {
      for (const [id] of charts) GlassChart.destroy(id);
    },
  };

  window.GlassChart = GlassChart;

  // ---- Helpers ----
  function safeId(str) { return String(str).replace(/[^a-zA-Z0-9_-]/g, "_"); }

  function extentOrDefault(values, fallback = [0, 1]) {
    const e = d3.extent(values);
    if (!e || e[0] == null || e[1] == null || isNaN(e[0]) || isNaN(e[1])) return fallback;
    if (e[0] === e[1]) return [e[0] - 1, e[1] + 1];
    return e;
  }

  function sendToShiny(name, value) {
    if (window.Shiny && typeof Shiny.setInputValue === "function") {
      Shiny.setInputValue(name, value, { priority: "event" });
    }
  }

  // ---- Chart Factory ----
  function createChart(containerId) {
    const fmt2 = d3.format(".2f");

    const container = document.getElementById(containerId);
    if (!container) return { update() {}, destroy() {} };

    container.tabIndex = 0;

    // Block Browser Zoom (Ctrl+Wheel)
    container.addEventListener("wheel", function(e) {
      if (e.ctrlKey) { e.preventDefault(); e.stopPropagation(); }
    }, { passive: false });

    container.addEventListener("mouseenter", () => {
      container.focus({ preventScroll: true });
    });

    // --- 1. KEYBOARD LISTENERS FOR ALT FEEDBACK ---
    container.addEventListener("keydown", (e) => {
      if (e.key === "Alt") {
        container.classList.add("alt-scroll-mode");
      }
    });

    container.addEventListener("keyup", (e) => {
      if (e.key === "Alt") {
        container.classList.remove("alt-scroll-mode");
      }
    });

    container.addEventListener("blur", () => {
       container.classList.remove("alt-scroll-mode");
    });

    // Keyboard Scrolling (Alt + Arrows)
    container.addEventListener("keydown", (e) => {
      if (e.key === "Escape") return;

      if (e.altKey) {
        const step = 50;
        let scrolled = false;
        if (e.key === "ArrowRight") { container.scrollLeft += step; scrolled = true; }
        else if (e.key === "ArrowLeft") { container.scrollLeft -= step; scrolled = true; }
        else if (e.key === "ArrowDown") { container.scrollTop += step; scrolled = true; }
        else if (e.key === "ArrowUp") { container.scrollTop -= step; scrolled = true; }

        if (scrolled) { e.preventDefault(); e.stopPropagation(); }
      }
    });

    const root = d3.select(container);
    const svg = root.append("svg").attr("class", "glass-chart")
      .style("overflow", "visible").attr("role", "img");

    const defs = svg.append("defs");
    defs.append("marker").attr("id", "arrow-end").attr("viewBox", "0 0 10 10")
      .attr("refX", 10).attr("refY", 5).attr("markerWidth", 6).attr("markerHeight", 6)
      .attr("orient", "auto").append("path").attr("d", "M 0 0 L 10 5 L 0 10 z").attr("fill", "black");

    const grad = defs.append("linearGradient")
      .attr("id", "shard-gradient")
      .attr("x1", "0%").attr("y1", "0%").attr("x2", "100%").attr("y2", "100%");
    grad.append("stop").attr("offset", "0%").attr("stop-color", "#7470B8");
    grad.append("stop").attr("offset", "50%").attr("stop-color", "#605CA8");
    grad.append("stop").attr("offset", "100%").attr("stop-color", "#4B4885");

    // --- UPDATED HINT TEXT ---
    root.append("div").attr("class", "glass-chart-hint")
      .html("Ctrl+Wheel: Zoom &bull; Ctrl+Drag: Box Zoom &bull; Ctrl+Shift: Sync &bull; Alt+Arrows: Scroll");

    const tooltipId = `glass-tooltip-${safeId(containerId)}`;
    d3.select(`#${CSS.escape(tooltipId)}`).remove();
    const tooltip = d3.select("body").append("div").attr("id", tooltipId)
      .attr("class", "glass-chart-tooltip").style("opacity", 0);

    const state = {
      lastPayload: null, facets: new Map(), selectedIds: new Set(), hoveredId: null,
      tooltipPinned: false, pinnedId: null, pinnedHtml: null, resizeObserver: null,
      isShift: false, isCtrl: false, hoveredFacet: null, focusMode: null
    };

    function updateFocusRects() {
      svg.selectAll(".facet-border").classed("focused", false).classed("global-focused", false);
      if (state.focusMode) return;
      if (state.isShift && state.isCtrl) {
        svg.selectAll(".facet-border").classed("global-focused", true);
        return;
      }
      if (state.isCtrl && state.hoveredFacet) {
        state.hoveredFacet.g.select(".facet-border").classed("focused", true);
      }
    }

    function triggerLimitFeedback(facetState) {
      const border = facetState.g.select(".facet-border");
      border.classed("limit-error", false);
      void border.node().offsetWidth;
      border.classed("limit-error", true);
      setTimeout(() => { border.classed("limit-error", false); }, 600);
    }

    d3.select("body")
      .on("keydown.glass_" + safeId(containerId), (e) => {
        if (e.shiftKey) { state.isShift = true; svg.classed("brush-mode", true); }
        if (e.ctrlKey) { state.isCtrl = true; svg.classed("zoom-box-mode", true); }
        if (e.key === "Enter" && state.isCtrl && state.hoveredFacet && !state.focusMode) {
          state.focusMode = state.hoveredFacet.comp;
          update(state.lastPayload, { preserveZoom: false });
        }
        updateFocusRects();
      })
      .on("keyup.glass_" + safeId(containerId), (e) => {
        if (!e.shiftKey) { state.isShift = false; svg.classed("brush-mode", false); }
        if (!e.ctrlKey) { state.isCtrl = false; svg.classed("zoom-box-mode", false); }
        updateFocusRects();
      });

    root.on("pointerdown", (event) => {
      const tipNode = tooltip.node();
      if (tipNode && tipNode.contains(event.target)) return;
      if (state.tooltipPinned) unpinTooltip();
    });

    container.addEventListener("keydown", (e) => {
      if (e.key === "Escape") {
        if (state.focusMode) {
          state.focusMode = null;
          update(state.lastPayload, { preserveZoom: false });
          return;
        }
        state.selectedIds.clear();
        applyGlobalSelectionStyles();
        state.hoveredId = null;
        applyGlobalHoverStyles();
        unpinTooltip(true);
        sendToShiny(`${containerId}_selected_ids`, []);
        sendToShiny(`${containerId}_hover_id`, null);
      }
    });

    state.resizeObserver = new ResizeObserver(() => {
      if (state.lastPayload) update(state.lastPayload, { preserveZoom: true });
    });
    state.resizeObserver.observe(container);

    function destroy() {
      try {
        state.resizeObserver && state.resizeObserver.disconnect();
        d3.select("body").on(".glass_" + safeId(containerId), null);
      } catch {}
      tooltip.remove();
      root.selectAll("*").remove();
    }

    function placeTooltip(clientX, clientY) {
      const tipNode = tooltip.node();
      if (!tipNode) return;
      const tw = tipNode.offsetWidth || 260; const th = tipNode.offsetHeight || 60;
      const pad = 8;
      let x = clientX + 14; let y = clientY - 10;
      if (x + tw > window.innerWidth - pad) x = clientX - tw - 14;
      if (y + th > window.innerHeight - pad) y = clientY - th - 14;
      if (x < pad) x = pad; if (y < pad) y = pad;
      tooltip.style("left", `${x}px`).style("top", `${y}px`);
    }

    function showTooltip(event, html, { pin = false, sampleId = null } = {}) {
      if (!pin && state.tooltipPinned) {
        if (sampleId && state.pinnedId && sampleId !== state.pinnedId) return;
      }
      if (pin) {
        state.tooltipPinned = true;
        state.pinnedId = sampleId;
        state.pinnedHtml = html;
        tooltip.classed("pinned", true);
      }
      tooltip.html(html).style("opacity", 1);
      placeTooltip(event.clientX, event.clientY);
    }

    function hideTooltip() {
      if (state.tooltipPinned) return;
      tooltip.classed("pinned", false).style("opacity", 0);
    }

    function unpinTooltip(forceHide = false) {
      state.tooltipPinned = false; state.pinnedId = null; state.pinnedHtml = null;
      tooltip.classed("pinned", false);
      svg.selectAll(".pinned-node").classed("pinned-node", false);
      if (forceHide) tooltip.style("opacity", 0); else hideTooltip();
    }

    function applyGlobalHoverStyles() {
      svg.selectAll(".cs-point").classed("hovered", false);
      if (!state.hoveredId) { sendToShiny(`${containerId}_hover_id`, null); return; }
      svg.selectAll(`[data-sampleid="${CSS.escape(String(state.hoveredId))}"]`).classed("hovered", true);
      sendToShiny(`${containerId}_hover_id`, state.hoveredId);
    }

    function applyGlobalSelectionStyles() {
      const hasSel = state.selectedIds.size > 0;
      svg.selectAll(".cs-point, .ce-point")
        .classed("selected", (d) => hasSel && state.selectedIds.has(d.SampleID))
        .classed("dimmed", (d) => hasSel && !state.selectedIds.has(d.SampleID));
      sendToShiny(`${containerId}_selected_ids`, Array.from(state.selectedIds));
    }

    // ---- Facet Skeleton ----
    function initFacet(comp, isFocusMode) {
      const g = svg.append("g").attr("class", "facet").attr("data-comp", comp);
      const zoomRect = g.append("rect").attr("class", "zoom-capture").attr("fill", "transparent");

      g.append("rect").attr("class", "facet-border");
      g.append("g").attr("class", "x-axis");
      g.append("g").attr("class", "y-axis");

      const xShaft = g.append("line").attr("class", "axis-shaft x-shaft").attr("marker-end", "url(#arrow-end)");
      const yShaft = g.append("line").attr("class", "axis-shaft y-shaft").attr("marker-end", "url(#arrow-end)");

      if (!isFocusMode) {
        g.append("path").attr("class", "facet-title-strip");

        // --- SPLIT TEXT ELEMENTS FOR ANIMATION ---
        g.append("text").attr("class", "facet-title facet-title-main");
        g.append("text").attr("class", "facet-title-split facet-title-y");
        g.append("text").attr("class", "facet-title-split facet-title-x");
      }

      const clipId = `clip-${safeId(containerId)}-${safeId(comp)}`;
      g.append("defs").append("clipPath").attr("id", clipId).append("rect");

      const brushLayer = g.append("g").attr("class", "brush-layer");
      const plotArea = g.append("g").attr("class", "plot-area").attr("clip-path", `url(#${clipId})`);
      const hoverLayer = plotArea.append("g").attr("class", "hover-layer");

      const facetState = {
        comp, g, clipId, plotArea, brushLayer, zoomRect, hoverLayer,
        xAxisG: g.select(".x-axis"), yAxisG: g.select(".y-axis"),
        xShaft, yShaft,
        x: null, y: null, currentX: null, currentY: null,
        zoom: null, zoomTransform: d3.zoomIdentity, rafPending: false,
        ribbon: null, csSel: null, errSel: null, capTopSel: null, capBotSel: null, ceSel: null,
        brush: null, isFocusMode: isFocusMode,
        redraw: function() {}
      };

      g.on("pointerenter", () => { state.hoveredFacet = facetState; updateFocusRects(); })
       .on("pointerleave", () => { if (state.hoveredFacet === facetState) { state.hoveredFacet = null; updateFocusRects(); } });

      state.facets.set(comp, facetState);
      return facetState;
    }

    // ---- Main Update ----
    function update(payload, { preserveZoom = true } = {}) {
      state.lastPayload = payload;
      if (!payload) return;

      const isFocus = !!state.focusMode;
      svg.classed("focused-view", isFocus);

      const csData = payload.cs || [];
      const pbData = payload.pb || [];
      const ceData = payload.ce || [];
      const params = payload.params || {};

      const csByComp = d3.group(csData, (d) => d.comparison);
      const pbByComp = d3.group(pbData, (d) => d.comparison);
      const ceByComp = d3.group(ceData, (d) => d.comparison);

      let comparisons = [];
      if (isFocus) {
        comparisons = [state.focusMode];
      } else {
        const comparisonsSet = new Set();
        [csData, pbData, ceData].forEach(arr => arr.forEach(d => { if(d.comparison) comparisonsSet.add(d.comparison); }));
        comparisons = Array.from(comparisonsSet);
      }

      const clientW = container.clientWidth || 800;
      const clientH = (container.clientHeight && container.clientHeight > 0) ? container.clientHeight : 600;

      let margin, nCols, nRows, facetSize, totalWidth, totalHeight;
      const gap = 50;

      if (isFocus) {
        margin = { top: 40, right: 40, bottom: 60, left: 60 };
        nCols = 1; nRows = 1;
        facetSize = Math.min(clientW - margin.left - margin.right, clientH - margin.top - margin.bottom);
        totalWidth = clientW;
        totalHeight = clientH;
      } else {
        // --- INCREASED TOP MARGIN FOR HINT TEXT ---
        margin = { top: 60, right: 20, bottom: 40, left: 50 };
        const nFacets = Math.max(comparisons.length, 1);

        nCols = Math.ceil(Math.sqrt(nFacets));
        nRows = Math.ceil(nFacets / nCols);

        const availableWidth = clientW - margin.left - margin.right - (gap * (nCols - 1));
        let calculatedSize = availableWidth / nCols;

        const MIN_FACET_SIZE = 150;
        const MAX_FACET_SIZE = 300;

        facetSize = Math.max(calculatedSize, MIN_FACET_SIZE);
        facetSize = Math.min(facetSize, MAX_FACET_SIZE);

        totalWidth = margin.left + margin.right + (nCols * facetSize) + ((nCols - 1) * gap);
        totalHeight = margin.top + margin.bottom + (nRows * facetSize) + ((nRows - 1) * gap);
      }

      svg.attr("width", totalWidth).attr("height", totalHeight);

      const facetWidth = isFocus ? (clientW - margin.left - margin.right) : facetSize;
      const facetHeight = isFocus ? (clientH - margin.top - margin.bottom) : facetSize;

      svg.selectAll(".close-button-group").remove();
      if (isFocus) {
        const btn = svg.append("g").attr("class", "close-button-group")
           .attr("transform", `translate(${totalWidth - 24}, 24)`)
           .on("click", () => {
             state.focusMode = null;
             update(state.lastPayload, { preserveZoom: false });
           });
        btn.append("circle").attr("class", "close-button-bg").attr("r", 14);
        btn.append("line").attr("class", "close-button-x").attr("x1", -5).attr("y1", -5).attr("x2", 5).attr("y2", 5);
        btn.append("line").attr("class", "close-button-x").attr("x1", 5).attr("y1", -5).attr("x2", -5).attr("y2", 5);
      }

      for (const [comp, facetState] of state.facets) {
        if (facetState.isFocusMode !== isFocus || !comparisons.includes(comp)) {
          facetState.g.remove();
          state.facets.delete(comp);
        }
      }

      comparisons.forEach(comp => { if (!state.facets.has(comp)) initFacet(comp, isFocus); });

      comparisons.forEach((comp, i) => {
        const facetState = state.facets.get(comp);
        const g = facetState.g;

        let tx, ty;
        if (isFocus) {
          tx = margin.left;
          ty = margin.top;
        } else {
          const row = Math.floor(i / nCols);
          const col = i % nCols;
          tx = margin.left + col * (facetWidth + gap);
          ty = margin.top + row * (facetHeight + gap);
        }

        const f_cs = csByComp.get(comp) || [];
        const f_pb = pbByComp.get(comp) || [];
        const f_ce = ceByComp.get(comp) || [];

        // ZETA Label
        const meta = f_ce[0] || {};
        const hasMeta = f_ce.length > 0;
        g.selectAll(".zeta-label").remove();
        if (hasMeta && meta.zeta != null) {
          const zText = `\u03B6\u0302 = ${fmt2(meta.zeta)} (${fmt2(meta.zeta_ci_lwr)} - ${fmt2(meta.zeta_ci_upr)})`;
          const zetaLabel = g.append("text").attr("class", "zeta-label").attr("x", 10).attr("y", 15).attr("text-anchor", "start").text(zText);
          zetaLabel.on("pointerenter", (event) => {
             const html = `<strong>Zeta Statistics</strong><br><span class="muted">Critical Value:</span> \u03B6\u0302<sub>critical</sub> = ${fmt2(meta.zeta_upper)}`;
             showTooltip(event, html, { pin: false });
          }).on("pointerleave", hideTooltip);
        }

        const dinsExcessive = (f_pb[0] && f_pb[0].dins_conclusion === 1);
        const ribbonFill = dinsExcessive ? "#999999" : (params.pb_fill || "#4CAF50");

        const allY = [
          ...f_cs.map(d => d.MP_A), ...f_ce.map(d => d.MP_A),
          ...f_ce.map(d => d.pi_lwr), ...f_ce.map(d => d.pi_upr),
          ...f_pb.map(d => d.pi_lwr), ...f_pb.map(d => d.pi_upr),
        ].filter(v => v != null && !isNaN(v));

        const allX = [
          ...f_cs.map(d => d.MP_B), ...f_ce.map(d => d.MP_B),
          ...f_pb.map(d => d.predictor),
        ].filter(v => v != null && !isNaN(v));

        const x = d3.scaleLinear().domain(extentOrDefault(allX, [0, 1])).nice().range([0, facetWidth]);
        const y = d3.scaleLinear().domain(extentOrDefault(allY, [0, 1])).nice().range([facetHeight, 0]);

        facetState.x = x; facetState.y = y;

        if (!preserveZoom) facetState.zoomTransform = d3.zoomIdentity;
        facetState.currentX = facetState.zoomTransform.rescaleX(x);
        facetState.currentY = facetState.zoomTransform.rescaleY(y);

        g.attr("transform", `translate(${tx},${ty})`);
        g.select(".zoom-capture").attr("width", facetWidth).attr("height", facetHeight);
        g.select(".facet-border").attr("width", facetWidth).attr("height", facetHeight);
        g.select(`#${CSS.escape(facetState.clipId)} rect`).attr("width", facetWidth).attr("height", facetHeight);

        if (isFocus) {
          const names = comp.split(" - ");
          const yName = names[0] || "Method 1";
          const xName = names[1] || "Method 2";
          g.selectAll(".axis-label-large").remove();
          g.append("text").attr("class", "axis-label-large").attr("x", facetWidth / 2).attr("y", facetHeight + 40).text(xName);
          g.append("text").attr("class", "axis-label-large").attr("transform", "rotate(-90)").attr("x", -facetHeight / 2).attr("y", -40).text(yName);
        } else {
          // --- ROUNDED FACET CORNERS ---
          const stripH = 24;
          const R = 8;

          const shardPath = `
            M 0 -${stripH - R}
            Q 0 -${stripH} ${R} -${stripH}
            L ${facetWidth - R} -${stripH}
            Q ${facetWidth} -${stripH} ${facetWidth} -${stripH - R}
            L ${facetWidth} 0
            L 0 0
            Z
          `;

          const strip = g.select(".facet-title-strip")
            .attr("d", shardPath)
            .attr("fill", "url(#shard-gradient)")
            .attr("stroke", "none");

          // --- ANIMATED AXIS LABELS ---
          const names = comp.split(" - ");
          const yName = names[0] || "Method 1";
          const xName = names[1] || "Method 2";

          const titleMain = g.select(".facet-title-main")
            .attr("x", facetWidth / 2).attr("y", -8)
            .attr("text-anchor", "middle") // <--- FIXED CENTER ALIGNMENT
            .attr("transform", null)
            .style("opacity", 1)
            .text(comp);

          const titleY = g.select(".facet-title-y")
            .text(yName)
            .style("opacity", 0)
            .attr("transform", `translate(${facetWidth/2}, -8) rotate(0)`);

          const titleX = g.select(".facet-title-x")
            .text(xName)
            .style("opacity", 0)
            .attr("transform", `translate(${facetWidth/2}, -8)`);

          // Animation Events
          strip.on("pointerenter.anim", () => {
             // 1. Fade out Main Title
             titleMain.transition().duration(400).style("opacity", 0);

             // 2. Move Y Label to Left Axis
             titleY.transition().duration(400)
               .style("opacity", 1)
               .attr("transform", `translate(-35, ${facetHeight/2}) rotate(-90)`);

             // 3. Move X Label to Inside Bottom Axis (CORRECTED POSITION)
             titleX.transition().duration(400)
               .style("opacity", 1)
               .attr("transform", `translate(${facetWidth/2}, ${facetHeight - 10})`);
          });

          strip.on("pointerleave.anim", () => {
             // Revert
             titleMain.transition().duration(400).style("opacity", 1);
             titleY.transition().duration(400)
               .style("opacity", 0)
               .attr("transform", `translate(${facetWidth/2}, -8) rotate(0)`);
             titleX.transition().duration(400)
               .style("opacity", 0)
               .attr("transform", `translate(${facetWidth/2}, -8)`);
          });
        }

        facetState.xShaft.attr("x1", 0).attr("y1", facetHeight).attr("x2", facetWidth).attr("y2", facetHeight);
        facetState.yShaft.attr("x1", 0).attr("y1", facetHeight).attr("x2", 0).attr("y2", 0);

        const scaleMult = isFocus ? 1.8 : 1.0;
        const radiusCS = 2.5 * scaleMult;
        const radiusCE = 5.0 * scaleMult;
        const strokeW = 1.5 * (isFocus ? 1.5 : 1.0);

        const plotArea = facetState.plotArea;
        const pbSorted = [...f_pb].sort((a, b) => (a.predictor || 0) - (b.predictor || 0));
        facetState.ribbon = plotArea.selectAll("path.ribbon").data([pbSorted]).join("path")
          .attr("class", "ribbon").attr("fill", ribbonFill).attr("fill-opacity", 0.25).attr("stroke", params.pb_border || "none");

        if (params.exclude_cs) {
          plotArea.selectAll("circle.cs-point").remove();
          facetState.csSel = null;
        } else {
          facetState.csSel = plotArea.selectAll("circle.cs-point").data(f_cs, d => d.SampleID).join("circle")
            .attr("class", "cs-point").attr("r", radiusCS).attr("fill", "#999").attr("stroke", "#333").attr("stroke-width", 0.5)
            .style("cursor", "pointer").attr("data-sampleid", d => d.SampleID);
        }

        const ceGroup = plotArea.selectAll("g.ce-objects").data([null]).join("g").attr("class", "ce-objects");
        const getStroke = (d) => (d.pi_inside === 1 ? "#1994DC" : "#DC1932");
        facetState.errSel = ceGroup.selectAll("line.err-line").data(f_ce, d => d.SampleID).join("line").attr("class", "err-line").attr("stroke-width", strokeW).attr("stroke", getStroke);
        facetState.capTopSel = ceGroup.selectAll("line.cap-top").data(f_ce, d => d.SampleID).join("line").attr("class", "cap-top").attr("stroke-width", strokeW).attr("stroke", getStroke);
        facetState.capBotSel = ceGroup.selectAll("line.cap-bot").data(f_ce, d => d.SampleID).join("line").attr("class", "cap-bot").attr("stroke-width", strokeW).attr("stroke", getStroke);
        facetState.ceSel = ceGroup.selectAll("circle.ce-point").data(f_ce, d => d.SampleID).join("circle").attr("class", "ce-point").attr("r", radiusCE).attr("stroke", "white").attr("stroke-width", 1).style("cursor", "pointer").attr("fill", getStroke).attr("data-sampleid", d => d.SampleID);

        function redraw(newX, newY) {
          facetState.xAxisG.attr("transform", `translate(0,${facetHeight})`).call(d3.axisBottom(newX).ticks(isFocus ? 10 : 5));
          facetState.yAxisG.call(d3.axisLeft(newY).ticks(isFocus ? 10 : 5));
          if (isFocus) g.selectAll(".tick text").style("font-size", "14px");
          else g.selectAll(".tick text").style("font-size", "10px");
          const area = d3.area().x(d => newX(d.predictor)).y0(d => newY(d.pi_lwr)).y1(d => newY(d.pi_upr));
          facetState.ribbon.attr("d", area);
          if (facetState.csSel) facetState.csSel.attr("cx", d => newX(d.MP_B)).attr("cy", d => newY(d.MP_A));
          facetState.errSel.attr("x1", d => newX(d.MP_B)).attr("x2", d => newX(d.MP_B)).attr("y1", d => newY(d.pi_lwr)).attr("y2", d => newY(d.pi_upr));
          const cap = 4 * scaleMult;
          facetState.capTopSel.attr("x1", d => newX(d.MP_B)-cap).attr("x2", d => newX(d.MP_B)+cap).attr("y1", d => newY(d.pi_upr)).attr("y2", d => newY(d.pi_upr));
          facetState.capBotSel.attr("x1", d => newX(d.MP_B)-cap).attr("x2", d => newX(d.MP_B)+cap).attr("y1", d => newY(d.pi_lwr)).attr("y2", d => newY(d.pi_lwr));
          facetState.ceSel.attr("cx", d => newX(d.MP_B)).attr("cy", d => newY(d.MP_A));
          facetState.hoverLayer.selectAll("*").remove();
        }

        facetState.redraw = redraw;
        redraw(facetState.currentX, facetState.currentY);

        // ... Attach Point Handlers ...
        function attachPointHandlers(sel, kind) {
          const names = comp.split(" - ");
          const yName = names[0] || "Method 1";
          const xName = names[1] || "Method 2";
          const buildTooltip = (d, isPinned) => {
            let statusHtml = "";
            if (kind === "ce") {
              if (d.dins_conclusion === 1) {
                const loc = (d.pi_inside === 1) ? "Inside PI" : "Outside PI";
                statusHtml = `<span style="color:#333">${loc}</span><br><span class="muted" style="font-style:italic">Cannot evaluate commutability<br>when DINS is excessive</span>`;
              } else {
                if (d.pi_inside === 1) statusHtml = `<span style="color:#28A745; font-weight:bold">Commutable</span>`;
                else statusHtml = `<span style="color:#E57373; font-weight:bold">Non-commutable</span>`;
              }
            }
            const piText = (kind === "ce") ? `[${fmt2(d.pi_lwr)} - ${fmt2(d.pi_upr)}]` : "";
            const footer = isPinned ? `<span class="muted" style="color:#2196F3">(Pinned — click to unpin)</span>` : `<span class="muted">(Click to pin)</span>`;
            if (kind === "ce") return `<strong>${d.SampleID}</strong><br>${statusHtml}<br><span class="muted">${xName}:</span> ${fmt2(d.MP_B)}<br><span class="muted">${yName}:</span> ${fmt2(d.MP_A)}<br><span class="muted">PI:</span> ${piText}<br><span class="muted">Rate:</span> ${(d.inside_rate * 100).toFixed(0)}%<br>${footer}`;
            else return `<strong>Clinical Sample</strong><br><span class="muted">ID:</span> ${d.SampleID}<br><span class="muted">${xName}:</span> ${fmt2(d.MP_B)}<br><span class="muted">${yName}:</span> ${fmt2(d.MP_A)}<br><span class="muted">IVD-MD Comparison:</span> ${comp}<br>${footer}`;
          };
          sel.on("pointerenter", function(event, d) {
              state.hoveredId = d.SampleID; applyGlobalHoverStyles();
              if (!state.tooltipPinned) {
                if (kind === "ce" && d.inside_rate > 0) {
                  const r = (isFocus ? 8 : 5) + 3; const c = 2 * Math.PI * r; const dash = c * d.inside_rate;
                  facetState.hoverLayer.append("circle").attr("class", "rate-ring-bg").attr("r", r).attr("cx", facetState.currentX(d.MP_B)).attr("cy", facetState.currentY(d.MP_A));
                  facetState.hoverLayer.append("circle").attr("class", "rate-ring").attr("r", r).attr("cx", facetState.currentX(d.MP_B)).attr("cy", facetState.currentY(d.MP_A)).style("stroke-dasharray", `${dash} ${c}`).style("transform-origin", `${facetState.currentX(d.MP_B)}px ${facetState.currentY(d.MP_A)}px`).style("transform", "rotate(-90deg)");
                } else if (kind === "cs") d3.select(this).attr("r", (isFocus ? 8 : 5)).attr("fill", "orange");
                showTooltip(event, buildTooltip(d, false), { pin: false, sampleId: d.SampleID });
              }
            })
            .on("pointermove", (e) => { if (!state.tooltipPinned) placeTooltip(e.clientX, e.clientY); })
            .on("pointerleave", function() {
              state.hoveredId = null; applyGlobalHoverStyles(); facetState.hoverLayer.selectAll("*").remove();
              if (!state.tooltipPinned) { if (kind === "cs") d3.select(this).attr("r", radiusCS).attr("fill", "#999"); hideTooltip(); }
            })
            .on("click", function(event, d) {
              event.stopPropagation();
              if (state.tooltipPinned && state.pinnedId === d.SampleID) { unpinTooltip(true); if (kind === "ce") d3.select(this).classed("pinned-node", false); }
              else { unpinTooltip(false); if (kind === "ce") d3.select(this).classed("pinned-node", true); showTooltip(event, buildTooltip(d, true), { pin: true, sampleId: d.SampleID }); }
            });
        }
        if (facetState.csSel) attachPointHandlers(facetState.csSel, "cs");
        attachPointHandlers(facetState.ceSel, "ce");

        if (!facetState.zoom) {
          facetState.zoom = d3.zoom()
            .scaleExtent([1, 200])
            .translateExtent([[0, 0], [facetWidth, facetHeight]])
            .extent([[0, 0], [facetWidth, facetHeight]])
            .wheelDelta((event) => -event.deltaY * (event.deltaMode === 1 ? 0.05 : event.deltaMode ? 1 : 0.002))
            .on("zoom", (event) => {
              if (facetState.zoomRect.node() !== this) facetState.zoomRect.property("__zoom", event.transform);
              if (facetState.brushLayer.node() !== this) facetState.brushLayer.property("__zoom", event.transform);
              const newK = event.transform.k;
              if ((Math.abs(newK - 1) < 0.001 && event.sourceEvent && event.sourceEvent.deltaY > 0) || (Math.abs(newK - 200) < 0.001 && event.sourceEvent && event.sourceEvent.deltaY < 0)) triggerLimitFeedback(facetState);
              facetState.zoomTransform = event.transform;
              if (event.sourceEvent && event.sourceEvent.shiftKey && event.sourceEvent.ctrlKey) {
                state.facets.forEach((other) => {
                  if (other !== facetState) {
                    other.zoomRect.call(other.zoom.transform, event.transform);
                    other.brushLayer.call(other.zoom.transform, event.transform);
                    if ((Math.abs(newK - 1) < 0.001 && event.sourceEvent && event.sourceEvent.deltaY > 0) || (Math.abs(newK - 200) < 0.001 && event.sourceEvent && event.sourceEvent.deltaY < 0)) triggerLimitFeedback(other);
                  }
                });
              }
              if (facetState.rafPending) return;
              facetState.rafPending = true;
              requestAnimationFrame(() => {
                facetState.rafPending = false;
                facetState.currentX = facetState.zoomTransform.rescaleX(facetState.x);
                facetState.currentY = facetState.zoomTransform.rescaleY(facetState.y);
                facetState.redraw(facetState.currentX, facetState.currentY);
                if (state.tooltipPinned && state.pinnedHtml) tooltip.classed("pinned", true).style("opacity", 1).html(state.pinnedHtml);
              });
            });

          facetState.zoom.filter((event) => {
            if (event.type === "mousedown") return !event.shiftKey && !event.ctrlKey;
            if (event.type === "wheel" && event.ctrlKey) return true;
            if (event.type === "dblclick") return true;
            return false;
          });

          facetState.zoomRect.call(facetState.zoom).on("dblclick.zoom", () => {
            const reset = d3.zoomIdentity;
            state.facets.forEach(f => { f.zoomRect.call(f.zoom.transform, reset); f.brushLayer.call(f.zoom.transform, reset); });
          });
          facetState.brushLayer.call(facetState.zoom);
        }

        facetState.zoom.extent([[0, 0], [facetWidth, facetHeight]]).translateExtent([[0, 0], [facetWidth, facetHeight]]);
        facetState.zoomRect.call(facetState.zoom.transform, facetState.zoomTransform);
        facetState.brushLayer.call(facetState.zoom.transform, facetState.zoomTransform);

        if (!facetState.brush) {
          facetState.brush = d3.brush().extent([[0, 0], [facetWidth, facetHeight]])
            .filter((event) => event.type === "mousedown" && event.button === 0 && (event.shiftKey || event.ctrlKey))
            .on("end", (event) => {
              const sel = event.selection;
              if (!sel) { if (!event.sourceEvent) return; state.selectedIds.clear(); applyGlobalSelectionStyles(); return; }
              if (state.isCtrl) {
                const [[x0, y0], [x1, y1]] = sel;
                facetState.brushLayer.call(facetState.brush.move, null);
                const dx = x1 - x0, dy = y1 - y0; if (dx === 0 || dy === 0) return;
                const s = Math.min(200 / facetState.zoomTransform.k, 0.9 / Math.max(dx / facetWidth, dy / facetHeight));
                const selectionCenter = [x0 + dx/2, y0 + dy/2];
                const worldCenter = facetState.zoomTransform.invert(selectionCenter);
                const newK = facetState.zoomTransform.k * s;
                const newX = (facetWidth / 2) - newK * worldCenter[0];
                const newY = (facetHeight / 2) - newK * worldCenter[1];
                const newT = d3.zoomIdentity.translate(newX, newY).scale(newK);
                facetState.zoomRect.transition().duration(750).call(facetState.zoom.transform, newT);
                facetState.brushLayer.transition().duration(750).call(facetState.zoom.transform, newT);
                return;
              }
              if (state.isShift) {
                const [[x0, y0], [x1, y1]] = sel;
                const chosen = new Set();
                const X = facetState.currentX; const Y = facetState.currentY;
                const check = (list) => { for (const d of list) { const px = X(d.MP_B); const py = Y(d.MP_A); if (px >= x0 && px <= x1 && py >= y0 && py <= y1) chosen.add(d.SampleID); } };
                check(f_ce); if (!params.exclude_cs) check(f_cs);
                state.selectedIds = chosen; applyGlobalSelectionStyles();
              }
            });
        }
        facetState.brush.extent([[0, 0], [facetWidth, facetHeight]]);
        facetState.brushLayer.attr("clip-path", `url(#${facetState.clipId})`).call(facetState.brush).on("click", (e) => e.stopPropagation());
        facetState.brushLayer.selectAll(".selection").attr("stroke", "rgba(0,0,0,0.35)").attr("fill", "rgba(0,0,0,0.10)");
      });

      applyGlobalHoverStyles();
      applyGlobalSelectionStyles();
      if (state.tooltipPinned && state.pinnedHtml) tooltip.classed("pinned", true).style("opacity", 1).html(state.pinnedHtml);
    }
    return { update, destroy };
  }
})();

