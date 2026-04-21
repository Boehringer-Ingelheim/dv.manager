// handle the dataset info bar
const dv_tab = (function () {

  // let log = console.log;
  let log = function () { return; };

  let containers = [];

  // This code assumes there will be a single instance of a dv_button_container
  // Lazy implementation once the need of several instances is found
  // Easy implementation just pass the container id across functions


  // If it is not notified, because of lazy evaluation, dynamic elements and outputs are not displayed
  // even when they are visible in the client
  const notify_shiny_display_change = function ($el, visibility) {
    $el.trigger(visibility);
  };

  const _set_tab_by_tab_id = function (tab_id, root_el) {
    let target_tab_element = root_el.querySelector(".dv_tab_activate_button[data-value='" + tab_id + "']");
    if (target_tab_element === null) {
      console.error("tab_id:'" + tab_id + "' not found in button set");
      return (false);
    }

    let next_element = tab_id;
    let clicked;

    do {
      clicked = root_el.querySelector(".dv_tab_activate_button[data-value='" + next_element + "']");
      const unclicked = clicked.parentElement.querySelector(".dv_tab_activate_button.clicked");

      // Apply changes elements
      unclicked.classList.remove("clicked");
      clicked.classList.add("clicked");

      // Look for next parent
      next_element = clicked.parentElement.getAttribute("value");
    } while (!clicked.parentElement.classList.contains("dv_root_button_level"));

    remove_active_all(root_el);
    const active_tab_id = set_clicked_active(root_el);
    return (active_tab_id);
  }

  const remove_active_all = function (root_el) {
    const button_levels_to_deactivate = root_el.querySelectorAll(".dv_child_button_level");
    for (let idx = 0; idx < button_levels_to_deactivate.length; ++idx) {
      const current_node = button_levels_to_deactivate[[idx]];
      current_node.classList.remove("active");
    }

    const tabs_to_deactivate = root_el.parentElement.parentElement.querySelectorAll(".dv_tab_container .dv_tab_content");
    for (let idx = 0; idx < tabs_to_deactivate.length; ++idx) {
      const current_node = tabs_to_deactivate[[idx]];
      current_node.classList.remove("active");
      notify_shiny_display_change($(current_node), "hidden");
    }
  }

  const set_clicked_active = function (root_el) {
    let curr_el = root_el.querySelector(".dv_root_button_level .dv_tab_activate_button.clicked");

    while (curr_el.getAttribute("data-type") !== "tab-button") {
      const curr_target_value = curr_el.getAttribute("data-value");
      const curr_button_type = curr_el.getAttribute("data-type");

      if (curr_button_type === "hier-button") {
        const lvl_to_activate = root_el.querySelector(".dv_button_level[value='" + curr_target_value + "']");
        lvl_to_activate.classList.add("active");
        curr_el = lvl_to_activate.querySelector(".clicked")
      } else {
        console.error("Unknown button type: " + curr_button_type);
        return;
      }
    }

    const tab_target = curr_el.getAttribute("data-value");

    root_el.parentElement.parentElement.querySelector(".dv_tab_container .dv_tab_content[value='" + tab_target + "']").classList.add("active");
    notify_shiny_display_change($(root_el).find(".dv_tab_content[value='" + tab_target + "']"), "shown");
    return (tab_target);
  }

  const on_click = function (e, root_el) {

    const container_id = root_el.id;
    const clicked = e !== null ? e.target : root_el.querySelector(".dv_root_button_level.clicked");

    if (clicked.classList.contains("dv_tab_activate_button")) {
      const unclicked = clicked.parentElement.querySelector(".dv_tab_activate_button.clicked");

      unclicked.classList.remove("clicked");
      clicked.classList.add("clicked");

      // Hide everything
      remove_active_all(root_el);
      // Show clicked
      const active_tab = set_clicked_active(root_el);
      const res = {
        active_tab: active_tab,
        container_id: container_id
      }
      return (res);
    } else {
      return (null);
    }

  }

  const set_tab_by_tab_id = function (tab_id, container_id) {
    const active_tab_id = _set_tab_by_tab_id(tab_id, document.getElementById(container_id));
    Shiny.setInputValue(container_id, active_tab_id)

  }

  const init = function (id) {
    log("Initializing: " + id);


    // Set listeners

    let in_set = function () {

      let root_el = document.getElementById(id);
      let default_tab = root_el?.getAttribute("default-tab");

      let first_tab_button = root_el.querySelector(".dv_tab_activate_button[data-type='tab-button']");
      let first_tab = first_tab_button !== null ? first_tab_button.getAttribute("data-value") : null;

      let active_tab;
      if (default_tab !== null) {
        active_tab = _set_tab_by_tab_id(default_tab, root_el);
      } else if (first_tab !== null) {
        active_tab = _set_tab_by_tab_id(first_tab, root_el);
      } else {
        // No buttons in nav header, no modules in the app
        active_tab = null;
      }

      // Send on start
      if (active_tab !== null) {
        Shiny.setInputValue(id, active_tab)
      }

      // Set listener for button presses
      root_el.addEventListener('click', function (event) {
        const response = on_click(event, root_el);
        if (!response !== null) {
          Shiny.setInputValue(id, response.active_tab)
        }
      });

      Shiny.addCustomMessageHandler("set_active_tab", function (message) {
        set_tab_by_tab_id(message.tab_id, message.id)
      });

      // Call once and remove
      $(document).off('shiny:connected', in_set);

    }

    $(document).on('shiny:connected', in_set);

  }

  const res = {
    init: init,
    set_tab_by_tab_id: set_tab_by_tab_id // Used in testing
  }

  return (res)
})();



const dv_overlay = (function () {

  // let log = console.log;
  let log = function () { return; };

  let C  = {
    ID: "__manager_overlay__",
    MIN_VISIBLE_MS: 500
  }

  let overlay = undefined;
  let show_time = undefined;

  let show_overlay = function(msg) {
    if(overlay) {
      hide_overlay();
    }
    if(!overlay){
      log("Showing overlay"); 
      overlay = document.createElement('div');      
      overlay.id = C.ID;
      overlay.className = "d-flex justify-content-center";
      document.body.appendChild(overlay);
      show_time = Date.now();

      let message = document.createElement("span");
      message.className = "text-white h1";
      message.textContent= msg;
      overlay.appendChild(message);
      let spinner = document.createElement("div");
      spinner.className = "spinner-border text-white m-5";            
      overlay.appendChild(spinner);
    }
  };

  let hide_overlay = function (_) {
    if (overlay) {      
      log("Hiding overlay");    

      const elapsed = Date.now() - show_time;
      const remaining = C.MIN_VISIBLE_MS - elapsed;

      if (remaining > 0) {
        setTimeout(hide_overlay, remaining);
      } else {
        overlay.remove();      
        overlay = undefined;
      }

    } else {
      console.warn(`#${C.ID} not found`)
    }
  };

  Shiny.addCustomMessageHandler("dv_manager_show_overlay", function (message) {
    show_overlay(message.message)
  });

  Shiny.addCustomMessageHandler("dv_manager_hide_overlay", function (_) {
    hide_overlay()
  });

  const res = {
    show: show_overlay,
    hide: hide_overlay
  }

  return (res)
})();

/* Flame graph*/

const dv_flame = (function () {

  let log = console.log;

  let C = {
    HEIGHT: 100,
    GUT: 1,
    MIN_RECT_WIDTH_FOR_TEXT: 18,
    TEXT_MARGIN: 3,
    EVENT_WIDTH :10
  };

  let ro = undefined;
  let cont_el = undefined;
  let data = undefined;
  let hover_listener = false;
  let click_listener = false;
  let svgns = "http://www.w3.org/2000/svg";
  let svg = undefined;
  let zoom = undefined;
  let current_idx = undefined;
  let current_is_event = undefined;

  let detail_id = ".._app_info..-server_init_time_detail";
  let detail_initialized = false;
  let detail_label_el = undefined;
  let detail_start_el = undefined;
  let detail_duration_el = undefined;

  let last_width = 0;

  function init_detail_panel() {
    if (detail_initialized) return;
    detail_initialized = true;
    document.getElementById(detail_id).innerHTML = `
      <div class="card shadow-sm">
        <div class="card-body">
          <h6 class="card-subtitle text-muted mb-1" style="font-size: 11px; text-transform: uppercase; letter-spacing: 0.05em;">Event</h6>
          <h5 class="card-title mb-3" id="dv_flame_detail_label"></h5>
          <div class="d-flex gap-2">            
            <div class="flex-fill bg-light rounded p-2">
              <div class="text-muted" style="font-size: 11px;">Respect to first entry</div>
              <div class="fw-semibold" id="dv_flame_detail_origin"></div>
            </div>
            <div class="flex-fill bg-light rounded p-2">
              <div class="text-muted" style="font-size: 11px;">Duration</div>
              <div class="fw-semibold" id="dv_flame_detail_duration"></div>
            </div>
            <div class="flex-fill bg-light rounded p-2">
              <div class="text-muted" style="font-size: 11px;">Time</div>
              <div class="fw-semibold" id="dv_flame_detail_start"></div>
            </div>
          </div>
        </div>
      </div>`;
    detail_label_el = document.getElementById("dv_flame_detail_label");
    detail_start_el = document.getElementById("dv_flame_detail_start");
    detail_duration_el = document.getElementById("dv_flame_detail_duration");
    detail_dv_flame_detail_origin_el = document.getElementById("dv_flame_detail_origin");
  }

  function update_detail(data, idx) {
    detail_label_el.textContent = data.label_st[idx];    
    detail_start_el.textContent = new Date(data.st[idx]*1000).toString();
    detail_duration_el.textContent = data.duration[idx].toPrecision(3) + "s";
    detail_dv_flame_detail_origin_el.textContent = (data.st[idx] - Math.min(...data.st)).toPrecision(3) + "s";
  }

  Shiny.addCustomMessageHandler("dv_manager_draw_flame_graph", function (message) {
    if (!ro) {
      ro = new ResizeObserver(() => {
        const w = document.getElementById(message.id).getBoundingClientRect().width;
        if (w !== last_width) {
          last_width = w;
          draw_me();
        }
      });
      ro.observe(document.getElementById(message.id));
    }

    if (!hover_listener) {
      hover_listener = true;
      document.getElementById(message.id).addEventListener("mouseover", (e) => {
        const el = e.target;
        if (el.tagName === "rect") {
          let idx = Number(el.getAttribute("idx"));
          let is_event = el.classList.contains("event");
          if (current_idx === idx && current_is_event == is_event) return;

          current_idx = idx;
          current_is_event = is_event;
          init_detail_panel();
          if(is_event) {
            update_detail(data, idx);
          } else {
            update_detail(data, idx);
            
          }          
        }
      });
    }

    // if (!click_listener) {
    //   click_listener = true;
    //   document.getElementById(message.id).addEventListener("click", (e) => {
    //     const el = e.target;
    //     if (el.tagName === "rect") {
    //       zoom = Number(el.getAttribute("idx"));
    //       draw_me();
    //     }
    //   });

    //   document.getElementById(message.id).addEventListener("dblclick", (e) => {
    //     zoom = undefined;
    //     draw_me();
    //   });
    // }\
    data = message.data;
    cont_el = document.getElementById(message.id);
    draw_me();
  });

  let draw_me = function () {
    draw(cont_el, data, zoom);
  };

  let draw = function (el, data, idx_restrict) {

    log("Drawing chart");

    if (svg) svg.remove();
    if (!el.checkVisibility()) return; // Do not redraw if I am not visible

    let create_period = function (st, et, label, depth, idx) {
      let group = document.createElementNS(svgns, 'g');
      let rect = document.createElementNS(svgns, 'rect');
      let rect_x = x_scale(st);
      let rect_width = x_scale(et) - x_scale(st);
      let rect_y = y_scale(depth - 1);
      rect.setAttribute('x', rect_x);
      rect.setAttribute('y', rect_y);
      rect.setAttribute('height', C.HEIGHT);
      rect.setAttribute('width', rect_width);
      rect.setAttribute('idx', idx);
      rect.setAttribute('rx', 5);
      rect.setAttribute('ry', 5);
      rect.setAttribute('class', 'period');

      group.appendChild(rect);

      if (rect_width > C.MIN_RECT_WIDTH_FOR_TEXT) {
        const text_el = document.createElementNS(svgns, "text");
        text_el.textContent = label;
        svg.appendChild(text_el);
        let label_height = text_el.getBBox().height;

        let text_kept_prop = Math.min(1, (rect_width - (C.TEXT_MARGIN * 2)) / text_el.getComputedTextLength());
        svg.removeChild(text_el);
        text_el.textContent = label.slice(0, Math.floor(label.length * text_kept_prop));

        text_el.setAttribute('x', rect_x + C.TEXT_MARGIN);
        text_el.setAttribute('y', rect_y + C.HEIGHT - C.TEXT_MARGIN - label_height);
        text_el.setAttribute('text-anchor', "start");
        text_el.setAttribute('dominant-baseline', "auto");
        text_el.setAttribute('font_size', 13);
        text_el.setAttribute('idx', idx);
        group.appendChild(text_el);
      }

      return (group);
    };

    let create_event = function (st, et, label, depth, idx) {
      let group = document.createElementNS(svgns, 'g');
      let rect = document.createElementNS(svgns, 'rect');
      let rect_x = x_scale(st);      
      let rect_width = C.EVENT_WIDTH;      
      let rect_y = y_scale(depth - 1);
      rect.setAttribute('x', rect_x);
      rect.setAttribute('y', rect_y);
      rect.setAttribute('height', C.HEIGHT);
      rect.setAttribute('width', rect_width);
      rect.setAttribute('idx', idx);
      rect.setAttribute('rx', 5);
      rect.setAttribute('ry', 5);
      rect.setAttribute('class', 'event');

      group.appendChild(rect);

    
      const text_el = document.createElementNS(svgns, "text");
      text_el.textContent = label;
      svg.appendChild(text_el);
      let label_height = text_el.getBBox().height;
      
      svg.removeChild(text_el);
      text_el.textContent = label.slice(0, Math.floor(label.length));

      text_el.setAttribute('x', rect_x + C.TEXT_MARGIN);
      text_el.setAttribute('y', rect_y + C.HEIGHT - C.TEXT_MARGIN - label_height);
      text_el.setAttribute('text-anchor', "start");
      text_el.setAttribute('dominant-baseline', "auto");
      text_el.setAttribute('font_size', 13);
      text_el.setAttribute('idx', idx);
      group.appendChild(text_el);
      

      return (group);
    };

    let time_period_max;
    let time_period_min;
    let base_depth;
    let MIN_X;
    let MAX_X;
    let MAX_Y;

    // if (zoom !== undefined) {
    //   time_period_max = et[idx_restrict];
    //   time_period_min = data.st[idx_restrict];
    //   base_depth = Number(data.depth[idx_restrict]);
    //   MIN_X = time_period_min;
    //   MAX_X = time_period_max;
    //   MAX_Y = Math.max(...data.depth) + 1;
    // } else {
      
    // }

    time_period_max = Math.max(...data.et);
    time_period_min = Math.min(...data.st);
    base_depth = 1;          
    MIN_X = time_period_min;
    MAX_X = time_period_max;
    MAX_Y = Math.max(...data.depth);

    let x_scale = function (x_coord) {
      return (((x_coord - MIN_X) / (MAX_X - MIN_X)) * (SIZE.width - C.EVENT_WIDTH)) + 0;
    };

    let y_scale = function (y_coord) {
      return y_coord * C.HEIGHT;
    };

    svg = document.createElementNS(svgns, "svg");
    el.appendChild(svg);

    svg.setAttribute('class', 'dv_flame');
    svg.setAttribute('width', '100%');
    svg.setAttribute('height', MAX_Y * C.HEIGHT);
    svg.setAttributeNS("http://www.w3.org/2000/xmlns/", "xmlns:xlink", "http://www.w3.org/1999/xlink");

    const SIZE = el.getBoundingClientRect();

    const fragment = document.createDocumentFragment();

    for (let idx = 0; idx < data.st.length; idx++) {
      if (data.st[idx] >= time_period_min && data.et[idx] <= time_period_max) {
        if (data.duration[idx] > 0) {
          fragment.appendChild(create_period(data.st[idx], data.et[idx], data.label_st[idx], data.depth[idx] - (base_depth - 1), idx));
        } else {
          fragment.appendChild(create_event(data.st[idx], data.et[idx], data.label_st[idx], MAX_Y-1, idx));
        }
      }
    }

    svg.appendChild(fragment);
  };
})();

$(document).ready(function () {  
  $("div.dv-sidebar-container input[type=checkbox][id=click]").change(function (event) {
    if ($(event.target).is(":checked")) {
      $(".dv-sidebar-container").removeClass("grid-resize");
    } else {
      $(".dv-sidebar-container").addClass("grid-resize");
    }
  });
});





