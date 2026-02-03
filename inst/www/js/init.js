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
})()



const dv_overlay = (function () {

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
      console.log("Showing overlay"); 
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
      console.log("Hiding overlay");    

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
})()

$(document).ready(function () {  
  $("div.dv-sidebar-container input[type=checkbox][id=click]").change(function (event) {
    if ($(event.target).is(":checked")) {
      $(".dv-sidebar-container").removeClass("grid-resize");
    } else {
      $(".dv-sidebar-container").addClass("grid-resize");
    }
  });
});
