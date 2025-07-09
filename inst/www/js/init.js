// handle the dataset info bar
const dv_tab = (function () {

  // This code assumes there will be a single instance of a dv_button_container
  // Lazy implementation once the need of several instances is found
  // Easy implementation just pass the container id across functions


  // If it is not notified, because of lazy evaluation, dynamic elements and outputs are not displayed
  // even when they are visible in the client
  const notify_shiny_display_change = function($el, visibility) {
    $el.trigger(visibility);
  };

  const _set_tab_by_tab_id = function(tab_id) {
    const container_id = document.querySelector(".dv_button_container").id;
    let target_tab_element = document.querySelector(".dv_button_container .dv_tab_activate_button[data-value='"+tab_id+"']");
    if(target_tab_element === null) {
      console.error("tab_id:'" + tab_id + "' not found in button set");
      return(false);
    }

    let next_element = tab_id;
    let clicked;
    
    do {
      clicked = document.querySelector(".dv_button_container .dv_tab_activate_button[data-value='"+next_element+"']");
      const unclicked = clicked.parentElement.querySelector(".dv_tab_activate_button.clicked");

      // Apply changes elements
      unclicked.classList.remove("clicked");
      clicked.classList.add("clicked");      

      // Look for next parent
      next_element = clicked.parentElement.getAttribute("value");            
    } while(!clicked.parentElement.classList.contains("dv_root_button_level"));

    remove_active_all();    
    const active_tab = set_clicked_active();
    const res = {
      active_tab : active_tab,
      container_id : container_id
    }
    return(res);    
  }

  const remove_active_all = function () {
    const button_levels_to_deactivate = document.querySelectorAll(".dv_button_container .dv_child_button_level");
    for (let idx = 0; idx < button_levels_to_deactivate.length; ++idx) {
      const current_node = button_levels_to_deactivate[[idx]];
      current_node.classList.remove("active");
    }

    const tabs_to_deactivate = document.querySelectorAll(".dv_tab_container .dv_tab_content");
    for (let idx = 0; idx < tabs_to_deactivate.length; ++idx) {
      const current_node = tabs_to_deactivate[[idx]];
      current_node.classList.remove("active");      
      notify_shiny_display_change($(current_node), "hidden");
    }
  }

  const set_clicked_active = function () {
    let curr_el = document.querySelector(".dv_button_container .dv_root_button_level .dv_tab_activate_button.clicked");
    let container_id = document.querySelector(".dv_button_container");

    while (curr_el.getAttribute("data-type") !== "tab-button") {
      const curr_target_value = curr_el.getAttribute("data-value");
      const curr_button_type = curr_el.getAttribute("data-type");

      if (curr_button_type === "hier-button") {
        const lvl_to_activate = document.querySelector(".dv_button_level[value='" + curr_target_value + "']");
        lvl_to_activate.classList.add("active");
        curr_el = lvl_to_activate.querySelector(".clicked")
      } else {
        console.error("Unknown button type: " + curr_button_type);
        return;
      }
    }

    const tab_target = curr_el.getAttribute("data-value");    
    document.querySelector(".dv_tab_container .dv_tab_content[value='" + tab_target + "']").classList.add("active");
    notify_shiny_display_change($(".dv_tab_container .dv_tab_content[value='" + tab_target + "']"), "shown");    
    return(tab_target);
  }

  const on_click = function (e) {

    const container_id = document.querySelector(".dv_button_container").id;
    const clicked = e !== null ? e.target : document.querySelector(".dv_button_container .dv_root_button_level.clicked");

    if (clicked.classList.contains("dv_tab_activate_button")) {
      const unclicked = clicked.parentElement.querySelector(".dv_tab_activate_button.clicked");

      unclicked.classList.remove("clicked");
      clicked.classList.add("clicked");

      // Hide everything
      remove_active_all();
      // Show clicked
      const active_tab = set_clicked_active();
      const res = {
        active_tab : active_tab,
        container_id : container_id
      }
      return(res);
    } else {
      return(null);
    }

  }

  const set_tab_by_tab_id = function(tab_id) {

    const response = _set_tab_by_tab_id(tab_id);
    Shiny.setInputValue(response.container_id, response.active_tab)

  }

  const on_init = function () {
    let default_tab = document.querySelector(".dv_button_container").getAttribute("default-tab");
    if(default_tab === null) {
      default_tab = document.querySelector(".dv_button_container .dv_tab_activate_button[data-type='tab-button']").getAttribute("data-value");
    }
    const active_tab = _set_tab_by_tab_id(default_tab);    
    return(active_tab);

  }

  const init = function () {

    // Set listeners

    let in_set = function () {

      // Send on start
      const response = on_init();      
      if (!response !== null) {
        Shiny.setInputValue(response.container_id, response.active_tab)
      }

      // Set listener for button presses
      document.getElementById('__button_container__')
        .addEventListener('click', function (event) {
          const response = on_click(event);
          if (!response !== null) {
            Shiny.setInputValue(response.container_id, response.active_tab)
          }
        });

      Shiny.addCustomMessageHandler("set_active_tab", function(message) {
        set_tab_by_tab_id(message.tab_id)
      });

      // Call once and remove    
      $(document).off('shiny:connected', in_set);

    }

    $(document).on('shiny:connected', in_set);

  }

  const res = {
    init: init,
    set: set_tab_by_tab_id // Used in testing
  }

  return (res)
})()

dv_tab.init()

$(document).ready(function () {
  //toggle sidebar resize  
  $("input[type=checkbox][id=click]").change(function () {
    if ($(this).is(":checked")) {
      $(".sidebar-container").removeClass("grid-resize");
    } else {
      $(".sidebar-container").addClass("grid-resize");
    }
  });
});
