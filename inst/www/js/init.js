// handle the dataset info bar
$(window).on("load", function () {
  $(".grid_page_date").detach().appendTo($("#main_tab_panel").parent());
});

//toggle sidebar resize
$(document).ready(function () {
  $("input[type=checkbox][id=click]").change(function () {
    if ($(this).is(":checked")) {
      $(".sidebar-container").removeClass("grid-resize");
    } else {
      $(".sidebar-container").addClass("grid-resize");
    }
  });
});

// const dv_manager = {
//   hide_filters: function(button_id) {
//     var button = document.getElementById(button_id);
//     var tag = button.querySelectorAll("span")[1];
//     var container = button.nextElementSibling;
//     var hidden = container.classList.toggle("dv_hidden");
//     var controls_container = container.querySelectorAll(":scope > div")[2];
//     var active_filters = controls_container.querySelectorAll(":scope > div").length;
    
//     tag.textContent=active_filters;

//     if (hidden) {
//       tag.classList.remove("dv_hidden");      
//     } else {
//       tag.classList.add("dv_hidden");
//     }
//   }
// };

