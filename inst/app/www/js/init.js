// handle the dataset info bar
$(window).on("load", function () {
  $(".grid_page_date").detach().appendTo($("#__tabset_0__").parent());
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
