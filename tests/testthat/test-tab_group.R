# app ----

local({
  skip_if_not_running_shiny_tests()
  skip_if_suspect_check()

  app_expr <- rlang::quo({
    dv.manager:::run_mock_app_tab_group()
  })
  root_app <- start_app_driver(app_expr)
  test_that("tab_group allows grouping of modules" |>
    vdoc[["add_spec"]](c(specs$tab_group$group_modules)), {
    app <- shinytest2::AppDriver$new(root_app$get_url())

    app$run_js("dv_tab.set('mod2')")

    expect_equal(
      app$get_js("$('#__button_container__ .dv_child_button_level.active').attr('value')"),
      "__tabset_1__"
    )

    expect_equal(
      app$get_js("$('.dv_tab_container .dv_tab_content.active').attr('value')"),
      "mod2"
    )

    expect_equal(
      app$get_value(input = "__button_container__"),
      "mod2"
    )

    v2 <- app$get_values(output = "mod2-text")[["output"]][["mod2-text"]]
    expect_equal(
      v2, "306"
    )
  })

  test_that("tab_group allows nesting of modules" |>
    vdoc[["add_spec"]](c(specs$tab_group$allows_nesting)), {
    app <- shinytest2::AppDriver$new(root_app$get_url())

    app$run_js("dv_tab.set('mod4')")

    app$wait_for_idle()

    active_tabs <- app$get_js("
        (function(){
            let res = {};
          try{
            let al = $('#__button_container__ .dv_child_button_level.active');
            res.length = al.length;
            res.value = [];
            for(let idx = 0; idx < al.length; ++idx) {
              console.lo
              res.value.push($(al[idx]).attr('value'))
            }
          } catch(error) {
            res.error = error;
          }
          
          return(res);
        })()        
      ")

    expect_equal(
      active_tabs$length, 2      
    )

    expect_equal(
      active_tabs$value[[1]], "__tabset_1__"
    )

    expect_equal(
      active_tabs$value[[2]], "__tabset_2__"
    )

    expect_equal(
      app$get_js("$('.dv_tab_container .dv_tab_content.active').attr('value')"),
      "mod4"
    )

    expect_equal(
      app$get_value(input = "__button_container__"),
      "mod4"
    )


    v4 <- app$get_values(output = "mod4-text")[["output"]][["mod4-text"]]
    expect_equal(
      v4, "306"
    )
  })

  test_that("tab_group output of grouped modules can be accesed by other modules" |>
    vdoc[["add_spec"]](c(specs$tab_group$output_accesible)), {
    app <- shinytest2::AppDriver$new(root_app$get_url())

    # Switch to Module tab
    app$run_js("dv_tab.set('mod_rec_2')")    
    app$wait_for_idle()
    app$run_js("dv_tab.set('mod_rec_1')")    
    app$wait_for_idle()

    v1 <- app$get_values(output = TRUE)[["output"]][["mod_rec_1-output"]]
    v2 <- app$get_values(output = TRUE)[["output"]][["mod_rec_2-output"]]

    expect_equal(v1, "a")
    expect_equal(v2, "1")
  })


  #
  test_that("tab_group other modules can switch into nested tabs" |>
    vdoc[["add_spec"]](c(specs$tab_group$allows_switching)), {
    app <- shinytest2::AppDriver$new(root_app$get_url())

    # Switch to Module tab
    app$run_js("dv_tab.set('mod_switch1')")    
    app$wait_for_idle()
    app$click("mod_switch1-switch")
    app$wait_for_idle()

expect_equal(
      app$get_js("$('.dv_tab_container .dv_tab_content.active').attr('value')"),
      "mod5"
    )
  })
})
