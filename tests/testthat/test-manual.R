# nolint start


test_that(
  vdoc[["add_spec"]]("dv.manager should provide SSO login", c(specs$azure_options, specs$SSO_login_option, specs$AzureAuth_integration, specs$SSO_authentication_option)),
  {
    skip("Untestable from development/Manual testing")
  }
)

# nolint end
