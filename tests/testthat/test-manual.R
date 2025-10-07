# nolint start


test_that(
  vdoc[["add_spec"]]("dv.manager should provide SSO login", c(specs$AUTHENTICATION$SSO_ENABLED, specs$AUTHENTICATION$SSO_DISABLED, specs$AUTHENTICATION$AZURE_OPTIONS_REQUIRED, specs$AUTHENTICATION$AZURE_OPTIONS_NULL, specs$AUTHENTICATION$SSO_ENABLED)),
  {
    skip("Untestable from development/Manual testing")
  }
)

# nolint end
