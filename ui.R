ui <- navbarPage(
  theme = "css/bcgov.css",
  title = "BC Housing Market",
  source(file.path("ui", "home.R"),  local = TRUE)$value,
  source(file.path("ui", "census.R"),  local = TRUE)$value,
  source(file.path("ui", "ptt.R"),  local = TRUE)$value,
  source(file.path("ui", "about.R"),  local = TRUE)$value,
  tabPanel("Feedback", id = "feedback"),
  tags$head(tags$script(src="js/bcgov.js")),
  tags$head(tags$link(rel="shortcut icon", href="/images/favicon.ico")),
  footer = HTML('
<div id="footer">
  <img src="/images/back-to-top.png" alt="Back to top" title="Back to top" class="back-to-top footer-overlap" style="bottom: 10px; display: inline;">
  <div id="footerWrapper">
    <div id="footerAdminSection">
      <div id="footerAdminLinksContainer" class="container">
        <div id="footerAdminLinks" class="row">
          <ul class="inline">
            <li data-order="0">
              <a href="/" target="_self">Home</a>
            </li>
            <li data-order="1">
              <a href="#tab-3898-1" target="_self" class="about-project">About</a>
            </li>
            <li data-order="2">
              <a href="//www2.gov.bc.ca/gov/content/home/disclaimer" target="_self">Disclaimer</a>
            </li>
            <li data-order="3">
              <a href="//www2.gov.bc.ca/gov/content/home/privacy" target="_self">Privacy</a>
            </li>
            <li data-order="4">
              <a href="//www2.gov.bc.ca/gov/content/home/accessibility" target="_self">Accessibility</a>
            </li>
            <li data-order="5">
              <a href="//www2.gov.bc.ca/gov/content/home/copyright" target="_self">Copyright</a>
            </li>
            <li data-order="6">
              <a href="//www2.gov.bc.ca/gov/content/home/contact-us" target="_self">Contact Us</a>
            </li>
          </ul>
        </div>
      </div>
    </div>
  </div>
</div>')
)
