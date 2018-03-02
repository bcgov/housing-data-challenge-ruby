$(document).ready(function() {
  // Explore population
  $('.explore-population').on('click', function() {
    $('a[data-value="Census Topics"]').click();
    setTimeout(findSpan, 500, "Population");
  });

  $('.explore-housing').on('click', function() {
    $('a[data-value="Census Topics"]').click();
    setTimeout(findSpan, 500, "Housing");
  });

  $('.explore-mobility').on('click', function() {
    $('a[data-value="Census Topics"]').click();
    setTimeout(findSpan, 500, "Mobility");
  });

  $('.explore-stir').on('click', function() {
    $('a[data-value="Census Topics"]').click();
    setTimeout(findSpan, 500, "STIR");
  });

  // Explore PTT
  $('.explore-ptt').on('click', function() {
    $('a[data-value="Property Transfer Tax"]').click();
    //setTimeout(findSpan, 500, "Population");
  });
});

function findSpan(spanLabel) {
  var spans = $('.leaflet-control-layers-base').find('span');
  $.each(spans, function(index) {
    if ($(this).text().trim() == spanLabel) {
      $(this).prev().click();
    }
  });
}
