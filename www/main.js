
// Functie om sidebar om hidden te zetten
/*
$( document ).ready(function() {

  // Verberg sidebar menu links
  //$(".sidebar-menu #tablist").parent().hide();
  // Verberg toggle sidebar links
  // TODO Toggle sidebar is bij inladen app nog wel zichtbaar. Misschien kan deze nog eerder worden
  // gehide (direct na aanmaken een JS-functie?)
  //document.getElementsByClassName('sidebar-toggle')[0].style.visibility = 'hidden';

})
*/
// Functie om iets te doen nadat een element is verschenen. Dit is handig omdat JS-code
// niet per se direct verschijnt bij document ready
/*
function waitForElementToDisplay(selector, callback, checkFrequencyInMs, timeoutInMs) {
  var startTimeInMs = Date.now();
  (function loopSearch() {
    if (document.querySelector(selector) != null) {
      // Voer callback uit
      callback();
      return;
    }
    else {
      setTimeout(function () {
        var now = Date.now();
        var end = startTimeInMs + timeoutInMs;
        if (now > end) {
          return;
        }
        loopSearch();
      }, checkFrequencyInMs);
    }
  })();
}
*/
// Selecteer eerste tab
// TODO: Meer abstract maken, controleren welke tab is geselecteerd in sidebar-menu in en vervolgens
// bijbehorende content hier activeren

/*

waitForElementToDisplay('#shiny-tab-tab1',function(){
  var element = document.querySelector('#shiny-tab-tab1');
  element.setAttribute('aria-hidden', 'false');
  element.setAttribute('tabindex', '0');
  element.classList.add('active');
},
1000,9000);
*/
