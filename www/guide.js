document.addEventListener('toggle', function (event) {
  const item = event.target;
  if (item.matches && item.matches('.tool') && item.open) {
    document.querySelectorAll('.tool').forEach(other => {
      if (other !== item) other.open = false;
    });
  }
}, true);
document.addEventListener('click', function (event) {
  const link = event.target.closest('.open-tool');
  if (!link) return;
  event.preventDefault();
  const target = document.getElementById(link.getAttribute('href').slice(1));
  if (target) {
    target.open = true;
    target.querySelector('summary').focus();
    target.scrollIntoView({block: 'start'});
  }
});
$(function () {
  Shiny.addCustomMessageHandler('guide-focus', function (message) {
    requestAnimationFrame(function () {
      document.querySelectorAll('.tool').forEach(item => item.open = false);
      document.getElementById('guided-main').focus({preventScroll: true});
      window.scrollTo({top: 0, behavior: 'instant'});
    });
  });
});
