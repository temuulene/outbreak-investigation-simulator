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
  Shiny.addCustomMessageHandler('dialogue-busy', function (message) {
    const button = document.getElementById('ask');
    if (!button) return;
    if (!button.dataset.readyLabel) button.dataset.readyLabel = button.textContent;
    button.disabled = message.busy;
    button.setAttribute('aria-busy', String(message.busy));
    button.textContent = message.busy ? 'Preparing reply…' : button.dataset.readyLabel;
  });
  const chat = document.getElementById('chat');
  if (chat) {
    let scheduled = false;
    const revealLatestAnswer = function () {
      if (scheduled) return;
      scheduled = true;
      requestAnimationFrame(function () {
        scheduled = false;
        const answer = chat.querySelector('.chat-answer:last-child');
        if (!answer || !answer.getClientRects().length) return;
        answer.scrollIntoView({
          block: answer.offsetHeight > answer.parentElement.clientHeight ? 'start' : 'nearest',
          inline: 'nearest',
          behavior: 'instant'
        });
      });
    };
    new MutationObserver(revealLatestAnswer).observe(chat, {childList: true, subtree: true});
    revealLatestAnswer();
  }
  Shiny.addCustomMessageHandler('guide-focus', function (message) {
    requestAnimationFrame(function () {
      document.querySelectorAll('.tool').forEach(item => item.open = false);
      document.getElementById('guided-main').focus({preventScroll: true});
      window.scrollTo({top: 0, behavior: 'instant'});
    });
  });
});
