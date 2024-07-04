document.addEventListener('DOMContentLoaded', () => {
  document.querySelectorAll('.spoiler').forEach(spoiler => {
    spoiler.title = 'Click to reveal spoiler';
    const old = spoiler.innerHTML;
    spoiler.textContent = 'click to reveal spoiler';
    spoiler.addEventListener('click', () => {
      spoiler.innerHTML = old;
      spoiler.dataset.revealed = true;
    });
  });
});
