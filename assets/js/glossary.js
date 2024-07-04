document.addEventListener('DOMContentLoaded', function() {
    const terms = document.querySelectorAll('.glossary-term');
    terms.forEach(function(term) {
      term.addEventListener('mouseenter', function() {
        const popup = document.createElement('div');
        popup.className = 'glossary-popup';
        popup.innerHTML = term.getAttribute('data-definition');
        document.body.appendChild(popup);
  
        const rect = term.getBoundingClientRect();
        popup.style.left = rect.left + 'px';
        popup.style.top = (rect.top - popup.offsetHeight - 10) + 'px';
      });
  
      term.addEventListener('mouseleave', function() {
        const popup = document.querySelector('.glossary-popup');
        if (popup) {
          popup.remove();
        }
      });
    });
  });