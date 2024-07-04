document.addEventListener('DOMContentLoaded', (event) => {
    const toggleDiv = document.getElementById('toggle-div')

    toggleDiv.style.display = document.querySelectorAll('.solutions').length > 0 ? 'flex' : 'none'

    const toggleSwitch = document.getElementById('toggle-switch');

    toggleSwitch.addEventListener('change', () => {
      document.querySelectorAll('.solutions').forEach(element => {
        element.style.display = toggleSwitch.checked ? 'block' : 'none'
      });
    });
});
