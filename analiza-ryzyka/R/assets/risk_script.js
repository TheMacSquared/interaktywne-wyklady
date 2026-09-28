// Pytania kontrolne w toku tekstu (risk_check w R/risk_block.R).
// Obsługa delegowana: działa także dla treści wstawionej później przez renderUI.
(function () {
  if (window.__lcRiskCheck) return;
  window.__lcRiskCheck = true;

  document.addEventListener('change', function (event) {
    var input = event.target;
    if (!input.matches || !input.matches('.lc-check input[type="radio"]')) return;
    var box = input.closest('.lc-check');
    var correct = input.value === box.getAttribute('data-correct');
    var feedback = box.querySelector('.lc-check-feedback');
    var explanation = box.querySelector('.lc-check-explanation');
    var hint = input.getAttribute('data-hint');

    box.classList.toggle('is-correct', correct);
    box.classList.toggle('is-wrong', !correct);
    feedback.textContent = correct
      ? 'Dobrze.'
      : 'Jeszcze nie. ' + (hint || 'Wróć do poprzedniego akapitu i spróbuj ponownie.');
    explanation.hidden = !correct;
  });
})();
