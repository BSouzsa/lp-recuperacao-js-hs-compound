// Enquanto estiver testando local, o backend é o localhost:8080
const API_URL = "http://localhost:8080/api/compound";

const form = document.getElementById("compound-form");
const resultDiv = document.getElementById("result");
const errorDiv = document.getElementById("error");

const formatBRL = (value) =>
  new Intl.NumberFormat("pt-BR", {
    style: "currency",
    currency: "BRL",
  }).format(value);

form.addEventListener("submit", async (event) => {
  event.preventDefault();
  resultDiv.textContent = "";
  errorDiv.textContent = "";

  const principal = parseFloat(document.getElementById("principal").value);
  const rate = parseFloat(document.getElementById("rate").value);
  const timesPerYear = parseInt(
    document.getElementById("timesPerYear").value,
    10
  );
  const years = parseFloat(document.getElementById("years").value);

  // Validação básica no frontend
  if (isNaN(principal) || principal <= 0) {
    errorDiv.textContent =
      "Informe um valor inicial (principal) maior que zero.";
    return;
  }

  if (isNaN(rate)) {
    errorDiv.textContent =
      "Informe uma taxa anual (rate) válida em decimal, ex: 0.12 para 12% a.a.";
    return;
  }

  if (isNaN(timesPerYear) || timesPerYear < 1) {
    errorDiv.textContent =
      "Informe o número de capitalizações por ano (n) maior ou igual a 1.";
    return;
  }

  if (isNaN(years) || years <= 0) {
    errorDiv.textContent =
      "Informe o tempo em anos (t) maior que zero (pode ser decimal).";
    return;
  }

  const payload = {
    principal,
    rate,
    timesPerYear,
    years,
  };

  try {
    const response = await fetch(API_URL, {
      method: "POST",
      headers: {
        "Content-Type": "application/json",
      },
      body: JSON.stringify(payload),
    });

    const data = await response.json();

    if (!response.ok) {
      errorDiv.textContent =
        data.error || "Erro ao calcular. Verifique os valores.";
      return;
    }

    const amount = data.amount;
    resultDiv.textContent = `Montante final: ${formatBRL(amount)}`;
  } catch (err) {
    console.error(err);
    errorDiv.textContent =
      "Erro de comunicação com o servidor. Tente novamente mais tarde.";
  }
});
