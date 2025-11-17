# Calculadora de Juros Compostos (JS + Haskell)

Atividade de recuperação da disciplina **Linguagens de Programação**.

Aplicação dividida em:

- **Backend:** Haskell (Stack, Scotty, Aeson) expondo a rota `/api/compound`
- **Frontend:** HTML + JavaScript consumindo a API via JSON

## Fórmula

A API calcula o montante final usando:

A = P * (1 + r / n)^(n * t)

- P: valor inicial (principal)
- r: taxa anual em decimal (ex.: 0.12 para 12% a.a.)
- n: número de capitalizações por ano
- t: tempo em anos (pode ser decimal)

## API (Backend Haskell)

### Endpoint

`POST /api/compound`

### Request (JSON)

```json
{
  "principal": 1000.0,
  "rate": 0.12,
  "timesPerYear": 12,
  "years": 2
}
