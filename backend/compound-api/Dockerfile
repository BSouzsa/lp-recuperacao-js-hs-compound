# Imagem base com GHC + Stack
FROM haskell:9.4

# Diretório dentro do container
WORKDIR /app

# Copia o repo inteiro
COPY . .

# Entra na pasta do backend
WORKDIR /app/backend/compound-api

# Instala deps e compila
RUN stack setup && stack build

# Expor porta (documentação)
EXPOSE 8080

# Rodar a API
CMD ["stack", "run"]