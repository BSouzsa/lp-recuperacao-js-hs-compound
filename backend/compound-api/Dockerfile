# Imagem base com a mesma versão de GHC que o stack.yaml espera
FROM haskell:9.10.3

# Diretório dentro do container
WORKDIR /app

# Copia o repo inteiro
COPY . .

# Entra na pasta do backend
WORKDIR /app/backend/compound-api

# Diz pro Stack usar o GHC do sistema (o da imagem) e NÃO tentar instalar outro
RUN stack config set system-ghc --global true \
    && stack build --system-ghc

# Expor porta (documentação)
EXPOSE 8080

# Rodar a API usando o GHC do sistema
CMD ["stack", "run", "--system-ghc"]
