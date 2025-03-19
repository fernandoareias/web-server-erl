# Servidor Web em Erlang

Um servidor web de alto desempenho implementado em Erlang, com suporte a processamento não-bloqueante de requisições HTTP.

## Características

- Processamento não-bloqueante de requisições HTTP
- Arquitetura baseada em supervisão para alta disponibilidade
- Suporte a múltiplas conexões simultâneas
- Cache de arquivos estáticos
- Métricas de desempenho em tempo real
- Configuração flexível de timeouts e parâmetros TCP

## Requisitos

- Erlang/OTP 24 ou superior
- Rebar3

## Instalação

1. Clone o repositório:
```bash
git clone https://github.com/seu-usuario/web-server-erl.git
cd web-server-erl
```

2. Compile o projeto:
```bash
make compile
```

## Uso

1. Inicie o servidor:
```bash
make shell
```

Ou simplesmente:
```bash
make run
```

O servidor iniciará na porta padrão 8091. Para usar uma porta diferente, configure no arquivo `config/sys.config`.

## Configuração

As principais configurações podem ser ajustadas em `config/sys.config`:

```erlang
[
  {web_server, [
    {http_port, 8091},
    {max_connections, 1024},
    {cache_enabled, true},
    {log_level, debug}
  ]}
].
```

## Estrutura do Projeto

```
web-server-erl/
├── apps/
│   └── web_server/              # Aplicação OTP principal
│       ├── src/
│       │   ├── core/            # Aplicação e supervisor
│       │   ├── http/            # Manipuladores HTTP
│       │   ├── tcp/             # Componentes TCP
│       │   ├── metrics/         # Métricas e telemetria
│       │   └── utils/           # Utilitários 
│       ├── include/             # Definições comuns
│       ├── priv/                # Recursos estáticos
│       └── test/                # Testes unitários e de integração
├── config/                      # Configuração do sistema
├── scripts/                     # Scripts de automação
│   ├── clean.sh                 # Limpeza de arquivos
│   ├── check_style.sh           # Verificação de estilo
│   └── release.sh               # Criação de releases
├── tools/                       # Ferramentas adicionais
├── docs/                        # Documentação
├── test/                        # Testes de alto nível
└── load-tests/                  # Testes de carga
```

## Comandos Disponíveis

```bash
make compile        # Compila o projeto
make clean          # Limpa os arquivos compilados
make test           # Executa os testes unitários e de integração
make shell          # Inicia um shell Erlang com o projeto
make release        # Cria um release do projeto
make run            # Compila e inicia o projeto
make docs           # Gera a documentação
make check          # Verifica o estilo do código
```

## Testes

Execute os testes unitários:
```bash
make test
```

Execute os testes de carga:
```bash
cd load-tests
k6 run test-script.js
```

## Métricas

O servidor expõe métricas em tempo real através do módulo `metrics`:

- Conexões ativas
- Requisições por segundo
- Tempo médio de resposta
- Uso de memória
- Cache hits/misses

## Contribuindo

1. Fork o projeto
2. Crie sua branch de feature (`git checkout -b feature/AmazingFeature`)
3. Commit suas mudanças (`git commit -m 'Add some AmazingFeature'`)
4. Push para a branch (`git push origin feature/AmazingFeature`)
5. Abra um Pull Request

## Licença

Este projeto está licenciado sob a MIT License - veja o arquivo [LICENSE.md](LICENSE.md) para detalhes.

## Autor

Fernando Areias - [@nando.calheirosx](https://github.com/seu-usuario)

## Agradecimentos

- Equipe Erlang/OTP
- Comunidade Erlang
- Contribuidores do projeto

# First request
<img width="1440" alt="Image" src="https://github.com/user-attachments/assets/e36711d3-d133-4970-ae99-4eda42cda7d1" />