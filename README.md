# Ollama Summarizer

An Erlang library for summarizing web content and HTML using the Ollama API.

[![Hex.pm](https://img.shields.io/hexpm/v/ollama_summarizer.svg)](https://hex.pm/packages/ollama_summarizer)
[![Hex Docs](https://img.shields.io/badge/hex-docs-blue.svg)](https://hexdocs.pm/ollama_summarizer)
[![License](https://img.shields.io/badge/license-Apache%202.0-blue.svg)](LICENSE)

## Features

- 📄 Summarize content from URLs
- 🔧 Summarize HTML content directly
- ⚙️ Configurable via environment variables or runtime configuration
- 🌐 Support for custom Ollama endpoints and models
- 📝 Customizable prompt templates

## Installation

Add to your `rebar.config`:

```erlang
{deps, [
    {ollama_summarizer, "0.1.0"}}
]}.
```

Or with hex (once published):

```erlang
{deps, [ollama_summarizer]}.
```

## Quick Start

```erlang
% Start your Erlang shell
$ rebar3 shell

% Summarize a webpage
1> ollama_summarizer:summarize_url("https://example.com").
{ok, <<"This is a summary of the webpage content...">>}

% Summarize HTML directly
2> Html = "<html><body><h1>Title</h1><p>Some content here</p></body></html>",
2> ollama_summarizer:summarize_html(Html).
{ok, <<"Summary of the HTML content...">>}

% Print the result nicely
3> Result = ollama_summarizer:summarize_url("https://news.ycombinator.com"),
3> ollama_summarizer:print_result(Result).
```

## Configuration

### Environment Variables

Set these environment variables to override defaults:

```bash
export OLLAMA_ENDPOINT="http://localhost:11434/api/generate"  # Default
export OLLAMA_MODEL="phi3"                                   # Default  
export OLLAMA_PROMPT="Résume ce contenu en français en quelques phrases claires et concises :\n\n~s"  # Default
```

### Runtime Configuration

```erlang
% Custom configuration
Config = #{
    endpoint => "http://my-ollama-server:11434/api/generate",
    model => <<"llama2">>,
    prompt_template => "Summarize this content in English:\n\n~s"
},

% Use custom config
ollama_summarizer:summarize_url("https://example.com", Config).

% Modify default config partially
BaseConfig = ollama_summarizer:get_env_config(),
MyConfig = BaseConfig#{model => <<"codellama">>},
ollama_summarizer:summarize_html(Html, MyConfig).
```

## API Reference

### Main Functions

#### `summarize_url/1,2`
```erlang
summarize_url(Url) -> {ok, Summary} | {error, Reason}.
summarize_url(Url, Config) -> {ok, Summary} | {error, Reason}.
```
Fetches and summarizes content from a URL.

#### `summarize_html/1,2`
```erlang  
summarize_html(Html) -> {ok, Summary} | {error, Reason}.
summarize_html(Html, Config) -> {ok, Summary} | {error, Reason}.
```
Summarizes HTML content directly.

#### `summarize_with_ollama/1,2`
```erlang
summarize_with_ollama(Text) -> {ok, Summary} | {error, Reason}.
summarize_with_ollama(Text, Config) -> {ok, Summary} | {error, Reason}.
```
Summarizes plain text content.

### Configuration Functions

#### `default_config/0`
```erlang
default_config() -> Config.
```
Returns hardcoded default configuration.

#### `get_env_config/0`
```erlang
get_env_config() -> Config.
```
Returns configuration with environment variable overrides.

### Utility Functions

#### `print_result/1`
```erlang
print_result(Result) -> ok | error.
```
Pretty prints summarization results to stdout.

## Configuration Map

```erlang
Config = #{
    endpoint => "http://localhost:11434/api/generate",  % Ollama API endpoint
    model => <<"phi3">>,                               % Model name as binary
    prompt_template => "Summarize this:\n\n~s"         % Prompt with ~s placeholder
}.
```

## Prerequisites

- Erlang/OTP 21+
- Running Ollama instance
- `inets` application for HTTP requests

## Dependencies

This library requires:
- `inets` (part of OTP) for HTTP client

Add to your `rebar.config`:

```erlang
{deps, [
    ollama_summarizer
]}.
```

## Examples

### Basic Usage

```erlang
% Summarize Hacker News
Result = ollama_summarizer:summarize_url("https://news.ycombinator.com"),
ollama_summarizer:print_result(Result).

% Summarize with different model
Config = #{model => <<"llama2">>},
ollama_summarizer:summarize_url("https://example.com", Config).
```

### Advanced Usage

```erlang
% Custom prompt for technical content
TechConfig = #{
    model => <<"codellama">>,
    prompt_template => "Provide a technical summary of this code/content:\n\n~s"
},

% Summarize GitHub README
ollama_summarizer:summarize_url("https://raw.githubusercontent.com/user/repo/main/README.md", TechConfig).
```

### Error Handling

```erlang
case ollama_summarizer:summarize_url("https://invalid-url.com") of
    {ok, Summary} ->
        io:format("Summary: ~s~n", [Summary]);
    {error, {fetch_failed, Reason}} ->
        io:format("Failed to fetch URL: ~p~n", [Reason]);
    {error, {ollama_error, StatusCode, Body}} ->
        io:format("Ollama API error ~p: ~s~n", [StatusCode, Body]);
    {error, Reason} ->
        io:format("Other error: ~p~n", [Reason])
end.
```

## Development

```bash
# Clone the repository
git clone https://github.com/roquess/ollama_summarizer.git
cd ollama_summarizer

# Compile
rebar3 compile

# Run tests (if available)
rebar3 eunit

# Start shell for testing
rebar3 shell
```

## Contributing

1. Fork the repository
2. Create a feature branch
3. Make your changes
4. Add tests if applicable
5. Submit a pull request

## License

MIT License - see LICENSE file for details.

## Changelog

### 0.1.0
- Initial release
- Basic URL and HTML summarization
- Environment variable configuration
- Custom configuration support
