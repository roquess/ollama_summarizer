-module(ollama_summarizer).

%% @doc Ollama Summarizer Library
%% This library provides functions to summarize web content and HTML using Ollama API.
%% It supports both default configuration and custom configuration per request.
%% Environment variables can be used to override default settings.

-author("Steve Roques").

%% Public API
-export([
    summarize_url/1, summarize_url/2,
    summarize_html/1, summarize_html/2,
    summarize_with_ollama/1, summarize_with_ollama/2,
    print_result/1,
    default_config/0,
    get_env_config/0
]).

%% Default configuration constants
-define(DEFAULT_ENDPOINT, "http://localhost:11434/api/generate").
-define(DEFAULT_MODEL, <<"phi3">>).
-define(DEFAULT_PROMPT_TEMPLATE, "Résume ce contenu en français en quelques phrases claires et concises :\n\n~s").

%% Types
-type config() :: #{
    endpoint => string(),
    model => binary(),
    prompt_template => string()
}.

-type summarize_result() :: {ok, binary()} | {error, term()}.

%% =============================================================================
%% Public API with default configuration
%% =============================================================================

%% @doc Summarize content from a URL using default/environment configuration.
%% @param Url The URL to fetch and summarize
%% @returns {ok, Summary} | {error, Reason}
-spec summarize_url(string()) -> summarize_result().
summarize_url(Url) ->
    summarize_url(Url, get_env_config()).

%% @doc Summarize HTML content using default/environment configuration.
%% @param Html The HTML content to summarize
%% @returns {ok, Summary} | {error, Reason}
-spec summarize_html(string()) -> summarize_result().
summarize_html(Html) ->
    summarize_html(Html, get_env_config()).

%% @doc Summarize plain text using default/environment configuration.
%% @param Text The text content to summarize
%% @returns {ok, Summary} | {error, Reason}
-spec summarize_with_ollama(string()) -> summarize_result().
summarize_with_ollama(Text) ->
    summarize_with_ollama(Text, get_env_config()).

%% =============================================================================
%% Public API with custom configuration
%% =============================================================================

%% @doc Summarize content from a URL using custom configuration.
%% @param Url The URL to fetch and summarize
%% @param Config Custom configuration map
%% @returns {ok, Summary} | {error, Reason}
-spec summarize_url(string(), config()) -> summarize_result().
summarize_url(Url, Config) ->
    case fetch_html(Url) of
        {ok, Html} ->
            CleanText = extract_text_from_html(Html),
            summarize_with_ollama(CleanText, Config);
        {error, Reason} ->
            {error, {fetch_failed, Reason}}
    end.

%% @doc Summarize HTML content using custom configuration.
%% @param Html The HTML content to summarize
%% @param Config Custom configuration map
%% @returns {ok, Summary} | {error, Reason}
-spec summarize_html(string(), config()) -> summarize_result().
summarize_html(Html, Config) ->
    CleanText = extract_text_from_html(Html),
    summarize_with_ollama(CleanText, Config).

%% @doc Summarize plain text using custom configuration.
%% @param Text The text content to summarize
%% @param Config Custom configuration map containing endpoint, model, and prompt_template
%% @returns {ok, Summary} | {error, Reason}
-spec summarize_with_ollama(string(), config()) -> summarize_result().
summarize_with_ollama(Text, Config) ->
    Endpoint = maps:get(endpoint, Config, ?DEFAULT_ENDPOINT),
    Model = maps:get(model, Config, ?DEFAULT_MODEL),
    PromptTemplate = maps:get(prompt_template, Config, ?DEFAULT_PROMPT_TEMPLATE),
    
    % Format the prompt with the text content
    Prompt = io_lib:format(PromptTemplate, [Text]),
    
    % Prepare JSON payload for Ollama API
    Payload = jsx:encode(#{
        <<"model">> => Model,
        <<"prompt">> => list_to_binary(Prompt),
        <<"stream">> => false
    }),
    
    % Make HTTP POST request to Ollama API
    Headers = [{"Content-Type", "application/json"}],
    case httpc:request(post, {Endpoint, Headers, "application/json", Payload}, [], []) of
        {ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} ->
            parse_ollama_response(Body);
        {ok, {{_Version, StatusCode, _ReasonPhrase}, _Headers, Body}} ->
            {error, {ollama_error, StatusCode, Body}};
        {error, Reason} ->
            {error, {request_failed, Reason}}
    end.

%% =============================================================================
%% Configuration functions
%% =============================================================================

%% @doc Get default hardcoded configuration.
%% @returns Default configuration map
-spec default_config() -> config().
default_config() ->
    #{
        endpoint => ?DEFAULT_ENDPOINT,
        model => ?DEFAULT_MODEL,
        prompt_template => ?DEFAULT_PROMPT_TEMPLATE
    }.

%% @doc Get configuration from environment variables with fallback to defaults.
%% Environment variables:
%% - OLLAMA_ENDPOINT: Ollama API endpoint (default: http://localhost:11434/api/generate)
%% - OLLAMA_MODEL: Model name to use (default: phi3)
%% - OLLAMA_PROMPT: Prompt template with ~s placeholder (default: French summary prompt)
%% @returns Configuration map with environment overrides
-spec get_env_config() -> config().
get_env_config() ->
    #{
        endpoint => os:getenv("OLLAMA_ENDPOINT", ?DEFAULT_ENDPOINT),
        model => list_to_binary(os:getenv("OLLAMA_MODEL", binary_to_list(?DEFAULT_MODEL))),
        prompt_template => os:getenv("OLLAMA_PROMPT", ?DEFAULT_PROMPT_TEMPLATE)
    }.

%% =============================================================================
%% Utility functions
%% =============================================================================

%% @doc Print the result of a summarization operation to stdout.
%% @param Result The result tuple from summarization functions
%% @returns ok | error
-spec print_result(summarize_result()) -> ok | error.
print_result({ok, Text}) ->
    io:format("~s~n", [Text]),
    ok;
print_result({error, Reason}) ->
    io:format("Erreur: ~p~n", [Reason]),
    error.

%% =============================================================================
%% Private functions
%% =============================================================================

%% @private
%% @doc Fetch HTML content from a URL.
%% @param Url The URL to fetch
%% @returns {ok, Html} | {error, Reason}
fetch_html(Url) ->
    % Ensure inets application is started for HTTP client
    application:start(inets),
    case httpc:request(get, {Url, []}, [], []) of
        {ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} ->
            {ok, Body};
        {ok, {{_Version, StatusCode, _ReasonPhrase}, _Headers, _Body}} ->
            {error, {http_error, StatusCode}};
        {error, Reason} ->
            {error, Reason}
    end.

%% @private
%% @doc Extract plain text from HTML by removing tags and normalizing whitespace.
%% @param Html The HTML content as string
%% @returns Clean text string
extract_text_from_html(Html) ->
    % Remove HTML tags
    NoTags = re:replace(Html, "<[^>]*>", " ", [global, {return, list}]),
    % Normalize whitespace (multiple spaces/newlines to single space)
    CleanSpaces = re:replace(NoTags, "\\s+", " ", [global, {return, list}]),
    % Trim leading/trailing whitespace
    string:strip(CleanSpaces).

%% @private
%% @doc Parse JSON response from Ollama API.
%% @param Body The HTTP response body as string
%% @returns {ok, ResponseText} | {error, Reason}
parse_ollama_response(Body) ->
    try
        Json = jsx:decode(list_to_binary(Body)),
        case maps:get(<<"response">>, Json, undefined) of
            undefined ->
                {error, no_response_field};
            Response ->
                {ok, Response}
        end
    catch
        _:Error ->
            {error, {json_parse_error, Error}}
    end.
