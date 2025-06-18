-module(ollama_summarizer).
-author("Steve Roques").

%% Public API
-export([
    summarize_url/1, summarize_url/2,
    summarize_html/1, summarize_html/2,
    summarize/1, summarize/2,
    print_result/1,
    default_config/0,
    get_env_config/0
]).

%% =============================================================================
%% Main summarization functions
%% =============================================================================

%% Summarize a web page by URL using environment/default config.
summarize_url(Url) ->
    summarize_url(Url, get_env_config()).

%% Summarize a web page by URL using a custom config.
summarize_url(Url, Config) ->
    case fetch_html(Url) of
        {ok, Html} -> summarize_html(Html, Config);
        {error, Reason} -> {error, {fetch_failed, Reason}}
    end.

%% Summarize HTML content using environment/default config.
summarize_html(Html) ->
    summarize_html(Html, get_env_config()).

%% Summarize HTML content using a custom config.
summarize_html(Html, Config) ->
    Text = extract_text_from_html(Html),
    summarize(Text, Config).

%% Summarize plain text using environment/default config.
summarize(Text) ->
    summarize(Text, get_env_config()).

%% Summarize plain text using a custom config.
summarize(Text, Config) ->
    %% Build the prompt using the template from config or default
    PromptTemplate = maps:get(prompt_template, Config, default_prompt_template()),
    Prompt = ollama_handler:format_prompt(PromptTemplate, [Text]),
    %% Delegate to ollama_handler with the full config
    ollama_handler:generate(Prompt, Config).

%% =============================================================================
%% Configuration helpers
%% =============================================================================

%% Returns the default config (handler defaults + our default prompt template).
default_config() ->
    maps:merge(
        ollama_handler:default_config(),
        #{prompt_template => default_prompt_template()}
    ).

%% Returns config from environment variables, falling back to defaults.
get_env_config() ->
    maps:merge(
        ollama_handler:get_env_config(),
        #{prompt_template => os:getenv("OLLAMA_PROMPT", default_prompt_template())}
    ).

%% The default prompt template for summarization.
default_prompt_template() ->
    "IMPORTANT : Résume ce contenu en français en quelques phrases claires et concises:\n\n~s".

%% =============================================================================
%% Utility functions
%% =============================================================================

%% Print the result of a summarization operation.
print_result(Result) ->
    ollama_handler:print_result(Result).

%% Fetch HTML content from a URL.
fetch_html(Url) ->
    application:ensure_all_started(inets),
    case httpc:request(get, {Url, []}, [], []) of
        {ok, {{_, 200, _}, _, Body}} -> {ok, Body};
        {ok, {{_, Code, _}, _, _}} -> {error, {http_error, Code}};
        {error, Reason} -> {error, Reason}
    end.

%% Extract plain text from HTML by removing tags and normalizing whitespace.
extract_text_from_html(Html) ->
    NoTags = re:replace(Html, "<[^>]*>", " ", [global, {return, list}]),
    CleanSpaces = re:replace(NoTags, "\\s+", " ", [global, {return, list}]),
    string:strip(CleanSpaces).

