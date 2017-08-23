:- module(hdt_server, []).

/** <module> HDT server

@author Wouter Beek
@version 2017/05-2017/08
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(atom_ext)).
:- use_module(library(conf_ext)).
:- use_module(library(dict_ext)).
:- use_module(library(error)).
:- use_module(library(html/html_ext)).
:- use_module(library(html/html_pagination)).
:- use_module(library(html/rdf_html)).
:- use_module(library(http/http_pagination)).
:- use_module(library(http/http_server)).
:- use_module(library(pagination)).
:- use_module(library(semweb/rdf_ext)).
:- use_module(library(settings)).
:- use_module(library(string_ext)).
:- use_module(library(uri/uri_ext)).
:- use_module(library(yall)).

:- http_handler(/, hdt_handler, [methods([get,head,options]),prefix]).
:- http_handler(root(graphs), graphs_handler,
                [methods([get,head,options]),prefix]).
:- http_handler(root(objects), objects_handler,
                [methods([get,head,options]),prefix]).
:- http_handler(root(predicates), predicates_handler,
                [methods([get,head,options]),prefix]).
:- http_handler(root(shared), shared_handler,
                [methods([get,head,options]),prefix]).
:- http_handler(root(subjects), subjects_handler,
                [methods([get,head,options]),prefix]).
:- http_handler(root(triples), triples_handler,
                [methods([get,head,options]),prefix]).

html:menu_item(hdt, graphs_handler, "graphs").
html:menu_item(hdt, objects_handler, "objects").       % est,     suggest
html:menu_item(hdt, predicates_handler, "predicates"). % est,     suggest
html:menu_item(hdt, shared_handler, "shared").         % est,     suggest
html:menu_item(hdt, subjects_handler, "subjects").     % est,     suggest
html:menu_item(hdt, triples_handler, "triples").       % est, rnd

html:handle_description(graphs_handler, "graphs").
html:handle_description(objects_handler, "objects").
html:handle_description(predicates_handler, "predicates").
html:handle_description(shared_handler, "shared").
html:handle_description(subjects_handler, "subjects").
html:handle_description(triples_handler, "triples").

:- initialization
   conf_json(Dict1),
   get_dict(graphs, Dict1, Dicts),
   forall(
     member(Dict2, Dicts),
     (
       _{file: File, name: Name} :< Dict2,
       rdf_global_id(graph:Name, G),
       hdt_init(File, G)
     )
   ).

:- meta_predicate
   terms_handler(+, 3, +),
   terms_method(+, 2, +, +, +).

:- multifile
    user:body//2,
    user:head//2.

:- set_setting(http:products, ['HDT-Server'-'v0.0.1']).

:- setting(
     default_page_size,
     positive_integer,
     10,
     "The default number of triples that is retreived in one request."
   ).
:- setting(
     maximum_page_size,
     positive_integer,
     100,
     "The maximum number of triples that can be retrieved in one request."
   ).



% /
hdt_handler(Request) :-
  rest_method(Request, hdt_method).

% GET, HEAD
hdt_method(Method, MediaTypes) :-
  http_is_get(Method),
  rest_media_type(MediaTypes, hdt_media_type).

% GET, HEAD: text/html
hdt_media_type(media(text/html,_)) :-
  html_page(hdt(_,[]), [], [\menu_deck(hdt)]).

menu_deck(Parent) -->
  {
    findall(
      card(Handle,Label,Content),
      (
        html:menu_item(Parent, Handle, Label),
        html:handle_description(Handle, Content)
      ),
      Cards
    )
  },
  html([
    h1("HDT Server"),
    div(class='card-columns', \html_maplist(menu_card, Cards))
  ]).

menu_card(card(Handle,Label,Content)) -->
  {http_link_to_id(Handle, [], Uri)},
  html(
    div(class=card,
      div(class='card-block', [
        a(href=Uri, h3(class='card-title', Label)),
        p(class='card-text', Content)
      ])
    )
  ).



% /graphs
graphs_handler(Request) :-
  rest_method(Request, graphs_method(Request)).

% GET, HEAD
graphs_method(Request, Method, MediaTypes) :-
  http_is_get(Method),
  setting(default_page_size, DefaultPageSize),
  setting(maximum_page_size, MaxPageSize),
  http_parameters(
    Request,
    [
      page(PageNumber, [
        default(1),
        description("The page number from the results set."),
        positive_integer
      ]),
      page_size(PageSize, [
        between(1, MaxPageSize),
        default(DefaultPageSize),
        description("The number of terms per full results set page.")
      ])
    ]
  ),
  memberchk(request_uri(RelUri), Request),
  http_absolute_uri(RelUri, Uri),
  pagination_bulk(
    [Gs]>>aggregate_all(set(G), hdt_graph(_, G), Gs),
    _{page_number: PageNumber, page_size: PageSize, uri: Uri},
    Page
  ),
  rest_media_type(MediaTypes, graphs_media_type(Page)).

% GET, HEAD: application/json
graphs_media_type(Page, media(application/json,_)) :-
  http_pagination_json(Page).
% GET, HEAD: text/html
graphs_media_type(Page, media(text/html,_)) :-
  http_pagination_header(Page),
  html_page(
    hdt(Page,["Graphs"]),
    [],
    [\html_pagination_result(Page, graph_table)]
  ).

graph_table(Gs) -->
  html(ul(\html_maplist(graph_row, Gs))).

graph_row(G) -->
  {
    http_link_to_id(triples_handler, [graph(G)], Uri),
    rdf_global_id(graph:Name, G)
  },
  html(li(a(href=Uri, Name))).



% /objects
objects_handler(Request) :-
  terms_handler(Request, hdt_object_, objects).

hdt_object_(false, Hdt, O) :-
  hdt_object(Hdt, O).
hdt_object_(true, Hdt, Id) :- % TBD
  hdt_object(Hdt, O),
  hdt_object_id(Hdt,O, Id).


% /predicates
predicates_handler(Request) :-
  terms_handler(Request, hdt_predicate_, predicates).

hdt_predicate_(false, Hdt, P) :-
  hdt_predicate(Hdt, P).
hdt_predicate_(true, Hdt, Id) :- % TBD
  hdt_predicate(Hdt, P),
  hdt_predicate_id(Hdt, P, Id).


% /shared
shared_handler(Request) :-
  terms_handler(Request, hdt_shared_, shared).

hdt_shared_(false, Hdt, Node) :-
  hdt_shared(Hdt, Node).
hdt_shared_(true, Hdt, Id) :- % TBD
  hdt_shared(Hdt, Node),
  hdt_subject_id(Hdt, Node, Id).


% /subjects
subjects_handler(Request) :-
  terms_handler(Request, hdt_subject_, subjects).

hdt_subject_(false, Hdt, S) :-
  hdt_subject(Hdt, S).
hdt_subject_(true, Hdt, Id) :- % TBD
  hdt_subject(Hdt, S),
  hdt_subject_id(Hdt, S, Id).


terms_handler(Request, Goal_3, Key) :-
  rest_method(Request, terms_method(Request, Goal_3, Key)).

% GET, HEAD
terms_method(Request, Goal_3, Key, Method, MediaTypes) :-
  http_is_get(Method),
  rdf_equal(graph:default, DefaultG),
  setting(default_page_size, DefaultPageSize),
  setting(maximum_page_size, MaxPageSize),
  http_parameters(
    Request,
    [
      graph(G, [
        atom,
        default(DefaultG),
        description("The named graph from which terms are enumerated.  When absent, terms are enumerated from the default graph.")
      ]),
      id(Id, [
        boolean,
        default(false),
        description("Return HDT identifiers i.o. RDF terms.")
      ]),
      page(PageNumber, [
        default(1),
        description("The page number from the results set."),
        positive_integer
      ]),
      page_size(PageSize, [
        between(1, MaxPageSize),
        default(DefaultPageSize),
        description("The number of terms per full results set page.")
      ])
    ]
  ),
  memberchk(request_uri(RelUri), Request),
  http_absolute_uri(RelUri, Uri),
  hdt_graph(Hdt, G),
  Options1 = _{page_number: PageNumber, page_size: PageSize, uri: Uri},
  (   var(G)
  ->  Options2 = Options1
  ;   put_dict(query, Options1, [graph(G)], Options2)
  ),
  pagination(
    P,
    call(Goal_3, Id, Hdt, P),
    {Key,G}/[N]>>rdf_statistic(hdt, Key, N, G),
    Options2,
    Page
  ),
  rest_media_type(MediaTypes, terms_media_type(Key, G, Page)).

% GET, HEAD: application/json
terms_media_type(_, _, Page, media(application/json,_)) :-
  http_pagination_json(Page).
% GET, HEAD: text/html
terms_media_type(Key, G, Page, media(text/html,_)) :-
  http_pagination_header(Page),
  atom_capitalize(Key, CKey),
  html_page(
    hdt(Page,[CKey]),
    [],
    [\html_pagination_result(Page, terms_table(G))]
  ).

terms_table(G, Ps) -->
  html(ul(\html_maplist(terms_row(G), Ps))).

terms_row(G, P) -->
  {
    (var(G) -> Query = [] ; Query = [graph(G)]),
    http_link_to_id(triples_handler, [subject(P)|Query], Uri1),
    http_link_to_id(triples_handler, [predicate(P)|Query], Uri2),
    http_link_to_id(triples_handler, [object(P)|Query], Uri3)
  },
  html(
    li([
      a(href=P, P),
      " ",
      a(href=Uri1, "(s)"),
      " ",
      a(href=Uri2, "(p)"),
      " ",
      a(href=Uri3, "(o)")
    ])
  ).



% /triples
triples_handler(Request) :-
  rest_method(Request, triples_method(Request)).

% GET, HEAD
triples_method(Request, Method, MediaTypes) :-
  http_is_get(Method),
  rdf_equal(graph:default, DefaultG),
  setting(default_page_size, DefaultPageSize),
  setting(maximum_page_size, MaxPageSize),
  http_parameters(
    Request,
    [
      est(Est, [
        boolean,
        default(false),
        description("Return the estimated number of results i.o. the actual results.  Default is `false'.")
      ]),
      graph(G, [
        atom,
        default(DefaultG),
        description("The named graph from which results are retrieved.  If absent, the default graph is used.")
      ]),
      id(Id, [
        boolean,
        default(false),
        description("Return HDT identifiers i.o. RDF terms.  Default is `false'.")
      ]),
      object(O1, [
        atom,
        description("Filter results with this object term."),
        optional(true)
      ]),
      page(PageNumber, [
        default(1),
        description("The page number of the results set."),
        positive_integer
      ]),
      page_size(PageSize, [
        between(1, MaxPageSize),
        default(DefaultPageSize),
        description("The number of data triples on each (full) page.")
      ]),
      predicate(P1, [
        atom,
        description("Filter results with this predicate term."),
        optional(true)
      ]),
      subject(S1, [
        atom,
        description("Filter results with this subject term."),
        optional(true)
      ])
    ]
  ),
  (   Id == true
  ->  maplist(id_arg_, [S1,P1,O1], [S2,P2,O2])
  ;   maplist(term_arg_, [S1,P1,O1], [S2,P2,O2])
  ),
  memberchk(request_uri(RelUri), Request),
  http_absolute_uri(RelUri, Uri),
  include(ground, [graph(G),object(O1),predicate(P1),subject(S1)], Query),
  (hdt_graph(Hdt, G) -> true ; existence_error(hdt_graph, G)),
  Options = _{
    page_number: PageNumber,
    page_size: PageSize,
    query: [id(Id)|Query],
    uri: Uri
  },
  (   Est == true
  ->  hdt_cost_(Id, Hdt, S2, P2, O2, Cost),
      rest_media_type(MediaTypes, triples_est_media_type(Cost))
  ;   pagination(
        rdf(S2,P2,O2),
        hdt_search_(Id, Hdt, S2, P2, O2),
        hdt_cost_(Id, Hdt, S2, P2, O2),
        Options,
        Page
      ),
      (   Id == true
      ->  rest_media_type(MediaTypes, triples_id_media_type(G, Page))
      ;   rest_media_type(MediaTypes, triples_media_type(G, Page))
      )
  ).

id_arg_(X, X) :-
  var(X), !.
id_arg_(Atom, N) :-
  atom_number(Atom, N),
  must_be(positive_integer, N).

term_arg_(X, X) :-
  var(X), !.
term_arg_(Atom, Term) :-
  rdf_atom_to_term(Atom, Term).

% HACK: In LL12 we did not clean literals based on their datatype IRI,
%       e.g., gYear',"1996-01-01T00:00:00-04:00"
hdt_search_(false, Hdt, S, P, O) :- !,
  catch(hdt_search(Hdt, S, P, O), _E, fail).
hdt_search_(true, Hdt, S, P, O) :- !,
  hdt_search_id(Hdt, S, P, O).

hdt_cost_(false, Hdt, S, P, O, Cost) :- !,
  hdt_search_cost(Hdt, S, P, O, Cost).
hdt_cost_(true, Hdt, S, P, O, Cost) :-
  hdt_search_cost_id(Hdt, S, P, O, Cost).

% GET, HEAD: application/json
triples_est_media_type(Cost, media(application/json,_)) :-
  format("Content-Type: application/json\n"),
  nl,
  json_write_dict(current_output, Cost).
% GET, HEAD: text/html
triples_est_media_type(Cost, media(text/html,_)) :-
  html_page(hdt(_,["Triples","Estimate"]), [], [\html_thousands(Cost)]).

% GET, HEAD: application/json
triples_id_media_type(_, Page, media(application/json,_)) :-
  http_pagination_json(Page).
% GET, HEAD: text/html
triples_id_media_type(G, Page, media(text/html,_)) :-
  http_pagination_header(Page),
  rdf_global_id(graph:GLocal, G),
  html_page(
    hdt(Page,["Triples","Numeric Identifiers",GLocal]),
    [],
    [\html_pagination_result(Page, hdt_id_table(Page.uri, G))]
  ).

hdt_id_table(Uri, G, Triples) -->
  table(
    \table_header_row(["Subject","Predicate","Object"]),
    \html_maplist(hdt_id_table_row(Uri, G), Triples)
  ).

hdt_id_table_row(Uri, G, rdf(S,P,O)) -->
  {
    (var(G) -> Query = [id(true)] ; Query = [graph(G),id(true)]),
    maplist(
      uri_comp_set(query, Uri),
      [[subject(S)|Query],[predicate(P)|Query],[object(O)|Query]],
      [UriS,UriP,UriO]
    )
  },
  html(
    tr([
      td(a(href=UriS, S)),
      td(a(href=UriP, P)),
      td(a(href=UriO, O))
    ])
  ).

% GET, HEAD: application/n-triples
triples_media_type(_, Page, media(application/'n-triples',_)) :-
  format("Content-Type: application/n-triples\n"),
  http_pagination_header(Page),
  nl,
  write_ntuples(triples, Page.results).
% GET, HEAD: text/html
triples_media_type(G, Page, media(text/html,_)) :-
  http_pagination_header(Page),
  rdf_global_id(graph:GLocal, G),
  html_page(
    hdt(Page,["Triples",GLocal]),
    [],
    [\html_pagination_result(Page, hdt_triple_table(Page.uri, G))]
  ).

hdt_triple_table(Uri, G, Triples) -->
  table(
    \table_header_row(["Subject","Predicate","Object"]),
    \html_maplist(hdt_triple_table_row(Uri, G), Triples)
  ).

hdt_triple_table_row(Uri, G, rdf(S,P,O)) -->
  {
    maplist(rdf_term_to_atom, [S,P,O], [AtomS,AtomP,AtomO]),
    (var(G) -> Query = [] ; Query = [graph(G)]),
    maplist(
      uri_comp_set(query, Uri),
      [[subject(AtomS)|Query],[predicate(AtomP)|Query],[object(AtomO)|Query]],
      [UriS,UriP,UriO]
    )
  },
  html(
    tr([
      td(a(href=UriS, \rdf_html_subject(S))),
      td(a(href=UriP, \rdf_html_predicate(P))),
      td(a(href=UriO, \rdf_html_object(O)))
    ])
  ).



% HTML STYLE %

user:head(hdt(Page,Subtitles), Content_0) -->
  {string_list_concat(["HDT-Server"|Subtitles], " ― ", Title)},
  html(
    head([
      \html_root_attribute(lang, en),
      meta(charset='utf-8', []),
      \meta_ie_latest,
      \meta_viewport,
      \favicon,
      \html_if_then(ground(Page), html_pagination_links(Page)),
      title(Title),
      \html_requires(html_ext)
    | Content_0
    ])
  ).

user:body(hdt(_,_), Content_0) -->
  html(body([\navbar(\brand, \menu, \krr)|Content_0])).

brand -->
  html("HDT-Server").

krr -->
  {
    aggregate_all(set(G), hdt_graph(_, G), Gs),
    http_absolute_location(img('krr.svg'), Image)
  },
  html([
    \navbar_dropdown_menu('graph-menu', "Graph", graph_item, Gs),
    a(href='http://krr.cs.vu.nl', img(src=Image, []))
  ]).

graph_item(G) -->
  {rdf_global_id(graph:Name, G)},
  html(option([value(G)], Name)).
