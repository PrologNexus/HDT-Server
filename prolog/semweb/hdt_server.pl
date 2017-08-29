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
:- use_module(library(graph/rdf2gml)).
:- use_module(library(html/html_doc)).
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

:- dynamic
    html:handle_description/2,
    html:menu_item/2,
    html:menu_item/3.

:- http_handler(/, hdt_handler, [methods([get,head,options]),priority(-1)]).
:- http_handler(root(doc), doc_handler, [methods([get,head,options])]).
:- http_handler(root(graphs), graphs_handler, [methods([get,head,options])]).
:- http_handler(root(objects), objects_handler, [methods([get,head,options])]).
:- http_handler(root(objects/est), objects_est_handler, [methods([get,head,options])]).
:- http_handler(root(objects/id), objects_id_handler, [methods([get,head,options])]).
:- http_handler(root(predicates), predicates_handler, [methods([get,head,options])]).
:- http_handler(root(predicates/est), predicates_est_handler, [methods([get,head,options])]).
:- http_handler(root(predicates/id), predicates_id_handler, [methods([get,head,options])]).
:- http_handler(root(shared), shared_handler, [methods([get,head,options])]).
:- http_handler(root(shared/est), shared_est_handler, [methods([get,head,options])]).
:- http_handler(root(shared/id), shared_id_handler, [methods([get,head,options])]).
:- http_handler(root(subjects), subjects_handler, [methods([get,head,options])]).
:- http_handler(root(subjects/est), subjects_est_handler, [methods([get,head,options])]).
:- http_handler(root(subjects/id), subjects_id_handler, [methods([get,head,options])]).
:- http_handler(root(triples), triples_handler, [methods([get,head,options])]).
:- http_handler(root(triples/est), triples_est_handler, [methods([get,head,options])]).
:- http_handler(root(triples/id), triples_id_handler, [methods([get,head,options])]).

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
   terms_handler(+, 2, +),
   terms_id_handler(+, 2, +),
   terms_id_method(+, 2, +, +, +),
   terms_method(+, 2, +, +, +).

:- multifile
    html:handle_description/2,
    html:menu_item/2,
    html:menu_item/3,
    user:body//2,
    user:head//2.

html:handle_description(doc_handler, "Documentation").
html:handle_description(graphs_handler, "Graphs").
html:handle_description(objects_handler, "Objects").
html:handle_description(predicates_handler, "Predicates").
html:handle_description(shared_handler, "Shared terms").
html:handle_description(subjects_handler, "Subjects").
html:handle_description(triples_handler, "Triples").

html:menu_item(doc_handler, "Documentation").
html:menu_item(hdt_terms, "Terms").
  html:menu_item(hdt_terms, graphs_handler, "Graphs").
  html:menu_item(hdt_terms, objects_handler, "Objects").
  html:menu_item(hdt_terms, predicates_handler, "Predicates").
  html:menu_item(hdt_terms, shared_handler, "Shared").
  html:menu_item(hdt_terms, subjects_handler, "Subjects").
html:menu_item(hdt_terms_id, "Term IDs").
  html:menu_item(hdt_terms_id, objects_id_handler, "Object IDs").
  html:menu_item(hdt_terms_id, predicates_id_handler, "Predicate IDs").
  html:menu_item(hdt_terms_id, shared_id_handler, "Shared IDs").
  html:menu_item(hdt_terms_id, subjects_id_handler, "Subject IDs").
html:menu_item(triples_handler, "Triples").
html:menu_item(triples_id_handler, "Triples IDs").

:- set_setting(http:products, ['HDT-Server'-'v0.0.1']).





% /
hdt_handler(Request) :-
  rest_method(Request, hdt_method).

% /: GET,HEAD
hdt_method(Method, MediaTypes) :-
  http_is_get(Method),
  rest_media_type(MediaTypes, hdt_media_type).

% /: GET,HEAD: text/html
hdt_media_type(media(text/html,_)) :-
  html_page(hdt(_,[]), [], [\hdt_page]).

hdt_page -->
  html([h1("HDT APIs"),\menu_deck]).

menu_deck -->
  {
    aggregate_all(
      set(menu_item(Handler,Label)),
      html:menu_item(Handler, Label),
      Nodes
    )
  },
  html(div(class='card-columns', \html_convlist(menu_card, Nodes))).

menu_card(menu_item(MajorHandler,MajorLabel)) -->
  {
    aggregate_all(
      set(menu_item(MinorHandler,MinorLabel)),
      html:menu_item(MajorHandler, MinorHandler, MinorLabel),
      MinorNodes
    ),
    MinorNodes \== []
  }, !,
  html([h2(MajorLabel),\html_maplist(menu_card, MinorNodes)]).
menu_card(menu_item(Handler,Label)) -->
  {http_link_to_id(Handler, [], Uri)},
  html(
    div(class=card,
      div(class='card-block', [
        a(href=Uri, h3(class='card-title', Label)),
        p(class='card-text', Label)
      ])
    )
  ).



% /doc
doc_handler(Request) :-
  rest_method(Request, doc_method).

% /doc: GET,HEAD
doc_method(Method, MediaTypes) :-
  http_is_get(Method),
  rest_media_type(MediaTypes, doc_media_type).

% /doc: GET,HEAD: text/html
doc_media_type(media(text/html,_)) :-
  html_page(hdt(_,[]), [], [\http_param_table(hdt_server)]).



% /graphs
graphs_handler(Request) :-
  rest_method(Request, graphs_method(Request)).

% /graphs: GET,HEAD
graphs_method(Request, Method, MediaTypes) :-
  http_is_get(Method),
  http_parameters(
    Request,
    [page(PageNumber),page_size(PageSize)],
    [attribute_declarations(http_param)]
  ),
  memberchk(request_uri(RelUri), Request),
  http_absolute_uri(RelUri, Uri),
  pagination_bulk(
    [Gs]>>aggregate_all(set(G), hdt_graph(_, G), Gs),
    _{page_number: PageNumber, page_size: PageSize, uri: Uri},
    Page
  ),
  rest_media_type(MediaTypes, graphs_media_type(Page)).

% /graphs: GET,HEAD: application/json
graphs_media_type(Page, media(application/json,_)) :-
  http_pagination_json(Page).
% /graphs: GET,HEAD: text/html
graphs_media_type(Page, media(text/html,_)) :-
  http_pagination_header(Page),
  html_page(hdt(Page,["Graphs"]), [],
            [\html_pagination_result(Page, graph_table)]).

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

hdt_object_(Hdt, O) :-
  hdt_object(Hdt, O).

% /objects/est
objects_est_handler(Request) :-
  terms_est_handler(Request, objects).

% /objects/id
objects_id_handler(Request) :-
  terms_id_handler(Request, hdt_object_id_, objects).

hdt_object_id_(Hdt, Id) :- % TBD
  hdt_object(Hdt, O),
  hdt_object_id(Hdt,O, Id).



% /predicates
predicates_handler(Request) :-
  terms_handler(Request, hdt_predicate_, predicates).

hdt_predicate_(Hdt, P) :-
  hdt_predicate(Hdt, P).

% /predicates/est
predicates_est_handler(Request) :-
  terms_est_handler(Request, predicates).

% /predicates/id
predicates_id_handler(Request) :-
  terms_id_handler(Request, hdt_predicate_id_, predicates).

hdt_predicate_id_(Hdt, Id) :- % TBD
  hdt_predicate(Hdt, P),
  hdt_predicate_id(Hdt, P, Id).



% /shared
shared_handler(Request) :-
  terms_handler(Request, hdt_shared_, shared).

hdt_shared_(Hdt, Node) :-
  hdt_shared(Hdt, Node).

% /shared/est
shared_est_handler(Request) :-
  terms_est_handler(Request, shared).

% /shared/id
shared_id_handler(Request) :-
  terms_id_handler(Request, hdt_shared_, shared).

hdt_shared_id_(Hdt, Id) :- % TBD
  hdt_shared(Hdt, Node),
  hdt_subject_id(Hdt, Node, Id).



% /subjects
subjects_handler(Request) :-
  terms_handler(Request, hdt_subject_, subjects).

hdt_subject_(Hdt, S) :-
  hdt_subject(Hdt, S).

% /subjects/est
subjects_est_handler(Request) :-
  terms_est_handler(Request, subjects).

% /subjects/id
subjects_id_handler(Request) :-
  terms_id_handler(Request, hdt_subject_id_, subjects).

hdt_subject_id_(Hdt, Id) :- % TBD
  hdt_subject(Hdt, S),
  hdt_subject_id(Hdt, S, Id).



% /terms
terms_handler(Request, Goal_2, Key) :-
  rest_method(Request, terms_method(Request, Goal_2, Key)).

% /terms: GET,HEAD
terms_method(Request, Goal_2, Key, Method, MediaTypes) :-
  http_is_get(Method),
  http_parameters(
    Request,
    [graph(G),page(PageNumber),page_size(PageSize),prefix(Prefix),rnd(_Rnd)],
    [attribute_declarations(http_param)]
  ),
  memberchk(request_uri(RelUri), Request),
  http_absolute_uri(RelUri, Uri),
  (hdt_graph(Hdt, G) -> true ; existence_error(hdt_graph, G)),
  Options = _{graph: G, page_number: PageNumber, page_size: PageSize, uri: Uri},
  (   ground(Prefix)
  ->  key_role_(Key, Role),
      pagination_bulk(
        hdt_suggestions(Hdt, Prefix, Role, PageSize),
        Options,
        Page
      ),
      rest_media_type(MediaTypes, terms_media_type(Key, G, Page))
  ;   pagination(
        Term,
        call(Goal_2, Hdt, Term),
        {Key,G}/[N]>>rdf_statistic(hdt, Key, N, G),
        Options,
        Page
      ),
      rest_media_type(MediaTypes, terms_media_type(Key, G, Page))
  ).

key_role_(objects, object).
key_role_(predicates, predicate).
key_role_(shared, subject). %HACK
key_role_(subjects, subject).

% /terms: GET,HEAD: application/json
terms_media_type(_, _, Page, media(application/json,_)) :-
  http_pagination_json(Page).
% /terms: GET,HEAD: text/html
terms_media_type(Key, G, Page, media(text/html,_)) :-
  http_pagination_header(Page),
  atom_capitalize(Key, CKey),
  html_page(hdt(Page,[CKey]), [],
            [\html_pagination_result(Page, terms_table(G))]).

terms_table(G, Terms) -->
  html(ul(\html_maplist(terms_row(G), Terms))).

terms_row(G, Term) -->
  {
    (rdf_equal(graph:default, G) -> Query = [] ; Query = [graph(G)]),
    http_link_to_id(triples_handler, [subject(Term)|Query], UriS),
    http_link_to_id(triples_handler, [predicate(Term)|Query], UriP),
    http_link_to_id(triples_handler, [object(Term)|Query], UriO)
  },
  html(
    li([
      a(href=Term, Term),
      " ",
      a(href=UriS, "(s)"),
      " ",
      a(href=UriP, "(p)"),
      " ",
      a(href=UriO, "(o)")
    ])
  ).



% /terms/est
terms_est_handler(Request, Key) :-
  rest_method(Request, terms_est_method(Request, Key)).

% /terms/est: GET,HEAD
terms_est_method(Request, Key, Method, MediaTypes) :-
  http_is_get(Method),
  http_parameters(Request, [graph(G)], [attribute_declarations(http_param)]),
  (hdt_graph(Hdt, G) -> true ; existence_error(hdt_graph, G)),
  rdf_statistic(hdt0, Key, Cost, Hdt),
  rest_media_type(MediaTypes, terms_est_media_type(Key, Cost)).

% /terms/est: GET,HEAD: application/json
terms_est_media_type(_, Cost, media(application/json,_)) :-
  http_reply_json(Cost).
% /terms/est: GET,HEAD: text/html
terms_est_media_type(Key, Cost, media(text/html,_)) :-
  atom_capitalize(Key, CKey),
  html_page(hdt(_,["Terms",CKey]), [], [\html_thousands(Cost)]).



% /terms/id
terms_id_handler(Request, Goal_2, Key) :-
  rest_method(Request, terms_id_method(Request, Goal_2, Key)).

% /terms/id: GET,HEAD
terms_id_method(Request, Goal_2, Key, Method, MediaTypes) :-
  http_is_get(Method),
  http_parameters(
    Request,
    [graph(G),page(PageNumber),page_size(PageSize),prefix(Prefix),rnd(_Rnd)],
    [attribute_declarations(http_param)]
  ),
  memberchk(request_uri(RelUri), Request),
  http_absolute_uri(RelUri, Uri),
  (hdt_graph(Hdt, G) -> true ; existence_error(hdt_graph, G)),
  Options = _{
    graph: G,
    page_number: PageNumber,
    page_size: PageSize,
    uri: Uri
  },
  (   ground(Prefix)
  ->  key_role_(Key, Role),
      pagination_bulk(
        hdt_suggestions(Hdt, Prefix, Role, PageSize),
        Options,
        Page
      )
  ;   pagination(
        Term,
        call(Goal_2, Hdt, Term),
        {Key,G}/[N]>>rdf_statistic(hdt, Key, N, G),
        Options,
        Page
      )
  ),
  rest_media_type(MediaTypes, terms_id_media_type(Key, G, Page)).

% /terms/id: GET,HEAD: application/json
terms_id_media_type(_, _, Page, media(application/json,_)) :-
  http_pagination_json(Page).
% /terms/id: GET,HEAD: text/html
terms_id_media_type(Key, G, Page, media(text/html,_)) :-
  http_pagination_header(Page),
  atom_capitalize(Key, CKey),
  html_page(hdt(Page,[CKey]), [],
            [\html_pagination_result(Page, terms_id_table(G))]).

terms_id_table(G, Ids) -->
  html(ul(\html_maplist(terms_id_row(G), Ids))).

terms_id_row(G, Id) -->
  {
    (rdf_equal(graph:default, G) -> Query = [] ; Query = [graph(G)]),
    http_link_to_id(triples_handler, [subject(Id)|Query], UriS),
    http_link_to_id(triples_handler, [predicate(Id)|Query], UriP),
    http_link_to_id(triples_handler, [object(Id)|Query], UriO)
  },
  html(
    li([
      Id,
      " ",
      a(href=UriS, "(s)"),
      " ",
      a(href=UriP, "(p)"),
      " ",
      a(href=UriO, "(o)")
    ])
  ).



% /triples
triples_handler(Request) :-
  rest_method(Request, triples_method(Request)).

% /triples: GET,HEAD
triples_method(Request, Method, MediaTypes) :-
  http_is_get(Method),
  http_parameters(
    Request,
    [
      graph(G),
      object(O1),
      page(PageNumber),
      page_size(PageSize),
      predicate(P1),
      subject(S1)
    ],
    [attribute_declarations(http_param)]
  ),
  maplist(term_arg_, [S1,P1,O1], [S2,P2,O2]),
  memberchk(request_uri(RelUri), Request),
  http_absolute_uri(RelUri, Uri),
  include(ground, [object(O1),predicate(P1),subject(S1)], Query),
  (hdt_graph(Hdt, G) -> true ; existence_error(hdt_graph, G)),
  pagination(
    rdf(S2,P2,O2),
    % HACK: In LL12 we did not clean literals based on their datatype
    %       IRI, e.g., gYear',"1996-01-01T00:00:00-04:00"
    catch(hdt_search(Hdt, S2, P2, O2), _E, fail),
    hdt_search_cost(Hdt, S2, P2, O2),
    _{
      page_number: PageNumber,
      page_size: PageSize,
      query: [graph(G)|Query],
      uri: Uri
    },
    Page
  ),
  rest_media_type(MediaTypes, triples_media_type(G, Page)).

term_arg_(X, X) :-
  var(X), !.
term_arg_(Atom, Term) :-
  rdf_atom_to_term(Atom, Term).

% /triples: GET,HEAD: application/n-triples
triples_media_type(_, Page, media(application/'n-triples',_)) :-
  format("Content-Type: application/n-triples\n"),
  http_pagination_header(Page),
  nl,
  write_ntuples(Page.results).
% /triples: GET,HEAD: text/html
triples_media_type(G, Page, media(text/html,_)) :-
  http_pagination_header(Page),
  rdf_global_id(graph:GLocal, G),
  html_page(hdt(Page,["Triples",GLocal]), [],
            [\html_pagination_result(Page, rdf_html_triple_table(Page.uri, G))]).



% /triples/est
triples_est_handler(Request) :-
  rest_method(Request, triples_est_method(Request)).

% /triples/est: GET,HEAD
triples_est_method(Request, Method, MediaTypes) :-
  http_is_get(Method),
  http_parameters(
    Request,
    [graph(G),object(O1),predicate(P1),subject(S1)],
    [attribute_declarations(http_param)]
  ),
  (hdt_graph(Hdt, G) -> true ; existence_error(hdt_graph, G)),
  maplist(term_arg_, [S1,P1,O1], [S2,P2,O2]),
  hdt_search_cost(Hdt, S2, P2, O2, Cost),
  rest_media_type(MediaTypes, triples_est_media_type(Cost)).

% /triples/est: GET,HEAD: application/json
triples_est_media_type(Cost, media(application/json,_)) :-
  http_reply_json(Cost).
% /triples/est: GET,HEAD: text/html
triples_est_media_type(Cost, media(text/html,_)) :-
  html_page(hdt(_,["Triples","Estimate"]), [], [\html_thousands(Cost)]).



% /triples/id
triples_id_handler(Request) :-
  rest_method(Request, triples_id_method(Request)).

% /triples/id: GET,HEAD
triples_id_method(Request, Method, MediaTypes) :-
  http_is_get(Method),
  http_parameters(
    Request,
    [
      graph(G),
      object(O1),
      page(PageNumber),
      page_size(PageSize),
      predicate(P1),
      subject(S1)
    ],
    [attribute_declarations(http_param)]
  ),
  maplist(id_arg_, [S1,P1,O1], [S2,P2,O2]),
  memberchk(request_uri(RelUri), Request),
  http_absolute_uri(RelUri, Uri),
  (hdt_graph(Hdt, G) -> true ; existence_error(hdt_graph, G)),
  include(ground, [object(O1),predicate(P1),subject(S1)], Query),
  pagination(
    rdf(S2,P2,O2),
    hdt_search_id(Hdt, S2, P2, O2),
    hdt:hdt_search_cost_id(Hdt, S2, P2, O2),
    _{
      page_number: PageNumber,
      page_size: PageSize,
      query: [graph(G)|Query],
      uri: Uri
    },
    Page
  ),
  rest_media_type(MediaTypes, triples_id_media_type(G, Page)).

id_arg_(X, X) :-
  var(X), !.
id_arg_(Atom, N) :-
  atom_number(Atom, N),
  must_be(positive_integer, N).

% /triples/id: GET,HEAD: GML
triples_id_media_type(_, Page, media(application/gml,_)) :-
  format("Content-Type: application/gml\n"),
  http_pagination_header(Page),
  nl,
  write_gml_triples(Page.results).
% /triples/id: GET,HEAD: application/json
triples_id_media_type(_, Page, media(application/json,_)) :-
  http_pagination_json(Page).
% /triples/id: GET,HEAD: text/html
triples_id_media_type(G, Page, media(text/html,_)) :-
  http_pagination_header(Page),
  rdf_global_id(graph:GLocal, G),
  html_page(hdt(Page,["Triples","Identifiers",GLocal]), [],
            [\html_pagination_result(Page, hdt_id_table(Page.uri, G))]).

write_gml_triples(Triples) :-
  maplist(write_gml_triple, Triples).

write_gml_triple(rdf(S,P,O)) :-
  format("edge [ label ~a source ~a target ~a ]\n", [P,S,O]).

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





% HTTP PARAMETERS %

http_param(est, [
  boolean,
  default(false),
  description("Return the estimated number results i.o. the actual results.")
]).
http_param(graph, [
  atom,
  default(G),
  description("The named graph from which results are retrieved.  When absent, results are retrieved from the default graph.")
]) :-
  rdf_equal(graph:default, G).
http_param(id, [
  boolean,
  default(false),
  description("Return HDT identifiers i.o. RDF terms.")
]).
http_param(object, [
  atom,
  description("Filter results with this object term."),
  optional(true)
]).
http_param(page, [
  default(1),
  description("The page number from the results set."),
  positive_integer
]).
http_param(page_size, [
  between(1, MaxPageSize),
  default(DefaultPageSize),
  description("The number of results per full result set page.")
]) :-
  setting(pagination:default_page_size, DefaultPageSize),
  setting(pagination:maximum_page_size, MaxPageSize).
http_param(predicate, [
  atom,
  description("Filter results with this predicate term."),
  optional(true)
]).
http_param(prefix, [
  atom,
  description("Filter for terms that have this prefix."),
  optional(true)
]).
http_param(rnd, [
  boolean,
  default(false),
  description("Retrieve a randomly chosen triple.  Default is `false'.")
]).
http_param(subject, [
  atom,
  description("Filter results with this subject term."),
  optional(true)
]).





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
  {http_absolute_location(img('krr.svg'), Image)},
  html(a(href='http://krr.cs.vu.nl', img(src=Image, []))).
