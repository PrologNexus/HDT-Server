:- module(hdt_server, []).

/** <module> HDT server

@author Wouter Beek
@version 2017/05-2017/09
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
    html:handler_description/2,
    html:menu_item/2,
    html:menu_item/3.

:- http_handler(/, hdt_handler, [methods([get,head,options]),priority(-1)]).
:- http_handler(root(doc), doc_handler, [methods([get,head,options])]).
:- http_handler(root(graph), graph_handler, [methods([get,head,options])]).
:- http_handler(root(node), node_handler, [methods([get,head,options])]).
:- http_handler(root(node/count), node_count_handler, [methods([get,head,options])]).
:- http_handler(root(node/id), node_id_handler, [methods([get,head,options])]).
:- http_handler(root(object), object_handler, [methods([get,head,options])]).
:- http_handler(root(object/count), object_count_handler, [methods([get,head,options])]).
:- http_handler(root(object/id), object_id_handler, [methods([get,head,options])]).
:- http_handler(root(predicate), predicate_handler, [methods([get,head,options])]).
:- http_handler(root(predicate/count), predicate_count_handler, [methods([get,head,options])]).
:- http_handler(root(predicate/id), predicate_id_handler, [methods([get,head,options])]).
:- http_handler(root(shared), shared_handler, [methods([get,head,options])]).
:- http_handler(root(shared/count), shared_count_handler, [methods([get,head,options])]).
:- http_handler(root(shared/id), shared_id_handler, [methods([get,head,options])]).
:- http_handler(root(subject), subject_handler, [methods([get,head,options])]).
:- http_handler(root(subject/count), subject_count_handler, [methods([get,head,options])]).
:- http_handler(root(subject/id), subject_id_handler, [methods([get,head,options])]).
:- http_handler(root(triple), triple_handler, [methods([get,head,options])]).
:- http_handler(root(triple/count), triple_count_handler, [methods([get,head,options])]).
:- http_handler(root(triple/id), triple_id_handler, [methods([get,head,options])]).

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
   term_handler(+, 2, +),
   term_id_handler(+, 2, +),
   term_id_method(+, 2, +, +, +),
   term_method(+, 2, +, +, +).

:- multifile
    html:handler_description/2,
    html:menu_item/2,
    html:menu_item/3,
    user:body//2,
    user:head//2.

html:handler_description(graph_handler, "Graphs").
html:handler_description(node_handler, "Nodes").
html:handler_description(object_handler, "Objects").
html:handler_description(predicate_handler, "Predicates").
html:handler_description(shared_handler, "Shared terms").
html:handler_description(subject_handler, "Subjects").
html:handler_description(triple_handler, "Triples").

html:menu_item(term, "Terms").
  html:menu_item(term, graph_handler, "Graphs").
  html:menu_item(term, node_handler, "Nodes").
  html:menu_item(term, object_handler, "Objects").
  html:menu_item(term, predicate_handler, "Predicates").
  html:menu_item(term, shared_handler, "Shared").
  html:menu_item(term, subject_handler, "Subjects").
html:menu_item(term_id, "Term IDs").
  html:menu_item(term_id, object_id_handler, "Node IDs").
  html:menu_item(term_id, node_id_handler, "Object IDs").
  html:menu_item(term_id, predicate_id_handler, "Predicate IDs").
  html:menu_item(term_id, shared_id_handler, "Shared IDs").
  html:menu_item(term_id, subject_id_handler, "Subject IDs").
html:menu_item(triple, "Triples").
  html:menu_item(triple, triple_handler, "Triples").
  html:menu_item(triple, triple_id_handler, "Triples IDs").

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
  html_page(
    hdt(_,[]),
    [],
    [
      \http_doc_handler(hdt_server, graph_handler),
      \http_doc_handler(hdt_server, node_handler),
      \http_doc_handler(hdt_server, node_count_handler),
      \http_doc_handler(hdt_server, node_id_handler),
      \http_doc_handler(hdt_server, object_handler),
      \http_doc_handler(hdt_server, object_count_handler),
      \http_doc_handler(hdt_server, object_id_handler),
      \http_doc_handler(hdt_server, predicate_handler),
      \http_doc_handler(hdt_server, predicate_count_handler),
      \http_doc_handler(hdt_server, predicate_id_handler),
      \http_doc_handler(hdt_server, shared_handler),
      \http_doc_handler(hdt_server, shared_count_handler),
      \http_doc_handler(hdt_server, shared_id_handler),
      \http_doc_handler(hdt_server, subject_handler),
      \http_doc_handler(hdt_server, subject_count_handler),
      \http_doc_handler(hdt_server, subject_id_handler),
      \http_doc_handler(hdt_server, triple_handler),
      \http_doc_handler(hdt_server, triple_count_handler),
      \http_doc_handler(hdt_server, triple_id_handler)
    ]
  ).



% /graph
graph_handler(Request) :-
  rest_method(Request, graph_method(Request)).

% /graph: GET,HEAD
graph_method(Request, Method, MediaTypes) :-
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
  rest_media_type(MediaTypes, graph_media_type(Page)).

% /graph: GET,HEAD: application/json
graph_media_type(Page, media(application/json,_)) :-
  http_pagination_json(Page).
% /graph: GET,HEAD: text/html
graph_media_type(Page, media(text/html,_)) :-
  http_pagination_header(Page),
  html_page(hdt(Page,["Graphs"]), [],
            [\html_pagination_result(Page, graph_table)]).

graph_table(Gs) -->
  html(ul(\html_maplist(graph_row, Gs))).

graph_row(G) -->
  {
    http_link_to_id(triple_handler, [graph(G)], Uri),
    rdf_global_id(graph:Name, G)
  },
  html(li(a(href=Uri, Name))).



% /node
node_handler(Request) :-
  term_handler(Request, hdt_node_, nodes).

hdt_node_(Hdt, Node) :-
  hdt_node(Hdt, Node).

% /node/count
node_count_handler(Request) :-
  term_count_handler(Request, nodes).



% /object
object_handler(Request) :-
  term_handler(Request, hdt_object_, objects).

hdt_object_(Hdt, O) :-
  hdt_object(Hdt, O).

% /object/count
object_count_handler(Request) :-
  term_count_handler(Request, objects).

% /object/id
object_id_handler(Request) :-
  term_id_handler(Request, hdt_object_id_, objects).

hdt_object_id_(Hdt, Id) :- % TBD
  hdt_object(Hdt, O),
  hdt_object_id(Hdt, O, Id).



% /predicate
predicate_handler(Request) :-
  term_handler(Request, hdt_predicate_, predicates).

hdt_predicate_(Hdt, P) :-
  hdt_predicate(Hdt, P).

% /predicate/count
predicate_count_handler(Request) :-
  term_count_handler(Request, predicates).

% /predicate/id
predicate_id_handler(Request) :-
  term_id_handler(Request, hdt_predicate_id_, predicates).

hdt_predicate_id_(Hdt, Id) :- % TBD
  hdt_predicate(Hdt, P),
  hdt_predicate_id(Hdt, P, Id).



% /shared
shared_handler(Request) :-
  term_handler(Request, hdt_shared_, shared).

hdt_shared_(Hdt, Node) :-
  hdt_shared(Hdt, Node).

% /shared/count
shared_count_handler(Request) :-
  term_count_handler(Request, shared).

% /shared/id
shared_id_handler(Request) :-
  term_id_handler(Request, hdt_shared_, shared).

hdt_shared_id_(Hdt, Id) :- % TBD
  hdt_shared(Hdt, Node),
  hdt_subject_id(Hdt, Node, Id).



% /subject
subject_handler(Request) :-
  term_handler(Request, hdt_subject_, subjects).

hdt_subject_(Hdt, S) :-
  hdt_subject(Hdt, S).

% /subject/count
subject_count_handler(Request) :-
  term_count_handler(Request, subjects).

% /subject/id
subject_id_handler(Request) :-
  term_id_handler(Request, hdt_subject_id_, subjects).

hdt_subject_id_(Hdt, Id) :- % TBD
  hdt_subject(Hdt, S),
  hdt_subject_id(Hdt, S, Id).



% /term
term_handler(Request, Goal_2, Key) :-
  rest_method(Request, term_method(Request, Goal_2, Key)).

% /term: GET,HEAD
term_method(Request, Goal_2, Key, Method, MediaTypes) :-
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
      rest_media_type(MediaTypes, term_media_type(Key, G, Page))
  ;   pagination(
        Term,
        call(Goal_2, Hdt, Term),
        {Key,G}/[N]>>rdf_statistic(hdt, Key, N, G),
        Options,
        Page
      ),
      rest_media_type(MediaTypes, term_media_type(Key, G, Page))
  ).

key_role_(objects, object).
key_role_(predicates, predicate).
key_role_(shared, subject). %HACK
key_role_(subjects, subject).

% /term: GET,HEAD: application/json
term_media_type(_, _, Page, media(application/json,_)) :-
  http_pagination_json(Page).
% /term: GET,HEAD: text/html
term_media_type(Key, G, Page, media(text/html,_)) :-
  http_pagination_header(Page),
  atom_capitalize(Key, CKey),
  html_page(hdt(Page,[CKey]), [],
            [\html_pagination_result(Page, term_table(G))]).

term_table(G, Terms) -->
  html(ul(\html_maplist(term_row(G), Terms))).

term_row(G, Term) -->
  {
    (rdf_equal(graph:default, G) -> Query = [] ; Query = [graph(G)]),
    http_link_to_id(triple_handler, [subject(Term)|Query], UriS),
    http_link_to_id(triple_handler, [predicate(Term)|Query], UriP),
    http_link_to_id(triple_handler, [object(Term)|Query], UriO)
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



% /term/count
term_count_handler(Request, Key) :-
  rest_method(Request, term_count_method(Request, Key)).

% /term/count: GET,HEAD
term_count_method(Request, Key, Method, MediaTypes) :-
  http_is_get(Method),
  http_parameters(Request, [graph(G)], [attribute_declarations(http_param)]),
  (hdt_graph(Hdt, G) -> true ; existence_error(hdt_graph, G)),
  rdf_statistic(hdt0, Key, Cost, Hdt),
  rest_media_type(MediaTypes, term_count_media_type(Key, Cost)).

% /term/count: GET,HEAD: application/json
term_count_media_type(_, Cost, media(application/json,_)) :-
  http_reply_json(Cost).
% /term/count: GET,HEAD: text/html
term_count_media_type(Key, Cost, media(text/html,_)) :-
  atom_capitalize(Key, CKey),
  html_page(hdt(_,["Terms",CKey]), [], [\html_thousands(Cost)]).



% /term/id
term_id_handler(Request, Goal_2, Key) :-
  rest_method(Request, term_id_method(Request, Goal_2, Key)).

% /term/id: GET,HEAD
term_id_method(Request, Goal_2, Key, Method, MediaTypes) :-
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
  rest_media_type(MediaTypes, term_id_media_type(Key, G, Page)).

% /term/id: GET,HEAD: application/json
term_id_media_type(_, _, Page, media(application/json,_)) :-
  http_pagination_json(Page).
% /term/id: GET,HEAD: text/html
term_id_media_type(Key, G, Page, media(text/html,_)) :-
  http_pagination_header(Page),
  atom_capitalize(Key, CKey),
  html_page(hdt(Page,[CKey]), [],
            [\html_pagination_result(Page, term_id_table(G))]).

term_id_table(G, Ids) -->
  html(ul(\html_maplist(term_id_row(G), Ids))).

term_id_row(G, Id) -->
  {
    (rdf_equal(graph:default, G) -> Query = [] ; Query = [graph(G)]),
    http_link_to_id(triple_id_handler, [subject(Id)|Query], UriS),
    http_link_to_id(triple_id_handler, [predicate(Id)|Query], UriP),
    http_link_to_id(triple_id_handler, [object(Id)|Query], UriO)
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



% /triple
triple_handler(Request) :-
  rest_method(Request, triple_method(Request)).

% /triple: GET,HEAD
triple_method(Request, Method, MediaTypes) :-
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
  rest_media_type(MediaTypes, triple_media_type(G, Page)).

term_arg_(X, X) :-
  var(X), !.
term_arg_(Atom, Term) :-
  rdf_atom_to_term(Atom, Term).

% /triple: GET,HEAD: application/n-triples
triple_media_type(_, Page, media(application/'n-triples',_)) :-
  format("Content-Type: application/n-triples\n"),
  http_pagination_header(Page),
  nl,
  write_ntuples(Page.results).
% /triple: GET,HEAD: text/html
triple_media_type(G, Page, media(text/html,_)) :-
  http_pagination_header(Page),
  rdf_global_id(graph:GLocal, G),
  html_page(hdt(Page,["Triples",GLocal]), [],
            [\html_pagination_result(Page, rdf_html_triple_table(Page.uri, G))]).



% /triple/count
triple_count_handler(Request) :-
  rest_method(Request, triple_count_method(Request)).

% /triple/count: GET,HEAD
triple_count_method(Request, Method, MediaTypes) :-
  http_is_get(Method),
  http_parameters(
    Request,
    [graph(G),object(O1),predicate(P1),subject(S1)],
    [attribute_declarations(http_param)]
  ),
  (hdt_graph(Hdt, G) -> true ; existence_error(hdt_graph, G)),
  maplist(term_arg_, [S1,P1,O1], [S2,P2,O2]),
  hdt_search_cost(Hdt, S2, P2, O2, Cost),
  rest_media_type(MediaTypes, triple_count_media_type(Cost)).

% /triple/count: GET,HEAD: application/json
triple_count_media_type(Cost, media(application/json,_)) :-
  http_reply_json(Cost).
% /triple/count: GET,HEAD: text/html
triple_count_media_type(Cost, media(text/html,_)) :-
  html_page(hdt(_,["Triples","Count"]), [], [\html_thousands(Cost)]).



% /triple/id
triple_id_handler(Request) :-
  rest_method(Request, triple_id_method(Request)).

% /triple/id: GET,HEAD
triple_id_method(Request, Method, MediaTypes) :-
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
  rest_media_type(MediaTypes, triple_id_media_type(G, Page)).

id_arg_(X, X) :-
  var(X), !.
id_arg_(Atom, N) :-
  atom_number(Atom, N),
  must_be(positive_integer, N).

% /triple/id: GET,HEAD: GML
triple_id_media_type(_, Page, media(application/gml,_)) :-
  format("Content-Type: application/gml\n"),
  http_pagination_header(Page),
  nl,
  write_gml_triples(Page.results).
% /triple/id: GET,HEAD: application/json
triple_id_media_type(_, Page, media(application/json,_)) :-
  http_pagination_json(Page).
% /triple/id: GET,HEAD: text/html
triple_id_media_type(G, Page, media(text/html,_)) :-
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





% HTTP MEDIA TYPES %

http_media_types(hdt_handler, [media(text/html,[])]).
http_media_types(doc_handler, [media(text/html,[])]).
http_media_types(graph_handler, [media(application/json,[]),
                                 media(text/html,[])]).
http_media_types(object_handler, [media(application/json,[]),
                                  media(text/html,[])]).
http_media_types(object_count_handler, [media(application/json,[]),
                                        media(text/html,[])]).
http_media_types(object_id_handler, [media(application/json,[]),
                                     media(text/html,[])]).
http_media_types(node_handler, [media(application/json,[]),
                                media(text/html,[])]).
http_media_types(node_count_handler, [media(application/json,[]),
                                      media(text/html,[])]).
http_media_types(node_id_handler, [media(application/json,[]),
                                   media(text/html,[])]).
http_media_types(predicate_handler, [media(application/json,[]),
                                     media(text/html,[])]).
http_media_types(predicate_count_handler, [media(application/json,[]),
                                           media(text/html,[])]).
http_media_types(predicate_id_handler, [media(application/json,[]),
                                        media(text/html,[])]).
http_media_types(shared_handler, [media(application/json,[]),
                                  media(text/html,[])]).
http_media_types(shared_count_handler, [media(application/json,[]),
                                        media(text/html,[])]).
http_media_types(shared_id_handler, [media(application/json,[]),
                                     media(text/html,[])]).
http_media_types(subject_handler, [media(application/json,[]),
                                   media(text/html,[])]).
http_media_types(subject_count_handler, [media(application/json,[]),
                                         media(text/html,[])]).
http_media_types(subject_id_handler, [media(application/json,[]),
                                      media(text/html,[])]).
http_media_types(triple_handler, [media(application/'n-triples',[]),
                                  media(application/'n-quads',[]),
                                  media(text/html,[])]).
http_media_types(triple_count_handler, [media(application/json,[]),
                                        media(text/html,[])]).
http_media_types(triple_id_handler, [media(application/json,[]),
                                     media(text/html,[]),
                                     media(text/'vnd.graphviz',[])]).





% HTTP PARAMETERS %

http_param(count, [
  boolean,
  default(false),
  description("Return the number of results.")
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

http_params(hdt_handler, []).
http_params(doc_handler, []).
http_params(graph_handler, [page,page_size]).
http_params(node_handler, [graph,page,page_size,prefix,rnd]).
http_params(node_count_handler, [graph]).
http_params(node_id_handler, [graph,page,page_size,prefix,rnd]).
http_params(object_handler, [graph,page,page_size,prefix,rnd]).
http_params(object_count_handler, [graph]).
http_params(object_id_handler, [graph,page,page_size,prefix,rnd]).
http_params(predicate_handler, [graph,page,page_size,prefix,rnd]).
http_params(predicate_count_handler, [graph]).
http_params(predicate_id_handler, [graph,page,page_size,prefix,rnd]).
http_params(shared_handler, [graph,page,page_size,prefix,rnd]).
http_params(shared_count_handler, [graph]).
http_params(shared_id_handler, [graph,page,page_size,prefix,rnd]).
http_params(subject_handler, [graph,page,page_size,prefix,rnd]).
http_params(subject_count_handler, [graph]).
http_params(subject_id_handler, [graph,page,page_size,prefix,rnd]).
http_params(triple_handler, [graph,object,page,page_size,predicate,subject]).
http_params(triple_count_handler, [graph,object,predicate,subject]).
http_params(triple_id_handler, [graph,object,page,page_size,pedicate,subject]).





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
/* ToC https://github.com/afeld/bootstrap-toc
  html(
    body(
      ['data-spy'("scroll"),'data-target'('#toc')],
      [
        link(
          [
            href='https://cdn.rawgit.com/afeld/bootstrap-toc/v0.4.1/dist/bootstrap-toc.min.css',
            rel=stylesheet
          ],
          []
        ),
        script(
          src='https://cdn.rawgit.com/afeld/bootstrap-toc/v0.4.1/dist/bootstrap-toc.min.js',
          []
        ),
        \navbar(\brand, \menu, \krr),
        nav(['data-toggle'(toc),id(toc)], [])
      | Content_0
      ]
    )
  ).
*/

brand -->
  html("HDT-Server").

krr -->
  {http_absolute_location(img('krr.svg'), Image)},
  html(a(href='http://krr.cs.vu.nl', img(src=Image, []))).
