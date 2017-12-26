:- module(hdt_server, []).

/** <module> HDT server

@author Wouter Beek
@version 2017/05-2017/12
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(atom_ext)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(dict_ext)).
:- use_module(library(error)).
:- use_module(library(html/html_doc)).
:- use_module(library(html/html_ext)).
:- use_module(library(html/html_pagination)).
:- use_module(library(html/rdf_html)).
:- use_module(library(http/http_pagination)).
:- use_module(library(http/http_server)).
:- use_module(library(media_type)).
:- use_module(library(pagination)).
:- use_module(library(semweb/hdt_api)).
:- use_module(library(semweb/hdt_graph)).
:- use_module(library(semweb/rdf_api)).
:- use_module(library(semweb/rdf_db), [rdf_save/1]).
:- use_module(library(semweb/rdf_export)).
:- use_module(library(semweb/turtle)).
:- use_module(library(settings)).
:- use_module(library(uri/uri_ext)).
:- use_module(library(yall)).

:- dynamic
    html:handler_description/2,
    html:menu_item/2,
    html:menu_item/3,
    http:media_types/2,
    http:param/2,
    http:params/2.

:- http_handler(/, home_handler,
                [methods([get,head,options])]).
:- http_handler(root(doc), doc_handler,
                [methods([get,head,options])]).
:- http_handler(root(node), node_handler,
                [methods([get,head,options])]).
:- http_handler(root(node/count), node_count_handler,
                [methods([get,head,options])]).
:- http_handler(root(node/id), node_id_handler,
                [methods([get,head,options])]).
:- http_handler(root(object), object_handler,
                [methods([get,head,options])]).
:- http_handler(root(object/count), object_count_handler,
                [methods([get,head,options])]).
:- http_handler(root(object/id), object_id_handler,
                [methods([get,head,options])]).
:- http_handler(root(predicate), predicate_handler,
                [methods([get,head,options])]).
:- http_handler(root(predicate/count), predicate_count_handler,
                [methods([get,head,options])]).
:- http_handler(root(predicate/id), predicate_id_handler,
                [methods([get,head,options])]).
:- http_handler(root(shared), shared_handler,
                [methods([get,head,options])]).
:- http_handler(root(shared/count), shared_count_handler,
                [methods([get,head,options])]).
:- http_handler(root(shared/id), shared_id_handler,
                [methods([get,head,options])]).
:- http_handler(root(sink), sink_handler,
                [methods([get,head,options])]).
:- http_handler(root(sink/count), sink_count_handler,
                [methods([get,head,options])]).
:- http_handler(root(sink/id), sink_id_handler,
                [methods([get,head,options])]).
:- http_handler(root(source), source_handler,
                [methods([get,head,options])]).
:- http_handler(root(source/count), source_count_handler,
                [methods([get,head,options])]).
:- http_handler(root(source/id), source_id_handler,
                [methods([get,head,options])]).
:- http_handler(root(subject), subject_handler,
                [methods([get,head,options])]).
:- http_handler(root(subject/count), subject_count_handler,
                [methods([get,head,options])]).
:- http_handler(root(subject/id), subject_id_handler,
                [methods([get,head,options])]).
:- http_handler(root(triple), triple_handler,
                [methods([get,head,options])]).
:- http_handler(root(triple/count), triple_count_handler,
                [methods([get,head,options])]).
:- http_handler(root(triple/id), triple_id_handler,
                [methods([get,head,options])]).

:- multifile
    html:handler_description/2,
    html:menu_item/2,
    html:menu_item/3,
    html:rest_exception/1,
    html_doc:custom_param_type//3,
    http:status_page/3,
    user:body//2,
    user:head//2.

html:handler_description(doc_handler, "Documentation").
html:handler_description(node_handler, "Nodes").
html:handler_description(object_handler, "Objects").
html:handler_description(predicate_handler, "Predicates").
html:handler_description(shared_handler, "Shared nodes").
html:handler_description(sink_handler, "Sinks").
html:handler_description(source_handler, "Sources").
html:handler_description(subject_handler, "Subjects").
html:handler_description(triple_handler, "Triples").

html:menu_item(doc_handler, "Documentation").
html:menu_item(term, "Terms").
  html:menu_item(term, node_handler, "Nodes").
  html:menu_item(term, object_handler, "Objects").
  html:menu_item(term, predicate_handler, "Predicates").
  html:menu_item(term, shared_handler, "Shared nodes").
  html:menu_item(term, sink_handler, "Sinks").
  html:menu_item(term, source_handler, "Sources").
  html:menu_item(term, subject_handler, "Subjects").
%html:menu_item(term_id, "Term IDs").
%  html:menu_item(term_id, node_id_handler, "Node IDs").
%  html:menu_item(term_id, object_id_handler, "Object IDs").
%  html:menu_item(term_id, predicate_id_handler, "Predicate IDs").
%  html:menu_item(term_id, shared_id_handler, "Shared IDs").
%  html:menu_item(term_id, sink_id_handler, "Sink IDs").
%  html:menu_item(term_id, source_id_handler, "Source IDs").
%  html:menu_item(term_id, subject_id_handler, "Subject IDs").
html:menu_item(triple_handler, "Triples").
%html:menu_item(triple_id_handler, "Triple IDs").

http:media_types(home_handler, [media(text/html,[])]).
http:media_types(doc_handler, [media(text/html,[])]).
http:media_types(graph_handler, [media(application/json,[]),
                                 media(text/html,[])]).
http:media_types(object_handler, [media(application/json,[]),
                                  media(text/html,[])]).
http:media_types(object_count_handler, [media(application/json,[]),
                                        media(text/html,[])]).
http:media_types(object_id_handler, [media(application/json,[]),
                                     media(text/html,[])]).
http:media_types(node_handler, [media(application/json,[]),
                                media(text/html,[])]).
http:media_types(node_count_handler, [media(application/json,[]),
                                      media(text/html,[])]).
http:media_types(node_id_handler, [media(application/json,[]),
                                   media(text/html,[])]).
http:media_types(predicate_handler, [media(application/json,[]),
                                     media(text/html,[])]).
http:media_types(predicate_count_handler, [media(application/json,[]),
                                           media(text/html,[])]).
http:media_types(predicate_id_handler, [media(application/json,[]),
                                        media(text/html,[])]).
http:media_types(shared_handler, [media(application/json,[]),
                                  media(text/html,[])]).
http:media_types(shared_count_handler, [media(application/json,[]),
                                        media(text/html,[])]).
http:media_types(shared_id_handler, [media(application/json,[]),
                                     media(text/html,[])]).
http:media_types(sink_handler, [media(application/json,[]),
                                media(text/html,[])]).
http:media_types(sink_count_handler, [media(application/json,[]),
                                      media(text/html,[])]).
http:media_types(sink_id_handler, [media(application/json,[]),
                                   media(text/html,[])]).
http:media_types(source_handler, [media(application/json,[]),
                                  media(text/html,[])]).
http:media_types(source_count_handler, [media(application/json,[]),
                                        media(text/html,[])]).
http:media_types(source_id_handler, [media(application/json,[]),
                                     media(text/html,[])]).
http:media_types(subject_handler, [media(application/json,[]),
                                   media(text/html,[])]).
http:media_types(subject_count_handler, [media(application/json,[]),
                                         media(text/html,[])]).
http:media_types(subject_id_handler, [media(application/json,[]),
                                      media(text/html,[])]).
http:media_types(triple_handler, [media(application/'n-triples',[]),
                                  media(application/'n-quads',[]),
                                  media(application/'rdf/xml',[]),
                                  %media(application/trig,[]),
                                  media(text/html,[]),
                                  media(text/turtle,[])]).
http:media_types(triple_count_handler, [media(application/json,[]),
                                        media(text/html,[])]).
http:media_types(triple_id_handler, [media(application/json,[]),
                                     media(text/html,[])]).

http:param(count, [
  boolean,
  default(false),
  description("Return the number of results.")
]).
http:param(g, Options) :-
  http:param(graph, Options).
http:param(graph, [
  atom,
  default(G),
  description("The named graph from which results are retrieved.  When absent, results are retrieved from the default graph.")
]) :-
  hdt_default_graph(G).
http:param(id, [
  boolean,
  default(false),
  description("Return HDT identifiers i.o. RDF terms.")
]).
http:param(o, Options) :-
  http:param(object, Options).
http:param(object, [
  atom,
  description("Filter results with this object term or identifier."),
  optional(true)
]).
http:param(p, Options) :-
  http:param(predicate, Options).
http:param(predicate, [
  atom,
  description("Filter results with this predicate term or identifier."),
  optional(true)
]).
http:param(prefix, [
  atom,
  description("Filter for terms that have this prefix."),
  optional(true)
]).
http:param(random, [
  boolean,
  default(false),
  description("Retrieve a randomly chosen result.  Default is `false'.")
]).
http:param(s, Options) :-
  http:param(subject, Options).
http:param(subject, [
  atom,
  description("Filter results with this subject term or identifier."),
  optional(true)
]).

http:params(home_handler, [page,page_size]).
http:params(doc_handler, []).
http:params(graph_handler, [page,page_size]).
http:params(node_handler, [g,graph,page,page_size,prefix,random]).
http:params(node_count_handler, [g,graph]).
http:params(node_id_handler, [g,graph,page,page_size,prefix,random]).
http:params(object_handler, [g,graph,page,page_size,prefix,random]).
http:params(object_count_handler, [g,graph]).
http:params(object_id_handler, [g,graph,page,page_size,prefix,random]).
http:params(predicate_handler, [g,graph,page,page_size,prefix,random]).
http:params(predicate_count_handler, [g,graph]).
http:params(predicate_id_handler, [g,graph,page,page_size,prefix,random]).
http:params(shared_handler, [g,graph,page,page_size,prefix,random]).
http:params(shared_count_handler, [g,graph]).
http:params(shared_id_handler, [g,graph,page,page_size,prefix,random]).
http:params(sink_handler, [g,graph,page,page_size,prefix,random]).
http:params(sink_count_handler, [g,graph]).
http:params(sink_id_handler, [g,graph,page,page_size,prefix,random]).
http:params(source_handler, [g,graph,page,page_size,prefix,random]).
http:params(source_count_handler, [g,graph]).
http:params(source_id_handler, [g,graph,page,page_size,prefix,random]).
http:params(subject_handler, [g,graph,page,page_size,prefix,random]).
http:params(subject_count_handler, [g,graph]).
http:params(subject_id_handler, [g,graph,page,page_size,prefix,random]).
http:params(triple_handler, [g,graph,o,object,page,page_size,p,predicate,s,subject]).
http:params(triple_count_handler, [g,graph,o,object,p,predicate,s,subject]).
http:params(triple_id_handler, [g,graph,o,object,page,page_size,p,predicate,s,subject]).

http:status_page(not_found(Uri), _, Dom) :-
  phrase(
    page(
      page(_,["Path Not Found",Uri],_),
      [],
      [
        h1(["Path Not Found: ",code(Uri)]),
        p(a(href='/',"Return to root"))
      ]
    ),
    Dom
  ).

:- set_setting(http:products, ["HDT-Server"-"v0.0.5"]).





% ROOT %

% /
home_handler(Request) :-
  rest_method(Request, home_method(Request)).

% /: GET,HEAD
home_method(Request, Method, MediaTypes) :-
  http_is_get(Method),
  http_parameters(
    Request,
    [graph(G,[optional(true)]),page(PageNumber),page_size(PageSize)],
    [attribute_declarations(http:param)]
  ),
  memberchk(request_uri(RelUri), Request),
  http_absolute_uri(RelUri, Uri),
  (   var(G)
  ->  pagination_bulk(
        [Gs]>>aggregate_all(set(G0), hdt_graph(G0), Gs),
        _{page_number: PageNumber, page_size: PageSize, uri: Uri},
        Page
      ),
      rest_media_type(MediaTypes, home_media_type(Page))
  ;   rest_media_type(MediaTypes, graph_media_type(G))
  ).

% /: GET,HEAD: text/html
graph_media_type(G, media(text/html,_)) :-
  html_page(
    page(_,["Graph"],G),
    [],
    [
      \table(
        \table_header_row(["Property","Value"]),
        \graph_rows(G)
      )
    ]
  ).

graph_rows(G) -->
  {
    hdt_graph(Hdt, G),
    http_link_to_id(node_handler, [graph(G)], NodesUri),
    hdt_term_count(Hdt, node, Nodes),
    http_link_to_id(object_handler, [graph(G)], ObjectsUri),
    hdt_term_count(Hdt, object, Objects),
    http_link_to_id(predicate_handler, [graph(G)], PredicatesUri),
    hdt_term_count(Hdt, predicate, Predicates),
    http_link_to_id(shared_handler, [graph(G)], SharedUri),
    hdt_term_count(Hdt, shared, Shared),
    http_link_to_id(sink_handler, [graph(G)], SinksUri),
    hdt_term_count(Hdt, sink, Sinks),
    http_link_to_id(source_handler, [graph(G)], SourcesUri),
    hdt_term_count(Hdt, source, Sources),
    http_link_to_id(subject_handler, [graph(G)], SubjectsUri),
    hdt_term_count(Hdt, subject, Subjects),
    http_link_to_id(triple_handler, [graph(G)], TriplesUri),
    hdt_triple_count(Hdt, _, _, _, Triples)
  },
  html([
    tr([td("Nodes"),td(a(href=NodesUri,\html_thousands(Nodes)))]),
    tr([td("Objects"),td(a(href=ObjectsUri,\html_thousands(Objects)))]),
    tr([td("Predicates"),td(a(href=PredicatesUri,\html_thousands(Predicates)))]),
    tr([td("Shared"),td(a(href=SharedUri,\html_thousands(Shared)))]),
    tr([td("Sinks"),td(a(href=SinksUri,\html_thousands(Sinks)))]),
    tr([td("Sources"),td(a(href=SourcesUri,\html_thousands(Sources)))]),
    tr([td("Subjects"),td(a(href=SubjectsUri,\html_thousands(Subjects)))]),
    tr([td("Triples"),td(a(href=TriplesUri,\html_thousands(Triples)))])
  ]).

% /graph: GET,HEAD: application/json
home_media_type(Page, media(application/json,_)) :-
  http_pagination_json(Page).
% /graph: GET,HEAD: text/html
home_media_type(Page, media(text/html,_)) :-
  http_pagination_header(Page),
  html_page(
    page(_,["Graph","Overview"],_),
    [],
    [\html_pagination_result(Page, graphs_table)]
  ).

graphs_table(Gs) -->
  table(
    \table_header_row(["Graph","Triples","Modified","Source"]),
    \html_maplist(graph_row, Gs)
  ).

graph_row(G) -->
  {
    hdt_graph(Hdt, G),
    % name
    http_link_to_id(home_handler, [graph(G)], GraphUri),
    % number of triples
    http_link_to_id(triple_handler, [graph(G)], TriplesUri),
    hdt_triple_count(Hdt, _, _, _, NumTriples),
    % TBD: modified
    once(hdt:hdt_triple_(Hdt, header, 0, _, '<http://purl.org/dc/terms/issued>', Modified)),
    % TBD: source
    once(hdt:hdt_triple_(Hdt, header, 0, Source, _, _))
  },
  html(
    tr([
      td(a(href=GraphUri,code(G))),
      td(a(href=TriplesUri,\html_thousands(NumTriples))),
      td(Modified),
      td(Source)
    ])
  ).





% DOCUMENTATION %

% /doc
doc_handler(Request) :-
  rest_method(Request, doc_method).

% /doc: GET,HEAD
doc_method(Method, MediaTypes) :-
  http_is_get(Method),
  rest_media_type(MediaTypes, doc_media_type).

% /doc: GET,HEAD: text/html
doc_media_type(media(text/html,_)) :-
  html_page(
    page(_,["Documentation"],_),
    [],
    [
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
      \http_doc_handler(hdt_server, sink_handler),
      \http_doc_handler(hdt_server, sink_count_handler),
      \http_doc_handler(hdt_server, sink_id_handler),
      \http_doc_handler(hdt_server, source_handler),
      \http_doc_handler(hdt_server, source_count_handler),
      \http_doc_handler(hdt_server, source_id_handler),
      \http_doc_handler(hdt_server, subject_handler),
      \http_doc_handler(hdt_server, subject_count_handler),
      \http_doc_handler(hdt_server, subject_id_handler),
      \http_doc_handler(hdt_server, triple_handler),
      \http_doc_handler(hdt_server, triple_count_handler),
      \http_doc_handler(hdt_server, triple_id_handler)
    ]
  ).





% TERMS %

% /node
node_handler(Request) :-
  term_handler(Request, node).
% /node/count
node_count_handler(Request) :-
  term_count_handler(Request, node).
% /node/id
node_id_handler(Request) :-
  term_id_handler(Request, node).

% /object
object_handler(Request) :-
  term_handler(Request, object).
% /object/count
object_count_handler(Request) :-
  term_count_handler(Request, object).
% /object/id
object_id_handler(Request) :-
  term_id_handler(Request, object).

% /predicate
predicate_handler(Request) :-
  term_handler(Request, predicate).
% /predicate/count
predicate_count_handler(Request) :-
  term_count_handler(Request, predicate).
% /predicate/id
predicate_id_handler(Request) :-
  term_id_handler(Request, predicate).

% /shared
shared_handler(Request) :-
  term_handler(Request, shared).
% /shared/count
shared_count_handler(Request) :-
  term_count_handler(Request, shared).
% /shared/id
shared_id_handler(Request) :-
  term_id_handler(Request, shared).

% /sink
sink_handler(Request) :-
  term_handler(Request, sink).
% /sink/count
sink_count_handler(Request) :-
  term_count_handler(Request, sink).
% /sink/id
sink_id_handler(Request) :-
  term_id_handler(Request, sink).

% /source
source_handler(Request) :-
  term_handler(Request, source).
% /source/count
source_count_handler(Request) :-
  term_count_handler(Request, source).
% /source/id
source_id_handler(Request) :-
  term_id_handler(Request, source).

% /subject
subject_handler(Request) :-
  term_handler(Request, subject).
% /subject/count
subject_count_handler(Request) :-
  term_count_handler(Request, subject).
% /subject/id
subject_id_handler(Request) :-
  term_id_handler(Request, subject).



% /term
term_handler(Request, Role) :-
  rest_method(Request, term_method(Request, Role)).

% /term: GET,HEAD
term_method(Request, Role, Method, MediaTypes) :-
  http_is_get(Method),
  http_parameters(
    Request,
    [
      g(G2),
      graph(G1),
      page(PageNumber),
      page_size(PageSize),
      prefix(Prefix),
      random(Random)
    ],
    [attribute_declarations(http:param)]
  ),
  random_page_number(Random, PageNumber),
  memberchk(request_uri(RelUri), Request),
  http_absolute_uri(RelUri, Uri),
  alt_atom_(G1, G2, G),
  hdt_graph(Hdt, G),
  Options = _{
    graph: G,
    page_number: PageNumber,
    page_size: PageSize,
    uri: Uri
  },
  (   Random == true
  ->  RandomOptions = Options.put(_{single_page: true}),
      pagination(
        Term,
        hdt_term_random_(Hdt, Role, Term),
        RandomOptions,
        Page
      )
  ;   atom(Prefix)
  ->  pagination(
        Term,
        hdt_term_prefix(Hdt, Role, Prefix, Term),
        Options,
        Page
      )
  ;   pagination(
        Term,
        hdt_term(Hdt, Role, Term),
        hdt_term_count(Hdt, Role),
        Options,
        Page
      )
  ),
  rest_media_type(MediaTypes, term_media_type(Hdt, Uri, Role, G, Page)).

% /term: GET,HEAD: application/json
term_media_type(_, _, _, _, Page, media(application/json,_)) :-
  http_pagination_json(Page).
% /term: GET,HEAD: text/html
term_media_type(Hdt, Uri, Role, G, Page, media(text/html,_)) :-
  http_pagination_header(Page),
  atom_capitalize(Role, CRole),
  html_page(
    page(Page,[CRole],G),
    [],
    [\html_pagination_result(Page, html_term_table(Hdt, Uri, G))]
  ).

html_term_table(Hdt, Uri, G, Terms) -->
  {uri_encode(Uri, EncodeUri)},
  html([
    a(href=EncodeUri, "[encode]"),
    ul(\html_maplist(html_term_row(Hdt, G), Terms))
  ]).

html_term_row(Hdt, G, Term) -->
  {
    (hdt_default_graph(G) -> T = [] ; T = [graph(G)]),
    rdf_term_to_atom(Term, Atom)
  },
  html(
    li([
      \rdf_html_term(Term, _{format: ntuples}),
      " 〈",
      \html_term_subject_link(Hdt, Term, Atom, T),
      ", ",
      \html_term_predicate_link(Hdt, Term, Atom, T),
      ", ",
      \html_term_object_link(Hdt, Term, Atom, T),
      "〉"
    ])
  ).

html_term_object_link(Hdt, Term, Atom, T) -->
  {hdt_triple(Hdt, _, _, Term)}, !,
  {http_link_to_id(triple_handler, [object(Atom)|T], Uri)},
  html(a(href=Uri, "o")).
html_term_object_link(_, _, _, _) -->
  html("o").

html_term_predicate_link(Hdt, Term, Atom, T) -->
  {hdt_triple(Hdt, _, Term, _)}, !,
  {http_link_to_id(triple_handler, [predicate(Atom)|T], Uri)},
  html(a(href=Uri, "o")).
html_term_predicate_link(_, _, _, _) -->
  html("p").

html_term_subject_link(Hdt, Term, Atom, T) -->
  {hdt_triple(Hdt, Term, _, _)}, !,
  {http_link_to_id(triple_handler, [subject(Atom)|T], Uri)},
  html(a(href=Uri, "s")).
html_term_subject_link(_, _, _, _) -->
  html("s").



% /term/count
term_count_handler(Request, Role) :-
  rest_method(Request, term_count_method(Request, Role)).

% /term/count: GET,HEAD
term_count_method(Request, Role, Method, MediaTypes) :-
  http_is_get(Method),
  http_parameters(
    Request,
    [g(G2),graph(G1)],
    [attribute_declarations(http:param)]
  ),
  alt_atom_(G1, G2, G),
  hdt_graph(Hdt, G),
  hdt_term_count(Hdt, Role, Count),
  rest_media_type(MediaTypes, term_count_media_type(G, Role, Count)).

% /term/count: GET,HEAD: application/json
term_count_media_type(_, _, Count, media(application/json,_)) :-
  http_reply_json(Count).
% /term/count: GET,HEAD: text/html
term_count_media_type(G, Role, Count, media(text/html,_)) :-
  atom_capitalize(Role, CRole),
  html_page(page(_,["Terms",CRole],G), [], [\html_thousands(Count)]).



% /term/id
term_id_handler(Request, Role) :-
  rest_method(Request, term_id_method(Request, Role)).

% /term/id: GET,HEAD
term_id_method(Request, Role, Method, MediaTypes) :-
  http_is_get(Method),
  http_parameters(
    Request,
    [
      g(G2),
      graph(G1),
      page(PageNumber),
      page_size(PageSize),
      prefix(Prefix),
      random(Random)
    ],
    [attribute_declarations(http:param)]
  ),
  random_page_number(Random, PageNumber),
  memberchk(request_uri(RelUri), Request),
  http_absolute_uri(RelUri, Uri),
  alt_atom_(G1, G2, G),
  hdt_graph(Hdt, G),
  Options = _{
    graph: G,
    page_number: PageNumber,
    page_size: PageSize,
    uri: Uri
  },
  (   Random == true
  ->  RandomOptions = Options.put(_{single_page: true}),
      pagination(
        Id,
        hdt_term_random_id(Hdt, Role, Id),
        RandomOptions,
        Page
      )
  ;   atom(Prefix)
  ->  pagination(
        Id,
        hdt_term_prefix_id(Hdt, Role, Prefix, Id),
        Options,
        Page
      )
  ;   pagination(
        Id,
        hdt_term_id_(Hdt, Role, Id),
        hdt_term_count(Hdt, Role),
        Options,
        Page
      )
  ),
  rest_media_type(MediaTypes, term_id_media_type(Uri, Role, G, Page)).

% /term/id: GET,HEAD: application/json
term_id_media_type(_, _, _, Page, media(application/json,_)) :-
  http_pagination_json(Page).
% /term/id: GET,HEAD: text/html
term_id_media_type(Uri, Role, G, Page, media(text/html,_)) :-
  http_pagination_header(Page),
  atom_capitalize(Role, CRole),
  html_page(
    page(Page,[CRole],G),
    [],
    [\html_pagination_result(Page, html_term_id_table(Uri, G))]
  ).

html_term_id_table(Uri, G, Ids) -->
  {uri_decode(Uri, DecodeUri)},
  html([
    a(href=DecodeUri, "[decode]"),
    ul(\html_maplist(html_term_id_row(G), Ids))
  ]).

html_term_id_row(G, id(Role,Id)) -->
  {(hdt_default_graph(G) -> T = [] ; T = [graph(G)])},
  html(
    li([
      Id,
      " 〈",
      \html_term_id_subject_link(Role, Id, T),
      ", ",
      \html_term_id_predicate_link(Role, Id, T),
      ", ",
      \html_term_id_object_link(Role, Id, T),
      "〉"
    ])
  ).

html_term_id_subject_link(Role, Id, T) -->
  {role_subrole(subject, Role)}, !,
  {http_link_to_id(triple_id_handler, [subject(Id)|T], Uri)},
  html(a(href=Uri, "s")).
html_term_id_subject_link(_, _, _) -->
  html("s").

html_term_id_predicate_link(Role, Id, T) -->
  {role_subrole(predicate, Role)}, !,
  {http_link_to_id(triple_id_handler, [predicate(Id)|T], Uri)},
  html(a(href=Uri, "p")).
html_term_id_predicate_link(_, _, _) -->
  html("p").

html_term_id_object_link(Role, Id, T) -->
  {role_subrole(object, Role)}, !,
  {http_link_to_id(triple_id_handler, [object(Id)|T], Uri)},
  html(a(href=Uri, "o")).
html_term_id_object_link(_, _, _) -->
  html("o").





% TRIPLES %

% /triple
triple_handler(Request) :-
  rest_method(Request, triple_method(Request)).

% /triple: GET,HEAD
triple_method(Request, Method, MediaTypes) :-
  http_is_get(Method),
  http_parameters(
    Request,
    [
      g(G2),
      graph(G1),
      o(OAtom2),
      object(OAtom1),
      page(PageNumber),
      page_size(PageSize),
      p(PAtom2),
      predicate(PAtom1),
      random(Random),
      s(SAtom2),
      subject(SAtom1)
    ],
    [attribute_declarations(http:param)]
  ),
  random_page_number(Random, PageNumber),
  memberchk(request_uri(RelUri), Request),
  http_absolute_uri(RelUri, Uri),
  maplist(
    alt_atom_,
    [SAtom1,PAtom1,OAtom1,G1],
    [SAtom2,PAtom2,OAtom2,G2],
    [SAtom,PAtom,OAtom,G]
  ),
  include(ground, [subject(SAtom),predicate(PAtom),object(OAtom)], T),
  hdt_graph(Hdt, G),
  maplist(
    arg_to_term_(Hdt),
    [subject,predicate,object],
    [SAtom,PAtom,OAtom],
    [S,P,O]
  ),
  Options = _{
    page_number: PageNumber,
    page_size: PageSize,
    query: [graph(G)|T],
    uri: Uri
  },
  (   Random == true
  ->  RandomOptions = Options.put(_{single_page: true}),
      pagination(
        rdf(S,P,O),
        hdt_triple_random_(Hdt, S, P, O),
        RandomOptions,
        Page
      )
  ;   Offset is (PageNumber - 1) * PageSize,
      findall(
        rdf(S,P,O),
        limit(PageSize, hdt_triple(Hdt, Offset, S, P, O)),
        Results
      ),
      length(Results, NumResults),
      hdt_triple_count(Hdt, S, P, O, TotalNumResults),
      merge_dicts(
        _{
          number_of_results: NumResults,
          results: Results,
          single_page: false,
          total_number_of_results: TotalNumResults
        },
        Options,
        Page
      )
  ),
  rest_media_type(MediaTypes, triple_media_type(Uri, G, Page)).

% /triple: GET,HEAD: application/n-quads
triple_media_type(_, _, Page, media(application/'n-quads',_)) :-
  format("Content-Type: application/n-quads\n"),
  http_pagination_header(Page),
  nl,
  maplist(rdf_write_tuple(current_output), Page.results).
% /triple: GET,HEAD: application/n-triples
triple_media_type(_, _, Page, media(application/'n-triples',_)) :-
  format("Content-Type: application/n-triples\n"),
  http_pagination_header(Page),
  nl,
  maplist(rdf_write_triple(current_output), Page.results).
% /triple: GET,HEAD: text/html
triple_media_type(Uri, G, Page, media(text/html,_)) :-
  http_pagination_header(Page),
  uri_encode(Uri, EncodeUri),
  html_page(
    page(Page,["Triples"],G),
    [],
    [
      a(href=EncodeUri, "[encode]"),
      \html_pagination_result(
        Page,
        [Triples]>>rdf_html_triple_table(
          Page.uri,
          G,
          Triples,
          _{format: ntuples}
        )
      )
    ]
  ).
% /triple: GET,HEAD: application/rdf+xml, application/trig, text/turtle
triple_media_type(_, _, Page, MediaType) :-
  rdf_media_type_(MediaType),
  atom_phrase(media_type(MediaType), Atom),
  format("Content-Type: ~a\n\n", [Atom]),
  rdf_transaction((
    rdf_retractall(_, _, _, _),
    maplist(rdf_assert, Page.results),
    uuid(File),
    (   MediaType = media(application/'rdf+xml',_)
    ->  rdf_save(File)
    %;   MediaType = media(application/trig,_)
    %->  rdf_save_canonical_trig(File, [])
    ;   MediaType = media(text/turtle,_)
    ->  rdf_save_canonical_turtle(File, [])
    ),
    setup_call_cleanup(
      open(File, read, In),
      copy_stream_data(In, current_output),
      close(In)
    ),
    delete_file(File)
  )).

rdf_media_type_(media(application/'rdf+xml',_)).
%rdf_media_type_(media(application/trig,_)).
rdf_media_type_(media(text/turtle,_)).



% /triple/count
triple_count_handler(Request) :-
  rest_method(Request, triple_count_method(Request)).

% /triple/count: GET,HEAD
triple_count_method(Request, Method, MediaTypes) :-
  http_is_get(Method),
  http_parameters(
    Request,
    [
      g(G2),
      graph(G1),
      o(OAtom2),
      object(OAtom1),
      p(PAtom2),
      predicate(PAtom1),
      s(SAtom2),
      subject(SAtom1)
    ],
    [attribute_declarations(http:param)]
  ),
  maplist(
    alt_atom_,
    [SAtom1,PAtom1,OAtom1,G1],
    [SAtom2,PAtom2,OAtom2,G2],
    [SAtom,PAtom,OAtom,G]
  ),
  hdt_graph(Hdt, G),
  maplist(
    arg_to_term_(Hdt),
    [subject,predicate,object],
    [SAtom,PAtom,OAtom],
    [S,P,O]
  ),
  hdt_triple_count(Hdt, S, P, O, Count),
  rest_media_type(MediaTypes, triple_count_media_type(G, Count)).

% /triple/count: GET,HEAD: application/json
triple_count_media_type(_, Count, media(application/json,_)) :-
  http_reply_json(Count).
% /triple/count: GET,HEAD: text/html
triple_count_media_type(G, Count, media(text/html,_)) :-
  html_page(page(_,["Triples","Count"],G), [], [\html_thousands(Count)]).



% /triple/id
triple_id_handler(Request) :-
  rest_method(Request, triple_id_method(Request)).

% /triple/id: GET,HEAD
triple_id_method(Request, Method, MediaTypes) :-
  http_is_get(Method),
  http_parameters(
    Request,
    [
      g(G2),
      graph(G1),
      o(OAtom2),
      object(OAtom1),
      page(PageNumber),
      page_size(PageSize),
      p(PAtom2),
      predicate(PAtom1),
      random(Random),
      s(SAtom2),
      subject(SAtom1)
    ],
    [attribute_declarations(http:param)]
  ),
  random_page_number(Random, PageNumber),
  memberchk(request_uri(RelUri), Request),
  http_absolute_uri(RelUri, Uri),
  maplist(
    alt_atom_,
    [SAtom1,PAtom1,OAtom1,G1],
    [SAtom2,PAtom2,OAtom2,G2],
    [SAtom,PAtom,OAtom,G]
  ),
  include(ground, [subject(SAtom),predicate(PAtom),object(OAtom)], T),
  hdt_graph(Hdt, G),
  maplist(
    arg_to_term_(Hdt),
    [subject,predicate,object],
    [SAtom,PAtom,OAtom],
    [S,P,O]
  ),
  Options = _{
    page_number: PageNumber,
    page_size: PageSize,
    query: [graph(G)|T],
    uri: Uri
  },
  (   Random == true
  ->  RandomOptions = Options.put(_{single_page: true}),
      pagination(
        IdTriple,
        hdt_triple_random_id(Hdt, S, P, O, IdTriple),
        RandomOptions,
        Page
      )
  ;   Offset is (PageNumber - 1) * PageSize,
      findall(
        IdTriple,
        limit(PageSize, (
          hdt_triple(Hdt, Offset, S, P, O),
          hdt_triple_id(Hdt, rdf(S,P,O), IdTriple)
        )),
        Results
      ),
      length(Results, NumResults),
      hdt_triple_count(Hdt, S, P, O, TotalNumResults),
      merge_dicts(
        _{
          number_of_results: NumResults,
          results: Results,
          single_page: false,
          total_number_of_results: TotalNumResults
        },
        Options,
        Page
      )
  ),
  rest_media_type(MediaTypes, triple_id_media_type(G, Page)).

% /triple/id: GET,HEAD: application/json
triple_id_media_type(_, Page, media(application/json,_)) :-
  http_pagination_json(Page).
% /triple/id: GET,HEAD: text/html
triple_id_media_type(G, Page, media(text/html,_)) :-
  http_pagination_header(Page),
  html_page(
    page(Page,["Triples","Identifiers"],G),
    [],
    [\html_pagination_result(Page, html_triple_id_table(Page.uri, G))]
  ).

html_triple_id_table(Uri, G, Triples) -->
  {uri_decode(Uri, DecodeUri)},
  html([
    a(href=DecodeUri, "[decode]"),
    \table(
      \table_header_row(["Subject","Predicate","Object"]),
      \html_maplist(html_triple_id_row(Uri, G), Triples)
    )
  ]).

html_triple_id_row(Uri, G, rdf(id(SRole,SId),id(PRole,PId),id(ORole,OId))) -->
  {
    (var(G) -> T = [id(true)] ; T = [graph(G),id(true)]),
    maplist(
      id_query_,
      [id(SRole,SId),id(PRole,PId),id(ORole,OId)],
      [SH,PH,OH]
    ),
    maplist(uri_comp_set(query, Uri), [[SH|T],[PH|T],[OH|T]], [SUri,PUri,OUri])
  },
  html(
    tr([
      td(a(href=SUri, SId)),
      td(a(href=PUri, PId)),
      td(a(href=OUri, OId))
    ])
  ).

id_query_(id(Role,Id), Query) :-
  Query =.. [Role,Id].





% GENERICS %

%! alt_atom_(?Atom1:atom, ?Atom2:atom, -Atom:atom) is det.
%
% Some parameters may be set in more than one way.  If this is the
% case, then choose the primary parameter value (`Atom1') over the
% secondary parameter value (`Atom2').

alt_atom_(Atom1, _, Atom1) :-
  atom(Atom1), !.
alt_atom_(_, Atom2, Atom2) :-
  atom(Atom2), !.
alt_atom_(_, _, _).



%! arg_to_term_(+Hdt:blob, +Role:atom, +Atom:atom, -Term:rdf_term) is det.

% variable
arg_to_term_(_, _, Var, Var) :-
  var(Var), !.
% HDT ID → RDF term
arg_to_term_(Hdt, Role, Atom, Term) :-
  atom_number(Atom, Id), !,
  hdt_term_id(Hdt, Role, Term, Id).
% Turtle 1.1 notation for RDF terms
arg_to_term_(_, _, Atom, Term) :-
  rdf_atom_to_term(Atom, Term), !.
arg_to_term_(_, _, Atom, _) :-
  type_error(rdf_term, Atom).



%! hdt_term_id_(+Hdt:blob, +Role:atom, -Id:compound) is nondet.

hdt_term_id_(Hdt, Role, id(LeafRole,Id)) :-
  hdt_term(Hdt, Role, LeafRole, Term),
  hdt_term_id(Hdt, LeafRole, Term, Id).



%! hdt_term_prefix_id(+Hdt:blob, +Role:atom, +Prefix:atom,
%!                    -Id:positive_integer) is nondet.

hdt_term_prefix_id(Hdt, Role, Prefix, Id) :-
  hdt_term_prefix(Hdt, Role, Prefix, LeafRole, Term),
  hdt_term_id(Hdt, LeafRole, Term, Id).



%! hdt_term_random_(+Hdt:blob, +Role:atom, -Term:compound) is nondet.

hdt_term_random_(Hdt, Role, Term) :-
  hdt_term_random_(Hdt, Role, _, Term).


hdt_term_random_(Hdt, Role, LeafRole, Term) :-
  repeat,
  hdt_term_random(Hdt, Role, LeafRole, Term).



%! hdt_term_random_id(+Hdt:blob, +Role:atom, -Id:compound) is nondet.

hdt_term_random_id(Hdt, Role, id(LeafRole,Id)) :-
  hdt_term_random_(Hdt, Role, LeafRole, Term),
  hdt_term_id(Hdt, LeafRole, Term, Id).



%! hdt_triple_random_(+Hdt:blob, ?S, ?P, ?O) is nondet.

hdt_triple_random_(Hdt, S, P, O) :-
  repeat,
  hdt_triple_random(Hdt, S, P, O).



%! hdt_triple_random_id(+Hdt:blob, ?S, ?P, ?O, -IdTriple:compound) is nondet.

hdt_triple_random_id(Hdt, S, P, O, IdTriple) :-
  hdt_triple_random_(Hdt, S, P, O),
  hdt_triple_id(Hdt, rdf(S,P,O), IdTriple).



%! random_page_number(+Random:boolean, +PageNumber:positive_integer) is det.
%
% Throws an HTTP exception in case random values are requests beyond
% page one.  The idea behind this is that randomly generated content
% has no pages.

random_page_number(true, PageNumber) :-
  PageNumber > 1, !,
  type_error(random_page, PageNumber).
random_page_number(_, _).



%! uri_decode(+Uri:atom, -DecodeUri:atom) is det.

uri_decode(Uri1, Uri2) :-
  uri_comps(Uri1, uri(Scheme,Authority,[Segment,id],Query,_)),
  uri_comps(Uri2, uri(Scheme,Authority,[Segment],Query,_)).



%! uri_encode(+Uri:atom, -EncodeUri:atom) is det.

uri_encode(Uri1, Uri2) :-
  uri_comps(Uri1, uri(Scheme,Authority,[Segment],Query,_)),
  uri_comps(Uri2, uri(Scheme,Authority,[Segment,id],Query,_)).





% HTML STYLE %

html:rest_exception(400) :-
  html_page(
    page(_,["Error","Bad Request"],_),
    [],
    [
      p("Bad request"),
      p(a(href='/',"Return to root"))
    ]
  ).

user:head(page(Page,Subtitles,_), Content_0) -->
  {atomics_to_string(["HDT-Server"|Subtitles], " ― ", Title)},
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

user:body(page(_,_,G), Content_0) -->
  html(body([\navbar("HDT-Server", \menu, \html_graph(G)),\row_1(Content_0)])).

html_graph(G) -->
  {var(G)}, !,
  "".
html_graph(G) -->
  html(["Currently querying: ",G]).
