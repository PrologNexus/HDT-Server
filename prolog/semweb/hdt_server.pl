:- module(hdt_server, []).
:- reexport(library(hdt_db)).

/** <module> HDT server

@author Wouter Beek
@version 2017/05-2017/10
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(atom_ext)).
:- use_module(library(conf_ext)).
:- use_module(library(error)).
:- use_module(library(hdt_db)).
:- use_module(library(html/html_doc)).
:- use_module(library(html/html_ext)).
:- use_module(library(html/html_pagination)).
:- use_module(library(html/rdf_html)).
:- use_module(library(http/http_pagination)).
:- use_module(library(http/http_server)).
:- use_module(library(pagination)).
:- use_module(library(semweb/rdf_api)).
:- use_module(library(semweb/rdf_export)).
:- use_module(library(settings)).
:- use_module(library(string_ext)).
:- use_module(library(uri/uri_ext)).

:- dynamic
    html:handler_description/2,
    html:menu_item/2,
    html:menu_item/3.

:- http_handler(/, hdt_handler,
                [methods([get,head,options])]).
:- http_handler(root(graph), graph_handler,
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

:- multifile
    html:handler_description/2,
    html:menu_item/2,
    html:menu_item/3,
    html_doc:custom_param_type//3,
    http:status_page/3,
    user:body//2,
    user:head//2.

html:handler_description(graph_handler, "Graphs").
html:handler_description(node_handler, "Nodes").
html:handler_description(object_handler, "Objects").
html:handler_description(predicate_handler, "Predicates").
html:handler_description(shared_handler, "Shared nodes").
html:handler_description(sink_handler, "Sinks").
html:handler_description(source_handler, "Sources").
html:handler_description(subject_handler, "Subjects").
html:handler_description(triple_handler, "Triples").

html:menu_item(term, "Terms").
  html:menu_item(term, graph_handler, "Graphs").
  html:menu_item(term, node_handler, "Nodes").
  html:menu_item(term, object_handler, "Objects").
  html:menu_item(term, predicate_handler, "Predicates").
  html:menu_item(term, shared_handler, "Shared nodes").
  html:menu_item(term, sink_handler, "Sinks").
  html:menu_item(term, source_handler, "Sources").
  html:menu_item(term, subject_handler, "Subjects").
html:menu_item(term_id, "Term IDs").
  html:menu_item(term_id, node_id_handler, "Node IDs").
  html:menu_item(term_id, object_id_handler, "Object IDs").
  html:menu_item(term_id, predicate_id_handler, "Predicate IDs").
  html:menu_item(term_id, shared_id_handler, "Shared IDs").
  html:menu_item(term_id, sink_id_handler, "Sink IDs").
  html:menu_item(term_id, source_id_handler, "Source IDs").
  html:menu_item(term_id, subject_id_handler, "Subject IDs").
html:menu_item(triple, "Triples").
  html:menu_item(triple, triple_handler, "Triples").
  html:menu_item(triple, triple_id_handler, "Triples IDs").

html_doc:custom_param_type(Spec) -->
  {memberchk(hdt_term, Spec)},
  html("HDT term (RDF term or positive integer)").

http:status_page(not_found(Uri), _Context, Dom) :-
  phrase(
    page(
      hdt(_,["Path Not Found",Uri]),
      [],
      [
        h1(["Path Not Found: ",code(Uri)]),
        p(a(href='/',"Return to root"))
      ]
    ),
    Dom
  ).

:- set_setting(http:products, ['HDT-Server'-'v0.0.2']).





% 404
'404_handler'(Request) :-
  rest_method(Request, '404_method').

'404_method'(Method, MediaTypes) :-
  http_is_get(Method),
  rest_media_type(MediaTypes, '404_media_type').

'404_media_type'(media(text/html,_)) :-
  html_page(
    hdt(_,[]),
    [],
    ["404"]
  ).



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
    hdt_graphs_,
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
  html_page(
    hdt(Page,["Graphs"]),
    [],
    [\html_pagination_result(Page, graph_table)]
  ).

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
      rnd(Rnd)
    ],
    [attribute_declarations(http_param)]
  ),
  memberchk(request_uri(RelUri), Request),
  http_absolute_uri(RelUri, Uri),
  alt_atom_(G1, G2, G),
  hdt_graph(Hdt, G),
  pagination(
    Term,
    hdt_term_(Hdt, Role, Prefix, Rnd, _LeafRole, Term),
    hdt_term_count_(Hdt, Role, Prefix),
    _{
      graph: G,
      page_number: PageNumber,
      page_size: PageSize,
      uri: Uri
    },
    Page
  ),
  rest_media_type(MediaTypes, term_media_type(Role, G, Page)).

% /term: GET,HEAD: application/json
term_media_type(_, _, Page, media(application/json,_)) :-
  http_pagination_json(Page).
% /term: GET,HEAD: text/html
term_media_type(Role, G, Page, media(text/html,_)) :-
  http_pagination_header(Page),
  atom_capitalize(Role, CRole),
  html_page(
    hdt(Page,[CRole]),
    [],
    [\html_pagination_result(Page, term_table(G))]
  ).

term_table(G, Terms) -->
  html(ul(\html_maplist(term_row(G), Terms))).

term_row(G, Term) -->
  {
    (rdf_equal(graph:default, G) -> T = [] ; T = [graph(G)]),
    rdf_term_to_atom(Term, Atom),
    http_link_to_id(triple_handler, [subject(Atom)|T], UriS),
    http_link_to_id(triple_handler, [predicate(Atom)|T], UriP),
    http_link_to_id(triple_handler, [object(Atom)|T], UriO)
  },
  html(
    li([
      \term_link(Term),
      " ",
      a(href=UriS, "(subject)"),
      " ",
      a(href=UriP, "(predicate)"),
      " ",
      a(href=UriO, "(object)")
    ])
  ).

term_link(Iri) -->
  {rdf_is_iri(Iri)}, !,
  html(a(href=Iri, \rdf_html_iri(Iri))).
term_link(Term) -->
  rdf_html_term(Term).



% /term/count
term_count_handler(Request, Role) :-
  rest_method(Request, term_count_method(Request, Role)).

% /term/count: GET,HEAD
term_count_method(Request, Role, Method, MediaTypes) :-
  http_is_get(Method),
  http_parameters(
    Request,
    [g(G2),graph(G1)],
    [attribute_declarations(http_param)]
  ),
  alt_atom_(G1, G2, G),
  hdt_graph(Hdt, G),
  hdt_term_count(Hdt, Role, Count),
  rest_media_type(MediaTypes, term_count_media_type(Role, Count)).

% /term/count: GET,HEAD: application/json
term_count_media_type(_, Count, media(application/json,_)) :-
  http_reply_json(Count).
% /term/count: GET,HEAD: text/html
term_count_media_type(Role, Count, media(text/html,_)) :-
  atom_capitalize(Role, CRole),
  html_page(hdt(_,["Terms",CRole]), [], [\html_thousands(Count)]).



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
      rnd(Rnd)
    ],
    [attribute_declarations(http_param)]
  ),
  memberchk(request_uri(RelUri), Request),
  http_absolute_uri(RelUri, Uri),
  alt_atom_(G1, G2, G),
  hdt_graph(Hdt, G),
  pagination(
    Id,
    hdt_term_id_(Hdt, Role, Prefix, Rnd, Id),
    hdt_term_count_(Hdt, Role, Prefix),
    _{
      graph: G,
      page_number: PageNumber,
      page_size: PageSize,
      uri: Uri
    },
    Page
  ),
  rest_media_type(MediaTypes, term_id_media_type(Role, G, Page)).

% /term/id: GET,HEAD: application/json
term_id_media_type(_, _, Page, media(application/json,_)) :-
  http_pagination_json(Page).
% /term/id: GET,HEAD: text/html
term_id_media_type(Role, G, Page, media(text/html,_)) :-
  http_pagination_header(Page),
  atom_capitalize(Role, CRole),
  html_page(
    hdt(Page,[CRole]),
    [],
    [\html_pagination_result(Page, term_id_table(G))]
  ).

term_id_table(G, Ids) -->
  html(ul(\html_maplist(term_id_row(G), Ids))).

term_id_row(G, id(Role,Id)) -->
  {
    role_triple_role(Role, TripleRole),
    H =.. [TripleRole,Id],
    (rdf_equal(graph:default, G) -> T = [] ; T = [graph(G)]),
    http_link_to_id(triple_id_handler, [H|T], Uri)
  },
  html(li([Id," ",a(href=Uri, ["(",TripleRole,")"])])).

role_triple_role(sink, object).
role_triple_role(source, subject).
role_triple_role(Role, Role).



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
      rnd(Rnd),
      s(SAtom2),
      subject(SAtom1)
    ],
    [attribute_declarations(http_param)]
  ),
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
  pagination(
    rdf(S,P,O),
    hdt_triple_(Hdt, Rnd, S, P, O),
    hdt_triple_count_(Hdt, Rnd, S, P, O),
    _{
      page_number: PageNumber,
      page_size: PageSize,
      query: [graph(G)|T],
      uri: Uri
    },
    Page
  ),
  rest_media_type(MediaTypes, triple_media_type(G, Page)).

% /triple: GET,HEAD: application/n-triples
triple_media_type(_, Page, media(application/'n-triples',_)) :-
  format("Content-Type: application/n-triples\n"),
  http_pagination_header(Page),
  nl,
  maplist(rdf_write_triple(current_output), Page.results).
% /triple: GET,HEAD: text/html
triple_media_type(G, Page, media(text/html,_)) :-
  http_pagination_header(Page),
  rdf_global_id(graph:GLocal, G),
  html_page(
    hdt(Page,["Triples",GLocal]),
    [],
    [\html_pagination_result(Page, rdf_html_triple_table(Page.uri, G))]
  ).



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
    [attribute_declarations(http_param)]
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
  rest_media_type(MediaTypes, triple_count_media_type(Count)).

% /triple/count: GET,HEAD: application/json
triple_count_media_type(Count, media(application/json,_)) :-
  http_reply_json(Count).
% /triple/count: GET,HEAD: text/html
triple_count_media_type(Count, media(text/html,_)) :-
  html_page(hdt(_,["Triples","Count"]), [], [\html_thousands(Count)]).



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
      rnd(Rnd),
      s(SAtom2),
      subject(SAtom1)
    ],
    [attribute_declarations(http_param)]
  ),
  maplist(
    alt_atom_,
    [SAtom1,PAtom1,OAtom1,G1],
    [SAtom2,PAtom2,OAtom2,G2],
    [SAtom,PAtom,OAtom,G]
  ),
  memberchk(request_uri(RelUri), Request),
  http_absolute_uri(RelUri, Uri),
  include(ground, [subject(SAtom),predicate(PAtom),object(OAtom)], T),
  hdt_graph(Hdt, G),
  maplist(
    arg_to_term_(Hdt),
    [subject,predicate,object],
    [SAtom,PAtom,OAtom],
    [S,P,O]
  ),
  pagination(
    IdTriple,
    hdt_triple_id_(Hdt, Rnd, S, P, O, IdTriple),
    hdt_triple_count_(Hdt, Rnd, S, P, O),
    _{
      page_number: PageNumber,
      page_size: PageSize,
      query: [graph(G)|T],
      uri: Uri
    },
    Page
  ),
  rest_media_type(MediaTypes, triple_id_media_type(G, Page)).

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
  html_page(
    hdt(Page,["Triples","Identifiers",GLocal]),
    [],
    [\html_pagination_result(Page, hdt_id_table(Page.uri, G))]
  ).

write_gml_triples(Triples) :-
  maplist(write_gml_triple, Triples).

write_gml_triple(rdf(SId,PId,OId)) :-
  format("edge [ label ~a source ~a target ~a ]\n", [PId,SId,OId]).

hdt_id_table(Uri, G, Triples) -->
  table(
    \table_header_row(["Subject","Predicate","Object"]),
    \html_maplist(hdt_id_table_row(Uri, G), Triples)
  ).

hdt_id_table_row(Uri, G, rdf(id(SRole,SId),id(PRole,PId),id(ORole,OId))) -->
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



%! arg_to_term_(+Hdt:blob, +Role:atom, +Atom:atom, -Term) is det.

% variable
arg_to_term_(_, _, X, X) :-
  var(X), !.
% HDT ID → RDF term
arg_to_term_(Hdt, Role, Atom, Term) :-
  atom_number(Atom, Id), !,
  hdt_term_translate(Hdt, Role, Term, Id).
% HDT atom → RDF term
arg_to_term_(_, _, Atom, Term) :-
  % Make sure this is an HDT atom.
  sub_atom(Atom, 0, 1, _, First),
  memberchk(First, ['"','<','_']), !,
  hdt_atom_to_term(Atom, Term).
% a
arg_to_term_(_, _, a, Term) :- !,
  rdf_equal(rdf:type, Term).
% Expansion of commonly used prefixes.
arg_to_term_(_, _, Atom1, Term) :-
  atomic_list_concat([Prefix,Local], :, Atom1),
  rdf_global_id(Prefix:Local, Atom2), !,
  hdt_atom_to_term(Atom2, Term).
arg_to_term_(_, _, Atom, _) :-
  throw(error(type_error(rdf_term,Atom))).



%! hdt_graphs_(-Gs:ordset(atom)) is det.

hdt_graphs_(Gs) :-
  aggregate_all(set(G), hdt_graph(G), Gs).



%! hdt_term_(+Hdt:blob, +Role:atom, +Prefix:atom, +Rnd:boolean,
%!           -LeafRole:atom, -Term:compound) is nondet.

hdt_term_(Hdt, Role, Prefix, Rnd, LeafRole, Term) :-
  (   ground(Prefix)
  ->  hdt_term_prefix(Hdt, Role, Prefix, LeafRole, Term)
  ;   Rnd == false
  ->  hdt_term(Hdt, Role, LeafRole, Term)
  ;   hdt_term_random(Hdt, Role, LeafRole, Term)
  ).



%! hdt_term_count_(+Hdt:blob, +Role:atom, +Prefix:atom, -Count:nonneg) is det.

hdt_term_count_(Hdt, Role, Prefix, Count) :-
  var(Prefix),
  hdt_term_count(Hdt, Role, Count).



%! hdt_term_id(+Hdt:blob, +Role:atom, ?Prefix:atom, +Rnd:boolean,
%!             -Id:compound) is nondet.

hdt_term_id_(Hdt, Role, Prefix, Rnd, id(LeafRole,Id)) :-
  hdt_term_(Hdt, Role, Prefix, Rnd, LeafRole, Term),
  hdt_term_translate(Hdt, LeafRole, Term, Id).



%! hdt_triple_(+Hdt:blob, +Rnd:boolean, ?S, ?P, ?O) is nondet.

hdt_triple_(Hdt, false, S, P, O) :- !,
  hdt_triple(Hdt, S, P, O).
hdt_triple_(Hdt, true, S, P, O) :-
  hdt_triple_random(Hdt, S, P, O).



%! hdt_triple_count_(+Hdt:blob, +Rnd:boolean, ?S, ?P, ?O, -N:nonneg) is nondet.

hdt_triple_count_(Hdt, false, S, P, O, N) :- !,
  hdt_triple_count(Hdt, S, P, O, N).
hdt_triple_count_(_, true, _, _, _, 1).



%! hdt_triple_id_(+Hdt:blob, +Rnd:boolean, ?S, ?P, ?O,
%!                -IdTriple:compound) is det.

hdt_triple_id_(Hdt, false, S, P, O, IdTriple) :- !,
  hdt_triple(Hdt, S, P, O),
  hdt_triple_translate(Hdt, rdf(S,P,O), IdTriple).
hdt_triple_id_(Hdt, true, S, P, O, IdTriple) :-
  hdt_triple_random(Hdt, S, P, O),
  hdt_triple_translate(Hdt, rdf(S,P,O), IdTriple).





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
http_media_types(sink_handler, [media(application/json,[]),
                                media(text/html,[])]).
http_media_types(sink_count_handler, [media(application/json,[]),
                                      media(text/html,[])]).
http_media_types(sink_id_handler, [media(application/json,[]),
                                   media(text/html,[])]).
http_media_types(source_handler, [media(application/json,[]),
                                  media(text/html,[])]).
http_media_types(source_count_handler, [media(application/json,[]),
                                        media(text/html,[])]).
http_media_types(source_id_handler, [media(application/json,[]),
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
http_param(g, Options) :-
  http_param(graph, Options).
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
http_param(o, Options) :-
  http_param(object, Options).
http_param(object, [
  atom,
  description("Filter results with this object term or identifier."),
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
http_param(p, Options) :-
  http_param(predicate, Options).
http_param(predicate, [
  atom,
  description("Filter results with this predicate term or identifier."),
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
http_param(s, Options) :-
  http_param(subject, Options).
http_param(subject, [
  atom,
  description("Filter results with this subject term or identifier."),
  optional(true)
]).

http_params(hdt_handler, []).
http_params(doc_handler, []).
http_params(graph_handler, [page,page_size]).
http_params(node_handler, [g,graph,page,page_size,prefix,rnd]).
http_params(node_count_handler, [g,graph]).
http_params(node_id_handler, [g,graph,page,page_size,prefix,rnd]).
http_params(object_handler, [g,graph,page,page_size,prefix,rnd]).
http_params(object_count_handler, [g,graph]).
http_params(object_id_handler, [g,graph,page,page_size,prefix,rnd]).
http_params(predicate_handler, [g,graph,page,page_size,prefix,rnd]).
http_params(predicate_count_handler, [g,graph]).
http_params(predicate_id_handler, [g,graph,page,page_size,prefix,rnd]).
http_params(shared_handler, [g,graph,page,page_size,prefix,rnd]).
http_params(shared_count_handler, [g,graph]).
http_params(shared_id_handler, [g,graph,page,page_size,prefix,rnd]).
http_params(sink_handler, [g,graph,page,page_size,prefix,rnd]).
http_params(sink_count_handler, [g,graph]).
http_params(sink_id_handler, [g,graph,page,page_size,prefix,rnd]).
http_params(source_handler, [g,graph,page,page_size,prefix,rnd]).
http_params(source_count_handler, [g,graph]).
http_params(source_id_handler, [g,graph,page,page_size,prefix,rnd]).
http_params(subject_handler, [g,graph,page,page_size,prefix,rnd]).
http_params(subject_count_handler, [g,graph]).
http_params(subject_id_handler, [g,graph,page,page_size,prefix,rnd]).
http_params(triple_handler, [g,graph,o,object,page,page_size,p,predicate,s,
                             subject]).
http_params(triple_count_handler, [g,graph,o,object,p,predicate,s,subject]).
http_params(triple_id_handler, [g,graph,o,object,page,page_size,p,predicate,s,
                                subject]).





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
  html(body([\navbar(\brand, \menu, "")|Content_0])).
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
        \navbar(\brand, \menu, ""),
        nav(['data-toggle'(toc),id(toc)], [])
      | Content_0
      ]
    )
  ).
*/

brand -->
  html("HDT-Server").
