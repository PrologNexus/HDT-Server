:- module(hdt_server, []).

/** <module> HDT server

@author Wouter Beek
@version 2017-2018
*/

:- use_module(library(apply)).
:- use_module(library(filesex)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_log)).
:- use_module(library(http/http_path)).
:- use_module(library(semweb/turtle)).
:- use_module(library(settings)).
:- use_module(library(solution_sequences)).
:- use_module(library(uuid)).
:- use_module(library(yall)).

:- use_module(library(atom_ext)).
:- use_module(library(conf_ext)).
:- use_module(library(dict)).
:- use_module(library(html/html_doc)).
:- use_module(library(html/html_ext)).
:- use_module(library(html/html_pagination)).
:- use_module(library(html/rdf_html)).
:- use_module(library(http/http_pagination)).
:- use_module(library(http/http_resource), []).
:- use_module(library(http/http_server)).
:- use_module(library(http/rdf_http)).
:- use_module(library(pagination)).
:- use_module(library(semweb/hdt_api)).
:- use_module(library(semweb/hdt_dataset)).
:- use_module(library(semweb/rdf_export)).
:- use_module(library(semweb/rdf_mem)).
:- use_module(library(semweb/rdf_prefix)).

:- dynamic
    html:handler_description/2,
    html:menu_item/2,
    html:menu_item/3,
    http:media_types/2,
    http:params/2.

:- http_handler(/, home_handler,
                [methods([get,head,options])]).
:- http_handler(root(doc), doc_handler,
                [methods([get,head,options])]).
:- http_handler(root(node), node_handler,
                [methods([get,head,options])]).
:- http_handler(root(node/count), node_count_handler,
                [methods([get,head,options])]).
:- http_handler(root(object), object_handler,
                [methods([get,head,options])]).
:- http_handler(root(object/count), object_count_handler,
                [methods([get,head,options])]).
:- http_handler(root(predicate), predicate_handler,
                [methods([get,head,options])]).
:- http_handler(root(predicate/count), predicate_count_handler,
                [methods([get,head,options])]).
:- http_handler(root(shared), shared_handler,
                [methods([get,head,options])]).
:- http_handler(root(shared/count), shared_count_handler,
                [methods([get,head,options])]).
:- http_handler(root(sink), sink_handler,
                [methods([get,head,options])]).
:- http_handler(root(sink/count), sink_count_handler,
                [methods([get,head,options])]).
:- http_handler(root(source), source_handler,
                [methods([get,head,options])]).
:- http_handler(root(source/count), source_count_handler,
                [methods([get,head,options])]).
:- http_handler(root(subject), subject_handler,
                [methods([get,head,options])]).
:- http_handler(root(subject/count), subject_count_handler,
                [methods([get,head,options])]).
:- http_handler(root(term), term_handler,
                [methods([get,head,options])]).
:- http_handler(root(term/count), term_count_handler,
                [methods([get,head,options])]).
:- http_handler(root(triple), triple_handler,
                [methods([get,head,options])]).
:- http_handler(root(triple/count), triple_count_handler,
                [methods([get,head,options])]).

:- initialization
   init_hdt_server.

:- multifile
    html:handler_description/2,
    html:menu_item/2,
    html:menu_item/3,
    html:page_exception/2,
    html_doc:custom_param_type//1,
    http:convert_parameter/3,
    http:media_types/2,
    http:param/2,
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
html:handler_description(term_handler, "Term").
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
html:menu_item(triple_handler, "Triples").

http:media_types(home_handler, [media(text/html,[])]).
http:media_types(doc_handler, [media(text/html,[])]).
http:media_types(graph_handler, [media(application/json,[]),
                                 media(text/html,[])]).
http:media_types(object_handler, [media(application/json,[]),
                                  media(text/html,[])]).
http:media_types(object_count_handler, [media(application/json,[]),
                                        media(text/html,[])]).
http:media_types(node_handler, [media(application/json,[]),
                                media(text/html,[])]).
http:media_types(node_count_handler, [media(application/json,[]),
                                      media(text/html,[])]).
http:media_types(predicate_handler, [media(application/json,[]),
                                     media(text/html,[])]).
http:media_types(predicate_count_handler, [media(application/json,[]),
                                           media(text/html,[])]).
http:media_types(shared_handler, [media(application/json,[]),
                                  media(text/html,[])]).
http:media_types(shared_count_handler, [media(application/json,[]),
                                        media(text/html,[])]).
http:media_types(sink_handler, [media(application/json,[]),
                                media(text/html,[])]).
http:media_types(sink_count_handler, [media(application/json,[]),
                                      media(text/html,[])]).
http:media_types(source_handler, [media(application/json,[]),
                                  media(text/html,[])]).
http:media_types(source_count_handler, [media(application/json,[]),
                                        media(text/html,[])]).
http:media_types(subject_handler, [media(application/json,[]),
                                   media(text/html,[])]).
http:media_types(subject_count_handler, [media(application/json,[]),
                                         media(text/html,[])]).
http:media_types(term_handler, [media(application/json,[]),
                                media(text/html,[])]).
http:media_types(term_count_handler, [media(application/json,[]),
                                      media(text/html,[])]).
http:media_types(triple_handler, [media(application/'n-triples',[]),
                                  media(application/'n-quads',[]),
                                  media(application/'rdf+xml',[]),
                                  media(application/trig,[]),
                                  media(text/html,[]),
                                  media(text/turtle,[])]).
http:media_types(triple_count_handler, [media(application/json,[]),
                                        media(text/html,[])]).

http:param(count, [
  boolean,
  default(false),
  description("Return the number of results.")
]).
http:param(g, Options) :-
  http:param(graph, Options).
http:param(graph, [
  rdf_term,
  description("The named graph from which results are retrieved.  When absent, results are retrieved from the default graph."),
  optional(true)
]).
http:param(o, Options) :-
  http:param(object, Options).
http:param(object, [
  rdf_term,
  description("Filter results with this object term or identifier."),
  optional(true)
]).
http:param(p, Options) :-
  http:param(predicate, Options).
http:param(predicate, [
  rdf_term,
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
  rdf_term,
  description("Filter results with this subject term or identifier."),
  optional(true)
]).
http:param(term, [
  rdf_term,
  description("Check for the presence of this particular RDF term."),
  optional(true)
]).

http:params(home_handler, [page,page_size]).
http:params(doc_handler, []).
http:params(graph_handler, [page,page_size]).
http:params(node_handler, [g,graph,page,page_size,prefix,random,term]).
http:params(node_count_handler, [g,graph]).
http:params(object_handler, [g,graph,page,page_size,prefix,random,term]).
http:params(object_count_handler, [g,graph]).
http:params(predicate_handler, [g,graph,page,page_size,prefix,random,term]).
http:params(predicate_count_handler, [g,graph]).
http:params(shared_handler, [g,graph,page,page_size,prefix,random,term]).
http:params(shared_count_handler, [g,graph]).
http:params(sink_handler, [g,graph,page,page_size,prefix,random,term]).
http:params(sink_count_handler, [g,graph]).
http:params(source_handler, [g,graph,page,page_size,prefix,random,term]).
http:params(source_count_handler, [g,graph]).
http:params(subject_handler, [g,graph,page,page_size,prefix,random,term]).
http:params(subject_count_handler, [g,graph]).
http:params(term_handler, [g,graph,page,page_size,prefix,random,term]).
http:params(term_count_handler, [g,graph]).
http:params(triple_handler, [g,graph,o,object,page,page_size,p,predicate,s,subject]).
http:params(triple_count_handler, [g,graph,o,object,p,predicate,s,subject]).

:- set_setting(http:products, ["HDT-Server"-"v0.0.8"]).
:- set_setting(pagination:default_page_size, 100).
:- set_setting(pagination:maximum_page_size, 1 000).

:- setting(tmp_directory, any, _, "").





% ROOT %

% /
home_handler(Request) :-
  rest_method(Request, home_method(Request)).

% /: GET,HEAD
home_method(Request, Method, MediaTypes) :-
  http_is_get(Method),
  rest_parameters(
    Request,
    [g(G1),graph(G2),page(PageNumber),page_size(PageSize)]
  ),
  http_parameter_alternatives([g(G1),graph(G2)], G),
  memberchk(request_uri(RelUri), Request),
  http_absolute_uri(RelUri, Uri),
  (   var(G)
  ->  pagination_bulk(
        G,
        hdt_graph(_, G),
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
      \html_table(
        \html_table_header_row(["Property","Value"]),
        \graph_rows(G)
      )
    ]
  ).

graph_rows(G) -->
  {
    hdt_graph(Hdt, G),
    rdf_http_query([g(G)], Query),
    http_link_to_id(node_handler, Query, NodesUri),
    hdt_term_count(Hdt, node, Nodes),
    http_link_to_id(object_handler, Query, ObjectsUri),
    hdt_term_count(Hdt, object, Objects),
    http_link_to_id(predicate_handler, Query, PredicatesUri),
    hdt_term_count(Hdt, predicate, Predicates),
    http_link_to_id(shared_handler, Query, SharedUri),
    hdt_term_count(Hdt, shared, Shared),
    http_link_to_id(sink_handler, Query, SinksUri),
    hdt_term_count(Hdt, sink, Sinks),
    http_link_to_id(source_handler, Query, SourcesUri),
    hdt_term_count(Hdt, source, Sources),
    http_link_to_id(subject_handler, Query, SubjectsUri),
    hdt_term_count(Hdt, subject, Subjects),
    http_link_to_id(term_handler, Query, TermsUri),
    hdt_term_count(Hdt, term, Terms),
    http_link_to_id(triple_handler, Query, TriplesUri),
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
    tr([td("Terms"),td(a(href=TermsUri,\html_thousands(Terms)))]),
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
    [\html_pagination_result(Page, html_graphs_table)]
  ).

html_graphs_table(Gs) -->
  html_table(
    \html_table_header_row(["Graph","Triples","Terms","Modified"]),
    \html_maplist(graph_row, Gs)
  ).

graph_row(G) -->
  {
    hdt_graph(Hdt, G),
    rdf_http_query([g(G)], Query)
  },
  html(
    tr([
      \graph_name(G, Query),
      \number_of_triples(Hdt, Query),
      \number_of_terms(Hdt, Query),
      \last_modified_date(Hdt)
    ])
  ).

graph_name(G, Query) -->
  {http_link_to_id(home_handler, Query, Uri)},
  html(td(a(href=Uri,code(G)))).

number_of_triples(Hdt, Query) -->
  {
    hdt_triple_count(Hdt, _, _, _, N),
    http_link_to_id(triple_handler, Query, Uri)
  },
  html(td(a(href=Uri,\html_thousands(N)))).

number_of_terms(Hdt, Query) -->
  {
    hdt_term_count(Hdt, term, N),
    http_link_to_id(term_handler, Query, Uri)
  },
  html(td(a(href=Uri,\html_thousands(N)))).

last_modified_date(Hdt) -->
  {once(hdt:hdt_triple_(Hdt, header, 0, _, '<http://purl.org/dc/terms/issued>', LMod))},
  html(td(LMod)).





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
      \http_doc_handler(hdt_server, object_handler),
      \http_doc_handler(hdt_server, object_count_handler),
      \http_doc_handler(hdt_server, predicate_handler),
      \http_doc_handler(hdt_server, predicate_count_handler),
      \http_doc_handler(hdt_server, shared_handler),
      \http_doc_handler(hdt_server, shared_count_handler),
      \http_doc_handler(hdt_server, sink_handler),
      \http_doc_handler(hdt_server, sink_count_handler),
      \http_doc_handler(hdt_server, source_handler),
      \http_doc_handler(hdt_server, source_count_handler),
      \http_doc_handler(hdt_server, subject_handler),
      \http_doc_handler(hdt_server, subject_count_handler),
      \http_doc_handler(hdt_server, term_handler),
      \http_doc_handler(hdt_server, term_count_handler),
      \http_doc_handler(hdt_server, triple_handler),
      \http_doc_handler(hdt_server, triple_count_handler)
    ]
  ).





% TERMS %

% /node
node_handler(Request) :-
  term_handler_(Request, node).
% /node/count
node_count_handler(Request) :-
  term_count_handler_(Request, node).

% /object
object_handler(Request) :-
  term_handler_(Request, object).
% /object/count
object_count_handler(Request) :-
  term_count_handler_(Request, object).

% /predicate
predicate_handler(Request) :-
  term_handler_(Request, predicate).
% /predicate/count
predicate_count_handler(Request) :-
  term_count_handler_(Request, predicate).

% /shared
shared_handler(Request) :-
  term_handler_(Request, shared).
% /shared/count
shared_count_handler(Request) :-
  term_count_handler_(Request, shared).

% /sink
sink_handler(Request) :-
  term_handler_(Request, sink).
% /sink/count
sink_count_handler(Request) :-
  term_count_handler_(Request, sink).

% /source
source_handler(Request) :-
  term_handler_(Request, source).
% /source/count
source_count_handler(Request) :-
  term_count_handler_(Request, source).

% /subject
subject_handler(Request) :-
  term_handler_(Request, subject).
% /subject/count
subject_count_handler(Request) :-
  term_count_handler_(Request, subject).



% /term
term_handler(Request) :-
  term_handler_(Request, term).

term_handler_(Request, TermRole) :-
  rest_method(Request, term_method(Request, TermRole)).

% /term: GET,HEAD
term_method(Request, TermRole, Method, MediaTypes) :-
  http_is_get(Method),
  rest_parameters(
    Request,
    [
      g(G1), graph(G2),
      page(PageNumber),
      page_size(PageSize),
      prefix(Prefix),
      random(Random),
      term(Term)
    ]
  ),
  http_parameter_alternatives([g(G1),graph(G2)], G),
  (   PageNumber > 1,
      Random == true
  ->  conflicting_http_parameters([page_number,random])
  ;   PageNumber > 1,
      ground(Term)
  ->  conflicting_http_parameters([page_number,term])
  ;   ground(Prefix),
      Random == true
  ->  conflicting_http_parameters([prefix,random])
  ;   Random == true,
      ground(Term)
  ->  conflicting_http_parameters([random,term])
  ;   true
  ),
  http_parameter_conflict(prefix(Prefix), term(Term)),
  rdf_http_query([g(G)], Query1),
  (ground(Prefix) -> Query2 = [prefix(Prefix)|Query1] ; Query2 = Query1),
  memberchk(request_uri(RelUri), Request),
  http_absolute_uri(RelUri, Uri),
  Options = _{
    graph: G,
    page_number: PageNumber,
    page_size: PageSize,
    query: Query2,
    uri: Uri
  },
  hdt_graph_(Hdt, G),
  (   Random == true
  ->  RandomOptions = Options.put(_{single_page: true}),
      pagination(
        Term,
        (
          repeat,
          hdt_term_random(Hdt, TermRole, Term)
        ),
        RandomOptions,
        Page
      )
  ;   atom(Prefix)
  ->  pagination(
        Term,
        hdt_term_prefix(Hdt, TermRole, Prefix, Term),
        Options,
        Page
      )
  ;   ground(Term)
  ->  Page = Options.put(_{number_of_results: 1, results: [Term]})
  ;   pagination(
        Term,
        hdt_term(Hdt, TermRole, Term),
        hdt_term_count(Hdt, TermRole),
        Options,
        Page
      )
  ),
  rest_media_type(MediaTypes, term_media_type(Hdt, TermRole, G, Page)).

% /term: GET,HEAD: application/json
term_media_type(_, _, _, Page, media(application/json,_)) :-
  http_pagination_json(Page).
% /term: GET,HEAD: text/html
term_media_type(Hdt, TermRole, G, Page, media(text/html,_)) :-
  http_pagination_header(Page),
  atom_capitalize(TermRole, RoleLabel),
  html_page(
    page(Page,[RoleLabel],G),
    [],
    [\html_pagination_result(Page, html_term_table(Hdt, G))]
  ).

html_term_table(Hdt, G, Terms) -->
  html(ul(\html_maplist(html_term_row(G, Hdt), Terms))).

html_term_row(G, Hdt, Term) -->
  html(
    li([
      \rdf_html_term(Term, _{format: ntuples}),
      " 〈",
      \html_term_subject_link(Hdt, Term, G),
      ", ",
      \html_term_predicate_link(Hdt, Term, G),
      ", ",
      \html_term_object_link(Hdt, Term, G),
      "〉"
    ])
  ).

html_term_subject_link(Hdt, S, G) -->
  {
    hdt_triple(Hdt, S, _, _), !,
    rdf_http_query([g(G),s(S)], Query),
    http_link_to_id(triple_handler, Query, Uri)
  },
  html(a(href=Uri, "s")).
html_term_subject_link(_, _, _) -->
  html("s").

html_term_predicate_link(Hdt, P, G) -->
  {
    hdt_triple(Hdt, _, P, _), !,
    rdf_http_query([g(G),p(P)], Query),
    http_link_to_id(triple_handler, Query, Uri)
  },
  html(a(href=Uri, "p")).
html_term_predicate_link(_, _, _) -->
  html("p").

html_term_object_link(Hdt, O, G) -->
  {
    hdt_triple(Hdt, _, _, O), !,
    rdf_http_query([g(G),o(O)], Query),
    http_link_to_id(triple_handler, Query, Uri)
  },
  html(a(href=Uri, "o")).
html_term_object_link(_, _, _) -->
  html("o").



% /term/count
term_count_handler(Request) :-
  term_count_handler_(Request, term).

term_count_handler_(Request, TermRole) :-
  rest_method(Request, term_count_method(Request, TermRole)).

% /term/count: GET,HEAD
term_count_method(Request, TermRole, Method, MediaTypes) :-
  http_is_get(Method),
  rest_parameters(Request, [g(G1),graph(G2)]),
  http_parameter_alternatives([g(G1),graph(G2)], G),
  hdt_graph_(Hdt, G),
  hdt_term_count(Hdt, TermRole, Count),
  rest_media_type(MediaTypes, term_count_media_type(G, TermRole, Count)).

% /term/count: GET,HEAD: application/json
term_count_media_type(_, _, Count, media(application/json,_)) :-
  http_reply_json(Count).
% /term/count: GET,HEAD: text/html
term_count_media_type(G, TermRole, Count, media(text/html,_)) :-
  atom_capitalize(TermRole, RoleLabel),
  html_page(page(_,["Terms",RoleLabel],G), [], [\html_thousands(Count)]).





% TRIPLES %

% /triple
triple_handler(Request) :-
  rest_method(Request, triple_method(Request)).

% /triple: GET,HEAD
triple_method(Request, Method, MediaTypes) :-
  http_is_get(Method),
  rest_parameters(
    Request,
    [
      g(G1), graph(G2),
      o(O1), object(O2),
      page(PageNumber),
      page_size(PageSize),
      p(P1), predicate(P2),
      random(Random),
      s(S1), subject(S2)
    ]
  ),
  http_parameter_alternatives([g(G1),graph(G2)], G),
  http_parameter_alternatives([o(O1),object(O2)], O),
  http_parameter_alternatives([p(P1),predicate(P2)], P),
  http_parameter_alternatives([s(S1),subject(S2)], S),
  (   PageNumber > 1,
      Random == true
  ->  conflicting_http_parameters([page_number,random])
  ;   true
  ),
  hdt_graph_(Hdt, G),
  rdf_http_query([s(S),p(P),o(O),g(G)], Query),
  memberchk(request_uri(RelUri), Request),
  http_absolute_uri(RelUri, Uri),
  Options = _{
    page_number: PageNumber,
    page_size: PageSize,
    query: Query,
    uri: Uri
  },
  (   Random == true
  ->  RandomOptions = Options.put(_{single_page: true}),
      pagination(
        rdf(S,P,O),
        (
          repeat,
          hdt_triple_random(Hdt, S, P, O)
        ),
        RandomOptions,
        Page
      )
  ;   % We do not use pagination/5 because we must use Offset in
      % Goal_0.
      Offset is (PageNumber - 1) * PageSize,
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
  rest_media_type(MediaTypes, triple_media_type(G, Page)).

% /triple: GET,HEAD: application/n-quads
triple_media_type(G, Page, media(application/'n-quads',_)) :-
  format("Content-Type: application/n-quads\n"),
  http_pagination_header(Page),
  nl,
  maplist({G}/[Triple]>>rdf_write_quad(current_output, Triple, G), Page.results).
% /triple: GET,HEAD: application/n-triples
triple_media_type(_, Page, media(application/'n-triples',_)) :-
  format("Content-Type: application/n-triples\n"),
  http_pagination_header(Page),
  nl,
  maplist(rdf_write_triple(current_output), Page.results).
% /triple: GET,HEAD: application/trig
triple_media_type(G, Page, media(application/trig,_)) :-
  format("Content-Type: application/trig\n"),
  http_pagination_header(Page),
  nl,
  rdf_write_iri(current_output, G),
  format(current_output, " {\n", []),
  maplist(rdf_write_triple(current_output), Page.results),
  format(current_output, "}\n", []).
% /triple: GET,HEAD: application/rdf+xml
triple_media_type(_, Page, media(application/'rdf+xml',_)) :-
  setting(tmp_directory, Dir),
  uuid(Local),
  directory_file_path(Dir, Local, File),
  format("Content-Type: application/rdf+xml; charset=utf-8\n\n"),
  rdf_transaction(
    call_cleanup(
      (
        maplist(rdf_assert_triple_(Local), Page.results),
        rdf_save_file(File, [graph(Local),media_type(media(application/'rdf+xml'))]),
        setup_call_cleanup(
          open(File, read, In),
          copy_stream_data(In, current_output),
          close(In)
        )
      ),
      delete_file(File)
    )
  ).
% /triple: GET,HEAD: text/html
triple_media_type(G, Page, media(text/html,_)) :-
  http_pagination_header(Page),
  html_page(
    page(Page,["Triples"],G),
    [],
    [
      \html_pagination_result(
        Page,
        [Triples]>>rdf_html_triple_table(Page.uri, G, Triples, _{format: ntuples})
      )
    ]
  ).
% /triple: GET,HEAD: text/turtle
triple_media_type(_, Page, media(text/turtle,_)) :-
  setting(tmp_directory, Dir),
  uuid(Local),
  directory_file_path(Dir, Local, File),
  format("Content-Type: text/turtle\n\n"),
  rdf_transaction(
    call_cleanup(
      (
        maplist(rdf_assert_triple_(Local), Page.results),
        rdf_save_file(File, [graph(Local),media_type(media(text/turtle,[]))]),
        setup_call_cleanup(
          open(File, read, In),
          copy_stream_data(In, current_output),
          close(In)
        )
      ),
      delete_file(File)
    )
  ).

rdf_assert_triple_(G, rdf(S,P,O)) :-
  rdf_assert_triple(S, P, O, G).



% /triple/count
triple_count_handler(Request) :-
  rest_method(Request, triple_count_method(Request)).

% /triple/count: GET,HEAD
triple_count_method(Request, Method, MediaTypes) :-
  http_is_get(Method),
  rest_parameters(
    Request,
    [
      g(G1), graph(G2),
      s(S1), subject(S2),
      p(P1), predicate(P2),
      o(O1), object(O2)
    ]
  ),
  http_parameter_alternatives([g(G1),graph(G2)], G),
  http_parameter_alternatives([o(O1),object(O2)], O),
  http_parameter_alternatives([p(P1),predicate(P2)], P),
  http_parameter_alternatives([s(S1),subject(S2)], S),
  hdt_graph_(Hdt, G),
  hdt_triple_count(Hdt, S, P, O, Count),
  rest_media_type(MediaTypes, triple_count_media_type(G, Count)).

% /triple/count: GET,HEAD: application/json
triple_count_media_type(_, Count, media(application/json,_)) :-
  http_reply_json(Count).
% /triple/count: GET,HEAD: text/html
triple_count_media_type(G, Count, media(text/html,_)) :-
  html_page(page(_,["Triples","Count"],G), [], [\html_thousands(Count)]).





% GENERICS %

%! hdt_graph_(-Hdt:blob, ?G:atom) is det.

hdt_graph_(Hdt, G) :-
  var(G), !,
  hdt_dataset(dataset(DGs,_)),
  member(G, DGs),
  hdt_graph(Hdt, G).
hdt_graph_(Hdt, G) :-
  (hdt_graph(Hdt, G) -> true ; existence_error(hdt_graph, G)).





% HTML STYLE %

html:page_exception(Status, Msg) :-
  % TBD: Why is the module prefix needed here?
  html_ext:html_page(
    page(_,["HTTP error",Status],_),
    [],
    [
      p(Msg),
      p(a(href='/',"Return to root"))
    ]
  ).

user:head(page(Page,Subtitles,_), Content_0) -->
  {
    setting(http:products, [Product-_|_]),
    atomics_to_string([Product|Subtitles], " ― ", Title)
  },
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

user:body(page(_,_,ExtraArgs), Content_0) -->
  {setting(http:products, [Product-_|_])},
  html(
    body([
      \navbar(Product, \menu, \extra_args(ExtraArgs)),
      \row_1(Content_0)
    ])
  ).

extra_args([G]) -->
  {ground(G)}, !,
  html_graph(G).
extra_args(_) -->
  html([]).

html_graph(G) -->
  {var(G)}, !,
  "".
html_graph(G) -->
  {
    rdf_http_query([g(G)], Query),
    http_link_to_id(home_handler, Query, Uri)
  },
  html(["Querying graph: ",a(href=Uri,code(G))]).





% INITIALIZATION %

init_hdt_server :-
  rdf_register_prefixes,
  conf_json(Conf),
  set_setting(tmp_directory, Conf.'tmp-directory'),
  directory_file_path(Conf.'tmp-directory', 'hdt_server.log', File),
  set_setting(http:logfile, File).
