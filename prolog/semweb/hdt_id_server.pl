:- http_handler(root(node/id), node_id_handler, [methods([get,head,options])]).
:- http_handler(root(object/id), object_id_handler, [methods([get,head,options])]).
:- http_handler(root(predicate/id), predicate_id_handler, [methods([get,head,options])]).
:- http_handler(root(shared/id), shared_id_handler, [methods([get,head,options])]).
:- http_handler(root(sink/id), sink_id_handler, [methods([get,head,options])]).
:- http_handler(root(source/id), source_id_handler, [methods([get,head,options])]).
:- http_handler(root(subject/id), subject_id_handler, [methods([get,head,options])]).
:- http_handler(root(term/id), term_id_handler, [methods([get,head,options])]).
:- http_handler(root(triple/id), triple_id_handler, [methods([get,head,options])]).

http:param(id, [
  positive_integer,
  description("Check for the presence of this particular HDT ID."),
  optional(true)
]).

html_doc:custom_param_type(Spec) -->
  {memberchk(hdt_subject, Spec)}, !,
  html("HDT subject term").
html_doc:custom_param_type(Spec) -->
  {memberchk(hdt_predicate, Spec)}, !,
  html("HDT predicate term").
html_doc:custom_param_type(Spec) -->
  {memberchk(hdt_object, Spec)}, !,
  html("HDT object term").

http:convert_parameter(hdt_object, Atom, O) :-
  (   atom_number(Atom, N)
  ->  must_be(positive_integer, N),
      O = id(object,N)
  ;   rdf_atom_term(Atom, O)
  ).
http:convert_parameter(hdt_predicate, Atom, P) :-
  (   atom_number(Atom, N)
  ->  must_be(positive_integer, N),
      P = id(predicate,N)
  ;   rdf_atom_term(Atom, P)
  ).
http:convert_parameter(hdt_subject, Atom, S) :-
  (   atom_number(Atom, N)
  ->  must_be(positive_integer, N),
      S = id(subject,N)
  ;   rdf_atom_term(Atom, S)
  ).

html:menu_item(term_id, "Term IDs").
  html:menu_item(term_id, node_id_handler, "Node IDs").
  html:menu_item(term_id, object_id_handler, "Object IDs").
  html:menu_item(term_id, predicate_id_handler, "Predicate IDs").
  html:menu_item(term_id, shared_id_handler, "Shared IDs").
  html:menu_item(term_id, sink_id_handler, "Sink IDs").
  html:menu_item(term_id, source_id_handler, "Source IDs").
  html:menu_item(term_id, subject_id_handler, "Subject IDs").
html:menu_item(triple_id_handler, "Triple IDs").

http:media_types(object_id_handler, [media(application/json,[]),
                                     media(text/html,[])]).
http:media_types(node_id_handler, [media(application/json,[]),
                                   media(text/html,[])]).
http:media_types(predicate_id_handler, [media(application/json,[]),
                                        media(text/html,[])]).
http:media_types(shared_id_handler, [media(application/json,[]),
                                     media(text/html,[])]).
http:media_types(sink_id_handler, [media(application/json,[]),
                                   media(text/html,[])]).
http:media_types(source_id_handler, [media(application/json,[]),
                                     media(text/html,[])]).
http:media_types(subject_id_handler, [media(application/json,[]),
                                      media(text/html,[])]).
http:media_types(term_id_handler, [media(application/json,[]),
                                   media(text/html,[])]).
http:media_types(triple_id_handler, [media(application/json,[]),
                                     media(text/html,[])]).

http:params(node_id_handler, [g,graph,page,page_size,prefix,random]).
http:params(object_id_handler, [g,graph,page,page_size,prefix,random]).
http:params(predicate_id_handler, [g,graph,page,page_size,prefix,random]).
http:params(shared_id_handler, [g,graph,page,page_size,prefix,random]).
http:params(sink_id_handler, [g,graph,page,page_size,prefix,random]).
http:params(source_id_handler, [g,graph,page,page_size,prefix,random]).
http:params(subject_id_handler, [g,graph,page,page_size,prefix,random]).
http:params(term_id_handler, [g,graph,page,page_size,prefix,random]).
http:params(triple_id_handler, [g,graph,o,object,page,page_size,p,predicate,s,subject]).

      %\http_doc_handler(hdt_server, node_id_handler),
      %\http_doc_handler(hdt_server, object_id_handler),
      %\http_doc_handler(hdt_server, predicate_id_handler),
      %\http_doc_handler(hdt_server, shared_id_handler),
      %\http_doc_handler(hdt_server, sink_id_handler),
      %\http_doc_handler(hdt_server, source_id_handler),
      %\http_doc_handler(hdt_server, subject_id_handler),
      %\http_doc_handler(hdt_server, term_id_handler),
      %\http_doc_handler(hdt_server, triple_id_handler)

% /node/id
node_id_handler(Request) :-
  term_id_handler_(Request, node).

% /object/id
object_id_handler(Request) :-
  term_id_handler_(Request, object).

% /predicate/id
predicate_id_handler(Request) :-
  term_id_handler_(Request, predicate).

% /shared/id
shared_id_handler(Request) :-
  term_id_handler_(Request, shared).

% /sink/id
sink_id_handler(Request) :-
  term_id_handler_(Request, sink).

% /source/id
source_id_handler(Request) :-
  term_id_handler_(Request, source).

% /subject/id
subject_id_handler(Request) :-
  term_id_handler_(Request, subject).



% /term/id
term_id_handler(Request) :-
  term_id_handler_(Request, term).

term_id_handler_(Request, TermRole) :-
  rest_method(Request, term_id_method(Request, TermRole)).

% /term/id: GET,HEAD
term_id_method(Request, TermRole, Method, MediaTypes) :-
  http_is_get(Method),
  rest_parameters(
    Request,
    [
      g(G), graph(G),
      page(PageNumber),
      page_size(PageSize),
      prefix(Prefix),
      random(Random),
      term(Term)
    ]
  ),
  (   PageNumber > 1,
      Random == true
  ->  throw(error(conflicting_http_parameters([page_number,random])))
  ;   PageNumber > 1,
      ground(Term)
  ->  throw(error(conflicting_http_parameters([page_number,term])))
  ;   ground(Prefix),
      Random == true
  ->  throw(error(conflicting_http_parameters([prefix,random])))
  ;   Random == true,
      ground(Term)
  ->  throw(error(conflicting_http_parameters([random,term])))
  ;   true
  ),
  http_parameter_conflict(id(N), term(Term)),
  http_parameter_conflict(prefix(Prefix), term(Term)),
  memberchk(request_uri(RelUri), Request),
  http_absolute_uri(RelUri, Uri),
  Options = _{
    graph: G,
    page_number: PageNumber,
    page_size: PageSize,
    uri: Uri
  },
  hdt_graph_(Hdt, G),
  (   Random == true
  ->  RandomOptions = Options.put(_{single_page: true}),
      pagination(
        Id,
        (
          hdt_term_random(Hdt, TermRole, Term),
          hdt_term_id(Hdt, TermRole, Term, Id)
        ),
        RandomOptions,
        Page
      )
  ;   atom(Prefix)
  ->  pagination(
        Id,
        (
          term_role_triple_role(TermRole, TripleRole),
          hdt_term_prefix(Hdt, TripleRole, Prefix, LeafRole, Term),
          hdt_term_id(Hdt, LeafRole, Term, Id)
        ),
        Options,
        Page
      )
  ;   ground(Term)
  ->  pagination_bulk(
        Id,
        hdt_term_id(Hdt, TermRole, Term, Id),
        Options,
        Page
      )
  ;   ground(N)
  ->  Id = id(_,N),
      pagination_bulk(
        Id,
        hdt_term_id(Hdt, TermRole, Term, Id),
        Options,
        Page
      )
  ;   pagination(
        Id,
        (
          hdt_term(Hdt, TermRole, Term),
          hdt_term_id(Hdt, TermRole, Term, Id)
        ),
        hdt_term_count(Hdt, TermRole),
        Options,
        Page
      )
  ),
  rest_media_type(MediaTypes, term_id_media_type(Uri, TripleRole, G, Page)).

% /term/id: GET,HEAD: application/json
term_id_media_type(_, _, _, Page, media(application/json,_)) :-
  http_pagination_json(Page).
% /term/id: GET,HEAD: text/html
term_id_media_type(Uri, TripleRole, G, Page, media(text/html,_)) :-
  http_pagination_header(Page),
  atom_capitalize(TripleRole, RoleLabel),
  html_page(
    page(Page,[RoleLabel],G),
    [],
    [\html_pagination_result(Page, html_term_id_table(Uri, G))]
  ).

html_term_id_table(Uri, G, Ids) -->
  {uri_decode(Uri, DecodeUri)},
  html([
    a(href=DecodeUri, "[decode]"),
    ul(\html_maplist(html_term_id_row(G), Ids))
  ]).

html_term_id_row(G, id(TripleRole,Id)) -->
  {rdf_http_query([g(G)], Query)},
  html(
    li([
      Id,
      " 〈",
      \html_term_id_subject_link(TripleRole, [s(Id)|Query]),
      ", ",
      \html_term_id_predicate_link(TripleRole, [p(Id)|Query]),
      ", ",
      \html_term_id_object_link(TripleRole, [o(Id)|Query]),
      "〉"
    ])
  ).

html_term_id_subject_link(TripleRole, Query) -->
  {role_subrole(subject, TripleRole)}, !,
  {http_link_to_id(triple_id_handler, Query, Uri)},
  html(a(href=Uri, "s")).
html_term_id_subject_link(_, _) -->
  html("s").

html_term_id_predicate_link(TripleRole, Query) -->
  {role_subrole(predicate, TripleRole)}, !,
  {http_link_to_id(triple_id_handler, Query, Uri)},
  html(a(href=Uri, "p")).
html_term_id_predicate_link(_, _) -->
  html("p").

html_term_id_object_link(TripleRole, Query) -->
  {role_subrole(object, TripleRole)}, !,
  {http_link_to_id(triple_id_handler, Query, Uri)},
  html(a(href=Uri, "o")).
html_term_id_object_link(_, _) -->
  html("o").



% /triple/id
triple_id_handler(Request) :-
  rest_method(Request, triple_id_method(Request)).

% /triple/id: GET,HEAD
triple_id_method(Request, Method, MediaTypes) :-
  http_is_get(Method),
  rest_parameters(
    Request,
    [
      g(G), graph(G),
      o(O), object(O),
      page(PageNumber),
      page_size(PageSize),
      p(P), predicate(P),
      random(Random),
      s(S), subject(S)
    ]
  ),
  (   PageNumber > 1,
      Random == true
  ->  throw(error(conflicting_http_parameters([page_number,random])))
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
        IdTriple,
        (
          hdt_triple_random(Hdt, S, P, O),
          hdt_triple_id(Hdt, rdf(S,P,O), IdTriple)
        ),
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
    \html_table(
      \html_table_header_row(["Subject","Predicate","Object"]),
      \html_maplist(html_triple_id_row(Uri, G), Triples)
    )
  ]).

html_triple_id_row(Uri, G, rdf(id(STripleRole,SId),id(PTripleRole,PId),id(OTripleRole,OId))) -->
  {
    rdf_http_query([g(G)], Query1),
    Query2 = [id(true)|Query1],
    maplist(
      id_query_,
      [id(STripleRole,SId),id(PTripleRole,PId),id(OTripleRole,OId)],
      [SH,PH,OH]
    ),
    maplist(
      uri_comp_set(query, Uri),
      [[SH|Query2],[PH|Query2],[OH|Query2]],
      [SUri,PUri,OUri]
    )
  },
  html(
    tr([
      td(a(href=SUri, SId)),
      td(a(href=PUri, PId)),
      td(a(href=OUri, OId))
    ])
  ).

id_query_(id(TripleRole,Id), Query) :-
  Query =.. [TripleRole,Id].



%! uri_decode(+Uri:atom, -DecodeUri:atom) is det.

uri_decode(Uri1, Uri2) :-
  uri_comps(Uri1, uri(Scheme,Authority,[Segment,id],Query,_)),
  uri_comps(Uri2, uri(Scheme,Authority,[Segment],Query,_)).



%! uri_encode(+Uri:atom, -EncodeUri:atom) is det.

uri_encode(Uri1, Uri2) :-
  uri_comps(Uri1, uri(Scheme,Authority,[Segment],Query,_)),
  uri_comps(Uri2, uri(Scheme,Authority,[Segment,id],Query,_)).



  {uri_encode(Uri, EncodeUri)},
    a(href=EncodeUri, "[encode]"),
