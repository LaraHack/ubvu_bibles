:- module(convert_ubvu_bibles,
	  [ convert_ubvu_bibles/0
	  ]).

%TODO
%

% Location of data files

user:file_search_path(data, .).

% Get core libraries

:- use_module(library(semweb/rdf_db)).		% Core RDF
:- use_module(library(semweb/rdf_library)).	% Load RDF from library
:- use_module(library(semweb/rdf_turtle_write)).% Save results
:- use_module(library(semweb/rdf_cache)).	% Cache control
:- use_module(library(semweb/rdf_persistency)).	% Persistency control
:- use_module(library(xmlrdf/rdf_convert_util)).

% Configure the environment:
%
%  - Make prefixes (namespaces) known
%  - Enable caching (speedup reloading large RDF files)
%  - Disable persistency for our rewrite graph(s).

:- rdf_register_ns(ore, 'http://www.openarchives.org/ore/terms/').
:- rdf_register_ns(edm, 'http://www.europeana.eu/schemas/edm/').
:- rdf_register_ns(ubvu, 'http://purl.org/collections/nl/ubvu/').
:- rdf_set_cache_options([ global_directory('cache/rdf'),
			   create_global_directory(true)
			 ]).
:- rdf_persistency(ubvu_prints, false).

% Load_ontologies:
%
%	Loads ontologies that we need.  The set below comes from the
%	ClioPatria library.ee

load_ontologies :-
    rdf_load_library(dc),
    rdf_load_library(skos),
    rdf_load_library(rdfs),
    rdf_load_library(owl).

% We always need the ontologies, and thus we load them with the program
:- initialization
    load_ontologies.

load_prints_tsv(PrintList) :-
    csv_read_file('individual_prints.tsv', Rows),
    skip_row(Rows, Rows1),
    read_prints(Rows1, PrintList).

skip_row([_Row|Rows],Rows).

read_prints([], []).
read_prints([Row|Rows], [PrintDict|PrintList]) :-
    get_info_row(Row, Title, Id, ShownBy, HasView),
    PrintDict = print{title:Title, id:Id, shown_by: ShownBy, has_view: HasView},
    read_prints(Rows, PrintList).



%%	get_info_row(+Row, -Title, -Id)
%
%	Get the info needed from row.
get_info_row(row(Title, Id, _, _, _, ShownBy, HasView, _, _), Title, Id, ShownBy, HasView).

%%	assert(+ListObjectDicts)
%
%	Assert the info from a list of object dictionaries to the rdf
%	store, following the main classes of the EDM.
assert_objects([]).
assert_objects([ObjectDict|ObjectList]) :-
    construct_uris(ObjectDict, AggregationUri, ObjectUri),
    assert_object(ObjectDict, AggregationUri, ObjectUri),
    assert_web_resource(ObjectDict, AggregationUri),
    assert_aggregation_data(ObjectDict, AggregationUri),
    assert_objects(ObjectList).

%%	construct_iris(+ObjectDict, -AggregationIri, -ObjectIri)
%
%	Construct Iris for the aggregation and for the object.
construct_uris(ObjectDict, AggregationUri, ObjectUri) :-
    ID = ObjectDict.id,
    downcase_atom(ID, LowerID),
    literal_to_id(['aggregation-', LowerID], ubvu, AggregationUri),
    %Now asserting 'print' iris, should be 'object'?
    literal_to_id(['print-', LowerID], ubvu, ObjectUri).

%%	assert_object(+ObjectDict, +AggregationIri, +ObjectIri)
%
%	Assert the information related to the provided cultural heritage
%	object into the rdf database.
assert_object(_ObjectDict, AggregationUri, ObjectUri) :-
    rdf_assert(AggregationUri, edm:aggregatedCHO, ObjectUri, ubvu_prints),
    rdf_assert(ObjectUri, rdf:type, edm:'ProvidedCHO',  ubvu_prints),
   % Creator = ObjectDict.creator,
   % assert_object_creator(Creator, ObjectUri),
   % Type = ObjectDict.type,
   % assert_object_type(Type, ObjectUri),
    assert_object_type(print, ObjectUri), % check for aat resource of print
    debug(ubvu, 'Asserting object: ~p', [ObjectUri]).

%%	assert_web_resource(+ObjectDict, +AggregationIri)
%
%	Assert resources available on the web representing the object.
assert_web_resource(ObjectDict, AggregationUri) :-
    WebResource = ObjectDict.shown_by,
    rdf_assert(AggregationUri, edm:isShownBy, WebResource, ubvu_prints),
    rdf_assert(WebResource, rdf:type, edm:'WebResource', ubvu_prints),
    HasView0 = ObjectDict.has_view,
    atom_concat('http://imagebase.ubvu.vu.nl/utils/getfile/collection/bis/id/', HasView0, HasView),
    rdf_assert(AggregationUri, edm:hasView, HasView, ubvu_prints),
    rdf_assert(WebResource, rdf:type, edm:'WebResource', ubvu_prints),

    debug(ubvu, 'Asserting view: ~p', [WebResource]).

%%	assert_aggregation_data(+ObjectDict, +AggregationIri)
%
%	Assert the aggregation connecting the object and the web
%	resources.
assert_aggregation_data(_ObjectDict, AggregationUri) :-
    Source = 'UBVU',
    rdf_assert(AggregationUri, edm:dataProvider, literal(Source), ubvu_prints),
    rdf_assert(AggregationUri, rdf:type, ore:'Aggregation', ubvu_prints),
    debug(ubvu, 'Asserting source: ~p', [Source]).

% TODO: Hardcoded the ulan id, should do a lookup instead.
assert_object_creator('Koekkoek, M.A.', ObjectUri) :-
    rdf_assert(ObjectUri, dc:creator, literal('Koekkoek, M.A.'), ubvu_prints),
    rdf_assert(ObjectUri, dcterms:creator, ulan:'500020715', ubvu_prints),
    rdf_assert(ulan:'500020715', rdf:type, edm:'Agent', ubvu_prints),
    !.

assert_object_creator(Creator, ObjectUri) :-
    rdf_assert(ObjectUri, dc:creator, literal(Creator), ubvu_prints).


assert_object_type(drawing, ObjectUri) :-
    rdf_assert(ObjectUri, dc:format, literal('drawings (visual works)'), ubvu_prints),
    rdf_assert(ObjectUri, dcterms:format, aat:'300033973', ubvu_prints),
    !.

assert_object_type(Type, ObjectUri) :-
    rdf_assert(ObjectUri, dc:format, literal(Type), ubvu_prints).

%%	save_birds
%
%	Save the result relative to the =data= search path.
save_prints :-
    absolute_file_name(data('../../git/ubvu_bibles/rdf/ubvu_bibles.ttl'),
		       File, [access(write)]),
    rdf_save_turtle(File, [graph(ubvu_prints)]).

%%	convert_naturalis_zangvogels
%
%	Perform the entire conversion:
%
%	  1. Load the data.
%	  2. Add RDF statements.
%	  3. Save the result.
convert_ubvu_bibles :-
    load_prints_tsv(ObjectList),
    assert_objects(ObjectList),
    save_prints.
