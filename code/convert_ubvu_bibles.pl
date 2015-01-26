:- module(convert_ubvu_bibles,
	  [ convert_ubvu_bibles/0
	  ]).

%TODO
%

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
:- rdf_set_cache_options([ global_directory('cache/rdf'),
			   create_global_directory(true)
			 ]).
:- rdf_persistency(ubvu_bibles, false).

%%	load_ontologies
%
%	Loads ontologies that we need.  The set below comes from the
%	ClioPatria library.

load_ontologies :-
    rdf_load_library(dc),
    rdf_load_library(skos),
    rdf_load_library(rdfs),
    rdf_load_library(owl).

% We always need the ontologies, and thus we load them with the program
:- initialization
    load_ontologies.

load_bible_tsv(PrintList) :-
    csv_read_file('export_bis.tsv', Rows),
    skip_row(Rows, Rows1),
    read_prints(Rows1, PrintList).

skip_row([_Row|Rows],Rows).

read_prints([], []).
read_prints([Row|Rows], [PrintDict|PrintList]) :-
    get_info_row(Row, Id),
    PrintDict = print{id:Id},
    read_prints(Rows, PrintList).



%%	get_info_row(+Row, -NaturalisID, -ScientificName)
%
%	Get the info needed from row.
get_info_row(row(Id, _, _, _, _, _, _, _, _, _, _,  _, _, _, _, _,  _, _, _, _, _,  _, _, _, _, _,  _, _, _, _, _,  _, _, _, _, _,  _, _, _, _, _,  _, _, _, _, _,  _, _, _, _, _,  _, _, _, _, _,  _, _, _, _, _,  _, _, _, _, _,  _, _, _, _, _,  _, _, _, _, _,  _, _, _ ), Id).

%%	assert(+ListObjectDicts)
%
%	Assert the info from a list of object dictionaries to the rdf
%	store, following the main classes of the EDM.
assert_objects([]).
assert_objects([ObjectDict|ObjectList]) :-
    construct_iris(ObjectDict, AggregationIri, ObjectIri),
    assert_object(ObjectDict, AggregationIri, ObjectIri),
    assert_web_resource(ObjectDict, AggregationIri),
    assert_aggregation_data(ObjectDict, AggregationIri),
    assert_objects(ObjectList).

%%	construct_iris(+ObjectDict, -AggregationIri, -ObjectIri)
%
%	Construct Iris for the aggregation and for the object.
construct_iris(ObjectDict, AggregationIri, ObjectIri) :-
    ID = ObjectDict.id,
    downcase_atom(ID, LowerID),
    literal_to_id(['aggregation-', LowerID], nat, AggregationIri),
    %Now asserting 'print' iris, should be 'object'?
    literal_to_id(['print-', LowerID], nat, ObjectIri).

%%	assert_object(+ObjectDict, +AggregationIri, +ObjectIri)
%
%	Assert the information related to the provided cultural heritage
%	object into the rdf database.
assert_object(ObjectDict, AggregationIri, ObjectIri) :-
    rdf_assert(AggregationIri, edm:aggregatedCHO, ObjectIri, naturalis_prints),
    rdf_assert(ObjectIri, rdf:type, edm:'ProvidedCHO', naturalis_prints),
    Creator = ObjectDict.creator,
    assert_object_creator(Creator, ObjectIri),
    Type = ObjectDict.type,
    assert_object_type(Type, ObjectIri),
    debug(ubvu, 'Asserting object: ~p', [ObjectIri]).

%%	assert_web_resource(+ObjectDict, +AggregationIri)
%
%	Assert resources available on the web representing the object.
assert_web_resource(ObjectDict, AggregationIri) :-
    WebResource = ObjectDict.image_url,
    rdf_assert(AggregationIri, edm:isShownBy, WebResource, naturalis_prints),
    rdf_assert(WebResource, rdf:type, edm:'WebResource', naturalis_prints),
    debug(ubvu, 'Asserting view: ~p', [WebResource]).

%%	assert_aggregation_data(+ObjectDict, +AggregationIri)
%
%	Assert the aggregation connecting the object and the web
%	resources.
assert_aggregation_data(ObjectDict, AggregationIri) :-
    Source = ObjectDict.source,
    rdf_assert(AggregationIri, edm:dataProvider, literal(Source), naturalis_prints),
    rdf_assert(AggregationIri, rdf:type, ore:'Aggregation', naturalis_prints),
    debug(ubvu, 'Asserting source: ~p', [Source]).

% TODO: Hardcoded the ulan id, should do a lookup instead.
assert_object_creator('Koekkoek, M.A.', ObjectIri) :-
    rdf_assert(ObjectIri, dc:creator, literal('Koekkoek, M.A.'), naturalis_prints),
    rdf_assert(ObjectIri, dcterms:creator, ulan:'500020715', naturalis_prints),
    rdf_assert(ulan:'500020715', rdf:type, edm:'Agent', naturalis_prints),
    !.

assert_object_creator(Creator, ObjectIri) :-
    rdf_assert(ObjectIri, dc:creator, literal(Creator), naturalis_prints).


assert_object_type(drawing, ObjectIri) :-
    rdf_assert(ObjectIri, dc:format, literal('drawings (visual works)'), naturalis_prints),
    rdf_assert(ObjectIri, dcterms:format, aat:'300033973', naturalis_prints),
    !.

assert_object_type(Type, ObjectIri) :-
    rdf_assert(ObjectIri, dc:format, literal(Type), naturalis_prints).

%%	save_birds
%
%	Save the result relative to the =data= search path.
save_prints :-
    absolute_file_name(data('../../../git/ubvu_bibles/rdf/ubvu_bibles.ttl'),
		       File, [access(write)]),
    rdf_save_turtle(File, [graph(ubvu_bibles)]).

%%	convert_naturalis_zangvogels
%
%	Perform the entire conversion:
%
%	  1. Load the data.
%	  2. Add RDF statements.
%	  3. Save the result.
convert_ubvu_bibles :-
    load_bible_tsv(ObjectList),
    assert_objects(ObjectList),
    save_prints.
