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
:- rdf_register_ns(aat, 'http://vocab.getty.edu/aat/').
:- rdf_register_ns(ulan, 'http://vocab.getty.edu/ulan/').
:- rdf_register_ns(nat, 'http://purl.org/collections/nl/naturalis/').
:- rdf_register_ns(nats, 'http://purl.org/collections/nl/naturalis/schema#').
:- rdf_register_ns(txn, 'http://lod.taxonconcept.org/ontology/txn.owl#').
:- rdf_register_ns(birds, 'http://purl.org/collections/birds/').
:- rdf_register_ns(dcterms, 'http://purl.org/dc/terms/').
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

load_naturalis_koekkoek_csv(PrintList) :-
    csv_read_file('Metadata Ornithologia Neerlandica platen Koekkoek.csv', Rows),
    skip_row(Rows, Rows1),
    read_prints(Rows1, PrintList).

skip_row([_Row|Rows],Rows).

read_prints([], []).
read_prints([Row|Rows], [PrintDict|PrintList]) :-
    get_info_row(Row,
		 Id,
		 NLName,
		 ENName,
		 ScientificName,
		 NLDescription,
		 ENDescription,
		 ImageUrl,
		 PublishedIn,
		 Creator,
		 ImageLicense,
		 LicenseNote,
		 Type,
		 Source
		),
    PrintDict = print{
		    id:Id,
		    nl_name:NLName,
		    en_name:ENName,
		    scientific_name:ScientificName,
		    nl_description:NLDescription,
		    en_description:ENDescription,
		    image_url:ImageUrl,
		    published_in:PublishedIn,
		    creator:Creator,
		    image_license:ImageLicense,
		    license_note:LicenseNote,
		    type:Type,
		    source:Source
		     },
    read_prints(Rows, PrintList).



%%	get_info_row(+Row, -NaturalisID, -ScientificName)
%
%	Get the info needed from row.
get_info_row(row(Id, _PlateOf, _By, NLName, ENName, ScientificName,
		 NLDescription, ENDescription, ImageUrl, _W800, _JPG,
		 PublishedIn, Creator, ImageLicense, LicenseNote, Type, Source
		),
	     Id,
	     NLName,
	     ENName,
	     ScientificName,
	     NLDescription,
	     ENDescription,
	     ImageUrl,
	     PublishedIn,
	     Creator,
	     ImageLicense,
	     LicenseNote,
	     Type,
	     Source
	    ).

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
    assert_object_subject(ObjectDict, ObjectIri),
    debug(naturalis, 'Asserting object: ~p', [ObjectIri]).

%%	assert_web_resource(+ObjectDict, +AggregationIri)
%
%	Assert resources available on the web representing the object.
assert_web_resource(ObjectDict, AggregationIri) :-
    WebResource = ObjectDict.image_url,
    rdf_assert(AggregationIri, edm:isShownBy, WebResource, naturalis_prints),
    rdf_assert(WebResource, rdf:type, edm:'WebResource', naturalis_prints),
    debug(naturalis, 'Asserting view: ~p', [WebResource]).

%%	assert_aggregation_data(+ObjectDict, +AggregationIri)
%
%	Assert the aggregation connecting the object and the web
%	resources.
assert_aggregation_data(ObjectDict, AggregationIri) :-
    Source = ObjectDict.source,
    rdf_assert(AggregationIri, edm:dataProvider, literal(Source), naturalis_prints),
    rdf_assert(AggregationIri, rdf:type, ore:'Aggregation', naturalis_prints),
    debug(naturalis, 'Asserting source: ~p', [Source]).

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

%%	assert_object_subject(ObjectDict, ObjectIri)
%
%	Assert the subject of the object (e.g. the depicted bird)
assert_object_subject(ObjectDict, ObjectIri) :-
    ScientificName = ObjectDict.scientific_name,
    decompose_scientific_name(ScientificName, Genus, Species, SubSpecies),
    downcase_atom(ScientificName, LowerName),
    construct_bird_iri(SubSpecies, LowerName, BirdIri),
    match(BirdIri, Match),
    assert_missing_bird_names(BirdIri, ObjectDict, Match),
    assert_subject_bird(Match, ObjectIri, BirdIri),
    debug(naturalis, 'Match ~p ObjectIri ~p ScientificName ~p Genus ~p Species ~p SubSpecies ~p',
	  [Match, ObjectIri, ScientificName, Genus, Species, SubSpecies]).

%%	construct_bird_iri(+Subspecies, +ScientificName, -Iri)
%
%	Constructs a uri specific to the type (deducted by determining
%	whether the subspecies is defined) and returns this type and the
%	constructed URI
construct_bird_iri(no_subspecies, ScientificName, Iri) :-
    literal_to_id(['species-', ScientificName], birds, Iri),
    !.

construct_bird_iri(_, ScientificName, Iri) :-
     literal_to_id(['subspecies-', ScientificName], birds, Iri).

%%	assert_missing_bird_names(+SubSpecies, +BirdIri, +ObjectDict)
%
%	Add names of birds when not yet present and iri is matched to a
%	resource in birds. TODO: how to properly add this while
%	considering the source? Now adding it to birds..
assert_missing_bird_names(_BirdIri, _ObjectDict, false) :-
    !.
assert_missing_bird_names(BirdIri, ObjectDict, true) :-
    rdf_equal(Predicate, txn:commonName),
    Graph = birds,
    NLName = ObjectDict.nl_name,
    assert_missing_info(BirdIri, Predicate, NLName, nl, Graph),
    ENName = ObjectDict.en_name,
    assert_missing_info(BirdIri, Predicate, ENName, en, Graph).

%%	assert_missing_info(+Iri, +Predicate, +Value, +LanguageTag,
%	+Graph)
%
%	Assert additional info in case it is not already present in the
%	graph.
assert_missing_info(Iri, Predicate, Value, LanguageTag, _) :-
    rdf(Iri, Predicate, literal(lang(LanguageTag, _Value))),
    debug(missing, 'Present: Iri ~p, Predicate ~p, Value ~p, LanguageTag, ~p',
	  [Iri, Predicate, Value, LanguageTag]),
    !.

assert_missing_info(Iri, Predicate, Value, LanguageTag, Graph) :-
    rdf_assert(Iri, Predicate, literal(lang(LanguageTag, Value)), Graph),
    debug(missing, 'Asserting: Iri ~p, Predicate ~p, Value ~p, LanguageTag ~p, Graph ~p ',
	  [Iri, _PredicateTEMP, Value, LanguageTag, Graph]).

%%	match(+Iri, -Boolean
%
%	Returns true when Iri is present, false otherwise.
match(Iri, true) :-
    rdf(Iri, _, _),
    !.

match(Iri, false) :-
    debug(no_match, 'No match: ~p', [Iri]).

%%	assert_subject_bird(+Boolean, +ObjectIri, +BirdIri)
%
%	Assert a bird as a subject on the object whenever the Iri is
%	present in the graph.
assert_subject_bird(true, ObjectIri, BirdIri) :-
    rdf_assert(ObjectIri, dc:subject, BirdIri, naturalis_prints),
    !.

assert_subject_bird(false, _ObjectIri, _BirdIri).

%%	save_birds
%
%	Save the result relative to the =data= search path.
save_prints :-
    absolute_file_name(data('../naturalis/rdf/naturalis_koekkoek.ttl'),
		       File, [access(write)]),
    rdf_save_turtle(File, [graph(naturalis_prints)]).

%%	convert_naturalis_zangvogels
%
%	Perform the entire conversion:
%
%	  1. Load the data.
%	  2. Add RDF statements.
%	  3. Save the result.
convert_ubvu_bibles :-
    load_naturalis_koekkoek_csv(ObjectList),
    assert_objects(ObjectList),
    save_prints.
