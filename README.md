# VU Library 18th century Illustrated Bibles Readme #
Dataset containing metadata about two illustrated bibles from the 18th century from the VU Library.

This repo contains the code and results of a conversion to RDF of two 18th century illustrated bibles, [first one](http://imagebase.ubvu.vu.nl/cdm/compoundobject/collection/bis/id/41) and [second](http://imagebase.ubvu.vu.nl/cdm/compoundobject/collection/bis/id/1004).

The following folders are included:

* source: source files of the two illustrated bibles
* code: prolog code for converting the source files
* rdf: the Turtle files resulting from the conversion

## Run conversion ##
This is a step by step description of how to run the conversion to RDF.

- Clone this repo: `$ git clone git@github.com/LaraHack/ubvu_bibles.git`

- [Install Prolog](http://www.swi-prolog.org/Download.html) and [ClioPatria](http://cliopatria.swi-prolog.org/help/) and create a server folder on the same level as the vbvu_bibles repo, e.g.:

```
$ mkdir conversion
$ cd conversion/
$ ../cliopatria
```

- Run the server on the commandline: `$ ./run.pl`

- Browser view of the local [Cliopatria instance] (http://localhost:3020/)

- Install the XMLRDF package: `?- cpack_install(xmlrdf).`

- Load the conversion code:  `?- ['../ubvu_bibles/code/convert_ubvu_bibles].`

- Run the conversion: `?- convert_ubvu_bibles.`

The results of this conversion can be found in the `ubvu_bibles/rdf/` folder.

## Change the conversion ##
The code for running the conversion can be tweaked for a specific scenario. To edit the code, the Prolog editor can be used: `?- edit(convert_ubvu_bibles).`

After the editing is finished, build the file using `?- make.`, then run again `?- convert_ubvu_bibles.`

## Licences ##
Both illustrated bibles in this repo belong to the UBVU Library and any public usage should be made in accordance with the VU library policies.
